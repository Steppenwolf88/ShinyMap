
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(maptools)
library(WriteXLS)
library(splancs)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

function(input, output) {

  datasetOutput <- function() {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data.old <- read.csv("Uganda_2000_2016.csv")
    data.new <- read.csv(inFile$datapath, header = TRUE)
    
    load("estim_Uganda.RData")
    
    z <- c(data.old$PfPR2.10/100,data.new$PfPR2.10/100) 
    Ex <- c(data.old$Ex,data.new$Ex)
    y <- log((Ex*z+0.5)/(Ex-Ex*z+0.5))
    n <- length(y)
    D <- cbind(rep(1,n))
    
    coords <- rbind(data.old[,c("Long","Lat")],data.new[,c("Long","Lat")])
    times <- c(data.old$YEAR,data.new$YEAR)
    U <-as.matrix(dist(coords))
    U.t <- as.matrix(dist(times))
    
    beta <- est.taper$par[1]
    sigma2 <- est.taper$par[2]
    phi <- est.taper$par[3]
    psi <- est.taper$par[4]
    tau2 <- est.taper$par[5]
    
    varcov <- function(U,U.t,phi,psi) {
      R.exp.s <- exp(-U/phi)
      R.exp.t <- exp(-U.t/psi)
      return(R.exp.s*R.exp.t)
    } 
    
    Sigma <- varcov(U,U.t,phi,psi)
    Sigma <- sigma2*Sigma
    diag(Sigma) <- diag(Sigma)+tau2
    Sigma.inv <- solve(Sigma)

    myshape<- input$file_shp
    if (is.null(myshape)) 
      return(NULL)       
    
    dir<-dirname(myshape[1,4])
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shp<-readShapePoly(getshp)
    
    n.distr <- length(shp)
    n.dist <- length(shp)
    
    year.set <- c(2017)
    n.year <- length(year.set)
    
    district.out <- matrix(NA,ncol=n.year,nrow=n.dist)
    rownames(district.out) <- shp@data$District
    
    district.q025.out <- matrix(NA,ncol=n.year,nrow=n.dist)
    rownames(district.q025.out) <- shp@data$District
    
    district.q975.out <- matrix(NA,ncol=n.year,nrow=n.dist)
    rownames(district.q975.out) <- shp@data$District
    
    district.se.out <- matrix(NA,ncol=n.year,nrow=n.dist)
    rownames(district.se.out) <- shp@data$District
    
    n.sim <- 1000
    
    library(pdist)
    for(i in 1:n.dist) {
      poly <- shp@polygons[[i]]@Polygons[[1]]@coords
      grid.pred <- gridpts(poly,xs=phi/30,ys=phi/30)
      
      if(length(grid.pred)==0) {
        grid.pred <- gridpts(poly,npts=10)
      }
      
      for(j in 1:n.year) {
        year <- year.set[j]
        U.t <- abs(times-year)
        
        U.pred <- as.matrix(dist(grid.pred))
        U.pred.obs <- as.matrix(pdist(grid.pred,coords))
        C.s.obs <- exp(-U.pred.obs/phi)
        Sigma.s <- sigma2*exp(-U.pred/phi)
        
        C <- sigma2*t(t(C.s.obs)*exp(-U.t/psi))
        A <- C%*%Sigma.inv
        
        mu.cond <- as.numeric(beta+A%*%(y-beta))
        Sigma.pred <- Sigma.s-A%*%t(C)
        Sigma.pred.sroot <- t(chol(Sigma.pred))
        
        n.pred <- nrow(grid.pred)
        eta.samples <- sapply(1:n.sim,function(x) as.numeric(mu.cond+Sigma.pred.sroot%*%rnorm(n.pred)))
        prev.samples <- 1/(1+exp(-eta.samples))
        
        
        district.samples <- apply(prev.samples,2,
                                  mean)
        district.out[i,j] <- mean(district.samples)
        district.q025.out[i,j] <- quantile(district.samples,0.025)
        district.q975.out[i,j] <- quantile(district.samples,0.975)
        district.se.out[i,j] <- sd(district.samples)
        
        cat("District: ",paste(shp@data$District[i]),"- Year",":",year,"\n")
      }
    }
    shp@data$Prevalence <- district.out
    finaltable <- data.frame(Estimates=district.out,
                             'Quantile 0.25%'= district.q025.out,
                               'Quantile 97.5%'= district.q975.out)
    rownames(finaltable) <- shp@data$District
    write.csv(finaltable,file = "temp_table.csv")
    return(finaltable)
  }
  

  
  output$Prediction_results.xls <- downloadHandler(
    filename = function() {
      paste("Prediction_results.xls")
    },
    content = function(file) {
      WriteXLS(datasetOutput(), ExcelFileName=file, row.names = TRUE)
    }
  )

  
  output$SHPplot<- renderLeaflet({
    myshape<- input$file_shp
    if (is.null(myshape)) 
      return(NULL)       
    
    dir<-dirname(myshape[1,4])
    
    for(i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
    }
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shape<-readShapePoly(getshp)
    
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.HOT") %>% 
      setView(lng = 32.5825, lat = 0.3476, zoom = 8) %>%
      addPolygons(data=shape,fillOpacity = 0,weight=1.5) 
  })
  
  output$plot_prev<- renderLeaflet({
    myshape<- input$file_shp
    if (is.null(myshape)) 
      return(NULL)       
    
    dir<-dirname(myshape[1,4])
    
    for(i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
    }
    
    prev <- read.csv("temp_table.csv")
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shape<-readShapePoly(getshp)
    
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.HOT") %>% 
      setView(lng = 32.5825, lat = 0.3476, zoom = 8) %>%
      addPolygons(data=shape,fillOpacity = 0,weight=1.5) 
  })
    
}
