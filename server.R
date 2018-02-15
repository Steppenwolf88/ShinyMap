
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(maptools)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

function(input, output) {
  output$contents <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    tab <- c(mean(data$PfPR2.10), sd(data$PfPR2.10))
    names(tab) <- c("Mean", "Standard deviation")
    tab
  })
  
  output$SHPplot <-  renderPlot({
    
    myshape<- input$file_shp
    if (is.null(myshape)) 
      return(NULL)       
    
    dir<-dirname(myshape[1,4])
    
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
    }
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shape<-readShapePoly(getshp)
    plot(shape)
    
  })
}
