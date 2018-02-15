
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(maptools)
library(WriteXLS)
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

function(input, output) {

  datasetOutput <- function() {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    finaltable <- data[1:10,]
    return(finaltable)
  }
  
  output$Prediction_results.xls <- downloadHandler(
    filename = function() {
      paste("Prediction_results.xls")
    },
    content = function(file) {
      WriteXLS(datasetOutput(), ExcelFileName=file, row.names = FALSE)
    }
  )

  
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
