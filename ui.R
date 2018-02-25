
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      fileInput('file_shp', 'Upload shapefile',
                accept = c(
                  '.shp',
                  '.dbf',
                  '.sbn',
                  '.sbx',
                  '.shx',
                  '.prj'
                ), multiple = TRUE
      ),
      checkboxInput("locations", "Show empirical prevalence", FALSE),
      selectInput("pred", "Target of prediction:", 
                  choices=c("None","Prevalence","Exceedance probabilities")),
      downloadButton("Prediction_results.xls", "Download")
    ),
    mainPanel(
      tableOutput('contents'),
      leafletOutput('SHPplot',height = 800)
    )
  )
)

