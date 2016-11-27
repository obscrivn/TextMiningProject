library(shiny)
library(rjson)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Metadata"),

  tabPanel(
    tags$h4("Metadata"),
    br(),
    p(
      "Metadata can consists of Id, Year, Title, Author, Category, Location. 
              You can upload your metadata in CSV, JSON or XML formats, 
              or extract it from your pdf files."
              
    ),
    fluidPage(
      fluidRow( 
        column(4, 
               tags$hr(),
               radioButtons('metadata_source', 'Choose metadata source',
                            c("From metadata of each uploaded PDF"="PDF",
                              "From separate CSV file"="CSV",
                              "From separate JSON file"="JSON",
                              "From separate XML file"="XML"),
                            "PDF"),
               uiOutput("place_for_file_browser")                                                              
               
        ),
        column(6, dataTableOutput("place_for_metadata_table"))
      )
    )
  )
)
)
