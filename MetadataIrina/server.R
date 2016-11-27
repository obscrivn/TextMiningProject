library(shiny)
library(jsonlite)
library(XML)

#Stand-alone metadata app
shinyServer(function(input, output) {

   output$place_for_file_browser <- renderUI({
    if( input$metadata_source == "PDF ") {
      metadata_file_name = NULL;
      return()
    }
    switch (input$metadata_source,
      "CSV" = column(6,
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        fileInput('csv_file_name', 'Choose CSV File', multiple=FALSE, accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
      ),
      "JSON"= fileInput('json_file_name', 'Choose JSON File', multiple=FALSE, accept=c('application/json',',JSON')),
      "XML" = fileInput('xml_file_name', 'Choose XML File', multiple=FALSE, accept=c('application/xml','text/xml','.xml'))
    )
  })
  ## Reading Metadata File from csv
  fileData <- reactive({ # loading data
    my_data = NULL
    if( !is.null(input$csv_file_name) ) {
      my_data <- read.csv(input$csv_file_name$datapath, header=input$header, sep=input$sep, quote=input$quote)
    }
    else if( !is.null(input$json_file_name) ) {
      my_data <- fromJSON(input$json_file_name$datapath, flatten = TRUE)
    }
    else if( !is.null(input$xml_file_name) ) {
      print(input$xml_file_name$datapath)
      my_data <- xmlToDataFrame(input$xml_file_name$datapath)
    }
    return(my_data)
  })
  ### Display Metadata from CSV
  output$place_for_metadata_table <- renderDataTable({
    data_from_metadata_file = fileData()
    if ( is.null(data_from_metadata_file) )  { return() }
    data_from_metadata_file
  })
})
