#source("itms_tabs.R")
library(shiny)


shinyUI(
  fluidPage(
    list(
      tags$head(
        tags$link(rel="stylesheet",href="app.css")
      )
    ),
    navbarPage(
      title = div(
        img(src="itmslogo.jpg", class="app-logo", height = 40),
        div(class="app-title", "Interactive Text Mining Suite ITMS")
      ),
      position="fixed-top",
      tabPanel("About", 
        div(class="jumbotron",
          h2(class="welcome", "Welcome!"),
          p("Interactive Text Mining Suite ITMS is a web application for text analysis. This application offers the computational and statistical power of R and the interactivity of Shiny web app."),
          a(href="http://shiny.rstudio.com", target="_blank", "Learn more about Shiny")
        ),
        p(style="margin-bottom:20px;", "ITMS project represents a data visualization toolkit for digital humanities research. The goal of this on-going research is to gather state-of-the-art visualization and statistical methods for non-statisticians and non-programmers. The research focuses on various types of data analysis: topic modeling, frequency analysis, diachronic analysis, data summary, clustering, among many others. Not all options are yet available for users."),
        div(class="panel panel-primary",
          div(class="panel-heading", "Data Preparation"),
          div(class="panel-body", 
            tags$ol(
              tags$li("Upload file(s) in pdf or txt format. If PDF extraction produces errors, convert your documents in a text format first, correct any OCR errors and then upload it for ITMS analysis."),
              tags$li("Upload or extract metadata (e.g. year of composition, title and author). To upload metadata, create a csv file with headers 'Year','Title', 'Author'. You can add an additional column for 'Category' (e.g. verse, prose, article etc).")
            )
          )
        ),
        div(class="panel panel-primary",
          div(class="panel-heading", "Data Visualization"),
          div(class="panel-body", 
            tags$ol(
              tags$li("Frequency"),
              tags$li("Topic (lda and stm)"),
              tags$li("Cluster"),
              tags$li("Punctuation Analysis"),
              tags$li("Dynamic Analysis")
            )
          )
        ),
        p("We welcome comments, suggestions, notes on any errors or problems encountered by users. Please contact Dr. Olga Scrivner (obscrivn AT indiana DOT edu)"),
        div(style="margin-bottom:20px",a(href="http://cl.indiana.edu/~obscrivn/", target="_blank", "Dr. Olga Scrivner"))
      ),
      navbarMenu(title = "File Uploads",
        tabPanel("Text Files",
          fluidPage(
            shiny::tags$head(shiny::tags$style(shiny::HTML(
              "#text { font-size: 15px; height: 300px; overflow: auto; }"
            ))),
            fluidRow( 
              column(5,
                tags$h4("Format - txt"),
                fileInput('file.article.txt', 'Choose File(s) in TEXT format',multiple=TRUE, accept=c('txt')),
                uiOutput("print_name_article_txt"),
                uiOutput("print_length_txt")
              ),
              column(5,
                h4("Text Segmentation")
              )
            ),
            fluidRow(
              column(12,
                tags$hr(),
                div(id = "text", uiOutput("print_content_txt"))
              )
            )
          ) 
        ),
        tabPanel("PDF Files",
          fluidPage(
            shiny::tags$head(shiny::tags$style(shiny::HTML(
              "#text { font-size: 15px; height: 300px; overflow: auto; }"
            ))),
            fluidRow( 
              column(5,
                tags$h4("Format - pdf"),
                fileInput('file.article', 'Choose File(s) in PDF format',multiple=TRUE, accept=c('pdf')),
                uiOutput("print_name_article"),
                uiOutput("print_length_pdf")
              ),
              column(6,
                tags$h4("Extract Abstracts"),
                uiOutput("choose_article_content")
              )
            ),
            fluidRow(
              column(8,
                tags$hr(),
                tags$h5("Full Text Viewer"),
                div(id = "text", uiOutput("print_content_pdf"))
              ),
              column(4,
                tags$hr(),
                tags$h5("Abstract Viewer"),
                div(id = "text", uiOutput("print_abstract"))
              )
            )
          ) 
        ),
        tabPanel("ZOTERO", "Under development"),
        tabPanel("JSON", "Under development")
      ),     
      tabPanel("Data Preparation",

        tabsetPanel(type = "tabs", 
          tabPanel(
            tags$h4("Data Cleaning"),
            fluidPage(
              shiny::tags$head(shiny::tags$style(shiny::HTML(
                "#text { font-size: 15px; height: 300px; overflow: auto; }"
              ))),
              fluidRow( 
                column(4,
                  tags$h4("Select Preprocessing Steps"),
                  tags$hr(),
                  checkboxInput('remove_punctuation', 'Remove Punctuation', FALSE),
                  radioButtons(
                    'exceptions', 
                    'Exceptions (keep hyphen or apostrophe)',
                    c(both='Keep apostrophe and hyphen', hyphen='Keep hyphen', apostrophe='Keep apostrophe'), 
                    FALSE
                  ),
                  tags$hr(),
                  checkboxInput('lower_case', 'Lower Case', FALSE),
                  tags$hr(),
                  checkboxInput('remove_numbers','Remove Numbers',FALSE),
                  tags$hr(),
                  checkboxInput('remove_references','Remove References',FALSE),
                  tags$hr(),
                  checkboxInput('remove_urls','Remove Urls',FALSE),
                  tags$hr(),
                  checkboxInput('remove_html','Remove HTML',FALSE)
                ),
                column(6,
                  tags$h4("Preprocessing Viewer"),
                  tags$hr(),
                  radioButtons(
                    'preprocessing', 
                    'Apply Steps or Default (no preprocessing) ',
                    c(apply='Apply Steps',default='No Changes'), 
                    'No Changes'
                  ),
                  div(id = "text", uiOutput("print_preprocessed"))
                )
              )
            )
          ),
          tabPanel(
            tags$h4("Stopwords"),
            br(),
            p(
              "List of stopwords: 1)", 
              tags$a(href="http://www.ranks.nl/stopwords","http://www.ranks.nl/stopwords"), 
              "2) ",
              tags$a(href="http://www.lextek.com/manuals/onix/stopwords1.html","http://www.lextek.com/manuals/onix/stopwords1.html"),
              " 3) ",
              tags$a(href="http://www.webconfs.com/stop-words.php","http://www.webconfs.com/stop-words.php"),
              "3) Extensive stopword list with proper names from Matthew Jockers",
              tags$a(href="http://www.matthewjockers.net/macroanalysisbook/expanded-stopwords-list/","http://www.matthewjockers.net/macroanalysisbook/expanded-stopwords-list/"),
              ". Words must be one per line."
            ), 
            p("Default is the list from tm package: stopwords(`SMART')"),
            # tags$hr(),
            # tags$h5("Upload a stopword list (left panel)"),
            fluidPage(
              fluidRow( 
                column(4,
                  tags$hr(), 
                  radioButtons(
                    'stops', 
                    'Select Default stopwords or Upload from your file below',
                    c(Default='Default',Upload='Upload'), 
                    FALSE
                  ),
                  tags$hr(),
                  fileInput('stopwords.txt', 'Upload stopwords in txt format (one word per line)',multiple=FALSE, accept=c('txt'))
                ),
                column(4,
                  tags$hr(), 
                  uiOutput("show_cutoff_lower"),
                  uiOutput("show_cutoff_high")
                ),
                column(4,
                  tags$hr(),
                  checkboxInput('show_stopwords','Show Stopwords',FALSE),
                  verbatimTextOutput("print_stopwords")
                )
              ),
              fluidRow(
                p("Manually remove additional stopwords below:"),
                helpText("Type words to be removed and click add"),
                column(12, 
                  uiOutput("choose_remove"),
                  uiOutput("printWords"),
                  br(),
                  br(),
                  tags$hr()
                )
              )
            )
          ),
          tabPanel(
            tags$h4("Stemming"),
            fluidPage(
              fluidRow(
                column(4, 
                  tags$hr(), 
                  tags$h5("Replacing words with Stems"),
                  checkboxInput('choose_stem','Stem',FALSE)
                ),
                column(6, 
                  tags$hr(),
                  tags$h5("Adding more Stems")
                  # p("Add manually additional stems:")
                )
              )
            )
          ),
          tabPanel(
            tags$h4("Metadata"),
            br(),
            p(
              "Metadata can consists of Ids, Year, Title, Author, Category, Location. 
              You can upload your metadata in csv format (see a template). 
              If your pdf files contain matadata - it will be printed here."
            ),
            fluidPage(
              fluidRow( 
                column(6, 
                  tags$hr(),
                  tags$h5("Select metadata from pdf files (it may not work for all pdf files)"),
                  uiOutput("load_metadata_pdf"),                                                              
                  tableOutput("print_metadata_pdf")
                ),
                column(6, 
                  tags$hr(),
                  tags$h5("Upload Metadata from csv file (left panel): Id, Year, Title(name), Author, Category"),
                  fileInput('file.metadata', 'Choose CSV File', multiple=FALSE, accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                  uiOutput("load_metadata_csv"), 
                  dataTableOutput("print_metadata_csv")
                )
              )
            )
          )
        )
      ),
      navbarMenu(title = "Data Visualization",
        tabPanel("Word Frequency",
          tabsetPanel(type = "tabs", 
            tabPanel("Frequency Table"),
            tabPanel("Length"),
            tabPanel("KWIC")
          )
        ),
        tabPanel("Cluster Analysis", "Under development"),
        tabPanel("Topic Analysis", "Under development")
      )
    )
  )
)


