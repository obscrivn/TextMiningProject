#source("itms_tabs.R")
library(shiny)

shinyUI(
  fluidPage(
    list(
      tags$head(
        tags$title("Interactive Text Mining Suite"),
        tags$link(rel="stylesheet",href="app.css")
      )
    ),
    navbarPage(
      title = div(
       img(src="itmslogo.jpg", class="app-logo", height = 40),
       div(class="app-title", tags$h5(strong("Interactive Text Mining Suite ITMS")))
      ),
     position="fixed-top",
   tabPanel(strong("About"), 
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
       tabPanel(strong("Text Files"),
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
      tabPanel(strong("PDF Files"),
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
                 radioButtons(
                   'article_content', 
                   'Select Abstract',
                   c(none = 'Full Text',
                     abstract='Abstract'), 
                   'Full Text'
                 )
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
      tabPanel(strong("ZOTERO"), "Under development"),
      tabPanel(strong("JSON"), "Under development"),
      tabPanel(strong("POS-Tagged Text"), "tm package - readTagged")
      ),     
      tabPanel(strong("Data Preparation"),
         tabsetPanel(type = "tabs", 
           tabPanel(
             tags$h4("Data Cleaning"),
             br(),
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
                       c(none = 'No exceptions',
                         both='Keep apostrophe and hyphen', 
                         hyphen='Keep hyphen', 
                         apostrophe='Keep apostrophe'), 
                         'No exceptions'
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
           tabPanel(tags$h4("Stopwords"),
              br(),
              fluidPage(
                 fluidRow( 
                   column(4, tags$h4("Select Stopwords"),
                     tags$hr(), 
                     radioButtons('stops', 
                       'Select Default or Upload',
                       c(None="None",
                         Default='Default',
                         Upload='Upload'),
                       'None'),
                     helpText("Default is the list from tm package: stopwords(`SMART')"),
                     tags$hr(),
                     fileInput('stopwords.txt', 'Upload stopwords - txt format (one word per line)',multiple=FALSE,
                     accept=c('txt')),
                     tags$hr(),
                     # checkboxInput('show_stopwords','Show Stopwords',FALSE),
                     div(id = "text", verbatimTextOutput("print_stopwords"))
                  ),
                  column(4,tags$h4("Manual Removal"),tags$hr(),
                   # checkboxInput('remove_manual','Apply Manual',FALSE),
                    helpText("Type words to be removed and click add"),
                    uiOutput("choose_remove"),
                    div(id = "text", uiOutput("printWords"))
                  ),
                  column(4,tags$h4("Viewer"),
                    tags$hr(),
                    radioButtons('stopwords', 'Apply Stopwords or None (no changes)',
                                 c(apply='Apply Stopwords',
                                   none='None'), 'None'),
                    #checkboxInput('stopwords','Apply Stopwords',FALSE),
                    div(id = "text", uiOutput("print_apply_stops"))
                    # selectizeInput("cutoff_lower", label = "Select or Type Lower Cutoff",choices = c(0,1,2,3,4),options = list(create = TRUE),selected = 0,multiple = FALSE),
                    #  selectizeInput("cutoff_high", label = "Select or Type Upper Cutoff",choices = c(300,200,100),options = list(create = TRUE),selected = NULL,multiple = FALSE) 
                 )
              ),
            fluidRow(
               column(12,
                 p("List of stopwords: 1)", tags$a(href="http://www.ranks.nl/stopwords",
                                         "http://www.ranks.nl/stopwords"), "2) ",
                 tags$a(href="http://www.lextek.com/manuals/onix/stopwords1.html","http://www.lextek.com/manuals/onix/stopwords1.html"),
                 " 3) ",
                 tags$a(href="http://www.webconfs.com/stop-words.php","http://www.webconfs.com/stop-words.php"),
                 "3) Extensive stopword list with proper names from Matthew Jockers
                 ",tags$a(href="http://www.matthewjockers.net/macroanalysisbook/
                  expanded-stopwords-list/",
                  "http://www.matthewjockers.net/macroanalysisbook/expanded-stopwords-list/"),".
                 Words must be one per line."),
                 br(),
                 tags$hr()  
               )
            )
        )
    ),
    tabPanel(
      tags$h4("Stemming"),
      br(),
      fluidPage(
        fluidRow(
          column(4, 
             tags$h4("Stems - tm package"),
             tags$hr(), 
             
            # checkboxInput('choose_stem','Stem',FALSE),
             radioButtons('language', 'Choose Language',
                          c(none='none',
                            English='english',
                            Spanish='spanish',
                            Danish="danish",
                            Dutch="dutch",
                            Finnish="finnish",
                            French="french",
                            German="german",   
                            Hungarian= "hungarian",
                            Italian="italian",
                            Norwegian="norwegian",
                            Porter="porter" #     "portuguese" "romanian"  
                           # [13] "russian"    "spanish"    "swedish"    "turkish"
                            ), 'none'),
             tags$hr()
          ),
          column(6, 
             tags$h4("Manual Stems"),
             tags$hr(),
             "Under development"
             # p("Add manually additional stems:")
          )
        ),
        fluidRow(
          column(12, tags$h4("Stem Viewer"),
            tags$hr(),
            div(id = "text", uiOutput("print_stemmer"))
          )
        )
      )
    ),
    tabPanel(
      tags$h4("Metadata"),
      br(),
      p(
        "Metadata consists of Ids, Year, Title, Author, Category, Location. 
        You can upload your metadata in csv format (see a template). 
        If your pdf files contain matadata - it will be printed here."
      ),
      fluidPage(
        fluidRow( 
          column(6, 
             tags$hr(),
             tags$h5("Select"),
             radioButtons('metadata', 'Metadata from pdf files',
                          c(none='None',
                            metadata='Load'), 'None'),
            # checkboxInput('metadata','Load',FALSE),
             tableOutput("print_metadata_pdf")
          ),
          column(6, 
             tags$hr(),
             tags$h5("Upload Metadata from csv file (left panel): Id, Year, Title(name), Author, Category"),
             fileInput('file.metadata', 'Choose CSV File', multiple=FALSE, accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
             #uiOutput("load_metadata_csv"),
             checkboxInput('header', 'Header', TRUE),
             radioButtons('sep', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ','),
             tags$hr(),
             radioButtons('metadata_csv', 'Read Metadata from CSV',
                          c(none='None',
                            upload='Load'), 'None'),

             tags$hr(),
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
            tabPanel(tags$h4("Frequency Table"),
             br(),
             fluidPage(
               fluidRow(
                  column(6, 
                     tags$h4("Raw Frequency"),
                     tags$hr(),
                    # checkboxInput('show_freq','Frequency',FALSE),
                      dataTableOutput("freq")
                   ),
                   column(5, 
                      tags$h5("Frequency Law"),
                      tags$hr(),
                      p("\"Zipf's law is an empirical law in linguistics describing commonly 
                      observed characteristics of term frequency distributions in corpora\" (tm Package)"),
                        #  p("Zipf plot - tm package"),
                     plotOutput("zipf")
                    # p("Heaps plot - tm package"),
                     #plotOutput("heaps")
                  )
               )
             )
         ),
         tabPanel(tags$h4("Word Clouds"),
            br(),
           fluidPage(
              fluidRow(
                column(5, 
                  radioButtons('corpus', 'Select Corpus Type',
                      c(raw='raw',
                      preprocessed='preprocessed'),
                                    'raw'),
                   tags$hr()
                ),
                # plotOutput("word_count")),
                column(5, 
                       # uiOutput("choose_cloud"),
                  uiOutput("choose_top"),
                  tags$hr()
                  )
               ),
               fluidRow(
                  column(12,
                    #tags$h5("Frequency Bar Plot"),
                    plotOutput("word_count")
                  )
                ),
                #hr(),
                fluidRow(
                   column(5,
                       p("Set the minimum frequency for your cloud visualization"),
                       uiOutput("choose_min_frequency")
                   ),
                   column(5,p("Set the maximum words per plot"),
                       uiOutput("choose_max_words")
                   )
                ),
                fluidRow(
                   column(12, 
                       
                     plotOutput("print_cloud")
                   )
                )#,
              )
         ),
         tabPanel(tags$h4("Length"),
           br(),
           fluidPage( p("qdap package: word_length and sentSplit"),
             fluidRow( p("Default sentence split is based on qdap sentSplit (? ! . |)"),
                column(4, 
                   tags$h4("Select Length Type"),
                  tags$hr(),
                   radioButtons('show_word', 'Select Word or Sentence Length',
                      c(none='None',
                        word='Word Length',
                        sentence='Sentence Length'), 'None'),
                        tags$hr()
                      # uiOutput("show_word")
                 ),
                 column(4, tags$h4("Select Text"),
                        tags$hr(),
                   uiOutput("choose_text"),
                   uiOutput("choose_bin"),
                   tags$hr()
                 )
            ),
          fluidRow( 
              column(3,   #verbatimTextOutput("length_table"),     
                 tableOutput("length_word_table")
              ),
             column(9, #uiOutput("show_sentence"),
                                                 # plotOutput("sentences")
               plotOutput("words")
             )
          )
         )
        ),
        tabPanel(tags$h4("KWIC"),
          br(),
          p("Key Word in Context Analysis"),
          fluidPage(
            fluidRow(
              column(5, 
                tags$h4("Select key word"),
                uiOutput("choose_term"),
                tags$hr()
              ),
              column(7, 
                tags$h4("Select the length of the context (left/right)"),
                uiOutput("choose_length"),
                tags$hr()
              )
            ),
           fluidRow(
              column(12, 
                uiOutput("term_print"),
                tags$hr(),
                div(id = "text", uiOutput("print_kwic"))
              )
            )
          )
        ),
        tabPanel(tags$h4("Punctuation"),
        fluidPage(                        
          p("This section is based on the punctuation analysis by Adam Calhoun's ",tags$a(href="https://medium.com/@neuroecology/punctuation-in-novels-8f316d542ec4","Blog"),". 
            In his heatmap visualization, periods and question marks and exclamation marks are red.
            Commas and quotation marks are green. 
            Semicolons and colons are blue"),
         # uiOutput("choose_punctuation"),
          fluidRow(
            column(4,
              tags$h4("Punctuation Frequency"),
              hr(),
              plotOutput("type_punctuation")
            ),
            column(4,
              tags$h4("Punctuation Map"),
              tags$hr(),
              plotOutput("heatmap_punct")
            )
          )
         )
        )
       )
      ),
      tabPanel("Cluster Analysis",
         p("Cluster Analysis (Descriptive Statistics) examines 
            how variables or individuals are grouped. The visual 
            representation is often referred to as a dendrogram,  
            as groups are clustered into tree branches"),
         fluidPage(
           fluidRow(
             column(4,
                tags$h4("Agglomeration Methods"),
                tags$hr(),
                radioButtons('method', 'Select method for cluster groups',
                             c(ward.D='ward.D',
                               single='single',
                               complete='complete',
                               average="ave",
                               median="median",
                               centroid="cen"), 'ward.D'),
                tags$hr()
             ),
             column(4,
               tags$h4("Distance Measure"),
               tags$hr(),
               radioButtons('distance', 'Select measure type',
                            c(euclidean='euclidean',
                              maximum='maximum',
                              manhattan='manhattan',
                              minkowski="minkowski",
                              canberra="canberra",
                              binary="binary"
                              ), 'euclidean'),
                tags$hr()
             ),
             column(4,
               tags$h4("Select Groups"),
               tags$hr(),
               radioButtons('color', 'Select color for cluster groups',
                 c(none="none",
                   red='red',
                 blue='blue',
                 green='green',
                 black="black"), 'red'),
               uiOutput("cuttree"),
               tags$hr()
             )
          ),
          fluidRow(
            column(12,
              plotOutput("cluster_plot")
            )
          )
        )
      ),
      tabPanel("Topic Analysis", "Under development")
   )
  )
 )
)