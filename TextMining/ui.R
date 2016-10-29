#library(shinydashboard)
library(shiny)

shinyUI(navbarPage(theme = "bootstrap.min.css",
                   # theme = shinytheme("readable"),                  
                   tags$h1("Interactive Text Mining Suite ITMS"),
                   img(src = "itmslogo.jpg", height = 60, width = 72),
                   inverse=TRUE, # dark with light text
                   collapsible=FALSE,
                   br(),
                   br(),
                   tags$head(
                     tags$style(HTML("
                                     .nav-tabs {font-size: 20px} "))),
                   tabPanel(tags$h3("About"),#tabName = "about", icon = icon("hand-o-right")) # describe project and show demo pictures
                            img(src = "itmslogo.jpg", height = 55, width = 80,style="float:right; padding-right:25px"),
                            h4("Interactive Text Mining Suite ITMS is a web application for text analysis. This application offers the computational and statistical power of R and the interactivity of Shiny web app."),
                            tags$a(href="http://shiny.rstudio.com/", "http://shiny.rstudio.com/"),
                            br(),
                            br(),
                            hr(),
                            p("ITMS project represents a data visualization toolkit for digital humanities research.
                               The goal of this on-going research is to gather state-of-the-art visualization and statistical methods
                              for non-statisticians and non-programmers. The research focuses on various types of data analysis: topic modeling, 
                              frequency analysis, diachronic analysis, data summary, clustering, among many others. Not all options are yet available for users."),
                            br(),
                            p("Three types of text documents can be processed: scholary articles (pdf and txt), literary and linguistic texts (txt), and transcripts (txt)."),
                            p("We welcome comments, suggestions, notes on any errors or problems encountered by users. 
 Please contact  Dr. Olga Scrivner (obscrivn AT indiana DOT edu)"),
                            tags$a(href="http://cl.indiana.edu/~obscrivn/","Dr. Olga Scrivner")                          
                   ),
                   tabPanel(tags$h3("Demo"),
                            img(src = "itmslogo.jpg", height = 55, width = 80,style="float:right; padding-right:25px"),
                            p(tags$b("Data Preparation"),": 1) Upload file(s) in pdf or txt format. If PDF extraction produces errors, convert your documents in a text format first, 
                              correct any OCR errors and then upload it for ITMS analysis. 2) Upload or extract metadata (e.g. year of composition, title and author). To upload metadata, create a csv file with headers 'Year','Title', 'Author'. You can add an additional column for 'Category' (e.g. verse, prose, article etc)."), 
                            #    tags$a(href="http://www.aclweb.org/anthology/")," 2) Select ", tags$b("Abstract or Full Text"), "3) Perform Frequency, Cluster and Topic Analysis (stemming is currently unavailable)"),
                            p(tags$b("Data Visualization"), ": 1) Frequency, 2) Topic (lda and stm), 3) Cluster, 4) Punctuation Analysis, 5) Dynamic Analysis")
                            #  p(tags$b("Sociolinguistic Mining"), ": 1) Upload any txt audio transcript, 2) Clean data and extract speakers for further analysis.")
                            
                   ),
                   tabPanel(tags$h3("Data Preparation"),
                            sidebarLayout(
                              sidebarPanel( 
                                h4("Upload File(s)"),
                                # helpText("Please select only one format (pdf or txt)."),
                                fileInput('file.article.txt', 'Choose File(s) in TEXT format',multiple=TRUE,
                                          accept=c('txt')),
                          #      uiOutput("print_name_article_txt"),
                                tags$hr(),
                                fileInput('file.article', 'Choose File(s) in PDF format',multiple=TRUE,
                                          accept=c('pdf')),
                               # uiOutput("print_name_article"),
                                tags$hr(),
                                h4("Upload Metadata"),
                                fileInput('file.metadata', 'Choose CSV File', multiple=FALSE,
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
                                # tags$hr(),
                                checkboxInput('header', 'Header', TRUE),
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                tags$hr(),
                                h4("Upload Stopword List"),
                                fileInput('stopwords.txt', 'Upload stopwords in txt format (one word per line)',multiple=FALSE,
                                          accept=c('txt')),
                                br()
                                # tags$hr(),
                                # h4("Upload transcripts"),
                                #  fileInput('file.transcript.txt', 'Choose File(s) in txt format', multiple=TRUE,
                                #         accept=c('txt')),
                                # uiOutput("print_name_transcript_txt"),
                                # fileInput('file.transcript.pdf', 'Choose File(s) in pdf format', multiple=TRUE,
                                #          accept=c('pdf')),
                                # uiOutput("print_name_transcript_pdf")
                              ),
                              mainPanel(   
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"),
                                img(src = "itmslogo.jpg", height = 55, width = 80,style="float:right; padding-right:25px"), 
                                tabsetPanel(type = "tabs", 
                                            tabPanel("Data",
                                                     br(),
                                                     tabsetPanel(title="Data",
                                                                 tabPanel("Upload Instructions", 
                                                                          fluidPage(
                                                                            shiny::tags$head(shiny::tags$style(shiny::HTML(
                                                                              "#text { font-size: 15px; height: 300px; overflow: auto; }"
                                                                            ))),
                                                                            fluidRow( 
                                                                              h5("Step One: Upload files on the left panel (pdf or text format)."),
                                                                              p("Single file or multiple files"),
                                                                              h5("Step Two: Go Data Structure to specify uploaded format"),
                                                                              p("Available formats: texts (novels, transcripts, etc.), scientific articles (two-column is currently not suported)"),
                                                                              h5("Step Three: Go Data Subset to choose a specific content"),
                                                                              p("Content: abstract, search terms and text length"),
                                                                              br()))),
                                                                 tabPanel("Data Structure",
                                                                          helpText("Specify Type of your data: text or article"),
                                                                          uiOutput("choose_type"),
                                                                          br(),
                                                                         # uiOutput("choose_column"),
                                                                          helpText("Corpus Size"),
                                                                          uiOutput("print_name_article"),
                                                                          uiOutput("print_name_article_txt"),
                                                                         uiOutput("print_length"),
                                                                          br()
                                                                 ),
                                                                 tabPanel("Data Subset",
                                                                          helpText("Select By Term to extract subset based on a word; select By Speaker to extract lines from a transcript"),
                                                                          helpText("Keep NULL to preserve the full text"),
                                                                          uiOutput("choose_article_content"),
                                                                          br(),
                                                                          uiOutput("choose_intro"),
                                                                          uiOutput("choose_term"),
                                                                          uiOutput("choose_length"),
                                                                          uiOutput("term_print"),
                                                                          #helpText("Select the title/name of speaker in transcripts"),
                                                                          uiOutput("choose_speaker"),
                                                                          #helpText("Select to remove interviewer dialogue from transcripts"),
                                                                          uiOutput("choose_interviewer"),
                                                                          br()
                                                                 ),
                                                                 tabPanel("Viewer", 
                                                                          helpText("Select Display option. 
                                                                                   You will have a scroll panel with all your documents."),
                                                                         # h5("Display Content:"),
                                                                          uiOutput("select_display_content"),
                                                                          div(id = "text", uiOutput("print_content"))
                                                                 ))),
                                                                 #          fluidPage(
                                                                 #            shiny::tags$head(shiny::tags$style(shiny::HTML(
                                                                 #              "#text { font-size: 15px; height: 300px; overflow: auto; }"
                                                                 #            ))),
                                                                 #            fluidRow( 
                                                                 #              h5("Step One: Upload files on the left panel (pdf or text format)."),
                                                                 #              
                                                                 #              h5("Step Two: Select full text or abstract on the right panel."),
                                                                 #              p("Some pdf formats may produce errors during conversion, to avoid this issue upload a txt format."),
                                                                 #              br(),
                                                                 #              p("Select Abstract, Full Text or Transcript (if applicable)"),
                                                                 #              column(4,
                                                                 #                     hr(),  
                                                                 #                     #helpText("Select abstract."),
                                                                 #                     uiOutput("choose_article_content")                                                  
                                                                 #              ),
                                                                 #              column(4,
                                                                 #                     uiOutput("replace_what"),
                                                                 #                     uiOutput("replace_by")),
                                                                 #              column(4,
                                                                 #                     uiOutput("choose_speaker"),
                                                                 #                     uiOutput("choose_interviewer")),
                                                                 #              column(12,
                                                                 #                     hr(), 
                                                                 #                     helpText("If you wish to have a closer look at your documents' 
                                                                 #         content, you can select Display option. 
                                                                 #         You will have a scroll panel with all your documents."),
                                                                 #                     h5("Display Content:"),
                                                                 #                     uiOutput("select_display_content"),
                                                                 #                     div(id = "text", uiOutput("print_content"))
                                                                 #              )
                                                                 #            ))))),
                                                                 # #  ),
                                                                 tabPanel("Data Cleaning",
                                                                          tabsetPanel(title="Data",
                                                                                      tabPanel("Remove",
                                                                                               fluidPage(p("If you document contains html tags, urls or bibliography, you can remove them from your data. Leave the default `Do not Remove'."),
                                                                                                         fluidRow( 
                                                                                                           column(4,uiOutput("choose_html"),
                                                                                                                  helpText("Check Processed Content in Data Content Tab, if any html or xml symbols remain, add them here:"),
                                                                                                                  uiOutput("remove_html_symbol")
                                                                                                           ),
                                                                                                           column(4,uiOutput("choose_references")),
                                                                                                           column(4,uiOutput("choose_urls"))))),
                                                                                      tabPanel("Replace",
                                                                                          uiOutput("replace_what"),
                                                                                          uiOutput("replace_by")),
                                                                                      tabPanel("Text Preprocessing",
                                                                                               fluidRow( p("You can choose whether to remove punctuation, numbers or/and lower cases."),
                                                                                                         column(4, uiOutput("choose_remove_numbers")),
                                                                                                         column(4,uiOutput("choose_lower_cases")),
                                                                                                         column(4, p("Remove punctuation and specify exceptions if needed"),
                                                                                                                uiOutput("choose_remove_punctuation"),
                                                                                                                uiOutput("exceptions")))),
                                                                                      tabPanel("Viewer", 
                                                                                               helpText("Select Display option. 
                                                                                                        You will have a scroll panel with all your documents."),
                                                                                               # h5("Display Content:"),
                                                                                               uiOutput("select_processed"),
                                                                                               div(id = "text", uiOutput("print_processed")))
                                                                                      )),
                                                                          #   p("If you document contains html tags, urls or bibliography, you can remove them from your data. Leave the default `Do not Remove'."),
                                                                          # fluidPage(p("If you document contains html tags, urls or bibliography, you can remove them from your data. Leave the default `Do not Remove'."),
                                                                          #           fluidRow( 
                                                                          #             column(4,uiOutput("choose_html"),
                                                                          #                    helpText("Check Processed Content in Data Content Tab, if any html or xml symbols remain, add them here:"),
                                                                          #                    uiOutput("remove_html_symbol")
                                                                          #             ),
                                                                          #             column(4,uiOutput("choose_references")),
                                                                          #             column(4,uiOutput("choose_urls"))),
                                                                          #           fluidRow( p("You can choose whether to remove punctuation, numbers or/and lower cases in your documents."),
                                                                          #                     column(4, uiOutput("choose_remove_numbers")),
                                                                          #                     column(4,uiOutput("choose_lower_cases")),
                                                                          #                     column(4, p("Remove punctuation and specify exceptions if needed"),
                                                                          #                            uiOutput("choose_remove_punctuation"),
                                                                          #                            uiOutput("exceptions"))
                                                                          #           ))))),                                           
                                                                 tabPanel("Stop Words",
                                                                          p("List of stopwords in English and other languages
                                                   can be found in the following websites: 1)", tags$a(href="http://www.ranks.nl/stopwords", "http://www.ranks.nl/stopwords"),
                                                                            "2) ",  tags$a(href="http://www.lextek.com/manuals/onix/stopwords1.html","http://www.lextek.com/manuals/onix/stopwords1.html"), " 3) ", 
                                                                            tags$a(href="http://www.webconfs.com/stop-words.php","http://www.webconfs.com/stop-words.php"),
                                                                            "3) Extensive stopword list with proper names from Matthew Jockers ",tags$a(href="http://www.matthewjockers.net/macroanalysisbook/
expanded-stopwords-list/", "http://www.matthewjockers.net/macroanalysisbook/expanded-stopwords-list/"),". Words must be one per line."),
                                                                          p("Default is the list from tm package: stopwords(`SMART')"),
                                                                          h5("Upload a stopword list (left panel)"),
                                                                          fluidPage(
                                                                            fluidRow( 
                                                                              column(5,uiOutput("stops")),
                                                                              column(5,uiOutput("show_stopwords"))),
                                                                            fluidRow( 
                                                                              column(12,verbatimTextOutput("print_stopwords"))),
                                                                            fluidRow( 
                                                                              column(4,uiOutput("show_cutoff_lower")),
                                                                              column(4,uiOutput("show_cutoff_high"))),
                                                                            fluidRow(   p("In addition to stopwords list you can manually remove words below:"),
                                                                                        helpText("Type words to be removed and click add"),#,
                                                                                        column(4, uiOutput("choose_remove")),#)
                                                                                        column(12, uiOutput("printWords"))
                                                                                        
                                                                            ))),
                                                                 tabPanel("Stemming",
                                                                          uiOutput("choose_stem")),
                                                                 tabPanel("Metadata",
                                                                          p("Metadata can consists of Ids, Year, Title, Author, Category, Location. 
You can upload your metadata in csv format (see a template). 
                                                     If your pdf files contain matadata - it will be printed here."),
                                                                          hr(), 
                                                                          
                                                                          h5("Metadata from csv file (left panel): Id, Year, Title(name), Author, Category"),
                                                                          uiOutput("load_metadata_csv"), 
                                                                          dataTableOutput("print_metadata_csv"),
                                                                          hr(), 
                                                                          h5("Select metadata from pdf files (it may not work for all pdf files)"),
                                                                          uiOutput("load_metadata_pdf"),                                                              
                                                                          tableOutput("print_metadata_pdf") 
                                                                 ),
                                            tabPanel("Viewer", 
                                                     helpText("Select Display option"))
                                            )))), 
                              #    tabPanel("Transcripts",
                              #             fluidPage(
                              #             p("This section aims to help sociolinguists to clean their transcript data: removing html/xml tags, comments and extracting speaker data. 
                              #               The files can be downloaded in a text format for further analysis."),
                              # h5("Upload transcript file(s) on the left panel"),
                              #   fluidRow( 
                              #     column(4#,
                              #           # uiOutput("choose_speaker")
                              #            ),
                              #     column(4#,
                              #    # uiOutput("choose_speaker"),
                              #    # uiOutput("choose_name")
                              #    ),
                              #     column(8,
                              #             uiOutput("show_transcript"),
                              #             downloadButton("downloadTranscript", "Download"),
                              #             uiOutput("transcript")))))
                              # )))),
                              #       )))
                              #     )),
                              tabPanel(tags$h3("Data Visualization"), 
                                       img(src = "itmslogo.jpg", height = 55, width = 80,style="float:right; padding-right:25px"),
                                       
                                       tabsetPanel(type = "tabs", 
                                                   tabPanel("Word Frequency",
                                                            tabsetPanel(type = "tabs", 
                                                                        tabPanel("Frequency Table",
                                                                                 fluidPage( p("Frequency and Zipf's/Heap's laws to examine characteristics of word distribution."),
                                                                                            fluidRow( p("Word frequency before or after removing stopwords"),
                                                                                                      column(8, uiOutput("show_freq"),
                                                                                                             # hr(),
                                                                                                             dataTableOutput("freq")),
                                                                                                      column(4, p("Zipf plot - tm package"),
                                                                                                             plotOutput("zipf"),
                                                                                                             p("Heaps plot - tm package"),
                                                                                                             plotOutput("heaps")
                                                                                                      ))
                                                                                 )),
                                                                        tabPanel("Frequency Visualization",  
                                                                                 fluidPage(
                                                                                   fluidRow(
                                                                                     column(8, 
                                                                                            uiOutput("choose_cloud"),
                                                                                            uiOutput("choose_top"),
                                                                                            
                                                                                            plotOutput("word_count")),
                                                                                     #hr(),
                                                                                     column(5,
                                                                                            p("Set the minimum frequency for your cloud visualization"),
                                                                                            uiOutput("choose_min_frequency")),
                                                                                     column(5,p("Set the maximum words per plot"),
                                                                                            uiOutput("choose_max_words")),
                                                                                     column(12, plotOutput("print_cloud"))#,
                                                                                   ))),
                                                                        tabPanel("Length",
                                                                                 fluidPage( p("qdap package: word_length and sentSplit"),
                                                                                            fluidRow( p("Default sentence split is based on qdap sentSplit (? ! . |)"),
                                                                                                      column(4, uiOutput("show_word")),
                                                                                                      column(4, uiOutput("choose_text"))
                                                                                            ),
                                                                                            fluidRow( column(3,   #verbatimTextOutput("length_table"),     
                                                                                                             tableOutput("length_word_table")),
                                                                                                      column(9, #uiOutput("show_sentence"),
                                                                                                             # plotOutput("sentences")
                                                                                                             plotOutput("words")
                                                                                                      )),
                                                                                            fluidRow( p("For more information about collocations, see Stefan Gries"),
                                                                                                      column(4, uiOutput("show_collocation"),
                                                                                                             # hr(),
                                                                                                             plotOutput("collocation")),
                                                                                                      column(4, p("Under construction")
                                                                                                      ))    
                                                                                 )))),                                
                                                   tabPanel("Topic Modeling",
                                                            tabsetPanel(type = "tabs", 
                                                                        tabPanel("Model Creation", 
                                                                                 p("This analysis is based on lda, topicmodels  and stm packages. For lda package - collapsed.gibbs.sampler is used, 
                                                             where the number of topics, iteration, alpha and eta values can be adjusted. By default, num of topics = 3; iteration = 500, alpha = 0.02, eta = 0.02",
                                                                                   tags$a("http://search.r-project.org/library/lda/html/rtm.collapsed.gibbs.sampler.html","lda package")),
                                                                                 fluidPage(
                                                                                   fluidRow(
                                                                                     column(5,
                                                                                            h5("Topic Selection:"),
                                                                                            hr(), 
                                                                                            helpText("Select number of topics - an integer representing the number of topics in the model. Default is 3."),
                                                                                            #  uiOutput("choose_stem"),
                                                                                            uiOutput("choose_topic_num"),
                                                                                            helpText("Select the top number of words associated with a given topic. Default is 3."),
                                                                                            uiOutput("choose_word_num")),   
                                                                                     column(5,
                                                                                            helpText("The number of sweeps of Gibbs sampling over the entire corpus to make. Default is 500"),
                                                                                            uiOutput("iter"),
                                                                                            helpText("Select alpha. Default is 0.02"),
                                                                                            uiOutput("alpha"),
                                                                                            helpText("Select eta. Default is 0.02"),
                                                                                            uiOutput("eta")
                                                                                     ),       
                                                                                     column(12,         
                                                                                            p("To determine the best number of topics, run Best Topic Number analysis based on Likelihood Log. set.seed(2013)"),
                                                                                            p("It may take a while to run this analysis depending 
                                                             on the number of documents and their size. Meanwhile you make explore 
                                                             your articles with frequency and cluster analyses."),#,tags$a("http://search.r-project.org/library/lda/html/rtm.collapsed.gibbs.sampler.html","lda package"),"set.seed(2013)"),
                                                                                            h5("Best Topic Number:"),
                                                                                            hr(), 
                                                                                            helpText("Run this analysis to determine the 
                                                                                  best number of topics to use."),
                                                                                            uiOutput("best_topic_num"),
                                                                                            tableOutput("best_k"),
                                                                                            plotOutput("best_k_plot"))                                                                                                        
                                                                                     #    helpText("Selected Topics LDA (lda.collapsed.gibbs.sampler from lda package)"),
                                                                                     # )
                                                                                   ))),
                                                                        tabPanel("LDA visualization",                                                         
                                                                                 fluidPage(                                     
                                                                                   fluidRow( uiOutput("choose_lda"),
                                                                                             column(7,  
                                                                                                    helpText("Selected Topics LDA (lda.collapsed.gibbs.sampler from lda package)"),
                                                                                                    tableOutput("topics"),
                                                                                                    #  tableOutput("lda_topic"),
                                                                                                    helpText("Plotting Documents and Topics Relations"),
                                                                                                    plotOutput("printCoordinates"),
                                                                                                    uiOutput("docsNames"),
                                                                                                    verbatimTextOutput("docs")
                                                                                             )
                                                                                   ))
                                                                        ),
                                                                        tabPanel("STM Visualization",
                                                                                 fluidPage(
                                                                                   fluidRow( p("STM is a structural topic modeling."),
                                                                                             column(8,  
                                                                                                    uiOutput("choose_stm"),             
                                                                                                    
                                                                                                    hr(),
                                                                                                    #verbatimTextOutput("docs"),
                                                                                                    helpText("Structural Topics STM"),
                                                                                                    plotOutput("perspectives")
                                                                                             ),
                                                                                             column(12,
                                                                                                    hr(), 
                                                                                                    h5("Structural Topic Modeling"),
                                                                                                    helpText("package stm with init LDA"),
                                                                                                    verbatimTextOutput("topics_stm"),
                                                                                                    h5("Proportion of Topics in Documents"),
                                                                                                    plotOutput("proportion"),
                                                                                                    h5("Most Common Topic Terms"),
                                                                                                    plotOutput("cloud_stm"),
                                                                                                    #plotOutput("perspectives"),
                                                                                                    h5("Correlation Plot"),
                                                                                                    plotOutput("corelation")
                                                                                             )  
                                                                                             
                                                                                             
                                                                                   ))), 
                                                                        
                                                                        tabPanel("Chronological Topic Visualization",
                                                                                 fluidPage( p("This analysis requires metadata - please load or extract metadata in Data Preparation Panel."),
                                                                                            fluidRow(
                                                                                              column(12,
                                                                                                     h5("Topic Modeling with topicmodels package:"),
                                                                                                     uiOutput("choose_chronology"),
                                                                                                     tableOutput("chronology_table"),
                                                                                                     verbatimTextOutput("chronology_top"),
                                                                                                     plotOutput("chronology_plot")
                                                                                              ))))
                                                            )),
tabPanel("Cluster Analysis",
         p("Cluster Analysis (Descriptive Statistics) examines 
                                                       how variables or individuals are grouped. The visual 
           representation is often referred to as a dendrogram,  
           as groups are clustered into tree branches"),
         p("Under Development"),
         uiOutput("choose_cluster"),
             uiOutput("cuttree"),
            uiOutput("color"),
         plotOutput("cluster_plot")),
tabPanel("Punctuation Analysis",
                                                              fluidPage(                        
                                                                p("This section is based on the punctuation analysis by Adam Calhoun's ",tags$a(href="https://medium.com/@neuroecology/punctuation-in-novels-8f316d542ec4","Blog"),". 
                                                           In his heatmap visualization, periods and question marks and exclamation marks are red.
                                                           Commas and quotation marks are green. 
                                                                  Semicolons and colons are blue"),
                                                                uiOutput("choose_punctuation"),
                                                                fluidRow(
                                                                  column(4,
                                                                         hr(),
                                                                         helpText("Punctuation Frequency"),
                                                                         plotOutput("type_punctuation")
                                                                     
                                                                  ),
                                                                  column(4,
                                                                         hr(),
                                                                         helpText("Punctuation Map"),
                                                                         plotOutput("heatmap_punct"))
                                                                ))),
                                                     tabPanel("Name Entity Analysis",
                                                              p("Coming in the future")),
                                                     tabPanel("Dynamic Plots",
                                                              p("Coming in the future"))
                                )
                            ),
             
                   tabPanel(tags$h3("References"),
                            img(src = "itmslogo.jpg", height = 55, width = 80,style="float:right; padding-right:25px"),
                            h4("Bibliography"),
                            p("Blei D.M., Ng A.Y., Jordan M.I. (2003). Latent Dirichlet Allocation. Journal of Machine Learning Research, 3, 993–1022"),
                            p("Casey Trey. 2013. Top Modeling Demo. http://faculty.washington.edu/jwilker/559/Notebooks/LDA_presentation.R"),
                            p("Cookbook for R - http://www.cookbook-r.com/"),
                            p("Griffiths, T.L., and M. Steyvers 2004. Finding scientific topics. Proceedings 
                                                        of the National Academy of Sciences of the United 
                                                        States of America 101(Suppl 1):5228-5235"),
                            p("Jockers, Matthew. 2014. Text Analysis with R for Students of Literature. Springer."),
                            p("Roberts, M., Stewart, B., Tingley, D., and Airoldi, E. (2013) The structural topic model and applied social science. In Advances in Neural Information Processing Systems 
                              Workshop on Topic Models: Computation, Application, and Evaluation. http://goo.gl/uHkXAQ"),
                            p("Taylor, Arnold and Lauren Tilton. 2015.Humanities Data in R. Springer."),
                            p("Williams, Graham. Hands-On Data Scrience with R Text Mining. Available from http://onepager.togaware.com/TextMiningO.pdf"),

                            h4("R Packages:"),
                            p("Hornik K, Grun B (2011). topicmodels: An R package for Fitting Topic Models.” Journal of
Statistical Software, 40(13), 1–30."),
                            p("Fellows I (2014). wordcloud: Word Clouds. R package version 2.5, URL http://CRAN.
R-project.org/package=wordcloud"),
                            p("Goodrich, Bryan, Dason Kurkiewicz and Tyler Rinker. 2015. gdap package. 
                              Bridging the Gap Between Qualitative Data and Quantitative Analysis"),
                            p("ggplot2, topicmodeling, tm, qdap, stringr, 
                            wordcloud, RColorBrewer, graphics, conf.design, fields,
                            stringi, lda, ca, dplyr, plyr, tidyr, ggplot2,stm"),
                            p("Image-Matrix - http://www.phaget4.org/R/image_matrix.html"),
                            p("Stopwords for Shakespeare from github - 
                              https://github.com/m0neysha/shakespeare-analysis/blob/master/stopwords.txt")
                   )
                   
                   
)
)



