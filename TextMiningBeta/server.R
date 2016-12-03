library(shiny)
source("myImagePlot.R")
source("multiplot.R")
source("MyPlot.R")
source("extractContent.R")
source("extractMetadata.R")
source("abstract.R")
source("words.R")
source("preprocess.R")
source("kwic.R")
source("parseJSON.R")
library(tm)
library(qdap)
library(qdapRegex)
library(stringr)
library(wordcloud)
library(topicmodels)
library(RColorBrewer)
library(graphics)
library(conf.design)
library(fields)
library(slam)
library(reshape2)
library(stm)
library(stringi)
library(lda)
library(ca)
library(plyr)
library(dplyr)
library(RcppArmadillo)
library(tidyr)
library(ggplot2)
library(RTextTools)
library(SnowballC)
library(jsonlite)
#library(rjson)
library(XML)
library(ggthemes)
shinyServer(function(input, output) {
  
  output$print_name_article <- renderPrint({
    if (is.null(input$file.article)) { return()}
    paste(input$file.article$name, sep="\n")
  }) 
  output$print_name_article_txt <- renderPrint({
    if (is.null(input$file.article.txt)) { return()}
    paste(input$file.article.txt$name, sep="\n")
  }) 
output$print_length_pdf <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml)))   { return() } 
  HTML(paste("Corpus Size Total: ", ListTerms()$len, sep=" ", collapse="<br/>"))
})
output$print_length_txt <- renderUI({
  if  (is.null(input$file.article.txt)) { return() }
  HTML(paste("Corpus Size Total: ", ListTerms()$len, sep=" ", collapse="<br/>"))
})

output$place_for_structured_data_browser <- renderUI ({
  switch (input$structured_data_file_source,
          "JSON"= fileInput('structured_data_file_json', 'Choose JSON File', multiple=FALSE, accept=c('application/json',',JSON')),
          "XML" = fileInput('structured_data_file_xml', 'Choose XML File', multiple=FALSE, accept=c('application/xml','text/xml','.xml'))
  )
})

## Reading Structured Data
structured_data <- reactive({ # loading data
  my_data = NULL
  if( !is.null(input$structured_data_file_json) ) {
    my_data <- fromJSON(input$structured_data_file_json$datapath, flatten = TRUE)
  }
  else if( !is.null(input$structured_data_file_xml) ) {
    my_data <- xmlToDataFrame(input$structured_data_file_xml$datapath)
  }
  return(my_data)
})

### Display Structured Data
output$place_for_structured_data <- renderDataTable({
  my_data = structured_data()
  if ( is.null(my_data) )  { return() }
  my_data
}) 

  ExtractRawContentPDF <- reactive ({
    if (is.null(input$file.article)) { return() }
   extractContentPdf(input$file.article)
  })
  
  output$choose_term <- renderUI({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml)))   { return() } 
   # word <- ListTerms()$d[[2]]#[1:x]
    word <- RemoveWordsStepOne()$d[[2]]
    selectizeInput("choose_term", label = "Type term", 
                   choices = word,
                   options = list(create = TRUE),
                   selected = NULL,
                   multiple = TRUE) 
  })   

  output$choose_length <- renderUI({
   # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml)))   { return() } 
   # d <- ListTerms()$d
    selectizeInput("len",
                "Context Length:",
                choices = c(1,2,3,4,5,6,7,8,9,10),
                options = list(create = TRUE),
                selected=3,
                multiple = FALSE) 
             #   min = min(d$freq),
             #   max = max(d$freq),
             #   value = min(d$freq)+2)             
  })
  
  ListTerms <-reactive({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml)))   { return() } 
    
    if(!is.null(input$file.article)) {
      corpus.lda <- ExtractRawContentPDF()#Selection()#$text.extract
    }
    if(!is.null(input$file.article.txt))  {
      corpus.lda <- ExtractRawContentTXT()#extractContent(input$file.article.txt) 
    }
    listTerms(corpus.lda)
    })  
  
  output$term_print <- renderUI ({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
    len <- input$len
    term <- input$choose_term
    HTML(paste("Your Term: ", term, "Your Context Length: ", len,  sep=" ", collapse="\n"))
  })
  
  Abstract <- reactive ({
    if (is.null(input$file.article))  { return() } 
    if (input$article_content=="Abstract") {
      extractAbstract(x=ExtractRawContentPDF(), y=input$file.article)
    }
  })
  
#Metadata Extracting
  metadataPdf <- reactive ({
    if (is.null(input$file.article)) { return() }
    x=input$file.article
    extractMetadata(x)
  })

## Reading Metadata File from csv

  output$place_for_file_browser <- renderUI({
  #  if( input$metadata_source == "None") {return()}
    if( input$metadata_source == "PDF") {
      metadata_file_name = input$file.article.name#NULL;
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
    else if( !is.null(input$file.article)) {
      a <- metadataPdf()$authors
      t <- metadataPdf()$titles
      dt <- metadataPdf()$datetimes
      my_data <- data.frame(date=dt, title=t, author = a)
     # my_data <- data.frame(metadataPdf()$metapdf)
    }
    else if( !is.null(input$structured_data_file_json) ) {
      parsed_json <- parseJSON(structured_data())
      my_data <- data.frame(date=parsed_json$dates, title=parsed_json$titles, author = parsed_json$authors)
    }
    return(my_data)
  })
  ### Display Metadata from CSV
  output$place_for_metadata_table <- renderDataTable({
    data_from_metadata_file = fileData()
    if ( is.null(data_from_metadata_file) )  { return() }
    if (input$metadata_source=="None")  { return() }
    data_from_metadata_file
  }) 

output$print_content_txt <- renderUI({
  if (is.null(input$file.article.txt))  { return() }
      txt.lines <-  ExtractRawContentTXT()
      withProgress(message = 'Loading txt file',
                   detail = '....', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.25)
                     }
                   })
      HTML(paste("<br/>", "Document: ",txt.lines, sep="<br/>"))
 

}) 

output$print_content_pdf <- renderUI({
  if (is.null(input$file.article))  { return() }
  pdf.lines <-  ExtractRawContentPDF()
  withProgress(message = 'Loading pdf file',
               detail = '....', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  HTML(paste("<br/>", "Document: ",pdf.lines, sep="<br/>"))
}) 

output$print_abstract <- renderUI({
  if (is.null(input$file.article))   { return() }
  if (input$article_content=="Full Text") {
    pdf.abstract="Abstract is not selected"
  }
  else {
    pdf.abstract <- Abstract()
  }
  HTML(paste("<br/>", "Document: ",pdf.abstract, sep="<br/>"))
}) 

output$print_preprocessed <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if (input$preprocessing=="No Changes") {
    pdf.lines <- "No pre-processing steps are applied"
  }
  else if (input$preprocessing=="Apply Steps") {
    withProgress(message = 'Preprocessing',
                 detail = 'Almost done...', value = 0, {
                   for (i in 1:60) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
      pdf.lines <- PreprocessingSteps()[6]#$lda.format#window.one()$lda.format
      pdf.lines <- unlist(pdf.lines)
    }
  HTML(paste("<br/>", "Document: ",pdf.lines, sep="<br/>"))
}) 


########
### Pre-Processing
PreprocessingSteps <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if(!is.null(input$file.article)) {
   if(input$article_content=="Full Text"){
    x <-ExtractRawContentPDF()
    }
   else if(input$article_content=="Abstract"){
      x <-Abstract()
    } 
    y <-input$file.article
  }
  if(!is.null(input$file.article.txt)) {
   x<- ExtractRawContentTXT()
   y <-input$file.article.txt
  }
  if(!is.null(input$structured_data_file_json)) {
    parsed_json = parseJSON(structured_data())
    x <- parsed_json$corpus
    y <- parsed_json$titles
  }
#  num<-length(file.names)
 # file.names <-x
  remove_urls <-input$remove_urls
  remove_references<-input$remove_references
  remove_html<-input$remove_html
  lower_case<-input$lower_case
  remove_numbers<-input$remove_numbers
  exceptions<-input$exceptions
  remove_punctuation <- input$remove_punctuation
  mylist<-tokenize(x,y,remove_urls,remove_references,remove_punctuation,
           remove_html,lower_case,remove_numbers,exceptions)
 # mylist<-tokenize(text,file.names,num,remove_urls,remove_references,remove_punctuation,
  #              remove_html,lower_case,remove_numbers,exceptions)
 return(mylist)
  })

##### STOP WORDS #######

stopWordsTxt <- reactive ({
  stop_words<-vector()
  if (input$stops=="Default") {
    stop_words <- stopwords("SMART")
  }
  if (input$stops=="Upload") {
    uris.name <- input$stopwords.txt$datapath
    text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)
    x=enc2utf8(text.scan)
    text.punct <- str_c(x)#text.punct)
    text.punct<- str_trim(text.punct)
    text.punct <- gsub("\\s\\s+", " ", text.punct)   
    stops<-strsplit(text.punct, " ")
    stop_words<-unlist(stops)
  }
  return(stop_words)
})

output$print_stopwords <- renderPrint({
  if (input$stops=="None") {"Stopwords are not selected"}
 else{ 
   stopWordsTxt()}
})

#Allows for interactive instant word removals from user
RemoveWordsStepOne <-reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  #cutoff.lower=0#input$cutoff_lower
  #cutoff.high=input$cutoff_high
  mycorpus <- PreprocessingSteps()[6]
  if  (input$stops=="None") {    
    doc.vect <- VectorSource(mycorpus)
  corpus.tm <-Corpus(doc.vect)
  corpus.tm <- tm_map(corpus.tm, stripWhitespace)
    corpus <- list()
    for (i in 1:length(corpus.tm)) {
      doc <-corpus.tm[[i]]$content
      corpus[[i]] <- doc
    }
    lda.corpus <- corpus
   # corpus <- mycorpus
    corpus <- unlist(corpus)
    corpus.paste <-paste(mycorpus, sep=" ")
    corpus.paste <-paste(corpus, sep=" ")
    corpus.paste <- str_c(corpus.paste)
    corpus.paste<- str_trim(corpus.paste)
    corpus.list <- strsplit(corpus.paste, "\\s+")
    terms <- table(unlist(corpus.list))
    terms.sorted <- sort(terms, decreasing = TRUE)
    terms.matrix<-as.matrix(terms.sorted)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    d <- d[order(d$frequency, decreasing = T), ]
   # words.list <- as.list(d$word)

  }
  else if  ((input$stops=="Default")||(input$stops=="Upload")) {
    remove_word <- stopWordsTxt()
    doc.vect <- VectorSource(mycorpus)
    corpus.tm <-Corpus(doc.vect)
    corpus.tm <- tm_map(corpus.tm,removeWords,stopWordsTxt())
    corpus.tm <- tm_map(corpus.tm, stripWhitespace)
    corpus <- list()
    for (i in 1:length(corpus.tm)) {
      doc <-corpus.tm[[i]]$content
      corpus[[i]] <- doc
    }
    lda.corpus <- corpus
    corpus <-unlist(corpus)
   # corpus.paste <-paste(mycorpus, sep=" ")
    corpus.paste <-paste(corpus, sep=" ")
    corpus.paste <- str_c(corpus.paste)
    corpus.paste<- str_trim(corpus.paste)
    corpus.list <- strsplit(corpus.paste, "\\s+")
    terms <- table(unlist(corpus.list))
     remove_word <- stopWordsTxt()
     del <- names(terms) %in% remove_word #| terms < cutoff.lower
     terms <- terms[!del]
    terms.sorted <- sort(terms, decreasing = TRUE)
    terms.matrix<-as.matrix(terms.sorted)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    d <- d[order(d$frequency, decreasing = T), ]
   # words.list <- as.list(d$word)
  }
  info <- list(corpus=corpus,d=d,lda.corpus=lda.corpus)
  return(info)
})

RemoveWordsStepTwo <-reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if (is.null(input$remove_words)) {
    corpus <-RemoveWordsStepOne()$corpus
    lda.corpus<-RemoveWordsStepOne()$lda.corpus
    d <-RemoveWordsStepOne()$d
   # words.list <-RemoveWordsStepOne()$words.list
  }
  else {
    mycorpus <-RemoveWordsStepOne()$corpus
    doc.vect <- VectorSource(mycorpus)
    corpus.tm <-Corpus(doc.vect)
   # corpus.tm <- removeWords(corpus.tm, c(input$remove_words))
    corpus.tm <- tm_map(corpus.tm,removeWords,c(input$remove_words))
    corpus.tm <- tm_map(corpus.tm, stripWhitespace)
    corpus <- list()
    for (i in 1:length(corpus.tm)) {
      doc <-corpus.tm[[i]]$content
      corpus[[i]] <- doc
    }
   # corpus <-unlist(corpus)
    corpus.paste <-paste(corpus, sep=" ")
    corpus.paste <- str_c(corpus.paste)
    corpus.paste<- str_trim(corpus.paste)
    corpus.list <- strsplit(corpus.paste, "\\s+")
    terms <- table(unlist(corpus.list))
    terms.sorted <- sort(terms, decreasing = TRUE)
   # remove_word <- stopWordsTxt()
   # del <- names(terms) %in% remove_word #| terms < cutoff.lower
   # terms <- terms[!del]
    terms.matrix<-as.matrix(terms)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    d <- d[order(d$frequency, decreasing = T), ] 
    #words.list <- as.list(d$word)
  }
  info <- list(corpus=corpus,d=d)#,words.list=words.list)
  return(info)
})

RemoveWordsStepThree <-reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  corpus <- stemming()
  doc.vect <- VectorSource(corpus)
  docs <-Corpus(doc.vect)
  tdm <- TermDocumentMatrix(docs)
  dtm <- DocumentTermMatrix(docs)
  term.matrix <- as.matrix(tdm)
  if(!is.null(input$file.article)) {
    file.names <-input$file.article$name
  }
  if(!is.null(input$file.article.txt))  {
    file.names <-input$file.article.txt$name
  }
  colnames(term.matrix) <- file.names
 # if(input$stopwords=="None") {
  #  corpus <-RemoveWordsStepOne()$corpus
  corpus.paste <-paste(corpus, sep=" ")
  corpus.paste <- str_c(corpus.paste)
  corpus.paste<- str_trim(corpus.paste)
  corpus.list <- strsplit(corpus.paste, "\\s+")
  terms <- table(unlist(corpus.list))
  terms.sorted <- sort(terms, decreasing = TRUE)
  # remove_word <- stopWordsTxt()
  # del <- names(terms) %in% remove_word #| terms < cutoff.lower
  # terms <- terms[!del]
  terms.matrix<-as.matrix(terms)
  d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
  d$word <- row.names(d)
  agg_freq <- aggregate(frequency ~ word, data = d, sum)
  d <- d[order(d$frequency, decreasing = T), ]
  #  d <-RemoveWordsStepOne()$d
    
   # words.list <-RemoveWordsStepOne()$words.list
#  }
 # else if(input$stopwords=="Apply Stopwords") {
   # if ((input$stops=="None") && (input$remove==FALSE))  {
  #    corpus <-RemoveWordsStepOne()$corpus
   #   d <-RemoveWordsStepOne()$d
     # words.list <-RemoveWordsStepOne()$words.list
      
   # }
   # else {
 #     corpus <-RemoveWordsStepTwo()$corpus
 #     d <-RemoveWordsStepTwo()$d
     # words.list <-RemoveWordsStepTwo()$words.list
   # }
 # }
  info <- list(d=d,corpus=corpus,tdm=tdm,term.matrix=term.matrix,dtm=dtm)
  return(info)
})

output$print_apply_stops <- renderUI({
  if (input$stopwords=="None") {"No changes are made. You need to select stopwords first"}
  if (input$stopwords=="Apply Stopwords") {
    HTML(paste0(RemoveWordsStepThree()$corpus))
  }
})

stemming <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if(input$stopwords=="None") {
    corpus <-RemoveWordsStepOne()$corpus
   # d <-RemoveWordsStepOne()$d
  }
  else if(input$stopwords=="Apply Stopwords") {
    corpus <-RemoveWordsStepTwo()$corpus
 #   d <-RemoveWordsStepTwo()$d
  }
  
  if (input$language=="none") {
    corpus <-RemoveWordsStepTwo()$corpus
   # text.punct="Select language"
    }
  else {
   doc.vect <- VectorSource(corpus)
    docs <-Corpus(doc.vect)
    corpus <- list()
    for (i in 1:length(docs)) {
      doc <-docs[[i]]$content
      text.split <- unlist(strsplit(doc, " "))
      text.stem <- paste(wordStem(text.split, language = input$language),collapse = " ")
      text.stem <- str_c(text.stem)
      text.stem<- str_trim(text.stem)
      corpus[[i]] <- text.stem
    }
    corpus <-unlist(corpus)
    # corpus.tm <- removeWords(corpus.tm, c(input$remove_words))
   # docs <- tm_map(docs,stemDocument(docs,language = input$language))
   # docs <- tm_map(docs, stripWhitespace)
   # corpus.paste <-paste(corpus, sep=" ")
   # corpus.paste <- str_c(corpus.paste)
   # corpus.paste<- str_trim(corpus.paste)
      #   corpus.paste <-paste(corpus, sep=" ")
       #  text.split <- unlist(strsplit(corpus.paste, " "))
      #   text.punct <- paste(wordStem(text.split, language = input$language),collapse = " ")
  }
  return(corpus)
})

output$print_stemmer <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
    # if (is.null(input$remove_manual)) { return() }
    HTML(paste0(stemming()))
    # HTML(paste0(RemoveWordsStepTwo()$words.list))#PreprocessingSteps()$lda.format))# RemoveWordsNew()$corpus.lda))
  })
#########
rawFrequency <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if(!is.null(input$file.article)) {
    x <-ExtractRawContentPDF()
  }
  if(!is.null(input$file.article.txt)) {
    x<- ExtractRawContentTXT()
  }
  d <-frequencyTable(x)
  return(d)
})

output$freq <- renderDataTable ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  d<-rawFrequency()$dataf
  return(d)#,options=list(lengthMenu = c(5, 10, 15), pageLength = 5))
})

output$zipf <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  tdm <- ListTerms()$tdm
  dtm <- ListTerms()$dtm
  p<- Zipf_plot(dtm, type="l")
  return(p)
})

#heaps not working
#output$heaps <- renderPlot ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  #if((is.null(input$show_freq)) || (input$show_freq=="NULL")){ return() }
 # if (input$show_freq=="Frequency") {
 # dtm <-ListTerms()$dtm  #RemoveWords()$tdm
#})


output$choose_text <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if (!is.null(input$file.article)) {
    names <- input$file.article$name
  }
  if (!is.null(input$file.article.txt)) {
    names <- input$file.article.txt$name
  }
  selectizeInput("show_text", label = "Select which document to analyze", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE) 
  
})

LengthWord <- reactive ({
  if (input$show_word=="None") {return()}
  i <- input$show_text
if(!is.null(input$file.article.txt)) {
  names <- input$file.article.txt$name
  n <- which(names==i)
  vec <- ExtractRawContentTXT()[[n]]
}
  if (!is.null(input$file.article)) {
    names <- input$file.article$name
    n <- which(names==i)
    vec <- ExtractRawContentPDF()[[n]]
  }
  corpus <- Corpus(VectorSource(vec))
  dtm <- DocumentTermMatrix(corpus)
  m <- as.matrix(Terms(dtm),Docs(dtm))
  wl <- word_length(m[,1])
  wl <- word_length(m[,1])
  wldf <- wl$prop
  tab <- t(wldf[3:length(wldf)])
  dt <- as.table(tab)
  newtab <- as.data.frame(cbind(as.numeric(dt),as.numeric(rownames(tab))))
  colnames(newtab) <- c("proportion","word_length")
  return(newtab)
})
output$length_table <- renderPrint ({
  if (input$show_word=="None") {return()} 
tab <- LengthSent()
return(tab)
})
output$length_word_table <- renderTable ({
  if (input$show_word=="None") {return()} 

  if (input$show_word=="Word Length") {
   tab <- LengthWord()
  }
  if (input$show_word=="Sentence Length") {
    tab <- LengthSent()$newtab
  }
  withProgress(message = 'Length calculation',
               detail = 'in progress...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  return(tab)
  })
output$choose_bin <-renderUI({
    # bins <- seq(min(barPlot()[[1]]), max(barPlot()[[1]]), length.out = input$bins + 1)
    #  bw <- diff(barPlot()[[1]]) / (2 * IQR(barPlot()[[1]]) / length(barPlot()[[1]])^(1/3))
  selectizeInput("breaks",
                label = "Number of bins in histogram:",
               choices = c(10, 20, 35, 50,60,70,80,90,100),
                selected = 30)
}) 
output$words <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if (input$show_word=="None") {return()}
  if (input$show_word=="Word Length") {
  newtab <- LengthWord()
  plots <- ggplot(newtab, aes(x=word_length,y=proportion)) + geom_line() + ggtitle(input$show_text)
  }
  if (input$show_word=="Sentence Length") {
    breaks=input$breaks
    counts <- LengthSent()$counts
    plots <- hist(counts,main="Histogram of Sentence Length", col="grey",freq=TRUE,as.numeric(breaks))
  }
  return(plots)
})

LengthSent <- reactive ({
 # if (input$show_text=="None") {return()}
  i <- input$show_text
  if(!is.null(input$file.article.txt)) {
    names <- input$file.article.txt$name
    n <- which(names==i)
    data <- ExtractRawContentTXT()[[n]]
  }
  if (!is.null(input$file.article)) {
    names <- input$file.article$name
    n <- which(names==i)
    data <- ExtractRawContentPDF()[[n]]
  }
  data.punct <- gsub("([.?!;:])|[[:punct:]]","\\1",data)
  data.punct <- gsub("\\s+"," ",data.punct)
  vec <- as.data.frame(data.punct)
  colnames(vec) <- "Sentence"
  sent <- sentSplit(vec,"Sentence",endmarks = c("?","!",".",";",":"))
  sent$split <- strsplit(sent$Sentence," ")
  sent$count<-word_count(sent[,2])
  counts <-sent$count
  splits <-sent$split
  sen <- sent$Sentence
  t<- table(counts)
  newtab <- as.data.frame(t)
  colnames(newtab) <- c("sentence_length","frequency")
  info <- list(counts=counts,newtab=newtab)
  return(info)
})

ExtractRawContentTXT <- reactive ({
  if (is.null(input$file.article.txt)) { return() }
  extractContentTxt(input$file.article.txt)
})

#     text <- paste(data, collapse = " ")
#   if (input$article_content =="Abstract") {
#     text1 <- str_replace(text, "Abstract|ABSTRACT|abstract", "mystringreplacement")
#     text1 <- sub(".*mystringreplacement","",text1)
#   #  text1 <- sub(".*(Abstract|ABSTRACT|abstract)","",text)
#     text.punct<-sub("(1.)?.?Introduction.*","",text1)    
#     text.punct<- rm_white(text.punct)

#      if ((!is.null(input$speaker_name)) && (input$speaker_name!="NULL")) {
#       name.speaker <- input$speaker_name
#       name.interviewer <- input$interviewer_name
#       new.line <- gsub(name.speaker, "NEWLINE SPEAKER",text.punct)
#       new.line <- gsub(name.interviewer, "NEWLINE INTERVIEWER",new.line)
#       split.sub <- unlist(strsplit(new.line,"NEWLINE"))
#       speaker <-grep("SPEAKER", split.sub)
#       text.sub <- split.sub[speaker]
#       text.sub <- gsub("SPEAKER","",text.sub)
#       text.punct <- paste(text.sub, collapse = " ")
#      }

output$choose_top <- renderUI({
  selectizeInput("top", label = "Select a number for top frequent words (ex. 10 top frequent words)", 
                 choices = c(10,20,30,40,50,60,70,80,90,100),
                 options = list(create = TRUE),
                 selected=10,
                 multiple = FALSE) 
}) 

output$choose_remove <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  word <- RemoveWordsStepOne()$d[[2]]
  selectizeInput("remove_words", label = "Select words to be removed", 
                 choices = word,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = TRUE) 
 # }
  })

output$printWords <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
 # if (is.null(input$remove_manual)) { return() }
  HTML(paste0(RemoveWordsStepOne()$d[[2]]))
 # HTML(paste0(RemoveWordsStepTwo()$words.list))#PreprocessingSteps()$lda.format))# RemoveWordsNew()$corpus.lda))
})

output$word_count <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  x <- input$top
#  word <- RemoveWordsNew()$d[[2]][1:x]
#  frequency <- RemoveWordsNew()$d[[1]][1:x]
  word <-  RemoveWordsStepThree()$d[[2]][1:x]
 # word <- RemoveWordsFinal()$d[[2]][1:x]
  frequency <- RemoveWordsStepThree()$d[[1]][1:x]
  plot <-barplot(frequency, names.arg=word, las=2,cex.names=0.8)
  return(plot)
})

output$choose_min_frequency <- renderUI({
#  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
 # if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  #d <- RemoveWordsStepThree()$d
 # word <-  RemoveWordsStepThree()$d[[2]][1:x]
  # word <- RemoveWordsFinal()$d[[2]][1:x]
 # freq <- RemoveWordsStepThree()$d[[1]]
 # sliderInput
  selectizeInput("min",
              "Minimum Frequency:",
              choices = c(1,2,3,4,5,6,7,8,9,10),
              options = list(create = TRUE),
              selected=1,
              multiple = FALSE) 
            #  min = min(freq),
            #  max = max(freq),
           #   value = 1)  
            #  min = min(d$freq),
            #  max = max(d$freq),
            #  value = min(d$freq)+2)             
})

output$choose_max_words <- renderUI({
#  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  #if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
 # d <- RemoveWordsStepThree()$d
 # d <- RemoveWordsStepThree()$d
 # num <- nrow(d)
  #sliderInput
  selectizeInput("max",
              "Maximum Words per Plot:",
              choices = c(100,150,200),
              options = list(create = TRUE),
              selected=150,
              multiple = FALSE) 
            #  min = 1,
            #  max = num,
           #   value = 100)             
})

output$print_cloud <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
 # if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  # withProgress(message = 'Plotting in progress',
  #              detail = 'This may take a while...', value = 0, {
  #                for (i in 1:15) {
  #                  incProgress(1/15)
  #                  Sys.sleep(0.25)
  #                }
  #              })
  wordcloud_rep <- repeatable(wordcloud)
   font <- input$font

  if (input$pal=="black"){
    pal="black"
  } else if (input$pal=="green"){
  pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:2)]
  } else if (input$pal=="multi") {
  
    pal <- brewer.pal(8,"Dark2")
  }
   if (input$multicloud=="Word Cloud") {
   d <- RemoveWordsStepThree()$d
  wordcloud_rep(d$word, d$freq, scale=c(8,0.2),ordered.colors=F,#ordered.colors=T,
                rot.per=.15,#c(8,0.3),
                min.freq = input$min,# 
                #vfont=c("sans serif","plain"),
                #vfont=c("script","plain"),
                vfont=c(font,"plain"),
               random.order=FALSE,
                max.words=input$max,#100,#input$freq, max.words=input$max,
               #colors=brewer.pal(6, "Dark2"))
                colors=pal )#"black")
   }
   else if (input$multicloud=="Commonality Cloud") {
     d <- RemoveWordsStepThree()$term.matrix
     commonality.cloud(d, max.words=40,random.order=FALSE,ordered.colors=F,#ordered.colors=T,
                   rot.per=.15,#c(8,0.3),
                 #  min.freq = input$min,# 
                   #vfont=c("sans serif","plain"),
                   #vfont=c("script","plain"),
                   vfont=c(font,"plain"),
                 #  random.order=FALSE,
                 #  max.words=input$max,#100,#input$freq, max.words=input$max,
                   #colors=brewer.pal(6, "Dark2"))
                   colors=pal )#"black")
   }
   else if (input$multicloud=="Comparison Cloud") {
     d <- RemoveWordsStepThree()$term.matrix
     comparison.cloud(d,max.words=40,random.order=FALSE)
   }
 # wordcloud(d$word, d$freq)
#  max.words =100,min.freq=3,scale=c(4,.5), 
 # random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
})

kwicAnalysis <- reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  len <- input$len
  term <- input$choose_term
  if(!is.null(input$file.article)) {
    text <- ExtractRawContentPDF()#Selection()#$text.extract
    num <-length(input$file.article$name)
  }
  if(!is.null(input$file.article.txt))  {
    text <- ExtractRawContentTXT()#extractContent(input$file.article.txt) 
    num <-length(input$file.article.txt$name)
  }
  kwic(len,term,text,num)
})

output$print_kwic <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if (is.null(input$choose_term)) { return() }
    lines <- kwicAnalysis()

  HTML(paste("<br/>", lines, sep="<br/>"))
}) 

##### LDA Analysis
output$choose_topic_num <- renderUI({
  selectizeInput("num", label = "Select or Type Number of Topics", 
                 choices = c(2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
}) 

output$choose_word_num <- renderUI({
  selectizeInput("word", label = "Select or Type Number of Words per Topic", 
                 choices = c(1,2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
}) 

output$iter <- renderUI({
  names <- c(500,1000)
  selectizeInput("iter", label = "Select or Type a number for iterations", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=500,
                 multiple = FALSE) 
}) 
output$alpha <- renderUI({
  names <- c(0.01,0.02,0.05,0.1)
  selectizeInput("alpha", label = "Select or Type a number for apha", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=0.02,
                 multiple = FALSE) 
}) 
output$eta <- renderUI({
  names <- c(0.01,0.02,0.05,0.1)
  selectizeInput("eta", label = "Select or Type a number for eta", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=0.02,
                 multiple = FALSE) 
}) 


chronology <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if( input$metadata_source == "None") {return()}
  if((is.null(input$chronology)) ||(input$chronology=="None")) { return() }
  remove.words.file <- stopWordsTxt()
  corpus.lda <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
  if(!is.null(input$file.article)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus #PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
  }
  if(!is.null(input$file.article.txt)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
  }
  corpus.lda <- removeWords(corpus.lda, remove.words.file)
  corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
  corpus.lda <- gsub("\\s+"," ",corpus.lda)
  corpus.lda <- str_c(corpus.lda)
  corpus.lda<- str_trim(corpus.lda)
  set.seed(2013)
  my.corpus <- Corpus(VectorSource(corpus.lda))
 # my.corpus <- tm_map(my.corpus,removeWords,stopwords("english"))
  language=input$language
  dtm <- DocumentTermMatrix(my.corpus)
 # if (!is.null(input$metadata_pdf)) {
  dates<-  fileData()$date
   # dates <- metadataPdf()$datetimes
 # }
  #if (!is.null(input$metadata_csv)) {
  #  dates <- newData()[[2]]
 # }
  for (i in 1:length(my.corpus)){
    meta(my.corpus[[i]], tag = "datetimestamp") <- dates[i]
  }
  dtm <- DocumentTermMatrix(my.corpus)
  n.topics <- as.numeric(input$num)
  lda.model <- LDA(dtm, n.topics,method="Gibbs")
  n.words <- as.numeric(input$word)
  term <- terms(lda.model,n.words)  
  df <- data.frame(id=names(topics(lda.model)), 
                   date=unlist(meta(my.corpus, type="local",tag="datetimestamp"),as.character)#, "%Y-%m-%d %H:%M:%S"))))#2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"   
  )
  dft <- cbind(df,posterior(lda.model)$topics)
 # dft2 < cbind(df,term)
  M <-melt(dft,id.vars=c("id","date")) 
  l<-length(my.corpus)
  info <- list(dft=dft, term=term,lda.model=lda.model,M=M,l=l)#,datetimestamp=datetimestamp)
  return(info)
})

output$chronology_top <- renderPrint ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((input$chronology=="None") || (is.null(input$chronology))) { return() }
   chronology()$term
})
output$chronology_plot <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((input$chronology=="None") || (is.null(input$chronology))) { return() }
  dft<-chronology()$M
#  terms <- chronology()$term
  g <- ggplot(dft,aes(x= dft[,2],y=dft[,4],color=dft[,3]))+xlab("Time Period") + ylab("Posterior") + geom_point(aes(size = dft[,4]))+ geom_density2d(alpha=.2) #+ geom_text(aes(label=terms))
  # p <- MyPlot(M[2:3],grouped.by=M[1])
  # p<-  ggplot(data=M, aes(y=value, x=date, color=variable))+geom_point() + geom_line() xlab("Time Period") + ylab("Posterior")  +
  return(g)
})
output$chronology_table <- renderTable ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((input$chronology=="NULL") || (is.null(input$chronology))) { return() }
  dft<-chronology()$M
 # g <- ggplot(dft,aes(x= dft[,2],y=dft[,3]),color=dft[,1])+geom_point()  + geom_density2d(alpha=.2)
 # p <- MyPlot(M[2:3],grouped.by=M[1])
 # p<-  ggplot(data=M, aes(y=value, x=date, color=variable))+geom_point() + geom_line()
  return(dft)
})

BestK <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  remove.words.file <- stopWordsTxt()
  cutoff.lower=0#input$cutoff_lower
 # cutoff.high=input$cutoff_high
  if(!is.null(input$file.article)) {
    novel.vector <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
  #  novel.vector <- ExtractContentPDF()$lda.format
   # num.documents <- length(ExtractContentPDF()$lda.format)
    num.documents <- length(PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    file.names <- input$file.article$name  
  }
  if(!is.null(input$file.article.txt)) {
    novel.vector <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
   # novel.vector <- ExtractContentTXT()$lda.format #novel.list
    num.documents <- length(PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    file.names <- input$file.article.txt$name  
  }
  
  novel.vector <- removeWords(novel.vector, remove.words.file)
  novel.vector <- removeWords(novel.vector, c(input$remove_words))
  pdf.corpus <- lexicalize(novel.vector, lower=TRUE)
  if (input$language=="None") {
    pdf.corpus$vocab <- pdf.corpus$vocab
  }
  else {
  pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language = input$language)
  }
  wc <- word.counts(pdf.corpus$documents)
  to.remove <- as.numeric(names(wc)[wc<=cutoff.lower])
  pdf.corpus$documents <- filter.words(pdf.corpus$documents , to.remove)
  if (!is.null(input$metadata_pdf)) {
    file.names <- metadataPdf()$names
  }
  if (!is.null(input$metadata_csv)) {
    file.names <- metadataPdf()$names
  }
  matrix <- create_matrix(cbind(as.vector(file.names),as.vector(novel.vector)), 
                          language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
  best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(matrix, d)})
  best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
  best.model.logLik.df <- data.frame(topics=c(2:50), LL=as.numeric(as.matrix(best.model.logLik)))
  
  p <- ggplot(best.model.logLik.df, aes(x=topics, y=LL)) +   
    xlab("Number of topics") + ylab("Log likelihood of the model") +   
    geom_line() +   theme_bw()
  
  k <- best.model.logLik.df[which.max(best.model.logLik.df$LL),]
  info <- list(p=p,k=k,best.model.logLik=best.model.logLik,best.model=best.model)
  return(info)
})
output$best_k <- renderTable ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:35) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  best.model <- BestK()$best.model
  best.model.logLik<-BestK()$best.model.logLik
  best.model.logLik.df <- data.frame(topics=c(2:50), LL=as.numeric(as.matrix(best.model.logLik)))
  best.model.logLik.df[which.max(best.model.logLik.df$LL),]
})
output$best_k_plot <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Plotting in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:35) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  BestK()$p
})

LdaAnalysis <- reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  set.seed(2013)
  remove.words.file <- stopWordsTxt()
  cutoff.lower=0#input$cutoff_lower
  #cutoff.high=input$cutoff_high
  if(!is.null(input$file.article)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus #PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
 #   num.documents <- length(RemoveWordsStepOne()$lda.corpus)#PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    n.docs <- as.numeric(length(input$file.article$name))  
  }
  if(!is.null(input$file.article.txt)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
   # num.documents <- length(RemoveWordsStepOne()$lda.corpus)
    #corpus.lda <-  window.one()$lda.format
   # num.documents <- length(window.one()$lda.format)
    n.docs <- as.numeric(length(input$file.article.txt$name))
  }
  corpus.lda <- removeWords(corpus.lda, remove.words.file)
  corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
  corpus.lda <- gsub("\\s+"," ",corpus.lda)
  corpus.lda <- str_c(corpus.lda)
  corpus.lda<- str_trim(corpus.lda)
 # empty.string <- lapply(corpus.lda, function(x) gsub(" +", " ", x))
 # pdf.corpus <- lexicalize(empty.string, lower=TRUE)

 # corpus <- Corpus(VectorSource(corpus.lda))
 # corpus <- tm_map(corpus,removeWords,remove.words.file)
 # newtext <-tm_map(corpus,removeWords,input$remove_words)

 # pdf.corpus <- lexicalize(newtext, lower=TRUE)
  pdf.corpus <- lexicalize(corpus.lda, lower=TRUE)
  
  if (input$language=="none") {
    pdf.corpus$vocab <- pdf.corpus$vocab
  }
  else {
    language=input$language
  pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language)# = "english")
  }
  wc <- word.counts(pdf.corpus$documents)
  to.remove <- as.numeric(names(wc)[wc<=cutoff.lower])
   pdf.corpus$documents <- filter.words(pdf.corpus$documents , to.remove)
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }
  K <- as.numeric(input$num)
  alphaK <-as.numeric(input$alpha)
  etaK<-as.numeric(input$eta)
  num.words <- as.numeric(input$word)
  iterK <-as.numeric(input$iter)
  pdf.lda <-
    lda.collapsed.gibbs.sampler(pdf.corpus$documents,K,pdf.corpus$vocab,iterK, alpha=alphaK, eta=etaK, compute.log.likelihood=TRUE)
  topics <- top.topic.words(pdf.lda$topics, num.words, by.score = T)
  docs <- top.topic.documents(pdf.lda$document_sums,n.docs)# num.documents)
  p_topic <- as.vector(pdf.lda$topic_sums / sum(pdf.lda$topic_sums))
  lda.coordinates <- mat.or.vec(n.docs,K)
  # for (i in 1:n.docs){
  #   for (j in 1:K){
  #     lda.coordinates[i,j] <-
  #       sum(pdf.lda$assignments[[i]]==(j-1))/length(pdf.lda$assignments[[i]])
  #   }
  # }
  info<-list(p_topic=p_topic,topics=topics, docs=docs,lda.coordinates=lda.coordinates)
  return(info)
})

output$topics <- renderTable({ #renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  LdaAnalysis()$topics
})

output$docs <- renderPrint({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  LdaAnalysis()$docs
})

output$docsNames <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$lda)) ||  (input$lda=="None")) { return() }
  if (!is.null(input$file.article.txt)) {
    k <- length(input$file.article.txt$name)
    n <- as.list(rep(1:k,1))
    HTML(paste("Document ",n, ":", input$file.article.txt$name, sep=" ", collapse="<br/>"))
  }
  else if (!is.null(input$file.article)) {
    k <- length(input$file.article$name)
    n <- as.list(rep(1:k,1))
    HTML(paste("Document ",n, ":", input$file.article$name,  metadataPdf()$titles, sep=" ", collapse="<br/>"))
  }
})

output$printCoordinates <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  
  distance <- LdaAnalysis()$lda.coordinates
  d<-dist(distance)
  # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  # fit # view results
  # plot solution 
  x <- fit$points[,1]
  y <- fit$points[,2]
  p <- plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
            main="Metric  MDS",	type="n")
  text(x, y,  cex=.9)  
  return(p)
})

output$best_topic_num <-renderUI({
    names <- c("NULL","Calculate")
    selectizeInput("best_num", label = "Select Calculate to find the best topic number (Log Likelihood) - It may take a long time", 
                   choices = names,
                   selected=FALSE,
                   multiple = FALSE) 
  }) 

stmAnalysis <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  novel.vector <- as.list(RemoveWordsStepTwo()$d$word)
  corpus <- Corpus(VectorSource(novel.vector))
  tdm <-DocumentTermMatrix(corpus)   
  out <- readCorpus(tdm, type="dtm")
  documents <- out$documents
  vocab <- out$vocab
  n.topics <- as.numeric(input$num)
  stmmodel <- stm(documents, vocab, n.topics, verbose=FALSE)
return(stmmodel)
})
### Print Topics

#### Print Documents for each topic
#Example of documents associated with topics
output$association <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$stm)) || (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  novel.vector <-RemoveWordsStepThree()$corpus#PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
  K=input$num
#par(mfrow = c(3, 3),mar = c(.5, .5, 1, .5))
for (i in 1:K){
  thoughts <- findThoughts(stmmodel, text=novel.vector,topic=i,n = 1)
  #thoughts <- paste(strsplit(as.character(thoughts3$docs[1]), " ")," ")
  #thoughts5 <- findThoughts(stmmodel, text=novel.vector,topic=5,n = 1)
  plotQuote(thoughts, width = 30, main = paste("Topic",i, " "))
}
})
### Summary
# Expected Topic Proportion
output$proportion <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$stm))|| (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
#par(mfrow=c(1,1),mar=c(5,5,5,5))
plot.STM(stmmodel, type = "summary", xlim = c(0, .9))
})

#####
output$perspectives <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$stm))|| (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  K=as.integer(input$num)
  par(mfrow=c(1,1),mar=c(1,1,1,1))
plot.STM(stmmodel, type = "labels")#, xlim = c(0, .9))#,xlim = c(1, 5))
})

output$cloud_stm <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$stm))|| (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  K=as.integer(input$num)
cloud(stmmodel, topic = K, scale = c(5,.25))
})

### graphical display of topic correlation
output$corelation <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$stm)) || (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  modoutcorr <- topicCorr(stmmodel)
#modoutcorr$cor
  plot.topicCorr(modoutcorr,vertex.label.cex = 1.0)
})

output$topics_stm <- renderPrint({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if((is.null(input$stm)) || (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  labelTopics(stmmodel)
 # stmmodel$documents
})





### CLUSTER ANALYSIS

cluster <-reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  method=input$method
  distance = input$distance
  dtm <-ListTerms()$dtm
  m <- as.matrix(dtm)
  d<-dist(m,distance)
  fit <-hclust(d,method)

  return(fit)
})

output$cluster_plot <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  par(cex=1.2,mar=c(5, 4, 4, 2))
  color=input$color
  k=as.numeric(input$cuttree)
  fit <- cluster()
 # fit$labels <- input$file.article.txt$name
 # if (!is.null(input$metadata_source)) {
 #   file.names <- fileData()$title
 # }
   if (!is.null(input$file.article.txt)) {
    file.names <- input$file.article.txt$name
  }
  else if (!is.null(input$file.article)){
    file.names <- input$file.article$name
  }
 # fit$labels <- file.names
  p<-  plot(fit)
  if (input$cuttree==0)
  #  p<-  plot(fit)
  p<- plot(fit,horiz=T,labels=file.names)
  else {
  #  p<-  plot(fit)
  p<-  plot(fit,horiz=T,labels=file.names)
  rect.hclust(fit,k, border=color)
  }
 # fit <- cluster()
  #return(fit)
})


output$cuttree <-renderUI({
  punct <- c(0,2,3,4,5,6,7,8,9)
  selectizeInput("cuttree", label = "Select number of groups", 
                 choices = punct,
                 selected = 0,
                 multiple = FALSE) 
})
 

# output$print_doc_cloud_dh <- renderPlot({
#   if (is.null(input$file.novel)) { return() }
#   if ((is.null(input$show_freq_dh)) | (input$show_freq_dh=="NULL")) { return() }
#     d <- ExtractContentTXT()$lda.format
#     corpus <- Corpus(VectorSource(d))
#     tdm <- TermDocumentMatrix(corpus)
#     tdm <-removeSparseTerms(tdm,0.1)
#     tdm.dense <- as.matrix(tdm)
#     tdm.dense = melt(tdm.dense, value.name = "count")
#   tdm.subset <- subset(tdm.dense, count > 3)
#  plot <- ggplot(tdm.subset, aes(x=Docs, y=Terms, fill=log10(count)))+
#    geom_tile(colour="white")+
#    scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
#         ylab("Words") +
#         theme(panel.background = element_blank()) +
#         theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
#   
#   return(plot)
# 
# })




# cluster_dh <-reactive ({
#   if (is.null(input$file.novel)) { return() }
#   if ((is.null(input$cluster_dh)) || (input$cluster_dh=="NULL")){ return() }
#   novel.list <- ExtractContentTXT()$novel.list
#   #  corpus <- Corpus(VectorSource(data))#ExtractContentTxt()$lda.format))
#   # tdm <- TermDocumentMatrix(corpus)
#   novel.dataframe <- mapply(data.frame, ID=seq_along(novel.list), 
#                             novel.list, SIMPLIFY = FALSE, 
#                             MoreArgs = list(stringsAsFactors=FALSE))
#   
#   novel.df <- do.call(rbind,novel.dataframe)
#   result <- xtabs(Freq ~ ID+text.split, data=novel.df)
#   final.m <- apply(result, 2, as.numeric)
#   smaller.corpus <- final.m[,apply(final.m,2,mean)>=.25]
#   documents <- dist(smaller.corpus)
#   fit <- hclust(documents,method="ward.D")
#   fit$labels<-names(novel.list)
#   
#  # if ((is.null(input$cuttree))|| (input$cuttree=="NULL")) {
#     p<-  plot(fit)
#    # p <- rect.hclust(fit,k=3, border="red")
#  # }
# #  else {
#  #   n=as.numeric(as.character(input$cuttree))
#  #   color = input$color
#  #   p <- rect.hclust(fit,k=n, border=color)
#  # }
#   return(p)
# })
# 
# 
# output$color_dh <-renderUI({
#   if (is.null(input$file.novel)) { return() }
#   if ((is.null(input$cluster_dh)) || (input$cluster_dh=="NULL")){ return() }
# punct <- c("red", "blue","green","black")
# selectizeInput("color", label = "Select color for cluster groups", 
#                choices = punct,
#                selected = "red",
#                multiple = FALSE) 
# })
# output$cuttree_dh <-renderUI({
#   if (is.null(input$file.novel)) { return() }
#   if ((is.null(input$cluster_dh)) || (input$cluster_dh=="NULL")){ return() }
#   punct <- c("NULL","2","3","4","5","6","7","8","9")
#   selectizeInput("cuttree", label = "Select number of groups", 
#                  choices = punct,
#                  selected = FALSE,
#                  multiple = FALSE) 
# })
#### PUNCTUATION ANALYSIS

punctuation <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  text <-PreprocessingSteps()[3]#$text.extract#window.one()$text.extract
  data.del <- gsub("[A-Za-z0-9]"," \\1", text) # only punctuation is left with space
  data.punct <-  gsub("\\s+","",data.del)
  punctuation <- unlist(strsplit(data.punct, ""))
  tab <-table(punctuation)
  m <- as.data.frame(tab)
  return(m)
})
output$type_punctuation <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  d<-punctuation()
     plot <- ggplot(d, aes(x=d[[1]], y=d[[2]])) +
       geom_bar(stat='identity') +
      theme_classic() +
      theme(axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=20))+
     ggtitle("Punctuation Frequency") + ylab("Frequency") + xlab("Type")+
       coord_flip()
return(plot)
})

output$chunks <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if (is.null(input$choose_term)){ return() }
  if(!is.null(input$file.article)) {
    text <- ExtractRawContentPDF()#Selection()#$text.extract
  }
  if(!is.null(input$file.article.txt))  {
    text <- ExtractRawContentTXT()#extractContent(input$file.article.txt) 
  }
  term <- input$choose_term
  split_chunks(text,term)
})


output$heatmap_punct <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if(!is.null(input$file.article)) {
    text <- ExtractRawContentPDF()#Selection()#$text.extract
  }
  if(!is.null(input$file.article.txt))  {
    text <- ExtractRawContentTXT()#extractContent(input$file.article.txt) 
  }
  heatmap(text)
  
 #  text <-PreprocessingSteps()[3]#$text.extract#window.one()$text.extract
 #  data.del <- gsub("[A-Za-z0-9]"," \\1", text) # only punctuation is left with space
 #  data.punct <-  gsub("\\s+","",data.del)
 #  data.num <- gsub("[0-9]+","", data.punct)
 #  s.col <- gsub("[!?]",1,data.num) # red
 #  s.col <- gsub("[,\"\'\\(\\)]",3,s.col) # green
 #  s.col <- gsub("[\\.:;-\\*]",2,s.col) # blue
 #  s.col <- gsub("[^[1:3] ]", "", s.col)
 # # s.col <- gsub("[[:punct:]]","",s.col)
 #  s.split <- strsplit(s.col,"")
 #  matr1 <-matrix(s.split[[1]])
 #  n<-length(matr1)
 #  f <- factorize(n)
 #  divide <-f[1]
 #  if (min(f)<4) {
 #    divide <- f[2]
 #  }
 #  matr <- matrix(s.split[[1]],ncol=divide,byrow = FALSE)
 #  class(matr) <- "numeric" 
 #  myImagePlot(matr, title="Punctuation Analysis")#,zlim=c(1,3))
  
}) 

output$choose_speaker <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if ((is.null(input$article_content)) || (input$article_content=="NULL")) {return()}
  if (is.null(input$choose_type)) {return()}
  if (input$article_content=="By Speaker") { #&& (input$choose_type=="Transcript")) {
    names <- c("NULL","speaker","hablante","informant")
    selectizeInput("speaker_name", label = "Select or Add speaker", 
                   choices = names,
                   options = list(create = TRUE),
                   selected = "NULL",
                   multiple = FALSE) 
  }
}) 

output$choose_interviewer <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(input$structured_data_file_json)) && (is.null(input$structured_data_file_xml))) { return() }
  if ((is.null(input$article_content)) || (input$article_content=="NULL")) {return()}
  if (is.null(input$choose_type)) {return()}
  if (input$article_content=="By Speaker") {#&& (input$choose_type=="Transcript")) {
    names <- c("NULL","interviewer","investigador")
    selectizeInput("interviewer_name", label = "Select or Add interviewer", 
                   choices = names,
                   options = list(create = TRUE),
                   selected = "NULL",
                   multiple = FALSE) 
  }
}) 

### END OF DATA PREPARATION ###########

output$remove_html_symbol <- renderUI({
  names <- c("NULL","\\","/","(",")")
  selectizeInput("remove_html_symbol", label = "Select or Type HTML/xml tags to remove", 
                 choices = names,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = TRUE) 
  
})

output$replace_what <- renderUI({
  names <- c("NULL","\\","/","(",")")
  selectizeInput("remove_what", label = "Type Character or String to be REPLACED", 
                 choices = names,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = FALSE) 
  
})

output$replace_by <- renderUI({
  names <- c("NULL","\\","/","(",")")
  selectizeInput("remove_by", label = "Type Character or String for REPLACEMENT", 
                 choices = names,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = FALSE) 
  
})


})