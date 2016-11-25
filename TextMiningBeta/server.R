library(shiny)
source("myImagePlot.R")
source("multiplot.R")
source("MyPlot.R")
source("extractContent.R")
source("extractMetadata.R")
source("abstract.R")
source("words.R")
source("preprocess.R")
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
#library(topicmodels)
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  HTML(paste("Corpus Size Total: ", ListTerms()$len, sep=" ", collapse="<br/>"))
})
output$print_length_txt <- renderUI({
  if  (is.null(input$file.article.txt)) { return() }
  HTML(paste("Corpus Size Total: ", ListTerms()$len, sep=" ", collapse="<br/>"))
})
 
  ExtractRawContentPDF <- reactive ({
    if (is.null(input$file.article)) { return() }
   extractContentPdf(input$file.article)
  })
  
  output$choose_term <- renderUI({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
    if (input$article_content!="By Term"){ return() }
    withProgress(message = 'Loading Term List',
                 value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    word <- ListTerms()$d[[2]]#[1:x]
    selectizeInput("choose_term", label = "Select term(s)", 
                   choices = word,
                   options = list(create = TRUE),
                   selected = NULL,
                   multiple = TRUE) 
  })   

  output$choose_length <- renderUI({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
    if (input$article_content!="By Term"){ return() }
    
    d <- ListTerms()$d
    sliderInput("len",
                "Context Length:",
                min = min(d$freq),
                max = max(d$freq),
                value = min(d$freq)+2)             
  })
  
  ListTerms <-reactive({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
    
    if(!is.null(input$file.article)) {
      corpus.lda <- ExtractRawContentPDF()#Selection()#$text.extract
    }
    if(!is.null(input$file.article.txt))  {
      corpus.lda <- ExtractRawContentTXT()#extractContent(input$file.article.txt) 
    }
    listTerms(corpus.lda)
    })  
  
  output$term_print <- renderUI ({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
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

fileData <- reactive({
  inFile <- input$file.metadata    
  my_data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                      quote=input$quote)
  dat <- my_data 
  return(dat)
})

### Display Metadata from CSV
output$print_metadata_csv <- renderDataTable({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if (input$metadata_csv=="Load") {
    d<-fileData()
  }
  else {"You need to select Upload"}
  
})

output$print_metadata_pdf <- renderTable({
  if (is.null(input$file.article)) { return() }
  if (input$metadata=="Load") {
   a <- metadataPdf()$authors
   t <- metadataPdf()$titles
   dt <- metadataPdf()$datetimes
   withProgress(message = 'Loading Metadata',
               value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  d <- data.frame(date=dt, title=t, author = a)
  }
  else if (input$metadata=="None"){
    d <- "You need to select metadata"
  }
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)))   { return() }
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
    }
  HTML(paste("<br/>", "Document: ",pdf.lines, sep="<br/>"))
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
########
### Pre-Processing
PreprocessingSteps <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)))   { return() } 
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
  remove_urls <-input$remove_urls
  remove_references<-input$remove_references
  remove_html<-input$remove_html
  lower_case<-input$lower_case
  remove_numbers<-input$remove_numbers
  exceptions<-input$exceptions
  remove_punctuation <- input$remove_punctuation
  mylist<-tokenize(x,y,remove_urls,remove_references,remove_punctuation,
           remove_html,lower_case,remove_numbers,exceptions)
 return(mylist)
  })


# }
#   if ((input$stem=="NULL") || (is.null(input$stem))) {
#     text.punct <- text.punct
#   }
#   else if (input$stem=="Stem") {
#     text.split <- unlist(strsplit(text.punct, " "))
#     text.punct <- paste(wordStem(text.split, language = "english"),collapse = " ")
#   }


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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  #cutoff.lower=0#input$cutoff_lower
  #cutoff.high=input$cutoff_high
  mycorpus <- PreprocessingSteps()[6]
  if  (input$stops=="None") {
    corpus.paste <-paste(mycorpus, sep=" ")
    corpus.list <- strsplit(corpus.paste, "\\s+")
    terms <- table(unlist(corpus.list))
    terms.sorted <- sort(terms, decreasing = TRUE)
    terms.matrix<-as.matrix(terms.sorted)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    d <- d[order(d$frequency, decreasing = T), ]
   # words.list <- as.list(d$word)
    corpus <- mycorpus
  }
  else if  ((input$stops=="Default")||(input$stops=="Upload")) {
    remove_word <- stopWordsTxt()
    doc.vect <- VectorSource(mycorpus)
    corpus.tm <-Corpus(doc.vect)
    corpus.tm <- tm_map(corpus.tm,removeWords,stopWordsTxt())
    corpus <- list()
    for (i in 1:length(corpus.tm)) {
      doc <-corpus.tm[[i]]$content
      corpus[[i]] <- doc
    }
    corpus <-unlist(corpus)
    corpus.paste <-paste(mycorpus, sep=" ")
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
  info <- list(corpus=corpus,d=d)
  return(info)
})

RemoveWordsStepTwo <-reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if (is.null(input$remove_words)) {
    corpus <-RemoveWordsStepOne()$corpus
    d <-RemoveWordsStepOne()$d
   # words.list <-RemoveWordsStepOne()$words.list
  }
  else {
    mycorpus <-RemoveWordsStepOne()$corpus
    doc.vect <- VectorSource(mycorpus)
    corpus.tm <-Corpus(doc.vect)
   # corpus.tm <- removeWords(corpus.tm, c(input$remove_words))
    corpus.tm <- tm_map(corpus.tm,removeWords,c(input$remove_words))
    corpus <- list()
    for (i in 1:length(corpus.tm)) {
      doc <-corpus.tm[[i]]$content
      corpus[[i]] <- doc
    }
    corpus <-unlist(corpus)
    corpus.paste <-paste(corpus, sep=" ")
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  
  if(input$stopwords=="None") {
    corpus <-RemoveWordsStepOne()$corpus
    d <-RemoveWordsStepOne()$d
    
   # words.list <-RemoveWordsStepOne()$words.list
  }
  else if(input$stopwords=="Apply Stopwords") {
   # if ((input$stops=="None") && (input$remove==FALSE))  {
  #    corpus <-RemoveWordsStepOne()$corpus
   #   d <-RemoveWordsStepOne()$d
     # words.list <-RemoveWordsStepOne()$words.list
      
   # }
   # else {
      corpus <-RemoveWordsStepTwo()$corpus
      d <-RemoveWordsStepTwo()$d
     # words.list <-RemoveWordsStepTwo()$words.list
   # }
  }
  info <- list(d=d,corpus=corpus)
  return(info)
})

output$print_apply_stops <- renderUI({
  if (input$stopwords=="None") {"No changes are made. You need to select stopwords first"}
  if (input$stopwords=="Apply Stopwords") {
    HTML(paste0(RemoveWordsStepThree()$corpus))
  }
})
#########
rawFrequency <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  d<-rawFrequency()$dataf
  return(d)#,options=list(lengthMenu = c(5, 10, 15), pageLength = 5))
})

output$zipf <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
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
  if ((input$show_text=="NULL")|| (is.null(input$show_text))) {return()}
  i <- input$show_text
  if (!is.null(input$file.article)) {
    names <- input$file.article$name
  }
  if (!is.null(input$file.article.txt)) {
    names <- input$file.article.txt$name
  }
  n <- which(names==i)
  vec <- ExtractContent()[[n]]
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
  if ((input$show_word=="NULL")|| (is.null(input$show_word))) {return()} 
tab <- LengthSent()
return(tab)
})
output$length_word_table <- renderTable ({
  if (input$show_word=="None") {"Length analysis is not selected"} 

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

output$words <- renderPlot ({
  if (input$show_word=="None") {"Select Word or Sentence Length"}
  if (input$show_word=="Word Length") {
  newtab <- LengthWord()
  plots <- ggplot(newtab, aes(x=word_length,y=proportion)) + geom_line() + ggtitle(input$show_text)
  }
  if (input$show_word=="Sentence Length") {
    counts <- LengthSent()$counts
    plots <- hist(counts,main="Histogram of Sentence Length", col="grey",freq=TRUE)
  }
  return(plots)
})

LengthSent <- reactive ({
  if (input$show_text=="None") {return()}
  i <- input$show_text
  if (!is.null(input$file.article)) {
    names <- input$file.article$name
  }
  if (!is.null(input$file.article.txt)) {
    names <- input$file.article.txt$name
  }
  n <- which(names==i)
  data <- ExtractContent()[[n]]
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
# else if (input$stem=="Stem") {
#   text.split <- unlist(strsplit(text.punct, " "))
#   text.punct <- paste(wordStem(text.split, language = "english"),collapse = " ")
# }
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

# RemoveWords <- reactive({
#   if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
#   corpus.lda <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
# # if(!is.null(input$file.article)) {
#  #  corpus.lda <- ExtractContentPDF()$lda.format
# # }
#  #if(!is.null(input$file.article.txt)) {
#   # corpus.lda <- ExtractContentTXT()$lda.format 
#  #}
#   corpus <- Corpus(VectorSource(corpus.lda))
#   tdm <- TermDocumentMatrix(corpus)
#   terms.matrix <- as.matrix(tdm)
#   d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
#   d$word <- row.names(d)
#   agg_freq <- aggregate(frequency ~ word, data = d, sum)
#   d <- d[order(d$frequency, decreasing = T), ] 
#   info <- list(d=d,corpus.lda=corpus.lda,tdm=tdm)
#   return(info)
# })

output$choose_top <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
 # if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  selectizeInput("top", label = "Select a number for top frequent words (ex. 10 top frequent words)", 
                 choices = c(10,20,30,40,50,60,70,80,90,100),
                 options = list(create = TRUE),
                 selected=10,
                 multiple = FALSE) 
}) 

output$choose_remove <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
 # if (is.null(input$remove_manual)) {"No manual selection is applied"}
  #if (input$remove_manual=="Apply Manual") {
  word <- RemoveWordsStepOne()$d[[2]]
  selectizeInput("remove_words", label = "Select words to be removed", 
                 choices = word,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = TRUE) 
 # }

  })

output$printWords <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
 # if (is.null(input$remove_manual)) { return() }
  HTML(paste0(RemoveWordsStepOne()$d[[2]]))
 # HTML(paste0(RemoveWordsStepTwo()$words.list))#PreprocessingSteps()$lda.format))# RemoveWordsNew()$corpus.lda))
})

output$word_count <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
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
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
 # if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  d <- RemoveWordsStepThree()$d
  sliderInput("min",
              "Minimum Frequency:",
              min = min(d$freq),
              max = max(d$freq),
              value = min(d$freq)+2)             
})
output$choose_max_words <- renderUI({
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
  #if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  d <- RemoveWordsStepThree()$d
  num <- nrow(d)
  sliderInput("max",
              "Maximum Words per Plot:",
              min = 1,
              max = num,
              value = 100)             
})

output$print_cloud <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  # withProgress(message = 'Plotting in progress',
  #              detail = 'This may take a while...', value = 0, {
  #                for (i in 1:15) {
  #                  incProgress(1/15)
  #                  Sys.sleep(0.25)
  #                }
  #              })
  wordcloud_rep <- repeatable(wordcloud)
  d <- RemoveWordsStepThree()$d
  wordcloud_rep(d$word, d$freq, scale=c(8,0.2),ordered.colors=T,
                rot.per=.15,#c(8,0.3),
                min.freq = input$min,# 
                vfont=c("sans serif","plain"),
               random.order=FALSE,
                max.words=input$max,#100,#input$freq, max.words=input$max,
               #colors=brewer.pal(6, "Dark2"))
                colors="black")
 # wordcloud(d$word, d$freq)
#  max.words =100,min.freq=3,scale=c(4,.5), 
 # random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
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
output$choose_lda <- renderUI({
  names <- c("NULL","RUN")
  selectizeInput("lda", label = "Select RUN to run an LDA analysis", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE) 
}) 
output$choose_stm <- renderUI({
  names <- c("NULL","RUN")
  selectizeInput("stm", label = "Select RUN to run an STM analysis", 
                 choices = names,
                 selected=FALSE,
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

output$choose_chronology <- renderUI({
  names <- c("NULL","RUN")
  selectizeInput("chronology", label = "Select RUN", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE) 
}) 

chronology <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$chronology)) ||(input$chronology=="NULL")) { return() }
  remove.words.file <- stopWordsTxt()
  corpus.lda <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
  corpus.lda <- removeWords(corpus.lda, remove.words.file)
  corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
  corpus.lda <- gsub("\\s+"," ",corpus.lda)
  corpus.lda <- str_c(corpus.lda)
  corpus.lda<- str_trim(corpus.lda)
  set.seed(2013)
  my.corpus <- Corpus(VectorSource(corpus.lda))
  my.corpus <- tm_map(my.corpus,removeWords,stopwords("english"))
  dtm <- DocumentTermMatrix(my.corpus)
  if (!is.null(input$metadata_pdf)) {
    dates <- metadataPdf()$datetimes
  }
  if (!is.null(input$metadata_csv)) {
    dates <- newData()[[2]]
  }
  for (i in 1:length(my.corpus)){
    meta(my.corpus[[i]], tag = "datetimestamp") <- dates[i]
  }
  dtm <- DocumentTermMatrix(my.corpus)
  n.topics <- as.numeric(input$num)
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((input$chronology=="NULL") || (is.null(input$chronology))) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
   chronology()$term
})
output$chronology_plot <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((input$chronology=="NULL") || (is.null(input$chronology))) { return() }
  withProgress(message = 'Plotting in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  dft<-chronology()$M
#  terms <- chronology()$term
  g <- ggplot(dft,aes(x= dft[,2],y=dft[,4],color=dft[,3]))+xlab("Time Period") + ylab("Posterior") + geom_point(aes(size = dft[,4]))+ geom_density2d(alpha=.2) #+ geom_text(aes(label=terms))
  # p <- MyPlot(M[2:3],grouped.by=M[1])
  # p<-  ggplot(data=M, aes(y=value, x=date, color=variable))+geom_point() + geom_line() xlab("Time Period") + ylab("Posterior")  +
  return(g)
})
output$chronology_table <- renderTable ({
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
  if((input$chronology=="NULL") || (is.null(input$chronology))) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  dft<-chronology()$M
 # g <- ggplot(dft,aes(x= dft[,2],y=dft[,3]),color=dft[,1])+geom_point()  + geom_density2d(alpha=.2)
 # p <- MyPlot(M[2:3],grouped.by=M[1])
 # p<-  ggplot(data=M, aes(y=value, x=date, color=variable))+geom_point() + geom_line()
  return(dft)
})

BestK <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
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
  pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language = "english")
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
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
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$lda)) || (input$lda=="NULL")) { return() }
  set.seed(2013)
  remove.words.file <- stopWordsTxt()
  cutoff.lower=0#input$cutoff_lower
  #cutoff.high=input$cutoff_high
  if(!is.null(input$file.article)) {
    corpus.lda <-  PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
    num.documents <- length(PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    n.docs <- as.numeric(length(input$file.article$name))  
  }
  if(!is.null(input$file.article.txt)) {
    corpus.lda <-  window.one()$lda.format
    num.documents <- length(window.one()$lda.format)
    n.docs <- as.numeric(length(input$file.article.txt$name))
  }
  corpus.lda <- removeWords(corpus.lda, remove.words.file)
  corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
  empty.string <- lapply(corpus.lda, function(x) gsub(" +", " ", x))
  pdf.corpus <- lexicalize(empty.string, lower=TRUE)

 # corpus <- Corpus(VectorSource(corpus.lda))
  #  corpus <- Corpus(VectorSource(novel.vector))
 # corpus <- tm_map(corpus,removeWords,remove.words.file)
 # newtext <-tm_map(corpus,removeWords,input$remove_words)
  
 # pdf.corpus <- lexicalize(newtext, lower=TRUE)
 # pdf.corpus <- lexicalize(corpus.lda, lower=TRUE)
 # pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language = "english")
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
  docs <- top.topic.documents(pdf.lda$document_sums, num.documents)
  p_topic <- as.vector(pdf.lda$topic_sums / sum(pdf.lda$topic_sums))
  lda.coordinates <- mat.or.vec(n.docs,K)
  for (i in 1:n.docs){
    for (j in 1:K){
      lda.coordinates[i,j] <-
        sum(pdf.lda$assignments[[i]]==(j-1))/length(pdf.lda$assignments[[i]])
    }
  }
  info<-list(p_topic=p_topic,topics=topics, docs=docs,lda.coordinates=lda.coordinates)
  return(info)
})

output$topics <- renderTable({ #renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$lda)) || (input$lda=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  LdaAnalysis()$topics
})

output$best_topic_num <-renderUI({
    names <- c("NULL","Calculate")
    selectizeInput("best_num", label = "Select Calculate to find the best topic number (Log Likelihood) - It may take a long time", 
                   choices = names,
                   selected=FALSE,
                   multiple = FALSE) 
  }) 

stmAnalysis <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$stm)) || (input$stm=="NULL")) { return() }
  remove.words.file <- stopWordsTxt()
  novel.vector <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
  corpus <- Corpus(VectorSource(novel.vector))
  corpus <- tm_map(corpus,removeWords,remove.words.file)
  corpus <-tm_map(corpus,removeWords,input$remove_words)
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$stm)) || (input$stm=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  stmmodel<-  stmAnalysis()
  novel.vector <-PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$stm))|| (input$stm=="NULL")) { return() }
  stmmodel<-  stmAnalysis()
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
#par(mfrow=c(1,1),mar=c(5,5,5,5))
plot.STM(stmmodel, type = "summary", xlim = c(0, .9))
})

#####
output$perspectives <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$stm))|| (input$stm=="NULL")) { return() }
  stmmodel<-  stmAnalysis()
  K=as.integer(input$num)
  par(mfrow=c(1,1),mar=c(1,1,1,1))
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
plot.STM(stmmodel, type = "labels")#, xlim = c(0, .9))#,xlim = c(1, 5))
})
output$cloud_stm <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$stm))|| (input$stm=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  stmmodel<-  stmAnalysis()
  K=as.integer(input$num)
cloud(stmmodel, topic = K, scale = c(5,.25))
})
### graphical display of topic correlation
output$corelation <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$stm)) || (input$stm=="NULL")) { return() }
  stmmodel<-  stmAnalysis()
  modoutcorr <- topicCorr(stmmodel)
#modoutcorr$cor
  plot.topicCorr(modoutcorr,vertex.label.cex = 1.0)
})

output$topics_stm <- renderPrint({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$stm)) || (input$stm=="NULL")) { return() }
  stmmodel<-  stmAnalysis()
#labelTopics(stmmodel)
  stmAnalysis$documents
})

output$docs <- renderPrint({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$lda)) || (input$lda=="NULL")) { return() }
  LdaAnalysis()$docs
})

output$docsNames <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$lda)) ||  (input$lda=="NULL")) { return() }
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
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$lda)) || (input$lda=="NULL")) { return() }
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

### CLUSTER
### CLUSTER ANALYSIS
output$choose_cluster <- renderUI({
  punct <- c("NULL", "Cluster")
  selectizeInput("cluster", label = "Select Cluster for cluster analysis", 
                 choices = punct,
                 selected = FALSE,
                 multiple = FALSE) 
})

cluster <-reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$cluster)) || (input$cluster=="NULL")){ return() }
    novel.list <-PreprocessingSteps()[2]#$novel.list#window.one()$novel.list
  novel.dataframe <- mapply(data.frame, ID=seq_along(novel.list), 
                            novel.list, SIMPLIFY = FALSE, 
                            MoreArgs = list(stringsAsFactors=FALSE))  
  novel.df <- do.call(rbind,novel.dataframe)
  result <- xtabs(Freq ~ ID+text.split, data=novel.df)
  final.m <- apply(result, 2, as.numeric)
  smaller.corpus <- final.m[,apply(final.m,2,mean)>=.25]
  documents <- dist(smaller.corpus)
  fit <- hclust(documents,method="ward.D")
 if ((!is.null(input$metadata_pdf))&& (is.null(input$metadata_csv))) {
   fit$labels<-names(novel.list)
 }
 else if (!is.null(input$metadata_pdf)) {
   file.names <- newData()[[3]]
   fit$labels <- file.names
 }
 else if (!is.null(input$metadata_csv)) {
   file.names <- newData()[[3]]
   fit$labels <- file.names
 }
  # if ((is.null(input$cuttree))|| (input$cuttree=="NULL")) {
  p<-  plot(fit)
  # p <- rect.hclust(fit,k=3, border="red")
  # }
  #  else {
  #   n=as.numeric(as.character(input$cuttree))
  #   color = input$color
  #   p <- rect.hclust(fit,k=n, border=color)
  # }
  return(p)
})
output$cluster_plot <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$cluster)) || (input$cluster=="NULL")){ return() }
  withProgress(message = 'Plotting in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  par(cex=1.2,mar=c(5, 4, 4, 2))
  fit <- cluster()
  return(fit)
})

output$color <-renderUI({
  if ((is.null(input$cluster)) || (input$cluster=="NULL")){ return() }
  punct <- c("red", "blue","green","black")
  selectizeInput("color", label = "Select color for cluster groups", 
                 choices = punct,
                 selected = "red",
                 multiple = FALSE) 
})
output$cuttree <-renderUI({
  if ((is.null(input$cluster)) || (input$cluster=="NULL")){ return() }
  punct <- c("NULL","2","3","4","5","6","7","8","9")
  selectizeInput("cuttree", label = "Select number of groups", 
                 choices = punct,
                 selected = FALSE,
                 multiple = FALSE) 
})
 

# Words <- reactive({
#   if (is.null(input$file.transcript.txt)) { return() }
#   if(is.null(input$remove_html)) { return() }
#   if(input$remove_html=="NULL") { return() }
#   corpus <- TranscriptHTML()
#   #corpus.lda <- removeWords(corpus.lda, remove.words)
#   corpus <- Corpus(VectorSource(corpus))#ExtractContentTxt()$lda.format))
#   tdm <- TermDocumentMatrix(corpus)
#   m <- as.matrix(tdm)
#   d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))
#   d$word <- row.names(d)
#   agg_freq <- aggregate(freq ~ word, data = d, sum)
#   d <- d[order(d$freq, decreasing = T), ] 
#   # info <- list(d=d,corpus.lda=corpus.lda)
#   return(d)
# })




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
output$choose_punctuation <- renderUI({
    punct <- c("NULL", "Punctuation")
    selectizeInput("punctuation", label = "Select Punctuation for punctuation analysis", 
                   choices = punct,
                   selected = FALSE,
                   multiple = FALSE) 
})

punctuation <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$punctuation)) || (input$punctuation=="NULL")) { return() }
  text <-PreprocessingSteps()[3]#$text.extract#window.one()$text.extract
  data.del <- gsub("[A-Za-z0-9]"," \\1", text) # only punctuation is left with space
  data.punct <-  gsub("\\s+","",data.del)
  punctuation <- unlist(strsplit(data.punct, ""))
  tab <-table(punctuation)
  m <- as.data.frame(tab)
  return(m)
})
output$type_punctuation <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$punctuation)) || (input$punctuation=="NULL")) { return() }
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

output$heatmap_punct <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$punctuation)) || (input$punctuation=="NULL")){ return() }
  text <-PreprocessingSteps()[3]#$text.extract#window.one()$text.extract
  data.del <- gsub("[A-Za-z0-9]"," \\1", text) # only punctuation is left with space
  data.punct <-  gsub("\\s+","",data.del)
  s.col <- gsub("[\\.!?]",1,data.punct) # red
  s.col <- gsub("[,\"\'\\(\\)]",3,s.col) # green
  s.col <- gsub("[:;-\\*]",2,s.col) # blue
  s.col <- gsub("[[:punct:]]","",s.col)
  s.split <- strsplit(s.col,"")
  matr1 <-matrix(s.split[[1]])
  n<-length(matr1)
  f <- factorize(n)
  divide <-f[1]
  if (min(f)<4) {
    divide <- f[2]
  }
  matr <- matrix(s.split[[1]],ncol=divide,byrow = FALSE)
  class(matr) <- "numeric" 
  myImagePlot(matr, title="Punctuation Analysis")#,zlim=c(1,3))
  
}) 

WindowSelect <- reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  extraction <- vector()
  doc<-vector()
  if(!is.null(input$file.article)) {
    corpus.lda <- Selection()$text.extract
    num <-length(input$file.article$name)
  }
  if(!is.null(input$file.article.txt))  {
    corpus.lda <- ExtractContent() 
    num <-length(input$file.article.txt$name)
  }
  
  if (input$article_content=="Full Text") {
    for (i in 1:num) { # documents number
      lda.list <- corpus.lda[[i]]
      #doc[y] <- lda.list
      extraction[i] <- lda.list
    }
  }
  else if (input$article_content=="By Term") {
    for (i in 1:num) {
      len <- input$len
      term <- input$choose_term
      for (i in 1:num) { # documents number
        lda.list <- unlist(strsplit(corpus.lda[[i]], "\\s+"))
        # loop over all requested terms
        #   for (z in 1:m){
        #    one.term <- list.terms[[z]]
        # }
        loc<- grep(term, lda.list)
        # loc<- grep(one.term, lda.list)
        for (y in 1:length(loc)) { # one term indexes
          ext <-  lda.list[(loc[y]-len):(loc[y]+len)] 
          ext.collapse <- paste(ext,collapse=" ")
          doc[y] <- ext.collapse
        }
        
        doc.collapse <-paste(doc, collapse=" ")
        #ext <-  lda.list[(loc-len):(loc+len)]   # EXTRACTING SECTIONS 
        # ext.collapse <- paste(ext,collapse=" ")
        extraction[i] <- doc.collapse
      }
    }
  }
  else if (input$article_content=="Abstract") {
    intro <- input$choose_intro
    for (i in 1:num) {
      lda.list <- unlist(strsplit(corpus.lda[[i]], "\\s+"))
      loc.a<- grep("Abstract|ABSTRACT", lda.list)
      loc.b <- grep(intro[1],lda.list)#grep("Introduction|INTRODUCTION",x)
      doc <-  lda.list[(loc.a+1):(loc.b-1)]
      doc.collapse <- paste(doc,collapse=" ")
      extraction[i] <- doc.collapse
      # lines =  x[(loc.a[1]+1):(loc.b-1)]   # EXTRACTING SECTIONS   
    }
  }
  else if (input$article_content=="By Speaker") {
    for (i in 1:num) {
      #  lda.list <- unlist(strsplit(corpus.lda[[i]], "\\s+"))
      extraction[i] <-corpus.lda[[i]]
      # lines <-x
    }
  }
  return(extraction)
})


output$choose_speaker <- renderUI({
  if ((is.null(input$file.article)) || (is.null(input$file.article.txt))) { return() }
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
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
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




})