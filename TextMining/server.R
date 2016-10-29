library(shiny)
source("myImagePlot.R")
source("multiplot.R")
source("MyPlot.R")
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
#library(slam)
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
    if (is.null(input$file.article)) { return() }
    paste(input$file.article$name, sep="\n")
  }) 
  output$print_name_article_txt <- renderPrint({
    if (is.null(input$file.article.txt)) { return() }
    paste(input$file.article.txt$name, sep="\n")
  }) 
output$print_length <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  HTML(paste("Corpus Size Total: ", ListTerms()$len, sep=" ", collapse="<br/>"))
})
### Section Data Preparation
  output$select_display_content <- renderUI({
   # if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
    names <- c("NULL","Show")#"Raw Content","Processed Content")
    selectizeInput("select_display_content", label = "Select Show Content", 
                   choices = names,
                   selected="NULL",
                   multiple = FALSE) 
  })
  output$select_processed <- renderUI({
    # if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
    names <- c("NULL","Show")#"Raw Content","Processed Content")
    selectizeInput("select_processed", label = "Select Show Content", 
                   choices = names,
                   selected="NULL",
                   multiple = FALSE) 
  })
  output$choose_article_content <- renderUI({
    #names <- c("NULL","Full text","Abstract","Transcript")
    names <- c("NULL","Full Text","Abstract","By Term", "By Speaker")
    selectizeInput("article_content", label = "Select Subset", #ABSTRACT, FULL TEXT 
                  # or TRANSCRIPT to analyze", 
                   choices = names,
                   selected="NULL",
                   multiple = FALSE)   
  }) 
  output$choose_type <- renderUI({
    #names <- c("NULL","Full text","Abstract","Transcript")
    names <- c("NULL","Text","Article")
    selectizeInput("choose_type", label = "Select Type", #ABSTRACT, FULL TEXT 
                   # or TRANSCRIPT to analyze", 
                   choices = names,
                   selected=FALSE,
                   multiple = FALSE)   
  }) 
  output$choose_intro <- renderUI({
     if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
    if ((is.null(input$choose_type)) || (input$choose_type=="NULL")) {return()}
    if ((input$choose_type=="Article") && (input$article_content=="Abstract"))  {
      #names <- c("NULL","Full text","Abstract","Transcript")
      names <- c("NULL","Introduction","1. Introduction","Section 1","Type here")
      selectizeInput("choose_intro", label = "Name of Section 1", #ABSTRACT, FULL TEXT 
                     # or TRANSCRIPT to analyze", 
                     options = list(create = TRUE),
                     choices = names,
                     selected=FALSE,
                     multiple = FALSE)  
    }
  }) 
  output$choose_column <- renderUI({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
    if ((is.null(input$choose_type)) || (input$choose_type=="NULL")) {return()}
    if (input$choose_type=="Article") {
    #names <- c("NULL","Full text","Abstract","Transcript")
    names <- c("NULL","two-column","one-column")
    selectizeInput("choose_column", label = "Select article format", #ABSTRACT, FULL TEXT 
                   # or TRANSCRIPT to analyze", 
                   choices = names,
                   selected=FALSE,
                   multiple = FALSE)  
  }
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
  
   
  
  Selection <- reactive ({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
   # if ((is.null(input$article_content)) || (input$article_content=="NULL")) {return()}
    if (is.null(input$file.article)) { return() }
    num <-length(input$file.article$name)
    text.extract <- vector()

    for (i in 1:num) {
      uris.name <- input$file.article$datapath[i]
      tempPDF <- readPDF(control = list(text = "-layout"))(elem = list(uri = uris.name),
                                                           language="en",id="id1")
      read.file <- tempPDF$content
      x <- enc2utf8(read.file)
        lines <-x
      test.columns <- gsub("^\\s\\s\\s+","", lines)
      test.columns <- gsub("^\\s","", test.columns)
      text.collapse <- paste(test.columns, collapse=" ")
      text.collapse <- enc2utf8(text.collapse)
      text.hyphen <- gsub("-\\s+", "", text.collapse)
      text.punct <- gsub("\\s\\s+", " ", text.hyphen) 
      text  <- text.punct
      text.extract[i] <- text
    }
    info <- list(lines=lines,text.extract=text.extract)
    return(info)
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
    word <- ListTerms()$corpus.lda#d[[2]]#[1:x]
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
  #  cutoff.lower=input$cutoff_lower
   # cutoff.high=input$cutoff_high
    
    if(!is.null(input$file.article)) {
      corpus.lda <- Selection()$text.extract
    }
    if(!is.null(input$file.article.txt))  {
      corpus.lda <- ExtractContent() 
    }
    
      lda.list <- strsplit(corpus.lda, "\\s+")
    
      terms.lda <- table(unlist(lda.list))
      terms.lda <- sort(terms.lda, decreasing = TRUE)
      len <- length(terms.lda)
      terms.matrix<-as.matrix(terms.lda)
      d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
      d$word <- row.names(d)
      agg_freq <- aggregate(frequency ~ word, data = d, sum)
      d <- d[order(d$word),]#frequency, decreasing = T), ] 
      corpus.lda <- as.list(d$word)
      vector.lda <- as.vector(unlist(corpus.lda))
      info=list(vector.lda=vector.lda,d=d,corpus.lda=corpus.lda,len=len)
      return(info)
    })  
  output$term_print <- renderUI ({
    if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
    len <- input$len
    term <- input$choose_term
    HTML(paste("Your Term: ", term, "Your Context Length: ", len,  sep=" ", collapse="\n"))
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
      # doc.collapse <-paste(doc, collapse=" ")
      #extraction[i] <- doc.collapse

  #  list.terms <- vector()
  #  m<-length(input$choose_term)
  #  for (i in 1:m) {
  #    list.terms <- c(list.terms,term[[i]])
  #  }
   # doc.collapse <-paste(doc, collapse=" ")
    #ext <-  lda.list[(loc-len):(loc+len)]   # EXTRACTING SECTIONS 
   # ext.collapse <- paste(ext,collapse=" ")
   # extraction[i] <- doc.collapse
  #  }
    return(extraction)
  })
  
#Metadata Uploading
metadataPdf <- reactive ({
  if (is.null(input$file.article)) { return() }
  if ((is.null(input$metadata_pdf))|| (input$metadata_pdf=="NULL")){ return() }
  titles <-vector()
  authors <-vector()
  datetimes <- vector()
  num <-length(input$file.article$name)
  names <- vector()
  cont <- rep("",num)
  for (i in 1:num) {
    uris.name <- input$file.article$datapath[i]
    tempPDF <- readPDF(control = list(text = "-layout"))(elem = list(uri = uris.name),
                                                         language="en",id="id1")
    read.file <- tempPDF$content
    x <- enc2utf8(read.file)
if (!is.null(tempPDF$meta$heading)) {
  title <- tempPDF$meta$heading
  author <- tempPDF$meta$author # Alan Ritter ; Colin Cherry ; Bill Dolan
  des <- tempPDF$meta$datetimestamp 
  y <- strsplit(as.character(des), "-")
  datetime <- y[[1]][1]
  name <- tempPDF$meta$id
}
if (is.null(tempPDF$meta$heading)) {
 title <- tempPDF$meta$id
 name <- tempPDF$meta$id
 author <- tempPDF$meta$author
 des <- tempPDF$meta$datetimestamp 
 y <- strsplit(as.character(des), "-")
 datetime <- y[[1]][1]        
}
if (length(title)<1) {
  title <- "NA"
}
if (length(author)<1) {
  author <- "NA"
}
if (length(datetime)<1) {
  datetime <-"NA"
}
if (length(name)<1) {
  name <-input$file.article$name[i]
}
titles[i] <- title
authors[i] <-author
datetimes[i] <- datetime
names[i] <-name
}
info <- list(titles=titles, authors=authors,datetimes=datetimes, names=name)
return(info)
})

output$load_metadata_pdf <- renderUI({
  if (is.null(input$file.article)) { return() }
 names = c("NULL","Load")
  selectizeInput("metadata_pdf", label = "Read Metadata from PDF", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE
  )
}) 
## Reading Metadata File from csv
output$load_metadata_csv <- renderUI({
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
  names=c("NULL","Load")
  selectizeInput("metadata_csv", label = "Read Metadata from CSV", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE
  )
}) 
fileData <- reactive({ # loading data 
  inFile <- input$file.metadata    
  if (is.null(inFile))
    return(NULL) 
  my_data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                      quote=input$quote)
})
newData <-reactive({   # creating a dataframe with loaded data
  if (is.null(input$file.metadata)) { return() }  
  if ((is.null(input$metadata_csv))|| (input$metadata_csv=="NULL")){ return() }
  dat <- fileData() 
  return(dat)
})
### Display Metadata from CSV
output$print_metadata_csv <- renderDataTable({
  if ((is.null(input$metadata_csv))|| (input$metadata_csv=="NULL")){ return() }
  newData()#, options=list(lengthMenu = c(5, 10, 15), pageLength = 5)
})

output$print_metadata_pdf <- renderTable({
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
  if (is.null(input$metadata_pdf)){ return() }
  withProgress(message = 'Loading Metadata',
               value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  a <- metadataPdf()$authors
  t <- metadataPdf()$titles
  dt <- metadataPdf()$datetimes
  d <- data.frame(date=dt, title=t, author = a)
  
})
output$print_content <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)))   { return() }
  if ((is.null(input$select_display_content)) || (input$select_display_content=="NULL")){ return() }
  withProgress(message = 'Loading in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:60) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })

  if (input$select_display_content=="Show") {
  if (!is.null(input$file.article)) {
 # pdf.lines <- ExtractContentPDF()$text.extract
    pdf.lines <-  WindowSelect()
   # pdf.lines <- Selection()$text.extract
  }
  if (!is.null(input$file.article.txt)) {
    pdf.lines <-  WindowSelect()
  #pdf.lines <-ExtractContent()
  }
  }
# if (input$select_display_content=="Processed Content") {
#   if (!is.null(input$file.article)) {
#     pdf.lines <- ExtractContentPDF()$lda.format
#   }
#   if (!is.null(input$file.article.txt)) {
#     pdf.lines <- ExtractContentTXT()$lda.format
#   }
# }
  HTML(paste("<br/>", "Document: ",pdf.lines, sep="<br/>"))
}) 
output$print_processed <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)))   { return() }
  if ((is.null(input$select_processed)) || (input$select_processed=="NULL")){ return() }
  withProgress(message = 'Loading in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:60) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  
  if (input$select_processed=="Show") {
   # if (!is.null(input$file.article)) {
      # pdf.lines <- ExtractContentPDF()$text.extract
      pdf.lines <-  window.one()$lda.format
      # pdf.lines <- Selection()$text.extract
   # }
   # if (!is.null(input$file.article.txt)) {
     # pdf.lines <-  WindowSelect()
      #pdf.lines <-ExtractContent()
   # }
  }

  HTML(paste("<br/>", "Document: ",pdf.lines, sep="<br/>"))
}) 

### END OF DATA PREPARATION ###########

### Section Data Cleaning
output$choose_remove_punctuation <- renderUI({
  names <- c("Do not Remove Punctuation","Remove Punctuation")
  selectizeInput("remove_punctuation", label = "Select `Remove' to remove all punctuation", 
                 choices = names,
                 selected="Do not Remove Punctuation",
                 multiple = FALSE) 
}) 
output$exceptions <- renderUI({
  names <- c("NULL","Keep apostrophe", "Keep hyphen", "Keep apostrophe and hyphen")
  selectizeInput("exceptions", label = "Select exceptions: 'Keep HYPHEN and/or APOSTROPHE'", 
                 choices = names,
                 selected="NULL",
                 multiple = FALSE)
})
output$choose_remove_numbers <- renderUI({
  names <- c("Do not Remove Numbers","Remove Numbers")
  selectizeInput("remove_numbers", label = "Select `Remove' to remove all number", 
                 choices = names,
                 selected="Do not Remove Numbers",
                 multiple = FALSE) 
}) 
output$choose_lower_cases <- renderUI({
  names <- c("Do not Lower Cases","Lower Cases")
  selectizeInput("lower_cases", label = "Select `Lower Cases' to replace all capital leters by small letters", 
                 choices = names,
                 selected="Do not Lower Cases",
                 multiple = FALSE) 
}) 

output$choose_references <- renderUI({
  names <- c("Do not Remove References","Remove References")
  selectizeInput("remove_references", label = "Select `Remove References' to delete a reference section (bibliography) and citation", 
                 choices = names,
                 selected="Do not Remove References",
                 multiple = FALSE) 
}) 
output$choose_urls <- renderUI({
  names <- c("Do not Remove Urls","Remove Urls")
  selectizeInput("remove_urls", label = "Select `Remove Urls' to delete all hyperlinks from your document", 
                 choices = names,
                 selected="Do not Remove Urls",
                 multiple = FALSE) 
}) 
output$choose_html <- renderUI({
  names <- c("Do not Remove HTML","Remove HTML")
  selectizeInput("remove_html", label = "Select HTML to remove html tags", 
                 choices = names,
                 selected = NULL,
                 multiple = FALSE) 
}) 


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
### Window Processing
window.one <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)))   { return() }
text.extract <- vector()
lda.format <- vector()  
novel.list <- list()
punct.list <-vector()
novel.lda <- list()
if(!is.null(input$file.article)) {
  num <-length(input$file.article$name)
  file.names <-input$file.article$name
}
if(!is.null(input$file.article.txt)) {
  num <-length(input$file.article.txt$name)
  file.names <-input$file.article.txt$name
}

#text <- vector()
datum<- WindowSelect()
for (i in 1:num) {
  data<- datum[i]
  text <- paste(data, collapse = " ")
  text.punct <- data
if ((input$remove_html=="Do not Remove HTML") || (is.null(input$remove_html))) {
  text.punct <- text.punct
}
else if (input$remove_html=="Remove HTML") {
  text.punct <- rm_bracket(text.punct)
}

if ((is.null(input$remove_what)) || (input$remove_what=="NULL")) {
  text.punct <- text.punct
}
else if ((!is.null(input$remove_what)) && (!is.null(input$remove_by))) {
  what <- input$remove_what
  by <- input$remove_by
  text.punct <-  gsub(what, by, text.punct)
}
if ((input$lower_cases=="Do not Lower Cases") || (is.null(input$lower_cases))) {
  text.punct <- text.punct
}
else if (input$lower_cases=="Lower Cases") {
  text.punct <- tolower(text.punct)
}
if ((input$remove_numbers=="Do not Remove Numbers")|| (is.null(input$remove_numbers))) {
  text.punct <- text.punct
}
else if (input$remove_numbers=="Remove Numbers") {
  text.punct<-  gsub('[[:digit:]]+', '', text.punct)
}
if ((input$remove_punctuation=="Do not Remove Punctuation") || (is.null(input$remove_punctuation))){
  text.punct <- text.punct
}
else if (input$remove_punctuation=="Remove Punctuation") {
  if ((input$exceptions=="NULL") || (is.null(input$exceptions))) {
    text.punct <- gsub("[^[:alnum:] ]", "", text.punct) 
  }
  else if (input$exceptions=="Keep apostrophe") {
    text.punct <- gsub("-", " ", text.punct) 
    text.punct <- strip(text.punct, char.keep="~~",digit.remove = FALSE, apostrophe.remove = FALSE,
                        lower.case = FALSE)
  }
  else if (input$exceptions=="Keep hyphen") {
    text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                        lower.case = FALSE)
  }
  else if (input$exceptions=="Keep apostrophe and hyphen") {
    text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                        lower.case = FALSE)}      
}
if ((input$remove_urls=="Do not Remove Urls") || (is.null(input$remove_urls))) {
  text.punct <- text.punct
}
else if (input$remove_urls=="Remove Urls") {
  text.punct <- rm_email(text.punct)
  text.punct <- rm_url(text.punct)
  text.punct <- rm_twitter_url(text.punct)
}
if ((input$remove_references=="Do not Remove References")|| (is.null(input$remove_references))) {
  text.punct <-text.punct
}
else if (input$remove_references=="Remove References") {
  text.punct <-rm_citation(text.punct)
  text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
  text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
  text.punct <-rm_round(text.punct)
  text.punct <-rm_curly(text.punct)
  text.punct <-rm_square(text.punct)
  text.split<-strsplit(text.punct, "References|references|REFERENCES")
  #text.split <- unlist(text.p)
  text.punct<-text.split[[1]]
  
}
if ((is.null(input$remove_html_symbol)) || (input$remove_html_symbol=="NULL")) {
  text.punct <- text.punct
}
else if ((!is.null(input$remove_html_symbol)) && (input$remove_html_symbol!="NULL")){
  htm <- input$remove_html_symbol
  for (i in 1:length(input$remove_html_symbol)) {
    text.punct <- gsub(htm[i],"",text.punct)
  }
}
  if ((input$stem=="NULL") || (is.null(input$stem))) {
    text.punct <- text.punct
  }
  else if (input$stem=="Stem") {
    text.split <- unlist(strsplit(text.punct, " "))
    text.punct <- paste(wordStem(text.split, language = "english"),collapse = " ")
  }
text.punct <- gsub("\\s\\s+"," ",text.punct)
text.punct <- str_c(text.punct)
text.punct<- str_trim(text.punct)  
text.split<-strsplit(text.punct, " ")
text.split <- unlist(text.split)
data.del <- gsub("[A-Za-z0-9]"," \\1", data)
data.del.w <- paste(data.del, collapse = " ")
data.no.punct<- gsub("([!¿?;,¡:]|(\\.+))", " \\1 ", data.del.w)
data.no.punct <-  gsub("\\s+"," ",data.no.punct)
file.name <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", file.names[i])#input$file.article.txt$name[i])   
text.split<-strsplit(text.punct, " ")
text.split <- unlist(text.split)
text.freq <- table(text.split)
text.relative <- 100*(text.freq/sum(text.freq))
novel.list[[file.name]] <- text.relative
punct.list[i] <-data.no.punct
novel.lda[i] <-text.punct
lda.format[i] <- text.punct
text.extract[i] <- text
#  }
}
info<-list(data=data, novel.list=novel.list,text.extract=text.extract,punct.list=punct.list,novel.lda=novel.lda,lda.format=lda.format,data=data)
return(info)
})



##### STOP WORDS #######
output$choose_stem <- renderUI({
  names <- c("NULL","Stem")
  selectizeInput("stem", label = "Select 'Stem' to replace words by stems", 
                 choices = names,
                 selected="NULL",
                 multiple = FALSE) 
}) 

output$stops <- renderUI({
  names=c("NULL","Upload","Default")
  selectizeInput("stops", label = "Select UPLOAD or DEFAULT", 
                 choices = names,
                 selected="NULL",
                 multiple = FALSE
  )
}) 
stopWordsTxt <- reactive ({
  stop_words<-vector()
  if (input$stops=="Default") {
    stop_words <- stopwords("SMART")
  #  word <- unlist(stop_words)
  }
  if (input$stops=="Upload") {
    uris.name <- input$stopwords.txt$datapath
    text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)
    x=enc2utf8(text.scan)
    x <- tolower(x)
    text.punct <- removePunctuation(x)
    text.punct <- str_c(text.punct)
    text.punct<- str_trim(text.punct)
    text.punct <- gsub("\\s\\s+", " ", text.punct)   
   # stops<-strsplit(text.punct, " ")
    stops<-strsplit(text.punct, " ")
    stop_words<-unlist(stops)
  }
  return(stop_words)
})

output$show_stopwords <- renderUI ({
  stops <- c("NULL", "Show Stopwords")
  selectizeInput("show_stopwords", label = "Select SHOW to display stopwords", 
                 choices = stops,
                 # options = list(create = TRUE),
                 selected = NULL,
                 multiple = FALSE) 
})

output$print_stopwords <- renderPrint({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if (input$show_stopwords=="Show Stopwords") {
    stopWordsTxt()
  }
})

output$show_cutoff_lower <-renderUI ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  freq <- c(0,1,2,3,4,5)
  selectizeInput("cutoff_lower", label = "Select or Type Lower Cutoff", 
                 choices = freq,
                 options = list(create = TRUE),
                 selected = 0,
                 multiple = FALSE) 
})
output$show_cutoff_high <-renderUI ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  freq <- c(100,50,25)
  selectizeInput("cutoff_high", label = "Select or Type High Cutoff", 
                 choices = freq,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = FALSE) 
})
#########
rawFrequency <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  corpus.lda <- window.one()$lda.format
 # if(!is.null(input$file.article)) {
   # corpus.lda <- ExtractContentPDF()$lda.format
 # }
 # if(!is.null(input$file.article.txt)) {
   # corpus.lda <- ExtractContentTXT()$lda.format 
  #}
  lda.list <- strsplit(corpus.lda, "\\s+")
  terms <- table(unlist(lda.list))
  terms <- sort(terms, decreasing = TRUE)
  terms.matrix<-as.matrix(terms)
  d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
  d$word <- row.names(d)
  agg_freq <- aggregate(frequency ~ word, data = d, sum)
  d <- d[order(d$frequency, decreasing = T), ] 
  return(d)
})
output$show_freq <-renderUI ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  freq <- c("NULL", "Frequency", "Frequency with stopwords")
  selectizeInput("show_freq", label = "Select FREQUENCY", 
                 choices = freq,
                 selected = NULL,
                 multiple = FALSE) 
})

output$freq <- renderDataTable ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$show_freq)) || (input$show_freq=="NULL")){ return() }
  if (input$show_freq=="Frequency") {
   word <- rawFrequency()[[2]]
   frequency <- rawFrequency()[[1]]
    df <- data.frame(word, frequency)
  relfreq <- df$frequency/sum(df$frequency)
  d <-data.frame(df,relfreq)
  }
  if (input$show_freq=="Frequency with stopwords") {
  word <- RemoveWordsFinal()$d[[2]]
  frequency <- RemoveWordsFinal()$d[[1]]
  df <- data.frame(word, frequency)
  relfreq <- df$frequency/sum(df$frequency)
  d <-data.frame(df,relfreq)
  }
  return(d)#,options=list(lengthMenu = c(5, 10, 15), pageLength = 5))
})
output$zipf <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$show_freq)) || (input$show_freq=="NULL")){ return() }
  withProgress(message = 'Plot Creation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  tdm <- RemoveWords()$tdm
  p<- Zipf_plot(tdm, type="l")
  return(p)
})

output$heaps <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if((is.null(input$show_freq)) || (input$show_freq=="NULL")){ return() }
  withProgress(message = 'Plot Creation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  tdm <- RemoveWords()$tdm
  p<- Heaps_plot(tdm, type="l")
  return(p)
})

output$show_word <- renderUI({
  names <- c("NULL","Word Length", "Sentence Length")
  selectizeInput("show_word", label = "Select Word or Sentence Length", 
                 choices = names,
                 selected="NULL",
                 multiple = FALSE) 
}) 

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
  if ((input$show_word=="NULL")|| (is.null(input$show_word))) {return()}
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
  if ((input$show_word=="NULL")|| (is.null(input$show_word))) {return()} 
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  if (input$show_word=="Word Length") {
   tab <- LengthWord()
  }
  if (input$show_word=="Sentence Length") {
    tab <- LengthSent()$newtab
  }
  return(tab)
  })

output$words <- renderPlot ({
  if ((input$show_word=="NULL")|| (is.null(input$show_word))) {return()}
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
  if ((input$show_text=="NULL")|| (is.null(input$show_text))) {return()}
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

##### Extracting Data
ExtractContentPDF <- reactive({
  if (is.null(input$file.article))  { return() }
  text.extract <- vector()
  lda.format <- vector()
  text.list <- vector()
  novel.list <- list()
  line="One"
  num <-length(input$file.article$name)
  cont <- rep("",num)

  for (i in 1:num) {
    uris.name <- input$file.article$datapath[i]
    tempPDF <- readPDF(control = list(text = "-layout"))(elem = list(uri = uris.name),
                                                         language="en",id="id1")
    read.file <- tempPDF$content
    x <- enc2utf8(read.file)
   w<-x
   #------------- Identifying One or Two column Format -------------#

    
    locAbstract<- grep("Abstract|ABSTRACT", x)  # line location - fine "abstract line"
    if (length(locAbstract)>0) {
      w<-x[(locAbstract):length(x)]  # shorter version from Abstract to the end   
      w.test <- gsub("^\\s\\s\\s+","", w) # delete spaces at the beginning of lines
      w.test <- gsub("^\\s","", w.test)
    }
    else {
      w <-x
      w.test <- gsub("^\\s\\s\\s+","", w) # delete spaces at the beginning of lines
      w.test <- gsub("^\\s","", w.test)
    }
   
    split.testing <- gsub("\\s\\s\\s+","SPLIT", w.test)
    if (grepl("SPLIT",split.testing)==FALSE) {   # ONE COLUMN DOCUMENT
      line="FALSE"
     # x <-gsub("\\f","",x)
      w <-gsub("\\f","",w)
      if ((input$article_content =="Full text") || (is.null(input$article_content))){
        lines = w
      }
      if (input$article_content =="Abstract"){
        loc.a<- grep("Abstract|ABSTRACT", w)
        loc.b <- grep("Introduction|INTRODUCTION",w)
        lines =  w[(loc.a+1):(loc.b-1)]   # EXTRACTING SECTIONS 
      }
      test.columns <- gsub("^\\s\\s\\s+","", lines)
      test.columns <- gsub("^\\s","", test.columns)
      text.collapse <- paste(test.columns, collapse=" ")
      text.collapse <- enc2utf8(text.collapse)
      text.hyphen <- gsub("-\\s+", "", text.collapse)
      text.punct <- gsub("\\s\\s+", " ", text.hyphen) 
   text  <- text.punct
      if ((input$lower_cases=="Do not Lower Cases") || (is.null(input$lower_cases))){
        text.punct <- text.punct
      }
      else if (input$lower_cases=="Lower Cases") {
        text.punct <- tolower(text.punct)
      }
      if ((input$remove_numbers=="Do not Remove Numbers") || (is.null(input$remove_numbers))){
        text.punct <- text.punct
      }
      else if (input$remove_numbers=="Remove Numbers") {
        text.punct<-  gsub('[[:digit:]]+', '', text.punct)
      }
   if ((input$remove_punctuation=="Do not Remove Punctuation")|| (is.null(input$remove_punctuation))) {
     text.punct <- text.punct
   }
      else if (input$remove_punctuation=="Remove Punctuation") {
        if (input$exceptions=="NULL") {
          text.punct <- gsub("[^[:alnum:] ]", "", text.punct) 
        }
        else if (input$exceptions=="Keep apostrophe") {
          text.punct <- gsub("-", " ", text.punct) 
          text.punct <- strip(text.punct, char.keep="~~",digit.remove = FALSE, apostrophe.remove = FALSE,
                              lower.case = FALSE)
        }
        else if (input$exceptions=="Keep hyphen") {
          text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                              lower.case = FALSE)
        }
        else if (input$exceptions=="Keep apostrophe and hyphen") {
          text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                              lower.case = FALSE)}      
      }
   if ((input$remove_urls=="Do not Remove Urls") || (is.null(input$remove_urls))){
     text.punct <- text.punct
   }
      else if (input$remove_urls=="Remove Urls") {
        text.punct <- rm_email(text.punct)
        text.punct <- rm_url(text.punct)
        text.punct <- rm_twitter_url(text.punct)
      }
   if ((input$remove_references=="Do not Remove References") || (is.null(input$remove_references))){
     text.punct <-text.punct
   }
      else if (input$remove_references=="Remove References") {
        text.punct <-rm_citation(text.punct)
        text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
        text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
        text.punct <-rm_round(text.punct)
        text.punct <-rm_curly(text.punct)
        text.punct <-rm_square(text.punct)
        text.split<-strsplit(text.punct, "References|references|REFERENCES")
        text.punct<-text.split[[1]]
        
      }
   if ((input$stem=="NULL")|| (is.null(input$stem))) {
     text.punct<-text.punct
   }
      else if (input$stem=="Stem") {
        text.splits <- unlist(strsplit(text.punct, " "))
        text.punct <- paste(wordStem(text.splits, language = "english"),collapse = " ")
      }
      text.punct <- str_c(text.punct)
      text.punct<- str_trim(text.punct)
      text.punct <- which(text.punct!="")
      lda.format[i] <- text.punct
      text.extract[i] <- text
      file.name <- input$file.article$name[i]
      text.freq <- table(text.punct)#split)
      text.relative <- 100*(text.freq/sum(text.freq))
      # # novel.list[i] <- text.punct # for stm
      novel.list[[file.name]] <- text.relative
    } # END ONE COLUMN
    # ### START TWO COLUMNS
    if (grepl("SPLIT",split.testing)==TRUE) {  # TWO-COLUMN DOCUMENT  
      line="TRUE"
      #### Splitting by Pages and Columns, then pasting each column separately and pasting pages together
      w.col <- gsub("^\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s\\s+","COLUMN2", w)
      text.split <- gsub("^\\s\\s\\s+","", w.col)
      text.split <-gsub("COLUMN2Abstract","Abstract",text.split)
      text.split <-gsub("\\s\\s\\s+","SPLIT", text.split)
      text.collapse <- paste(text.split, collapse="NEWLINE")    
      text.vector <- lapply(text.collapse, FUN=function(x){strsplit(x, "\\f")})  # split fy page
      cols1 <-vector()
      cols2<-vector()
      pages.all <-vector()
      pages.papers <- vector()
      page.columns <- vector()
      for (l in 1:length(text.vector[[1]][[1]])) {   # PAGES
        new.line <- strsplit(text.vector[[1]][[1]][l], "NEWLINE")   # PAGE ONE
        for (j in 1:length(new.line[[1]])) {   # LOOP OVER FIRST PAGE LINES
          mid.split <- as.list(strsplit(new.line[[1]][j],"SPLIT"))   # SPLIT THE FIRST LINE
          substring="COLUMN2"
          if (grepl(substring, mid.split)==TRUE) { # FIND EMPTY 1st COLUMN
            col2 = mid.split[1]
            col2 = gsub("COLUMN2","",col2)
            col1 = ""
          }
          if (grepl(substring, mid.split)==FALSE) {  # FIND FULL TWO COLUMNS
            if (length(mid.split[[1]]>1)) {
              col1 <- mid.split[[1]][1]
              col2 <-mid.split[[1]][2]
            }
            if (length(mid.split[[1]]<2)) {
              col1 <- mid.split[[1]][1]
              col2 <-""
            }
          }
          cols1[j]<-col1   # Collecting all lines for 1st column
          cols2[j]<-col2    # Collecting lines for 2nd column   
        }  # End of ONE PAGE
        column1 <- paste(cols1, collapse= " ")   # joing lines for the 1st column
      #  column1 <-gsub("- ", "", column1)
        column1 <-gsub("- ", "-", column1)
        column2 <-paste(cols2, collapse=" ")  # joing lines for the 2nd column
        column2 <-gsub("- ", "-", column2) 
     # column2 <-gsub("- ", "", column2) 
        columns <- paste(column1, column2, collapse=" ")  # joing two columns on the same page       
        pages.all[l]<-columns
        full.page <- paste(pages.all, collapse=" ")
        if ((input$article_content =="Full text")||(is.null(input$article_content))){
          lines <- full.page
        }
        if (input$article_content =="Abstract"){
          loc.a <- str_extract(full.page, "Abstract.+Introduction")
          loc.a <- gsub("1.Introduction","",loc.a[1])
          lines=gsub("^\\s\\s\\s+","", loc.a)
        }  
        text.punct <- gsub("\\s*\\([^\\)]+\\)", "", lines)#clean)
        text.punct <- gsub("^\\s","",text.punct)
        text.punct <- gsub("\\s\\s+"," ",text.punct)
      text <- text.punct
      if ((input$lower_cases=="Do not Lower Cases")||(is.null(input$lower_cases))) {
        text.punct <- text.punct
      }
      else if (input$lower_cases=="Lower Cases") {
        text.punct <- tolower(text.punct)
      }
    if ((input$remove_numbers=="Do not Remove Numbers")|| (is.null(input$remove_numbers))) {
      text.punct <- text.punct
    }
     else if (input$remove_numbers=="Remove Numbers") {
        text.punct<-  gsub('[[:digit:]]+', '', text.punct)
      }
    if ((input$remove_punctuation=="Do not Remove Punctuation")||(is.null(input$remove_punctuation))) {
      text.punct <- text.punct
    }
      else if (input$remove_punctuation=="Remove Punctuation") {
        if (input$exceptions=="NULL") {
          text.punct <- gsub("[^[:alnum:] ]", "", text.punct) 
        }
        else if (input$exceptions=="Keep apostrophe") {
          text.punct <- gsub("-", " ", text.punct) 
          text.punct <- strip(text.punct, char.keep="~~",digit.remove = FALSE, apostrophe.remove = FALSE,
                              lower.case = FALSE)
        }
        else if (input$exceptions=="Keep hyphen") {
          text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                              lower.case = FALSE)
        }
        else if (input$exceptions=="Keep apostrophe and hyphen") {
          text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                              lower.case = FALSE)}      
      }
    if ((input$remove_urls=="Do not Remove Urls") || (is.null(input$remove_urls))){
      text.punct <-  text.punct
    }
     else if (input$remove_urls=="Remove Urls") {
        text.punct <- rm_email(text.punct)
        text.punct <- rm_url(text.punct)
        text.punct <- rm_twitter_url(text.punct)
      }
      
    if ((input$remove_references=="Do not Remove References") ||(is.null(input$remove_references))) {
      text.punct <-  text.punct
    }
     else if (input$remove_references=="Remove References") {
        text.punct <-rm_citation(text.punct)
        text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
        text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
        text.punct <-rm_round(text.punct)
        text.punct <-rm_curly(text.punct)
        text.punct <-rm_square(text.punct)
        text.split<-strsplit(text.punct, "References|references|REFERENCES")
        text.punct<-text.split[[1]]
        
      }
    if ((input$stem=="NULL") || (is.null(input$stem))){
      text.punct<-text.punct
    }
     else if (input$stem=="Stem") {
        text.split <- unlist(strsplit(text.punct, " "))
        text.punct <- paste(wordStem(text.split, language = "english"),collapse = " ")
      }
  
        text.punct <- gsub("\\s*\\([^\\)]+\\)", "", text.punct)#clean)
        text.punct <- gsub("^\\s","",text.punct)
        text.punct <- gsub("\\s\\s+"," ",text.punct)

        text.punct <- str_c(text.punct)
        text.punct<- str_trim(text.punct)
        file.name <- input$file.article$name[i]
        text.split<-strsplit(text.punct, " ")
        text.split <- unlist(text.split)
        text.freq <- table(text.split)
        text.relative <- 100*(text.freq/sum(text.freq))
        # novel.list[i] <- text.punct # for stm
        novel.list[[file.name]] <- text.relative
        lda.format[i] <- text.punct
        text.extract[i] <- text
    
      }  # End TWO-COLUMN
    }
  }
    info <- list(lda.format = lda.format, text.extract = text.extract,novel.list=novel.list)
    return(info)  
  })  

ExtractContent <- reactive ({
  if (is.null(input$file.article.txt)) { return() }
  num <-length(input$file.article.txt$name)
  textum <- vector()
  for (i in 1:num) {
    uris.name <- input$file.article.txt$datapath[i]
    text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)  
    data=enc2utf8(text.scan)
    text <- paste(data, collapse = " ")
   # text <- gsub("^\\s\\s\\s+","",text)
    text <-gsub("-\\s+", "", text)
    textum[i] <- text
  } 
 # len <- length(textum)
  return(textum)
  
})
ExtractContentTXT <- reactive ({
  if (is.null(input$file.article.txt)) { return() }
  text.extract <- vector()
  lda.format <- vector()  
  novel.list <- list()
  punct.list <-vector()
  novel.lda <- list()
  num <-length(input$file.article.txt$name)
  text <- vector()
  datum<- ExtractContent()
  for (i in 1:num) {
  data<- datum[i]
    text <- paste(data, collapse = " ")
  if (input$article_content =="Abstract") {
    text1 <- str_replace(text, "Abstract|ABSTRACT|abstract", "mystringreplacement")
    text1 <- sub(".*mystringreplacement","",text1)
  #  text1 <- sub(".*(Abstract|ABSTRACT|abstract)","",text)
    text.punct<-sub("(1.)?.?Introduction.*","",text1)    
    text.punct<- rm_white(text.punct)
#    text.punct <- ex_between(text,"Abstract|ABSTRACT","Introduction|INTRODUCTION")
 # text.punct <-text
  }
  else {
    text.punct <-text
  }
   
    
    if ((input$remove_html=="Do not Remove HTML") || (is.null(input$remove_html))) {
      text.punct <- text.punct
    }
    else if (input$remove_html=="Remove HTML") {
      text.punct <- rm_bracket(text.punct)
    }
    
    if ((is.null(input$remove_what)) || (input$remove_what=="NULL")) {
      text.punct <- text.punct
    }
    else if ((!is.null(input$remove_what)) && (!is.null(input$remove_by))) {
      what <- input$remove_what
      by <- input$remove_by
      text.punct <-  gsub(what, by, text.punct)
    }
  if ((input$lower_cases=="Do not Lower Cases") || (is.null(input$lower_cases))) {
    text.punct <- text.punct
  }
  else if (input$lower_cases=="Lower Cases") {
    text.punct <- tolower(text.punct)
  }
    if ((input$remove_numbers=="Do not Remove Numbers")|| (is.null(input$remove_numbers))) {
      text.punct <- text.punct
    }
   else if (input$remove_numbers=="Remove Numbers") {
      text.punct<-  gsub('[[:digit:]]+', '', text.punct)
    }
   if ((input$remove_punctuation=="Do not Remove Punctuation") || (is.null(input$remove_punctuation))){
      text.punct <- text.punct
    }
    else if (input$remove_punctuation=="Remove Punctuation") {
      if ((input$exceptions=="NULL") || (is.null(input$exceptions))) {
        text.punct <- gsub("[^[:alnum:] ]", "", text.punct) 
      }
      else if (input$exceptions=="Keep apostrophe") {
        text.punct <- gsub("-", " ", text.punct) 
        text.punct <- strip(text.punct, char.keep="~~",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)
      }
      else if (input$exceptions=="Keep hyphen") {
        text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)
      }
      else if (input$exceptions=="Keep apostrophe and hyphen") {
        text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)}      
    }
if ((input$remove_urls=="Do not Remove Urls") || (is.null(input$remove_urls))) {
  text.punct <- text.punct
}
    else if (input$remove_urls=="Remove Urls") {
      text.punct <- rm_email(text.punct)
      text.punct <- rm_url(text.punct)
      text.punct <- rm_twitter_url(text.punct)
    }
    if ((input$remove_references=="Do not Remove References")|| (is.null(input$remove_references))) {
      text.punct <-text.punct
    }
   else if (input$remove_references=="Remove References") {
      text.punct <-rm_citation(text.punct)
      text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
      text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
      text.punct <-rm_round(text.punct)
      text.punct <-rm_curly(text.punct)
      text.punct <-rm_square(text.punct)
      text.split<-strsplit(text.punct, "References|references|REFERENCES")
      #text.split <- unlist(text.p)
      text.punct<-text.split[[1]]
      
    }
    if ((input$stem=="NULL") || (is.null(input$stem))) {
      text.punct <- text.punct
    }
else if (input$stem=="Stem") {
  text.split <- unlist(strsplit(text.punct, " "))
  text.punct <- paste(wordStem(text.split, language = "english"),collapse = " ")
}
     if ((!is.null(input$speaker_name)) && (input$speaker_name!="NULL")) {
      name.speaker <- input$speaker_name
      name.interviewer <- input$interviewer_name
      new.line <- gsub(name.speaker, "NEWLINE SPEAKER",text.punct)
      new.line <- gsub(name.interviewer, "NEWLINE INTERVIEWER",new.line)
      split.sub <- unlist(strsplit(new.line,"NEWLINE"))
      speaker <-grep("SPEAKER", split.sub)
      text.sub <- split.sub[speaker]
      text.sub <- gsub("SPEAKER","",text.sub)
      text.punct <- paste(text.sub, collapse = " ")
     }
    else {
      text.punct <- text.punct
    }
    
    if ((is.null(input$remove_html_symbol)) || (input$remove_html_symbol=="NULL")) {
      text.punct <- text.punct
    }
    else if ((!is.null(input$remove_html_symbol)) && (input$remove_html_symbol!="NULL")){
      htm <- input$remove_html_symbol
      for (i in 1:length(input$remove_html_symbol)) {
      text.punct <- gsub(htm[i],"",text.punct)
      }
    }
   # else if ((is.null(input$speaker_name)) && (is.null(input$interviewer_name))) { 
    text.punct <- gsub("\\s\\s+"," ",text.punct)
    text.punct <- str_c(text.punct)
    text.punct<- str_trim(text.punct)  
    text.split<-strsplit(text.punct, " ")
    text.split <- unlist(text.split)
    data.del <- gsub("[A-Za-z0-9]"," \\1", data)
    data.del.w <- paste(data.del, collapse = " ")
    data.no.punct<- gsub("([!¿?;,¡:]|(\\.+))", " \\1 ", data.del.w)
    data.no.punct <-  gsub("\\s+"," ",data.no.punct)
    file.name <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", input$file.article.txt$name[i])   
    text.split<-strsplit(text.punct, " ")
    text.split <- unlist(text.split)
    text.freq <- table(text.split)
    text.relative <- 100*(text.freq/sum(text.freq))
    novel.list[[file.name]] <- text.relative
    punct.list[i] <-data.no.punct
    novel.lda[i] <-text.punct
    lda.format[i] <- text.punct
    text.extract[i] <- text
  #  }
  }
  info<-list(novel.list=novel.list,text.extract=text.extract,punct.list=punct.list,novel.lda=novel.lda,lda.format=lda.format,data=data)
  return(info)
})

#Allows for interactive instant word removals from user
RemoveWordsNew <-reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  cutoff.lower=input$cutoff_lower
  cutoff.high=input$cutoff_high
 # if(!is.null(input$file.article)) {
   # corpus.lda <- ExtractContentPDF()$lda.format
    corpus.lda <- window.one()$lda.format 
 # }
 # if(!is.null(input$file.article.txt))  {
  #  corpus.lda <- ExtractContentTXT()$lda.format 
 #   corpus.lda <- window.one()$lda.format 
 # }

if  ((is.null(input$stops))||(input$stops=="NULL")) {
    lda.list <- strsplit(corpus.lda, "\\s+")
    terms <- table(unlist(lda.list))
    terms <- sort(terms, decreasing = TRUE)
  #  empty <- names(terms) %in% names(terms)==""
    del <- names(terms) %in%  terms < cutoff.lower
    terms <- terms[!del]
    terms.matrix<-as.matrix(terms)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    d <- d[order(d$frequency, decreasing = T), ] 
    corpus.lda <- as.list(d$word)
    vector.lda <- as.vector(unlist(corpus.lda))
  }
  else if  ((input$stops=="Default")||(input$stops=="Upload")) {
    remove_word <- stopWordsTxt()
    lda.list <- strsplit(corpus.lda, "\\s+")
    terms <- table(unlist(lda.list))
    terms <- sort(terms, decreasing = TRUE)
    del <- names(terms) %in% remove_word | terms < cutoff.lower
    terms <- terms[!del]
    terms.matrix<-as.matrix(terms)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    d <- d[order(d$frequency, decreasing = T), ] 
    corpus.lda <- as.list(d$word)
    vector.lda <- as.vector(unlist(corpus.lda))
  } 
  

  info <- list(d=d, corpus.lda=corpus.lda,vector.lda=vector.lda)
  return(info)
})

RemoveWordsFinal <-reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }

  if(is.null(input$remove_words)) {
    corpus.lda <- RemoveWordsNew()$corpus.lda
    terms <- table(unlist(corpus.lda))
    terms <- sort(terms, decreasing = TRUE)
    terms.matrix<-as.matrix(terms)
   d <-RemoveWordsNew()$d
    list.lda <- as.list(d$word)
    vector.lda <- as.vector(unlist(list.lda))
  }

  if(!is.null(input$remove_words)) {
    corpus.lda <- window.one()$lda.format
   # if(!is.null(input$file.article)) {
    #  corpus.lda <- ExtractContentPDF()$lda.format
    #}
    #if(!is.null(input$file.article.txt)) {
     # corpus.lda <- ExtractContentTXT()$lda.format 
    #}
    cutoff.lower=input$cutoff_lower
    cutoff.high=input$cutoff_high
    corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
    corpus <- Corpus(VectorSource(corpus.lda))
    remove_word <- stopWordsTxt()
    lda.list <- strsplit(corpus.lda, "\\s+")
    terms <- table(unlist(lda.list))
    terms <- sort(terms, decreasing = TRUE)
    del <- names(terms) %in% remove_word | terms < cutoff.lower
    terms <- terms[!del]
    terms.matrix<-as.matrix(terms)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    d <- d[order(d$frequency, decreasing = T), ] 
    list.lda <- as.list(d$word)
    vector.lda <- as.vector(unlist(corpus.lda))
  }
 
  info <- list(d=d, list.lda=list.lda,vector.lda=vector.lda)
  return(info)
})

RemoveWords <- reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  corpus.lda <- window.one()$lda.format 
# if(!is.null(input$file.article)) {
 #  corpus.lda <- ExtractContentPDF()$lda.format
# }
 #if(!is.null(input$file.article.txt)) {
  # corpus.lda <- ExtractContentTXT()$lda.format 
 #}
  corpus <- Corpus(VectorSource(corpus.lda))
  tdm <- TermDocumentMatrix(corpus)
  terms.matrix <- as.matrix(tdm)
  d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
  d$word <- row.names(d)
  agg_freq <- aggregate(frequency ~ word, data = d, sum)
  d <- d[order(d$frequency, decreasing = T), ] 
  info <- list(d=d,corpus.lda=corpus.lda,tdm=tdm)
  return(info)
})

output$choose_top <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  selectizeInput("top", label = "Select a number for top frequent words (ex. 10 top frequent words)", 
                 choices = c(10,20,30,40,50,60,70,80,90,100),
                 options = list(create = TRUE),
                 selected=10,
                 multiple = FALSE) 
}) 
output$choose_remove <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  word <- RemoveWordsNew()$d[[2]]#[1:x]
  selectizeInput("remove_words", label = "Select words to be removed", 
                 choices = word,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = TRUE) 
}) 

output$word_count <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  x <- input$top
#  word <- RemoveWordsNew()$d[[2]][1:x]
#  frequency <- RemoveWordsNew()$d[[1]][1:x]
  word <- RemoveWordsFinal()$d[[2]][1:x]
  frequency <- RemoveWordsFinal()$d[[1]][1:x]
  plot <-barplot(frequency, names.arg=word, las=2,cex.names=0.8)
  return(plot)
})

output$choose_cloud <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  punct <- c("NULL", "Show")
  selectizeInput("choose_cloud", label = "Select Show to perform visualization", 
                 choices = punct,
                 selected = FALSE,
                 multiple = FALSE) 
})
output$choose_min_frequency <- renderUI({
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  d <- RemoveWordsFinal()$d
  sliderInput("min",
              "Minimum Frequency:",
              min = min(d$freq),
              max = max(d$freq),
              value = min(d$freq)+2)             
})
output$choose_max_words <- renderUI({
  if ((is.null(input$file.article)) & (is.null(input$file.article.txt))) { return() }
  if ((is.null(input$choose_cloud)) || (input$choose_cloud=="NULL")) { return() }
  d <- RemoveWordsFinal()$d
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
  withProgress(message = 'Plotting in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  wordcloud_rep <- repeatable(wordcloud)
  d <- RemoveWordsFinal()$d
  wordcloud_rep(d$word, d$freq, scale=c(8,0.2),ordered.colors=T,
                rot.per=.15,#c(8,0.3),
                min.freq = input$min,# 
               random.order=FALSE,
                max.words=input$max,#100,#input$freq, max.words=input$max,
               #colors=brewer.pal(6, "Dark2"))
                colors="black")
 # wordcloud(d$word, d$freq)
})

output$printWords <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  withProgress(message = 'Loading in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  HTML(paste0(RemoveWordsNew()$corpus.lda))
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
  corpus.lda <- window.one()$lda.format 
  #if(!is.null(input$file.article)) {
  #  corpus.lda <- ExtractContentPDF()$lda.format
  #}
  #if(!is.null(input$file.article.txt)) {
  #  corpus.lda <- ExtractContentTXT()$lda.format #novel.list
  #}
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
  cutoff.lower=input$cutoff_lower
  cutoff.high=input$cutoff_high
  if(!is.null(input$file.article)) {
    novel.vector <- window.one()$lda.format 
  #  novel.vector <- ExtractContentPDF()$lda.format
   # num.documents <- length(ExtractContentPDF()$lda.format)
    num.documents <- length(window.one()$lda.format)
    file.names <- input$file.article$name  
  }
  if(!is.null(input$file.article.txt)) {
    novel.vector <- window.one()$lda.format 
   # novel.vector <- ExtractContentTXT()$lda.format #novel.list
    num.documents <- length(window.one()$lda.format)
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
  cutoff.lower=input$cutoff_lower
  cutoff.high=input$cutoff_high
  if(!is.null(input$file.article)) {
    corpus.lda <-  window.one()$lda.format
    num.documents <- length(window.one()$lda.format)
   # corpus.lda <- ExtractContentPDF()$lda.format
   # num.documents <- length(ExtractContentPDF()$lda.format)
    n.docs <- as.numeric(length(input$file.article$name))  
  }
  if(!is.null(input$file.article.txt)) {
    corpus.lda <-  window.one()$lda.format
    num.documents <- length(window.one()$lda.format)
   # corpus.lda <- ExtractContentTXT()$lda.format #novel.list
  #  num.documents <- length(ExtractContentTXT()$lda.format)
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
 # novel.vector <- RemoveWordsFinal()$corpus.lda
  remove.words.file <- stopWordsTxt()
  novel.vector <- window.one()$lda.format
 # novel.vector <- ExtractContentTXT()$lda.format
 # corpus.lda <- removeWords(novel.vector, remove.words.file)
 # corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
 # newtext <- lapply(corpus.lda, function(x) gsub(" +", " ", x))
  corpus <- Corpus(VectorSource(novel.vector))
#  corpus <- Corpus(VectorSource(newtext))
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
#if(!is.null(input$file.article)) {
  novel.vector <-window.one()$lda.format
  #novel.vector <- ExtractContentPDF()$lda.format
#}
#if(!is.null(input$file.article.txt)) {
 # novel.vector <- ExtractContentTXT()$lda.format #novel.list
#}
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
 # if(!is.null(input$file.article)) {
   # novel.list <- ExtractContentPDF()$novel.list
    novel.list <-window.one()$novel.list
 # }
 # if(!is.null(input$file.article.txt)) {
  #  novel.list <- ExtractContentTXT()$novel.list #novel.list  
  #}
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
  # punct <- ExtractContentTXT()$data.punct
  text <-window.one()$text.extract
  #if(!is.null(input$file.article)) {
   # text <-ExtractContentPDF()$text.extract
 # }
 # if(!is.null(input$file.article.txt)) {
   # text <-ExtractContentTXT()$text.extract#text.extract #novel.list
  #}
  data.del <- gsub("[A-Za-z0-9]"," \\1", text) # only punctuation is left with space
  data.punct <-  gsub("\\s+","",data.del)
  punctuation <- unlist(strsplit(data.punct, ""))
  tab <-table(punctuation)
  m <- as.data.frame(tab)
  #  y <- count(punct.frame)
  #   #y[[1]] - level
  #   #y[[2]]  - frequency    
  #   d <- y[order(y[[2]], decreasing = T), ] 
  
  # info <- list(data.punct=data.punct, punct.frame=punct.frame, y=y,data.split=data.split)
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
 # if(!is.null(input$file.article)) {
  text <-window.one()$text.extract
   # text <-ExtractContentPDF()$text.extract
 # }
  #if(!is.null(input$file.article.txt)) {
  #  text <-ExtractContentTXT()$text.extract #novel.list
 # }
  # text <-ExtractContentTXT()$text.extract
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

})