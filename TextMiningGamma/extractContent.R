# function to extract text content (without processing from a txt document)
#uris.name <- "A context-based model for Sentiment Analysis in Twitter.txt"
#uris.name <- "Beyond Twitter Text: A Preliminary Study on Twitter Hyperlink and its Application.txt"
#x<-file.choose()
#num=1
#y <-extractContentTxt(x)
extractContentTxt <- function (x){
  num <-length(x$name)
  text.extract <- list()
  for (i in 1:num) {
    uris.name <- x$datapath[i]
    text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = TRUE) 
    texts <- enc2utf8(text.scan)
    text.collapse<-paste(texts,collapse=" ")
    text.hyphen <- gsub("-\\s+","",text.collapse)
     text.space<- gsub("\\s\\s+"," ",text.hyphen)
    text.extract[[i]] <-text.space
  } 
  text.extract <- unlist(text.extract)
  return(text.extract)
}

## extract content from pdf file
extractContentPdf <- function (x) {
num <-length(x$name)
text.extract <- vector()
for (i in 1:num) {
  uris.name <- x$datapath[i] #info="-f"
  tempPDF <- readPDF(control = list(text = "-layout"))(elem = list(uri = uris.name),language="en",id="id1")
  read.file <- tempPDF$content
 # meta.extract <-tempPDF$meta$heading
  texts <- enc2utf8(read.file)
  text.collapse <- paste(texts, collapse=" ")
  text.collapse <- enc2utf8(text.collapse)
  text.hyphen <- gsub("-\\s+", "", text.collapse)
  text.space <- gsub("\\s\\s+", " ", text.hyphen) 
  text.extract[i] <- text.space
}
return(text.extract)
}

# extractZotero <- function(x) {
#   
# }
# uris.name <- file.choose()
# 
# pattern <- "sentiment"
# tempPDF <- readPDF(control = list(text = "-layout"))(elem = list(uri = uris.name),language="en",id="id1")
# texts <- enc2utf8(read.file)
# read.file <- tempPDF$content
# texts <- enc2utf8(read.file)
# 
# text.extract <- vector()
# if (grepl("Twitter", texts)) {
#   text.collapse <- paste(texts, collapse=" ")
#   text.collapse <- enc2utf8(text.collapse)
#   text.hyphen <- gsub("-\\s+", "", text.collapse)
#   text.space <- gsub("\\s\\s+", " ", text.hyphen) 
#   text.extract[i] <- text.space
# }
# 
# reg <- regexpr(pattern, text, ignore.case = TRUE)
# 
# reg <- grepl(text, pattern,ignore.case = TRUE)
# if reg==TRUE

