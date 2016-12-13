# parse rdf zotero files
# shoudl have subfolders with files
#inputFile <- "~/Documents/MCS/TextMiningClass/Project/TestingFiles/EuropeanInfluencecopy.rdf"
#inputFile <- "~/Documents/MCS/TextMiningClass/Project/TestingFiles/EuropeanInfluencecopy.rdf"
#inputFile <- gsub("EuropeanInfluencecopy.rdf","",inputFile)
 #f <- "files/137484/Auby - 2014 - About Europeanization of Domestic Judicial Review.pdf"
#z<- paste0(inputFile,f)
#z <- getwd()
#unlist(strsplit(z,""))[1]
#zotero <- function(x) {
   zotero <- function(x) {
#   #  uris.name <- "EuropeanInfluencecopy.rdf"
     uris.name <- x
   #  zot.lines <- scan(uris.name, what="character", sep="\n",blank.lines.skip = TRUE) 
# #zot.file <-x$datapath# "/Users/majdavis/Downloads/European Influence/EuropeanInfluence.rdf"
# #zot.file<-file.choose()
# #Get all the lines of interest in the file
 zot.lines <- readLines(uris.name)#zot.file)
 #zot.lines <- zot.lines[grepl("<rdf:resource", zot.lines)]
 zot.lines <- zot.lines[grep("<rdf:resource",zot.lines)]
 #zot.lines <- zot.lines[grepl(".pdf\"", zot.lines)]

zot.line.parser <- function(z){
  #This takes one of our rdfresource lines and returns the pdf title
  #This relies on the rdf:resource tag having exactly two double quotes
  #Seems safe enough, but just to be safe we take the first and last quote mark
  #This avoids issues with quotes in a title/file name.
  require(stringr)
  
  first <- 1
  last <- dim(str_locate_all(z,'"')[[1]])[1]
  
  start <- str_locate_all(z,'"')[[1]][first] + 1
  stop <- str_locate_all(z[1],'"')[[1]][last] -1
  
  substr(z, start, stop)
}

zot.pdf <- unlist(lapply(zot.lines, zot.line.parser))

}

#z <- zotero("EuropeanInfluencecopy.rdf")

#  pattern1 <- "law"
#  pattern2 <- "human"

extractZotero <- function(z,y){#},p1,p2) {
  path = y
 #  pattern1 <- p1#"law"
 #  pattern2 <- p2#"human"
  # dirfile <- gsub(d$name,"",d)
#dirfile <- d
  # path <- paste(dirfile,z,collapse="")
 #  num <- 1
#z <- "files/137482/Giliker - 2015 - The Influence of Eu and European Human Rights Law .pdf"
   #z=as.character("Auby - 2014 - About Europeanization of Domestic Judicial Review.pdf")
   num <-length(z)
  # getwd()
   #setwd()
  # d <- as.character("/Users/olgascrivner/Documents/MCS/TextMiningClass/Project/TestingFiles")
    text.extract <- list()
    for (i in 1:num) {
      uris <- z[i]
      #sprintf("file://%s", system.file(file.path("doc", "tm.pdf"), package = "tm"))
    #  setwd("/Users/olgascrivner/Documents/MCS/TextMiningClass/Project/TestingFiles/files/137484/")
      uris.name <-"file:///Users/olgascrivner/Documents/MCS/TextMiningClass/Project/TestingFiles/files/137482/Giliker - 2015 - The Influence of Eu and European Human Rights Law .pdf"
      # f <- "files/137484/Auby - 2014 - About Europeanization of Domestic Judicial Review.pdf"
    #  backslash <- unlist(strsplit(d,""))[1]
     # uris.name<- paste0(d,backslash,uris)
     # uris.name <- paste(dirfile,uris,collapse="")
      tempPDF <- readPDF(control = list(text = "-layout"))(elem = list(uri = uris.name),language="en",id="id1")
      read.file <- tempPDF$content
     # tempPDF$meta
      texts <- enc2utf8(read.file)
      # pat1 <- grepl(pattern1, texts, ignore.case = TRUE)
      #pat2 <- grepl(pattern2, texts, ignore.case = TRUE)
    # p1p2 <- grepl(pattern1, texts, ignore.case = TRUE) & grepl(pattern2, texts, ignore.case = TRUE)
      #if((pat1==TRUE) &&(pat2==TRUE))  {
   #   if ((is.na(pat1)) &&(is.na(pat2)))  {
      text.collapse<-paste(texts,collapse=" ")
      text.hyphen <- gsub("-\\s+","",text.collapse)
      text.space<- gsub("\\s\\s+"," ",text.hyphen)
      text.extract[[i]] <-text.space
     # }
    text.extract <- unlist(text.extract)
   # return(text.extract)
    return(path)
  }
  
}

