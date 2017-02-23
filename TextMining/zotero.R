# parse rdf zotero files
   zotero <- function(x) {
     uris.name <- x
# #Get all the lines of interest in the file
 zot.lines <- readLines(uris.name)#zot.file)
 zot.lines <- zot.lines[grep("<rdf:resource",zot.lines)]
 #zot.lines <- zot.lines[grepl(".pdf\"", zot.lines)]
 zot.lines <-  zot.lines[grep("\\.pdf", zot.lines)]
zot.line.parser <- function(z){
  #This takes one of our rdfresource lines and returns the pdf title
  #This relies on the rdf:resource tag having exactly two double quotes
  #Seems safe enough, but just to be safe we take the first and last quote mark
  #This avoids issues with quotes in a title/file name.
  require(stringr)
  
  first <- 1
  last <- dim(str_locate_all(z,'"')[[1]])[1]
  
  start <- str_locate_all(z,'"')[[1]][first] + 1
  stop <- str_locate_all(z[1],'"')[[1]][last] - 1
  
  substr(z, start, stop)
}

zot.pdf <- unlist(lapply(zot.lines, zot.line.parser))

   }
   
zoteroMeta <- function(x) {
  uris.name <- x
  # #Get all the lines of interest in the file
  zot.lines <- readLines(uris.name)#zot.file)
  date.lines <- zot.lines[grep("<dc:date",zot.lines)]
  author.lines <- zot.lines[grep("<foaf:surname",zot.lines)]
  abstract.lines <- zot.lines[grep("<dcterms:abstract",zot.lines)]
  date.lines <- gsub("<dc:date>","",date.lines)
  date.lines <- gsub("</dc:date>","",date.lines)
  date.lines <- gsub("[a-zA-Z\\s]*","",date.lines)
  date.lines <- as.integer(date.lines)
  author.lines <- gsub("<foaf:surname>","",author.lines)
  author.lines <- gsub("</foaf:surname>","",author.lines)
  author.lines <- gsub("\\s+","",author.lines)
  abstract.lines <- gsub("<dcterms:abstract>","",abstract.lines)
  abstract.lines <- gsub("</dcterms:abstract>","",abstract.lines)
  abstract.lines <- gsub("\\s+"," ",abstract.lines)
  info <- list(date.lines,abstract.lines,author.lines,zot.pdf)
  return(info)
}  

extractPdfZotero <- function(list.rdf){
  list.rdf <- list.rdf
  num <- length(list.rdf)
  titles <-vector()
  authors <-vector()
  datetimes <- vector()
  names <- vector()
 # cont <- rep("",num)
  text.extract <- list()
  for (i in 1:num) {
    uris.name <- as.character(paste0(list.rdf[i]))
    # if (nchar(uris.name)<100) {
      tempPDF <- readPDF(control = list(info="-f",text = "-layout"))(elem = list(uri = uris.name),
                                                           language="en",id="id1")
      read.file <- tempPDF$content
      texts <- enc2utf8(read.file)
      text.collapse <- paste(texts,collapse=" ")
      text.hyphen <- gsub("-\\s+","",text.collapse)
      text.space <- gsub("\\s\\s+"," ",text.hyphen)
   if (!is.null(tempPDF$meta$heading)) {
     title <- tempPDF$meta$heading
     author <- tempPDF$meta$author # Alan Ritter ; Colin Cherry ; Bill Dolan
     #id <-tempPDF$meta$id
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
     datetime <- des#y[[1]][1]
   }
   # if (length(title)<1) {
   #   title <- "NA"
   # }
   # if (length(author)<1) {
   #   author <- "NA"
   # }
   # if (length(datetime)<1) {
   #   datetime <- "NA"
   # }
   # if (length(name)<1) {
   #   name <-x$name[i]
   # }
   titles[i] <- title
   authors[i] <- author
   datetimes[i] <- datetime
   names[i] <- name
    text.extract[[i]] <- text.space
    text.extract <- unlist(text.extract)
   
    
   #  }
  }
  info <- list(titles=titles, authors=authors,datetimes=datetimes, names=names,text.extract=text.extract) 
  return(info)
}
  
  
extractZotero <- function(extracts,query,query2,len,words,author,title,date){#p1,p2) {
  extract = extracts
 # wordnum=as.numeric(words)  
  author.extract <- author
    title.extract <-title
    date.extract <- date
  query=query
 query2=query2
  # query<- unlist(strsplit(query1," "))[1]
  len <-len
   num <- length(extract)
    text.extract <- list()
   # text.extract[[1]] <-text.space
  #  text.full <- vector()
    titles <- vector()
    authors <- vector()
    datetimes <- vector()
   # abstracts <- vector()
    n <- 1
    for (i in 1:num) {
      data <- extract[i]
      # data<- text.extract[1]
      text <- paste(data, collapse = " ")
      text.hyphen <- gsub("-\\s+","",text)
      text.space<- gsub("\\s\\s+"," ",text.hyphen)
     
       k <- kwic(text.space, query,len,valuetype = "regex")

       if (length(k)==5) {
         kpaste <- paste(k[,3],k[,4],k[,5])
         kpaste <- gsub("\\s+"," ",kpaste)
      #for (n in 1:length(kpaste)) {
       # grepl(query2,kpaste[n]) 
        pat <- grep(query2,kpaste)
        if (length(pat>0)) {
        zots <- kpaste[pat]
        zot <-paste(zots, collapse=" ")

        author <- author.extract[i]
        title <- title.extract[i]
        datetime <- date.extract[i]
     #   datetime <- data.lines[i]
      #  abstract <- abstract.lines[i]
      #  author <- author.lines[i]
      #  title <- title.lines[i]#tempPDF$meta$id
         text.extract[[n]] <- zot
      #   text.full[i] <-text.space
         titles[n] <- title
         authors[n] <-author
         datetimes[n] <- datetime
       #  abstracts[i] <- abstract
         text.extract <- unlist(text.extract)
         n=n+1
        }
       }
      }
    info <- list(text.extract=text.space,titles=titles,datetimes=datetimes, authors=authors)#, text.full=text.full,abstracts=abstracts)
    return(info)
}

