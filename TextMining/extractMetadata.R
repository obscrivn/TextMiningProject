
extractMetadata <-function(x) {
titles <-vector()
authors <-vector()
datetimes <- vector()
num <-length(x$name)
names <- vector()
cont <- rep("",num)
for (i in 1:num) {
  uris.name <- x$datapath[i]
  tempPDF <- readPDF(control = list(text = "-layout"))(elem = list(uri = uris.name),
                                                       language="en",id="id1")
  metapdf <- tempPDF$meta
  if (!is.null(tempPDF$meta$heading)) {
    title <- tempPDF$meta$heading
    author <- tempPDF$meta$author # Alan Ritter ; Colin Cherry ; Bill Dolan
    id <-tempPDF$meta$id
    
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
    name <-x$name[i]
  }
  titles[i] <- title
  authors[i] <-author
  datetimes[i] <- datetime
  names[i] <-name
}
list(titles=titles, authors=authors,datetimes=datetimes, names=names,metapdf=metapdf)
}