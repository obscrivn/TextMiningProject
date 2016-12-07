#Parsing MODS (library-specific metadata XML schema)
parseMODS <-function(x) {
  #Extracting metadata
  titles_XML <- getNodeSet(x, "//mods:mods/mods:titleInfo[not (@type = 'alternative')]/mods:title", namespaces = c(mods="http://www.loc.gov/mods/v3"))
  titles <- sapply(titles_XML, xmlValue)
  authors_XML <- getNodeSet(x, "//mods:mods//mods:namePart", namespaces = c(mods="http://www.loc.gov/mods/v3"))
  authors <- sapply(authors_XML, xmlValue)
  dates_XML <- getNodeSet(x, "//mods:mods//mods:dateIssued", namespaces = c(mods="http://www.loc.gov/mods/v3"))
  dates <- sapply(dates_XML, xmlValue)
  corpus_XML <- getNodeSet(x, "//mods:mods//mods:subject", namespaces = c(mods="http://www.loc.gov/mods/v3"))
  corpus <- sapply(corpus_XML, xmlValue)
  corpus <- gsub("\\s--\\s", " ", corpus)
  corpus <- paste(unlist(corpus), sep = " ")
  return(list(titles = titles, authors = authors, dates = dates, corpus = corpus))  
}
  
