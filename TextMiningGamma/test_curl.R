library(curl)
library(jsonlite)
source("parseJSON.R")


getAllGoogleData <- function (query, numBooksToGet, startIndex = 0) {
  query = "mavo"
  con <- curl( URLencode( paste("https://www.googleapis.com/books/v1/volumes?q=", query, "&maxResults=40", "&startIndex=", startIndex, sep = "") ) )
  
  text <- readLines(con)
}

  


query = "mavo"
con <- curl( URLencode( paste("https://www.googleapis.com/books/v1/volumes?q=", query, "&maxResults=40", sep = "") ) )
text1 <- readLines(con)
close(con)

con <- curl( URLencode( paste("https://www.googleapis.com/books/v1/volumes?q=", query, "&maxResults=40", "&startIndex=41", sep = "") ) )
text2 <- readLines(con)
close(con)
my_data <- fromJSON(text)
parsed_json <- parseJSON(my_data)
print(parsed_json)