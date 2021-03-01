
library(tidyverse)
library(purrr)
library(stringr)
library(knitr)
library(rebus)
library(lubridate)
library(rvest)
library(jsonlite)
library(httr)
library(listviewer)
library(readxl)
library(RCurl)
library(rvest)

url <- "http://www.imdb.com/search/title?year=2017&title_type=feature&"

webpage <- read_html(url)


gross <-  html_nodes(webpage,".ghost~ .text-muted+ span , .text-primary")
gross<- html_text(gross)
gross <- gsub(".*\\.$","x", gross)



missingValues <- function(attribute){
  missingValuesInAttribute <- c()
  for(i in 1:(length(attribute)-1)){
    if( (attribute[i] == "x") & (attribute[i+1] == "x") ){
      missingValuesInAttribute <- append(i+1, missingValuesInAttribute)
    }
  }
  
  for (i in missingValuesInAttribute){
    
    a<-attribute[1:(i-1)]
    
    b<-attribute[i:length(attribute)]
    
    attribute <-append(a,list("NA"))
    
    attribute<-append(attribute,b)
    
  }
  
  if(attribute[length(attribute)] == "x"){
    attribute <-append(attribute, "NA")
  }
  
  attribute <- attribute[! (attribute %in% "x") ]
  return(attribute)
}


gross <- missingValues(gross)
gross
gross2 <- c()
for(i in 1:length(gross)){
  gross2 <- c(gross2, gross[[i]])
}
gross <- gross2