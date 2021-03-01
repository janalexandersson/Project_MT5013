
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
library(purrr)


# webpage <- read_html("http://www.imdb.com/search/title?year=2017&title_type=feature&page=3&ref_=adv_nxt")
# 
# #popularity_rank
# test <- html_nodes(webpage,'.ratings-imdb-rating strong , .favorable , .lister-item-header a , .text-primary')
# test <- html_text(test)
# test
# 
# 
# 
# test2 <- html_nodes(webpage,'.ghost~ .text-muted+ span , .text-primary')
# test2 <- html_text(test2)
# test2

url <- "http://www.imdb.com/search/title?year=2017&title_type=feature&"


################################################################################
# webpage = read_html("http://www.imdb.com/search/title?year=2017&title_type=feature&page=3&ref_=adv_nxt")
# test3 <- html_nodes(webpage,".ghost~ .text-muted+ span , .text-primary")
# test3 <- html_text(test3)
# test3
# test3 <- gsub(".*\\.$","x", test3)
# test3
# 
# metascore <- html_nodes(webpage,'.unfavorable , .mixed , .favorable , .text-primary')
# metascore <- html_text(metascore)
# metascore <- gsub(".*\\.","x", metascore)
# 
# 
# 
# 
# 
# attribute <- test3
# 
# ######################################################
# missingValues <- function(attribute){
#   missingValuesInAttribute <- c()
#   for(i in 1:(length(attribute)-1)){
#     if( (attribute[i] == "x") & (attribute[i+1] == "x") ){
#       missingValuesInAttribute <- append(i+1, missingValuesInAttribute)
#     }
#   }
# 
#   for (i in missingValuesInAttribute){
#     
#     a<-attribute[1:(i-1)]
#   
#     b<-attribute[i:length(attribute)]
#     
#     attribute <-append(a,list("NA"))
#   
#     attribute<-append(attribute,b)
#     
#   }
# 
#   if(attribute[length(attribute)] == "x"){
#     attribute <-append(attribute, "NA")
#   }
# 
#   attribute <- attribute[! (attribute %in% "x") ]
#   return(attribute)
# }
# 
# missingValues(test3)
  ###################################################################################































imdb_func <- function(url){
  
  webpage <- read_html(url)
  
  #popularity_rank
  popularity_rank <- html_nodes(webpage,'.text-primary')
  popularity_rank <- html_text(popularity_rank)
  
  #title
  title <- html_nodes(webpage,'.lister-item-header a')
  title <- html_text(title)
  
  #genre         
  genre <-  html_nodes(webpage,'.genre')
  genre <- html_text(genre)
  
  #imdb_score        
  imdb_score <-  html_nodes(webpage,'.ratings-imdb-rating strong')
  imdb_score<- html_text(imdb_score)
  
  #gross       
  gross <-  html_nodes(webpage,".ghost~ .text-muted+ span , .text-primary")
  gross<- html_text(gross)
  gross <- gsub(".*\\.$","x", gross)
  
  #runtime
  runtime <- html_nodes(webpage,'.text-muted .runtime')
  runtime <- html_text(runtime)
  
  #director
  director <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
  director <- html_text(director)
  
  #actors
  actor1 <- html_nodes(webpage,'.lister-item-content .ghost+ a')
  actor1 <- html_text(actor1)
  
  actor2 <- html_nodes(webpage,'.text-muted+ p a:nth-child(4)')
  actor2 <- html_text(actor2)
  
  #metascore
  metascore <- html_nodes(webpage,'.unfavorable , .mixed , .favorable , .text-primary')
  metascore <- html_text(metascore)
  metascore <- gsub(".*\\.","x", metascore)
  
  v <- list(title, popularity_rank, genre, imdb_score, gross, runtime, director,
            actor1,metascore, actor2)
  
  return(v)
}

imdb_func(url)




##########################################
temp = imdb_func("http://www.imdb.com/search/title?year=2017&title_type=feature&")
title <- temp[[1]]
popularity_rank <- temp[[2]]
genre <- temp[[3]]
imdb_score <- temp[[4]]
gross <- temp[[5]]
runtime <- temp[[6]]
director <- temp[[7]]
actor1 <- temp[[8]]
metascore <- temp[[9]]
actor2 <- temp[[10]]
###################################
for(i in 2:4){
  
  url2 <- paste0("http://www.imdb.com/search/title?year=2017&title_type=feature&page=" ,
                 i,"&view=advanced&ref_=adv_nxt")
  
  temp2 = imdb_func(url2)
  
  title <- c(title,temp2[[1]])
  popularity_rank <- c(popularity_rank, temp2[[2]])
  genre <- c(genre, temp2[[3]])
  imdb_score <- c(imdb_score, temp2[[4]])
  gross <- c(gross,temp2[[5]])
  runtime <- c(runtime, temp2[[6]] )
  director <- c(director, temp2[[7]] )
  actor1 <- c(actor1, temp2[[8]])
  metascore <- c(metascore, temp2[[9]] )
  actor2 <- c(actor2, temp2[[10]])
}



##################################
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
gross2 <- c()
for(i in 1:length(gross)){
  gross2 <- c(gross2, gross[[i]])
}
gross <- gross2

metascore2 <- c()
metascore <- missingValues(metascore)
for(i in 1:length(metascore)){
  metascore2 <- c(metascore2, metascore[[i]])
}
metascore <- metascore2
######################################################

# 
# for (i in c(25,29, 55,66,71,75,77,81,85,86,101,105,107,109:114,
#             116,119,123,128,131,134,138:141,143,145,148:150,154,155,
#             158,159,162:164,169,172,175,177:182,186,187,191,193,194,197:199)){
#   
#   a<-gross[1:(i-1)]
#   
#   b<-gross[i:length(gross)]
#   
#   gross<-append(a,list("NA"))
#   
#   gross<-append(gross,b)
#   
# }
# 
# gross2 <- c()
# for(i in 1:length(gross)){
#   gross2 <- c(gross2, gross[[i]])
# }
# gross <- gross2
# ############################
# 
# missingValuesMetascore <- c()
# for(i in 1:length(metascore)){
#   if( (metascore[i] == "x") & (metascore[i+1] == "x") ){
#     missingValuesMetascore <- append(i+1, missingValuesMetascore)
#   }
# }
# 
# for (i in missingValuesMetascore){
#   
#   a<-metascore[1:(i-1)]
#   
#   b<-metascore[i:length(metascore)]
#   
#   metascore <-append(a,list("NA"))
#   
#   metascore<-append(metascore,b)
#   
# }
# metascore <- metascore[! (metascore %in% "x") ]
# metascore
# metascore2 <- c()
# 
# for(i in 1:length(metascore)){
#   metascore2 <- c(metascore2, metascore[[i]])
# }
# metascore <- metascore2
# gross
##########################################################


for (i in c(137,151)){
  
  a<-actor2[1:(i-1)]
  
  b<-actor2[i:length(actor2)]
  
  actor2<-append(a,list("NA"))
  
  actor2<-append(actor2,b)
  
}

actor2.2 <- c()
for(i in 1:length(actor2)){
  actor2.2 <- c(actor2.2, actor2[[i]])
}
actor2 <- actor2.2
##############################################

length(metascore)

#create the data frame
imdb_df <- data.frame(popularity_rank, genre, imdb_score, gross, runtime, director,
                      actor1,actor2,metascore) 




#everything was of the type factor before
#mutating and cleaning the data frame
imdb_df <- imdb_df %>% 
  mutate(popularity_rank = gsub("\\." , "" , as.character(popularity_rank))) %>%
  mutate(popularity_rank = as.numeric(popularity_rank)) %>% 
  mutate(genre = gsub("\n" , "", genre)) %>% 
  mutate(imdb_score = as.numeric(as.character(imdb_score))) %>%
  mutate(gross = gsub("\\$|M", "", as.character(gross))) %>% 
  mutate(gross = as.numeric(gross)) %>% 
  mutate(runtime = gsub(" min", "" , as.character(runtime))) %>% 
  mutate(runtime = as.numeric(runtime)) %>% 
  mutate(director = as.character(director)) %>% 
  mutate(actor1 = as.character(actor1)) %>% 
  mutate(actor2 = as.character(actor2))  %>% 
  mutate(metascore = as.numeric(metascore))
rownames(imdb_df) <- title



imdb_df


