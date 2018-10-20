#Katz-Backoff algorithm - Prediction Model
#Coursera Data Science Specialization - Capstone by Chris Blycha
#Loading the required packages.
library(shiny)
library(tm)
library(stringr)
library(dplyr)
library(reshape2)
library(shinydashboard)
# Load the n-gram data
setwd("~/Documents/DS capstone/data")
load(file = "ngram_2.Rda",envir=.GlobalEnv)
load(file = "ngram_3.Rda",envir=.GlobalEnv)
load(file = "ngram_4.Rda",envir=.GlobalEnv)
#Adding probabilities Column
data_frame2$probabilities = rep(1, nrow(data_frame2))
data_frame3$probabilities = rep(1, nrow(data_frame3))
data_frame4$probabilities = rep(1, nrow(data_frame4))
#Adding Word Length
data_frame2$word_length = rep(1, nrow(data_frame2))
data_frame3$word_length = rep(1, nrow(data_frame3))
data_frame4$word_length = rep(1, nrow(data_frame4))

#Cleaning the text entered,
CleanInputString <- function(input)
{
  input_corpus<- VCorpus(VectorSource(input))
  input_corpus<- tm_map(input_corpus, content_transformer(tolower))
  input_corpus<- tm_map(input_corpus, removePunctuation)
  input_corpus<- tm_map(input_corpus, removeNumbers)
  input_corpus<- tm_map(input_corpus, stripWhitespace)
  input_string<- as.character(input_corpus[[1]])
  input_string<- gsub("(^[[:space:]]+|[[:space:]]+$)", "", input_string)
  text <- unlist(strsplit(input_string, split=" ")); #Separates the words.
  return(text)
}



Word_4 <- function(input,Next_word)
{
  input_corpus<- VCorpus(VectorSource(input))
  input_corpus<- tm_map(input_corpus, content_transformer(tolower))
  input_corpus<- tm_map(input_corpus, removePunctuation)
  input_corpus<- tm_map(input_corpus, removeNumbers)
  input_corpus<- tm_map(input_corpus, stripWhitespace)
  input_string<- as.character(input_corpus[[1]])
  input_string<- gsub("(^[[:space:]]+|[[:space:]]+$)", "", input_string)
  input_string_check <- unlist(strsplit(input_string, split=" ")); #Separates the words.
  input_stringLen <- length(input_string_check); #Determines the word count
  Target_word_3 <- str_split(input_string_check,"_")[[3]][1]
  Target_word_2 <- str_split(input_string_check,"_")[[2]][1]
  Target_word_1 <- str_split(input_string_check,"_")[[1]][1] 
  Target_word <- paste(Target_word_1,"_",Target_word_2,"_",Target_word_3,sep="")
  word_length_1 <-nchar(Target_word_1) 
  word_length_2 <-nchar(Target_word)
  data_frame4$word_length_3<- word_length_2
  filtered_df_4 <- data_frame4 %>% dplyr::filter(substr(Content,1,data_frame4$word_length_3) == Target_word);# collate the terms
  filtered_df_4 <- filtered_df_4 %>%  mutate(probabilities = (Frequency-0.2) / sum(Frequency)* 100); #Discounting
  term_next <- head(filtered_df_4[order(filtered_df_4$probabilities, decreasing= T),], n = 10)
  term_next <- term_next[ , c("Content","probabilities")]
  newColNames <- c("Input_word_1", "Input_word_2","Input_word_3","Next_word")
  newCols <- colsplit(term_next$Content, "_", newColNames)
  after <- cbind(term_next, newCols$Input_word_1,newCols$Input_word_2,newCols$Input_word_3,newCols$Next_word)
  term_next <- after[ , c("newCols$Input_word_1","newCols$Input_word_2","newCols$Input_word_3","newCols$Next_word","probabilities")]
  colnames(term_next )[colnames(term_next )=="newCols$Input_word_1"] <- "Input_word_1" # Rename a column in R
  colnames(term_next )[colnames(term_next )=="newCols$Input_word_2"] <- "Input_word_2" # Rename a column in R
  colnames(term_next )[colnames(term_next )=="newCols$Input_word_3"] <- "Last_Input_word" # Rename a column in R
  colnames(term_next )[colnames(term_next )=="newCols$Next_word"] <- "Next_word"
  newdata <- term_next[c(4:5)]
  term_next <-newdata 
  term_next
  empty <- (is.data.frame(term_next) && nrow(term_next)==0)
  if(empty==TRUE) {
    w2 <- "Null"
    return(w2)
  }else{
    return(term_next)
  }  
}


Word_3 <- function(input,Next_word)
{
  input_corpus<- VCorpus(VectorSource(input))
  input_corpus<- tm_map(input_corpus, content_transformer(tolower))
  input_corpus<- tm_map(input_corpus, removePunctuation)
  input_corpus<- tm_map(input_corpus, removeNumbers)
  input_corpus<- tm_map(input_corpus, stripWhitespace)
  input_string<- as.character(input_corpus[[1]])
  input_string<- gsub("(^[[:space:]]+|[[:space:]]+$)", "", input_string)
  input_string_check <- unlist(strsplit(input_string, split=" ")); #Separates the words.
  Target_word_2 <- str_split(input_string_check,"_")[[2]][1]
  Target_word_1 <- str_split(input_string_check,"_")[[1]][1] 
  Target_word <- paste(Target_word_1,"_",Target_word_2,"",sep="")
  word_length_1 <-nchar(Target_word_1) 
  word_length_2 <-nchar(Target_word)
  data_frame3$word_length_2 <- word_length_2
  filtered_df_3 <- data_frame3 %>% dplyr::filter(substr(Content,1,data_frame3$word_length_2) == Target_word);# collate the terms
  filtered_df_3 <- filtered_df_3 %>%  mutate(probabilities = (Frequency-0.2) / sum(Frequency)* 100); #Discounting
  term_next <- head(filtered_df_3[order(filtered_df_3$probabilities, decreasing= T),], n = 10)
  term_next <- term_next[ , c("Content","probabilities")]
  newColNames <- c("Input_word_1", "Input_word_2","Next_word")
  newCols <- colsplit(term_next$Content, "_", newColNames)
  after <- cbind(term_next, newCols$Input_word_1,newCols$Input_word_2,newCols$Next_word)
  term_next <- after[ , c("newCols$Input_word_1","newCols$Input_word_2","newCols$Next_word","probabilities")]
  colnames(term_next )[colnames(term_next )=="newCols$Input_word_1"] <- "Input_word_1" # Rename a column in R
  colnames(term_next )[colnames(term_next )=="newCols$Input_word_2"] <- "Last_Input_word" # Rename a column in R
  colnames(term_next )[colnames(term_next )=="newCols$Next_word"] <- "Next_word"
  newdata <- term_next[c(3:4)]
  term_next <-newdata 
  term_next
  empty <- (is.data.frame(term_next) && nrow(term_next)==0)
  if(empty==TRUE) {
    w2 <- "Null"
    return(w2)
  }else{
    return(term_next);
  }  
}

Word_2 <- function(input,Next_word)
{
  input_corpus<- VCorpus(VectorSource(input))
  input_corpus<- tm_map(input_corpus, content_transformer(tolower))
  input_corpus<- tm_map(input_corpus, removePunctuation)
  input_corpus<- tm_map(input_corpus, removeNumbers)
  input_corpus<- tm_map(input_corpus, stripWhitespace)
  input_string<- as.character(input_corpus[[1]])
  input_string<- gsub("(^[[:space:]]+|[[:space:]]+$)", "", input_string)
  input_string_check <- unlist(strsplit(input_string, split=" ")); #Separates the words.
  input_stringLen <- length(input_string_check); #Determines the word count
  Target_word_1 <- str_split(input_string_check,"_")[[1]][1] 
  Target_word <- paste(Target_word_1,"_",sep="")
  word_length_1 <-nchar(Target_word) 
  data_frame2$word_length_1<- word_length_1
  filtered_df_2 <- data_frame2 %>% dplyr::filter(substr(Content,1,data_frame2$word_length_1) == Target_word);# collate the terms
  filtered_df_2 <- filtered_df_2 %>%  mutate(probabilities = (Frequency-0.2) / sum(Frequency)* 100); #Discounting
  term_next <- head(filtered_df_2[order(filtered_df_2$probabilities, decreasing= T),], n = 10)
  term_next <- term_next[ , c("Content","probabilities")]
  newColNames <- c("Input_word_1","Next_word")
  newCols <- colsplit(term_next$Content, "_", newColNames)
  after <- cbind(term_next, newCols$Input_word_1,newCols$Next_word)
  term_next <- after[ , c("newCols$Input_word_1","newCols$Next_word","probabilities")]
  colnames(term_next )[colnames(term_next )=="newCols$Input_word_1"] <- "Last_Input_word" # Rename a column in R
  colnames(term_next )[colnames(term_next )=="newCols$Next_word"] <- "Next_word"
  newdata <- term_next[c(2:3)]
  term_next <-newdata 
  term_next
  empty <- (is.data.frame(term_next) && nrow(term_next)==0)
  if(empty==TRUE) {
    w2 <- "Null"
    return(w2)
  }else{
    return(term_next);
  }  
}

Word_Feeder <- function(input) {
  input_stringLen <- length(input); 
  {
    if(input_stringLen==10) {
      Target_word_8 <- str_split(input,"_")[[8]][1] 
      Target_word_9 <- str_split(input,"_")[[9]][1]
      Target_word_10 <- str_split(input,"_")[[10]][1] 
      Target_word <- paste(Target_word_8," ",Target_word_9," ",Target_word_10,sep=" ")
      return(Target_word);
    }
    if(input_stringLen==9) {
      Target_word_7 <- str_split(input,"_")[[7]][1] 
      Target_word_8 <- str_split(input,"_")[[8]][1]
      Target_word_9 <- str_split(input,"_")[[9]][1] 
      Target_word <- paste(Target_word_7," ",Target_word_8," ",Target_word_9,sep=" ")
      return(Target_word);
    }
    if(input_stringLen==8) {
      Target_word_6 <- str_split(input,"_")[[6]][1] 
      Target_word_7 <- str_split(input,"_")[[7]][1]
      Target_word_8 <- str_split(input,"_")[[8]][1] 
      Target_word <- paste(Target_word_6," ",Target_word_7," ",Target_word_8,sep=" ")
      return(Target_word);
    }
    if(input_stringLen==7) {
      Target_word_5 <- str_split(input,"_")[[5]][1] 
      Target_word_6 <- str_split(input,"_")[[6]][1]
      Target_word_7 <- str_split(input,"_")[[7]][1] 
      Target_word <- paste(Target_word_5," ",Target_word_6," ",Target_word_7,sep=" ")
      return(Target_word);
    }
    if(input_stringLen==6) {
      Target_word_4 <- str_split(input,"_")[[4]][1] 
      Target_word_5 <- str_split(input,"_")[[5]][1]
      Target_word_6 <- str_split(input,"_")[[6]][1] 
      Target_word <- paste(Target_word_4," ",Target_word_5," ",Target_word_6,sep=" ")
      return(Target_word);
    }
    if(input_stringLen==5) {
      Target_word_3 <- str_split(input,"_")[[3]][1] 
      Target_word_4 <- str_split(input,"_")[[4]][1]
      Target_word_5 <- str_split(input,"_")[[5]][1] 
      Target_word <- paste(Target_word_3," ",Target_word_4," ",Target_word_5,sep="")
      return(Target_word);
    }
    if(input_stringLen==4) {
      Target_word_2 <- str_split(input,"_")[[2]][1] 
      Target_word_3 <- str_split(input,"_")[[3]][1]
      Target_word_4 <- str_split(input,"_")[[4]][1] 
      Target_word <- paste(Target_word_2," ",Target_word_3," ",Target_word_4,sep="")
      return(Target_word);
    }
    if(input_stringLen==3) {
      Target_word_1 <- str_split(input,"_")[[1]][1] 
      Target_word_2 <- str_split(input,"_")[[2]][1]
      Target_word_3 <- str_split(input,"_")[[3]][1] 
      Target_word <- paste(Target_word_1," ",Target_word_2," ",Target_word_3,sep="")
      return(Target_word);
    }
    if(input_stringLen==2) {
      Target_word_1 <- str_split(input,"_")[[1]][1] 
      Target_word_2 <- str_split(input,"_")[[2]][1]
      Target_word <- paste(Target_word_1," ",Target_word_2,sep="")
      return(Target_word);
    }
    if(input_stringLen==1) {
      Target_word_1 <- str_split(input,"_")[[1]][1] 
      Target_word <- paste(Target_word_1,sep="")
      return(Target_word);
    }  
  }
}


Word_4_predictor <- function(input,Prediction){
  input_string <- Word_Feeder(CleanInputString(input))
  text <- unlist(strsplit(input_string, split=" ")); #Separates the words.
  input_string_check <- text 
  Target_word_1 <- str_split(input_string_check,"_")[[1]][1] 
  Target_word_2 <- str_split(input_string_check,"_")[[2]][1]
  Target_word_3 <- str_split(input_string_check,"_")[[3]][1]
  Word_4_target <- paste(Target_word_1," ",Target_word_2," ",Target_word_3,sep="")
  Word_3_target <- paste(Target_word_2," ",Target_word_3,sep="")
  Word_2_target <- paste(Target_word_3,sep="") 
  Prediction_1 <- Word_4(Word_4_target)
  Prediction_2 <- Word_3(Word_3_target)
  Prediction_3 <- Word_2(Word_2_target)
  Check_1 <- class(Prediction_1)
  Check_2 <- class(Prediction_2)
  Check_3 <- class(Prediction_3)
  Prediction <- "Null"
  if(Check_1=="data.frame") {
    Prediction <- Prediction_1
    return(Prediction);
  }
  if(Check_2=="data.frame") {
    Prediction <- Prediction_2
    return(Prediction);
  }
  if(Check_3=="data.frame") {
    Prediction <- Prediction_3
    return(Prediction);
  }
  if(Check_1!="data.frame" | Check_2!="data.frame" | Check_3!="data.frame") {
    w2 <- "We dont have that word, please try again"
    as.data.frame(w2)
    return(w2);
  }
}

Word_3_predictor <- function(input,Prediction){
  input_string <- Word_Feeder(CleanInputString(input))
  text <- unlist(strsplit(input_string, split=" ")); #Separates the words.
  input_string_check <- text 
  Target_word_1 <- str_split(input_string_check,"_")[[1]][1] 
  Target_word_2 <- str_split(input_string_check,"_")[[2]][1]
  Word_3_target <- paste(Target_word_1," ",Target_word_2,sep="")
  Word_2_target <- paste(Target_word_2,sep="") 
  Prediction_2 <- Word_3(Word_3_target)
  Prediction_3 <- Word_2(Word_2_target)
  Check_2 <- class(Prediction_2)
  Check_3 <- class(Prediction_3)
  Prediction <- "Null"
  if(Check_2=="data.frame") {
    Prediction <- Prediction_2
    return(Prediction);
  }
  if(Check_3=="data.frame") {
    Prediction <- Prediction_3
    return(Prediction);
  }
  if(Check_2!="data.frame" | Check_3!="data.frame") {
    w2 <- "We dont have that word, please try again"
    as.data.frame(w2)
    return(w2);
  }
}

Word_2_predictor <- function(input,Prediction){
  input_string <- Word_Feeder(CleanInputString(input))
  text <- unlist(strsplit(input_string, split=" ")); #Separates the words.
  input_string_check <- text 
  Target_word_1 <- str_split(input_string_check,"_")[[1]][1] 
  Word_2_target <- paste(Target_word_1,sep="") 
  Prediction_3 <- Word_2(Word_2_target)
  Check_3 <- class(Prediction_3)
  Prediction <- "Null"
  if(Check_3=="data.frame") {
    Prediction <- Prediction_3
    return(Prediction);
  }
  if(Check_3!="data.frame") {
    w2 <- "We dont have that word, please try again"
    as.data.frame(w2)
    return(w2);
  }
}


fib <- function(input,n) {
  input_string <-Word_Feeder(CleanInputString(input))
  text <- unlist(strsplit(input_string, split=" "))
  n <- length(text)
  return(n)
}



predicter <- function(input,word)
{ 
  input_stringLen <- fib(Word_Feeder(CleanInputString(input)))
  if(input_stringLen==3) {word <- Word_4_predictor(Word_Feeder(CleanInputString(input)))
  return(word);
  }
  if(input_stringLen==2) {word <- Word_3_predictor(Word_Feeder(CleanInputString(input)))
  return(word);
  }
  if(input_stringLen==1) {word <- Word_3_predictor(Word_Feeder(CleanInputString(input)))
  return(word);
  }
}




shinyServer(
  function(input,output) {
    output$caption <- renderText({
      paste(CleanInputString(input$inText))
    })
    output$mytable1 <- renderTable(Word_Input <- predicter(input$inText))  
})




    