#Capstone Sampliing data:

#Set Options
options(stringsAsFactors=F)
Sys.setlocale('LC_ALL','C')
#cleaning menmory
rm(list = ls(all.names = TRUE))

setwd("~/Documents/DS capstone/data")
suppressMessages(library(stringr))
suppressMessages(library(quanteda))
suppressMessages(library(dplyr))
suppressMessages(library(stringi))
suppressMessages(library(tm))

#Data Download and Pre processing
blogs_lines <- readLines(con = "~/Documents/DS capstone/data/en_US.blogs.txt", encoding= "UTF-8", skipNul = T)
twitter_lines <- readLines(con = "~/Documents/DS capstone/data/en_US.twitter.txt", encoding= "UTF-8", skipNul = T)
news_lines <- readLines(con = "~/Documents/DS capstone/data/en_US.news.txt", encoding= "UTF-8", skipNul = T)

#Ramdomly sampling data 0.15% of the data files using the LaF R Paackage.

set.seed(250)
blogs_lines <- blogs_lines[sample(seq(length(blogs_lines)))]
twitter_lines <- twitter_lines[sample(seq(length(twitter_lines)))]
news_lines <- news_lines[sample(seq(length(news_lines)))]

b <- length(blogs_lines)
t <- length(twitter_lines)
n <- length(news_lines)
blog_sample <- blogs_lines[1:floor(b*0.20)]
twitter_sample <- twitter_lines[1:floor(t*0.20)]
news_sample <- news_lines[1:floor(n*0.20)]

sample_data <- rbind(blog_sample,twitter_sample,news_sample) #Combine the samples together

#Processing sample:
# Reading Symbols

sample_data = sample_data[setdiff(seq(1,length(sample_data)),grep("<",sample_data))]
sample_data = sample_data[setdiff(seq(1,length(sample_data)),grep(">",sample_data))]
sample_data = sample_data[setdiff(seq(1,length(sample_data)),grep("]",sample_data))]
sample_data = sample_data[setdiff(seq(1,length(sample_data)),grep("}",sample_data))]
sample_data = sample_data[setdiff(seq(1,length(sample_data)),grep("_",sample_data))]
sample_data = sample_data[setdiff(seq(1,length(sample_data)),grep("\\/",sample_data))]
sample_data = str_replace_all(sample_data,"[<>{}()&;,.\n]"," ")

length(sample_data) #Checking the length.

text_sample_df <- data_frame(line = 1:1360202, text = sample_data)
text_sample_df$ID <- seq.int(nrow(text_sample_df)) #Add an ID field

#Most common America swear used on facebook in 2013.
my_custom_stopwords <- c("shit","fuck","damn","bitch","crap","dick","piss","darn","pussy","fag","cock","asshole","bastard","fag","douche","slut","boobs")


#Cleaning
text_sample_df$text<- tolower(text_sample_df$text)
text_sample_df$text<- removeWords(text_sample_df$text,c(stopwords('en')))
text_sample_df$text<- removePunctuation(text_sample_df$text)
text_sample_df$text<- stripWhitespace(text_sample_df$text)
text_sample_df$text<- removeNumbers(text_sample_df$text)
text_sample_df$text<- removeWords(text_sample_df$text,c(stopwords('en')))
text_sample_df$text<- removeWords(text_sample_df$text,my_custom_stopwords)

quanteda_options("threads" = 8) #Multi threading


corp_qu <- quanteda::corpus(text_sample_df$text)
toks <- quanteda::tokens(corp_qu, remove_numbers = TRUE, remove_punct = TRUE,remove_separators = TRUE,remove_symbols = TRUE,remove_twitter = TRUE,remove_hyphens = TRUE,remove_url = TRUE,remove = stopwords("english"))

#ngram_2
ngram_2 <- quanteda::tokens_ngrams(toks, n = 2:2)
ngram_2_dfm <- quanteda::dfm(ngram_2)
gram_2_df <- data.frame(Content = featnames(ngram_2_dfm), Frequency = colSums(ngram_2_dfm),row.names = NULL, stringsAsFactors = FALSE)
gram_2_df <- gram_2_df [order(gram_2_df$Frequency,decreasing = TRUE),]
save(gram_2_df, file = "ngram_2.Rda")


#ngram_3
ngram_3 <- quanteda::tokens_ngrams(toks, n = 3:3)
ngram_3_dfm <- quanteda::dfm(ngram_3)
gram_3_df <- data.frame(Content = featnames(ngram_3_dfm), Frequency = colSums(ngram_3_dfm),row.names = NULL, stringsAsFactors = FALSE)
gram_3_df <- gram_3_df [order(gram_3_df$Frequency,decreasing = TRUE),]
save(gram_3_df, file = "ngram_3.Rda")


#ngram_4
ngram_4 <- quanteda::tokens_ngrams(toks, n = 4:4)
ngram_4_dfm <- quanteda::dfm(ngram_4)
gram_4_df <- data.frame(Content = featnames(ngram_4_dfm), Frequency = colSums(ngram_4_dfm),row.names = NULL, stringsAsFactors = FALSE)
gram_4_df <- gram_4_df [order(gram_4_df$Frequency,decreasing = TRUE),]
save(gram_4_df, file = "ngram_4.Rda")


  