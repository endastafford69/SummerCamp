#these are the packages we need for this example - executing this line will install them
install.packages(c("devtools", "rjson", "bit64", "httr", "plyr", "ggplot2", "doBy", "XML"))
install.packages("rvest")
install.packages("rtweet")

#devtools allows us to install from github
library(devtools)
library("rvest")
library(tm)
## load rtweet package
library(rtweet)


###############################
###############################





#if (!requireNamespace("remotes", quietly = TRUE)) {
#  install.packages("remotes")
#}

## install dev version of rtweet from github
#remotes::install_github("mkearney/rtweet")

## load rtweet package
#library(rtweet)



###########################
#set the working directory for file outputs
setwd("C:/Users/User/Documents/NCI_Sem3/Summer Camp")

getwd() # check we have moved

#these are various libraries that we use throughout this example
library(plyr)

library(twitteR)


########################################################################################
############################## SET UP ACCESS TO TWITTER ################################
########################################################################################
#here my api keys would go, to run this example you need to input your keys, and secrets
########################################################################################

app= "esd1"
api_key <- "Zmw0r2fA6sNBHoo4a3Ue1A0oo"
api_secret <- "mEMKTGD84oWRQktMrxXUWWu0uUEojRpVIEOkx7LSLTCSbGxqDn"
access_token <- "215278925-8ADgRa98JCqdvXFp2B0VYchKzVuNWAPCBbQGVmVX"
access_token_secret <- "ST2RzqOQFSjhJ4OJOP0Ki9xEWoV0oBrSCUaCVCC3jR1Fu"



token = create_token(app, api_key,api_secret,access_token,access_token_secret)
#Use a local file to cache OAuth access credentials between R sessions?
#1: Yes
#2: No
#Select 1 i.e Yes use local file to cache credentials


########################################################################################
######################## SCRAPE TWITTER FOR SPECIFIC HANDLE ############################
########################################################################################


#@LFC
lfc <- search_tweets(
  "@LFC", n = 1500, include_rts = FALSE
)


##############################################################################
######################## SENTIMENT ANALYSIS ##################################
##############################################################################

pos.words = scan('pos_words.txt', what = 'character', comment.char='')
neg.words = scan('neg_words.txt', what = 'character', comment.char='')


# We are only interested in the first column the tweet text, so isolate that (column 1)

lfc_df = lfc$text

lfc.score = score.sentiment(lfc_df, pos.words, neg.words)

#skim only the most positive or negative tweets to throw away noise near 0
lfc.score$very.pos = as.numeric( lfc.score$score >= 2)
lfc.score$very.neg = as.numeric( lfc.score$score <= -2)
#write.csv(lfc.score, file="lfc2.csv", row.names=F)

#now we construct the twitter data frame and simultaneously compute the pos/neg sentiment scores for each #TAG

lfc.pos = aggregate(lfc.score$score, by=list(Sent=lfc.score$very.pos), FUN=sum)
lfc.neg = aggregate(lfc.score$score, by=list(Sent=lfc.score$very.neg), FUN=sum)

#and here the general sentiment 
lfc.tot = lfc.pos[2,2] + lfc.neg[2,2]

lfc.tot

#our first function
score.sentence <- function(sentence, pos.words, neg.words) {
  #here some cleaning
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl::]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  sentence = tolower(sentence)
  
  #basic data structure construction
  word.list = str_split(sentence, '\\s+')
  words = unlist(word.list)
  
  #here we count the number of words that are positive and negative
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  
  #throw away those that didn't match
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  #compute the sentiment score for each sentence/tweet
  score = sum(pos.matches) - sum(neg.matches)
  
  return(score)
}

#our second function that takes an array of sentences and sentiment analyses them
score.sentiment <- function(sentences, pos.words, neg.words) {
  require(plyr)
  require(stringr)
  
  #here any sentence/tweet that causes an error is given a sentiment score of 0 (neutral)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    tryCatch(score.sentence(sentence, pos.words, neg.words ), error=function(e) 0)
  }, pos.words, neg.words)
  
  #now we construct a data frame
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
}





##############################################################################
############################# DEMO CODE ######################################
##############################################################################
########################### TWEET ANALYSIS ###################################
##############################################################################
####################  Look at a specific tag  ################################

#@LFC
lfc = searchTwitter("@LFC",n=1500)
lfc_df = twListToDF(lfc)
#write the output of the analysis to a csv file
write.csv(lfc_df, file="lfc1.csv", row.names=F)
names(lfc_df)

#Look at tweet 618
lfc_df[618, c("id", "created", "screenName", "replyToSN",
              "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
# print tweet #618 and make text fit for slide width
writeLines(strwrap(lfc_df$text[618], 60))



##############################################################################
#### From Web API Tut (3)
#### From the total tweets what words are most frequent
##############################################################################


# build a corpus, and specify the source to be character vectors
livCorpus <- Corpus(VectorSource(lfc_df$text))

# View the content of first entry 
livCorpus$content[1]

# convert to lower case
livCorpus <- tm_map(livCorpus, content_transformer(tolower))

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
livCorpus <- tm_map(livCorpus, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
livCorpus <- tm_map(livCorpus, content_transformer(removeNumPunct))

# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp","jürgen")
livCorpus <- tm_map(livCorpus, removeWords, myStopwords)

# remove extra whitespace
livCorpus <- tm_map(livCorpus, stripWhitespace)

# keep a copy for stem completion later
livCorpusCopy <- livCorpus

livCorpus <- tm_map(livCorpus, stemDocument) # stem words
writeLines(strwrap(livCorpus[[618]]$content, 60))

## r refer card data mine now provid link packag cran packag
## mapreduc hadoop ad
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

##### This takes some time to complete
livCorpus <- lapply(livCorpus, stemCompletion2, dictionary=livCorpusCopy)
livCorpus <- Corpus(VectorSource(livCorpus))

######## write the tweet 618, the cleaned up version #######
writeLines(strwrap(livCorpus[[618]]$content, 60))

######## Compare to the original ###########################
writeLines(strwrap(lfc_df$text[618], 60))


## ---- tidy=TRUE------Step 8----------------------------------------------------
# count word frequence
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}
n.klopp <- wordFreq(livCorpusCopy, "klopp")
n.champions <- wordFreq(livCorpusCopy, "champions")
cat(n.klopp, n.champions)
n.jürgen <- wordFreq(livCorpusCopy, "jürgen")


tdm_liv <- TermDocumentMatrix(livCorpus, control = list(wordLengths = c(1, Inf)))

library(tidyr)
idx <- which(dimnames(tdm_liv)$Terms %in% c("champions", "klopp", "anfield"))
as.matrix(tdm_liv[idx, 1:30])

idx

# inspect frequent words, must be contained in at least 'lowfreq' tweets
(freq.terms <- findFreqTerms(tdm_liv, lowfreq = 200))

# create a complete term/freq matrix for the tweet Corpus
term.freq <- rowSums(as.matrix(tdm_liv))

############### Print terms that occur at least 20 times ################
term.freq_20 <- subset(term.freq, term.freq >= 20)
df_liv_20 <- data.frame(term = names(term.freq_20), freq = term.freq_20)

library(ggplot2)
ggplot(df_liv_20, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Liverpool Terms >= 20") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

############### Print terms that occur at least 100 times ################
term.freq_100 <- subset(term.freq, term.freq >= 100)
df_liv_100 <- data.frame(term = names(term.freq_100), freq = term.freq_100)

ggplot(df_liv_100, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Liverpool Terms >= 100") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))


qplot(data=df_liv_100,freq, fill=term, bins =30)



################################################################################
# Calculate and display the most freq words and the tweet in which they occur
################################################################################


library(tidyverse)
library(stringr)
library(tidytext)

liv_tb <- tibble(tweeeet = seq_along(lfc),
                  text = lfc_df$text)

liv_tb %>%
  unnest_tokens(word, text)

lfc_series <- liv_tb %>%
  unnest_tokens(word, text)

lfc_series$tweeeet <- factor(lfc_series$tweeeet)

lfc_series %>%
  count(word, sort = TRUE)

lfc_series %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)


lfc_series %>% # %>% pipe the object forward into a function or call expression
  anti_join(stop_words) %>%
  group_by(tweeeet) %>%
  count(word, sort = TRUE) %>%
  top_n(10)





