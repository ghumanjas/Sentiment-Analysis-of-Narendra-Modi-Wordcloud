rm(list=ls())
library(twitteR)
library(ROAuth)
library(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)
library(httr)

#Setting the working directory
getwd()
drive <- "D:/R/Project/Twitter - Sentiment analysis"
setwd(drive)

# Set API Keys
api_key <- "XXXXXXXXXXXXXXXXXXXXX"
api_secret <- "XXXXXXXXXXXXXXXXXXXXX"
access_token <- "XXXXXXXXXXXXXXXXXXXXX"
access_token_secret <- "XXXXXXXXXXXXXXXXXXXXX"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Getting latest tweets on Narendra Modi - Prime minister of india
modi_tweets <- searchTwitter('Narendra+Modi', n=3000)

# Loop over tweets and extract text
tweet_feed = laply(modi_tweets, function(t) t$getText())

# Dictionary to separate out positive and negative words trait
dir(drive)
good = scan('positive-words.txt',what='character', comment.char=';')
bad = scan('negative-words.txt',what='character', comment.char=';')

# Add a few twitter-specific negative phrases
bad_text = c(bad, 'wtf', 'epicfail', 'douchebag','idiot')
good_text = c(good, 'upgrade', ':)', '#iVoted', 'voted')

#scoring the tweet texts based on how many "good" and "bad" words show up
score.sentiment = function(sentences, good_text, bad_text, .progress='none')
{
  scores = laply(sentences, function(sentence, good_text, bad_text) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)        
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


# Call the function and return a data frame
df <- score.sentiment(tweet_feed, good_text, bad_text, .progress='text')
df$name <- "Narendra Modi"
write.csv(df,'Narendra Modi_tweets.csv',row.names = FALSE)

# Cut the text, just gets in the way
names(df)
plotdat <- df[c("name", "score")]
write.csv(plotdat,'Narendra Modi_tweets1.csv',row.names = FALSE)

# Remove neutral values of 0
str(plotdat)
plotdat1 <- plotdat[!plotdat$score == 0, ]

# Remove anything less than -3 or greater than 3
plotdat1 <- plotdat1[!plotdat1$score > 3, ]
plotdat1 <- plotdat1[!plotdat1$score < (-3), ]

# Nice little quick plot
library(ggplot2)
qplot(factor(score), data=plotdat1, geom="bar", 
      fill=factor(name),
      xlab = "Sentiment Score")

# frequency of negative + positive comments histgram


#Wordcloud
library(tm)

# Create corpus

df1 <- read.csv('Narendra Modi_tweets.csv')
feed1 = unlist(df1$text)
write.csv(feed1,'Narendra Modi_tweets2.csv',row.names = FALSE)

corpus=Corpus(VectorSource(feed1))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

set.seed(1234)
col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(2,1),rot.per = 0.25,
          random.color=T, max.word=200, random.order=F,colors=col)
  
