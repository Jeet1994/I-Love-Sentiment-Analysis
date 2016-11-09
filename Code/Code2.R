#Pulling only 15 tweets because I have a slow internet connection

library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

#connect to API
customer_key <- "udwLTM7gchrQYMQcW15wzj3wj"
customer_secret <- "cK5xlJhQ3rf2Vaix1uCOpg8LJcozmMIQM8g16JNvO2AtDqB0Ed"
access_token <- "466566632-1y63l7WVwsOTOcevkAFexWqqJ6lXHgpHz8GKbHTB"
access_token_secret <- "spMYZGwCk69iCEuO7LSLE5l4qSNFgFqThUuiU5thZScdk"
setup_twitter_oauth(customer_key, customer_secret, access_token, access_token_secret)

#the function of tweets accessing and analyzing
search <- function(searchterm)
{
  #access tweets and create cumulative file
  list <- searchTwitter(searchterm, n=15)
  typeof(list)
  df <- twListToDF(list)
  df <- df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d')
  if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
  #merge last access with cumulative file and remove duplicates
  stack <- read.csv(file=paste(searchterm, '_stack.csv'))
  stack <- rbind(stack, df)
  stack <- subset(stack, !duplicated(stack$text))
  write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
  #evaluation tweets function
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    scores <- laply(sentences, function(sentence, pos.words, neg.words){
      sentence <- gsub('[[:punct:]]', "", sentence)
      sentence <- gsub('[[:cntrl:]]', "", sentence)
      sentence <- gsub('\\d+', "", sentence)
      sentence <- tolower(sentence)
      word.list <- str_split(sentence, '\\s+')
      words <- unlist(word.list)
      
      pos.matches <- match(words, pos.words)
      neg.matches <- match(words, neg.words)
      pos.matches <- !is.na(pos.matches)
      neg.matches <- !is.na(neg.matches)
      score <- sum(pos.matches) - sum(neg.matches)
      return(score)
    }, pos.words, neg.words, .progress=.progress)
    scores.df <- data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  pos <- scan('C:\\Users\\user\\Documents\\R\\MyRScripts\\SentimentAnalysis\\I Love Sentiment Analysis\\OpinionLexicon\\positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
  neg <- scan('C:\\Users\\user\\Documents\\R\\MyRScripts\\SentimentAnalysis\\I Love Sentiment Analysis\\OpinionLexicon\\negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
  pos.words <- c(pos, 'upgrade')
  neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
  Dataset <- stack
  Dataset$text <- as.factor(Dataset$text)
  scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
  write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results into the file
  #total evaluation: positive / negative / neutral
  stat <- scores
  stat$created <- stack$created
  stat$created <- as.Date(stat$created)
  stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
  by.tweet <- group_by(stat, tweet, created)
  by.tweet <- summarise(by.tweet, number=n())
  write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)
  #create chart
  ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
    geom_point(aes(group=tweet, color=tweet), size=4) +
    theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
    #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
    ggtitle(searchterm)
  ggsave(file=paste(searchterm, '_plot.jpeg'))
}
search("Modi") #enter keyword
