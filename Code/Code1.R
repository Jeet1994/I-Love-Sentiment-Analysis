library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(stringr)
library(ggplot2)
library(plotly)

customer_key <- "udwLTM7gchrQYMQcW15wzj3wj"
customer_secret <- "cK5xlJhQ3rf2Vaix1uCOpg8LJcozmMIQM8g16JNvO2AtDqB0Ed"
access_token <- "466566632-1y63l7WVwsOTOcevkAFexWqqJ6lXHgpHz8GKbHTB"
access_token_secret <- "spMYZGwCk69iCEuO7LSLE5l4qSNFgFqThUuiU5thZScdk"
setup_twitter_oauth(customer_key, customer_secret, access_token, access_token_secret)


tweets_trump <- searchTwitter('@realDonaldTrump', n=100)
tweets_clinton <- searchTwitter('@HillaryClinton', n=100)


feed_trump <- laply(tweets_trump, function(t) t$getText())
feed_sanders <- laply(tweets_sanders, function(t) t$getText())
feed_clinton <- laply(tweets_clinton, function(t) t$getText())
feed_cruz <- laply(tweets_cruz, function(t) t$getText())

#Source of text files : https://github.com/williamgunn/SciSentiment
goood <- scan('~OpinionLexicon\\positive-words.txt', what='character', comment.char=';')
baaad <- scan('~OpinionLexicon\\negative-words.txt',what='character', comment.char=';')


bad_text <- c(baaad, 'wtf', 'wait', 'waiting','epicfail', 'slow')
good_text <- c(goood, 'upgrade', ':)', '#iVoted', 'voted')

score.sentiment <- function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
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

# Retreive scores and add candidate name.
thedonald <- score.sentiment(feed_trump, good_text, bad_text, .progress='text')
thedonald$name <- 'Trump'
clinton <- score.sentiment(feed_clinton, good_text, bad_text, .progress='text')
clinton$name <- 'Clinton'
# Merge into one dataframe for plotting
plotdat <- rbind(thedonald, clinton)
# Cut the text, just gets in the way
plotdat <- plotdat[c("name", "score")]
# Remove neutral values of 0
plotdat <- plotdat[!plotdat$score == 0, ]
# Remove anything less than -3 or greater than 3
plotdat <- plotdat[!plotdat$score > 3, ]
plotdat <- plotdat[!plotdat$score < (-3), ]

# Nice little quick plot
qplot(factor(score), data=plotdat, geom="bar", 
      fill=factor(name),
      xlab = "Sentiment Score")

# Or get funky with ggplot2 + Plotly
ep <- plotdat %>%
  ggplot(aes(x = score, fill = name)) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = c("#0067F7", "#F70000")) +
  theme_classic(base_size = 12) +
  scale_x_continuous(name = "Score") +
  scale_y_continuous(name = "count of tweets") +
  ggtitle("Sentiment Analysis")
theme(axis.title.y = element_text(face="bold", colour="#000000", size=10),
      axis.title.x = element_text(face="bold", colour="#000000", size=8),
      axis.text.x = element_text(angle=16, vjust=0, size=8))
ggplotly(ep)

