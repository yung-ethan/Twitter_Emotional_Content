
###This R script loads the lexicons, relevant R packages, cleans tweet text, and scores the sentiment of tweets 

# R packages used
library('streamR')
library('ROAuth')
library("stringr")
library("plyr")
library('XML')

#############################################################################################

###                Part 1: Setting up the lexicon files and sentiment Analysis            ###

#############################################################################################


##### Lexicon Files #####


#### MPQA Lexicon ####

#read file into R
lex=readLines("MPQA Lexicon.tff")

# clean it up and make it into dataframe of just words and polarity (can include subjectivity of the word)
clean_lexicon = function(lexicon=lex,subjectivity=FALSE){
  lexicon=lex
  revised.lexicon=gsub("len=1","",lexicon)
  revised.lexicon=gsub("word1","word",revised.lexicon) 
  
  # Grab the word
  word.lexicon=str_extract(revised.lexicon,"word=[[:alpha:]]+")
  word=gsub("word=","",word.lexicon)
  
  # Grab polarity
  polarity.lexicon=str_extract(revised.lexicon,"priorpolarity=[[:alpha:]]+")
  polarity=gsub("priorpolarity=","",polarity.lexicon)
  
  if(subjectivity){
  # grab subjectivity
  subj.lexicon=str_extract(revised.lexicon,"type=[[:alpha:]]+")
  subjectivity=gsub("type=","",subj.lexicon)
  subjectivity=gsub("subj","",subjectivity)
  return(data.frame(word,polarity,subjectivity))
  }  
  return(data.frame(word,polarity))
}

wordpolarity=clean_lexicon(lex,subjectivity=TRUE)

# MPQA Word Lists by Polarity
MPQA.negative=wordpolarity$word[wordpolarity$polarity=="negative"]
MPQA.positive=wordpolarity$word[wordpolarity$polarity=="positive"]
MPQA.both=wordpolarity$word[wordpolarity$polarity=="both"]
MPQA.neutral=wordpolarity$word[wordpolarity$polarity=="neutral"]

#### default Lexicon developed in a paper by Hu and Liu in 2004 ####

default.pos=readLines("positive_words.txt")
default.neg=readLines("negative_words.txt")

#### WordNet Affect Emotion Word Lists ####

# function to clean and prep the lexicon files 
readEmo = function(Emofile){
  if(is.character(Emofile)==F){
    stop("Input should be character string indicate the location of the file")
  }
  raw = scan(Emofile,
             what = "character",
             sep = " ")
  delete = grep("[[:alpha:]][#][[:digit:]]+",raw)
  wordlist = raw[-delete]
  wordlist = gsub("[_]"," ",wordlist,ignore.case = TRUE)
  return(wordlist)
}

# Emotion Word Lists (Custom Pos/Neg word Lists from the WordNet Affect Emotion Lexicon)
Emo.pos=readEmo("joy.txt")
Emo.neg=c(anger = readEmo("anger.txt"),disgust = readEmo("disgust.txt"),fear = readEmo("fear.txt"),sadness = readEmo("sadness.txt"))
Emo.both=readEmo("surprise.txt") # because this list has words that can have either positive or negative associated sentiment

# remove problem entries in the lexicons (too subjective)
Emo.pos=Emo.pos[-match(c("like","look for","look to"), Emo.pos)]
Emo.neg=Emo.neg[-match(c("blue","down","low","alarm","sorry","sick","rag","bad"),Emo.neg)]

#### Overlapping Lexicons ####

EmoDef.pos.overlap=default.pos[which(!is.na(match(default.pos,Emo.pos)))]
EmoDef.neg.overlap=default.neg[which(!is.na(match(default.neg,Emo.neg)))]
MPQAEmoDef.neg=MPQA.negative[which(!is.na(match(MPQA.negative,EmoDef.neg.overlap)))]
MPQAEmoDef.pos=MPQA.positive[which(!is.na(match(MPQA.positive,EmoDef.pos.overlap)))]
MPQAEmoDef.pos=MPQAEmoDef.pos[-which(MPQAEmoDef.pos=="like")]
both=c(paste(MPQA.both),Emo.both)

#### Psych Lexicon ####

# Some webscraping work
psych=readHTMLTable("http://www.psychpage.com/learning/library/assess/feelings.html")
psych=psych[[1]]
pos=psych[1:21,] #open, happy, alive, good
socialpos=psych[22:39,] #love, interested, positive,strong
neg=psych[42:65,] # angry, depressed, confused, helpless
socialneg=psych[66:85,] #indifferent, afraid, hurt, sad

# Cleaning up the lexicon
extractwords=function(column){tolower(str_extract(column,"[[:alpha:]].+")[!is.na(str_extract(column,"[[:alpha:]].+"))])}

# Psych Lexicon Lists
psych.pos=c(paste(unlist(apply(pos,2,extractwords))),paste(unlist(apply(socialpos,2,extractwords))))           
psych.neg=c(paste(unlist(apply(neg,2,extractwords))),paste(unlist(apply(socialneg,2,extractwords))))                  

#### Small Internet Lingo List to capture obvious sentiment ####
pos.abbrev=c("lol","rofl","lmao","lmfao","jk","ilu","ily","thx","tu","haha","ha","hahaha","whoo","woohoo")
neg.abbrev=c("wtf")

###################################################################

##                          Lexicon Index                        ##

# default: default.pos (2006), default.neg (4783)
# MPQA by Polarity: MPQA.positive (2718), MPQA.negative (4912), MPQA.both (21), MPQA.neutral (570)
# Custom Pos/Neg from Emotion Lexicon: Emo.pos (536), Emo.neg (899), Emo.both (90)
# Overlap: MPQAEmoDef.pos (189), MPQAEmoDef.neg (391), EmoDef.pos.overlap (169), EmoDef.neg.overlap (336)
# Both: both (111) (overlap in MPQA.both and Emo.both)
# Psych 'Feeling' List: psych.pos (122), psych.neg (147)
# Internet Lingo: pos.abbrev (14), neg.abbrev (1)

###################################################################


######### Sentiment Score Function ##########

# Note: This function originates from R code in a presentation given by Jeffrey Breen but has been significantly modified

score.sentiment = function(sentences, threshold=.99, pos.words=default.pos, neg.words=default.neg, neutral.words=MPQA.neutral, both.words=MPQA.both, weight=TRUE)
{
  scores = laply(sentences, #function in plyr package; not a typo. Sentences are tweet text in our case
                 function(sentence, pos.words, neg.words,neutral.words,both.words,weight){     
                   # define error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionary terms              
                   pos.matches = !is.na(match(words, pos.words))
                   neg.matches = !is.na(match(words, neg.words))
                   neutral.matches=!is.na(match(words,neutral.words))
                   both.matches=!is.na(match(words,both.words))
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   
                   if(weight==TRUE){
                     # weights assess some initial overlap in lexicons
                     pos.weight1=sum(!is.na(match(words,Emo.pos)))+sum(!is.na(match(words,EmoDef.pos.overlap)))
                     neg.weight1=sum(!is.na(match(words,Emo.neg)))+sum(!is.na(match(words,EmoDef.neg.overlap)))
                     # additional weights assess presence in two additional lexicons: psych feeling words and internet lingo
                     pos.weight2=sum(!is.na(match(words,psych.pos)))+sum(!is.na(match(words,pos.abbrev)))
                     neg.weight2=sum(!is.na(match(words,psych.neg)))+sum(!is.na(match(words,neg.abbrev)))
                     #overall counts to be used in score calculation
                     pos=sum(pos.matches)+pos.weight1+pos.weight2 
                     neg=sum(neg.matches)+neg.weight1+neg.weight2 
                     both=sum(both.matches)
                     #final score
                     score=pos-neg
                     # is the tweet emotional? Yields a logical. True means emotional, false means non-emotional.
                     emotion=((score>threshold)|(pos>threshold)|(neg>threshold)|(both>threshold))
                     # give a vector with sentiment information
                     return(c(score,pos,neg,sum(neutral.matches),both,emotion))
                   }
                    # if weights ==FALSE, need to redefine emotion
                   emotion=((score>threshold)|(sum(pos.matches)>threshold)|(sum(neg.matches)>threshold)|(sum(both.matches)>threshold))
                   # returns a vector with sentiment information
                   return(c(score,sum(pos.matches),sum(neg.matches),sum(neutral.matches),sum(both.matches),emotion))
                 }, pos.words, neg.words, neutral.words, both.words, weight) #finishes the laply function arguments
  
  # data frame with scores for each tweet (i.e. sentences)
  scores.df = data.frame(score=scores,text=sentences)
  score=scores.df$score.1 ; pos=scores.df$score.2 ; neg= scores.df$score.3 ; neutral=scores.df$score.4 ; both=scores.df$score.5 ; emotion=scores.df$score.6
  colnames(scores.df)=c("score","pos","neg","neutral","both","emotion","text")
  scores.df$emotion=as.logical(scores.df$emotion)
  return(scores.df)
}

#############################################################################################

###                          Part 2: Tweet Sentiment Processing                           ###

#############################################################################################


#### Load and Clean tweets ####

# load tweets
load("cleaned_tweets.RData")  

### clean up tweets using Yekun's function ###
cleanTweet = function(some_txt)
{
  # remove retweet entities
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  # remove at people
  some_txt = gsub("@\\w+", "", some_txt)
  # remove punctuation
  some_txt = gsub("[[:punct:]]", "", some_txt)
  # remove numbers
  some_txt = gsub("[[:digit:]]", "", some_txt)
  # remove html links
  some_txt = gsub("http\\w+", "", some_txt)
  # remove unnecessary spaces
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = iconv(some_txt, "latin1", "ASCII", sub="")
  # remove emoticons
  some_txt = some_txt = gsub("[\n]", " ", some_txt)
  
  return(some_txt)
} 

# clean all tweet text
cleaned_tweets[1]=as.data.frame(sapply(cleaned_tweets[1],cleanTweet))

divide_tweets = cleaned_tweets

div = 10000
tweets = list()
while (nrow(divide_tweets) > div) {
  tweets = c(tweets, list(divide_tweets[1:div,]))
  divide_tweets = divide_tweets[(div + 1):nrow(divide_tweets),]
}
tweets = c(tweets, list(divide_tweets[1:nrow(divide_tweets),]))


#### Sentiment Processing ####

#### Overall polarity score for tweet as integer ###
weight.scores = lapply(tweets, function(tweet_df){score.sentiment(tweet_df[[1]])})
combined.df = weight.scores[[1]]
for (i in 2:length(weight.scores)){
  combined.df = rbind(combined.df, weight.scores[[i]]) 
}
weight.scores = combined.df

#### Data frame of all tweets with added sentiment score information ####
weight.time.sentiment.df=data.frame(cleaned_tweets,weight.scores)

# save it to a file (because this script was run as a batch job on the stats server over all the tweet files)
save(weight.time.sentiment.df,file="Tweet_Sentiment_Dataframe.RData")

