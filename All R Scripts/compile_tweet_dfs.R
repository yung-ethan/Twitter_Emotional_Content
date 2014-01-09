### Compile All Tweets into Single Dataframe

# following code simply consolidates tweets into single large data frame.


ranges = c('01_10','11_20', '21_30','31_40','41_50','51_60','61_70','71_80','81_90','91_100','101_110',
           '111_120','121_130','131_140','141_150','151_160','161_170','171_180','181_190')

compiled_tweets = data.frame()
for (r in ranges){
  file_name = paste('Tweet_Sentiment_Dataframe', r, '.RData', sep = '')
  print(file_name)
  load(file_name)
  compiled_tweets = rbind(compiled_tweets, weight.time.sentiment.df)
  rm(weight.time.sentiment.df)  
}

compiled_tweets = rbind(compiled_tweets, weight.time.sentiment.df)
save(compiled_tweets, file = 'compiled_tweets.RData')