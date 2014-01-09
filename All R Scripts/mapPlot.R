### Plotting Maps of Tweet Sentiments ###

# plots a series of maps for each hour for the given week of data.

library('maps')
library('RColorBrewer')
library('scales')

cols = brewer.pal(9, 'RdYlGn')
plot(y = rep(1,9), x = seq(1,9, 1), col = cols)

tweets4_df = compiled_tweets[-4 <= compiled_tweets$score & compiled_tweets$score <= 4,]
tweets4_df$time = format(tweets4_df$local_time, '%I%p %a %m-%d')
tweetsMapInfo = data.frame(lat = tweets4_df$lat, lon = tweets4_df$lon, 
                           scores = tweets4_df$score, time = tweets4_df$time)


tweetsMapInfo$lat = as.numeric(as.character(tweetsMapInfo$lat))
tweetsMapInfo$lon = as.numeric(as.character(tweetsMapInfo$lon))
tweetsMapInfo$time = as.character(tweetsMapInfo$time)
save(tweetsMapInfo, file = 'tweetsMapInfo.RData')

lat = tweetsMapInfo$lat
lon = tweetsMapInfo$lon

tweetsMapInfo$hr = substr(tweetsMapInfo$time,1,4)
tweetsMapInfo$date = substr(tweetsMapInfo$time,10,14)
tweet_hr = tweetsMapInfo$hr
tweet_date = tweetsMapInfo$date

#dates = '11-26'
#hrs = '12AM'

dates = c('11-26', '11-27', '11-28', '11-29', '11-30', '11-31', 
          '12-01', '12-02', '12-03')
hrs = c('12AM', '01AM', '02AM', '03AM', '04AM', '05AM', '06AM',
        '07AM', '08AM', '09AM', '10AM', '11AM', '12PM', '01PM',
        '02PM', '03PM', '04PM', '05PM', '06PM', '07PM', '08PM',
        '09PM', '10PM', '11PM')

ct = 1
for (d in dates){
  for (h in hrs){
    jpeg(paste(ct, ' ','map',d, ' ', h, '.jpeg', sep = ''), width = 1400, height = 1000, quality = 100) 
    plot(y = lat[tweet_date == d & tweet_hr == h], x = lon[tweet_date == d & tweet_hr == h],
         col = alpha(cols[tweetsMapInfo$scores + 5], 0.8), pch = 20, cex = 1.9,
         xlab = 'longitude', ylab = 'latititude', xlim = c(-125, -65), ylim = c(25,50))
    title(main = paste('Sentiment of Tweets in United States -- ', d, ', ', h, sep = ''), cex.main = 2.3)
    legend(x = -78, y = 31, legend = c('positive', 'neutral', 'negative'), 
           col = c(cols[9], cols[5], cols[1]), pch = 19, cex = 2.5)
    map(database = 'state', add = T, fill = TRUE, col = alpha('gray',0.06))
    dev.off()
    ct = ct + 1
  }
}


