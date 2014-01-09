# Stat 133 Final Project Data Analysis
# setwd('C:/Users/Yekun Wang/Dropbox/School/Stat 133/Final Project/DataProcess')
library('streamR')
library('ROAuth')
library('twitteR')
library('ggplot2')
library('RColorBrewer')
#load('C:/Users/Yekun Wang/Dropbox/School/Stat 133/Final Project/DataProcess/workspace.Rdata')
content_tweets = compiled_tweets[complete.cases(compiled_tweets),]

##Total percent of emotions  
PctEmo = sum(compiled_tweets$emotion)/nrow(compiled_tweets) #0.535869

## Day of the Week
uDays = unique(substr(content_tweets$local_time,1,10))
uWeekdays = c('Mon1','Tue1','Wed1','Thu','Fri','Sat','Sun','Mon2','Tue2','Wed2')

Tue1 = content_tweets[substr(content_tweets$local_time,1,10)== uDays[1],]
Wed1 = content_tweets[substr(content_tweets$local_time,1,10)== uDays[3],]
Thu = content_tweets[substr(content_tweets$local_time,1,10)== uDays[4],]
Fri = content_tweets[substr(content_tweets$local_time,1,10)== uDays[5],]
Sat = content_tweets[substr(content_tweets$local_time,1,10)== uDays[6],]
Sun = content_tweets[substr(content_tweets$local_time,1,10)== uDays[7],]
Mon2 = content_tweets[substr(content_tweets$local_time,1,10)== uDays[8],]
Tue2 = content_tweets[substr(content_tweets$local_time,1,10)== uDays[9],]

AvgDayScore = c(mean(Tue1$score),mean(Wed1$score),mean(Thu$score),
                mean(Fri$score),mean(Sat$score),mean(Sun$score),mean(Mon2$score),
                mean(Tue2$score))
Date = paste(substr(uDays,6,10),uWeekdays)[2:9]

## Daily positive and negative
pctValence = function(dataF){
  pctPos = 1 - sum(dataF$pos == 0)/nrow(dataF)
  pctNeg = 1 - sum(dataF$neg == 0)/nrow(dataF)
  pctNeu = 1 - sum(dataF$neutral == 0)/nrow(dataF)
  t = matrix(c(pctPos,pctNeg,pctNeu),1,3)
  colnames(t) = c('PercentPositive','PercentNegative','PercentNeutral')
  return(t)
}

qplot(Date,AvgDayScore,
     main = "Average Sentiment Score vs. Day of the Week",
     xlab = "Day",
     ylab = "Average Sentiment Score")

## Hourly Data
# calculate average score for each hour
AvgHrScore = function(dataF){
  uniqueHr = unique(substr(dataF$local_time,12,13))
  uniqueHrScore = sapply(uniqueHr,function(x){mean(dataF[substr(dataF$local_time,12,13)==x,]$score)})
  l = list(uniqueHr[order(uniqueHr)],uniqueHrScore[order(uniqueHr)])
  return(l)
}

HSTue1 = AvgHrScore(Tue1)
HSWed1 = AvgHrScore(Wed1)
HSThu = AvgHrScore(Thu)
HSFri = AvgHrScore(Fri)
HSSat = AvgHrScore(Sat)
HSSun = AvgHrScore(Sun)
HSMon2 = AvgHrScore(Mon2)
HSTue2 = AvgHrScore(Tue2)

# Plot for each day
myColors = brewer.pal(8,'Spectral')
par(mfrow = c(2,4))
plot(HSTue1[[1]],HSTue1[[2]],main = "Tue1",
     ylab = "Avg.Sentiment Score", xlab = "Hr",
     pch = 19,cex = 1.5, col = myColors[1])
plot(HSWed1[[1]],HSWed1[[2]],main = "Wed1",
     ylab = "Avg.Sentiment Score", xlab = "Hr",
     pch = 19,cex = 1.5, col = myColors[2])
plot(HSThu[[1]],HSThu[[2]],main = "Thu",
     ylab = "Avg.Sentiment Score", xlab = "Hr",
     pch = 19,cex = 1.5, col = myColors[3])
plot(HSFri[[1]],HSFri[[2]],main = "Fri",
     ylab = "Avg.Sentiment Score", xlab = "Hr",
     pch = 19,cex = 1.5, col = myColors[4])
plot(HSSat[[1]],HSSat[[2]],main = "Sat",
     ylab = "Avg.Sentiment Score", xlab = "Hr",
     pch = 19,cex = 1.5, col = myColors[5])
plot(HSSun[[1]],HSSun[[2]],main = "Sun",
     ylab = "Avg.Sentiment Score", xlab = "Hr",
     pch = 19,cex = 1.5, col = myColors[6])
plot(HSMon2[[1]],HSMon2[[2]],main = "Mon2",
     ylab = "Avg.Sentiment Score", xlab = "Hr",
     pch = 19,cex = 1.5, col = myColors[7])
plot(HSTue2[[1]],HSTue2[[2]],main = "Tue2",
     ylab = "Avg.Sentiment Score", xlab = "Hr",
     pch = 19,cex = 1.5, col = myColors[8])
par(mfrow = c(1,1))

# Plot for entire week
dfTue1 = data.frame(Weekday = rep('Tue1',length(HSTue1[[1]])),
                    Hr= HSTue1[[1]], AvgScore = HSTue1[[2]],
                    Date = rep('11-26',length(HSTue1[[1]])))

dfWed1 = data.frame(Weekday = rep('Wed1',length(HSWed1[[1]])), 
                    Hr= HSWed1[[1]], AvgScore = HSWed1[[2]],
                    Date = rep('11-27',length(HSWed1[[1]])))

dfThu = data.frame(Weekday = rep('Thu',length(HSThu[[1]])), 
                   Hr= HSThu[[1]], AvgScore = HSThu[[2]],
                   Date = rep('11-28',length(HSThu[[1]])))

dfFri = data.frame(Weekday = rep('Fri',length(HSFri[[1]])), 
                   Hr= HSFri[[1]], AvgScore = HSFri[[2]],
                   Date = rep('11-29',length(HSFri[[1]])))

dfSat= data.frame(Weekday = rep('Sat',length(HSSat[[1]])), 
                  Hr= HSSat[[1]], AvgScore = HSSat[[2]],
                  Date = rep('11-30',length(HSSat[[1]])))

dfSun= data.frame(Weekday = rep('Sun',length(HSSat[[1]])), 
                  Hr= HSSun[[1]], AvgScore = HSSun[[2]],
                  Date = rep('12-01',length(HSSun[[1]])))

dfMon2 = data.frame(Weekday = rep('Mon2',length(HSMon2[[1]])), 
                    Hr= HSMon2[[1]], AvgScore = HSMon2[[2]],
                    Date = rep('12-02',length(HSMon2[[1]])))

dfTue2 = data.frame(Weekday = rep('Tue2',length(HSTue2[[1]])), 
                    Hr= HSTue2[[1]], AvgScore = HSTue2[[2]],
                    Date = rep('12-03',length(HSTue2[[1]])))
# Combined hourly average sentiment score
EmoHrAvg = rbind(dfTue1,dfWed1,dfThu,dfFri,dfSat,dfSun,dfMon2,dfTue2)
EmoHrAvg$time = paste('2013',EmoHrAvg$Date,EmoHrAvg$Hr,sep = '-')

# Plot hourly average sentiment score
theme_set(theme_bw(15))
p = ggplot() + ggtitle('Avg.Sentiment Score vs. Hour')
p = p +geom_line(data = EmoHrAvg, aes(x = Hr, y = AvgScore, group = Weekday, colour = Weekday))
p = p +geom_point(data = EmoHrAvg, aes(x = Hr, y = AvgScore, group = Weekday, colour = Weekday))
p = p +scale_colour_manual(values=myColors)
p = p + xlab('Hour') + ylab('Avg.Sentiment Score')
p

# boxplot
qplot(Weekday, AvgScore, data=EmoHrAvg, geom=c("boxplot"), 
      fill = Weekday,
      main="Hourly Average Sentiment Score",
      xlab="Day", ylab="Average Sentiment Score") + scale_fill_manual(values=myColors)

# timeseires
plot(EmoHrAvg$AvgScore,xaxt = "n",pch = 19,cex = 0.6, col = "black",
     main = "TimeSeries: 2013/11/26-2013/12/3",
     xlab = "Time",
     ylab = "Average Score")
axis(1, at=seq(1,192,length=8),label = uWeekdays[2:9] )
abline(v = 49,col = "orange")
abline(v = 72,col = 'orange')
mtext('Thanksgiving',3, col = 'orange', cex = 0.7, at = 55)
abline(v = 112.5, col = 'darkgreen')
mtext("PaulWalker'sDeath", 3, col = 'darkgreen', cex = 0.7, at = 115)
abline(v = 183, col = 'blue')
text(155,1.2,"Yankees Signed Ellsbury", col = 'blue', cex = 0.7)
