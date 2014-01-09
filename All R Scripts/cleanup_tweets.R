# This script will load filtered_tweets from RData files
# collected using filtered_tweet_stream.R. Clean up tweets
# by removing invalid tweets filled with NA,
# by removing tweets written in a langauge other than english,
# by removing unnecessary columns,
# and by converting "created_at" (in utc time) to a localized time
# and saving the localized time in a new column "local_time".
# Then, cleaned up tweets will be saved in one file.

# CONSTANTS (change these before running this script)
# Combine and use tweets from files tweets[START].RData ~ tweets[END].RData.
START = 11
END = 20

OUT_FILE = paste("cleaned_tweets", START, "_", END, ".RData", sep="")

# Combine all tweets into one variable "ft" (filtered_tweets)
# This 6.145 seconds for one hour worth of filtered tweets
ft = NULL
for (i in START:END) {
  s = as.character(i)
  if (nchar(s) == 1) s = paste("0", s, sep="")
  load(paste("tweets", s, ".RData", sep=""))
  ft = rbind(ft, filtered_tweets)
}

# Remove invalid tweets filled with NA,
ft = ft[!is.na(ft$created_at),]
ft = ft[!is.na(ft$text),]
ft = ft[!is.na(ft$lat),]
ft = ft[!is.na(ft$lon),]
ft = ft[!is.na(ft$utc_offset),]
ft = ft[!is.na(ft$time_zone),] # dim(ft): 5858 40

# Remove tweets written in a langauge other than english,
ft = ft[ft$lang=="en",] # dim(ft): 5747 40

# Remove unnecessary columns,
ft$favorited=ft$retweet_count=ft$truncated=ft$in_reply_to_screen_name=NULL
ft$source=ft$retweeted=ft$in_reply_to_status_id_str=ft$in_reply_to_user_id_str=NULL
ft$listed_count=ft$verified=ft$location=ft$description=ft$geo_enabled=NULL
ft$user_created_at=ft$statuses_count=ft$followers_count=ft$favourites_count=NULL
ft$protected=ft$user_url=ft$name=ft$lang=ft$friends_count=ft$expanded_url=ft$url=NULL
# dim(ft): 5747 16
# names(ft): 
# [1] "text"         "id_str"       "created_at"   "user_id_str"  "time_zone"   
# [6] "id"           "utc_offset"   "screen_name"  "country_code" "country"     
# [11] "place_type"   "full_name"    "place_name"   "place_id"     "lon"         
# [16] "lat"

# Convert time string from created_at field of data.frame
# to "POSIXlt" "POSIXt" format.
# example of time: "Tue Nov 26 07:01:33 +0000 2013"
monthToInt = list("Nov"=11, "Dec"=12)
toPosix = function(time) {
  month = substr(time, 5, 7)
  newTime = paste(monthToInt[month], substring(time, 9, 19), substring(time, 27))
  return(strptime(newTime, "%m %d %H:%M:%S %Y"))
}

# Convert "created_at" (in utc time) to a localized time
# and save the localized time in a new column "local_time".
# local_time = utc_offset (in seconds) + created_at
# This takes 99s to run for filtered tweets collected over one hour.
now <- Sys.time()
local_times = rep(now, dim(ft)[1])#c(as.integer(ft$utc_offset)[1] + toPosix(ft$created_at[1]))
for (i in 1:dim(ft)[1]) {
  local_times[i] = as.integer(ft$utc_offset)[i] + toPosix(ft$created_at[i])
}
ft$local_time = local_times

# Save the cleaned tweets.
N = dim(ft)[1]
cleaned_tweets = ft
save(cleaned_tweets, file=OUT_FILE)
print("Saved...")
