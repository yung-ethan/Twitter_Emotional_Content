Emotional Content on Twitter

This was a university project. Our objective was to perform sentiment analysis on Twitter posts to identify possible trends over time. We collected and analyzed a week's worth of Twitter posts. Details on research questions, our process, and findings are in the Final Project Report. The pdf contains the full report and detail, while the HTML is more brief, and perhaps more visually appealing.

All project code was written in R. The code in the "All R Scripts" directory works as follows (the sample files that were produced are included as reference):

filtered_tweet_stream.R --> tweets[n].RData
This uses the streamR package to stream raw tweet data from within the US (approximated by a longitude-latitude grid), which is saved as separate R Data Frames every hour. A Twitter authentication is required in order to run the filterStream function (code assumes that the authentication is contained in "twitter authentication.RData" in the current directory, which was not uploaded); a personal authentication can be obtained by creating a Twitter developer account. The code will run forever unless halted, but data files from every hour before the halt will not be lost.
An unresolved issue is that the Data Frame from each hour will contain a large amount of tweets whose fields are all empty (NAs). For example, from tweets01.RData, there are a total of about 56,000 tweets from that hour, but only 9,000 have filled-in data. This was still a substantial amount of data and the issue did not unduly bias our results, so we simply removed the NA tweets during clean up.

cleanup_tweets.R --> cleaned_tweets1_10.RData,
cleans and prepares Tweet text for analysis.

compile_tweet_dfs.R was simply used to combine all of the Data Frames into one large file. This is typically not recommended; piecemeal analysis was the only feasible method for us, given the data file sizes.

Final_Sentiment_Script.R --> Tweet_Sentiment_Dataframe01_10.RData,
scores the emotional content in each Tweet by using various lexicon dictionaries, and adds the scores to the Data Frames.

FinalProjectGraphics.R and mapPlot.R were used to create the plots included in the final report.

