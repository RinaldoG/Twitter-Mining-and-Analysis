#### Mining and Analysing Tweets from Various Twitter Accounts ####
### @BernieSanders ###
### @elonmusk ###
### @chiefs ###
### @kwucoyotes ###
### @MAC_Bulldogs ###

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

## Package Installation ##

# Installing package 'Pacman' to help install and load other packages
if (!require("pacman")) install.packages("pacman")
# Pacman package installing and loading all required packages for project
pacman::p_load(rtweet,tidyverse,tidytext,tm,wordcloud,syuzhet,forestmangr,rmarkdown,tinytex)
tinytex::install_tinytex()

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

## Twitter Data Access ##

#Twitter Token with required Twitter credentials obtained from API in order to obtain Tweet data from Twitter
twitter_token <- create_token(
  app = ("Senior Seminar App"),
  consumer_key = ("Fill In"),
  consumer_secret = ("Fill In"),
  access_token = ("Fill In"),
  access_secret = ("Fill In"),
  set_renv = TRUE)

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

### @BernieSanders ### - Politician Bernie Sanders

## Tweet Mining ##

#Varible bernie_tweets will contain tweets from @BernieSanders "Bernie Sanders Twitter Account"
bernie_tweets <- get_timeline("@BernieSanders", n=3200)

## Tweet Analysis ##

# Remove retweets
bernie_tweets_organic <- bernie_tweets[bernie_tweets$is_retweet==FALSE, ] 
# Remove replies
bernie_tweets_organic <- subset(bernie_tweets_organic, is.na(bernie_tweets_organic$reply_to_status_id)) 

#Organic Tweet with most Favourites 
bernie_tweets_favorite <- bernie_tweets_organic %>% arrange(-favorite_count)
bernie_tweets_favorite[1,5]
#Organic Tweet with most Retweets 
bernie_tweets_retweet <- bernie_tweets_organic %>% arrange(-retweet_count)
bernie_tweets_retweet[1,5]

## RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS ## 

# Keeping only the retweets
bernie_retweets <- bernie_tweets[bernie_tweets$is_retweet==TRUE,]
bernie_retweets
# Keeping only the replies
bernie_replies <- subset(bernie_tweets, !is.na(bernie_tweets$reply_to_status_id))
bernie_replies

#Creating a data frame
data_bernie <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2332, 799, 67)
)

# Adding columns 
data_bernie$fraction = data_bernie$count / sum(data_bernie$count)
data_bernie$percentage = data_bernie$count / sum(data_bernie$count) * 100
data_bernie$ymax = cumsum(data_bernie$fraction)
data_bernie$ymin = c(0, head(data_bernie$ymax, n=-1))

# Rounding the data to two decimal points
data_bernie <- round_df(data_bernie, 2)

# Specify what the legend should say
Type_of_Tweet_Bernie <- paste(data_bernie$category, data_bernie$percentage, "%")

#Plotting a Donut Chart
ggplot(data_bernie, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet_Bernie)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

## Tweet Timeline Production ##

#Linegraph of tweet frequency
colnames(bernie_tweets)[colnames(bernie_tweets)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(bernie_tweets, Twitter_Account), "week") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Bernie Sanders",
    subtitle = "Tweet counts aggregated by week",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## Tweet Publish Location ##

#Bernie Sanders Tweet Locations
sanders_app <- bernie_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
sanders_app <- subset(sanders_app, count > 11)

#Data Frame Creation
data.sanders <- data.frame(
  category=sanders_app$source,
  count=sanders_app$count
)

# Adding columns 
data.sanders$fraction = data.sanders$count / sum(data.sanders$count)
data.sanders$percentage = data.sanders$count / sum(data.sanders$count) * 100
data.sanders$ymax = cumsum(data.sanders$fraction)
data.sanders$ymin = c(0, head(data.sanders$ymax, n=-1))
data.sanders <- round_df(data.sanders, 2)
Source <- paste(data.sanders$category, data.sanders$percentage, "%")

#Donut Chart of Tweet Locations
ggplot(data.sanders, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


## Word Frequency Analysis from all @BernieSanders Tweets ##

#Cleaning of unwanted characters
bernie_tweets_organic$text <-  gsub("https\\S*", "", bernie_tweets_organic$text)
bernie_tweets_organic$text <-  gsub("@\\S*", "", bernie_tweets_organic$text) 
bernie_tweets_organic$text  <-  gsub("amp", "", bernie_tweets_organic$text) 
bernie_tweets_organic$text  <-  gsub("[\r\n]", "", bernie_tweets_organic$text)
bernie_tweets_organic$text  <-  gsub("[[:punct:]]", "", bernie_tweets_organic$text)

#Removal of stop words such as 'and' and 'is'
bernie_tweets <- bernie_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
bernie_tweets <- bernie_tweets %>%
  anti_join(stop_words)

#Bar chart of the most frequent words found in the tweets
bernie_tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Bernie Sanders",
       subtitle = "Stop words removed from the list")

## Hashtag Frequency Analysis ##

#Obtaining all hashtags from @BernieSanders tweets
bernie_tweets_organic$hashtags <- as.character(bernie_tweets_organic$hashtags)
bernie_tweets_organic$hashtags <- gsub("c\\(", "", bernie_tweets_organic$hashtags)

#Creating Wordcloud of Hashtag Frequencies
set.seed(1234)
wordcloud(bernie_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## Retweet Account Frequency Analysis ##

#Creating Wordcloud of Retweet Account Frequencies
set.seed(1234)
wordcloud(bernie_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

## Sentiment Analysis of Tweets ##

# Converting tweets to ASCII to trackle strange characters
bernie_tweets <- iconv(bernie_tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets
bernie_tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",bernie_tweets)

# removing mentions
bernie_tweets <-gsub("@\\w+","",bernie_tweets)
ew_sentiment<-get_nrc_sentiment((bernie_tweets))
bernie_sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(bernie_sentimentscores) <- "Score"
bernie_sentimentscores <- cbind("sentiment"=rownames(bernie_sentimentscores),bernie_sentimentscores)
rownames(bernie_sentimentscores) <- NULL

#Plotting a bar graph based on sentiment scores
ggplot(data=bernie_sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Bernie Sanders total sentiment based on scores")+
  theme_minimal()

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

### @elonmusk ### - Elon Musk Influencer

## Tweet Mining ##

#Varible elon_tweets will contain tweets from @elonmusk "Elon Musk Twitter Account"
elon_tweets <- get_timeline("@elonmusk", n=3200)

## Tweet Analysis ##

# Remove retweets
elon_tweets_organic <- elon_tweets[elon_tweets$is_retweet==FALSE, ] 
# Remove replies
elon_tweets_organic <- subset(elon_tweets_organic, is.na(elon_tweets_organic$reply_to_status_id)) 

#Organic Tweet with most Favourites 
elon_tweets_favorite <- elon_tweets_organic %>% arrange(-favorite_count)
elon_tweets_favorite[1,5]
#Organic Tweet with most Retweets 
elon_tweets_retweet <- elon_tweets_organic %>% arrange(-retweet_count)
elon_tweets_retweet[1,5]

## RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS ## 

# Keeping only the retweets
elon_retweets <- elon_tweets[elon_tweets$is_retweet==TRUE,]
elon_retweets
# Keeping only the replies
elon_replies <- subset(elon_tweets, !is.na(elon_tweets$reply_to_status_id))
elon_replies

#Creating a data frame
data_elon <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(465,316,2418)
)

# Adding columns 
data_elon$fraction = data_elon$count / sum(data_elon$count)
data_elon$percentage = data_elon$count / sum(data_elon$count) * 100
data_elon$ymax = cumsum(data_elon$fraction)
data_elon$ymin = c(0, head(data_elon$ymax, n=-1))

# Rounding the data to two decimal points
data_elon <- round_df(data_elon, 2)

# Specify what the legend should say
Type_of_Tweet_Elon <- paste(data_elon$category, data_elon$percentage, "%")

#Plotting a Donut Chart
ggplot(data_elon, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet_Elon)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


## Tweet Timeline Production ##

#Linegraph of tweet frequency
colnames(elon_tweets)[colnames(elon_tweets)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(elon_tweets, Twitter_Account), "week") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Elon Musk",
    subtitle = "Tweet counts aggregated by week",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## Tweet Publish Location ##

#Elon Musk Tweet Locations
musk_app <- elon_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
musk_app <- subset(musk_app, count > 11)

#Data Frame Creation
data.musk <- data.frame(
  category=musk_app$source,
  count=musk_app$count
)

# Adding columns 
data.musk$fraction = data.musk$count / sum(data.musk$count)
data.musk$percentage = data.musk$count / sum(data.musk$count) * 100
data.musk$ymax = cumsum(data.musk$fraction)
data.musk$ymin = c(0, head(data.musk$ymax, n=-1))
data.musk <- round_df(data.musk, 2)
Source <- paste(data.musk$category, data.musk$percentage, "%")

#Donut Chart of Tweet Locations
ggplot(data.musk, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


## Word Frequency Analysis from all @elonmusk Tweets ##

#Cleaning of unwanted characters
elon_tweets_organic$text <-  gsub("https\\S*", "", elon_tweets_organic$text)
elon_tweets_organic$text <-  gsub("@\\S*", "", elon_tweets_organic$text) 
elon_tweets_organic$text  <-  gsub("amp", "", elon_tweets_organic$text) 
elon_tweets_organic$text  <-  gsub("[\r\n]", "", elon_tweets_organic$text)
elon_tweets_organic$text  <-  gsub("[[:punct:]]", "", elon_tweets_organic$text)

#Removal of stop words such as 'and' and 'is'
elon_tweets <- elon_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
elon_tweets <- elon_tweets %>%
  anti_join(stop_words)

#Bar chart of the most frequent words found in the tweets
elon_tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Elon Musk",
       subtitle = "Stop words removed from the list")

## Hashtag Frequency Analysis ##

#Obtaining all hashtags from @elonmusk tweets
elon_tweets_organic$hashtags <- as.character(elon_tweets_organic$hashtags)
elon_tweets_organic$hashtags <- gsub("c\\(", "", elon_tweets_organic$hashtags)

#Creating Wordcloud of Hashtag Frequencies
set.seed(1234)
wordcloud(elon_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## Retweet Account Frequency Analysis ##

#Creating Wordcloud of Retweet Account Frequencies
set.seed(1234)
wordcloud(elon_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

## Sentiment Analysis of Tweets ##

# Converting tweets to ASCII to trackle strange characters
elon_tweets <- iconv(elon_tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets
elon_tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",elon_tweets)

# removing mentions
elon_tweets <-gsub("@\\w+","",elon_tweets)
ew_sentiment<-get_nrc_sentiment((elon_tweets))
elon_sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(elon_sentimentscores) <- "Score"
elon_sentimentscores <- cbind("sentiment"=rownames(elon_sentimentscores),elon_sentimentscores)
rownames(elon_sentimentscores) <- NULL

#Plotting a bar graph based on sentiment scores
ggplot(data=elon_sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Elon Musk total sentiment based on scores")+
  theme_minimal()

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

### @chiefs ### - Kansas City Chiefs Major Sporting Team

## Tweet Mining ##

#Varible chiefs_tweets will contain tweets from @chiefs "Kansas City Chiefs Twitter Account"
chiefs_tweets <- get_timeline("@chiefs", n=3200)

## Tweet Analysis ##

# Remove retweets
chiefs_tweets_organic <- chiefs_tweets[chiefs_tweets$is_retweet==FALSE, ] 
# Remove replies
chiefs_tweets_organic <- subset(chiefs_tweets_organic, is.na(chiefs_tweets_organic$reply_to_status_id)) 

#Organic Tweet with most Favourites 
chiefs_tweets_favorite <- chiefs_tweets_organic %>% arrange(-favorite_count)
chiefs_tweets_favorite[1,5]
#Organic Tweet with most Retweets 
chiefs_tweets_retweet <- chiefs_tweets_organic %>% arrange(-retweet_count)
chiefs_tweets_retweet[1,5]

## RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS ## 

# Keeping only the retweets
chiefs_retweets <- chiefs_tweets[chiefs_tweets$is_retweet==TRUE,]
chiefs_retweets
# Keeping only the replies
chiefs_replies <- subset(chiefs_tweets, !is.na(chiefs_tweets$reply_to_status_id))
chiefs_replies

#Creating a data frame
data_chiefs <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2791,361,48)
)

# Adding columns 
data_chiefs$fraction = data_chiefs$count / sum(data_chiefs$count)
data_chiefs$percentage = data_chiefs$count / sum(data_chiefs$count) * 100
data_chiefs$ymax = cumsum(data_chiefs$fraction)
data_chiefs$ymin = c(0, head(data_chiefs$ymax, n=-1))

# Rounding the data to two decimal points
data_chiefs <- round_df(data_chiefs, 2)

# Specify what the legend should say
Type_of_Tweet_Chiefs <- paste(data_chiefs$category, data_chiefs$percentage, "%")

#Plotting a Donut Chart
ggplot(data_chiefs, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet_Chiefs)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

## Tweet Timeline Production ##

#Linegraph of tweet frequency
colnames(chiefs_tweets)[colnames(chiefs_tweets)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(chiefs_tweets, Twitter_Account), "week") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Kansas City Chiefs",
    subtitle = "Tweet counts aggregated by week",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## Tweet Publish Location ##

#Chiefs Tweet Locations
chiefs_app <- chiefs_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
chiefs_app <- subset(chiefs_app, count > 11)

#Data Frame Creation
data.city <- data.frame(
  category=chiefs_app$source,
  count=chiefs_app$count
)

# Adding columns 
data.city$fraction = data.city$count / sum(data.city$count)
data.city$percentage = data.city$count / sum(data.city$count) * 100
data.city$ymax = cumsum(data.city$fraction)
data.city$ymin = c(0, head(data.city$ymax, n=-1))
data.city <- round_df(data.city, 2)
Source <- paste(data.city$category, data.city$percentage, "%")

#Donut Chart of Tweet Locations
ggplot(data.city, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


## Word Frequency Analysis from all @chiefs Tweets ##

#Cleaning of unwanted characters
chiefs_tweets_organic$text <-  gsub("https\\S*", "", chiefs_tweets_organic$text)
chiefs_tweets_organic$text <-  gsub("@\\S*", "", chiefs_tweets_organic$text) 
chiefs_tweets_organic$text  <-  gsub("amp", "", chiefs_tweets_organic$text) 
chiefs_tweets_organic$text  <-  gsub("[\r\n]", "", chiefs_tweets_organic$text)
chiefs_tweets_organic$text  <-  gsub("[[:punct:]]", "", chiefs_tweets_organic$text)

#Removal of stop words such as 'and' and 'is'
chiefs_tweets <- chiefs_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
chiefs_tweets <- chiefs_tweets %>%
  anti_join(stop_words)

#Bar chart of the most frequent words found in the tweets
chiefs_tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Kansas City Chiefs",
       subtitle = "Stop words removed from the list")

## Hashtag Frequency Analysis ##

#Obtaining all hashtags from @elonmusk tweets
chiefs_tweets_organic$hashtags <- as.character(chiefs_tweets_organic$hashtags)
chiefs_tweets_organic$hashtags <- gsub("c\\(", "", chiefs_tweets_organic$hashtags)

#Creating Wordcloud of Hashtag Frequencies
set.seed(1234)
wordcloud(chiefs_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## Retweet Account Frequency Analysis ##

#Creating Wordcloud of Retweet Account Frequencies
set.seed(1234)
wordcloud(chiefs_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

## Sentiment Analysis of Tweets ##

# Converting tweets to ASCII to trackle strange characters
chiefs_tweets <- iconv(chiefs_tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets
chiefs_tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",chiefs_tweets)

# removing mentions
chiefs_tweets <-gsub("@\\w+","",chiefs_tweets)
ew_sentiment<-get_nrc_sentiment((chiefs_tweets))
chiefs_sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(chiefs_sentimentscores) <- "Score"
chiefs_sentimentscores <- cbind("sentiment"=rownames(chiefs_sentimentscores),chiefs_sentimentscores)
rownames(chiefs_sentimentscores) <- NULL

#Plotting a bar graph based on sentiment scores
ggplot(data=chiefs_sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Kansas City Chiefs total sentiment based on scores")+
  theme_minimal()

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

### @kwucoyotes ### - Kansas Wesleyan Coyotes College Athletic Twitter

## Tweet Mining ##

#Varible kwu_tweets will contain tweets from @kwucoyotes "Kansas Wesleyan Coyotes Twitter Account"
kwu_tweets <- get_timeline("@kwucoyotes", n=3200)

## Tweet Analysis ##

# Remove retweets
kwu_tweets_organic <- kwu_tweets[kwu_tweets$is_retweet==FALSE, ] 
# Remove replies
kwu_tweets_organic <- subset(kwu_tweets_organic, is.na(kwu_tweets_organic$reply_to_status_id)) 

#Organic Tweet with most Favourites 
kwu_tweets_favorite <- kwu_tweets_organic %>% arrange(-favorite_count)
kwu_tweets_favorite[1,5]
#Organic Tweet with most Retweets 
kwu_tweets_retweet <- kwu_tweets_organic %>% arrange(-retweet_count)
kwu_tweets_retweet[1,5]

## RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS ## 

# Keeping only the retweets
kwu_retweets <- kwu_tweets[kwu_tweets$is_retweet==TRUE,]
kwu_retweets
# Keeping only the replies
kwu_replies <- subset(kwu_tweets, !is.na(kwu_tweets$reply_to_status_id))
kwu_replies

#Creating a data frame
data_kwu <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2863,266,71)
)

# Adding columns 
data_kwu$fraction = data_kwu$count / sum(data_kwu$count)
data_kwu$percentage = data_kwu$count / sum(data_kwu$count) * 100
data_kwu$ymax = cumsum(data_kwu$fraction)
data_kwu$ymin = c(0, head(data_kwu$ymax, n=-1))

# Rounding the data to two decimal points
data_kwu <- round_df(data_kwu, 2)

# Specify what the legend should say
Type_of_Tweet_KWU <- paste(data_kwu$category, data_kwu$percentage, "%")

#Plotting a Donut Chart
ggplot(data_kwu, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet_KWU)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

## Tweet Timeline Production ##

#Linegraph of tweet frequency
colnames(kwu_tweets)[colnames(kwu_tweets)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(kwu_tweets, Twitter_Account), "week") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Kansas Wesleyan Coyotes",
    subtitle = "Tweet counts aggregated by week",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## Tweet Publish Location ##

#Chiefs Tweet Locations
kwu_app <- kwu_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
kwu_app <- subset(kwu_app, count > 11)

#Data Frame Creation
data.coyote <- data.frame(
  category=kwu_app$source,
  count=kwu_app$count
)

# Adding columns 
data.coyote$fraction = data.coyote$count / sum(data.coyote$count)
data.coyote$percentage = data.coyote$count / sum(data.coyote$count) * 100
data.coyote$ymax = cumsum(data.coyote$fraction)
data.coyote$ymin = c(0, head(data.coyote$ymax, n=-1))
data.coyote <- round_df(data.coyote, 2)
Source <- paste(data.coyote$category, data.coyote$percentage, "%")

#Donut Chart of Tweet Locations
ggplot(data.coyote, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

### Score Shots ###

## Word Frequency Analysis from all @elonmusk Tweets ##

#Cleaning of unwanted characters
kwu_tweets_organic$text <-  gsub("https\\S*", "", kwu_tweets_organic$text)
kwu_tweets_organic$text <-  gsub("@\\S*", "", kwu_tweets_organic$text) 
kwu_tweets_organic$text  <-  gsub("amp", "", kwu_tweets_organic$text) 
kwu_tweets_organic$text  <-  gsub("[\r\n]", "", kwu_tweets_organic$text)
kwu_tweets_organic$text  <-  gsub("[[:punct:]]", "", kwu_tweets_organic$text)

#Removal of stop words such as 'and' and 'is'
kwu_tweets <- kwu_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
kwu_tweets <- kwu_tweets %>%
  anti_join(stop_words)

#Bar chart of the most frequent words found in the tweets
kwu_tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Kansas Wesleyan Coyotes",
       subtitle = "Stop words removed from the list")

## Hashtag Frequency Analysis ##

#Obtaining all hashtags from @elonmusk tweets
kwu_tweets_organic$hashtags <- as.character(kwu_tweets_organic$hashtags)
kwu_tweets_organic$hashtags <- gsub("c\\(", "", kwu_tweets_organic$hashtags)

#Creating Wordcloud of Hashtag Frequencies
set.seed(1234)
wordcloud(kwu_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## Retweet Account Frequency Analysis ##

#Creating Wordcloud of Retweet Account Frequencies
set.seed(1234)
wordcloud(kwu_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

## Sentiment Analysis of Tweets ##

# Converting tweets to ASCII to trackle strange characters
kwu_tweets <- iconv(kwu_tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets
kwu_tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",kwu_tweets)

# removing mentions
kwu_tweets <-gsub("@\\w+","",kwu_tweets)
ew_sentiment<-get_nrc_sentiment((kwu_tweets))
kwu_sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(kwu_sentimentscores) <- "Score"
kwu_sentimentscores <- cbind("sentiment"=rownames(kwu_sentimentscores),kwu_sentimentscores)
rownames(kwu_sentimentscores) <- NULL

#Plotting a bar graph based on sentiment scores
ggplot(data=kwu_sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Kansas Wesleyan Coyotes total sentiment based on scores")+
  theme_minimal()

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

### @MAC_Bulldogs ### - McPherson College Bulldogs Athletic Twitter

## Tweet Mining ##

#Varible mcpherson_tweets will contain tweets from @Mac_College "McPherson College Twitter Account"
mcpherson_tweets <- get_timeline("@Mac_College", n=3200)

## Look at larger figures in the Twitter World
## Look at schools in the same class as Mcpherson (NAIA, Size, Location)

## Tweet Analysis ##

# Remove retweets
mcpherson_tweets_organic <- mcpherson_tweets[mcpherson_tweets$is_retweet==FALSE, ] 
# Remove replies
mcpherson_tweets_organic <- subset(mcpherson_tweets_organic, is.na(mcpherson_tweets_organic$reply_to_status_id)) 
mcpherson_tweets_organic

#Organic Tweet with most Favourites 
mcpherson_tweets_favorite <- mcpherson_tweets_organic %>% arrange(-favorite_count)
mcpherson_tweets_favorite[1,5]
#Organic Tweet with most Retweets 
mcpherson_tweets_retweet <- mcpherson_tweets_organic %>% arrange(-retweet_count)
mcpherson_tweets_retweet[1,5]

## RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS ## 

# Keeping only the retweets
mcpherson_retweets <- mcpherson_tweets[mcpherson_tweets$is_retweet==TRUE,]
mcpherson_retweets
# Keeping only the replies
mcpherson_replies <- subset(mcpherson_tweets, !is.na(mcpherson_tweets$reply_to_status_id))
mcpherson_replies

#Creating a data frame
mac_data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(1937, 616, 638)
)

# Adding columns 
mac_data$fraction = mac_data$count / sum(mac_data$count)
mac_data$percentage = mac_data$count / sum(mac_data$count) * 100
mac_data$ymax = cumsum(mac_data$fraction)
mac_data$ymin = c(0, head(mac_data$ymax, n=-1))

# Rounding the data to two decimal points
mac_data <- round_df(mac_data, 2)

# Specify what the legend should say
Type_of_Tweet_MAC <- paste(mac_data$category, mac_data$percentage, "%")

#Plotting a Donut Chart
ggplot(mac_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet_MAC)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

## Tweet Timeline Production ##

#Linegraph of tweet frequency
colnames(mcpherson_tweets)[colnames(mcpherson_tweets)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(mcpherson_tweets, Twitter_Account), "week") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from McPherson College",
    subtitle = "Tweet counts aggregated by week",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## We observed a 3 year timeline on @Mac_College Twitter account
## Tweeting in the year 2019 should gradual decline
## We would like to explore the comparitive school and overlay the information for a comparison 
## We would also like to compare the most associated account also under McPherson College control

## Tweet Publish Location ##

#McPherson College Tweet Locations
Mac_app <- mcpherson_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
Mac_app <- subset(Mac_app, count > 11)

#Data Frame Creation
data.bulldog <- data.frame(
  category=Mac_app$source,
  count=Mac_app$count
)

# Adding columns 
data.bulldog$fraction = data.bulldog$count / sum(data.bulldog$count)
data.bulldog$percentage = data.bulldog$count / sum(data.bulldog$count) * 100
data.bulldog$ymax = cumsum(data.bulldog$fraction)
data.bulldog$ymin = c(0, head(data.bulldog$ymax, n=-1))
data.bulldog <- round_df(data.bulldog, 2)
Source <- paste(data.bulldog$category, data.bulldog$percentage, "%")

#Donut Chart of Tweet Locations
ggplot(data.bulldog, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

## Basis of this information is that Tweets are localised to iPhone and Computer
## Due to this a lack of Twitter compostion is created as every tweet must be typed and posted in that instance
## Utilising TwitterDeck or Web App would allow for planned/timed Tweets to be sent throughout the year

## Word Frequency Analysis from all @Mac_College Tweets ##

#Cleaning of unwanted characters
mcpherson_tweets_organic$text <-  gsub("https\\S*", "", mcpherson_tweets_organic$text)
mcpherson_tweets_organic$text <-  gsub("@\\S*", "", mcpherson_tweets_organic$text) 
mcpherson_tweets_organic$text  <-  gsub("amp", "", mcpherson_tweets_organic$text) 
mcpherson_tweets_organic$text  <-  gsub("[\r\n]", "", mcpherson_tweets_organic$text)
mcpherson_tweets_organic$text  <-  gsub("[[:punct:]]", "", mcpherson_tweets_organic$text)

#Removal of stop words such as 'and' and 'is'
mcpherson_tweets <- mcpherson_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
mcpherson_tweets <- mcpherson_tweets %>%
  anti_join(stop_words)

#Bar chart of the most frequent words found in the tweets
mcpherson_tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of McPherson College",
       subtitle = "Stop words removed from the list")

## Tweets contain basic college wording with repatition of words occuring
## A diverse selection of words in Tweets may boost Tweet range

## Hashtag Frequency Analysis ##

#Obtaining all hashtags from @Mac_College tweets
mcpherson_tweets_organic$hashtags <- as.character(mcpherson_tweets_organic$hashtags)
mcpherson_tweets_organic$hashtags <- gsub("c\\(", "", mcpherson_tweets_organic$hashtags)

#Creating Wordcloud of Hashtag Frequencies
set.seed(1234)
wordcloud(mcpherson_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## We want to find another school to compare hashtag presence

## Retweet Account Frequency Analysis ##

#Creating Wordcloud of Retweet Account Frequencies
set.seed(1234)
wordcloud(mcpherson_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

## Twitter accounts affiliated with school provide largest retweet basis
## Coaches and students filter in as secondary responders to retweeting a @Mac_College Tweet

## Sentiment Analysis of Tweets ##

# Converting tweets to ASCII to trackle strange characters
mcpherson_tweets <- iconv(mcpherson_tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets
mcpherson_tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",mcpherson_tweets)

# removing mentions
mcpherson_tweets <-gsub("@\\w+","",mcpherson_tweets)
ew_sentiment<-get_nrc_sentiment((mcpherson_tweets))
mac_sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(mac_sentimentscores) <- "Score"
mac_sentimentscores <- cbind("sentiment"=rownames(mac_sentimentscores),mac_sentimentscores)
rownames(mac_sentimentscores) <- NULL

#Plotting a bar graph based on sentiment scores
ggplot(data=mac_sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

## Overall the account has a positive attitude embraced in its tweeting style
## Explain sentiment scaling
## Large comparison in sentimental positive and negative tweets 

#### Fin ####

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

