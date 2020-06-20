#Make sure to install libraries (twitteR,plyr,dplyr,tm,wordcloud,ggplot2,sentimentr,stringr,shiny,ROAuth) for this App.
library(shiny)
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(tm)
library(wordcloud)
library(ggplot2)
library(sentimentr)
library(stringr)


#Setting up connection with Twitter.  
consumer_api_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" # Enter your Consumer Key
consumer_api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxx" # Enter your Consumer Secret
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" # Enter your Access Token
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxx" # Enter your Access Secret
setup_twitter_oauth(consumer_api_key, consumer_api_secret, access_token, access_token_secret) # Setting up the Authoriztion with Twitter



# Function to create a data frame from tweets
shinyServer(function(input, output) {


  
  
#Creating different Functions for Analysis.
  
#Creating the Function for Searching latest 200 tweets related to the Searched word and creating the dataframe.    
 RawTweets.df = function(searchedWord)                                    
  {
    tweet = searchTwitter(searchedWord,n=150,lang="en")          #After removing the Retweets, final list will comprised of less than 100 tweets.
    tweet.df = do.call("rbind",lapply(tweet,as.data.frame))
    tweet.df = tweet.df[tweet.df$isRetweet == FALSE, ]           #Deleting the Retweet.
    tweet.df = data.frame(tweet.df['text']) 
    return(tweet.df)
  }
  

#Creating the Function for Cleaning the raw tweets into much more sensible sentence.
 CleaningTweets = function(tweets)
  {
    tweets = str_replace_all(tweets,"https://t([\\.])co/([a-zA-Z0-9]+)"," ")  # Getting rid of URLs.
    
    tweets = str_replace(tweets,"RT @([a-zA-Z0-9_]*): "," ")                  # Taking out retweet header.
    
    tweets = str_replace_all(tweets,"@([a-zA-Z0-9]*)"," ")                    # Getting rid of references to other usernames.
    return(tweets)
  }

 
#CODE for this Function is copied and modifying few arguments. Creating the Function for creating word cloud by wordcloud library.
 wordcloudentity = function(entitycleantext)
  {
    tweetCorpus<-Corpus(VectorSource(CleaningTweets(entitycleantext)))                        #Forming a Document Corpus
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,             #Creating TDM for selected words in Sentence as 1/0.
                                                          stopwords=c(stopwords('english')),   
                                                          removeNumbers=TRUE,tolower=TRUE,
                                                          wordLengths = c(4, Inf)))
    tdMatrix <- as.matrix(tweetTDM)                                                           #creating a data matrix from TDM
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE)                                     #Calculate row sum of each words and sorting in descending order. i.e. most repeating words comes first.
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)                        #Creating Data.Frame with names as extracted words from previous command with their frequencies.
    
    wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,min.freq=1,max.words=150,scale=c(5.5,.5), colors=rainbow(100), random.order=FALSE)  #Finally creating WordCloud by wordcloud function.        
    print(wcloudentity)
  }
  

#Creating the Function for finding Sentiment score by Jeffrey Breen's Algorithm.   
 score.sentiment = function(sentence, pos.words, neg.words)
  {
   #What we have done is taking each sentence(Individual Tweets) individually and seperating the words. 
   #And then matching those words from positive and negative word lexicon(list).   
   #And then calculating the score based on numbers of positive and negative words match in each Tweets.
   #And finally converting those into arrays by laply function.
    
     scores = laply(sentence, function(sentence, pos.words, neg.words) {          
       
       sentence = gsub('[[:punct:]]', '', sentence)                 # clean up sentences with R's regex-driven global substitute, gsub().
       sentence = gsub('[[:cntrl:]]', '', sentence)
       sentence = gsub('\\d+', '', sentence)
       sentence = gsub('\n', '', sentence)
       sentence = tolower(sentence)                                 #Converting to lower case.
        
       word.list = str_split(sentence, '\\s+')                      #Spliting the words in tweets.
       words = unlist(word.list)
       
       
       pos.matches = match(words, pos.words)                        #Comparing our words to the dictionaries of positive & negative terms.
       neg.matches = match(words, neg.words)
       
       pos.matches = !is.na(pos.matches)                            #We just want a TRUE/FALSE rather than postion.
       neg.matches = !is.na(neg.matches)
       
       score = sum(pos.matches) - sum(neg.matches)                  #Finally creating the scores by (positive match - negative match) in each tweet.
       
       return(score)
     }, pos.words, neg.words)
     
     #Creating the Data Frame consist of Scores, tweets, Serial Number(For Plotting purpose).
     scores.df = data.frame(score=scores, text=sentence, ID=seq(length(scores)))
     return(scores.df)
  }
 

#Finally Creating Function for calling Above Scoring Function, Tweets, positive and Negative Words list and finally creating the Data Frame.
 sentiment_analysis = function(sentence,searchedWord)
  {
   #Positive and Negative Words Opinion Lexicon was downloaded from this site-
   #http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html

   positivewords = readLines("positive-words.txt")              
   negativewords = readLines("negative-words.txt")
   
   #Applying score.sentiment algorithm to cleaned tweets and getting data frames of tweets, net sentiment score for a tweet. 
   
   score_1.df = score.sentiment(CleaningTweets(sentence),positivewords,negativewords)
   
   #This extra column is added for plotting purpose, so that Searched word will appear on Plots.  
   score_1.df$entity = searchedWord

   return(score_1.df)
   
  }   


#Creating Function for finding scores by alternative method by sentimentr package. This will automatically create score for each tweet by sentiment() function.  
 sentimentr_scores = function(sentences,searchedWord)
  {
   sentiment.score = sentiment(sentences)                                   #Creating Scores.
   sentiment.score = sentiment.score %>% group_by(element_id) %>% summarise(score = mean(sentiment))
   sentiment.score$text = CleaningTweets(sentences)                         #Adding tweets to data frame.
   sentiment.score = data.frame(sentiment.score[c('score','text')])         #Filtering only the Tweet and score columns.
   
   sentiment.score$score[sentiment.score$score < 0] = as.integer(-1)
   sentiment.score$score[sentiment.score$score == 0] = as.integer(0)        #As sentiment function creates score from -1 to 1.  
   sentiment.score$score[sentiment.score$score > 0] = as.integer(1)         #So substituting negative score to -1 and postive score to 1. 
   sentiment.score$ID = seq(nrow(sentiment.score))
   sentiment.score$entity = searchedWord                                    #This extra column is added for plotting purpose.
   return(sentiment.score) 
  }
 
 

 
#Time for Implemenation.
  
 
#Reading the Searched Word.
  tweet.df = reactive({                                                      #We are using reactive expression so that it will cache the input value and update only when input will change.
      tweet.df = RawTweets.df(input$searchedword)})
  
#Creating sentiment scores by !st method.
  scores_1 = reactive({                                                 
    scores_1 = sentiment_analysis(tweet.df()$text,input$searchedword)})
 
#Creating sentiment scores by 2nd method.
  scores_2 = reactive({
    scores_2 = sentimentr_scores(tweet.df()$text,input$searchedword)})
  
  
  
  
#Finally creating the Outputs.
  
#Creating the boxplot Output for 1st method.   
  output$word1 = renderText({input$searchedword})
  output$scores_1_boxplot = renderPlot({
    scores_1_boxplot = ggplot(scores_1(),aes(x=ID,y=score))+
                  facet_grid(entity ~ .)+
                  geom_point(color = "black",size = 1.5, alpha = 0.4)+
                  geom_smooth(method = "loess",se=FALSE,col='red',size=1.1)+
                  geom_hline(yintercept=0)+
                  xlab('Tweet number')+
                  ylab('Sentiment Score')
    print(scores_1_boxplot)})
  
#Creating the boxplot Output for 2nd method. 
  output$word2 = renderText({input$searchedword})
  output$scores_2_boxplot = renderPlot({
    scores_2_boxplot = ggplot(scores_2(),aes(x=ID,y=score))+
                  facet_grid(entity ~ .)+
                  geom_point(color = "black",size = 1.5, alpha = 0.4)+
                  geom_smooth(method = "loess",se=FALSE,col='red',size=1.1)+
                  geom_hline(yintercept=0)+
                  xlab('Tweet number')+
                  ylab('Sentiment Score')
    print(scores_2_boxplot)})
  
#Glimpse of how the data.frame of final scores by both the method looks like.  
  output$scores_1_head = renderTable({top5 = head(scores_1(),5)})
  output$scores_2_tail = renderTable({bottom5 = tail(scores_2(),5)})

  
#Bar plot for both the methods.
  output$word3 = renderText({input$searchedword})
  output$bar_1 = renderPlot({
          bar_1 = ggplot(scores_1(),aes(x=score))+
                   geom_bar(colour = 'red',fill = 'grey')
          print(bar_1)})
  
  output$bar_2 = renderPlot({
          bar_2 = ggplot(scores_2(),aes(x=score))+
                   geom_bar(colour = 'red',fill = 'grey')
          print(bar_2)})
  
#Pie Chart for both the methods.
  output$word4 = renderText({input$searchedword})
  output$pie_1 = renderPlot({
         x = c((nrow(scores_1() %>% filter(score<0))) , (nrow(scores_1() %>% filter(score>0))))
           pie(x,labels = c("Negative","Postive"),main = "Pie Chart of Sentiment by 1st method.", col = blues9)})  #Colours is using differet shades of blue.
  
  output$pie_2 = renderPlot({
         x = c((nrow(scores_2() %>% filter(score<0))) , (nrow(scores_2() %>% filter(score>0))))
           pie(x,labels = c("Negative","Postive"),main = "Pie Chart of Sentiment by 2nd method.", col = blues9)})
  
   
#Creating the WordCloud for tweets word. 
  output$word5 = renderText({input$searchedword})
  output$wc_plot = renderPlot({
    wordcloudentity(tweet.df()$text)})
  
  
#Raw Tweets.
  output$word6 = renderText({input$searchedword})
  output$rawtweet_table =  renderTable({table = tweet.df()[1]})
  
})