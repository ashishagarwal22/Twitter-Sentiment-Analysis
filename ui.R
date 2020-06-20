shinyUI(fluidPage(
    
    titlePanel("Sentiment Analysis On Tweets using R."),
    
    #Getting User Inputs
    sidebarLayout(
    
        sidebarPanel(
        
           wellPanel(                                                  #Using wellpanel just for nicer UI.
               helpText("Enter a particular word or Hastag on which you want to perform a Sentiment Analysis."),
               textInput("searchedword", "Search Word/Hashtag ","#Example"),
               helpText("Please wait for few seconds after entering new words, as it will take some time to extract the tweets."),
               helpText("Note: 2 Different Algorithms/Methods are used to find the Sentiment/Score for Tweets on which data are shown.")
           )
        
        ),
    
    mainPanel(
      #Using the tabsetPanel for Nicer UI to show differnt things under diffferent Tabs.    
        tabsetPanel(
            
            #Tab 1 - Showing the Boxplot from 1st method scores, and glimpse of few head and tail data of final Data frame created.
            tabPanel("SENTIMENTAL ANALYSIS 1",br(),h2(textOutput("word1")),br(),HTML("<div><strong>Sentiment Analysis by Algorithm 1. </strong></div>"),br(), plotOutput("scores_1_boxplot"),br(), HTML
                     ("<div> This plot shows the distribution of positive/negative sentiments about each entity. Note that tweets were cleaned before this analysis was performed. For each tweet, score
                         was computed and this plot shows the distribution of scores.A higher sentiment score suggests more positive (or a less negative) discussion of that entity than the other.</div>"),
                     br(),br(),HTML("<div><strong>Glimpse of first few Sentiment data Analysed by 1st method. </strong></div>"),br(),tableOutput("scores_1_head"),id="test"),
            
            #Tab 2 - Showing the Boxplot from 2nd method scores.
            tabPanel("SENTIMENTAL ANALYSIS 2",br(),h2(textOutput("word2")),br(),HTML("<div><strong>Sentiment Analysis by Algorithm 2. </strong></div>"),br(), plotOutput("scores_2_boxplot"),br(), HTML
                     ("<div> This plot shows the distribution of positive/negative sentiments about each entity. Note that tweets were cleaned before this analysis was performed. For each tweet, score was computed and this plot shows the distribution of scores. 1 means Positive, -1 means negative and 0 means neutral.</div>"),
                     br(),br(),HTML("<div><strong>Glimpse of last few Sentiment data Analysed by 2nd method. </strong></div>"),br(),tableOutput("scores_2_tail")),
            
            #Tab 3 - Showing the Bar Graph by both the methods.
            tabPanel("BAR GRAPH",br(),h2(textOutput("word3")),br(),HTML("<div><strong>Bar Plot of score by 1st method.</strong></div>"),br(),plotOutput("bar_1"),br(),br(),HTML("<div><strong>Bar plot of score
                         by 2nd method.</strong></div>"),br(),plotOutput("bar_2")),
            
            
            #Tab 4 - Showing the Pie Charts by both the nethods.
            tabPanel("PIE CHART",br(),h2(textOutput("word4")),br(),HTML("<div><h5>Note : Scores of '0' is excluded while Plotting the Pie Chart. Positive Integer are Postive Sentiments. 
                                                                        Negative Integer are Negative Sentiments.</h5></div>"),plotOutput("pie_1"),plotOutput("pie_2")),
            
            #Tab 5 - It Will show the Wordcloud.
            tabPanel("WORD CLOUD",br(),h2(textOutput("word5")),h2(textOutput("wc")),plotOutput("wc_plot")),
            
            
            #Tab 6 - Showing the Raw Tweets extracted.
            tabPanel("RAW TWEETS",br(),h2(textOutput("word6")),tableOutput("rawtweet_table"))
        )
      )
   )  
))