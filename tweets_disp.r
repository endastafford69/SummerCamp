library(shiny)


## load rtweet package
library(rtweet)
library(stringr)

app= "esd1"
api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""



token = create_token(app, api_key,api_secret,access_token,access_token_secret)

# define UI for application, sets an UI object

shinyUI <- fluidPage(

  titlePanel("Tweets Sentiments"),
  sliderInput(inputId = "num",
              label = "Choose a number of tweets to fetch",
              min = 1, max = 500, value = 200),
  sliderInput(inputId = "disp",
              label = "Choose the minimum number of tweets to display",
              min = 1, max = 50, value = 3),
  radioButtons("htag", choices= c("@LFC", "@MUFC"), label = "@LFC"),
  tableOutput("tweets"),
  textOutput("result"),
  imageOutput("img1")
)

#renderDataTable() An interactive table
#renderImage() An image (saved as a link to a source file)
#renderPlot() A plot
#renderPrint() A code block of printed output
#renderTable() A table
#renderText() A character string
#renderUI() a Shiny UI element

# Define server logic required to draw a chart, sets a server object
shinyServer <- function(input, output) {

  output$tweets <- renderTable({
    lfc <- search_tweets(
      input$htag, n = input$num, include_rts = FALSE
    )
    #lfc <- search_tweets(
    #  "@LFC", n = input$num, include_rts = FALSE
    #)
    lfc_df = data.frame(lfc)
    head(lfc_df$text,input$disp)})
  output$result <-renderText({
    lfc <- search_tweets(
      input$htag, n = input$num, include_rts = FALSE
    )
    lfc_df = lfc$text
    paste("The lfc_df size is:", nrow(lfc_df))

    lfc.score = score.sentiment(lfc_df, pos.words, neg.words)
    lfc.score$very.pos = as.numeric( lfc.score$score >= 2)
    lfc.score$very.neg = as.numeric( lfc.score$score <= -2)

    #now we construct the twitter data frame and simultaneously compute the pos/neg sentiment scores for each #TAG
    
    lfc.pos = aggregate(lfc.score$score, by=list(Sent=lfc.score$very.pos), FUN=sum)
    lfc.neg = aggregate(lfc.score$score, by=list(Sent=lfc.score$very.neg), FUN=sum)
    
    #and here the general sentiment 
    lfc.tot = lfc.pos[2,2] + lfc.neg[2,2]
    paste("The sentiment result for ", input$htag, "is:", lfc.tot)
  })
 
  }

#our first function
score.sentence <- function(sentence, pos.words, neg.words) {
  #here some cleaning
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl::]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  sentence = tolower(sentence)
  
  #basic data structure construction
  word.list = str_split(sentence, '\\s+')
  words = unlist(word.list)
  
  #here we count the number of words that are positive and negative
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  
  #throw away those that didn't match
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  #compute the sentiment score for each sentence/tweet
  score = sum(pos.matches) - sum(neg.matches)
  
  return(score)
}

#our second function that takes an array of sentences and sentiment analyses them
score.sentiment <- function(sentences, pos.words, neg.words) {
  require(plyr)
  require(stringr)
  
  #here any sentence/tweet that causes an error is given a sentiment score of 0 (neutral)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    tryCatch(score.sentence(sentence, pos.words, neg.words ), error=function(e) 0)
  }, pos.words, neg.words)
  
  #now we construct a data frame
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
}

#our first function
score.comic <- function(sentence, pos.words) {
  #here some cleaning
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl::]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  sentence = tolower(sentence)
  
  #basic data structure construction
  word.list = str_split(sentence, '\\s+')
  words = unlist(word.list)
  
  #here we count the number of words that are positive and negative
  pos.matches = match(words, pos.words)
  
  #throw away those that didn't match
  pos.matches = !is.na(pos.matches)
  
  #compute the sentiment score for each sentence/tweet
  score = sum(pos.matches)
  
  return(score)
}

pos.words = scan('pos_words.txt', what = 'character', comment.char='')
neg.words = scan('neg_words.txt', what = 'character', comment.char='')

# combines the two objects defined above into a shiny app
shinyApp(ui = shinyUI, server = shinyServer)

