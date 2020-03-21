# Shiny application
# this is trevor's branch

library(shiny)
library(rvest)
library(RSelenium)
library(wdman)
library(tidyverse)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(plotly)
library(textcat)
library(textdata)
library(tidytext)

sv <- phantomjs(port = 4565L, verbose = FALSE)
rd <- remoteDriver(browser = "phantomjs", port = 4565L)
rd$open(silent = TRUE)

# Helper function to convert the upvote string to a usable number
convertUpvotes = function(u) {
  numUp = 0
  if(length(u) > 0 && u[1] != "•") {
    newU = u[1]
    newU = str_replace(newU, "k", " 1000")
    newU = str_replace(newU, "K", " 1000")
    newU = str_replace(newU, "m", " 1000000")
    newU = str_replace(newU, "M", " 1000000")
    numbers = unlist(str_split(newU, " "))
    numUp = 1
    for(num in numbers) {
      numUp = numUp* as.numeric(num)
    }
  }
  return(as.integer(numUp))
}

ui <- fluidPage(
  titlePanel("RedditScraper"),
  fluidRow(
      column(3, textInput(inputId = "redditPage","Enter SubReddit Title")),
      column(2, textInput(inputId = "seconds","Enter Wait Time")),
      column(1, actionButton("start", "Go!"))
    ),
      tabsetPanel(
        tabPanel("Title", 
                 h3(" "),
                 plotlyOutput("titleLength", height = "400px", width = "600px")
        ), 
        tabPanel("Content", 
                 fluidRow(
                   column(6,
                          h3("Set Upvotes Range:"),
                          sliderInput("min",
                                         "Minimum :",
                                         min = 0,  max = 1499, value = 0),
                          sliderInput("max",
                                      "Maximum :",
                                      min = 1,  max = 1500,  value = 1000)),
                   column(6, 
                          plotOutput('wordcloud'))
                 ),
                 plotOutput('histfreq')
                 ), 
        tabPanel("Sentiments", 
                 h3("Upvotes by Sentiment"),
                 h6("Outliers have been removed"),
                 plotOutput('boxplots'),
                 selectInput("sentiment",
                                      "Title Sentiment:",
                                      c("All", "positive", "negative", "neutral")),
                 dataTableOutput("sentimentTable")
                 ), 
        tabPanel("Picture", 
                 h3(" "),
                 plotlyOutput("pictureEffect", height = "400px", width = "600px")
                 ),
        tabPanel("Ads", 
                 h3(" "),
                 plotlyOutput("promoEffect", height = "400px", width = "600px")
        )
      )
  
)

server <- function(input, output) {
  
  # some reactive values to be used by the majority of functions
  v = reactiveValues(data = NULL, sentiment = NULL)
  
  words = reactive({
    dat = v$data
    if(is.null(dat)) {
      return()
    }
    dat$Content = str_replace_all(dat$Content,' No Text','notext')
    min = 0
    max = 1000
    if(!is.null(input$min)){
      min = input$min
    }
    if(!is.null(input$max)){
      max = input$max
    }
    dat2 = dat %>% filter(Upvotes >= min) %>% filter(Upvotes <= max)
    textm = dat2$Content
    docs <- Corpus(VectorSource(textm))
    #docs
    #inspect(docs)
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove English common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("I'm")) 
    # Remove the punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    data.frame(word = names(v),freq=v) %>% filter(word != "notext")
  })

  # get the data and put it into v$data and v$sentiment
  observeEvent(input$start, {
    v$data = NULL
    v$sentiment = NULL
    
    page = input$redditPage
    table = data.frame(1:10, 1:10)
    url = paste("https://www.reddit.com/r/", page, sep = "")
    #navigate to the reddit
    rd$navigate(url)
    n <- input$seconds
    if(is.na(n)) {
      n = 1
    }
    else if(is.na(as.numeric(n))) {
      n = 1
    }
    else {
      n = as.integer(as.numeric(n)/2)
    }
    
    # scroll down to get the amount of data, 
    # proprotional to the input number of seconds specified by user
    for (i in 1:n) {
      element = rd$findElement("class name", "FohHGMokxXLkon1aacMoi")
      element$getElementLocationInView()
      Sys.sleep(2)
    }
    
    html = rd$getPageSource() %>%
      str_flatten() %>%
      read_html()
    
    
    # seperate into posts
    all = html %>% html_nodes("div.Post")
    
    titles = c()
    upvotes = c()
    content = c()
    pic = c()
    promoted = c()
    
    # break each post into individaul pieces
    for(node in all) {
      t = node %>% 
        html_nodes("h3._eYtD2XCVieq6emjKBH3m") %>% 
        html_text()
      
      u = node %>% 
        html_nodes("div._1rZYMD_4xY3gRcSS3p8ODO") %>%
        html_text()
      
      c = node %>% 
        html_nodes("div.STit0dLageRsa2yR4te_b")
      
      prom = node %>% html_nodes("span._2oEYZXchPfHwcf9mTMGMg8") %>% html_text()
      
      contentTexts = c %>% html_nodes("p") %>% html_text()
      contentPic = c %>% html_nodes("img") %>% html_text()
      
      upvotes = append(upvotes, convertUpvotes(u))
      
      promoted = append(promoted, length(prom) > 0)
      pic = append(pic, length(contentPic) > 0)
      if(length(contentTexts) == 0) {
        contentTexts = "No Text"
      }
      cT = ""
      for(cont in contentTexts) {
        cT = paste(cT, cont, sep = " ")
      }
      content = append(content, cT)
      titles = append(titles, t[1])
    }
    
    # create data table
    table = data.frame(X = 1:length(titles), Title = titles, Upvotes = upvotes, Content = content, Pics = pic, Promoted = promoted)
    
    table$Title = as.character(table$Title)
    table$Content = as.character(table$Content)
    
    v$data = table
    # create sentiment table
    v$sentiment = table %>%
      select(X, Title, Upvotes, Content)%>%
      unnest_tokens(word,Title)%>%
      anti_join(stop_words)%>%
      left_join(get_sentiments("bing"))%>%
      group_by(X) %>%
      summarize(
        positive = sum(sentiment == "positive", na.rm = TRUE),
        negative = sum(sentiment == "negative", na.rm = TRUE),
        neutral = n() - positive - negative) %>%
      mutate(
        X,
        positive = positive, negative = negative, neutral = neutral, 
        sentiment = case_when(
          positive > negative ~ "positive",
          positive < negative ~ "negative",
          TRUE ~ "netural"
        )) %>%
      left_join(select(table, X, Title, Upvotes, Content)) %>% 
      select(X, positive, negative, neutral, sentiment, Upvotes, Title, Content)
    v$sentiment$sentiment = factor(v$sentiment$sentiment)
  })
  
  # create word cloud
  output$wordcloud = renderPlot({
    if(is.null(v$data)) return()
    wordcloud(words = words()$word, freq = words()$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  # create histogram for word frequency
  output$histfreq = renderPlot({
    if(is.null(v$data)) return()
    barplot(words()$freq[1:10], las = 2, names.arg = words()$word[1:10],
            col ="lightblue", main ="Most frequent words",
            ylab = "Word frequencies")
  })
  
  # create sentiment boxplots
  output$boxplots = renderPlot({
    if(is.null(v$sentiment)) return()
    outliers <- boxplot(Upvotes ~ sentiment, data = v$sentiment, plot=FALSE)$out
    dataNoOutliers = v$sentiment[-which(v$sentiment$Upvotes %in% outliers),]
    boxplot(Upvotes ~ sentiment, data = dataNoOutliers)
  })
  
  # create title length scatter plot
  output$titleLength = renderPlotly({
    if(is.null(v$data)) return()
    data = v$data %>%
      mutate(Title_length = sapply(strsplit(Title, " "), length))
    data %>%
      group_by(Title_length) %>%
      summarize(Average_Upvotes = mean(Upvotes)) %>%
      plot_ly(y = ~ Average_Upvotes, x = ~ Title_length) %>%
      layout(title = "Effect of Title Length on Upvotes", yaxis = list(title = 'Average Upvotes'), xaxis = list(title = 'Title Length (Words)'))  
    
  })
  
  # create picture effect bar chart
  output$pictureEffect = renderPlotly({
    if(is.null(v$data)) return()
    v$data %>%
      group_by(Pics) %>%
      summarize(Average_Upvotes = mean(Upvotes)) %>%
      plot_ly(y = ~ Average_Upvotes, x = ~ factor(Pics)) %>%
      add_bars(color = ~ Pics) %>%
      layout(title = "Effect of Picture on Upvotes", yaxis = list(title = 'Average Upvotes'), xaxis = list(title = 'Post Contains a Picture (True/False)'))  
  })
  
  # create ad effect bar chart
  output$promoEffect = renderPlotly({
    if(is.null(v$data)) return()
    v$data %>%
      group_by(Promoted) %>%
      summarize(Average_Upvotes = mean(Upvotes)) %>%
      plot_ly(y = ~ Average_Upvotes, x = ~ factor(Promoted)) %>%
      add_bars(color = ~ Promoted) %>%
      layout(title = "Effect of Ads on Upvotes", yaxis = list(title = 'Average Upvotes'), xaxis = list(title = 'Post is an Ad (True/False)'))  
  })
  
  # create sentiment data table
  output$sentimentTable <- renderDataTable({
    if(is.null(v$sentiment)) return()
    data = v$sentiment %>% select(sentiment, Upvotes, Title, Content)
    if (input$sentiment != "All") {
      data <- data[data$sentiment == input$sentiment,]
    }
    data
  })
  
}

# make sure to close server and window on close of application
onStop(function() {
  rd$close()
  sv$stop()
})
shinyApp(ui = ui, server = server)
