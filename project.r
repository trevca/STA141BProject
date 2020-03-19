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

sv <- chrome(port = 4564L, verbose = FALSE, version = "80.0.3987.106")
rd <- remoteDriver(browser = "chrome", port = 4564L)
rd$open(silent = TRUE)

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
  textInput(inputId = "redditPage","Enter SubReddit Title"),
  actionButton("start", "Go!"),
  sliderInput("min",
              "Minimum :",
              min = 0,  max = 1500, value = 100),
  sliderInput("max",
              "Maximum :",
              min = 100,  max = 1600,  value = 100),
  plotOutput('wordcloud'),
  plotOutput('histfreq'),
  tableOutput("table")
)

server <- function(input, output) {
  
  v = reactiveValues(data = NULL, text = NULL)
  
  words = reactive({
    dat = v$data
    dat$Content = str_replace_all(dat$Content,' No Text','notext')
    dat2 = dat %>% filter(Upvotes >= input$min) %>% filter(Upvotes <= input$max)
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

  observeEvent(input$start, {
    v$text = "Loading..."
    page = input$redditPage
    table = data.frame(1:10, 1:10)
    url = paste("https://www.reddit.com/r/", page, sep = "")
    rd$navigate(url)
    n <- 15
    for (i in 1:n) {
      rd$executeScript("window.scrollTo(0,document.body.scrollHeight)")
      Sys.sleep(2)
    }
    html = rd$getPageSource() %>% 
      str_flatten() %>%
      read_html()
    fileConn<-file("output.txt")
    texttext = html_text(html)
    writeLines(texttext,fileConn)
    close(fileConn)
    all = html %>% html_nodes("div.Post")
    
    titles = c()
    upvotes = c()
    content = c()
    pic = c()
    promoted = c()
    
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
    lengths = c(length(titles), length(upvotes), length(content), length(pic))
    table = data.frame(Title = titles, Upvotes = upvotes, Content = content, Pics = pic, Promoted = promoted)
    table$Title = as.character(table$Title)
    table$Content = as.character(table$Content)
    write.csv(table, "~/newTable.csv")
    v$data = table
    v$text = NULL
  })
  
  output$wordcloud = renderPlot({
    wordcloud(words = words()$word, freq = words()$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$histfreq = renderPlot({
    barplot(words()$freq[1:10], las = 2, names.arg = words()$word[1:10],
            col ="lightblue", main ="Most frequent words",
            ylab = "Word frequencies")
  })
  
  output$table = renderTable({
    if(is.null(v$data)) return()
    v$data
  })
  
}

shinyApp(ui = ui, server = server)
