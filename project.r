# Shiny application
# this is trevor's branch

library(shiny)
library(rvest)
library(RSelenium)
library(wdman)
library(tidyverse)

server <- chrome(port = 4560L, verbose = FALSE, version = "80.0.3987.106")
rd <- remoteDriver(browser = "chrome", port = 4560L)
rd$open(silent = TRUE)
rd$navigate("https://www.reddit.com/")

ui <- fluidPage(
  textInput(inputId = "redditPage","Enter SubReddit then press enter"),
  plotOutput("pieChart")
)

server <- function(input, output) {
  html = ""
  output$pieChart = renderPlot({
    
    page = input$redditPage
    if(page != ""){
      print(page)
      url = paste("https://www.reddit.com/r/", page, sep = "")
      rd$navigate(url)
      n <- 20
      for (i in 1:n) {
        rd$executeScript("window.scrollTo(0,document.body.scrollHeight)")
        Sys.sleep(1)
      }
      html = rd$getPageSource() %>% 
        str_flatten() %>%
        read_html()
      
      all = html %>% html_nodes("div.Post")
      
      titles = c()
      upvotes = c()
      content = c()
      pic = c()
      
      for(node in all) {
        t = node %>% 
          html_nodes("h3._eYtD2XCVieq6emjKBH3m") %>% 
          html_text()
        
        u = node %>% 
          html_nodes("div._1rZYMD_4xY3gRcSS3p8ODO") %>%
          html_text()
        
        c = node %>% 
          html_nodes("div.STit0dLageRsa2yR4te_b")
        
        contentTexts = c %>% html_nodes("p") %>% html_text()
        contentPic = c %>% html_nodes("img") %>% html_text()
        
        if(u[1] != '•') {
          upvotes = append(upvotes, u[1])
          
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
        
      }
      print(titles)
      print(upvotes)
      print(content)
      print(pics)
      table = data.frame(Title = titles, Upvotes = upvotes, Content = content, Pics = pic)
      print(table)
      }
      slices <- c(10*runif(1), 10*runif(1), 10*runif(1), 10*runif(1), 10*runif(1))
      lbls <- c("US", "UK", "Australia", "Germany", "France")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=rainbow(length(lbls)),
          main="Pie Chart of Countries")
    })
  
}

shinyApp(ui = ui, server = server)
