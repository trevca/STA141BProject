# Shiny application
# this is trevor's branch

library(shiny)
library(rvest)
library(RSelenium)
library(wdman)
library(tidyverse)

sv <- chrome(port = 4565L, verbose = FALSE, version = "80.0.3987.106")
rd <- remoteDriver(browser = "chrome", port = 4565L)
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
  tableOutput("table"),
  textOutput("text")
)

server <- function(input, output) {
  
  v = reactiveValues(data = NULL, text = NULL)

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
    write.csv(table, "~/newTable.csv")
    v$data = table
    v$text = NULL
  })
  
  output$table = renderTable({
    if(is.null(v$data)) return()
    v$data
  })
  
  output$text = renderText({
    if(!is.null(v$data)) return()
    ""
  })
  
}

shinyApp(ui = ui, server = server)
