# url <- "https://ucdavis.campuslabs.com/engage/organization/regents-scholars-society"
# html <- paste(readLines(url), collapse="\n")
# library(stringr)
# matched <- str_match_all(html, "mailto")

library(rvest)
library(RSelenium)
library(wdman)
library(tidyverse)

server <- chrome(port = 4560L, verbose = FALSE, version = "80.0.3987.106")
rd <- remoteDriver(browser = "chrome", port = 4560L)
rd$open(silent = TRUE)
rd$navigate("https://www.reddit.com/")

page = "Swimming"

url = paste("https://www.reddit.com/r/", page, sep = "")
rd$navigate(url)
n <- 30
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

table = data.frame(Title = titles, Upvotes = upvotes, Content = content, Pics = pic)
table

#content: div STit0dLageRsa2yR4te_b -> all p, might have an img
# h3 <- title
# div class _1rZYMD_4xY3gRcSS3p8ODO <- rating

