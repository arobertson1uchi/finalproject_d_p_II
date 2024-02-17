install.packages("tidyverse")
install.packages("rvest")
library(tidyverse)
library(rvest)

###On main page
url <- "https://info.harris.uchicago.edu/harris-by-the-numbers"
response <- read_html(url)

link_url <- response %>%
  html_elements("a") %>%
  html_attr("href")

link_text <- response %>%
  html_elements("a") %>%
  html_text()

link_table <- data.frame(Link_Text = link_text, Link_URL = link_url)

print(head(link_table))


