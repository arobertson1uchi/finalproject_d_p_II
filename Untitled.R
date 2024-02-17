install.packages("rvest")
library(rvest)
library(dplyr)

url <- "https://info.harris.uchicago.edu/harris-by-the-numbers"
page <- read_html(url)
data_nodes <- html_nodes(page, "")
data_text <- html_text(data_nodes)

print(data_text)

data <- read.csv("/Users/aaliyahrobertson/Desktop/data/Most-Recent-Cohorts-Field-of-Study.csv")

Project <- data %>% filter(INSTNM == "University of Chicago", CREDDESC == "Master's Degree")

Project_filtered <- Project %>%
  select(where(~ !any(grepl("PrivacySuppressed", .x))))


