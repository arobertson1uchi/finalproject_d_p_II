install.packages("tidyverse")
install.packages("rvest")
library(tidyverse)
library(rvest)

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

library(ggplot2)
library(dplyr)

filtered_links <- link_table %>%
  filter(nchar(Link_Text) > 0) %>%
  filter(!grepl("Log in|Search|Directory", Link_Text))

grouped_links <- filtered_links %>%
  mutate(Category = case_when(
    grepl("Admissions|Prospective Students|Financial Aid", Link_Text) ~ "Admissions",
    grepl("Academics|Courses|Faculty Directory", Link_Text) ~ "Academics",
    grepl("Student Life|Student Organizations|Career Development", Link_Text) ~ "Student Life",
    grepl("News & Events|Events", Link_Text) ~ "Events",
    TRUE ~ "Other"
  ))

category_freq <- grouped_links %>%
  group_by(Category) %>%
  summarise(Frequency = n())

grouped_links$Category <- factor(grouped_links$Category, levels = category_freq$Category[order(category_freq$Frequency)])

my_colors <- c("Admissions" = "#4E79A7", "Academics" = "#F28E2B", "Student Life" = "#E15759", "Events" = "#76B7B2", "Other" = "#59A14F")

ggplot(grouped_links, aes(x = Category, fill = Category)) +
  geom_bar(width = 0.6) +
  labs(title = "Simplified Link Categories", x = "Category", y = "Frequency") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels
        axis.title = element_text(face = "bold"),  # Bold axis titles
        legend.position = "none") +  # Remove legend
  scale_fill_manual(values = my_colors) +  # Use custom color palette
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5)  # Add frequency labels

# Assuming httr is installed and loaded
library(httr)

# Define the URL for the robots.txt file
robots_url <- "https://grad.uchicago.edu/robots.txt"

# Fetch the content of the robots.txt file
response <- GET(robots_url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Print the content of robots.txt
  cat(content(response, "text", encoding = "UTF-8"))
} else {
  cat("Failed to retrieve robots.txt: HTTP status", status_code(response))
}



library(httr)
library(rvest)

# The URL you want to scrape


library(readr)
MERGED2021_22_PP <- read_csv("data/MERGED2021_22_PP.csv")
MERGED2021_22_PP <- read_csv("data/MERGED2021_22_PP.csv")

uchicago_df <- MERGED2021_22_PP %>% 
  filter(CITY == "Chicago") %>%
  select(INSTNM, CITY, STABBR, ZIP, SCH_DEG, PREDDEG, HIGHDEG, ADM_RATE, ADM_RATE_ALL,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         SATVRMID, SATMTMID, SATWRMID,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75)

  
numeric_columns <- c("SATVR25", "SATVR75", "SATMT25", "SATMT75", "SATWR25", "SATWR75",
                     "SATVRMID", "SATMTMID", "SATWRMID", "ACTCM25", "ACTCM75", "ACTEN25", "ACTEN75")

uchicago_df[numeric_columns] <- lapply(uchicago_df[numeric_columns], function(x) as.numeric(as.character(x)))

uchicago_df$SATVR25[is.na(uchicago_df$SATVR25)] <- mean(uchicago_df$SATVR25, na.rm = TRUE)
uchicago_df$SATVR25[is.na(uchicago_df$SATVR25)] <- median(uchicago_df$SATVR25, na.rm = TRUE)

cleaned_df <- uchicago_df[!is.na(uchicago_df$SATVR25) & !is.na(uchicago_df$SATMT25), ]

  
# Summary statistics for SAT Verbal (Reading) and Math scores
summary(uchicago_df$SATVRMID)  # Middle 50% SAT Verbal scores
summary(uchicago_df$SATMTMID)  # Middle 50% SAT Math scores

# Summary statistics for ACT Composite scores
summary(uchicago_df$ACTCM25)  # 25th percentile ACT Composite scores
summary(uchicago_df$ACTCM75)  # 75th percentile ACT Composite scores

library(ggplot2)

# Combine SAT Verbal and Math scores for a composite view
ggplot(cleaned_df, aes(x = INSTNM, y = SATMTMID)) +
  geom_boxplot(aes(fill = INSTNM)) +
  labs(title = "SAT Math Midpoint Scores by Institution", x = "Institution", y = "SAT Math Midpoint Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Repeat for SATVRMID if desired

# Using the mean of the 25th and 75th percentile scores as a proxy for midpoint
uchicago_df$ACTCM_MID <- with(uchicago_df, (ACTCM25 + ACTCM75) / 2)

ggplot(uchicago_df, aes(x = INSTNM, y = ACTCM_MID, fill = INSTNM)) +
  geom_bar(stat = "identity") +
  labs(title = "ACT Composite Score Midpoints by Institution", x = "Institution", y = "ACT Composite Score Midpoint") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





















# Assuming master's degree or higher is what we're interested in; HIGHDEG = 3 might indicate master's as the highest degree
  filter(HIGHDEG >= 3) %>%
  # Select relevant columns including SAT and ACT score ranges
  select(INSTNM, CITY, STABBR, ZIP, SCH_DEG, PREDDEG, HIGHDEG, ADM_RATE, ADM_RATE_ALL,
         SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         SATVRMID, SATMTMID, SATWRMID,
         ACTCM25, ACTCM75, ACTEN25, ACTEN75)

# View the filtered DataFrame
print(uchicago_df)


# Filter dataset for schools in Chicago
chicago_schools <- df %>%
  filter(CITY == "Chicago") %>%
  select(INSTNM, SATVR25, SATVR75, SATMT25, SATMT75, ACTCM25, ACTCM75)

# Proceed with analysis and visualization






















