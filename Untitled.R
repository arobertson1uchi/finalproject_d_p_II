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




