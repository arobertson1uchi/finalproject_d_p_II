install.packages("rvest")
library(rvest)
library(dplyr)
library(ggplot2)
install.packages("yr")
library(yr)

data <- read.csv("/Users/aaliyahrobertson/Desktop/data/Most-Recent-Cohorts-Field-of-Study.csv")

Project <- data %>% filter(INSTNM == "University of Chicago", CREDDESC == "Master's Degree")

Project_filtered <- Project %>% select(where(~ !any(grepl("PrivacySuppressed", .x))))

library(dplyr)


Project_filtered_continued <- Project_filtered %>%
  select(-UNITID, -OPEID6, -MAIN, -CREDLEV, -DISTANCE)


head(Project_filtered_continued)

Project_filtered_continued <- Project_filtered_continued %>%
  mutate(IPEDSCOUNT1 = as.numeric(na_if(IPEDSCOUNT1, "NULL")),
         IPEDSCOUNT2 = as.numeric(na_if(IPEDSCOUNT2, "NULL")))

Project_summary <- Project_filtered_continued %>%
  group_by(CIPDESC) %>%
  summarize(AverageCount1 = mean(IPEDSCOUNT1, na.rm = TRUE),
            AverageCount2 = mean(IPEDSCOUNT2, na.rm = TRUE))



Project_summary_long <- Project_summary %>%
  pivot_longer(
    cols = starts_with("AverageCount"),
    names_to = "Metric",
    values_to = "Value"
  )

library(ggplot2)





