library(readr)
library(shiny)
library(ggplot2)
library(ggplot2)
library(shiny)
library(ggplot2)
library(shiny)
library(dplyr)
library(plotly)

Cohorts_Field_of_Study <- read_csv("~/Desktop/data/Most-Recent-Cohorts-Field-of-Study.csv")
Cohorts_Institution <- read_csv("~/Desktop/data/Most-Recent-Cohorts-Institution.csv")

filtered_fields_of_study <- Cohorts_Field_of_Study %>%
  select(UNITID, INSTNM, CIPCODE, CIPDESC, CREDDESC) %>%
  filter(grepl("Chicago", INSTNM)) %>%
  filter(grepl("Master", CREDDESC, ignore.case = TRUE))

filtered_institutions <- Cohorts_Institution %>%
  select(
    UNITID, INSTNM, CITY, STABBR, SCH_DEG, CONTROL,
    ADM_RATE, SATVR25, SATVR75, SATMT25, SATMT75, SATWR25,
    SATWR75, SATVRMID, SATMTMID, SATWRMID, ACTCM25, ACTCM75,
    ACTEN25, ACTEN75
  ) %>%
  filter(grepl("Chicago", CITY)) %>%
  filter(grepl("3", SCH_DEG, ignore.case = TRUE))

filtered_institutions <- filtered_institutions %>%
  mutate(UNITID = as.character(UNITID))

merged_data <- left_join(filtered_fields_of_study, filtered_institutions, by = "UNITID")

View(merged_data)

#Interactive Plot Looking at Admission Rates by Institution 

ui <- fluidPage(
  titlePanel("Admission Rates of Institutions in Chicago"),
  fluidRow(
    column(width = 6,
           selectInput("variable", "Select Variable", 
                       choices = c("ADM_RATE" = "ADM_RATE", "SATVR25" = "SATVR25"),
                       selected = "ADM_RATE")),
    column(width = 6,
           uiOutput("second_input"))
  ),
  fluidRow(
    plotlyOutput("admission_plot")
  )
)

server <- function(input, output) {
  
  observe({
    if (sum(!is.na(merged_data$ADM_RATE)) == 0) {
      shiny::showNotification("No data points to plot!", duration = 5, type = "warning")
    }
  })
  
  output$second_input <- renderUI({
    selectInput("variable_range", "Select Range",
                choices = unique(merged_data[[input$variable]]),
                selected = unique(merged_data[[input$variable]])[1])
  })
  
  filtered_data <- reactive({
    merged_data %>%
      filter(!is.na(ADM_RATE)) %>%
      filter(ADM_RATE != "NULL")
  })
  
  output$admission_plot <- renderPlotly({
    plot_ly(data = merged_data, x = ~INSTNM.y, y = ~ADM_RATE, text = ~paste("Institution: ", INSTNM.x, "<br>Admission Rate: ", ADM_RATE)) %>%
      add_markers(marker = list(color = "pink", size = 10)) %>%
      layout(title = "Admission Rates of Institutions",
             xaxis = list(title = "Institution"),
             yaxis = list(title = "Admission Rate"))
  })
}

shinyApp(ui = ui, server = server)

#ggPlot examining admission rates vs. SAT scores by Institution Type (Public vs Private)

prepared_data <- merged_data %>%
  mutate(
    SATMT25 = as.numeric(as.character(SATMT25)),
    SATMT75 = as.numeric(as.character(SATMT75)),
    SATVR25 = as.numeric(as.character(SATVR25)),
    SATVR75 = as.numeric(as.character(SATVR75)),
    ADM_RATE = as.numeric(as.character(ADM_RATE))
  ) %>%
  
  mutate(
    SATMT_MID = (SATMT25 + SATMT75) / 2,
    SATVR_MID = (SATVR25 + SATVR75) / 2
  ) %>%
  
  filter(!is.na(SATMT_MID) & !is.na(SATVR_MID) & !is.na(ADM_RATE))
str(prepared_data)

prepared_data$CONTROL <- factor(prepared_data$CONTROL, levels = c(1, 2), labels = c("Public", "Private"))

str(prepared_data$CONTROL)

ggplot(prepared_data, aes(x = SATMT_MID, y = ADM_RATE, size = SATVR_MID, color = CONTROL)) +
  geom_point(alpha = 0.7) + 
  scale_size(name = "Mid-range SAT Verbal Score") + 
  scale_color_manual(values = c("Public" = "green", "Private" = "pink")) + 
  theme_minimal() +
  labs(
    x = "Mid-range SAT Math Score",
    y = "Admission Rate",
    title = "Admission Rates vs. SAT Scores by Institution Type",
    color = "Institution Type"
  ) +
  theme(legend.position = "bottom")






