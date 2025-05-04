# Load Libraries
library(shiny)
library(plotly)
library(highcharter)
library(fmsb)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(viridisLite)

# Load dataset
merged <- read_csv("merged_harassment_dataset.csv",
                   col_types = cols(.default = col_double(),
                                    LEA_STATE = col_character(),
                                    LEA_STATE_NAME = col_character()))

# Fix casing
merged$LEA_STATE_NAME <- str_to_title(merged$LEA_STATE_NAME)

# State abbreviation mapping
state_codes <- data.frame(
  LEA_STATE_NAME = state.name,
  state_code = state.abb
)

# Calculate staff per 100 students
merged <- merged %>%
  mutate(
    total_enrollment = TOT_ENR_M + TOT_ENR_F
  ) %>%
  filter(!is.na(total_enrollment) & total_enrollment > 0) %>%
  mutate(
    counselors_per_100 = (SCH_FTECOUNSELORS / total_enrollment) * 100,
    nurses_per_100 = (SCH_FTESERVICES_NUR / total_enrollment) * 100,
    psychologists_per_100 = (SCH_FTESERVICES_PSY / total_enrollment) * 100,
    security_per_100 = (SCH_FTESECURITY_GUA / total_enrollment) * 100
  )

state_summary <- merged %>%
  group_by(LEA_STATE_NAME) %>%
  summarise(
    avg_counselors = mean(counselors_per_100, na.rm = TRUE),
    avg_nurses = mean(nurses_per_100, na.rm = TRUE),
    avg_psychologists = mean(psychologists_per_100, na.rm = TRUE),
    avg_security = mean(security_per_100, na.rm = TRUE)
  ) %>%
  left_join(state_codes, by = "LEA_STATE_NAME") %>%
  filter(!is.na(state_code))

# Bullying rate data
enrollment_cols <- c(
  "SCH_ENR_HI_M", "SCH_ENR_HI_F",
  "SCH_ENR_AM_M", "SCH_ENR_AM_F",
  "SCH_ENR_AS_M", "SCH_ENR_AS_F",
  "SCH_ENR_HP_M", "SCH_ENR_HP_F",
  "SCH_ENR_BL_M", "SCH_ENR_BL_F",
  "SCH_ENR_WH_M", "SCH_ENR_WH_F",
  "SCH_ENR_TR_M", "SCH_ENR_TR_F"
)

harassment_data <- merged %>%
  mutate(TOTAL_ENROLLMENT = rowSums(across(all_of(enrollment_cols)), na.rm = TRUE))

bullying_data <- harassment_data %>%
  group_by(LEA_STATE) %>%
  summarise(
    total_allegations = sum(HBALLEGATIONS_RAC + HBALLEGATIONS_REL + HBALLEGATIONS_ORI, na.rm = TRUE),
    total_students = sum(TOTAL_ENROLLMENT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rate_per_100 = round(total_allegations / total_students * 100, 2)) %>%
  mutate(`hc-key` = paste0("us-", tolower(LEA_STATE)))

# --- UI ---
ui <- fluidPage(
  titlePanel("School Staffing and Harassment Visualization"),
  tabsetPanel(
    tabPanel("Staff Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("map_metric", "Select Staff Type:", choices = c(
                   "Counselors" = "avg_counselors",
                   "Nurses" = "avg_nurses",
                   "Psychologists" = "avg_psychologists",
                   "Security Guards" = "avg_security"
                 ))
               ),
               mainPanel(
                 plotlyOutput("us_map", height = "600px")
               )
             )
    ),
    
    tabPanel("Radar Chart",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_state", "Select a State:",
                             choices = sort(unique(merged$LEA_STATE_NAME)),
                             selected = "Massachusetts")
               ),
               mainPanel(
                 plotOutput("radar_chart", height = "500px")
               )
             )
    ),
    
    tabPanel("Bar Chart",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_state_bar", "Select a State:",
                             choices = sort(unique(merged$LEA_STATE_NAME)),
                             selected = "Massachusetts")
               ),
               mainPanel(
                 plotOutput("bar_chart", height = "500px")
               )
             )
    ),
    
    tabPanel("Bullying Rate Map",
             highchartOutput("bullying_map", height = "700px")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  output$us_map <- renderPlotly({
    plot_ly(
      data = state_summary,
      type = "choropleth",
      locations = ~state_code,
      locationmode = "USA-states",
      z = ~get(input$map_metric),
      key = ~state_code,
      text = ~paste(LEA_STATE_NAME, "<br>", round(get(input$map_metric), 2), "per 100 students"),
      colorscale = "YlGnBu",
      hoverinfo = "text",
      colorbar = list(title = "Staff per 100")
    ) %>%
      layout(geo = list(scope = 'usa'),
             title = "Average Staff per 100 Students by State")
  })
  
  output$radar_chart <- renderPlot({
    state_data <- merged %>%
      filter(LEA_STATE_NAME == input$selected_state) %>%
      summarise(
        Counselors = mean(SCH_FTECOUNSELORS, na.rm = TRUE),
        Nurses = mean(SCH_FTESERVICES_NUR, na.rm = TRUE),
        Psychologists = mean(SCH_FTESERVICES_PSY, na.rm = TRUE),
        Security_Guards = mean(SCH_FTESECURITY_GUA, na.rm = TRUE)
      )
    
    if (nrow(state_data) == 0 || any(is.na(state_data))) {
      return(NULL)
    }
    
    chart_data <- rbind(
      c(4, 2, 1.5, 3),
      c(0, 0, 0, 0),
      state_data
    )
    
    radarchart(chart_data,
               axistype = 1,
               pcol = "darkblue",
               pfcol = rgb(0.1, 0.4, 0.7, 0.5),
               plwd = 2,
               cglcol = "grey",
               cglty = 1,
               axislabcol = "black",
               caxislabels = c(0, 1, 2, 3, 4),
               vlcex = 0.9,
               title = paste("Avg Staff per School in", input$selected_state))
  })
  
  output$bar_chart <- renderPlot({
    state_data <- merged %>%
      filter(LEA_STATE_NAME == input$selected_state_bar) %>%
      summarise(
        Counselors = sum(SCH_FTECOUNSELORS, na.rm = TRUE),
        Nurses = sum(SCH_FTESERVICES_NUR, na.rm = TRUE),
        Psychologists = sum(SCH_FTESERVICES_PSY, na.rm = TRUE),
        Security_Guards = sum(SCH_FTESECURITY_GUA, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Staff", values_to = "Count")
    
    barplot(state_data$Count,
            names.arg = state_data$Staff,
            col = "tomato",
            main = paste("Total Staff Count in", input$selected_state_bar),
            ylab = "Total Count",
            las = 2)
  })
  
  output$bullying_map <- renderHighchart({
    hcmap(
      "countries/us/us-all",
      data = bullying_data,
      name = "Allegations per 100 students",
      value = "rate_per_100",
      joinBy = c("hc-key", "hc-key"),
      borderWidth = 0,
      nullColor = "#d3d3d3"
    ) %>%
      hc_colorAxis(
        stops = color_stops(colors = inferno(10)),
        min = 0
      ) %>%
      hc_title(text = "Harassment/Bullying Allegations per 100 Students by State") %>%
      hc_tooltip(pointFormat = "{point.name}: {point.value}")
  })
}

# --- Launch App ---
shinyApp(ui, server)
