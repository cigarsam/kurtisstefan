
library(tidyverse)
data <- read.csv("export.csv")

# setwd("/Users/kurtisstefan/Documents/Code/kurtisstefan/")
# shinylive::export(appdir = ".", destdir = "docs")
#  shinylive::export(appdir = "/Users/kurtisstefan/Documents/Code/kurtisstefan/kurtisstefan/", destdir = "/Users/kurtisstefan/Documents/Code/kurtisstefan/kurtisstefan/docs")
#  httpuv::runStaticServer("/Users/kurtisstefan/Documents/Code/kurtisstefan/kurtisstefan/docs/", port=8008)

# 
# transformed_data <- data %>%
#   pivot_longer(
#     cols = V3:V112, # Specify the range of columns (V3 to V112 in your real data)
#     names_to = "Variable",
#     values_to = "Details"
#   ) %>% 
#   mutate(
#     PROGRAM = stringr::str_split(Details, pattern = "\\(", simplify = TRUE)[, 1],  # Everything before '('
#     Details = paste0("(", stringr::str_split(Details, pattern = "\\(", simplify = TRUE)[, 2]),
#     Details = iconv(Details, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
#     ) %>%
#   separate_rows(Details, sep = "\\(\\s*\\)") %>%  
#   # Clean up extra spaces and parentheses
#    # mutate(
#    #   Details = str_replace_all(Details, "^\\(|\\)$", "")  # Remove the outer parentheses
#    # ) %>% 
#   filter(str_detect(PROGRAM, "\\S")) %>%   # Keep rows where Details is not empty
#   filter(!str_equal(Details, "\\(")) %>%  # Keep rows where Details is not empty
#  filter(!str_equal(Details, "")) %>%  # Keep rows where Details is not empty
#   mutate(
#     ApplicantDegree = str_extract(Details, "((?i)MD|(?i)DO|(?i)Md|(?i)Do|(?i)do|(?i)md)"),
#     ApplicantIMG = str_extract(Details, "(IMG|img|Img)"), 
#     ApplicantInstate = str_extract(Details, "(OOS|IS|oos)"),
#     GeoPreference = str_extract(Details, "((?i)\\+geo|(?i)\\-geo|(?i)geo -|(?i)geo +|(?i)geo+|(?i)geo-)"),
#     Signal = str_extract(Details, "(\\+gold|\\+silver|\\-sig|\\+sig|(?i)\\bNo sig\\b)"),  
#     Step2 = str_extract(Details, "(22[x\\d]+|23[x\\d]+|24[x\\d]+|25[x\\d]+|26[x\\d]+|27[x\\d]+|28[x\\d]+)"), 
#     AOA = str_extract(Details, "(?i)AOA"), 
#     GHHS = str_extract(Details, "(?i)GHHS"), 
#     HomeProgram = str_extract(Details, "(?i)home")
#   )  
# Split details into multiple rows by newline or comma
# separate_rows(Details, sep = "\\n|,\\s*")
# Trim whitespace for neatness
# mutate(Details = trimws(Details))

# View the transformed data
#view(transformed_data[1:100,])


data <- data %>%
  mutate(
    lower_bound = as.numeric(sub("-.*", "", Step.2.Score)),  # Extract lower bound
    upper_bound = as.numeric(sub(".*-", "", Step.2.Score)),  # Extract upper bound
    mid_point = (lower_bound + upper_bound) / 2  # Calculate midpoint
  ) %>% 
  mutate(
    region = case_when(
      State %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont") ~ "NewEngland",
      State %in% c("New Jersey", "New York", "Pennsylvania") ~ "MiddleAtlantic",
      State %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin") ~ "ENC",
      State %in% c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "WNC",
      State %in% c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", "Puerto Rico", "South Carolina", "Virginia", "West Virginia") ~ "SouthAtlantic",
      State %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee") ~ "ESC",
      State %in% c("Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "WSC",
      State %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming") ~ "Mountain",
      State %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington") ~ "Pacific",
      TRUE ~ "Unknown"  ))


library(shiny)
# Define UI
library(ggplot2)
library(bslib)


ui <- page_navbar(
  # full_screen = TRUE,
  sidebar = sidebar(
    selectInput("InputProgram", label="PROGRAM",
                choices=as.character(data$Program.Name), 
                multiple=TRUE),
    selectInput("InputRegion", label="REGION",
                choices=as.character(data$region), 
                multiple=TRUE),
    numericInput("MyStep2", label="STEP2", value = 210)),
  nav_panel("Table",  DT::dataTableOutput("mytable")),
  nav_panel("Step 2", plotOutput("histogram")), 
  nav_panel("Fit per Region", DT::dataTableOutput("mytableregion")),
  nav_panel("Signal Needed?",selectInput("InputProgrambyRegion", label="Program Within Region",
                                         choices=NULL, 
                                         multiple=TRUE), plotOutput("SignalNeeded"))
)



# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$InputProgram)  # Ensure input is available before filtering
    data %>%
      filter(Program.Name %in% input$InputProgram)  # Filter by selected programs
  })
  
  f0_data <- reactive({
    req(input$InputRegion)
    data %>%
      filter(region %in% input$InputRegion)
  })
  
  observe({
    filtered_df <- f0_data()  # Get filtered data based on region
    updateSelectInput(session, 
                      "InputProgrambyRegion", 
                      choices = as.character(filtered_df$Program.Name))
  })
  
  f1_data <- reactive({
    req(input$InputRegion)  # Ensure input is available before filtering
    req(input$MyStep2)
    data %>%
      filter(region %in% input$InputRegion)  %>% 
      filter(as.numeric(input$MyStep2) >= as.numeric(lower_bound))# Filter by selected programs
  })
  output$mytableregion <- DT::renderDataTable({
    DT::datatable(f1_data())
  })
  
  # Render the filtered data table
  output$mytable <- DT::renderDataTable({
    DT::datatable(filtered_data())
  })
  
  
  output$histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = mid_point)) +
      geom_histogram(bins = 5, color = "black", fill = "blue", alpha = 0.7) +
      labs(title = "Histogram from Bin Ranges", x = "Value", y = "Frequency") +
      theme_minimal()
  })
  
  output$SignalNeeded <- renderPlot({
    req(input$InputProgrambyRegion)
    f1_data() %>% 
      filter(Program.Name %in% input$InputProgrambyRegion) %>% 
      group_by(Program.Name, Signal) %>% 
      count(Program.Name, Signal) %>% 
      ggplot(aes(x=Program.Name, y=n, fill=Signal)) + 
      geom_bar(position="dodge", stat="identity") + theme_bw()
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
