#
# WICS_HACK_25
#

library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)

# Define user interface
ui <- fluidPage(
  tags$style(HTML("
    .well {
      background-color: #ADD8E6;
    }
    body {
      font-family: 'Georgia';
      background-color: #ADD8E6
    }
  ")),

  absolutePanel(
    top = 50, left = 400, fixed = TRUE,
    tags$img(src = "https://www.austinmonitor.com/wp-content/uploads/2020/03/18241799911_0ee016e973_o-2.jpg", 
             height = "300px")
  ),
  
  # Sidebar and main panels 
  sidebarLayout(
    sidebarPanel(
      
      
      # Radio buttons input
      radioButtons("radio", label = h4("Choose a variable to compare with type of outcome:"), 
                   choices=list("Age" = 1, "Condition" = 2, "Situation upon Intake" = 3, 
                                "Breed" = 4), 
                   selected = 1),
      hr(),
      
      # Slider input
      sliderInput("slider", label = h4("Year Range"), min = 2013, 
                  max = 2018, value = c(2013, 2018))
    ),
    
    hr()),
  
  
  # Main panel
  mainPanel(
    fluidRow(verbatimTextOutput("statistics")), 
    plotOutput("variable_model", height = "550px", width = "850px"),
    style = "display: block; margin-left: auto; margin-right: auto;"
  )
) 

# Define server function
server <- function(input, output) {
  
  #Import UFO Data and clean/parse the variables
  shelter_intakes <- read_csv("aac_intakes.csv")
  shelter_outcome <- read_csv("aac_outcomes.csv")
  shelter <- inner_join(shelter_intakes, shelter_outcome, by ="animal_id")
  shelter <- select(shelter, -(breed.x), -(animal_type.x), -(color.x), -(datetime2), -(monthyear), -(name.y))
  
  intake_date <- as.character(shelter$datetime.x)
  intake_date <- str_split(intake_date, " ")
  intake_date <- sapply(intake_date, `[`, 1)
  intake_date <- ymd(intake_date)
  shelter <- cbind(shelter, intake_date)
  
  outcome_date <- as.character(shelter$datetime.y)
  outcome_date <- str_split(outcome_date, " ")
  outcome_date <- sapply(outcome_date, `[`, 1)
  outcome_date <- ymd(outcome_date)
  shelter <- cbind(shelter, outcome_date)
  
  shelter <- select(shelter, -(datetime.x), -(datetime.y))
  
  # Access the values of the widgets to create a plot
  output$variable_model <- renderPlot({
  
    if (input$slider[1] == 2013 || input$slider[2] == 2013) {
      shelter <- filter(shelter, year(outcome_date) >= input$slider[1] & year(outcome_date) <= input$slider[2])
      }
    else if (input$slider[1] == 2014 || input$slider[2] == 2014) {
      shelter <- filter(shelter, year(outcome_date) >= input$slider[1] & year(outcome_date) <= input$slider[2])
    }
    else if (input$slider[1] == 2015 || input$slider[2] == 2015) {
      shelter <- filter(shelter, year(outcome_date) >= input$slider[1] & year(outcome_date) <= input$slider[2])
    }
    else if (input$slider[1] == 2016 || input$slider[2] == 2016) {
      shelter <- filter(shelter, year(outcome_date) >= input$slider[1] & year(outcome_date) <= input$slider[2])
    }
    else if (input$slider[1] == 2017 || input$slider[2] == 2017) {
      shelter <- filter(shelter, year(outcome_date) >= input$slider[1] & year(outcome_date) <= input$slider[2])
    }
    else if (input$slider[1] == 2018 || input$slider[2] == 2018) {
      shelter <- filter(shelter, year(outcome_date) >= input$slider[1] & year(outcome_date) <= input$slider[2])
    }
    
    # Conditions for radio and graphs
    if (input$radio == 1) {
      model <- ggplot(shelter) + geom_bar(aes(x = age_upon_outcome, fill = outcome_type)) + coord_flip() +
      labs(title = paste("Relationship between Age and Type of Outcome between", input$slider[1], "and", input$slider[2]), 
      x = "Age", y = "Frequency", fill = "Outcome") + theme(plot.background = element_rect(fill = "#ADD8E6"),
      panel.background = element_rect(fill = "#ADD8E6"), legend.background = element_rect(fill = "#ADD8E6"), plot.title = element_text(size = 17), 
      axis.title = element_text(size = 14), axis.text = element_text(size = 10), legend.text = element_text(size = 12), legend.title = element_text(size = 14))
    }
    else if (input$radio == 2) {
      model <- ggplot(shelter) + geom_bar(aes(x = intake_condition, fill = outcome_type)) + coord_flip() +
      labs(title = paste("Relationship between Condition when Admitted and Type of Outcome between", input$slider[1], "and", input$slider[2]), 
      x = "Condition", y = "Frequency", fill = "Outcome") + theme(plot.background = element_rect(fill = "#ADD8E6"),
      panel.background = element_rect(fill = "#ADD8E6"), legend.background = element_rect(fill = "#ADD8E6"), plot.title = element_text(size = 15), 
      axis.title = element_text(size = 14), axis.text = element_text(size = 10), legend.text = element_text(size = 12), legend.title = element_text(size = 14))
    }
    else if (input$radio == 3) {
      model <- ggplot(shelter) + geom_bar(aes(x = intake_type, fill = outcome_type)) + coord_flip() +
      labs(title = paste("Relationship between Situation when Admitted and Type of Outcome between", input$slider[1], "and", input$slider[2]), 
      x = "Living Situation", y = "Frequency", fill = "Outcome") + theme(plot.background = element_rect(fill = "#ADD8E6"),
      panel.background = element_rect(fill = "#ADD8E6"), legend.background = element_rect(fill = "#ADD8E6"), plot.title = element_text(size = 15),  
      axis.title = element_text(size = 14), axis.text = element_text(size = 10), legend.text = element_text(size = 12), legend.title = element_text(size = 14))
    }
    else if (input$radio == 4) {
      shelter <- mutate(shelter, is_pitbull = ifelse(grepl("Pit Bull", breed.y), "Pit Bull", "Non-Pit Bull"))
      model <- ggplot(shelter) + geom_bar(aes(x = is_pitbull, fill = outcome_type)) +
      labs(title = paste("Relationship between Agression and Type of Outcome between", input$slider[1], "and", input$slider[2]), 
      x = "Breed", y = "Frequency", fill = "Outcome") + theme(plot.background = element_rect(fill = "#ADD8E6"),
      panel.background = element_rect(fill = "#ADD8E6"), legend.background = element_rect(fill = "#ADD8E6"), plot.title = element_text(size = 17),
      axis.title = element_text(size = 14), axis.text = element_text(size = 10), legend.text = element_text(size = 12), legend.title = element_text(size = 14))
    }
    print(model)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
