library(shiny)
library(timevis)
library(dplyr)

#loading data about the protest, renaming, and deselecting columns that don't matter
data_copy <- read.csv("project_data.csv")

colnames(data_copy) <- c("college", "source", "description", "link", "date_source", "start", 
                         "number_students", "scope_size", "reason", "cause", "tags", "against", "group_category", "group_name", "effect", "method", "location", "is_linked", "umbrella_tag", "text_ocr")

data_copy <- data_copy %>%
  select(-text_ocr, -umbrella_tag)

# INPUT SECTION STARTS HERE
ui <- fluidPage(
  
  titlePanel("Vassar and Haverford Protest Timeline"),
  
  #adding white space to the visual
  br(),
  br(),
  
  titlePanel("Change settings"),
  helpText("Select which protests you want to view.",
           "Protests and their associated information will be listed in the table at the bottom",
           "If you select a particular point of interest the associated information 
           will be listed below the timeline"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(column(4, uiOutput("protest_dates")
  )),
  
  fluidRow(
    column(4,
           selectInput("vc.hv",
                       "College:",
                       c("All",
                         unique(as.character(data_copy$college))))
    ),
    column(4,
           selectInput("scp",
                       "Scope:",
                       c("All",
                         unique(as.character(data_copy$scope_size))))
    ),
    column(4,
           selectInput("grp",
                       "Protesting group:",
                       c("All",
                         unique(as.character(data_copy$group_category))))
    )
  ),
  
  dateRangeInput(
    inputId= "protest_date_range" ,
    label= "Select Date Range to Filter:",
    start = "1970-01-01", 
    end = "2021-01-01",
    format = "yyyy-mm-dd",
    startview = "years",
    separator = " to "
  ),
 
  br(),
  br(),
  
   # Setting the actual output of the above set up
  titlePanel("Timeline of protests"),
  timevisOutput("timelineCustom"),
  setOptions(list(editable = TRUE)),
  
br(),
br(),
  
  titlePanel("Table of events placed above"),
  DT::dataTableOutput("table")
  )

#OUTPUT SECTION STARTS HERE
server <- function(input, output) {
  data <- data_copy
  output$timelineCustom <- renderTimevis({
    config <- list(
      editable = TRUE,
      multiselect = TRUE
    )
    timevis(data, options = config)
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- data_copy
    if (input$vc.hv != "All") {
      data <- data[data$college == input$vc.hv,]
    }
    if (input$grp != "All") {
      data <- data[data$scope_size == input$scp,]
    }
    if (input$scp != "All") {
      data <- data[data$group_category == input$grp,]
    }
    data
  }) 
  )
  
  output$date_range <- renderUI({
    input$protest_date_range
  })
}

shinyApp(ui = ui, server = server)