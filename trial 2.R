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
  
  br(),
  helpText("This is an interactive timeline of protests recorded by Vassar and Haverford archives,",
           "users can filter what kinds of protests they would like to view on the timeline and",
           "those protests represented are in the table below. If you select a single protest",
           "it will show up under the *Item selected* tab."),
  br(),
  
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
  
   # Setting the actual output of the above set up
  titlePanel("Timeline of protests"),
  timevisOutput("timelineCustom"),
  
  br(),
  
  titlePanel("Item Selected Currently"),
  fluidRow(textOutput("selected", inline = TRUE)),
  
  #insert the "Item currently selected" part
  
  br(),
  br(),
  br(),
  
  titlePanel("Table of events placed above"),
  DT::dataTableOutput("table")
  )



#OUTPUT SECTION STARTS HERE

  #adding the "input$selected" part of the code
server <- function(input, output) {
  data <- data_copy
  output$timelineCustom <- renderTimevis({
    config <- list(
      editable = TRUE,
      multiselect = TRUE
    )
    timevis(data, options = config)
  })
  
  #this is a little section that isn't working about what is being selected
  output$selected <- renderText(
    paste(input$timelineCustom_selected, collapse = " ")
  )
  
  observeEvent(input$focusSelection, {
    centerItem("timelineInteractive", input$timelineCustom_selected)
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