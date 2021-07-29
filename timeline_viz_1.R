library(shiny)
library(timevis)
library(dplyr)

#loading data about the protest, renaming, and deselecting columns that don't matter
data_copy <- read.csv("project_data.csv")

colnames(data_copy) <- c("college", "source", "description", "link", "date_source", "date_event", 
                         "number_students", "scope_size", "reason", "cause", "tags", "against", "group_category", "group_name", "effect", "method", "location", "is_linked", "umbrella_tag", "text_ocr")

data_copy <- data_copy %>%
  select(-text_ocr, -umbrella_tag)

# User experience section 
ui <- fluidPage(
  
  titlePanel("Vassar and Haverford Protest Timeline"),
  
  # Create a new Row in the UI for selectInputs
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
  # Create a new row for the table.
  DT::dataTableOutput("table")
  
  #adding a section so you can see what the data is representing
  column(4,
         div(
           id = "timelinedata",
           class = "optionsSection",
           tags$h4("Data:"),
           tableOutput("table"),
           hr(),
           div(tags$strong("Visible window:"),
               textOutput("window", inline = TRUE)),
           div(tags$strong("Selected items:"),
               textOutput("selected", inline = TRUE)),
           div(tags$strong("Visible items:"),
               textOutput("visible", inline = TRUE)),
         )
  )
)
      
  )

server <- function(input, output) {
  
  
}



shinyApp(ui = ui, server = server)