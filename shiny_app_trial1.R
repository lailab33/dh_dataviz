library(shiny)
library(timevis)
library(dplyr)

data_copy <- read.csv("project_data.csv")

ui <- fluidPage(
  titlePanel("Table of protests"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("college",
                       "College:",
                       c("All",
                         unique(as.character(data_copy$college))))
    ),
    column(4,
           selectInput("scope_size",
                       "Scope:",
                       c("All",
                         unique(as.character(data_copy$scope_size))))
    ),
    column(4,
           selectInput("group",
                       "Category of group",
                       c("All",
                         unique(as.character(data_copy$group_category))))
    ),
    column(4,
           selectInput("tag",
                       "Tags on protest",
                       c("All",
                         unique(as.character(data_copy$tags))))
    ),
    column(4,
           selectInput("against",
                       "Protest against",
                       c("All",
                         unique(as.character(data_copy$against))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

server <- function(input, output) {
  output$table <- DT::renderDataTable(DT::datatable({
    data <- data_copy
    if (input$college != "All") {
      data <- data[data$college == input$college,]
    }
    if (input$cyl != "All") {
      data <- data[data$scope == input$scope,]
    }
    if (input$trans != "All") {
      data <- data[data$group == input$group,]
    }
    if (input$trans != "All") {
      data <- data[data$tag == input$tag,]
    }
    if (input$trans != "All") {
      data <- data[data$againt == input$against,]
    }
    data
  }))
  
}

shinyApp(ui = ui, server = server)

