library(shiny)
library(timevis)
library(dplyr)

#loading data about the protest, renaming, and deselecting columns that don't matter
data_copy <- read.csv("project_data.csv")

colnames(data_copy) <- c("college", "source", "description", "link", "start", "date_event", 
                         "number_students", "scope_size", "reason", "cause", "tags", "against", "group_category", "group_name", "effect", "method", "location", "is_linked", "umbrella_tag", "text_ocr")

#DELETE THIS LATER
data_copy <- data_copy %>%
  select(-text_ocr)


#function that creates a random id
idmaker <- function(x)
{
  max.val = x*100
  count <- nchar(as.character(max.val))                       # find out how many 'numbers' each ID will have after the letter
  size <- paste("%0",count,"d",sep="")                        # set the variable to be fed into 'sprintf' to ensure we have leading 0's
  lets <- toupper(sample(letters,x, replace=T))  
  
  # randomising the letters 
  nums <- sprintf(size,sample(1:max.val)[1:x])                # randominsing the numbers, and ensuing they all have the same number of characters
  ids <- paste(lets,nums,sep="")                              # joining them together
  return(ids)
}

#adding a random id to each row
data_copy <- data_copy %>%
  mutate(object_id=idmaker(44))

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
    inputId= "date_range" ,
    label= "Select Date Range to Filter:",
    format = "yyyy-mm-dd", 
    start= as.Date("1970-01-01", format = "%Y-%m-%d"), #IS THIS WHAT ACTUALLY NEEDS TO HAPPEN HERE?
    end= as.Date("2021-01-01", format = "%Y-%m-%d"), #IS THIS WHAT ACTUALLY NEEDS TO HAPPEN HERE?
    startview = "decades",
    separator = " to "
  ),
  
  br(),
  
  # Setting the actual output of the above set up
  titlePanel("Timeline of protests"),
  timevisOutput("timeline"),
  
  br(),
  

  conditionalPanel(condition = "output.show_protest_details", #this needs to be added
                   titlePanel("Item Currently Selected"),
                   fluidRow(column(4, wellPanel(h3("Object Source"),
                                                htmlOutput(outputId = "protest_source_info")
                   )
                   ),
                   column(4, wellPanel(h3("Protest Details"),
                                       tableOutput(outputId = "protest_info") 
                   )
                   ),
                   column(4, wellPanel(h4("Object ORC"), 
                                       htmlOutput(outputId = "protest_ocr")
                   )
                   ))),

  br(),
  br(),
  br(),
  
  titlePanel("Table of events in timeline above"),
  DT::dataTableOutput("table")
  )


#------------------------------------------------------------------------------------------------------
##SERVER/OUTPUT SECTION STARTS HERE

#adding the "input$selected" part of the code
server <- function(input, output, session) {



  #observe a click on the timeline
  observeEvent(input$focusSelection, {
    centerItem("timelineInteractive", input$timelineInteractive_selected)
  })
  
  # I don't know what this thing below does
  
  #creating a data table of information variable to inputs of the filter 
    data <- reactive({
      
      #this needs to be here for some reason
      data <- data_copy
      
      #the drop down menu selection reactive functions
      if (input$vc.hv != "All") {
        data <- data[data$college == input$vc.hv,]
      }  
      if (input$grp != "All") {
        data <- data[data$scope_size == input$scp,]
      } 
      if (input$scp != "All") {
        data <- data[data$group_category == input$grp,]
      }
    
      #date range filter code! which worksssssss!!!!!
      data <- data [which(data$start >= input$date_range[1] & data$start <= input$date_range[2]),]
    
    data })
    
  
#output to render the table
  output$table <- DT::renderDataTable(DT::datatable(data()))

#output to render the timeline
output$timeline <- renderTimevis({
  timevis(data())
  })
  
}

shinyApp(ui = ui, server = server)