library(tidyverse)
library(shiny)
library(janitor)

data = USArrests %>% rownames_to_column(var = "State") %>% clean_names()

shinyApp(
  
  ui = fluidPage(
    textInput("ask", "Ask question:", ""),
    actionButton("askbutton", "Ask"),
    tableOutput("data")
  ),
  
  server = function(input, output) {
    
    questions = reactiveValues()
    
    observeEvent(input$askbutton,{
      questions$data = tribble(~text,
                               input$ask %>% tolower()) 
      })
    
    metrics = reactive({
      str_extract(questions$data, data%>%colnames) %>% na.omit()
    })
    
    output$data <- renderTable({
      data %>%
        select(metrics()[1], metrics()[2]) %>%
        arrange(desc(.[2]))
    })
    
  }
  
)