library(tidyverse)
library(shiny)
library(janitor)

shinyApp(
  
  ui = fluidPage(
    textInput("ask", "Ask question:", ""),
    actionButton("askbutton", "Ask"),
    tableOutput("data")
  ),
  
  server = function(input, output) {
    
    data = USArrests %>% rownames_to_column(var = "State") %>% clean_names()
    
    questions = reactiveValues()
    
    observeEvent(input$askbutton,{
      questions$data = tribble(~text, 
                               input$ask) %>%
                       mutate(v = word(text, 2),
                              v2 = word(text, 6) %>% str_remove(., "[?]"))
    })
    
    findVar = function(metric) {
      metric = enquo(metric)
      
      questions$data %>%
        pull(!!metric)
    }
    
    output$data <- renderTable({
      data %>%
        select(findVar(v), findVar(v2)) %>%
        arrange(desc(.[2]))
    })
    
  }
  
)