library(tidyverse)
library(shiny)
library(janitor)

shinyApp(
  
  ui = fluidPage(
    selectInput("question", "Question:", datasearch$question),
    tableOutput("data")
  ),
  
  server = function(input, output) {
    
    data = USArrests %>% rownames_to_column(var = "State") %>% clean_names()
    
    datasearch = tribble(~question,
                         "Which state has the most murder?",
                         "Which state has the most assault?",
                         "Which state has the most rape?") %>% 
      mutate(v = word(question, 2),
             v2 = word(question, 6) %>% str_remove(., "[?]"))
    
    findVar = function(metric) {
      metric = enquo(metric)
      
      datasearch %>%
        filter(question == input$question) %>%
        pull(!!metric)
    }
    
    output$data <- renderTable({
      data %>%
        select(findVar(v), findVar(v2)) %>%
        arrange(desc(.[2]))
    })
  }
)