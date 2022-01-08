library(shiny)
library(tidyverse)

#Module 1
player1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("p1_choice"), "Choice", c("1" = 1, "2" = 2, "3" = 3))
  )
}

player1_server <- function(id, values) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent( input$p1_choice , {
        values$player1 <- input$p1_choice
      })
      
    }
  )
}

#Module 1
player2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("p2_choice"), "Choice", c("1" = 1, "2" = 2, "3" = 3))
  )
}

player2_server <- function(id, values) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent( input$p2_choice , {
        values$player2 <- input$p2_choice
      })
      
    }
  )
}

# Module 3
calculate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("print"))
  )
}

calculate_server <- function(id, values) {
  moduleServer(
    id,
    function(input, output, session) {
      
      winner = reactive({ case_when(
        values$player1 == values$player2 ~ "Correct!",
        TRUE ~ "Guess Again"
      ) })
      
      output$print <- renderPrint({
        winner()
      })
    }
  )
}

# Application
app_ui <- function() {
  fluidPage(
    player1_ui("player1_ui_1"),
    player2_ui("player2_ui_1"),
    calculate_ui("calculate_ui_1")
  )
}

app_server <- function(input, output, session) {
  values <- reactiveValues()
  player1_server("player1_ui_1", values = values)
  player2_server("player2_ui_1", values = values)
  calculate_server("calculate_ui_1", values = values)
}

shinyApp(app_ui, app_server)
