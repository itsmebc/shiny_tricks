library(shiny)
library(tidyverse)

ui <- fluidPage(h2("Easily join tables by uploading them on the panel."), 
                br(), 
                h4("Select which join function you want and enter by which column the join should be performed on."), 
                hr(),
  sidebarLayout(
    sidebarPanel( width=2,
      fileInput("df1", "First dataframe",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      fileInput("df2", "Second dataframe",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      checkboxInput("header", "Header", TRUE),
      hr(),
      radioButtons("joins", label = ("Functions"),
                   choices = list("Left join" = 1, 
                                  "Right join" = 2, 
                                  "Inner join" = 3, 
                                  "Outer join" = 4, 
                                  "Semi join" = 5, 
                                  "Anti join" = 6), 
                   selected = NULL),
      textInput("by", label = ("By"))),
    mainPanel(
      tabsetPanel(
        tabPanel("First dataframe", tableOutput("df1_contents"), verbatimTextOutput("df1_glimpse")),
        tabPanel("Second dataframe", tableOutput("df2_contents"), verbatimTextOutput("df2_glimpse")),
        tabPanel("Joined dataframe", downloadButton("download", "Download"), tableOutput("df3_contents"))
      )
    )
  )
)

server <- function(input, output) {
  
  
  #df1
  output$df1_contents <- renderTable({
    if (is.null(input$df1)) return(NULL)
    read.csv(input$df1$datapath, header = input$header) %>% head()
  })
  
  output$df1_glimpse <- renderPrint({
    if (is.null(input$df1)) return(NULL)
    read.csv(input$df1$datapath, header = input$header) %>% glimpse()
  })

  
  
  
  #df2
  output$df2_contents <- renderTable({
    if (is.null(input$df2)) return(NULL)
    read.csv(input$df2$datapath, header = input$header) %>% head()
  })
  
  output$df2_glimpse <- renderPrint({
    if (is.null(input$df2)) return(NULL)
    read.csv(input$df2$datapath, header = input$header) %>% glimpse()
  })
  
  
  
  
  #join
  df1 = reactive({ read.csv(input$df1$datapath, header = input$header) %>% mutate(across(.cols=everything(), as.factor))})
  df2 = reactive({ read.csv(input$df2$datapath, header = input$header) %>% mutate(across(.cols=everything(), as.factor))})
  
  joinFunc <- reactive({
    if(input$joins == 1) {
      left_join(df1(), df2(), by=input$by)
    }
    else if (input$joins == 2) {
      right_join(df1(), df2(), by=input$by)
    }
    else if (input$joins == 3) {
      inner_join(df1(), df2(), by=input$by)
    }
    else if (input$joins == 4) {
      full_join(df1(), df2(), by=input$by)
    }
    else if (input$joins == 5) {
      semi_join(df1(), df2(), by=input$by)
    }
    else if (input$joins == 6) {
      anti_join(df1(), df2(), by=input$by)
    }
  })
  

  output$df3_contents = renderTable({
    joinFunc()
    })

  
  output$download <- downloadHandler(
    filename = function() {
      paste("joined_df", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(joinFunc(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)
