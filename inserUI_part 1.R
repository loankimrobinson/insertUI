library(shiny)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("slate"),
  h2("insertUI without Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload data", accept = ".csv"),
      uiOutput("variable")
    ),
    mainPanel(
      div(id = "container")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    tryCatch(
      read.csv(input$file$datapath[1]),
      error = function(e) {
        showNotification(
          ui = tagList(strong("Error:"), conditionMessage(e)),
          type = "error"
        )
        req(FALSE)
      }
    )
  })
  

  output$variable <- renderUI({
    choices <- c("Select variable " = "", names(dataset()))
    tagList(
      selectInput("xvar", "X variable", choices),
      selectInput("yvar", "Y variable", choices),
      conditionalPanel("input.xvar && input.yvar",actionButton("add", "Add plot"),actionButton("remove", "Remove plot")
      )
    )
  })
 
  
   
  count <- 0
  insert <- c()

  
    
  observeEvent(input$add, {
    count <<- count + 1
    id <- paste0("plot", count)
  
    
    xvar <- input$xvar
    yvar <- input$yvar
 
    output[[id]] <- renderPlot({
      df <- dataset()
      ggplot(df, aes_string(xvar, yvar)) +
        geom_point(alpha = 0.6) +
        guides(color = FALSE) +
        xlab(xvar) + ylab(yvar) + 
        labs(title = paste0(xvar, " vs ", yvar), 
             subtitle = "This is subtitle... ", 
             caption = paste0( 'We just add ', count, " plot"))
    })
    insertUI("#container", 
             where = "beforeEnd",
             ui = div(plotOutput(id, width = 500, height = 275), br(),br())
    )
    insert <<- c(id, insert)
  })
  
  
  observeEvent(input$remove, {
    removeUI(
       selector = paste0('#', insert[length(insert)])
    )
    insert <<- insert[-length(insert)]
  })

}

shinyApp(ui, server)