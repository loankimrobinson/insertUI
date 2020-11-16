library(shiny)
library(ggplot2)
library(shinythemes)

#===========================
# addmlUI <- function(id) {
#   ns <- NS(id)
#   div(id = id,
#       fluidRow(
#         hr(style = "height: 10px;border: 0;box-shadow: 0 10px 10px -10px #40E0D0 inset;"),
#         column(width = 4,
#                tagList(
#                  fileInput(ns("file"), "Upload data", accept = ".csv"),
#                  uiOutput(ns("variable"))
#                )),
#         column(
#           width = 6,
#           offset = 1,
#           htmlOutput(ns("text")),
#           plotOutput(ns("plot_out"), height = 250)
#         )
#       ), br(), br())
#   
# }

addmlUI <- function(id) {
  ns <- NS(id)
  div(id = id,
      hr(style = "height: 10px;border: 0;box-shadow: 0 10px 10px -10px #40E0D0 inset;"),
      sidebarLayout(
        sidebarPanel(
                 fileInput(ns("file"), "Upload data", accept = ".csv"),
                 uiOutput(ns("variable"))
               ),
      mainPanel(
        fluidRow(column(width = 10, offset = 1,
          htmlOutput(ns("text")),
          plotOutput(ns("plot_out"), height = 300)))
        )
      ), br(), br())
  
}

addmlServer <- function(input, output, session) {
  ns <- session$ns
  
  dataset <- reactive({
    req(input$file)
    tryCatch(
      read.csv(input$file$datapath[1]),
      error = function(e) {
        showNotification(ui = tagList(strong("Error:"), conditionMessage(e)),
                         type = "error")
        req(FALSE)
      }
    )
  })
  
  
  output$variable <- renderUI({
    req(dataset())
    choices <- c("Select variable " = "", names(dataset()))
    tagList(
      selectInput(ns("xvar"), "X variable", choices),
      selectInput(ns("yvar"), "Y variable", choices),
      actionButton(ns("submit"), "Plot")
    )
  })
  
  
  xvar <- reactive({
    req(dataset())
    input$xvar
  })
  yvar <- reactive({
    req(dataset())
    input$yvar
  })
  
  
  observeEvent(input$submit, {
    output$plot_out <- renderPlot({
      df <- dataset()
      ggplot(df, aes_string(xvar(), yvar())) +
        geom_point(alpha = 0.6) +
        guides(color = FALSE) +
        xlab(xvar()) + ylab(yvar())
    })
    output$text <- renderText({
      HTML(
        paste0(
          "<div style='color:#40E0D0;font-size: 20px;'>Plot: ",
          xvar(),
          " vs ",
          yvar(),
          "</div>"
        )
      )
    })
    
  })
  
}
#end of module
#======================================
#start app here
ui <- fluidPage(
  theme = shinytheme("slate"),
  h2(
    "insertUI with Module",
    actionButton("add", "insertUI", style = "width:100px;"),
    actionButton("remove", "removeUI",style = "width:100px;"),
    style = "text-align:center;"
  ),
  tags$div(id = "container")
)

server <- function(input, output, session) {
  
  uiCount = reactiveVal(0)
  insert <- c()
  
  observeEvent(input$add, {
    uiCount(uiCount() + 1)
    
    id <- paste0("plot", uiCount())

    insertUI("#container",
             where = "beforeEnd",
             ui = addmlUI(id))
    callModule(addmlServer, id = id)
    
    #if you want to delete the last UI to the first UI
    # "plot1" "plot2" "plot3"
    insert <<- c(insert,id)
    
    #if you want to delete the first UI to the last UI
    #insert <<- c(id, insert)
    #"plot3" "plot2" "plot1"
  })
  
  
  observeEvent(input$remove, {
    removeUI(selector = paste0("#", insert[length(insert)]))
    insert <<- insert[-length(insert)]
  })
  
  
  
  
}

#=================================
shinyApp(ui, server)


