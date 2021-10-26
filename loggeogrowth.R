require(shiny)
require(plotly)
require(shinydashboard)
require(mathjaxr)
require(ggplot2)

ui1 <- dashboardPage(
  dashboardHeader(title = "Geometric & Logistic Growth"),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    fluidRow(
      column(width = 3, 
    selectInput('type', 'Geometric or logistic?', c("Geometric", "Logistic")),
    conditionalPanel(
      condition = "input.type == 'Geometric'",
      numericInput('lambda', 'Growth rate', 0, min=-1, max=1, step=0.05)
    ),
    conditionalPanel(
      condition = "input.type == 'Logistic'",
      numericInput('K', 'Carrying capacity', 30, min=10, max=1000, step=5), 
      numericInput('rmax', "Maximum growth rate", 0, min=-1, max=1, step=0.05)
    )), 
    column(width = 3, tableOutput('pop')),
    column(width=6, 
           plotlyOutput('growthPlot'))
    
    )
  ))


######## server #########
server1 <- function(input, output, session) {
  observe({
  if (input$type == "Geometric") {
  output$pop <- renderTable({
    df <- data.frame(Time = 0:15, Individuals = rep(NA, 16))
    df[1,2] <- 3 #N0
    for(i in 2:nrow(df)) {
      df[i,2] <- df[i-1,2]+(df[i-1,2]*input$lambda)
    }
    return(df)
    })
  }
  else if (input$type == "Logistic") {
    output$pop <- renderTable({
      df <- data.frame(Time = 0:15, Individuals = rep(NA, 16))
      df[1,2] <- 3 #N0
      for(i in 2:nrow(df)) {
        df[i,2] <- df[i-1,2] + (df[i-1,2] * input$rmax * (1 - (df[i-1,2]/input$K)))
      }
      return(df)
      })
  }
  })
  
  observe({
    if (input$type == "Geometric") {
      output$growthPlot <- renderPlotly({
        df <- data.frame(Time = 0:15, Individuals = rep(NA, 16))
        df[1,2] <- 3 #N0
        for(i in 2:nrow(df)) {
          df[i,2] <- df[i-1,2]+(df[i-1,2]*input$lambda)
        }
        fig <- plot_ly(df, x = ~Time, y = ~Individuals, type='scatter', mode='markers')
        fig <- fig %>% add_trace(mode = 'lines+markers') %>% 
          layout(showlegend = FALSE)
        return(fig)
      })
    }
    else if (input$type == "Logistic") {
      output$growthPlot <- renderPlotly({
        df <- data.frame(Time = 0:15, Individuals = rep(NA, 16))
        df[1,2] <- 3 #N0
        for(i in 2:nrow(df)) {
          df[i,2] <- df[i-1,2] + (df[i-1,2] * input$rmax * (1 - (df[i-1,2]/input$K)))
        }
        fig <- plot_ly(df, x = ~Time, y = ~Individuals, type='scatter', mode='markers')
        fig <- fig %>% add_trace(mode = 'lines+markers') %>% 
          layout(showlegend = FALSE)
        return(fig)
      })
    }
  })
  
 

}

shinyApp(ui1, server1)



