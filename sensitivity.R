require(shiny)
require(plotly)
require(shinydashboard)
require(mathjaxr)

######### ui ###########
ui1 <- dashboardPage(
  dashboardHeader(title = "Sensitivity Analysis"),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    fluidRow(
      column(width=4, 
             box(width=NULL, title = "Input parameters:", status="primary", solidHeader = TRUE,
               numericInput('s1', 'Fawn survival', 0.3, min=0, max=1, step=0.05),
               numericInput('s2', 'Yearling survival', 0.9, min=0, max=1, step=0.05),
               numericInput('s3', 'Adult survival', 0.7, min=0, max=1, step=0.05),
               numericInput('f1', 'Fawn fecundity', 0.3, min=0, max=3, step=0.05),
               numericInput('f2', 'Yearling fecundity', 0.9, min=0, max=3, step=0.05),
               numericInput('f3', 'Adult fecundity', 0.7, min=0, max=3, step=0.05)),
             box(width=NULL,
                 uiOutput('ex1'))),
      column(width=7,
             box(width=NULL,
               plotlyOutput("agePie")),
             fluidRow(
               column(width=5,
                      box(width=NULL,
                          title = "Projection Matrix", status="primary", solidHeader=TRUE,
                          tableOutput('tableProjMat'))),
               column(width=7,
                      box(width=NULL, status="primary",
                          tableOutput('ageRepro'),
                          tableOutput('lambda')))
             )))))


######## server #########
server1 <- function(input, output, session) {
  
  projMat <- reactive({
    A <- matrix(c(input$f1, input$f2, input$f3,
                  input$s1, 0, 0,
                  0, input$s2, input$s3),
                nrow=3, byrow=TRUE)
    eA <- eigen(A)
    lambda <- Re(eA$values[1])
    ageDist <- Re(eA$vectors[,1])
    ageDist <- ageDist/sum(ageDist)
    reproValue <- Re(eigen(t(A))$vectors[,1])
    reproValue <- reproValue/reproValue[1]
    return(list(A=A, lambda=lambda, ageDist=ageDist, reproValue=reproValue))
  })
 
  output$tableProjMat <- renderTable({
    A <- projMat()$A
    return(A)
  }, colnames=FALSE)
  
  output$tableProjMat <- renderTable({
    A <- projMat()$A
    return(A)
  }, colnames=FALSE)
  
  output$lambda <- renderTable({
    lambda <- rbind("Growth rate (lambda)" = projMat()$lambda)
    colnames(lambda) <- "Growth rate (lambda)"
    return(lambda)
  }, rownames=FALSE, colnames=TRUE)
  
  output$ageRepro <- renderTable({
    eA <- projMat()
    ageRepro <- rbind("Age distribution" = eA$ageDist,
                      "Reproductive value" = eA$reproValue)
    colnames(ageRepro) <- c("Fawns", "Yearlings", "Adults")
    return(ageRepro)
  }, rownames=TRUE, colnames=TRUE)
  
  output$agePie <- renderPlotly({
    eA <- projMat()
    fig <- plot_ly(as.data.frame(eA$ageDist), labels = c("Fawns", "Yearlings", "Adults"),
                   values= eA$ageDist,
                    type = 'pie')
    fig %>% layout(title = 'Age Distribution',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$ex1 <- renderUI({
    withMathJax(helpText('$$sensitivity = \\frac{\\Delta \\lambda}{\\Delta \\theta} \\  \\ as \\lim\\limits_{\\Delta \\theta \\to 0}$$'),
                helpText('$$ where \\ \\lambda \\ = \\ growth \\ rate \\ and \\\\ \\theta \\ = \\ a \\ population \\ parameter \\ (e.g. \\ survival \\ or \\ fecundity)$$'))
  })
  
}




shinyApp(ui1, server1)
