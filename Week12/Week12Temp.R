library(shiny)

penguinData <- read.csv("https://tkelleman.github.io/tkweb/week3/w03-penguins.csv")
x.names = names(penguinData)[4:6]
y.names = names(penguinData)[4:6]

ui <- fluidPage(
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
  br(), br(), 
  wellPanel(style = "background-color:#acdfe6;",
            titlePanel(h2("Analysis of Penguin Data", align = "center",
                          style = "color:darkred; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;")),
  ),
  sidebarLayout(
    sidebarPanel(style = "background-color:#acdfe6;",
                 tags$head(
                   tags$style("body {background-color:white }")  # 
                 ),
                 radioButtons(inputId = "species",
                              label = "Species",
                              choices = c("Adelie",
                                          "Gentoo",
                                          "Chinstrap",
                                          "All"),
                              inline = FALSE,
                              selected = "All"),
                 br(),
                 
                 selectInput(inputId = "Y",
                             label = "Response Variable: Y",
                             choices = y.names),
                 
                 selectInput(inputId = "X",
                             label =  "Predictor Variable: X",
                             choices = x.names,
                             selected = x.names[2]),
                
                 
                 sliderInput(inputId = "newX", 
                             label = "New Value for Prediction:", 
                             value = 2.5, 
                             min = 0, 
                             max = 20, 
                             step = 0.1),
                 hr(),
    ),   
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel(h5("Scatter Plot", style ="color:darkgreen; font-weight: bold;"), plotOutput("plot")),
                  tabPanel(h5("Regression Coefficients", style ="color:darkgreen; font-weight: bold;"), tableOutput("table")),
                  tabPanel(h5("Diagnostics", style ="color:darkgreen; font-weight: bold;"), plotOutput("diagnosis")),
                  tabPanel(h5("Prediction", style ="color:darkgreen; font-weight: bold;"), plotOutput("predPlt"))
      )
    )     
  )          
) 

## Server function
server <- function(input, output) {
  workDat = function(){
    if (input$species == "Adelie") {
      workingData = penguinData[which(penguinData$species == "Adelie"),]
    } else if (input$species == "Gentoo") {
      workingData = penguinData[which(penguinData$species == "Gentoo"),]
    } else if (input$species == "Chinstrap") {
      workingData = penguinData[which(penguinData$species == "Chinstrap"),]
    } else {
      workingData = penguinData
    }
    workingData 
  }
  ## Scatter Plot
  output$plot <- renderPlot({
    par(bg="lightgray")
    dataset = workDat()[,-1]
    plot(dataset[,input$X], dataset[,input$Y], 
         xlab = input$X,
         ylab = input$Y,
         main = paste("Relationship between", input$Y, "and", input$X)
    )

    abline(lm(dataset[,input$Y] ~ dataset[,input$X]),
           col = "blue",
           lwd = 2)
  })
  ## Regression Coefficients
  output$table <- renderTable({
    br()
    br()
    dataset = workDat()[,-1]
    m0 = lm(dataset[,input$Y] ~ dataset[,input$X])
    regcoef = data.frame(coef(summary(m0)))
    regcoef$Pvalue = regcoef[,names(regcoef)[4]]
    regcoef$Variable = c("Intercept", input$X)
    regcoef[,c(6, 1:3, 5)]
    
  })
  
  ## Diagnostics
  output$diagnosis <- renderPlot({
    par(bg="lightgray")
    dataset = workDat()[,-1]
    m1=lm(dataset[,input$Y] ~ dataset[,input$X])
    par(mfrow=c(2,2))
    plot(m1)
  })   
  
  ## Prediction
  output$predPlt <- renderPlot({
    par(bg="lightgray")
    dataset = workDat()[,-1]
    m3 = lm(dataset[,input$Y] ~ dataset[,input$X])
    pred.y = coef(m3)[1] + coef(m3)[2]*input$newX
    plot(dataset[,input$X], dataset[,input$Y], 
         ylab = input$Y,
         xlab = input$X,
         main = paste("Relationship between", input$Y, "and", input$X)
    )
    abline(m3,
           col = "red",
           lwd = 1,
           lty=2)
    points(input$newX, pred.y, pch = 19, col = "red", cex = 2)
  })
}

shinyApp(ui = ui, server = server)
