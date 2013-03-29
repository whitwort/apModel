library(shiny)
library(ggplot2)
library(reshape)

# Load model into the local environment
source("model.R", local = TRUE)

# Define server logic required to generate the plot
shinyServer(function(input, output) {
  
  #Session store is a reactive values ~list
  store              <- reactiveValues()
  store$summaryData  <- data.frame()
  
  # Capture input variables in a reactive expression
  runArgs   <- reactive({
    
    # Bind initial state and parameter inputs    
    return(list( 
        state = vapply( names(state)
                      , function(name) { as.numeric(input[[name]]) }
                      , FUN.VALUE = numeric(1)
                      )
        
      , parameters = vapply( names(parameters)
                           , function(name) { as.numeric(input[[name]]) }
                           , FUN.VALUE = numeric(1)
                           )
        
      ))
  })
  
  # Run the model in a reactive expression
  runModel  <- reactive({
    
    args  <- runArgs()
    
    # Simulation time depth
    depth <- (time["end"] - time["start"]) / time["step"]
    
    # Run the simulation; convert result to a data.frame    
    result <- data.frame(solver(
        y     = args$state
      , times = seq(time["start"], input$time.end, by = abs(input$time.end - time["start"]) / depth)
      , func  = model
      , parms = args$parameters
    ))
    
    return(result)
    
  })
  
  # Simulation plot
  output$voltagePlot <- renderPlot({
    
    p <- ggplot(melt(runModel()[,c("time","v")], id = "time")) +
      geom_line( aes(time, value, colour = variable) )  +
      ylab("mV")                                
    
    print(p)
    
  })
  output$conductancePlot <- renderPlot({
    
    p <- ggplot(melt(runModel()[,c("time","m","h","n")], id = "time")) +
          geom_line( aes(time, value, colour = variable) )  +
          ylab("[variable]")                                
    
    print(p)
    
  })
  
})