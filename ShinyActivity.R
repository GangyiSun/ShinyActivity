library(shiny)


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Presidential Forecast"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Include clarifying text ----
      helpText("Here are the results of presidential forecasts from 1952-2008"),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Last X elections to view:",
                   value = 15),
      
      # Input: Selector for choosing specific forecast to add to plot ----
      selectInput(inputId = "forecast",
                  label = "Choose a forecast to plot:",
                  choices = c("Actual","Campbell", "Lewis-Beck", "EWT2C2", "Fair",
                              "Hibbs", "Abramowitz"))

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Display table of data 
      tableOutput("view"),
      
      # Display plot
      plotOutput("plot", click="plot_click"),
      
      # Display x and y coordinates of clicked point 
      h4("Clicked points"),
      verbatimTextOutput("plot_clickinfo")

    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Loads dataset, sort in descending order 
  library(EBMAforecast)
  data("presidentialForecast")
  presidentialForecast$Year <- as.numeric(row.names(presidentialForecast))
  presidentialForecast<-presidentialForecast[order(presidentialForecast$Year,decreasing=TRUE), ]
  names(presidentialForecast)[2]<-"LewisBeck"
 
  # Generates table showing the last X elections (as selectd by the user)
  output$view <- renderTable({
    head(presidentialForecast, n=input$obs)
  })  
  
  # Plot the election results and a specific forecast
  output$plot <- renderPlot({
    if (input$forecast=="Actual"){
      plot(presidentialForecast$Actual, main="Plot of Election Results and Forecasts", 
           xlab="Year of Election", ylab="Vote Share, Percentage", xaxt='n', ylim=c(40, 70))
      axis(side=1, at = c(seq(1,15,1)), labels=c(seq(1952,2008,4)))
      legend("topright", inset=0.01, legend=c("Actual"), pch=c("o"), col=c("black"))
    } else if (input$forecast=="Campbell"){
      plot(presidentialForecast$Actual, main="Plot of Election Results and Forecasts", 
           xlab="Year of Election", ylab="Vote Share, Percentage", xaxt='n', ylim=c(40, 70))
      points(presidentialForecast$Campbell, pch="x", col="blue")
      axis(side=1, at = c(seq(1,15,1)), labels=c(seq(1952,2008,4)))
      legend("topright", inset=0.01, legend=c("Actual", "Campbell"), pch=c("o","x"), col=c("black", "blue"))
    } else if (input$forecast=="Lewis-Beck"){
      plot(presidentialForecast$Actual, main="Plot of Election Results and Forecasts", 
           xlab="Year of Election", ylab="Vote Share, Percentage", xaxt='n', ylim=c(40, 70))
      points(presidentialForecast$LewisBeck, pch="x", col="blue")
      axis(side=1, at = c(seq(1,15,1)), labels=c(seq(1952,2008,4)))
      legend("topright", inset=0.01, legend=c("Actual", "Lewis-Beck"), pch=c("o","x"), col=c("black", "blue"))
    } else if (input$forecast=="EWT2C2"){
      plot(presidentialForecast$Actual, main="Plot of Election Results and Forecasts", 
           xlab="Year of Election", ylab="Vote Share, Percentage", xaxt='n', ylim=c(40, 70))
      points(presidentialForecast$EWT2C2, pch="x", col="blue")
      axis(side=1, at = c(seq(1,15,1)), labels=c(seq(1952,2008,4)))
      legend("topright", inset=0.01, legend=c("Actual", "EWT2C2"), pch=c("o","x"), col=c("black", "blue"))
    } else if (input$forecast=="Fair"){
      plot(presidentialForecast$Actual, main="Plot of Election Results and Forecasts", 
           xlab="Year of Election", ylab="Vote Share, Percentage", xaxt='n', ylim=c(40, 70))
      points(presidentialForecast$Fair, pch="x", col="blue")
      axis(side=1, at = c(seq(1,15,1)), labels=c(seq(1952,2008,4)))
      legend("topright", inset=0.01, legend=c("Actual", "Fair"), pch=c("o","x"), col=c("black", "blue"))
    } else if (input$forecast=="Hibbs"){
      plot(presidentialForecast$Actual, main="Plot of Election Results and Forecasts", 
           xlab="Year of Election", ylab="Vote Share, Percentage", xaxt='n', ylim=c(40, 70))
      points(presidentialForecast$Hibbs, pch="x", col="blue")
      axis(side=1, at = c(seq(1,15,1)), labels=c(seq(1952,2008,4)))
      legend("topright", inset=0.01, legend=c("Actual", "Hibbs"), pch=c("o","x"), col=c("black", "blue"))
    } else if (input$forecast=="Abramowitz"){
      plot(presidentialForecast$Actual, main="Plot of Election Results and Forecasts", 
           xlab="Year of Election", ylab="Vote Share, Percentage", xaxt='n', ylim=c(40, 70))
      points(presidentialForecast$Abramowitz, pch="x", col="blue")
      axis(side=1, at = c(seq(1,15,1)), labels=c(seq(1952,2008,4)))
      legend("topright", inset=0.01, legend=c("Actual", "Abramowitz"), pch=c("o","x"), col=c("black", "blue"))
    } 
  })
  
  # Prints point clicked, x = year, y = vote share percentage 
  output$plot_clickinfo <- renderText({
    paste0("Year=", round(4*input$plot_click$x+1948), "\nVote Share, Percentage=", input$plot_click$y)
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
