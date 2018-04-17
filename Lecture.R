## Shiny Lecture
install.packages("shiny")
library("shiny")
install.packages("EBMAforecast")
library("EBMAforecast")


## Let's build our own (This is due next Tuesday as a problem set)

#1 ) 
#### As our first step, we are going to make a UI that does nothing.  We are going to say:

# Presidential Forecasts

# Here are the results of presidential forecasts from 1952-2008
# (this shoudl be in a lower font)

#2)
## As our second step, we are going to follow example 2 above and have it show the last X elections (as selectd by the user)

#3) Now we are going to have it plot the election results 

# https://shiny.rstudio.com/reference/shiny/1.0.2/plotOutput.html

# 4) Now we are going to add a line to add a dropdown window to add a specific forecast to the plot

# 5) Now we are going to make it so it prints out the data points when clicked on

# https://shiny.rstudio.com/articles/plot-interaction.html

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Presidential Forecasts"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing model ----
      selectInput(inputId = "model",
                  label = "Choose a predictor model:",
                  choices = c("Campbell", "Campbell","Lewis-Beck","EWT2C2","Fair","Hibbs","Abramowitz")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "View the last how many elections?",
                   value = 10),
      
      ## Clairifying info
      helpText("Here are the results of presidential forecasts from 1952-2008")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view"),
      
      ##show Plot
      plotOutput("plot", click = "plot_click"),
      verbatimTextOutput("info")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
 data("presidentialForecast")
  output$view <- renderTable({presidentialForecast})
  # Return the requested dataset ----
  datasetInput <- reactive({
    presidentialForecast
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  ## Choose data set
  forecast <- reactive({
    switch(input$model,
           "Campbell" = presidentialForecast$Campbell,
           "Lewis-Beck" = presidentialForecast$`Lewis-Beck`,
           "EWT2C2" = presidentialForecast$EWT2C2,
           "Fair" = presidentialForecast$Fair,
           "Hibbs" = presidentialForecast$Hibbs,
           "Abramowitz" = presidentialForecast$Abramowitz)
  })
  
  ##Plot
  output$plot <- renderPlot({
    input$newplot
    
    plot(x=1:input$obs, y=presidentialForecast$Actual[1:input$obs], xlab = "Indexed Elections", ylab = "vote share of incumbent")
    points(x=1:input$obs, y=forecast()[1:input$obs], col = "dark green")
    legend(1,62, c("Actual","Model"), lty=c(3,3), col=c("dark green","black"))
    
  })
  
  ## printing clicked on points
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
