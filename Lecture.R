## Shiny Lecture
install.packages("shiny")
library("shiny")
install.packages("EBMAforecast")
library("EBMAforecast")

### Let's start with an example
runExample("01_hello") # a histogram

### We can make our own example by modifying this.


#### More examples
runExample("02_text") # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg") # global variables
runExample("05_sliders") # slider bars
runExample("06_tabsets") # tabbed panels
runExample("07_widgets") # help text and submit buttons
runExample("08_html") # Shiny app built from HTML
runExample("09_upload") # file upload wizard
runExample("10_download") # file download wizard
runExample("11_timer") # an automated timer

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
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Campbell", "")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
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
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)