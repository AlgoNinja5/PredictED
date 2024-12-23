library(shiny)
library(ggplot2)
library(readxl)
library(DBI)
library(RSQLite)
library(DT)
library(caret)
library(e1071) 


# For SVM

# Define the UI
ui <- fluidPage(
  titlePanel("Upload File and Create ggplot2 Charts"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", "Choose Data Source",
                   choices = c("CSV" = "csv", "Excel" = "excel", "Database" = "db")),
      conditionalPanel(
        condition = "input.data_source == 'csv'",
        fileInput("file_csv", "Choose CSV File", accept = ".csv")
      ),
      conditionalPanel(
        condition = "input.data_source == 'excel'",
        fileInput("file_excel", "Choose Excel File", accept = ".xlsx")
      ),
      conditionalPanel(
        condition = "input.data_source == 'db'",
        textInput("db_path", "Database Path", value = "my_database.sqlite"),
        textInput("db_query", "SQL Query", value = "SELECT * FROM my_table")
      ),
      actionButton("load_data", "Load Data"),
      hr(),
      h4("Edit Column Names"),
      uiOutput("column_edit"),
      actionButton("update_names", "Update Column Names"),
      hr(),
      uiOutput("choose_x_header"),
      uiOutput("choose_y_header"),
      hr(),
      selectInput("independent_var", "Select Independent Variable", ""),
      selectInput("dependent_var", "Select Dependent Variable", ""),
      hr(),
      radioButtons("ml_model", "Select Machine Learning Model",
                   choices = c("Random Forest", "KNN", "K Mean Cluster", "SVM")),
      numericInput("dependent_value", "Enter Dependent Variable Value", value = 0),
      actionButton("predict_value", "Predict Independent Variable")
    ),
    mainPanel(
      DTOutput("contents"),
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatter_plot")),
        tabPanel("Bar Plot", plotOutput("bar_plot")),
        tabPanel("Line Plot", plotOutput("line_plot")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Boxplot", plotOutput("boxplot")),
        tabPanel("Predictive Assessment", dataTableOutput("ml_results"))
      )
    )
  )
)


# Define the server logic
server <- function(input, output, session) {
  # Reactive value to store the data
  rv <- reactiveValues(data = NULL, edited_data = NULL)
  
  observeEvent(input$load_data, {
    if (input$data_source == "csv") {
      req(input$file_csv)
      rv$data <- read.csv(input$file_csv$datapath)
    } else if (input$data_source == "excel") {
      req(input$file_excel)
      rv$data <- read_excel(input$file_excel$datapath)
    } else if (input$data_source == "db") {
      req(input$db_path, input$db_query)
      con <- dbConnect(RSQLite::SQLite(), input$db_path)
      rv$data <- dbGetQuery(con, input$db_query)
      dbDisconnect(con)
    }
    rv$edited_data <- rv$data
    updateSelectInput(session, "independent_var", choices = colnames(rv$data))
    updateSelectInput(session, "dependent_var", choices = colnames(rv$data))
  })
  
  output$contents <- renderDT({
    req(rv$data)
    datatable(rv$edited_data, editable = "column")
  })
  
  # UI for editing column names
  output$column_edit <- renderUI({
    req(rv$data)
    lapply(names(rv$data), function(col) {
      textInput(paste0("edit_", col), col, col)
    })
  })
  
  observeEvent(input$update_names, {
    req(rv$data)
    new_names <- sapply(names(rv$data), function(col) {
      input[[paste0("edit_", col)]]
    })
    
    # Clean the new column names
    new_names <- make.names(new_names, unique = TRUE)
    
    colnames(rv$edited_data) <- new_names
  })
  
  output$choose_x_header <- renderUI({
    req(rv$edited_data)
    colnames <- names(rv$edited_data)
    selectInput("xcol", "X-axis Variable", colnames)
  })
  
  output$choose_y_header <- renderUI({
    req(rv$edited_data)
    colnames <- names(rv$edited_data)
    selectInput("ycol", "Y-axis Variable", colnames, selected = NULL)
  })
  
  # Run Machine Learning Models
  observeEvent(input$predict_value, {
    req(input$independent_var, input$dependent_var, input$dependent_value)
    
    # Implement the machine learning model based on user selection
    # This part is where you would write the code to train and predict using the selected ML model
    # Depending on the model chosen, you would call the appropriate functions from caret or other ML libraries
    # For SVM, you can use e1071 package
    # For Random Forest, KNN, etc., you would use the respective functions
    
    # The specific implementation of the ML models is not provided in the code snippet shared
    
    # After running the model, you can display the results in the "ml_results" DataTableOutput
  })
}

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Upload CSV file"),
      h4("Drag and drop to upload data"),
      actionButton("run_analysis", "Run Predictive Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary", verbatimTextOutput("summary_text")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot")),
        tabPanel("Bar Plot", plotOutput("bar_plot")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Box Plot", plotOutput("box_plot")),
        tabPanel("Predictive Analysis", verbatimTextOutput("prediction_result"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file_upload)
    read.csv(input$file_upload$datapath)
  })
  
  output$summary_text <- renderPrint({
    summary(data())
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(data(), aes(x = Sepal.Length, y = Sepal.Width)) + 
      geom_point()
  })
  
  output$bar_plot <- renderPlot({
    ggplot(data(), aes(x = Species)) + 
      geom_bar()
  })
  
  output$histogram <- renderPlot({
    ggplot(data(), aes(x = Sepal.Length)) + 
      geom_histogram()
  })
  
  output$box_plot <- renderPlot({
    ggplot(data(), aes(x = Species, y = Sepal.Width)) + 
      geom_boxplot()
  })
  
  observeEvent(input$run_analysis, {
    prediction <- lm(Sepal.Length ~ Sepal.Width, data = data())
    output$prediction_result <- renderPrint({
      summary(prediction)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)


