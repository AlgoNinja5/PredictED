library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(fresh)
library(DBI)
library(RMySQL)
library(readxl)
library(jsonlite)


conn <- dbConnect(
  RMySQL::MySQL(),
  dbname = "evidyalvdb",
  host = "localhost",
  user = "root",
  password = "Manushiv25!"
)
# Fetch table names with backticks for spaces in names
query_tables <- "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = DATABASE()"
table_names <- dbGetQuery(conn, query_tables)$TABLE_NAME
table_names_with_backticks <- paste0("`", table_names, "`")

# Initialize a data frame to store results
row_counts_df <- data.frame(TableName = character(), RowCount = numeric(), stringsAsFactors = FALSE)

# Iterate over all tables
for (table_name in table_names_with_backticks) {
  # Construct and execute the query to get row count
  query <- paste0("SELECT COUNT(*) AS RowCount FROM ", table_name)
  result <- dbGetQuery(conn, query)
  
  # Append to data frame
  row_counts_df <- rbind(row_counts_df, data.frame(TableName = table_name, RowCount = result$RowCount))
}

# Filter out tables with RowCount == 0
non_empty_tables_df <- row_counts_df[row_counts_df$RowCount > 0, ]


# Custom theme
theme <- create_theme(
  bs4dash_color(
    lime = "#86BC25", # Sidebar color
    olive = "#86BC25",
    purple = "#4CAF50"
  ),
  bs4dash_status(
    primary = "#000000", # Black background for body
    info = "#000000"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFFFFF", # White text
    text_light = "#FFFFFF"
  )
)

# Custom CSS with Background Animation
custom_css <- "
@keyframes backgroundAnimation {
  0% { background-color: #4CAF50; }
  50% { background-color: #FFEB3B; }
  100% { background-color: #4CAF50; }
}

body {
  background-color: #000000 !important; /* Black background for body */
  animation: backgroundAnimation 10s infinite;
  color: #000000 !important; /* White text */
}

.navbar, .control-sidebar, .main-header, .main-sidebar {
  background-color: #000000 !important; /* Sidebar and control bar color */
}

h1, h2, h3, h4, h5, h6, .info-box, .info-box-icon, .info-box-text, .info-box-number, .nav-sidebar a {
  color: #FFFFFF !important; /* White text */
}

footer {
  background-color: #86BC25 !important; /* Footer color matches the sidebar */
  color: white !important; /* White text in footer */
}

.jumbotron {
  height: 100vh !important; /* Full height of the viewport */
  display: flex;
  align-items: center;
  justify-content: center;
  text-align: center;
  background-size: cover;
  background-position: center;
}


.header-image {
  width: 60%;
  height: 30px; /* You can adjust the height as needed */
  object-fit: cover; /* Ensure the image auto-fits the space */
}

.jumbotron h1, .jumbotron p {
  color: #FFFFFF !important; /* Ensure the text color is white */
}
"





# UI
ui <- dashboardPage(
  freshTheme = theme,
  dark = FALSE,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  
  # Header
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      HTML('<img src="Delo.jpg" class="header-image">')
    ),
    controlbarIcon = icon("circle-info"),
    fixed = TRUE
  ),
  
  # Sidebar
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Get Data", tabName = "get_data", icon = icon("upload")),
      menuItem("SQL Connection", tabName = "sql", icon = icon("database")),
      menuItem("Run Analysis", tabName = "run_analysis", icon = icon("cogs")),
      menuItem("PredicteD.", tabName = "results", icon = icon("chart-line"))
    )
  ),
    
  
  # Body
  body = dashboardBody(
    tags$style(HTML(custom_css)), # Apply custom CSS
    tabItems(
      # Home tab
      tabItem(
        tabName = "home",
        jumbotron(
          HTML('<video autoplay loop muted>
                <source src="E.mp4" type="video/mp4">
                Your browser does not support the video tag.
               </video>')
        )
      ),
      
      # Get Data tab
      tabItem(
        tabName = "get_data",
        fluidRow(
          box(
            title = "Upload Data", 
            width = 6, 
            fileInput("file", "Upload CSV, Excel, Text, or JSON file", 
                      accept = c(
                        ".csv", ".xlsx", ".xls",  # Excel and CSV files
                        ".txt",                  # Text files
                        ".json",                 # JSON files
                        ".tsv"                 # Tab-separated values
                       
                      )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Data Preview",
            width = 12,
            DTOutput("dataTable")  # Data table output
          )
        )
      ),
      tabItem(
        tabName = "sql",
        fluidRow(
          box(
            title = "Select a Table", 
            width = 6,
            selectInput("table", "Select a Table:", choices = non_empty_tables_df$TableName)  # Choices will be populated dynamically
          ),
        
        fluidRow(
          box(
            title = "Data Preview",
            width = 12,
            DTOutput("table_preview")  # Data table output for selected table data
          )
        )
      ),
      
      tabItem(
        tabName = "run_analysis",
        fluidRow(
          box(title = "Select Analysis Options", width = 12,
              selectInput("algorithm", "Select Algorithm",
                          choices = c("Linear Regression", "Clustering", "Decision Tree","Random Forest", "Classification")),
              uiOutput("target_var"),
              uiOutput("predictor_vars"),
              actionButton("run", "Run Analysis"),
              actionButton("accuracy", "Show Model Accuracy"),
              uiOutput("manual_input_ui"),
              actionButton("predict", "Predict")
            )
          )
        ),
      tabItem(tabName = "results",
              fluidRow(
                box(title = "Model Summary", width = 12, verbatimTextOutput("summary")),
                box(title = "Model Accuracy", width = 12, verbatimTextOutput("model_accuracy")),
                box(title = "Predictions", width = 12, tableOutput("predictions")),
                box(title = "Manual Prediction", width = 12, verbatimTextOutput("manual_prediction"))
              ),
              fluidRow(
                box(title = "Main Plot", width = 12, plotOutput("plot")),
                box(title = "Additional Plot 1", width = 6, plotOutput("additional_plot1")),
                box(title = "Additional Plot 2", width = 6, plotOutput("additional_plot2"))
              )
      )
      )
    )
  )
)


# Server
server <- function(input, output, session) { 
  
  data <- reactive({
    req(input$file)  # Ensure a file is uploaded
    
    # Get the file extension
    ext <- tools::file_ext(input$file$name)
    
    # Depending on the file extension, read the file accordingly
    switch(ext,
           csv = read.csv(input$file$datapath),
           tsv = read.delim(input$file$datapath),
           xlsx = readxl::read_excel(input$file$datapath),
           xls = readxl::read_excel(input$file$datapath),
           txt = read.table(input$file$datapath, header = TRUE, sep = "\t"),
           json = jsonlite::fromJSON(input$file$datapath),
           stop("Unsupported file type")
    )
  })
  
  # Render the data table in the UI after data is uploaded
  output$dataTable <- renderDT({
    req(data())  # Ensure that the data is available
    datatable(data())  # Show the uploaded data as a table
  })
  
  
  # Update the table selection based on non-empty tables
  observe({
    updateSelectInput(session, "table", choices = non_empty_tables_df$TableName)
  })
  
  # Render the preview of the selected table
  output$table_preview <- renderDT({
    req(input$table)  # Ensure a table is selected
    query <- paste0("SELECT * FROM ", input$table, " LIMIT 5000")
    datatable(dbGetQuery(conn, query))
  })
  
  output$target_var <- renderUI({
    req(non_empty_tables_df)
    selectInput("target", "Select Target Variable", choices = names(non_empty_tables_df))
  })
  
  output$predictor_vars <- renderUI({
    req(non_empty_tables_df)
    selectInput("predictors", "Select Predictor Variables", choices = names(data()), multiple = TRUE)
  })
  
  model <- reactiveVal(NULL)
  
  output$summary <- renderPrint({
    req(input$run)
    isolate({
      df <- data()
      target <- input$target
      predictors <- input$predictors
      algo <- input$algorithm
      
      if (is.null(target) || is.null(predictors)) {
        return("Please select both target and predictor variables.")
      }
      
      df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      
      formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
      
      model_result <- switch(algo,
                             "Linear Regression" = lm(formula, data = df),
                             "Clustering" = {
                               df_scaled <- scale(df %>% select(all_of(predictors)))
                               kmeans(df_scaled, centers = 3)
                             },
                             "Decision Tree" = rpart(formula, data = df),
                             "Random Forest" = randomForest(formula, data = df),
                             "Classification" = train(formula, data = df, method = "rpart"),
                       
      )
      model(model_result)
      if (algo == "Clustering") {
        model_result
      } else {
        summary(model_result)
      }
    })
  })
  
  output$plot <- renderPlot({
    req(input$run)
    isolate({
      df <- data()
      target <- input$target
      predictors <- input$predictors
      algo <- input$algorithm
      
      if (is.null(target) || is.null(predictors)) {
        return(NULL)
      }
      
      df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      
      formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
      
      plot_result <- switch(algo,
                            "Linear Regression" = {
                              model <- lm(formula, data = df)
                              ggplot(df, aes_string(x = target, y = predict(model))) +
                                geom_point(color = "blue") +
                                geom_smooth(method = "lm", color = "red") +
                                labs(title = "Linear Regression", x = target, y = "Predicted")
                            },
                            "Clustering" = {
                              df_scaled <- scale(df %>% select(all_of(predictors)))
                              kmeans_result <- kmeans(df_scaled, centers = 3)
                              clusplot(df_scaled, kmeans_result$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
                            },
                            "Decision Tree" = {
                              model <- rpart(formula, data = df)
                              rpart.plot(model, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)
                            },
                            
                        
                            "Random Forest" = {
                              model <- randomForest(formula, data = df)
                              ggplot(df, aes_string(x = target, y = predict(model))) +
                                geom_point(color = "blue") +
                                labs(title = "Random Forest", x = target, y = "Predicted")
                            },
                            "Classification" = {
                              model <- train(formula, data = df, method = "rpart")
                              rpart.plot(model$finalModel, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)
                            }
                            
      )
      plot_result
    })
  })
  
  output$model_accuracy <- renderPrint({
    req(input$accuracy)
    isolate({
      df <- data()
      target <- df[[input$target]]
      predictors <- input$predictors
      algo <- input$algorithm
      
      if (is.null(target) || is.null(predictors)) {
        return("Please select both target and predictor variables.")
      }
      
      df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      
      formula <- as.formula(paste(input$target, "~", paste(predictors, collapse = "+")))
      model <- model()
      accuracy <- switch(algo,
                         "Linear Regression" = {
                           pred <- predict(model, newdata = df)
                           100 - mean(abs(pred - target) / target * 100)  # Accuracy as 100% - MAPE
                         },
                         "Decision Tree" = {
                           pred <- predict(model, newdata = df, type = "class")
                           accuracy <- sum(pred == target) / length(target)
                           accuracy * 100  # Convert to percentage
                         },
                         "Neural Networks" = {
                           pred <- predict(model, newdata = df)
                           100 - mean(abs(pred - target) / target * 100)  # Accuracy as 100% - MAPE
                         },
                         "Random Forest" = {
                           pred <- predict(model, newdata = df)
                           accuracy <- sum(pred == target) / length(target)
                           accuracy * 100  # Convert to percentage
                         },
                         "Classification" = {
                           pred <- predict(model, newdata = df)
                           accuracy <- sum(pred == target) / length(target)
                           accuracy * 100  # Convert to percentage
                         },
                         "Naive Bayes" = {
                           pred <- predict(model, newdata = df)
                           accuracy <- sum(pred == target) / length(target)
                           accuracy * 100  # Convert to percentage
                         }
      )
      return(paste("Model Accuracy:", round(accuracy, 2), "%"))
    })
  })
  
  output$manual_input_ui <- renderUI({
    req(input$predictors)
    predictors <- input$predictors
    
    lapply(predictors, function(pred) {
      numericInput(pred, label = paste("Input value for", pred), value = 0)
    })
  })
  
  output$manual_prediction <- renderPrint({
    req(input$predict)
    isolate({
      predictors <- input$predictors
      if (is.null(predictors)) {
        return("Please select predictor variables.")
      }
      
      new_data <- data.frame(matrix(ncol = length(predictors), nrow = 1))
      colnames(new_data) <- predictors
      
      for (pred in predictors) {
        new_data[[pred]] <- input[[pred]]
      }
      
      model <- model()
      if (is.null(model)) {
        return("No model has been created. Please run the analysis first.")
      }
      
      prediction <- predict(model, newdata = new_data)
      paste("Predicted value:", prediction)
    })
  })
  
  output$additional_plot1 <- renderPlot({
    req(input$run)
    df <- data()
    target <- input$target
    predictors <- input$predictors
    algo <- input$algorithm
    
    if (algo == "Clustering") {
      df_scaled <- scale(df %>% select(all_of(predictors)))
      kmeans_result <- kmeans(df_scaled, centers = 3)
      df$cluster <- as.factor(kmeans_result$cluster)
      ggplot(df, aes_string(x = predictors[1], y = predictors[2], color = "cluster")) +
        geom_point() +
        labs(title = "Clustering Visualization")
    } else if (algo == "Outliers") {
      ggplot(df, aes_string(y = target)) +
        geom_boxplot(outlier.color = "red", fill = "lightblue") +
        labs(title = "Outliers Visualization")
    }
  })
  
  output$additional_plot2 <- renderPlot({
    req(input$run)
    df <- data()
    target <- input$target
    predictors <- input$predictors
    algo <- input$algorithm
    
    if (algo == "Clustering") {
      df_scaled <- scale(df %>% select(all_of(predictors)))
      kmeans_result <- kmeans(df_scaled, centers = 3)
      df$cluster <- as.factor(kmeans_result$cluster)
      pairs(df[predictors], col = df$cluster, main = "Clustering Pair Plot")
    }
  })
  
  output$map <- renderLeaflet({
    req(input$file)
    df <- read.csv(input$file$datapath)
    leaflet(df) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, popup = ~as.character(city))
  })

# Disconnect the database when the app is closed
  onStop(function() {
    dbDisconnect(conn)
  })
}

shinyApp(ui, server)

