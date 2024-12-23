# Install packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")
if (!requireNamespace("nnet", quietly = TRUE)) install.packages("nnet")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")

# Load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(rpart.plot)
library(nnet)
library(randomForest)
library(caret)
library(e1071)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Predictive Analytics Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")),
      menuItem("Run Analysis", tabName = "run_analysis", icon = icon("cogs")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Welcome to the Predictive Analytics Dashboard"),
              p("This dashboard allows you to upload data, run predictive models, and visualize results.")
      ),
      tabItem(tabName = "upload_data",
              fluidRow(
                box(title = "Upload CSV File", width = 6, fileInput("file", "Upload CSV File", accept = ".csv")),
                box(title = "Upload Logo Image", width = 6, fileInput("image", "Upload Logo Image", accept = c('image/png', 'image/jpeg')))
              )
      ),
      tabItem(tabName = "run_analysis",
              fluidRow(
                box(title = "Select Analysis Options", width = 12,
                    selectInput("algorithm", "Select Algorithm",
                                choices = c("Linear Regression", "Clustering", "Decision Tree", "Outliers", "Neural Networks", "Random Forest", "Classification", "Naive Bayes")),
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
      ),
      tabItem(tabName = "map",
              leafletOutput("map", height = 500)
      )
    ),
    tags$head(
      tags$style(HTML("
                      /* Custom CSS */
                      .skin-blue .main-header .logo {
                        background-color: #0033A0;
                        color: #FFFFFF;
                        border-bottom: 0 solid transparent;
                      }
                      .skin-blue .main-header .navbar {
                        background-color: #0033A0;
                      }
                      .skin-blue .main-sidebar {
                        background-color: #1F3A93;
                      }
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
                        background-color: #0033A0;
                        color: #FFFFFF;
                      }
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
                        background-color: #1F3A93;
                        color: #FFFFFF;
                      }
                      .content-wrapper, .right-side {
                        background-color: #F4F6F9;
                      }
                      .box.box-solid.box-primary {
                        border-top-color: #0033A0;
                      }
                      .box.box-primary {
                        border-top-color: #0033A0;
                      }
                      "))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  observe({
    req(input$image)
    img_path <- input$image$datapath
    update_logo(img_path)
  })
  
  update_logo <- function(img_path) {
    session$sendCustomMessage(type = "update_logo", message = list(src = img_path))
  }
  
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- df %>% mutate_if(is.character, as.factor)
    return(df)
  })
  
  output$target_var <- renderUI({
    req(data())
    selectInput("target", "Select Target Variable", choices = names(data()))
  })
  
  output$predictor_vars <- renderUI({
    req(data())
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
                             "Outliers" = boxplot.stats(df[[target]])$out,
                             "Neural Networks" = nnet(formula, data = df, size = 5, linout = TRUE),
                             "Random Forest" = randomForest(formula, data = df),
                             "Classification" = train(formula, data = df, method = "rpart"),
                             "Naive Bayes" = naiveBayes(formula, data = df)
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
                            "Outliers" = {
                              boxplot(df[[target]], main = "Outliers", ylab = target, col = "lightblue")
                            },
                            "Neural Networks" = {
                              model <- nnet(formula, data = df, size = 5, linout = TRUE)
                              ggplot(df, aes_string(x = target, y = predict(model))) +
                                geom_point(color = "blue") +
                                labs(title = "Neural Networks", x = target, y = "Predicted")
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
                            },
                            "Naive Bayes" = {
                              model <- naiveBayes(formula, data = df)
                              ggplot(df, aes_string(x = target, y = predict(model))) +
                                geom_point(color = "blue") +
                                labs(title = "Naive Bayes", x = target, y = "Predicted")
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
}

# Custom JavaScript to update the logo
js <- '
Shiny.addCustomMessageHandler("update_logo", function(message) {
  document.getElementById("logo_img").src = message.src;
});
'

# Include JavaScript in the Shiny app
ui <- tagList(
  tags$head(
    tags$script(HTML(js))
  ),
  ui
)

# Run the application 
shinyApp(ui = ui, server = server)
