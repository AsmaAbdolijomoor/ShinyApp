library(shiny)
library(ggplot2)
library(caret)
library(dplyr)
library(plotly)
library(randomForest)
# install.packages("pROC")  # If not already installed
library(pROC)

# Load and prepare the data
data <- read.csv("data.csv")  # Replace with your actual file path
data <- data %>% select(-X)
data$diagnosis <- as.factor(data$diagnosis)  # Ensure diagnosis is a factor

# Preprocess the Data
pca_data <- data %>%
  select(-id, -diagnosis) %>%
  scale()  # Standardize the data (z-score normalization)

# Perform PCA
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_result)

data <- data %>% select(-id)  # Remove the 'id' column for modeling

# Train a Random Forest model
set.seed(123)
trainIndex <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]
model <- train(diagnosis ~ ., data = trainData, method = "rf")

# Define the UI (User Interface)
ui <- fluidPage(
  titlePanel("Breast Cancer Diagnosis Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Tumor Characteristics"),
      
      # Inputs for prediction
      numericInput("radius", "Radius Mean", value = 14, min = 0, max = 30, step = 0.1),
      numericInput("texture", "Texture Mean", value = 20, min = 0, max = 30, step = 0.1),
      numericInput("perimeter", "Perimeter Mean", value = 90, min = 0, max = 200, step = 0.1),
      numericInput("area", "Area Mean", value = 600, min = 0, max = 5000, step = 1),
      actionButton("predict_btn", "Predict"),
      
      hr(),
      h4("Model Prediction"),
      verbatimTextOutput("prediction")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualizations", 
                 plotOutput("histogram_radius"),
                 plotOutput("histogram_perimeter"),
                 plotOutput("histogram_texture"),
                 plotOutput("histogram_area"),
                 plotOutput("boxplot_radius"),
                 plotOutput("boxplot_perimeter"),
                 plotOutput("boxplot_texture"),
                 plotOutput("boxplot_area"),
                 plotOutput("pair_plot"),
                 plotOutput("diagnosis_count_bar"),
                 plotOutput("pca_scree_plot"),
                 plotOutput("pca_scatter_plot"),
                 plotOutput("importance_plot")),  # Feature importance plot
        tabPanel("Correlation Heatmap", plotOutput("correlation_heatmap")),
        tabPanel("Model Performance", 
                 plotlyOutput("model_perf"),
                 plotOutput("roc_curve"))
      )
    )
  )
)



server <- function(input, output) {
  observeEvent(input$predict_btn, {
    # Collect all input features dynamically
    new_data <- data.frame(
      radius_mean = as.numeric(input$radius),
      texture_mean = as.numeric(input$texture),
      perimeter_mean = as.numeric(input$perimeter),
      area_mean = as.numeric(input$area),
      smoothness_mean = as.numeric(input$smoothness),
      compactness_mean = as.numeric(input$compactness),
      concavity_mean = as.numeric(input$concavity),
      concave_points_mean = as.numeric(input$concave_points),
      symmetry_mean = as.numeric(input$symmetry),
      fractal_dimension_mean = as.numeric(input$fractal_dimension)
    )
    
    # Predict using the updated model
    prediction <- predict(model, new_data)
    
    # Render prediction result
    output$prediction <- renderText({
      paste("Prediction: ", prediction)
    })
  })

  output$prediction <- renderPrint({
    req(input$smoothness_mean)  # Check if input exists
    
    # Create a new data frame for prediction
    new_data <- data.frame(
      smoothness_mean = as.numeric(input$smoothness_mean),
      radius_mean = as.numeric(input$radius_mean),
      # Add all other required variables here...
    )
    
    # Check if all required columns are present
    print(str(new_data))  # Debugging purpose
    
    # Make prediction
    predict(model, new_data)
  })
  
  # Histogram: Distribution of Radius Mean
  output$histogram_radius <- renderPlot({
    ggplot(data, aes(x = radius_mean)) +
      geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
      labs(title = "Histogram of Radius Mean", x = "Radius Mean", y = "Frequency") +
      theme_minimal()
  })
  
  # Histogram: Distribution of perimeter Mean
  output$histogram_perimeter <- renderPlot({
    ggplot(data, aes(x = perimeter_mean)) +
      geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
      labs(title = "Histogram of primeter Mean", x = "perimeter Mean", y = "Frequency") +
      theme_minimal()
  })
  
  # Histogram: Distribution of texture Mean
  output$histogram_texture <- renderPlot({
    ggplot(data, aes(x = texture_mean)) +
      geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
      labs(title = "Histogram of texture Mean", x = "texture mean", y = "Frequency") +
      theme_minimal()
  })
  
  # Histogram: Distribution of area Mean
  output$histogram_area <- renderPlot({
    ggplot(data, aes(x = area_mean)) +
      geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
      labs(title = "Histogram of area Mean", x = "area mean", y = "Frequency") +
      theme_minimal()
  })
  
  # Boxplot: radius Mean by Diagnosis
  output$boxplot_radius <- renderPlot({
    ggplot(data, aes(x = diagnosis, y = radius_mean, fill = diagnosis)) +
      geom_boxplot() +
      labs(title = "Boxplot of radius Mean by Diagnosis", x = "Diagnosis", y = "radius Mean") +
      scale_fill_manual(values = c("M" = "red", "B" = "green")) +
      theme_minimal()
  })
  
  # Boxplot: perimeter Mean by Diagnosis
  output$boxplot_perimeter <- renderPlot({
    ggplot(data, aes(x = diagnosis, y = perimeter_mean, fill = diagnosis)) +
      geom_boxplot() +
      labs(title = "Boxplot of Perimeter Mean by Diagnosis", x = "Diagnosis", y = "Perimeter Mean") +
      scale_fill_manual(values = c("M" = "red", "B" = "green")) +
      theme_minimal()
  })
  
  # Boxplot: Texture Mean by Diagnosis
  output$boxplot_texture <- renderPlot({
    ggplot(data, aes(x = diagnosis, y = texture_mean, fill = diagnosis)) +
      geom_boxplot() +
      labs(title = "Boxplot of texture Mean by Diagnosis", x = "Diagnosis", y = "Texture Mean") +
      scale_fill_manual(values = c("M" = "red", "B" = "green")) +
      theme_minimal()
  })
  
  # Boxplot: Area Mean by Diagnosis
  output$boxplot_area <- renderPlot({
    ggplot(data, aes(x = diagnosis, y = area_mean, fill = diagnosis)) +
      geom_boxplot() +
      labs(title = "Boxplot of Area Mean by Diagnosis", x = "Diagnosis", y = "Area Mean") +
      scale_fill_manual(values = c("M" = "red", "B" = "green")) +
      theme_minimal()
  })
  
  # Pair Plot: Relationships between numeric features
  output$pair_plot <- renderPlot({
    selected_data <- data %>%
      select(radius_mean, texture_mean, perimeter_mean, area_mean)
    
    pairs(selected_data, 
          col = ifelse(data$diagnosis == "M", "red", "green"),
          pch = 16,
          main = "Pair Plot of Numeric Features")
  })
  
  # Bar Plot: Count of Malignant vs Benign Tumors
  output$diagnosis_count_bar <- renderPlot({
    ggplot(data, aes(x = diagnosis)) +
      geom_bar(fill = c("red", "green")) +
      labs(title = "Count of Malignant vs Benign Tumors", x = "Diagnosis", y = "Count") +
      scale_x_discrete(labels = c("B" = "Benign", "M" = "Malignant")) +
      theme_minimal()
  })
  
  # Bar Plot: Explained Variance (Scree Plot)
  output$pca_scree_plot <- renderPlot({
    pca_variance <- summary(pca_result)$importance[2, ]
    barplot(pca_variance, 
            main = "Scree Plot - PCA Explained Variance",
            xlab = "Principal Components", 
            ylab = "Proportion of Variance",
            col = "lightblue", 
            border = "black")
  })
  
  # Scatter Plot: First Two Principal Components
  output$pca_scatter_plot <- renderPlot({
    pca_data_scores <- data.frame(pca_result$x)  # Get the scores
    ggplot(pca_data_scores, aes(x = PC1, y = PC2)) +
      geom_point(aes(color = data$diagnosis), alpha = 0.6) +
      labs(title = "PCA: First Two Principal Components",
           x = "Principal Component 1", y = "Principal Component 2") +
      theme_minimal() +
      scale_color_manual(values = c("M" = "red", "B" = "green"))
  })
  
  # Heatmap: Correlation between features and diagnosis
  output$correlation_heatmap <- renderPlot({
    # Convert diagnosis to numeric for correlation
    data$diagnosis_numeric <- ifelse(data$diagnosis == "M", 1, 0)
    
    # Compute correlations
    correlation_matrix <- cor(data[, sapply(data, is.numeric)])
    
    # Convert to long format for ggplot
    correlation_df <- as.data.frame(as.table(correlation_matrix))
    colnames(correlation_df) <- c("Feature1", "Feature2", "Correlation")
    
    # Create the heatmap
    ggplot(correlation_df, aes(x = Feature1, y = Feature2, fill = Correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Feature Correlation Heatmap", fill = "Correlation")
  })
  
  # Feature Importance Visualization
  output$importance_plot <- renderPlot({
    varImp <- varImp(model, scale = FALSE)
    varImp_df <- as.data.frame(varImp$importance)
    
    ggplot(varImp_df, aes(x = reorder(rownames(varImp_df), Overall), y = Overall)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Feature Importance for Breast Cancer Diagnosis",
           x = "Features", y = "Importance Score") +
      theme_minimal()
  })
  
  output$model_perf <- renderPlotly({
    # Ensure testData has levels matching the model
    pred <- predict(model, testData)
    pred <- factor(pred, levels = levels(testData$diagnosis))
    confusion <- confusionMatrix(pred, testData$diagnosis)
    
    cm_df <- as.data.frame(as.table(confusion$table))
    colnames(cm_df) <- c("Prediction", "Reference", "n")
    
    # Ensure no NULL or invalid position argument
    plot_ly(
      data = cm_df,
      x = ~Prediction,
      y = ~Reference,
      z = ~n,
      type = 'heatmap',
      colors = "Blues"
    ) %>% layout(
      title = "Confusion Matrix",
      xaxis = list(title = "Prediction"),
      yaxis = list(title = "Reference")
    )
  })
  
  # ROC Curve
  output$roc_curve <- renderPlot({
    prob <- predict(model, testData, type = "prob")
    roc_curve <- roc(testData$diagnosis, prob$M)
    plot(roc_curve, main = "ROC Curve for Breast Cancer Diagnosis Model")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
