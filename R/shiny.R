library(shiny)
library(pROC)
library(NemoFunctionPackage)

# Load data
mydata = read.csv('https://9a445b17-559b-4189-8cb1-84cfa56a0745.usrfiles.com/ugd/9a445b_298a3200cec14e9cbb2e07eec2b21c69.csv')

# UI
GUI = fluidPage(
  titlePanel("Prediction of Observed Issues in College Students (Logistic Regression)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("trainProp", "Training Proportion (%)", 
                  min = 50, max = 90, value = 75),
      sliderInput("threshold", "Probability Threshold for Predicting 'Yes' (Observed Issues)", 
                  min = 0.1, max = 0.9, value = 0.5, step = 0.05)
    ),
    
    mainPanel(
      verbatimTextOutput("accuracyText"),
      plotOutput("rocPlot"),
      verbatimTextOutput("aucText"),
      plotOutput("importancePlot")
    )
  )
)

# Server
myserver = function(input, output) {
  
  # Reactive data split
  data_split = reactive({
    # Prepare data
    df = mydata[, c("age", "current_gpa", "financial_status", "social_circle_satisfaction", "observed_issues")]
    
    # Convert outcome to binary
    df$Y = ifelse(df$observed_issues == "Yes", 1, 0)
    
    # Convert categorical predictors to factors
    df$financial_status = factor(df$financial_status)
    df$social_circle_satisfaction = factor(df$social_circle_satisfaction)
    
    # Build design matrix
    X = model.matrix(~ age + current_gpa + financial_status + social_circle_satisfaction, data = df)[, -1]
    Y = df$Y
    
    # Remove missing values
    valid = complete.cases(X, Y)
    X = X[valid, , drop = FALSE]
    Y = Y[valid]
    
    # Split data
    tetr(X, Y, p = input$trainProp / 100, seed = 123)
  })
  
  # Logistic regression model
  model = reactive({
    d = data_split()
    mylogistic(d$Xtraining, d$Ytraining)
  })
  
  # Predicted probabilities on test set
  preds = reactive({
    model()$logi.predict(data_split()$Xtesting)
  })
  
  # Convert probabilities to binary predictions
  binary_preds = reactive({
    as.numeric(preds() >= input$threshold)
  })
  
  # Ground truth binary outcomes
  binary_truth = reactive({
    as.numeric(data_split()$Ytesting)
  })
  
  # Output: accuracy
  output$accuracyText = renderPrint({
    acc = mean(binary_preds() == binary_truth())
    paste("Prediction Accuracy:", round(acc * 100, 2), "%")
  })
  
  # Output: ROC curve
  output$rocPlot = renderPlot({
    roc_obj = roc(binary_truth(), preds())
    plot(roc_obj, col = "darkred", main = "ROC Curve")
  })
  
  # Output: AUC
  output$aucText = renderPrint({
    auc_val = auc(binary_truth(), preds())
    paste("AUC:", round(auc_val, 4))
  })
  
  # Output: Importance bar plot
  output$importancePlot = renderPlot({
    coeffs = model()$bhat[-1] 
    labels = names(coeffs)
    barplot(coeffs,
            names.arg = labels,
            col = "steelblue",
            main = "Predictor Importance (Coefficients)",
            las = 2,
            xlab = "Predictors",
            ylab = "Coefficient Value")
  })
}

# Run the app
shinyApp(ui = GUI, server = myserver)
