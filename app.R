library(shiny)

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Relative CPU Performance Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      #Selector for file upload
      fileInput('datafile', 'Choose CSV file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      div(style = "margin-top:-35px"),
      
      # Input: Checkbox for whether headers should be included ----
      checkboxInput("headers", "First Line Headers", TRUE),
      
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Nearest Neighbours:",
                  value = 5,
                  min = 1,
                  max = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(id = "tabset", type = "tabs",
                  tabPanel("Description", helpText( 
                                                   br(),
                                                   h1(strong("k - Nearest Neighbour")),
                                                   h5("The k-nearest neighbor (KNN) algorithm is a machine learning method used for both classification and regression tasks. 
                                                      In the case of continuous target variables, KNN can be used for regression tasks. The KNN algorithm uses the distance between 
                                                      data points to identify the K-nearest neighbors to a given data point. These neighbors are then used to predict the target value 
                                                      for the data point in question. The hyper-parameter k in KNN represents the number of nearest neighbors used to make predictions. 
                                                      The optimal value of k depends on the data and must be determined through experimentation. Choosing a value of k that is too small 
                                                      can lead to overfitting, while a value that is too large can lead to underfitting. To implement the KNN algorithm for continuous 
                                                      target variables, the Euclidean distance or other distance metrics can be used to measure the distance between data points. 
                                                      After identifying the K-nearest neighbors, the target variable can be predicted by taking the average of the target values of the 
                                                      neighbors. Overall, the KNN algorithm is a simple and effective method for regression tasks, but it can become computationally 
                                                      expensive for large datasets.",
                                                      style="text-align:justify;color:black;padding:15px"),
                                                   br(),
                                                   
                                                   h1("About This App"),
                                                   p("Through this application, it is intended to develop a learning environment for anyone who is starting in the study of statistical modeling, 
                                                      specifically K-NN. In this application, we will focus on the different choices of K and not on the mathematical processes.", 
                                                      style="text-align:justify;color:black;background-color:lavender;padding:50px;border-radius:10px"),
                                                   br(),
                                                   
                                                   p("The data used in this application are publicly available on the page", 
                                                     em("https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/"),
                                                     style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                                                   
                                                   
                                                   
                                                   
                                                   )),
                  tabPanel("Data", tableOutput("filetable")),
                  tabPanel("Summary", verbatimTextOutput("summary1"),
                           plotOutput("plot")),
                  
                  
                  tabPanel("k_NN", helpText(strong("Performance Metrics on Test Data")), 
                           tableOutput("table"),
                           
                           textOutput("summary2")))
                           
                           
      )
      
    )
  )




library(caret)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the model ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression

  #This function is responsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath, header = input$headers)
  })
  

  model_results = reactive({ 
    
    if(input$tabset == "k_NN"){  

      set.seed(123)
      a <- createDataPartition(filedata()$V7, p = 0.70, list=FALSE)
      training <- filedata()[a,]
      test <- filedata()[-a,]
      
      knnFit <- train(V10 ~ V3 + V4 + V5 + V6 + V7 + V8 + V9, 
                      data = training, 
                      method="knn",  
                      tuneGrid=data.frame(k=input$n))
      
      knn_pred <- predict(knnFit, newdata = test)
      head(knn_pred)
      
      perf = postResample(pred = knn_pred, obs = test$V10)
      perfname = c("Root Mean Square Error", "R-Squared", "Mean Absolute Error")
      
      perf = cbind.data.frame(perfname, perf)
      colnames(perf) = c("Performance Metric", "Value")
      perf
      list(knnFit, perf)
      }  
    })
  
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata()
  })  
  
  
  # Generate a plot of the data ----
  output$plot <- renderPlot({
    
    par(mfrow = c(2,2))
    plot(V10 ~ V3, data = filedata(),
            main = paste("Plot of V10 against V3"),
            col = "#75AADB")
    
    plot(V10 ~ V4, data = filedata(),
         main = paste("Plot of V10 against V4"),
         col = "#75AADB")
    
    plot(V10 ~ V5, data = filedata(),
            main = paste("Plot of V10 against V5"),
            col = "#75AADB")
    
    plot(V10 ~ V6, data = filedata(),
            main = paste("Plot of V10 against V6"),
            col = "#75AADB")
    
  })
  
  # Generate a summary of the data ----
  output$summary1 <- renderPrint({
    summary(filedata())
  })
  
  # Generate a summary of the data ----
  output$summary2 <- renderPrint({
    model_results()[1]
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    model_results()[2]
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)