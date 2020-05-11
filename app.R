library(shiny)
library (shinythemes)
library(shinyBS)
library(rms)
library(plotly)



setwd("~/shiny-app/prostate-salvage-dfs")
modelcoxa <- readRDS("coxph.RDS")
modelcoxa2 <- readRDS("cph.RDS")
#progi <- readRDS("progi.RDS")

# Define UI for slider demo app ----
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "konsta.css"),
    
    HTML("<link rel=\"author\" href=\"https://plus.google.com/109644123848417917359\" />"),
    HTML("<script>
         (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
         (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
         m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
         })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
         
         ga('create', 'UA-53584749-4', 'auto');
         ga('send', 'pageview');
         
         </script>")
  ),
  # App title ----
  titlePanel("Predict the probability of relapse or reccurence (disease-free survival) after adjuvant or salvage radiation therapy for prostate cancer."),
  
  # Sidebar layout with input and output definitions ----
    p("Opis..."),
    hr(),
    # Sidebar to demonstrate various slider options ----
    fluidRow(
      column(4,offset = 0,
      # Input: Simple integer interval ----
      h4("Initial treatment:"),
      selectInput("wysokiGGG", "Post-op Gleason grading group (GGG):",
                        choices = list("High (GGG ≥ 4 / Gleason score ≥ 8)" = 2, 
                        "Low (GGG < 4 / Gleason score < 8)" = 1), selected = 2),

      selectInput("ART", "Prior intention of radiotherapy:",
                        choices = list("Salvage" = 2, 
                        "Adjuvant" = 1), selected = 2),

      selectInput("indication", "Primary indication for radiotherapy treatment:",
                        choices = list("Post-op pT3 feature" = 1, 
                        "Local reccurence (mass)" = 2, "Other (biochemical, margin etc.)" = 3), selected = 1)),

      column(3,offset = 0,
      h4("Current status:"),
      # Input: Decimal interval with step value ----
      
            selectInput("wysokiePSA", "Latest serum PSA level:",
                        choices = list("Low (PSA < 0.2 ng/ml)" = 1, 
                        "High (PSA ≥ 0.2 ng/ml)" = 2), selected = 1),

            selectInput("ADT", "Currently on ADT:",
                        choices = list("No" = 1, 
                        "Yes" = 2), selected = 1)),
      
      #submitButton("Update filters")
      
      column(5,offset = 0,
           h4("Prediction:"),
           # Output: Table summarizing the values entered ----
           tableOutput("values"),
           em("The table above presents the probability of relapse or reccurence of prostate cancer for this particular patient as well as its 95% confidence interval."),
           
    ),
      
    
  ),
  fluidRow(
    # Main panel for displaying outputs ----
    
    column(12,
    h4("Probability over time:"),br(),br(),
    plotlyOutput(height=250,"wykres"))
  ),
  hr(),
  p("Model details:"),
verbatimTextOutput("modelInfo"), 

  hr(),
  p(HTML("<a href=\"#\" target=\"_blank\">This software is a part of paper entitled ''.</a>"),
    br(),
    
    HTML(paste0("<table border='0' width='100%'><tr><td><i>© Version 1.0 revision ", system("git rev-list HEAD --count", intern = TRUE) ," (", system("git log -1 --pretty='%H'", intern = TRUE) ,")</i><br /><a href='http://link.konsta.com.pl/cv'><font size='1'>Software author, technical issues: <br/><b>Konrad Stawiski, MD</b> (contact: konrad@konsta.com.pl; Department of Biostatistics and Translational Medicine, Medical University of Lodz, Poland)</font></a></td><td><img align='right' width='200' src='logo.png'/></td></tr></table>"))
  )
  
  
  )


# Define server logic for slider examples ----
server <- function(input, output) {
  
  
  modelInfo = reactive({ modelcoxa2 })
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    wskazanie_T3 = as.numeric(as.character(factor(ifelse(input$indication == 1, 2, 1), levels = c(1,2))))
    wskazanie_wnowmiejsc = as.numeric(as.character(factor(ifelse(input$indication == 2, 2, 1), levels = c(1,2))))
    wysokiGGG = as.numeric(as.character(factor(input$wysokiGGG, levels = c(1,2))))
    wysokiePSA = as.numeric(as.character(factor(input$wysokiePSA, levels = c(1,2))))
    ADT = as.numeric(as.character(factor(input$ADT, levels = c(1,2))))
    ART = as.numeric(as.character(factor(input$ART, levels = c(1,2))))

    y_pred_12 = 1- Predict(modelcoxa2, time = 12, wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$yhat
    y_pred_24 = 1-Predict(modelcoxa2, time = 24,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$yhat
    y_pred_36 = 1-Predict(modelcoxa2, time = 36,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$yhat
    y_pred_48 = 1-Predict(modelcoxa2, time = 60, wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$yhat
    
    y_pred_12l = 1-Predict(modelcoxa2, time = 12,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$lower
    y_pred_24l = 1-Predict(modelcoxa2, time = 24,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$lower
    y_pred_36l = 1-Predict(modelcoxa2, time = 36,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$lower
    y_pred_48l = 1-Predict(modelcoxa2, time = 60,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$lower
    
    y_pred_12u = 1-Predict(modelcoxa2, time = 12,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$upper
    y_pred_24u = 1-Predict(modelcoxa2, time = 24,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$upper
    y_pred_36u = 1-Predict(modelcoxa2, time = 36,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$upper
    y_pred_48u = 1-Predict(modelcoxa2, time = 60,  wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$upper
    
    dec_12 = ifelse(y_pred_12>(1-0.5),"+","-")
    dec_24 = ifelse(y_pred_24>(1-0.5),"+","-")
    dec_36 = ifelse(y_pred_36>(1-0.5),"+","-")
    dec_48 = ifelse(y_pred_48>(1-0.5),"+","-")
    
    ci12 = paste0(round(y_pred_12u, digits = 2), " - ", round(y_pred_12l, digits = 2))
    ci24 = paste0(round(y_pred_24u, digits = 2), " - ", round(y_pred_24l, digits = 2))
    ci36 = paste0(round(y_pred_36u, digits = 2), " - ",round(y_pred_36l, digits = 2))
    ci48 = paste0(round(y_pred_48u, digits = 2), " - ", round(y_pred_48l, digits = 2))
    
    res = data.frame(
      "Period" = c("1-Year follow-up","2-Year follow-up","3-Year follow-up","5-Year follow-up"),
      "Score" = c(y_pred_12, y_pred_24, y_pred_36, y_pred_48),
      "CI" = c(ci12,ci24,ci36,ci48),
      #"Threshold" = c(">0.70",">0.62",">0.61",">0.59"),
      "Prediction" = c(dec_12,dec_24,dec_36,dec_48),
      stringsAsFactors = FALSE)
    
    colnames(res) = c("Period","Probability","95% CI","Prediction")
    
    res
    
  })
  
  thresholdPlot <- reactive({
    trace_0 = vector()
    for (i in 12:60) {
      wskazanie_T3 = as.numeric(as.character(factor(ifelse(input$indication == 1, 2, 1), levels = c(1,2))))
      wskazanie_wnowmiejsc = as.numeric(as.character(factor(ifelse(input$indication == 2, 2, 1), levels = c(1,2))))
      wysokiGGG = as.numeric(as.character(factor(input$wysokiGGG, levels = c(1,2))))
      wysokiePSA = as.numeric(as.character(factor(input$wysokiePSA, levels = c(1,2))))
      ADT = as.numeric(as.character(factor(input$ADT, levels = c(1,2))))
      ART = as.numeric(as.character(factor(input$ART, levels = c(1,2))))
      
      trace_0 =  c(trace_0, 1-as.numeric(Predict(modelcoxa2, time = i, wysokiGGG = wysokiGGG, wskazanie_T3 = wskazanie_T3, wskazanie_wnowmiejsc = wskazanie_wnowmiejsc, wysokiePSA = wysokiePSA, ADT = ADT, ART = ART)$yhat))
    }
    
    trace_1 <- 1-0.5

    x <- c(12:60)
    
    data <- data.frame(x, trace_0, trace_1)
    
    p <- plot_ly(data, x = ~x, y = ~trace_0, name = 'Score', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~0.5, name = 'Threshold', mode = 'lines') %>%
      layout(
          xaxis = list(title = "Months of follow-up"),
          yaxis = list(title = "Probability", range=c(0,1))
        )
    
    
    p
  })
  
  # Show the values in an HTML table ----

  output$values <- renderTable({
    sliderValues()
  })
  
  output$wykres <- renderPlotly({
    thresholdPlot()
  })
  
  output$modelInfo <- renderPrint({
    modelInfo()
  })
  

}

# Create Shiny app ----
shinyApp(ui, server)