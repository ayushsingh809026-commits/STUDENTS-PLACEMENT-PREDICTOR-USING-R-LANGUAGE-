library(shiny)
library(ggplot2)

# Load dataset
Placement <- read.csv("college_student_placement_dataset.csv")

# Convert Yes/No to 1/0
Placement$Placement <- ifelse(Placement$Placement=="Yes",1,0)
Placement$Internship_Experience <- ifelse(Placement$Internship_Experience=="Yes",1,0)

# Machine Learning Model
model <- glm(
  Placement ~ IQ + Prev_Sem_Result + CGPA +
    Academic_Performance + Internship_Experience +
    Extra_Curricular_Score + Communication_Skills +
    Projects_Completed,
  data = Placement,
  family = "binomial"
)

# ---------------- UI ----------------

ui <- fluidPage(
  
  titlePanel("AI Agent Based Student Placement Analytics Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Prediction Agent"),
      
      numericInput("cgpa_input","Enter CGPA:",7),
      numericInput("iq_input","Enter IQ:",120),
      
      selectInput(
        "internship_input",
        "Internship Experience:",
        choices=c("No"=0,"Yes"=1)
      ),
      
      numericInput("communication_input","Communication Skills (1-10):",7),
      numericInput("projects_input","Projects Completed:",2),
      
      actionButton("predict","Predict Placement"),
      
      hr(),
      
      h4("Ask Data Agent"),
      
      textInput("question","Ask question about data:"),
      
      helpText("Recommended questions:"),
      
      tags$ul(
        tags$li("What is the placement rate?"),
        tags$li("Average CGPA of placed students"),
        tags$li("Average IQ of students"),
        tags$li("Does CGPA affect placement?")
      ),
      
      actionButton("ask","Ask AI"),
      
      hr(),
      
      h4("Filters"),
      
      sliderInput(
        "cgpa_range",
        "Select CGPA Range:",
        min=0,
        max=10,
        value=c(0,10)
      ),
      
      sliderInput(
        "iq_range",
        "Select IQ Range:",
        min=min(Placement$IQ),
        max=max(Placement$IQ),
        value=c(min(Placement$IQ),max(Placement$IQ))
      ),
      
      sliderInput(
        "communication_range",
        "Communication Skills Range:",
        min=1,
        max=10,
        value=c(1,10)
      ),
      
      sliderInput(
        "projects_range",
        "Projects Completed Range:",
        min=0,
        max=max(Placement$Projects_Completed),
        value=c(0,max(Placement$Projects_Completed))
      ),
      
      radioButtons(
        "internship_filter",
        "Internship Experience:",
        choices=c("All","Yes","No"),
        selected="All"
      ),
      
      checkboxGroupInput(
        "placement_filter",
        "Placement Status:",
        choices=c("Not Placed"=0,"Placed"=1),
        selected=c(0,1)
      )
      
    ),
    
    mainPanel(
      
      h3("Prediction Result"),
      verbatimTextOutput("prediction"),
      
      h3("Recommendation Agent"),
      verbatimTextOutput("advice"),
      
      h3("Data Agent Answer"),
      verbatimTextOutput("ai_answer"),
      
      h3("AI Insights"),
      verbatimTextOutput("insight"),
      
      fluidRow(
        column(6,plotOutput("barPlot")),
        column(6,plotOutput("histPlot"))
      ),
      
      fluidRow(
        column(6,plotOutput("scatterPlot")),
        column(6,plotOutput("boxPlot"))
      )
      
    )
  )
)

# ---------------- SERVER ----------------

server <- function(input,output){
  
  # Prediction Agent
  prediction_result <- eventReactive(input$predict,{
    
    new_data <- data.frame(
      IQ=input$iq_input,
      Prev_Sem_Result=7,
      CGPA=input$cgpa_input,
      Academic_Performance=7,
      Internship_Experience=as.numeric(input$internship_input),
      Extra_Curricular_Score=5,
      Communication_Skills=input$communication_input,
      Projects_Completed=input$projects_input
    )
    
    prob <- predict(model,new_data,type="response")
    
    paste("Placement Probability:",round(prob*100,2),"%")
    
  })
  
  output$prediction <- renderText({
    prediction_result()
  })
  
  # Recommendation Agent
  output$advice <- renderText({
    
    if(input$cgpa_input < 6){
      "Recommendation: Improve CGPA for better placement chances."
    }
    else if(input$cgpa_input < 7){
      "Recommendation: Moderate chance. Improve communication and projects."
    }
    else{
      "Recommendation: Good CGPA. High chance of placement."
    }
    
  })
  
  # Ask Data Agent
  ai_response <- eventReactive(input$ask,{
    
    q <- tolower(input$question)
    
    if(grepl("placement rate",q)){
      rate <- mean(Placement$Placement)*100
      paste("Overall placement rate is",round(rate,2),"%")
    }
    
    else if(grepl("cgpa",q)){
      avg <- mean(Placement$CGPA)
      paste("Average CGPA of students is",round(avg,2))
    }
    
    else if(grepl("iq",q)){
      avg_iq <- mean(Placement$IQ)
      paste("Average IQ of students is",round(avg_iq,2))
    }
    
    else if(grepl("affect",q)){
      "Students with higher CGPA tend to have better placement chances."
    }
    
    else{
      "I can answer questions about placement rate, CGPA, IQ, and placement trends."
    }
    
  })
  
  output$ai_answer <- renderText({
    ai_response()
  })
  
  # ---------------- FILTERED DATA ----------------
  
  filtered <- reactive({
    
    data <- Placement
    
    data <- data[
      data$CGPA >= input$cgpa_range[1] &
        data$CGPA <= input$cgpa_range[2],]
    
    data <- data[
      data$IQ >= input$iq_range[1] &
        data$IQ <= input$iq_range[2],]
    
    data <- data[
      data$Communication_Skills >= input$communication_range[1] &
        data$Communication_Skills <= input$communication_range[2],]
    
    data <- data[
      data$Projects_Completed >= input$projects_range[1] &
        data$Projects_Completed <= input$projects_range[2],]
    
    if(input$internship_filter=="Yes"){
      data <- data[data$Internship_Experience==1,]
    }
    
    if(input$internship_filter=="No"){
      data <- data[data$Internship_Experience==0,]
    }
    
    data <- data[data$Placement %in% input$placement_filter,]
    
    data
    
  })
  
  # ---------------- GRAPHS ----------------
  
  output$barPlot <- renderPlot({
    
    ggplot(filtered(),aes(x=factor(Placement)))+
      geom_bar(fill="blue")+
      labs(title="Placement Count",x="Placement",y="Count")
    
  })
  
  output$histPlot <- renderPlot({
    
    ggplot(filtered(),aes(x=CGPA))+
      geom_histogram(fill="orange",bins=15)+
      labs(title="CGPA Distribution")
    
  })
  
  output$scatterPlot <- renderPlot({
    
    ggplot(filtered(),aes(x=CGPA,y=IQ,color=factor(Placement)))+
      geom_point(size=3)+
      labs(title="CGPA vs IQ",color="Placement")
    
  })
  
  output$boxPlot <- renderPlot({
    
    ggplot(filtered(),aes(x=factor(Placement),y=CGPA))+
      geom_boxplot(fill="green")+
      labs(title="CGPA vs Placement")
    
  })
  
  # ---------------- INSIGHT AGENT ----------------
  
  output$insight <- renderText({
    
    data <- filtered()
    
    placed_rate <- mean(data$Placement)*100
    avg_cgpa <- mean(data$CGPA)
    
    paste(
      "Placement Rate:",round(placed_rate,2),"%\n",
      "Average CGPA:",round(avg_cgpa,2),"\n",
      "Insight: Higher CGPA students generally have better placement chances."
    )
    
  })
  
}

shinyApp(ui,server)