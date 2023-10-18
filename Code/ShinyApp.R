library(shiny)
library(readr)
library(ggplot2)
library(dplyr)




BodyFat1 <- read.csv("newdata.csv")
BodyFat2 <-BodyFat1 %>% select(-c(X,IDNO,DENSITY))
set.seed(123)
sample_index <- sample(1:nrow(BodyFat2), size = 0.7 * nrow(BodyFat2))  
train <- BodyFat2[sample_index, ]
test <- BodyFat2[-sample_index, ]

modelsub <- lm(BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data = train)
## summary(modelsub)

B0<- summary(modelsub)$coefficient[1,1]
B1<- summary(modelsub)$coefficient[2,1]
B2<- summary(modelsub)$coefficient[3,1]
B3<- summary(modelsub)$coefficient[4,1]
B4<- summary(modelsub)$coefficient[5,1]


ui <- fluidPage(
 
  fluidRow(
  column(12, align = "center", titlePanel("Body Fat Percentage Prediction Machine🤖️"))),
  h4("Please enter some personal information:️"),
  sidebarLayout(
    
    sidebarPanel(width = 4,
                 numericInput(inputId = "Weight",
                              label = "Weight (kg)",
                              min = 50.9,
                              max = 170.1,
                              value = 80.1,
                              step = 0.5),
                 helpText("Weight is a measure of weight without items located on the person."),
                 
                 numericInput(inputId = "Abdomen",
                              label = "Abdomen circumference (cm)",
                              min = 40.1,
                              max = 200.1,
                              value = 90.1,
                              step = 0.5),
                 helpText("Abdominal circumference is a measure of the distance of the abdomen around the body at the height of the navel."),
                 
                 numericInput(inputId = "Forearm",
                              label = "Forearm circumference (cm)",
                              min = 15.9,
                              max = 100.1,
                              value = 29.1,
                              step = 0.25),
                 helpText("Forearm circumference is a measure of the circumference or distance of the forearm (the part of the arm located between the elbow and the wrist)."),
                 
                 numericInput(inputId = "Wrist",
                              label = "Wrist Circumference (cm)",
                              min = 10.9,
                              max = 30.1,
                              value = 18.1,
                              step = 0.25),
                 helpText("Wrist Circumference is a measure of the length of a week at the widest part of the wrist."),
                 
                 submitButton(text="Calculate！")
    ),
    
    mainPanel(width =8,
              h4("Predicted Body Fat Percentage:"),
              textOutput("fin"),
              plotOutput("hist"),
              
              p(),
              h5("The red line indicates where your body fat percentage is in the population.", align="center"),
              

    )
  ),
  
  plotOutput("plot")
)



server <- function(input, output) {
  fin<- reactive({
    if ((B0 + (B1*input$Weight) + (B2*input$Abdomen) + (B3*input$Forearm) + (B4*input$Wrist)) >= 4 & 
        (B0 + (B1*input$Weight) + (B2*input$Abdomen) + (B3*input$Forearm) + (B4*input$Wrist)) <= 50){
      B0 + (B1*input$Weight) + (B2*input$Abdomen) + (B3*input$Forearm) + (B4*input$Wrist)
    }
    else{0}
  })
  
  output$hist <- renderPlot({
    ggplot(BodyFat2,aes(BODYFAT))+
      geom_histogram(bins=40, color = "black",fill = "#e4ce00")+
      xlab("Body Fat Percentage")+
      ylab("Count")+
      geom_vline(xintercept= fin(), color = "#cc340c", linetype="longdash", size = 1.25)
  })
  
  output$fin <- renderText({
    if ((B0 + (B1 * input$Weight) + (B2 * input$Abdomen) + (B3 * input$Forearm) + (B4 * input$Wrist)) >= 4 &
        (B0 + (B1 * input$Weight) + (B2 * input$Abdomen) + (B3 * input$Forearm) + (B4 * input$Wrist)) <= 50) {
      paste(round(B0 + (B1 * input$Weight) + (B2 * input$Abdomen) + (B3 * input$Forearm) + (B4 * input$Wrist), 3), "%")
    } else {
      "0%"
    }
  })
  
  


}



shinyapp<- shinyApp(ui,server)
