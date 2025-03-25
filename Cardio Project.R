#Part One: Verify Data Import

# View the first few rows
chooseCRANmirror(graphics = FALSE, ind = 1)
install.packages("readxl")
library(readxl)
cardio_train <- read_excel("~/Documents/cardio_train.xlsx")
head(cardio_train)

# Check structure and types of variables
str(cardio_train)

# Get summary statistics
summary(cardio_train)

# Count missing values
sum(is.na(cardio_train))



# Part Two: Data Cleaning & Formatting

# Convert age from days to years
cardio_train$age_years <- cardio_train$age / 365

# Convert categorical variables to factors
cardio_train$gender <- factor(cardio_train$gender, levels = c(1, 2), labels = c("Male","Female"))
# 0 = No, 1 = Yes
cardio_train$cardio <- as.factor(cardio_train$cardio) 
 

# Identify Extreme Values for systolic blood pressure
install.packages("dplyr")  
library(dplyr)

summary(cardio_train$ap_hi)
cardio_train %>% filter(ap_hi > 250 | ap_hi < 50)

# Cap Extreme Values
cardio_train$ap_hi <- ifelse(cardio_train$ap_hi > 200, 200, cardio_train$ap_hi)
cardio_train$ap_hi <- ifelse(cardio_train$ap_hi < 90, 90, cardio_train$ap_hi)
summary(cardio_train$ap_hi)

# Identify Extreme Values for diastolic BP
summary(cardio_train$ap_lo)
cardio_train %>% filter(ap_lo > 150 | ap_lo < 40)

summary(cardio_train$height)
summary(cardio_train$weight)
summary(cardio_train$age_years)

# Cap Extreme Values
cardio_train$ap_lo <- ifelse(cardio_train$ap_lo > 120, 120, cardio_train$ap_lo)
cardio_train$ap_lo <- ifelse(cardio_train$ap_lo < 60, 60, cardio_train$ap_lo)



# Part Three: Exploratory Data Analysis

# Histogram for Age Distribution
install.packages("ggplot2") 
library(ggplot2) 

ggplot(cardio_train, aes(x = age_years)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(35, 70)) +
  labs(title = "Age Distribution", x = "Age (Years)", y = "Count") +
  theme_minimal()

# Boxplot for Blood Pressure
boxplot(cardio_train$ap_hi, main="Systolic Blood Pressure", col="lightpink")
boxplot(cardio_train$ap_lo, main="Cleaned Diastolic Blood Pressure", col="lightblue")

# Boxplot for Age vs. Cardiovascular Disease
install.packages("ggplot2") 
library(ggplot2)  

ggplot(cardio_train, aes(x = as.factor(cardio), y = age_years, fill = as.factor(smoke))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  labs(title = "Age vs. Cardiovascular Disease by Smoking Status",
       x = "Cardiovascular Disease (0 = No, 1 = Yes)",
       y = "Age (Years)", fill = "Smoking Status") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "lightpink", "1" = "lightblue"))



# Part Four: Predictive Modeling

# Logistic Regression
heart_model <- glm(cardio ~ age_years + ap_hi + cholesterol + smoke + active, 
                   data = cardio_train, family = "binomial")
summary(heart_model)

# Decision Tree
install.packages("rpart.plot")
library(rpart.plot)
tree_model <- rpart(cardio ~ age_years + ap_hi + ap_lo + cholesterol + smoke + active, 
                    data = cardio_train, method = "class", minsplit = 20, cp = 0.001)
rpart.plot(tree_model, type = 3, extra = 104, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)


# Evaluate Model Performance
library(caret)
predictions <- predict(tree_model, cardio_train, type = "class")
conf_matrix <- confusionMatrix(predictions, cardio_train$cardio)
print(conf_matrix)



# Part Five: Shiny Dashboard

# Build as Shiny Dashboard
cardio_train$age_years <- cardio_train$age/365
cardio_train$gender <- factor(cardio_train$gender, levels = c(1,2), labels = c("Male", "Female"))
cardio_train$cardio <- as.factor(cardio_train$cardio)                             

heart_model <- glm(cardio ~ age_years + ap_hi + ap_lo + cholesterol + smoke + active, 
                   data = cardio_train, family = "binomial")

install.packages("shiny")
library(shiny)

ui <- fluidPage(
  titlePanel("Cardiovascular Disease Risk Explorer"),
  tabsetPanel(
    tabPanel("Exploratory Data Analysis",
             fluidRow(
               column(6, plotOutput("ageHist")),
               column(6, plotOutput("bpBox"))
             ),
             fluidRow(
               column(12, plotOutput("ageCardioBox"))
             )
    ),
    tabPanel("Predict Risk",
             sidebarLayout(
               sidebarPanel(
                 numericInput("age", "Age (in years):", value = 50, min = 30, max = 80),
                 numericInput("ap_hi", "Systolic BP:", value = 120),
                 numericInput("ap_lo", "Diastolic BP:", value = 80),
                 selectInput("cholesterol", "Cholesterol:", choices = c(1,2,3)),
                 selectInput("smoke", "Smoker:", choices = c(0,1)),
                 selectInput("active", "Physically Active:", choices = c(0,1)),
                 actionButton("predict", "Predict")
               ),
               mainPanel(
                 verbatimTextOutput("prediction")
               )
             )
    )
  )
)

server <- function(input, output) {
  # EDA Visuals
  output$ageHist <- renderPlot({
    ggplot(cardio_train, aes(x = age_years)) +
      geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
      labs(title = "Age Distribution", x = "Age (Years)", y = "Count") +
      theme_minimal()
  })
  
  output$bpBox <- renderPlot({
    boxplot(cardio_train$ap_hi, cardio_train$ap_lo,
            names = c("Systolic BP", "Diastolic BP"),
            col = c("lightpink", "lightblue"),
            main = "Blood Pressure Boxplots")
  })
  
  output$ageCardioBox <- renderPlot({
    ggplot(cardio_train, aes(x = as.factor(cardio), y = age_years, fill = as.factor(smoke))) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      labs(title = "Age vs. Cardiovascular Disease by Smoking Status",
           x = "Cardiovascular Disease (0 = No, 1 = Yes)",
           y = "Age (Years)", fill = "Smoker") +
      scale_fill_manual(values = c("0" = "lightpink", "1" = "lightblue")) +
      theme_minimal()
  })
  
  observeEvent(input$predict, {
    user_data <- data.frame(
      age_years = input$age,
      ap_hi = input$ap_hi,
      ap_lo = input$ap_lo,
      cholesterol = as.numeric(input$cholesterol),
      smoke = as.numeric(input$smoke),
      active = as.numeric(input$active)
    )
    
    prob <- predict(heart_model, newdata = user_data, type = "response")
    result <- ifelse(prob > 0.5, "At Risk", "Not at Risk")
    
    output$prediction <- renderText({
      paste("Predicted Risk:", result, "\nProbability of cardiovascular disease:", round(prob, 3))
    })
  })
}

shinyApp(ui = ui, server = server)


         