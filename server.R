#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rpart)
library(caret)
library('e1071')
require(randomForest)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        
        Training<- read.csv('Training.csv',na.strings=c("", "NA", "NULL"))
        

        #split data
        inTrain <-createDataPartition(y=Training$classe, p=.7, list=FALSE)
        Training_Data <- Training[inTrain,]
        Testing_Data <- Training[-inTrain,]

        set.seed(12345)
        modFitDT <- rpart(classe ~ ., data = Training_Data,
                          method="class",
                          control = rpart.control(method = "cv",
                                                  number = 10))

        Model_RF=randomForest(classe~.,
                              data=Training_Data,
                              ntree=10,
                              importance=TRUE)

        output$Plot_Rf<- renderPlot({
                if(input$select1 == "Random Forest"){
                        varImpPlot(Model_RF)
                }
        })

        output$Plot_Tree<- renderPlot({
                plot(modFitDT)
                text(modFitDT,cex=.8)
        })

        output$text_Tree <- renderText({
                User_Selected_Data <- input$slider1
                Pre_Tree <- predict(modFitDT, Testing_Data[User_Selected_Data,], type = "class")
                paste("Prediction value:",as.character(Pre_Tree),sep="")
        })

        output$text_True <- reactive({
                User_Selected_Data <- input$slider1
                data<-as.character(Testing_Data[User_Selected_Data,]$classe)
                paste("True value:",data,sep="")
        })

        output$text_Rf <- renderText({
                User_Selected_Data <- input$slider1
                Pre_RF <- predict(Model_RF, newdata = Testing_Data[User_Selected_Data,],type = "class")
                paste("Random-Forest Prediction value:",as.character(Pre_RF),sep="")
        })

        output$text_Rf_True <- reactive({
                User_Selected_Data <- input$slider1
                data<-as.character(Testing_Data[User_Selected_Data,]$classe)
                paste("True value:",data,sep="")
        })
  
})
