Introduction of my first Shiny App
========================================================
author: Bo Liu
date: 17 Jul 2017
autosize: true

Open App
========================================================
This presentation is for introducing my first simple Shiny App.

Let's try this App first, open [this link](https://freefrog.shinyapps.io/Reproducible_Pitch/)
in your webbrowser to access my App.

you can find source code at [this github link](https://github.com/freefrog1986/Reproducible_Pitch)

Also there is a readme file about how to get start with this App.

This presenetation introduce more details of The App.

Loading and Cleanning data
========================================================
First loading data when running, The data for this app come from [this link](http://groupware.les.inf.puc-rio.br/har )    

Let's have a glance at the data.

```
[1] 19622   160
```
There are 19622 entrans and 160 variables in this dataset.   

Then cleaning data by removing NA and unrelated variables.

```
[1] 19622    53
```
Variabls reducing from 160 to 53.

Then split the training data into Training and Testing data. 

```
[1] "Training Data: 13737 53"
```

```
[1] "Testing_Data: 5885 53"
```

Building the Model
========================================================
Depending on the user selection, we fit a Regression Tree Model or a Random Forest Model, and plot it.   
![plot of chunk unnamed-chunk-4](my_shiny_app-figure/unnamed-chunk-4-1.png)![plot of chunk unnamed-chunk-4](my_shiny_app-figure/unnamed-chunk-4-2.png)

You have to be patient cause this part run a bit slow.

Testing the model
========================================================
By selecting a value from the slider of this app, you can test this two model.   
When selecting a value, server will predict using models.   
Then show results and real value to the interface for comparing.   

```r
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
```


