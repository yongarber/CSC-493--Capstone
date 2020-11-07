library(shiny)
library(tidyverse)
library(ggplot2)
library(broom)
library("tidyverse")
library("caTools")
library("rpart")
library("rpart.plot")
library("lattice")
library("caret")
library(DT)
library(imager)


###################################
#   Pre-Analysis                  #
###################################

## Data Importing and cleaning
#data1 <- vroom::vroom("../data/estate.csv")

#data1 <- read.csv("Data/covid-19.csv")
data1 <- read.csv("../covid_data_all numerics.csv")

data1 %>%
    filter(result!="other") %>% 
    filter(age_60_and_above!="NULL")->
    data2

positive_data<-subset(data2,result=="positive")
negative_data<-subset(data2,result=="negative")


sub_neg<-sample_n(negative_data, 50000)
sub_pos<-sample_n(positive_data,50000)


set.seed(42)
data3<-rbind(sub_neg,sub_pos)
rows <- sample(nrow(data3))
data3 <- data3[rows, ]

set.seed(123)
sample = sample.split(data3,SplitRatio = 0.75)

train =subset(data3,sample ==TRUE) 
test =subset(data3, sample==FALSE)

###################################
#   UI                            #
###################################

ui <- fluidPage(
    titlePanel("Covid-19 Diagnosis Tool"),
    tabsetPanel(
        tabPanel("Data",fluidRow(
        column(12, DT::dataTableOutput("spreadsheet"))
        )),
        tabPanel("Predictive Models", fluidRow(column(2,
                                             actionButton('run_rig', label = 'Run Logistic Regression'),
                                             actionButton('run_lda', label = 'Run LDA'),
                                             actionButton('run_qda', label = 'Run QDA')
        ),
        column(6, verbatimTextOutput("output_of_model")),
        column(2, h5('What is your situation?'),
               checkboxInput("cough", "Cough?", FALSE),
               checkboxInput("fever", "Fever?", FALSE),
               checkboxInput("sore", "Sore throat?", FALSE),
               checkboxInput("breath", "Shortness of breath?", FALSE),
               checkboxInput("head", "Head ache?", FALSE),
               checkboxInput("age", "Are you older than 60?", FALSE),
               checkboxInput("male", "male?", FALSE)),
        column(2, verbatimTextOutput("result_from_predictive_model"))
        ),fluidRow(column(12,plotOutput('model_plot')))), 
        tabPanel("Decision Tree",
                 sidebarLayout(
                     sidebarPanel(
                         h3('Custom Decision Tree'),
                         uiOutput('choose_y'),
                         uiOutput('choose_x'),
                         actionButton('c50', label = 'Generate Results')
                     ),
                     mainPanel(
                         verbatimTextOutput('tree_summary'),
                         plotOutput('tree_plot_c50')
                     )
                 )
                 
        )
        
    )
)

###################################
#   Server                        #
###################################


server <- function(input, output, session) {
    
    output$spreadsheet <-
        DT::renderDataTable(data1, options= list(pageLength = 10))
    
    ## Logistic Regression part
    observeEvent(input$run_rig, {
        glm.fits=glm(as.factor(result)~cough+fever+sore_throat+shortness_of_breath
                     +head_ache+age_60_and_above+gender , data=data3, family = binomial)
        output$output_of_model <- renderPrint(summary(glm.fits))
        glm.probs=predict(glm.fits,type="response")
        glm.pred=rep("0",50000)
        glm.pred[glm.probs >0.5]="1"
        table_matrix<-table(glm.pred,data3$result)
        output$model_plot <- renderPlot(fourfoldplot(table_matrix, color = c("#CC6666", "#99CC99"),
                                                     conf.level = 0, margin = 1,std='ind.max', main = "Confusion Matrix")+ 
                                            text(-0.4,0.4, "TN", cex=1) + 
                                            text(0.4, -0.4, "TP", cex=1) + 
                                            text(0.4,0.4, "FP", cex=1) + 
                                            text(-0.4, -0.4, "FN", cex=1))
        glm.probs<-predict(glm.fits,newdata = data.frame(cough=1*!!input$cough,fever=1*!!input$fever,sore_throat=1*input$sore,shortness_of_breath=1*input$breath,head_ache=1*input$head,age_60_and_above=1*input$age,gender=1*input$male),type="response")
        output$result_from_predictive_model <-renderPrint(glm.probs)
    })
    ## LDA part
    observeEvent(input$run_lda, {
        trControl <- trainControl(method  = "cv",number  = 5)
        ldafit <-train(as.factor(result)~cough+fever+sore_throat+shortness_of_breath
                       +head_ache+age_60_and_above+gender,
                       method     = "lda",
                       trControl  = trControl,
                       data=data3)
        output$output_of_model <- renderPrint(ldafit)
        pred.class = predict(ldafit, data3)
        table_matrix<-table(pred.class, data3$result)
        output$model_plot <- renderPlot(fourfoldplot(table_matrix, color = c("#CC6666", "#99CC99"),
                                                     conf.level = 0, margin = 1,std='ind.max',
                                                     main = "Confusion Matrix")+ 
                                            text(-0.4,0.4, "TN", cex=1) + 
                                            text(0.4, -0.4, "TP", cex=1) + 
                                            text(0.4,0.4, "FP", cex=1) + 
                                            text(-0.4, -0.4, "FN", cex=1))
        pred.lda <- predict(ldafit, newdata = data.frame(cough=1*!!input$cough,fever=1*!!input$fever,sore_throat=1*input$sore,shortness_of_breath=1*input$breath,head_ache=1*input$head,age_60_and_above=1*input$age,gender=1*input$male))
        output$result_from_predictive_model <-renderPrint(pred.lda)
    })
    
    ## QDA part
    observeEvent(input$run_qda, {
        trControl <- trainControl(method  = "cv",number  = 5)
        qdafit <-train(as.factor(result)~cough+fever+sore_throat+shortness_of_breath
                       +head_ache+age_60_and_above+gender,
                       method     = "qda",
                       trControl  = trControl,
                       data=data3)
        output$output_of_model <- renderPrint(qdafit)
        pred.class = predict(qdafit, data3)
        table_matrix<-table(pred.class, data3$result)
        output$model_plot <- renderPlot(fourfoldplot(table_matrix, color = c("#CC6666", "#99CC99"),
                                                     conf.level = 0, margin = 1,std='ind.max',
                                                     main = "Confusion Matrix")+ 
                                            text(-0.4,0.4, "TN", cex=1) + 
                                            text(0.4, -0.4, "TP", cex=1) + 
                                            text(0.4,0.4, "FP", cex=1) + 
                                            text(-0.4, -0.4, "FN", cex=1))
        pred.qda<- predict(qdafit, newdata = data.frame(cough=1*!!input$cough,fever=1*!!input$fever,sore_throat=1*input$sore,shortness_of_breath=1*input$breath,head_ache=1*input$head,age_60_and_above=1*input$age,gender=1*input$male))
        output$result_from_predictive_model <-renderPrint(pred.qda)
    })

    
    ## Decision Tree part
    
    output$choose_y <- renderUI({
        y_choices <- c("result","Next Part Coming Soon")
        selectInput('choose_y', label = 'Choose Target Variable', choices = y_choices)
    })
    
    output$choose_x <- renderUI({
        x_choices <- names(data1)[!names(data1) %in% input$choose_y]
        checkboxGroupInput('choose_x', label = 'Choose Predictors', choices = x_choices)
    })
    
    observeEvent(input$c50, {
        form <- paste(isolate(input$choose_y), '~', paste(isolate(input$choose_x), collapse = '+'))
        fit <- rpart(form,data = data1, method = 'class')
        output$tree_plot_c50 <- renderPlot({
            rpart.plot(fit)
        })
        output$tree_summary <- renderPrint(summary(fit))
    })
    

}

shinyApp(ui, server)
