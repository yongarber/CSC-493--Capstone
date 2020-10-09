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

###################################
#   Pre-Analysis                  #
###################################

## Data Importing and cleaning
#data <- vroom::vroom("../data/estate.csv")

data1 <- vroom::vroom("../covid_data.csv")



###################################
#   UI                            #
###################################

ui <- fluidPage(
    titlePanel("Covid-19 Diagnosis Tool"),
    tabsetPanel(
        tabPanel("Data",fluidRow(
        column(12, DT::dataTableOutput("spreadsheet"))
        )),
        tabPanel("Predictive Models", fluidRow(column(4,
                                                      radioButtons("model", "Pick your model:",
                                                                   c("Logistic Regression" = "logistic",
                                                                     "LDA" = "lda",
                                                                     "QDA" = "qda"))
        ),
        column(4, plotOutput("output_of_model")),
        column(2, checkboxInput("f1", "Cough?", FALSE),
               checkboxInput("check", "Fever?", FALSE),
               checkboxInput("check", "Sore throat?", FALSE),
               checkboxInput("check", "Shortness of breath?", FALSE),
               checkboxInput("check", "Head ache?", FALSE),
               checkboxInput("check", "Are you older than 60?", FALSE),
               checkboxInput("check", "Head ache?", FALSE)),
        column(2, plotOutput("result_from_predictive_model"))
        ), 
        fluidRow(column(4,verbatimTextOutput("text")),
                 column(4,plotOutput("plotOLS1")),
                 column(4,plotOutput("plotOLS2")))
        ),
        tabPanel("Decision Tree", fluidRow(column(12,plotOutput("decision_tree")
        )
        ))
    )
)

###################################
#   Server                        #
###################################


server <- function(input, output, session) {
    
    output$spreadsheet <-
        DT::renderDataTable(data1, options= list(pageLength = 10))
    

}

shinyApp(ui, server)
