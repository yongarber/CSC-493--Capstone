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
#data <- vroom::vroom("../data/estate.csv")

data1 <- read.csv("Data/covid-19.csv")



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
