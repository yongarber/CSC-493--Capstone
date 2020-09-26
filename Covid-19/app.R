#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(leaflet)
library(usmap)
library(shinythemes)
library(plotly)
library(tmap)
library(spData)
library(rstatix)
library(broom)
library(bootstraplib)


##############################################################################################
# UI Code
##############################################################################################
ui <- fluidPage(
    bootstrap(),
    titlePanel("Covid-19 Diagnosis Tool"), #title
    
    tabsetPanel( #starting 3 tabs
        type = "pills",
        # Tab 1
        tabPanel("EDA",
                 
                 h3("Explore the Data"),
                 ## Row 1
                 fluidRow(
                     ### Column 1
                     column(4,
                            wellPanel(
                                sliderInput("observation", "How many observations do you want?", min = 0, max =1050000, value = c(0,500)),
                                radioButtons("sick",
                                             "Positive or negative observations?",
                                             c("Positive", "Negative")) # Input tab 1
                            )
                     ),
                     ### Column 2
                     column(8,
                            plotOutput("freq")) # Output tab 1
                 )
        ), # End of tab one with two fluidrow.
        
        
        # Tab 2
        tabPanel("Predictive models",
                 ## Row 1
                 fluidRow(
                     ### Column 1
                     column(2,
                            wellPanel(
                                radioButtons("map_color",
                                             "Select variable to color map",
                                             c("# of Tweets", "Average # of Hashtags", "SenticNet Score", "SlangSD Score")
                                )
                            )
                     ),
                     ### Column 2
                     column(10,
                            tabsetPanel(
                                #### Tab 1: US states
                                tabPanel("Trends by U.S. state",
                                         tmapOutput("dyn_us", width = "100%", height = 500)),
                                tabPanel("Trends by country",
                                         tmapOutput("dyn_world", width = "100%", height = 500))
                            )
                            # h2("Trends by U.S. state"),
                            # tmapOutput("dyn_us")
                     )
                 ),
                 
                 ## Row 2
                 fluidRow(
                     h3("Inspect individual tweets"),
                     ### Column 1
                     column(12,
                            leafletOutput("map")
                     )
                 )
                 
                 
        ),# End of tab two with two fluidrow.
        
        
        # Tab 3
        tabPanel("Decision Tree",
                 ## Row 1
                 fluidRow(
                     ### Column 1
                     column(4,
                            wellPanel(
                                
                            )
                     ), # End column 1
                     
                     ### Column 2
                     column(8,
                            plotOutput("boxplot1"),
                     ) # End column 2
                     
                 )
        ) #  End of tab three with 4 fluidrow.
    ) # End of all tabs
) # end of UI



# ##############################################################################################
# # Server Code
# ##############################################################################################
 server <- function(input, output, session) {
#     output$freq <- renderPlot(height = 350,{tweets %>%
#             filter(retweet_count<input$retweet[2] & retweet_count>input$retweet[1])%>%
#             filter(followers_count>input$follower)%>%
#             ggplot(aes(x=retweet_count))+
#             theme_bw()+
#             geom_freqpoly(bins= 1000)})
#     
#     output$country <- renderPlot(height = 350,{tweets %>%
#             group_by(country_loc) %>%
#             filter(n() >= 50) %>%
#             ungroup %>%
#             filter(is.na(country_loc) != TRUE)%>%
#             ggplot(aes(x=country_loc))+
#             geom_bar(color = "black", fill = "#FBEE1F", size = 1.5)+
#             theme(axis.text.x = element_text(angle = 60, hjust = 1))+
#             theme_bw()+
#             xlab("Countries with 50+ tweets")+
#             ylab("# of tweets")})
#     
#     output$state <- renderPlot(height = 350,{ggplot(data= states,aes(x=state, y=count))+
#             geom_bar(stat = "identity", color = "black", fill = "#FBEE1F", size = 1.5)+
#             theme(axis.text.x = element_text(angle = 60, hjust = 1))+
#             theme_bw()+
#             xlab("States with most tweets")+
#             ylab("# of tweets")})
#     
#     # term frequency plots
#     output$termFreq_plot <- renderPlotly({
#         # Depending on input selection, show terms, hashtags, mentions, or emojis
#         switch(input$term_type,
#                "Terms" = tweets_words,
#                "Hashtags" = tweets_hashtags,
#                "User mentions" = tweets_mentions,
#                "Emojis" = tweets_emojis %>% mutate(word = description)) %>%
#             count(word) %>%
#             top_n(20) %>%
#             mutate(word = reorder(word, n)) ->
#             df
#         ggplot(df, aes(x = word, y = n, fill = word)) +
#             geom_col(show.legend = FALSE) +
#             scale_y_log10() +
#             xlab(
#                 switch(input$term_type,
#                        "Terms" = "Word",
#                        "Hashtags" = "Hashtag",
#                        "User mentions" = "Mention",
#                        "Emojis" = "Emoji")
#             ) +
#             ylab("Count") +
#             theme_bw() +
#             coord_flip() -> myPlot
#         
#         myPlot %>%
#             ggplotly(tooltip = c("x", "y")) %>%
#             layout(showlegend = FALSE)
#     })
#     
#     
#     # topic modeling plot
#     # INSERT HERE
#     
#     
#     # Create interactive map to show individual tweets
#     output$map <- renderLeaflet({
#         leaflet() %>%
#             addTiles() %>%
#             setView(lng = -98.58,
#                     lat = 39.0,
#                     zoom = 4)  %>%
#             addMarkers(data = tweets,
#                        lat = ~map_lat,
#                        lng = ~map_lon,
#                        popup = ~text,
#                        icon = twitterIcon,
#                        clusterOptions = markerClusterOptions())
#     })
#     
#     # Create dynamic US map
#     #
#     # known issue with tmap not taking reactive inputs:
#     # https://stackoverflow.com/questions/59643313/reactivity-problem-while-using-rendertmap-within-r-shiny
#     #
#     # create tmap reactive
#     tmap_us <- reactive({
#         tweets_states <- tweets_states %>%
#             mutate(log_numTweets = log(num_tweets))
#         us_df <- left_join(us_states, tweets_states, by = c("NAME" = "state"))
#         us_df <- us_df[,c("NAME", setdiff(names(us_df), "NAME"))]
#         
#         hi_df <- left_join(hawaii, tweets_states, by = c("NAME" = "state"))
#         hi_df <- hi_df[,c("NAME", setdiff(names(hi_df), "NAME"))]
#         
#         ak_df <- left_join(alaska, tweets_states, by = c("NAME" = "state"))
#         ak_df <- ak_df[,c("NAME", setdiff(names(ak_df), "NAME"))]
#         
#         switch(input$map_color,
#                "# of Tweets" = {
#                    tmap_options(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldTopoMap"))
#                    tmap_mode("view")
#                    tm_shape(us_df, projection = 2163) +
#                        tm_polygons("log_numTweets", title = "# of Tweets (Nat. log-scale)") +
#                        tm_shape(ak_df) +
#                        tm_polygons("log_numTweets", legend.show = FALSE) +
#                        tm_shape(hi_df) +
#                        tm_polygons("log_numTweets", legend.show = FALSE) +
#                        tm_view(set.view = c(-98.58, 49.0, 3.3))
#                },
#                "Average # of Hashtags" = {
#                    tmap_mode("view")
#                    tm_shape(us_df, projection = 2163) +
#                        tm_polygons("ave_num_ht", title = "Average # of Hashtags") +
#                        tm_shape(ak_df) +
#                        tm_polygons("ave_num_ht", legend.show = FALSE) +
#                        tm_shape(hi_df) +
#                        tm_polygons("ave_num_ht", legend.show = FALSE) +
#                        tm_view(set.view = c(-98.58, 49.0, 3.3))
#                },
#                "SenticNet Score" = {
#                    tmap_mode("view")
#                    tm_shape(us_df, projection = 2163) +
#                        tm_polygons("sent_score_senticnet", title = "Average SenticNet Score") +
#                        tm_shape(ak_df) +
#                        tm_polygons("sent_score_senticnet", legend.show = FALSE) +
#                        tm_shape(hi_df) +
#                        tm_polygons("sent_score_senticnet", legend.show = FALSE) +
#                        tm_view(set.view = c(-98.58, 49.0, 3.3))
#                },
#                "SlangSD Score" = {
#                    tmap_mode("view")
#                    tm_shape(us_df, projection = 2163) +
#                        tm_polygons("sent_score_slang", title = "Average SlangSD Score") +
#                        tm_shape(ak_df) +
#                        tm_polygons("sent_score_slang", legend.show = FALSE) +
#                        tm_shape(hi_df) +
#                        tm_polygons("sent_score_slang", legend.show = FALSE) +
#                        tm_view(set.view = c(-98.58, 49.0, 3.3))
#                }
#         )
#     })
#     
#     # create output
#     output$dyn_us <- renderTmap({tmap_us()})
#     
#     
#     # Create dynamic world map
#     #
#     # create tmap reactive
#     tmap_world <- reactive({
#         map_tweets <- tweets_world[,c("iso_a3", setdiff(names(tweets_world), "iso_a3"))]
#         switch(input$map_color,
#                "# of Tweets" = {
#                    map_tweets %>%
#                        mutate(log_numTweets = log(num_tweets)) ->
#                        map_tweets
#                    tmap_mode("view")
#                    tm_shape(map_tweets) +
#                        tm_polygons("log_numTweets", title = "# of Tweets (Nat. log-scale)") +
#                        tm_view(set.view = c(0, 25, 1.5))
#                },
#                "Average # of Hashtags" = {
#                    tmap_mode("view")
#                    tm_shape(map_tweets) +
#                        tm_polygons("ave_num_ht", title = "Average # of Hashtags") +
#                        tm_view(set.view = c(0, 25, 1.5))
#                },
#                "SenticNet Score" = {
#                    tmap_mode("view")
#                    tm_shape(map_tweets) +
#                        tm_polygons("sent_score_senticnet", title = "Average SenticNet Score") +
#                        tm_view(set.view = c(0, 25, 1.5))
#                },
#                "SlangSD Score" = {
#                    tmap_mode("view")
#                    tm_shape(map_tweets) +
#                        tm_polygons("sent_score_slang", title = "Average SlangSD Score") +
#                        tm_view(set.view = c(0, 25, 1.5))
#                }
#         )
#     })
#     
#     # create output
#     output$dyn_world <- renderTmap({tmap_world()})
#     
#     
#     # Show levels for the discrete variable selected in input$selected_incl
#     output$selected_incl <- renderUI({
#         checkboxGroupInput(inputId = "incl_levels",
#                            label = "Boxplot: Select one or more hashtags",
#                            choices = choices_incl(),
#                            selected = choices_incl())
#     })
#     
#     choices_incl <- reactive({
#         df_incl <- select(tweets_incl, input$incl)
#         return(levels(df_incl[[1]]))      
#     })
#     
#     
#     output$barplot <- renderPlot(height=350,{
#         if (input$incl == "Inclusive") {
#             b <- ggplot(tweets_incl, aes(x = word)) +
#                 labs(title = "Number of Tweets by Hashtag, Inclusive*",
#                      x = "Hashtag", y = "Count", caption = "*More than one hashtag may be counted in the same tweet")
#         } #end if inclusive
#         
#         else if (input$incl == "Exclusive") {
#             b <- ggplot(tweets_excl, aes(x = word)) +
#                 labs(title = "Number of Tweets by Hashtag, Exclusive*",
#                      x = "Hashtag", y = "Count", caption = "*Limited to tweets with only specified BLM-related hashtag")
#             
#         } #end if exclusive
#         
#         b <- b +
#             geom_bar(stat = "count", color = "black", fill = "#FBEE1F", size = 1.5) +
#             scale_y_continuous(limits = c(0, 30000)) +
#             theme_bw() +
#             theme(plot.title = element_text(hjust = 0.5))
#         
#         b
#     }) 
#     
#     
#     
#     # output small boxplot for tab 3
#     
#     output$boxplot1 <- renderPlot(height = 350,{
#         
#         #generate df based on inputs selected
#         if (input$incl == "Inclusive") {
#             df_incl <- select(tweets_incl, input$sent, !!input$incl) %>%
#                 filter(!!(as.name(input$incl)) %in% input$incl_levels)
#             
#             p <- ggplot(data = df_incl, aes_string(x = input$incl,
#                                                    y = input$sent,
#                                                    fill = input$incl)) +
#                 labs(title = "Sentiment Score by Hashtag, Inclusive*",
#                      x = "Hashtag", caption = "*More than one hashtag may be counted in the same tweet")
#             
#         } else if (input$incl == "Exclusive") {
#             df_excl <- select(tweets_excl, input$sent, !!input$incl) %>%
#                 filter(!!(as.name(input$incl)) %in% input$incl_levels)
#             
#             p <- ggplot(data = df_excl, aes_string(x = input$incl,
#                                                    y = input$sent,
#                                                    fill = input$incl)) +
#                 labs(title = "Sentiment Score by Hashtag, Exclusive*",
#                      x = "Hashtag", caption = "*Limited to tweets with only specified BLM-related hashtag")
#             
#         }
#         
#         p <- p + 
#             geom_boxplot(is.na = FALSE) +
#             scale_fill_manual(values=cbbPalette, na.translate = F) +
#             theme_bw() +
#             theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#         p
#         
#     })  # end renderPlot for small boxplot
#     
#     #Output anova table for tab 1
#     output$anova_results <- renderTable(
#         striped = TRUE,
#         bordered = TRUE,
#         hover = TRUE,
#         spacing = "s",
#         { # start expression
#             
#             if (input$incl == "Exclusive") {
#                 tweets_excl %>%
#                     filter(Exclusive != "All blm-related hashtags") ->
#                     aov_df
#                 aov_results <- aov(aov_df[[input$sent]] ~ Exclusive, data = aov_df)
#                 
#             } else {  
#                 
#                 #Add weight to inclusive dataset (to give less weight to tweets with more than 1 hashtag)  
#                 tweets_incl %>%
#                     add_count(status_id) %>%
#                     mutate(weight = case_when(n == 4 ~ 0.25,
#                                               n == 3 ~ 0.33,
#                                               n == 2 ~ 0.5,
#                                               n == 1 ~ 1)) ->
#                     aov_df
#                 
#                 model = lm(aov_df[[input$sent]] ~ Inclusive, data = aov_df, weights = weight)
#                 aov_results <- aov(model)
#                 
#             }
#             
#             #Tukey Honest Significant Difference for multiple pairwise-comparison between the means of groups
#             TukeyHSD(aov_results) %>%
#                 tidy() %>%
#                 select("Comparison" = 2, 
#                        "Estimate" = estimate, 
#                        "95% Lower" = conf.low, 
#                        "95% Higher" = conf.high, 
#                        "Adjusted P-value" = adj.p.value)
#             
#         }) #end renderTable
#     
#     
#     # Show levels for the discrete variable selected in input$selected_hash
#     output$selected_hash <- renderUI({
#         checkboxGroupInput(inputId = "show_levels",
#                            label = "Boxplot: Select hashtag combinations of interest",
#                            choices = choices_hash(),
#                            selected = choices_hash())
#     })
#     
#     choices_hash <- reactive({
#         df <- select(tweets_analysis, input$hash)
#         return(levels(df[[1]]))
#     })
#     
#     #output large boxplot for tab 3
#     output$boxplot <- renderPlot({
#         
#         #generate df based on inputs selected
#         df <- select(tweets_analysis, input$sent, input$hash) %>%
#             filter(!!(as.name(input$hash)) %in% input$show_levels)
#         
#         ggplot(data = df, aes_string(x = input$hash,
#                                      y = input$sent,
#                                      fill = input$hash)) +
#             geom_boxplot() +
#             scale_fill_manual(values=cbbPalette) +
#             theme_bw() +
#             theme(legend.position = "none") +
#             coord_flip()
#         
#     }) #end renderPlot for boxplot
#     
} #end of server

shinyApp(ui, server)

