
######################################################################################################################
# This a shiny dashboard report with a simulated marketing data on customer spending given some variables like age 
# and income.
# Again, this is for showcasing what I can do with shiny to be showcased to employers/recruiters

#Finalized on June 21, 2025 13:54 GMT
# Additional files: viz_report_file.Rmd, watermark_file_2

#Published on June 25, 2025, 11:45 GMT with all features showing for Emma to review and then a new one without some
# of the features will be published. 


#The download button and the prediction tab have all being disabled for deployment until asked
######################################################################################################################
###Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(shinyWidgets)
library(DT)
library(broom)
library(kableExtra)

######################
###Data Simulation
# Simulate dataset
set.seed(123)
n <- 1500
simulated_data <- data.frame(
                      Age = round(rnorm(n, 40, 12)),
                      Income = round(rnorm(n, 60000, 15000)),
                      EducationLevel = sample(c("High School", "Bachelor", "Master", "PhD"), n, replace = TRUE),
                      Satisfaction = round(runif(n, 1, 10), 1),
                      SpendingScore = round(rnorm(n, 50, 20)),
                      LoyaltyYears = round(runif(n, 0, 10), 1)
                      )
simulated_data$EducationLevel <- as.factor(simulated_data$EducationLevel)

simulated_data <- simulated_data %>%
                   mutate(Age = ifelse(Age <= 19, 20, Age),
                          SpendingScore = ifelse(SpendingScore <= 0, 1, SpendingScore))
 

# Fit model
model <- lm(SpendingScore ~ Age + Income + EducationLevel + Satisfaction + LoyaltyYears, data = simulated_data)


############################
## The shiny code
###########################

############
### UI
###########
ui <- dashboardPage(
                    skin = 
                         'green',
                         #"purple",
                    dashboardHeader(title = "Spending Behavior"),
                    dashboardSidebar(br(), h6(" Created by: David Senyo A."), br(), hr(), 
                                     sidebarMenu( 
                                                 menuItem("Data Overview", tabName = "overview"), hr(),
                                                 menuItem("Visualizations", tabName = "viz"), hr(),
                                                 menuItem("Inferential Analysis", tabName = "inference"), hr()
                                                # menuItem("Predict an Outcome", tabName = "predict")
                                                
                                                 )
                                    ),
                  dashboardBody(
                               tabItems(
      
      ### Overview tab
                                        tabItem(tabName = "overview",
                                                fluidRow(
                                                   h4("Some Basic Overview Statistics"), hr(), 
                                                        valueBoxOutput("meanAge"),
                                                        valueBoxOutput("meanIncome"),
                                                        valueBoxOutput("meanSpending")
                                                          ), br(), hr(),
                                                fluidRow(
                                                         box(title = "Dataset Summary", width = 12, htmlOutput("summary"))
                                                           
                                                         )
                                               ),
      
      ### Visualizations tab
                                         tabItem(tabName = "viz",
                                                 fluidRow(
                                                          #box(title = "Download Visualization Report", width = 12, downloadButton("downloadVizFile2", "Download Report")),
                                                          box(title = "Correlation Heatmap", width = 6, plotOutput("corHeat")),
                                                          box(title = "Boxplot: Spending by Education", width = 6, plotOutput("boxEdu"))
                                                          ),
                                                 fluidRow(
                                                          box(title = "Density Plot: Satisfaction", width = 6, plotOutput("densitySat")),
                                                          box(title = "Pairs Plot", width = 6, plotOutput("pairsPlot"))

                                                        )
                                                ),
                                        
      
      
      
      ### Inference tab
                                          tabItem(tabName = "inference",
                                                  fluidRow(
                                                           box(title = "Regression Output", width = 12,
                                                           h5(em("This Regresion model shows how much a customer typically 
                                                              spends (not in $ terms but points) given these variables")),     
                                                           DTOutput("regTable"))
                                                           ),
                                                  fluidRow(
                                                           box(title = "Residuals vs Fitted", width = 6, plotOutput("residPlot")),
                                                           box(title = "Cook's Distance", width = 6, plotOutput("cookPlot"))
                                                          )
                                                    )
      
      ### Prediction tab
                                          # tabItem(tabName = "predict",
                                          #         fluidRow(
                                          #                 box(title = "Input Values", width = 6,
                                          #                     numericInput("inputAge", "Age", value = 40, min = 18, max = 70),
                                          #                     numericInput("inputIncome", "Income", value = 60000, min = 20000, max = 150000),
                                          #                     selectInput("inputEdu", "Education Level", choices = levels(simulated_data$EducationLevel)),
                                          #                     sliderInput("inputSat", "Satisfaction", min = 1, max = 10, value = 5, step = 0.1),
                                          #                     sliderInput("inputLoyal", "Loyalty Years", min = 0, max = 10, value = 3, step = 0.1)
                                          #                     ),
                                          #                 box(title = "Predicted Spending Score", width = 6,
                                          #                     valueBoxOutput("predictionValue"),
                                          #                     htmlOutput("predictionCI"),
                                          #                     htmlOutput("predictNote")
                                          #                      )
                                          #                  )
                                          #         )
                                        )
                                )
                    )


################
### Server
###############

server <- function(input, output) {
  ##########################
  ### Overview value boxes
  ##########################
  output$meanAge <- renderValueBox({
                                   valueBox(round(mean(simulated_data$Age), 1), "Average Age", icon = icon("user"), color = "yellow")
                                    })
  
  output$meanIncome <- renderValueBox({
                                      valueBox(paste0("$", formatC(mean(simulated_data$Income), format = "f", digits = 0, big.mark = ",")),
                                     "Average Income", icon = icon("dollar-sign"), color = "green")
                                      })
  
  output$meanSpending <- renderValueBox({
                                        valueBox(round(mean(simulated_data$SpendingScore), 1), "Average Spending Score", icon = icon("chart-line"), color = "purple")
                                        })
  
  
  output$summary<-renderUI({ 
    numeric_data <- select_if(simulated_data, is.numeric)
                            summary_df <- data.frame(
                                                    Variable = names(numeric_data),
                                                    Min = sapply(numeric_data, min, na.rm = TRUE),
                                                    Q1 = sapply(numeric_data, quantile, probs = 0.25, na.rm = TRUE),
                                                    Q2 = sapply(numeric_data, median, na.rm = TRUE),
                                                    Mean = round(sapply(numeric_data, mean, na.rm = TRUE), 2),
                                                    Q3 = sapply(numeric_data, quantile, probs = 0.75, na.rm = TRUE),
                                                    Max = sapply(numeric_data, max, na.rm = TRUE),
                                                    row.names = NULL
                                                    )
    
                            kbl(summary_df, format = 'html')%>%kable_styling(bootstrap_options =c("striped", "hover"), fixed_thead = T)%>%
                                column_spec(5, background = '#cef3bb', color = '#40091c')%>%
                                HTML()
  })
  
  
  #########################
  ### Visualization plots 
  #########################
  output$corHeat <- renderPlot({
                                num_data <- simulated_data %>% select(Age, Income, Satisfaction, SpendingScore, LoyaltyYears)
                                corr <- cor(num_data)
                                corrplot(corr, method = "color", type = "upper", tl.cex = 0.8)
                               })
  
  output$boxEdu <- renderPlot({
                              ggplot(simulated_data, aes(x = EducationLevel, y = SpendingScore, fill = EducationLevel)) +
                              geom_boxplot() +
                              theme_minimal() +
                              labs(y = "Spending Score", x = "Education Level") +
                              theme(legend.position = "none")
                              })
  
  output$densitySat <- renderPlot({
                                   ggplot(simulated_data, aes(x = Satisfaction)) +
                                   geom_density(fill = "#e7789e", alpha = 0.5) +
                                   theme_minimal()
                                  })
  
  output$pairsPlot <- renderPlot({
                                 ggpairs(simulated_data[, c("Age", "Income", "Satisfaction", "SpendingScore", "LoyaltyYears")])
                                 })
  
  
  
  
  ###Downloading
  # output$downloadVizFile2 <- downloadHandler(
  #   filename = function() {
  #                          paste0("visualizations-report-", Sys.Date(), ".html")
  #                         },
  #   content = function(file) {
  #                             rmarkdown::render(
  #                                               input = "viz_report_file_2.Rmd",
  #                                               output_file = file,
  #                                               params = list(
  #                                                             data = simulated_data,
  #                                                             email = "davidackuaku@gmail.com"
  #                                                            ),
  #                                              envir = new.env(parent = globalenv())
  #                                             )
  #                               }
  #                                           )

  
  ########################
  ###  Inference tables 
  ########################
  output$regTable <- renderDT({
                              conf_df <- as.data.frame(confint(model, level = 0.95))
                              conf_df <- cbind(term = rownames(conf_df), conf_df)
                              rownames(conf_df) <- NULL
                              names(conf_df) <- c("term", "CI_lower", "CI_upper")
                              
                              coef_df <- broom::tidy(model) %>%
                              select(term, estimate, statistic, p.value) %>%
                              left_join(conf_df, by = "term") %>%
                              rename(
                                      Estimate = estimate,
                                      't value' = statistic,
                                      'p value' = p.value,
                                      '95% CI Lower' = CI_lower,
                                      '95% CI Upper' = CI_upper
                                       ) %>%
                              mutate(across(where(is.numeric), ~ round(.x, 4)))
                              
                              datatable(coef_df, options = list(pageLength = 10, dom = 't'))
                                })
  
  
  
  output$residPlot <- renderPlot({
                                  plot(model$fitted.values, model$residuals,
                                       xlab = "Fitted Values", ylab = "Residuals", pch = 20, col = "darkblue")
                                       abline(h = 0, col = "red")
                                 })
  
  output$cookPlot <- renderPlot({
                                 plot(cooks.distance(model), type = "h", col = "darkred",
                                 main = "Cook's Distance", ylab = "Distance")
                                 abline(h = 4/n, col = "blue", lty = 2)
                               })
  
  ##########################
  ### Prediction tab
  #########################
  # output$predictionValue <- renderValueBox({
  #                                           new_data <- data.frame(
  #                                                                 Age = input$inputAge,
  #                                                                 Income = input$inputIncome,
  #                                                                 EducationLevel = input$inputEdu,
  #                                                                 Satisfaction = input$inputSat,
  #                                                                 LoyaltyYears = input$inputLoyal
  #                                                                  )
  #                                         pred <- predict(model, newdata = new_data, interval = "confidence")
  #                                         fit <- round(pred[1, "fit"], 2)
  #                                         valueBox(fit, "Predicted Score", icon = icon("lightbulb"), color = "navy")
  #                                           })
  # 
  # output$predictionCI <- renderUI({
  #                                 new_data <- data.frame(
  #                                                       Age = input$inputAge,
  #                                                       Income = input$inputIncome,
  #                                                       EducationLevel = input$inputEdu,
  #                                                       Satisfaction = input$inputSat,
  #                                                       LoyaltyYears = input$inputLoyal
  #                                                      )
  #                               pred <- predict(model, newdata = new_data, interval = "confidence")
  #                               lower <- round(pred[1, "lwr"], 2)
  #                               upper <- round(pred[1, "upr"], 2)
  #                               HTML(paste0("<p style='font-size:12px;'>95% CI: [", lower, ", ", upper, "]</p>"))
  # 
  #                                  })
  # 
  # 
  # output$predictNote <- renderUI({
  #                                new_data <- data.frame(
  #                                                       Age = input$inputAge,
  #                                                       Income = input$inputIncome,
  #                                                       EducationLevel = input$inputEdu,
  #                                                       Satisfaction = input$inputSat,
  #                                                       LoyaltyYears = input$inputLoyal
  #                                                     )
  # 
  #                               pred <- predict(model, newdata = new_data, interval = "confidence")
  #                               fit <- round(pred[1, "fit"], 1)
  # 
  #                               msg <- paste0(
  #                                             "<p style='font-size:11px; color:gray;'>",
  #                                             "Note: A customer aged ", input$inputAge, " with a ", input$inputEdu,
  #                                             " degree, earning $", formatC(input$inputIncome, big.mark = ","),
  #                                             ", satisfaction score ", input$inputSat,
  #                                             ", and loyalty of ", input$inputLoyal, " years is predicted to spend around ",
  #                                             "<strong>", fit, "</strong> points.",
  #                                             "</p>"
  #                                            )
  # 
  #                               HTML(msg)
  # 
  #                               })

  
  
  
  
  
}


#################
# Run App
shinyApp(ui, server)


