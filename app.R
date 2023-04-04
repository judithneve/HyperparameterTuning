library(shiny)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("How do different hyperparameter tuning procedures impact model predictive performance?"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      radioButtons(
        inputId = "study",
        label = "Which study do you want to look at?",
        choices = list(
          "Study 1 (hyperparameters to tune)" = "Study1",
          "Study 2 (optimisation metrics)"    = "Study2",
          "Study 3 (search algorithms)"       = "Study3")),
      checkboxGroupInput(
        inputId = "measure",
        label = "Performance measure",
        choices = list(
          "Discrimination (AUC)" = "AUC",
          "Calibration (slope)" = "CalSlope",
          "Calibration (intercept)" = "CalIntercept",
          "Brier score" = "BrierScore",
          "Logarithmic loss" = "LogLoss",
          "Accuracy" = "Accuracy",
          "Cohen's Kappa" = "CohensKappa"
         ),
        selected = "CalSlope"),
      checkboxGroupInput(
        inputId = "p_chosen",
        label = "Number of predictors",
        choices = list(
          "8" = "8",
          "16" = "16"
        ),
        selected = "8"),
      checkboxGroupInput(
        inputId = "EF_chosen",
        label = "Event fraction",
        choices = list(
          "0.1" = "0.1",
          "0.3" = "0.3",
          "0.5" = "0.5"
        ),
        selected = "0.3"),
      checkboxGroupInput(
        inputId = "n_chosen",
        label = "Proportion of the minimum sample size",
        choices = list(
          "Half" = "0.5",
          "Whole" = "1"
        ),
        selected = "1"),
      checkboxGroupInput(
        inputId = "levels",
        label = "Figure out what this should say",
        choices = "try",
        selected = "1"),
      width = 3
    ),
    
    mainPanel(
      plotOutput(outputId = "perfplot",
                 height = "1000px"),
      width = 9
    )
  )
)

server <- function(input, output) {
  load("appdata.RData")
  
  observe({
    updateCheckboxGroupInput(inputId = "levels",
                             choices = if (input$study == "Study1") {HP.labs} else {opt.labs},
                             selected = if (input$study == "Study1") {HP.labs} else {opt.labs})
  })
  
  output$perfplot <- renderPlot({
    
    dat[[input$study]] %>%
      filter(Metric %in% input$measure) %>%
      filter(n_prop %in% as.numeric(input$n_chosen),
             p      %in% as.numeric(input$p_chosen),
             EF     %in% as.numeric(input$EF_chosen)) %>%
      filter(variable %in% input$levels) %>% 
      ggplot(aes(x = time,
                 y = Performance,
                 group = variable)) +
      geom_point(aes(col = variable,
                     shape = variable)) +
      xlim(0, max((dat[[input$study]] %>%
                     filter(Metric %in% input$measure) %>%
                     filter(variable %in% input$levels) %>% 
                     filter(n_prop %in% as.numeric(input$n_chosen),
                            p      %in% as.numeric(input$p_chosen),
                            EF     %in% as.numeric(input$EF_chosen)))$time)) +
      geom_point(data = df_scale_setter[[input$study]] %>%
                   filter(Metric %in% input$measure) %>% 
                   mutate(Metric = as.character(Metric)), alpha = 0) +
      geom_hline(data = df_targets[[input$study]] %>%
                   filter(Metric %in% input$measure) %>% 
                   mutate(Metric = as.character(Metric)), aes(yintercept = Target), col = "red", lty = "dotted") +
      facet_wrap(scenario ~ Metric,
                 scales = "free",
                 labeller = labeller(Metric = Metric.labs),
                 ncol = length(input$measure)) +
      theme_classic() +
      theme(
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, 'cm'),
        legend.background = element_rect(),
        panel.spacing = unit(1.2, "lines"),
        plot.caption = element_text(hjust = 0)) +
      scale_color_mine(input$study) +
      scale_shape_mine(input$study) +
      labs(caption = "Red dotted lines show ideal performance.",
           x = "Runtime (seconds)",
           col = ifelse(input$study == "Study1",
                        "Hyperparameter combination",
                        ifelse(input$study == "Study2",
                               "Optimisation metric",
                               "Search algorithm")),
           shape = ifelse(input$study == "Study1",
                          "Hyperparameter combination",
                          ifelse(input$study == "Study2",
                                 "Optimisation metric",
                                 "Search algorithm")))
    
  })
  
}


shinyApp(ui = ui, server = server)
