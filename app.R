library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Steel Paradox Counterfactual Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Policy Parameters"),
      
      sliderInput("baseline_rate",
                  "Baseline Purchase Rate (firearms per 100k residents per month):",
                  min = 200,
                  max = 800,
                  value = 530,
                  step = 25),
    
      conditionalPanel(
        condition = "input.baseline_rate == 530",
        tags$div(style = "color: #2E86AB; font-style: italic; margin-top: -10px; margin-bottom: 10px;",
                 "530 ≈ Oregon baseline rate pre-114 election")
      ),
      sliderInput("reduction_pct",
                  "Policy Reduction in Purchases (%):",
                  min = 0,
                  max = 100,
                  value = 20,
                  step = 5),
      
      numericInput("initial_stock",
                   HTML("Steel Paradox Anticipation Stock<br><span style='font-weight:normal;'>(Excess background checks from policy anticipation)</span>"),                   value = 63000,
                   min = 0,
                   max = 100000,
                   step = 1000),
      
      sliderInput("trend_rate",
                  HTML("Monthly Harvesting Rate  (%):<br><span style='font-weight:normal;'>(only for Steel Paradox + no enactment)</span>"),
                  min = 0,
                  max = 100,
                  value = 0,
                  step = 5),
      
      numericInput("scale_factor",
                   "Scale Stock Values By:",
                   value = 1,
                   min = 0.01,
                   max = 100,
                   step = 0.1),
      
      tags$div(style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
               tags$p(style = "margin: 0;", "Conversion of background checks to firearms: paper uses 1.05 conversion rate")
      ),
      
      hr(),
      
      h5("Scenario Summary:"),
      verbatimTextOutput("summary_text")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Excess Stock Over Time",
                 plotOutput("stock_plot", height = "500px")),
       
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive data generation
  scenario_data <- reactive({
    # Time parameters
    # Reactive data generation
   
      n_months <- 24  # Total months to simulate
      months <- 0:(n_months-1)  # Event time: 0, 1, 2, ...
      
      policy_month <-  0
      
      # Monthly harvesting rate (for baseline demand)
      monthly_trend <- (1 + input$trend_rate/100)^(1/12)
      
      # Initialize vectors
      baseline_purchases <- rep(input$baseline_rate*42, n_months)
      policy_purchases <- rep(input$baseline_rate*42, n_months)  # No harvesting applied
      
      # Apply harvesting rate to baseline purchases only
      for (i in 2:n_months) {
        baseline_purchases[i] <- baseline_purchases[i] * (monthly_trend ^ (i-1))
      }
      
      # Apply policy reduction after implementation
      if (policy_month < n_months) {
        policy_purchases[(policy_month+1):n_months] <- 
          policy_purchases[(policy_month+1):n_months] * (1 - input$reduction_pct/100)
      }
      
      # Calculate stock over time
      baseline_stock <- numeric(n_months)
      policy_stock <- numeric(n_months)
      
      baseline_stock[1] <- input$initial_stock
      policy_stock[1] <- input$initial_stock
      
      for (i in 2:n_months) {
        # Add to stock (no trend applied in this loop anymore)
        baseline_stock[i] <- baseline_stock[i-1] + baseline_purchases[i]
        policy_stock[i] <- policy_stock[i-1] + policy_purchases[i]
      }
      
      # Create data frame
      data.frame(
        month = months,
        baseline_purchases = baseline_purchases,
        policy_purchases = policy_purchases,
        baseline_stock = baseline_stock,
        policy_stock = policy_stock,
        impact = baseline_stock - policy_stock,
        scenario = ifelse(months >= policy_month, "Post-Policy", "Pre-Policy")
      )
        })

    
 


# Summary text
  output$summary_text <- renderText({
    data <- scenario_data()
    final_baseline <- tail(data$baseline_stock, 1)
    final_policy <- tail(data$policy_stock, 1)
    
    # Calculate zero stock scenario
    zero_stock <- numeric(nrow(data))
    zero_stock[1] <- 0
    for (i in 2:nrow(data)) {
      zero_stock[i] <- zero_stock[i-1] + data$baseline_purchases[i]
    }
    final_zero <- tail(zero_stock, 1)
    
    difference_pol <- final_policy - final_zero
    pct_reduction_pol <- (difference_pol / final_zero) * 100
    
    
    paste0("By month 24 (end of year 2):\n",
           "If policy enacted, compared \n",
           "to if no Steel Paradox\n",
           "and no policy implementation, \n",
           "stock changes by:\n",
           "\n", format(round(difference_pol * input$scale_factor, 0), big.mark = ","), 
           " (", round(pct_reduction_pol, 1), "%)")
    
  })

  # Stock over time plot
  output$stock_plot <- renderPlot({
    data <- scenario_data()
    policy_month <- 0
    
    # Calculate zero initial stock scenario
    zero_stock <- numeric(nrow(data))
    zero_stock[1] <- 0
    for (i in 2:nrow(data)) {
      zero_stock[i] <- zero_stock[i-1] + data$baseline_purchases[i]
    }
    
    # Find the month where zero stock crosses policy stock
    cross_index <- which(zero_stock >= data$policy_stock)[1]
    
    if (is.na(cross_index) || cross_index == 1) {
      cross_month <- nrow(data) - 1
      max_month <- nrow(data) - 1
    } else {
      # Interpolate to find exact crossing point
      y1 <- zero_stock[cross_index - 1]
      y2 <- zero_stock[cross_index]
      p1 <- data$policy_stock[cross_index - 1]
      p2 <- data$policy_stock[cross_index]
      
      # Linear interpolation
      if ((y2 - y1) != (p2 - p1)) {
        fraction <- (p1 - y1) / ((y2 - y1) - (p2 - p1))
        cross_month <- data$month[cross_index - 1] + fraction
      } else {
        cross_month <- data$month[cross_index]
      }
      
      max_month <- min(data$month[cross_index] + 4, nrow(data) - 1)
    }
    

    
    
    ggplot(data, aes(x = month)) +
      geom_line(aes(y = baseline_stock * input$scale_factor, color = "Steel Paradox, No Enactment"), 
                linewidth = 1.2) +
      geom_line(aes(y = policy_stock * input$scale_factor, color = "Steel Paradox, Enactment"), 
                linewidth = 1.2, linetype = "solid") +
      geom_line(aes(y = zero_stock * input$scale_factor, color = "Zero Initial Anticipation"), 
                linewidth = 1.2, linetype = "dotted") +
      geom_vline(xintercept = as.numeric(policy_month), 
                 linetype = "dashed", color = "red", linewidth = 0.8) +
      annotate("text", x = policy_month, y = max(data$baseline_stock) * 0.95,
               label = "Policy Implementation", angle = 90, vjust = -0.5, size = 3.5) +
      geom_vline(xintercept = cross_month, 
                 linetype = "dashed", color = "black", linewidth = 0.8) +
      annotate("text", x = cross_month, y = max(data$baseline_stock) * 0.85,
               label = "Zero Stock Crosses Policy", angle = 90, vjust = -0.5, size = 3.5) +
      scale_x_continuous(limits = c(0, max_month),
                        breaks = c(seq(0, max_month, by = 2), cross_month),
                        labels = function(x) ifelse(x == cross_month, 
                                                    as.character(round(cross_month, 1)), 
                                                    as.character(x))) +      
      scale_y_continuous(limits = c(0, ifelse(!is.na(cross_index) && cross_index < nrow(data),
                                              data$baseline_stock[min(cross_index + 4, nrow(data))] * input$scale_factor,
                                              data$baseline_stock[nrow(data)] * input$scale_factor)),
                         labels = scales::comma) +
      scale_color_manual(values = c("Steel Paradox, No Enactment" = "#2E86AB", 
                                    "Steel Paradox, Enactment" = "#A23B72",
                                    "Zero Initial Anticipation" = "#06A77D"  )) +
      labs(title = "Firearm Stock Under Different Scenarios",
           subtitle = paste0(input$reduction_pct, "% reduction in purchases starting at month 0" 
                             ),
           x = "Month",
           y = "Stock",
           color = "Scenario") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold", size = 16),
            panel.grid.minor = element_blank())
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)