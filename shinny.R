

# Load necessary libraries  
library(shiny)  
library(shinydashboard) 
library(plotly)  
library(DT)  
library(forecast)  
library(randomForest)  
library(gbm)  
library(dplyr)  
library(tidyr)  
library(plotly)  

# Load your data  

# Top metro station by ridership top_metro_station <- metro.2 %>% arrange(desc(Annual_Ridership_.Millions.)) %>%slice_head(n = 1) %>% pull(Name)  
metro.2 <- read.csv("clean_data.csv")  

# Top metro station by ridership 
top_metro_station <- metro.2 %>% 
  arrange(desc(Annual_Ridership_.Millions.)) %>% 
  slice_head(n = 1) %>% 
  pull(Name) 

# Prepare ridership data  

library(dplyr)  

ridership_years <- metro.2 %>%  
  group_by(Annum) %>%  
  summarize(Total_ridership = sum(Annual_Ridership_.Millions.), .groups = 'drop')  

# Prepare additional ridership data  

ridership_years.2 <- metro.2 %>%  
  group_by(Country.region, Annum) %>%  
  summarize(Total_ridership = sum(Annual_Ridership_.Millions., na.rm = TRUE), .groups = 'drop')  

dev_data <- read.csv("development.csv")  

metro.2 <- metro.2 |> left_join(dev_data, by = "Country.region")  

corr <- cor(metro.2$System_Length_ml, metro.2$Annual_Ridership_.Millions., use = "complete.obs")

metro <- metro.2 |> filter(!is.na(Annual_Ridership_.Millions.))  


metro_years <- metro %>%  
  group_by(Name, Annum) %>%  
  summarize(Total_ridership = sum(Annual_Ridership_.Millions.,na.rm = TRUE),  
            .groups = 'drop')  


metro_growth <- metro %>%  
  group_by(Service.opened) %>%  
  summarize(Cumulative_Count = n()) %>%  
  mutate(Cumulative_Count = cumsum(Cumulative_Count))  


metro_growthdd <- metro %>%  
  group_by(Service.opened, Development.Status) %>%  
  summarize(Cumulative_Count = n(), .groups = 'drop') %>%  
  group_by(Development.Status) %>%  
  mutate(Cumulative_Count = cumsum(Cumulative_Count))  


top_10_metro_data <- metro %>% arrange(desc(Annual_Ridership_.Millions.)) %>% head(10)  



# Find the top 10 Metro Stations by total ridership  
top_metro <- metro_years %>%  
  group_by(Name) %>%  
  summarize(Total_ridership = sum(Total_ridership), .groups = 'drop') %>%  
  arrange(desc(Total_ridership)) %>%  
  slice_head(n = 10) %>%  
  pull(Name)  



# Filter the original summarized data to include only the top 10 countries  
metro_years <- metro_years %>%  
  filter(Name %in% top_metro)  


# Find the top 10 countries by total ridership  
top_countries <- ridership_years.2 %>%  
  group_by(Country.region) %>%  
  summarize(Total_ridership = sum(Total_ridership), .groups = 'drop') %>%  
  arrange(desc(Total_ridership)) %>%  
  slice_head(n = 10) %>%  
  pull(Country.region)  


# Filter the original summarized data to include only the top 10 countries  
ridership_years.2 <- ridership_years.2 %>%  
  filter(Country.region %in% top_countries)  

density_data <- density(metro.2$System_Length_ml)  
all_countries <- 195  

metro_countries <- metro.2 %>% filter(!is.na(Annual_Ridership_.Millions.)) %>% distinct(Country.region) %>% nrow()  

percentage_metro <- (metro_countries / all_countries) * 100  

prevelance_metro <- metro |> group_by(Development.Status) |>   
  summarize(prevelance_metro = (n()/189)* 100)  


prevalence_metro <- data.frame(  
  Development.Status = prevelance_metro$Development.Status,  
  prevalence_metro = prevelance_metro$prevelance_metro  # Example values  
)  


length_metro <- metro.2 |> group_by(Development.Status) |>   
  summarize(length_metro_median = median(System_Length_ml, na.rm = TRUE),   
length_metro_sum = sum(System_Length_ml, na.rm = TRUE))  

Ridership_metro <- metro.2 |> group_by(Development.Status) |>   
  summarize(Ridership_metro_median = median(Annual_Ridership_.Millions., na.rm = TRUE),
            Ridership_metro_sum = sum(Annual_Ridership_.Millions., na.rm = TRUE))

# Convert numeric year to Date format (assuming January 1st of each year)  
metro$Annum <- as.Date(paste0(metro$Annum, "-01-01"))  

# Calculate mean ridership by year  
pred_data <- metro %>%  
  group_by(Annum) %>%  
  summarize(Annual_Ridership_Millions = median(Annual_Ridership_.Millions., na.rm = TRUE), .groups = 'drop')  


# Convert Annum to Year as numeric  
pred_data$Year <- as.numeric(format(pred_data$Annum, "%Y"))  

# Split data into training and testing sets  
train_data <- pred_data   
test_data <- pred_data %>% filter(Annum >= "2009-01-01")  


# ARIMA Model  

# Convert to time series  
train_ts <- ts(train_data$Annual_Ridership_Millions, start = min(train_data$Year), end = max(train_data$Year), frequency = 1)  

# Fit ARIMA model  
fit_arima <- auto.arima(train_ts)  

# Forecast  
forecast_arima <- forecast(fit_arima, h = nrow(test_data))  

# Evaluate ARIMA  
mae_arima <- mean(abs(forecast_arima$mean - test_data$Annual_Ridership_Millions))  
rmse_arima <- sqrt(mean((forecast_arima$mean - test_data$Annual_Ridership_Millions)^2))  

# Random Forest Model  

# Fit Random Forest model  
fit_rf <- randomForest(Annual_Ridership_Millions ~ Year, data = train_data)  







# Predict  



predictions_rf <- predict(fit_rf, newdata = test_data)  







# Evaluate Random Forest  



mae_rf <- mean(abs(predictions_rf - test_data$Annual_Ridership_Millions))  



rmse_rf <- sqrt(mean((predictions_rf - test_data$Annual_Ridership_Millions)^2))  







# Gradient Boosting Model  



# Fit Gradient Boosting model with adjusted parameters  



fit_gbm <- gbm(Annual_Ridership_Millions ~ Year,  
               
               
               
               data = train_data,  
               
               
               
               distribution = "gaussian",  
               
               
               
               n.trees = 1000,  
               
               
               
               interaction.depth = 4,  
               
               
               
               n.minobsinnode = 2,  # Adjusted parameter  
               
               
               
               shrinkage = 0.07,    # Added learning rate parameter  
               
               
               
               bag.fraction = 0.75)  # Adjusting the subsampling rate  







# Predict  



predictions_gbm <- predict(fit_gbm, newdata = test_data, n.trees = 1000)  







# Evaluate Gradient Boosting  



mae_gbm <- mean(abs(predictions_gbm - test_data$Annual_Ridership_Millions))  



rmse_gbm <- sqrt(mean((predictions_gbm - test_data$Annual_Ridership_Millions)^2))  







forecast_df <- data.frame(  
  
  
  
  Year = test_data$Year,  
  
  
  
  ARIMA = as.numeric(forecast_arima$mean),  
  
  
  
  RF = predictions_rf,  
  
  
  
  GBM = predictions_gbm,  
  
  
  
  Actual = test_data$Annual_Ridership_Millions  
  
  
  
)  







# Melt dataframe for plotting  



forecast_melted <- forecast_df %>%  
  
  
  
  pivot_longer(cols = c(ARIMA, RF, GBM, Actual), names_to = "Model", values_to = "Ridership")  























# Define UI for application  



ui <- dashboardPage(  
  
  
  
  dashboardHeader(title = "Deep Learners"),  
  
  
  
  dashboardSidebar(  
    
    
    
    sidebarMenu(  
      
      
      
      menuItem("Tab 1", tabName = "tab1", icon = icon("info")),  
      
      
      
      menuItem("Tab 2", tabName = "Ridership trend", icon = icon("chart-line")),  
      
      
      
      menuItem("Tab 3", tabName = "tab3", icon = icon("project-diagram")),  
      
      
      
      menuItem("Tab 4", tabName = "tab4", icon = icon("cogs")),  
      
      
      
      menuItem("Tab 5", tabName = "tab5", icon = icon("user-cog")),  
      
      
      
      menuItem("Tab 6", tabName = "tab6", icon = icon("tools"))  
      
      
      
    )  
    
    
    
  ),  
  
  
  
  dashboardBody(  
    
    
    
    
    
    
    
    tags$style(HTML("  

  

      #plot2, #plot2_2, {  

  

        padding: 10px;  

  

        margin: 25px;  

  

        display: flex;  

  

        justify-content: center;  

  

        align-items: center;  

  

        width: 100%;  

  

        background-color: #f9f9f9;  

  

        border-radius: 8px;  

  

        box-shadow: 2px 2px 5px orange;  

  

      }  

  

      #table2, #length_metro_table, #ridership_metro_table {  

  

        padding: 20px;  

  

        margin: 10px;  

  

        display: flex;  

  

        justify-content: center;  

  

        align-items: center;  

  

        overflow-x: auto;   

  

        width: 100%;  

  

        background-color: #f0f0f0;  

  

        border-radius: 10px;  

  

        box-shadow: 2px 2px 5px orange;  

  

      }  

  

    ")),  
    
    
    
    fluidRow(  
      
      
      
      # Info boxes  
      
      
      
      infoBox(  
        "Countries with Metro Systems", metro_countries, 
        icon = icon("flag"), color = "blue", width = 3 
      ),  
      
      
      infoBox(  
        "Percentage of Countries", paste0(round(percentage_metro, 2), "%"),
        icon = icon("globe"), color = "green", width = 3 
      ),  
      
      
      infoBox(  
        "Top Metro Station", top_metro_station,   
        icon = icon("subway"), color = "purple", width = 3 
      ),
      
      infoBox(  
        "Correlation Length vs Ridership", round(corr, 2), 
        icon = icon("chart-line"), color = "orange", width = 3
      )  
      
      
    ),  
    
    
    
    
    
    
    
    tabBox(  
      
      
      
      id = "tabs",  
      
      
      
      width = 12,  
      
      
      
      tabPanel("Tab 1",  
               
               
               
               h2("Tab 1 Content"),  
               
               
               
               p("Description of the first aspect of the project.")  
               
               
               
      ),  
      
      
      
      tabPanel("Ridership by Year",  
               
               
               
               fluidRow(  
                 
                 box(title = "Trend of Total Ridership over Years", status = "primary", solidHeader = TRUE,
                     plotlyOutput("plot2")),
                 box(title = "Annual Ridership Trends by Country", status = "primary", solidHeader = TRUE,
                     plotlyOutput("plot2_2")),
                 box(title = "Growth of Metro Systems Over Time", status = "primary", solidHeader = TRUE,
                     plotlyOutput("metro_growth_plot")),   
                 box(title = "Top 10 Metro System by Ridership", status = "primary", solidHeader = TRUE,
                     plotlyOutput("metrosystem_trends_plot"))
               )
      ),  
      
      
      
      tabPanel("Lenth Distribution",  
               
               fluidRow(  
                 
                 box(title = "System Lenth Distribution", status = "primary", solidHeader = TRUE,
                     plotlyOutput("plot3")),
                 box(title = "Metro System Size vs. Annual Ridership", status = "primary", solidHeader = TRUE,
                     plotlyOutput("plot4"))
               )
               
      ),  

      
      tabPanel("Forcecasting",  
               
               fluidRow(
                 infoBox(  
                   "ARIMA", round(mae_arima,2),
                   icon = icon("clock"), color = "orange", width = 2
                 ),
                 infoBox(  
                   "Random Forest", round(mae_rf,2),
                   icon = icon("tree"), color = "navy", width = 2
                 ),
                 infoBox(  
                   "Gradient Boosting", round(mae_gbm,2),
                   icon = icon("rocket"), color = "maroon", width = 2
                 ),
                 infoBox(  
                   "Metro System Length for Developed", 
                   HTML("<b>Median Developed: </b> 28 <br> <b>Median Developing: </b> 30.5"),
                   fill = TRUE,
                   icon = icon("road"), color = "teal", width = 3
                 ),
                 
                 infoBox(  
                   "Metro System Ridership for Developed", 
                   HTML("<b>Median Developed: </b> 99 <br> <b>Median Developing: </b> 104.95"),
                   fill = TRUE,
                   icon = icon("user"), color = "aqua", width = 3
                 )
               ),  
               
               fluidRow(  
                 box(title = "Forecasting Future Ridership", status = "primary", solidHeader = TRUE,
                     plotlyOutput("forecast_plot")),
                 box(title = "Prevelance of Metro System by Development Status", status = "primary", solidHeader = TRUE,
                     plotlyOutput("metro_prevalence_pie"))
                 ) 
                 
                 
               )     
          ) 
       )
)
















# Define server logic  



server <- function(input, output) {  
  
  
  
  
  
  
  
  # Reactive expression to filter data based on the development status  
  
  
  
  filtered_data <- reactive({  
    
    
    
    if (input$development_status == "All") {  
      
      
      
      metro_growth  
      
      
      
    } else {  
      
      
      
      metro_growth %>% filter(Development.Status == input$development_status)  
      
      
      
    }  
    
    
    
  })  
  
  
  
  
  
  
  
  
  
  
  
  output$plot2 <- renderPlotly({  
    
    
    
    plot_ly(ridership_years, x = ~Annum, y = ~Total_ridership, type = 'scatter', mode = 'lines+markers', name = 'Trend',  
            
            
            
            line = list(color = 'black')) %>%  
      
      
      
      add_bars(name = 'Total Ridership', marker = list(color = "orange")) %>%  
      
      
      
      layout(
             xaxis = list(title = "Year"),  
             
             
             
             yaxis = list(title = "Total Ridership (millions)", tickformat = ".0f"),  
             
             
             
             plot_bgcolor = 'lightgrey',   
             
             
             
             autosize = TRUE,           # Automatically size the plot  
             
             
             
             margin = list(l = 40, r = 40, t = 40, b = 40)) %>% config(displayModeBar = FALSE)  
    
    
    
  })  
  
  
  
  
  
  
  
  output$plot2_2 <- renderPlotly({  
    
    
    
    # Plot the ridership trend for the top 10 countries  
    
    
    
    plot_ly(data = ridership_years.2, x = ~Total_ridership, y = ~Country.region, type = 'bar',  
            
            
            
            color = ~as.factor(Annum), text = ~sprintf("%.2f", Total_ridership), hoverinfo = 'text') %>%  
      
      
      
      config(displayModeBar = FALSE) %>%  
      
      
      
      layout(
             
             
             
             xaxis = list(title = "Ridership"),  
             
             
             
             yaxis = list(title = "Country"),  
             
             
             
             barmode = 'stack',  
             
             
             
             autosize = TRUE,           # Automatically size the plot  
             
             
             
             margin = list(l = 40, r = 40, t = 40, b = 40),  
             
             
             
             legend = list(title = list(text = 'Year'), orientation = 'v', x = 1, y = 1))  
    
    
    
  })  
  
  
  
  
  
  
  
  output$table2 <- renderDT({  
    
    
    
    datatable(ridership_years.2, options = list(pageLength = 10, autoWidth = TRUE), filter = 'top')  
    
    
    
  })  
  
  
  
  
  
  
  
  output$plot3 <- renderPlotly({  
    
    
    
    # Create histogram with Plotly  
    
    
    
    Length_distribution <- plot_ly(data = metro.2, x = ~System_Length_ml, type = 'histogram',   
 marker = list(color = 'cyan', line = list(color = 'black', width = 1))) %>% config(displayModeBar = FALSE)  %>%  

      # Add density line  
 
      add_lines(x = density_data$x, y = density_data$y * length(metro.2$System_Length_ml) * diff(density_data$x[1:2]),   
                line = list(color = 'red', width = 2), name = 'Density Line') %>%  
      layout(
             xaxis = list(title = "System Length (ml)"),   
             yaxis = list(title = "Count"))  
    
    
    
    
    
    
    
    Length_distribution  
    
    
    
  })  
  
  
  
  
  
  
  
  output$plot4 <- renderPlotly({  
    
    
    
    # Create scatter plot  
    
    
    
    Length.ridership <- plot_ly(data = metro.2, 
                                x = ~System_Length_ml, 
                                y = ~Annual_Ridership_.Millions., 
                                color = ~Development.Status,   # Changed from 'fill' to 'color'
                                type = 'scatter', 
                                mode = 'markers',   
                                marker = list(size = 10,
                                              line = list(color = "black", width = 1))) %>% config(displayModeBar = FALSE)  %>%  
      layout(title = list( x = 0.5), 
             xaxis = list(title = "System Length (Miles)"), 
             yaxis = list(title = "Annual Ridership (Millions)"))  
    
    Length.ridership 
  })  
  
  
  
  
  
  
  
  output$metro_prevalence_pie <- renderPlotly({  
    
    
    
    plot_ly(prevalence_metro, labels = ~Development.Status, values = ~prevalence_metro, type = 'pie') %>% config(displayModeBar = FALSE) %>%  
      
      
      
      layout(
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),  
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
  })  
  
  
  
  
  
  
  
  output$length_metro_table <- renderDT({  
    
    
    
    datatable(length_metro, options = list(pageLength = 10, autoWidth = TRUE), filter = 'top')  
    
    
    
    
    
    
    
  })  
  
  
  
  
  
  
  
  output$ridership_metro_table <- renderDT({  
    
    
    
    Ridership_metro <- metro.2 |> group_by(Development.Status) |>   
      
      
      
      summarize(Ridership_metro_median = median(Annual_Ridership_.Millions., na.rm = TRUE),   
                
                
                
                Ridership_metro_sum = sum(Annual_Ridership_.Millions., na.rm = TRUE))  
    
    
    
    Ridership_metro  
    
    
    
  })  
  
  
  
  
  
  
  
  output$metro_growth_plot <- renderPlotly({  
    
    
    
    fig <- plot_ly(metro_growth, x = ~Service.opened, y = ~Cumulative_Count, type = 'scatter', mode = 'lines+markers') %>% 
      config(displayModeBar = FALSE)  %>%  
      layout(  
             xaxis = list(title = 'Year'),   
             yaxis = list(title = 'Cumulative Count of Metro Systems'))  
    fig    
  })  
  
  
  
  
  
  
  
  output$top_10_metro_plot <- renderPlotly({  
    fig <- plot_ly(top_10_metro_data, x = ~Name, y = ~Annual_Ridership_.Millions., type = 'bar',   
              text = ~paste('Ridership:', Annual_Ridership_.Millions., 'Million'),   
                   
                   
                   
                   hoverinfo = 'text',   
                   
                   
                   
                   marker = list(color = 'rgba(55, 83, 109, 0.7)',   
                                 
                                 
                                 
                                 line = list(color = 'rgba(55, 83, 109, 1.0)', width = 2))) %>% config(displayModeBar = FALSE)  %>%  
      
      
      
      layout(
             
             
             
             xaxis = list(title = 'Metro System'),  
             
             
             
             yaxis = list(title = 'Ridership (Millions)'),  
             
             
             
             margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4),  
             
             
             
             paper_bgcolor = 'rgba(245, 246, 249, 1)',  
             
             
             
             plot_bgcolor = 'rgba(245, 246, 249, 1)')  
    
    
    
    
    
    
    
    fig  
    
    
    
  })  
  
  
  
  
  
  
  
  output$metrosystem_trends_plot <- renderPlotly({  
    
    
    
    fig <- plot_ly(data = metro_years, x = ~Total_ridership, y = ~Name, type = 'bar', 
                   color = ~as.factor(Annum), text = ~sprintf("%.2f", Total_ridership), hoverinfo = 'text') %>% 
      config(displayModeBar = FALSE)  %>%  
       layout( xaxis = list(title = "Ridership"),  
               yaxis = list(title = "Country"),   
               barmode = 'stack',  
               legend = list(title = list(text = 'Year'), orientation = 'v', x = 1, y = 1))  
    
    
    
    
    
    
    
    fig  
    
    
    
  })  
  
  
  
  
  
  
  
  output$evaluation_table <- renderDT({  
    
    
    
    # Create a summary table of evaluation metrics  
    
    
    
    results <- data.frame(  
      
      
      
      Model = c("ARIMA", "Random Forest", "Gradient Boosting"),  
      
      
      
      MAE = c(mae_arima, mae_rf, mae_gbm),  
      
      
      
      RMSE = c(rmse_arima, rmse_rf, rmse_gbm)  
      
      
      
    )  
    
    
    
    
    
    
    
    # Render the table using datatable  
    
    
    
    datatable(results, options = list(pageLength = 10, autoWidth = TRUE))  
    
    
    
  })  
  
  
  
  
  
  
  
  output$forecast_plot <- renderPlotly({  
    
    
    
    # Create plot for forecasting  
    
    
    
    fig <- plot_ly(forecast_melted, x = ~Year, y = ~Ridership, color = ~Model, type = 'scatter', mode = 'lines+markers') %>% 
      config(displayModeBar = FALSE)  %>%  
      layout(
             xaxis = list(title = 'Year'),  
             
             
             
             yaxis = list(title = 'Ridership', range = c(0, max(forecast_melted$Ridership) * 1.5)))  
    
    
    
    
    
    
    
    # Return the plot  
    
    
    
    fig  
    
    
    
  })  
  
  
  
  
  
  
  
  
  
  
  
}  







# Run the application   



shinyApp(ui = ui, server = server) 

