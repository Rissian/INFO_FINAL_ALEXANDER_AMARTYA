library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(tidyverse)

rm(list=ls())
APP_HOME=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(APP_HOME)
APP_HOME

options(scipen = 999)

list.files()

pollution_summary_df <- read.csv("final_project_df_two.csv")
pollution_summary_df <- dplyr::mutate(
  pollution_summary_df,
  population = as.numeric(stringr::str_replace_all(Total, ",", ""))
)


# story # 1 pollution vs population

pollution_parameters <- grep("Mean$", names(pollution_summary_df), value = TRUE)

createPollutionPlot <- function(pollution_param, data_frame = pollution_summary_df) {
  pollution_param <- as.character(pollution_param)
  
  summarized_df <- group_by(data_frame, State)
  summarized_df <- summarize(summarized_df, avg_value = mean(get(pollution_param), na.rm = TRUE))
  summarized_df <- arrange(summarized_df, avg_value)
  
  summarized_df$State <- factor(summarized_df$State, levels = summarized_df$State)
  
  plot <- ggplot(summarized_df[!is.na(summarized_df$avg_value), ], aes(x = State, y = avg_value)) +
    geom_col(fill = rgb(0, 0, 1), color = "white", alpha = .3, width = .7) +
    coord_flip() +
    labs(
      title = paste("Average values of", pollution_param, "by State"),
      x = "State",
      y = paste("Average", pollution_param)
    )
  
  return(plot)
}

createPollutionPlot(pollution_parameters[3])

pollution_parameters <- grep("Mean$", names(pollution_summary_df), value = TRUE)

createPollutionPlot <- function(pollution_param, data_frame = pollution_summary_df) {
  pollution_param <- as.character(pollution_param)
  
  summarized_df <- group_by(data_frame, State)
  summarized_df <- summarize(
    summarized_df,
    population = first(population),
    avg_value = mean(get(pollution_param), na.rm = TRUE)
  )
  
  summarized_df$label <- paste(summarized_df$State, summarized_df$population, sep = ", ")
  
  plot <- ggplot(summarized_df, aes(x = population, y = avg_value, color = State, label = label)) +
    geom_point(alpha = .3, size = 3) +
    ggrepel::geom_label_repel(nudge_y = 0.025, size = 2.5, alpha = 0.7) +
    scale_color_viridis_d(guide = FALSE) +
    scale_x_continuous(
      labels = scales::comma,
      breaks = seq(0, max(summarized_df$population, na.rm = TRUE), by = 2500000)
    ) +
    labs(
      title = paste("Average values of", pollution_param, "by State"),
      x = "Population",
      y = paste("Average", pollution_param)
    )
  
  return(plot)
}

createPollutionPlot(pollution_parameters[3])

# story # 2 (population vs income)

income_parameters <- grep("dollars", names(pollution_summary_df), value = TRUE)
income_parameters

createIncomePlot <- function(income_param, data_frame = pollution_summary_df) {
  income_param <- as.character(income_param)
  
  data_frame[[income_param]] <- as.numeric(gsub(",", ".", data_frame[[income_param]]))
  
  summarized_df <- group_by(data_frame, State)
  summarized_df <- summarize(
    summarized_df,
    population = first(population),
    avg_value = mean(get(income_param), na.rm = TRUE)
  )
  
  summarized_df$label <- paste(summarized_df$State, summarized_df$population, sep = ", ")
  
  plot <- ggplot(summarized_df, aes(x = population, y = avg_value, color = State, label = label)) +
    geom_point(alpha = .3, size = 3) +
    ggrepel::geom_label_repel(nudge_y = 0.025, size = 2.5, alpha = 0.7) +
    scale_color_viridis_d(guide = FALSE) +
    scale_x_continuous(
      labels = scales::comma,
      breaks = seq(0, max(summarized_df$population, na.rm = TRUE), by = 2500000)
    ) +
    labs(
      title = paste("Average values of", income_param, "by State"),
      x = "Population",
      y = paste("Average", income_param)
    )
  
  return(plot)
}

createIncomePlot(income_parameters[2])

# (3) income vs pollution at the state level

income_parameters
pollution_parameters

data_frame <- pollution_summary_df
income_param <- income_parameters[1]
pollution_param <- pollution_parameters[1]

data_frame[[income_param]] <- as.numeric(gsub(",", ".", data_frame[[income_param]]))

filtered_data <- filter(pollution_summary_df, State == "Alabama")



data <- ggplot(filtered_data, aes(x = get(income_param), y = get(pollution_param))) +
  geom_boxplot() +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
  labs(
    title = paste("Income vs Pollution for", filtered_data$State),
    x = "Income",
    y = "Pollution"
  )
print(data)







ui <- navbarPage(
  "Final Project",
  
  tabPanel("Overview",
           mainPanel(
             h2("Project Overview"),
             p("In this Shiny app, we explore the connections between income, population, and pollution.We will be trying to answer the question: Are you more likely to live in a polluted area if you are ower income?"),
             
             h3("Why is this Interesting?"),
             p("Understanding how income levels influence pollution patterns provides insights into societal development and its impact on people. Such analyses can inform policy decisions aimed at achieving a balance between economic growth and ecological sustainability, fostering a more equitable and environmentally conscious future."),
             
             h3("Data Source"),
             p("The dataset used for this analysis is joined from two data sources."),
             HTML("Click <a href='https://github.com/Rissian/INFO_FINAL_ALEXANDER_AMARTYA/blob/main/final_project_df_two.csv' target='_blank'>here</a> for the joined dataset"),
             p("The joined data set was made of two data sets.The first was the 2022 Income rates collected by the United States Census Bureau."),
             HTML("Click <a href='https://data.census.gov/table/ACSST1Y2022.S1901?q=median%20household%20income%20by' target='_blank'>here</a> for the income dataset"),
             p("The second data set was about US pollution data from the years 2000-2023 collected by the Enviromental Protection Agency."),
             HTML("Click <a href='https://www.kaggle.com/datasets/guslovesmath/us-pollution-data-200-to-2022' target='_blank'>here</a> for the income dataset"),
             
             h3("What Analysis Are We Doing?"),
             p("We will explore the data through descriptive, exploratory, and inferential analyses. Plotting data to try to find answers to our questions.")
           )
  ),
  
  tabPanel("Story 1",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("pollution_param_select",
                           label = "Select Pollution Parameter",
                           choices = pollution_parameters),
               textOutput("pollution_description")  
             ),
             mainPanel(
               width = 9,
               h2("What is the connection between pollution and population?"),
               plotOutput("story1_plot")
             )
           )
  ),
  
  tabPanel("Story 2",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("income_param_select",
                           label = "Select Income Parameter",
                           choices = income_parameters),
               textOutput("income_decription")
             ),
             mainPanel(
               width = 9,
               h2("What is the connection between population and median and mean income? How are they different?"),
               plotOutput("story2_plot")
             )
           )
  ),
  
  tabPanel("Story 3",
            sidebarLayout(
              sidebarPanel(
                width = 3,
                selectInput("income_param_select_story3",
                            label = "Select Income Parameter",
                            choices = income_parameters),
                selectInput("pollution_param_select_story3",
                            label = "Select Pollution Parameter",
                            choices = pollution_parameters),
                textOutput("pollution_description3")
                
              ),
              mainPanel(
                width = 9,
                h2("Making conclusions. How do income rates relate to different kinds of pollution?"),
                plotOutput("story3_plot")
              )
            )
  )
)



server <- function(input, output) {
  output$story1_plot <- renderPlot({
    # makes sure that a pollution parameter is selected
    if (is.null(input$"pollution_param_select")) {
      return(NULL)
    }
    createPollutionPlot(pollution_param = input$"pollution_param_select", data_frame = pollution_summary_df)
  })
  output$pollution_description <- renderText({
    # makes sure that a pollution parameter is selected
    if (is.null(input$"pollution_param_select")) {
      return("Select a Pollution Parameter")
    }
    selected_pollution_param <- input$"pollution_param_select"
    if (selected_pollution_param == "O3.Mean") {
      paste("As we can see, population has little affect on ground level ozone pollution. This is intersting because it is generaly caused by emmisions from cars, powerplants, and chemical plants so it is easy to assume more populus states would have higher levels of ground ozone pollution.")
    }
    else if (selected_pollution_param == "CO.Mean") {
      paste("As we can see, there is a general connection between population and carbon monoxide pollution. As population goes up, carbon monoxide emmisions go up. Interesting outliers include Arkansas, being one of the least populated states with the highest levels of carbon monoxide pollution. Also texas for the opposite reason. One of the highest populations but one of the least polluted states in terms of carbon monoxide.")
    } else if (selected_pollution_param == "NO2.Mean") {
      paste("The findings about nitrogen dioxide are very intersting. There seems to be almost no correlation between population and nitrogen dioxide emmisions. Nitrogen dioxide emmisions are caused by gas vehicles, welding and using explosives. They are also caused by food production, which we believe to be the main reason behind why there is such a big range in emmisions from similarly populated states. States food productions vary a lot more than car population, energy industry, ect.")
    } else if (selected_pollution_param == "SO2.Mean") {
      paste("Similar carbon monoxide, there is a general connection between population and sulfur dioxide pollution. Actually, the pollution rates for the two look extremely similar. This is interesting because unlike carbon monoxide pollution, which is generaly caused by gas vehicle emmisions, sulfur dioxide emmisions are generaly cause by coal burning electric utilities.")
    }
    
  })
  
  output$story2_plot <- renderPlot({
    # makes sure  that an income parameter is selected
    if(is.null(input$"income_param_select")) {
      return(NULL)
    }
    
    createIncomePlot(income_param = input$"income_param_select", data_frame = pollution_summary_df)
  })
  
  output$income_decription <- renderText({
    if (is.null(input$"income_param_select")) {
      return("Select a Pollution Parameter")
    }
    selected_income <- input$"income_param_select"
    if (selected_income == "Median.income..dollars.") {
      paste("As we can see, there is only a small connection between states populations and income levels. This goes against what we expected, as we thought that states with higher populations would have more job opportunities and so would have a higher median income. Surprisingly, New York is not as much of outlier as expected. It has a  high population, but despite it's reputation for job opportunitites, does not have a particularly high median income.")
    } else if (selected_income == "Mean.income..dollars.") {
      paste("When selecting mean income, we see that almost every states income jumps nearly 30 thousand dollars. Some, like New York, jump 40 thousand. This huge difference between the median income and mean income shows the great income inequality found throughout the country. While most people make close to the median amount, the mean is significantly higher as the gap between middle and upper class is so big that the mean gets a massive boost.")
    }
  })
  
  output$story3_plot <- renderPlot({
    if (is.null(input$income_param_select_story3) || is.null(input$pollution_param_select_story3)) {
      return(NULL)
    }
    
    income_param_story3 <- input$income_param_select_story3
    pollution_param_story3 <- input$pollution_param_select_story3
    
    data_frame_story3 <- pollution_summary_df
    data_frame_story3[[income_param_story3]] <- as.numeric(gsub(",", ".", data_frame_story3[[income_param_story3]]))
    
    ggplot(data_frame_story3, aes(x = get(income_param_story3), y = get(pollution_param_story3))) +
      geom_boxplot() +
      geom_jitter(alpha = 0.5, color = "blue") +
      labs(
        title = paste("Boxplot of", pollution_param_story3, "vs", income_param_story3, "at State Level"),
        x = income_param_story3,
        y = pollution_param_story3
      )
  })
  output$pollution_description3 <- renderText({
    # makes sure that a pollution parameter is selected
    if (is.null(input$"pollution_param_select_story3")) {
      return("Select a Pollution Parameter")
    }
    selected_pollution_param <- input$"pollution_param_select_story3"
    if (selected_pollution_param == "O3.Mean") {
      paste("Looking at the box plot for ground level ozone pollution, we can see that there seems to be no big connection between income and pollution. We know that population is a factor for ozone pollution, but we've learned that population has little affect on income. Changing from median to mean income has little effect on Ozone pollution, meaning that lower income individuals are not more likely to be exposed to more ozone emissions.")
    }
    else if (selected_pollution_param == "CO.Mean") {
      paste("Looking at the box plot for carbon monoxide pollution for the median income, we can see that there seems to be no big connnection between income and pollution. Unlike ozone, there is a lot more varience in carbon monoxide pollution, with a few very big outliers that are low income and have very high carbon monoxide pollution. These outliers include Arkansas, our big outlier in the population to pollution graph for carbon monoxide. When comparing the mean to the median, we can see a fair decrease in pollution. This shows that lower income individuals are more likely to be exposed to carbon monoxide pollution.")
    } else if (selected_pollution_param == "NO2.Mean") {
      paste("The results of the box plot for nitrogen dioxide to income are very intersting. There is a very clear trend that shows that higher income individuals are more likely to experience higher levels of nitrogen dioxide emmisions. This is intersting because it is exactly opposite to what we expected to find. There is a decrease when changing from meadian to mean, but the overall trend stays. As we learned, population has little affect on nitrogen dioxide pollution, so we know that is not the cause.")
    } else if (selected_pollution_param == "SO2.Mean") {
      paste("As expected after the pollution to population analysis, sulfur dioxide emmisions relationship to income is very similar to carbon monoxides relationship to income. Again, states like Arkansas are major outliers with very high pollution rates and low incomes. When comparing the mean to the median, we can see a fair decrease in pollution. This shows that lower income individuals are more likely to be exposed to sulfur dioxide pollution.")
    }
    
  })
}

shinyApp(ui = ui, server = server)

