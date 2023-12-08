library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)

df <- read.csv("final_project_df_two.csv")
df_unique_state <- df[!duplicated(df$Total), ]
ui <- dashboardPage(
  dashboardHeader(title = "Info Final"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Intro", tabName = "home"),
      menuItem("story1", tabName = "data1"),
      menuItem("story2", tabName = "data2"),
      menuItem("story3", tabName = "data3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("home",
              fluidPage(
                titlePanel("Connections between household incomes and pollution"),
                p("subtitle"),
              )
      ),
      tabItem("data1",
              fluidPage(
                titlePanel("Population vs Pollution"),
               # dropdownMenu(),
                plotOutput("scatterPlot")
              )
      ),
      tabItem("data2",
              fluidPage(
                titlePanel("Title"),
                p("subtitle")
              )
      ),
      tabItem("data3",
              fluidPage(
                titlePanel("Title"),
                p("subtitle")
              )
      )
    )
  )
)

server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(df_unique_state, aes(x = Total, y = CO.Mean)) +
      geom_point() +
      labs(title = "Population vs Pollution",
           x = "Population",
           y = "Pollution") +
      theme_minimal()
  })
}

shinyApp(ui, server)