library(shiny)
library(shinydashboard)
library(survival)

# Load the cancer survival dataset
data(lung)

# Define the UI
ui <- dashboardPage(
  
  dashboardHeader(
    title = "Cancer Survival Data Analysis",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plots", tabName = "plots", icon = icon("chart-bar")),
      selectInput("variable", "Choose Variable:", 
                  choices = c("age", "sex", "ph.ecog", "ph.karno"),
                  selected = "age"),
      sliderInput("bins", "Number of Bins:", min = 5, max = 20, value = 10)
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
                      /* Dark Theme */
                      body, .main-sidebar, .left-side, .skin-black .main-header .logo {
                        background-color: #222d32 !important;
                        color: #b8c7ce !important;
                      }
                      .main-sidebar .sidebar-menu>li.header {
                        color: #4b646f !important;
                      }
                      .main-sidebar .sidebar-menu>li>a {
                        color: #b8c7ce !important;
                      }
                      .main-sidebar .sidebar-menu>li:hover>a, .main-sidebar .sidebar-menu>li.active>a {
                        background-color: #1e282c !important;
                      }
                      .main-sidebar .sidebar-menu>li>.treeview-menu {
                        background-color: #2c3b41 !important;
                      }
                      .content-wrapper, .right-side, .main-footer {
                        background-color: #2c3b41 !important;
                      }
                      .content-wrapper {
                        background-color: #2c3b41 !important;
                      }
                      ")),
      tags$style(HTML(".content-wrapper, .right-side, .main-header, .sidebar, .main-footer {
                        -webkit-box-shadow: none !important;
                        -moz-box-shadow: none !important;
                        box-shadow: none !important;
                      }"))
    ),
    tabItems(
      tabItem(
        tabName = "plots",
        fluidRow(
          box(
            title = "Age Distribution",
            plotOutput("histogram", height = 300)
          ),
          box(
            title = "Survival Status over Time",
            plotOutput("linegraph", height = 300)
          ),
          box(
            title = "Performance Status Scatter Plot",
            plotOutput("scatterplot", height = 300)
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive function to generate histogram based on user input
  output$histogram <- renderPlot({
    data <- lung[[input$variable]]
    if (!is.numeric(data)) {
      return(NULL)  # Return NULL if the variable is not numeric
    }
    hist(data, breaks = input$bins, xlab = input$variable,
         main = paste("Distribution of", input$variable), col = "skyblue", 
         ylab = "Frequency", border = "white")
  })
  
  # Reactive function to generate line graph
  output$linegraph <- renderPlot({
    data <- lung[[input$variable]]
    if (!is.numeric(data)) {
      return(NULL)  # Return NULL if the variable is not numeric
    }
    plot(data, type = "l", col = "darkgreen", xlab = "Time",
         ylab = "Survival Status", main = "Survival Status Over Time")
  })
  
  # Reactive function to generate scatter plot
  output$scatterplot <- renderPlot({
    data_x <- lung$ph.ecog
    data_y <- lung$ph.karno
    plot(data_x, data_y, pch = 19, col = "orange", 
         xlab = "Performance Status (ECOG)", ylab = "Performance Status (Karnofsky)",
         main = "Performance Status Scatter Plot")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
