#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

source('strong.R')


# UI
sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("fp", "Upload Exported CSV File",
              multiple = FALSE, accept = c(".csv")),
    menuItem("Exercise view", icon = icon("dumbbell"), tabName = "exercise"),
    menuItem("Workout view", tabName = "workout", icon = icon("dashboard"),
             badgeLabel = "new", badgeColor = "green")
    
  )
)

workout_tab <- tabItem(tabName = "workout",

  fluidRow(
    box(
      title = 'Weight moved per workout',
      plotOutput("calplot", width = '100%'), width = 12)
    )
  
)
  
exercise_tab <- tabItem(tabName = "exercise",

  fluidRow(
    box(
      title = selectInput('lift_sel', 'Upload file to begin...', 'This field will autofill from file'),
      plotOutput("liftplot"),
      width = 8
    ),
    box(
      # Dynamic valueBoxes
      valueBoxOutput("prBox", width = '100%'),
      valueBoxOutput("totBox", width = '100%'),
      width = 4
    )
  )
)


ui <- dashboardPage(
  skin = 'midnight',
  dashboardHeader(title = "Strong Tracker",
                  tags$li(class = "dropdown",
                          dropMenu(
                            dropdownButton(icon = icon('info')),
                            h3(strong('Information')),
                            br(),
                            h5('For use with Strong App fitness tracker. Export your data by following settings>export data.'),
                            br(),
                            h5('Find the source code on github! github.com/harrig12/strong'),
                            arrow = F)
                          
                  )),
  sidebar,
  dashboardBody(tabItems(exercise_tab, workout_tab))
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  df <- reactive({
    req(input$fp)
    df <- process_file_upload(input$fp$datapath)
    updateSelectInput(session, "lift_sel","Best set over time for: ", choices = unique(df$`Exercise Name`))
    return(df)
  })
  #df <- process_file_upload(fp)
  
  output$calplot <- renderPlot({make_fitness_cal(df())})
  output$liftplot <- renderPlot({make_lift_plot(df(), input$lift_sel)})
  output$prBox <- renderValueBox({
    valueBox(
      paste0(lift_pr(df(), input$lift_sel), "lbs"), "Weight PR", icon = icon("trophy"),
      color = "orange"
    )
  })
  output$totBox <- renderValueBox({
    valueBox(
      paste0(lift_tot(df(), input$lift_sel), "lbs"), "Total Moved", icon = icon("weight-hanging"),
      color = "purple"
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
