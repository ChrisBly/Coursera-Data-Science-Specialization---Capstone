#Coursera Data Science Specialization - Capstone by Chris Blycha
library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Katz's back-off model - Text Prediction Application",
                  titleWidth = 500),
  dashboardSidebar(
    width = 620,
    sidebarMenu(
      menuItem(h1("Text Prediction Application Instructions")),
      br(),
      h3("1: Please enter your text here: in section 3"),
      br(),
      br(),
      h3("2: What you have entered will be displayed Section 2 "),
      br(),
      br(),
      h3("3: The predicted next words to be displayed Section 1"),
      br(),
      h3("4: Once text has been enter the application will start")
    ),
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    
    # The dynamically-generated user panel
    uiOutput("userpanel")
  ),
  dashboardBody(
    fluidRow(
      box(
        width = 20, background = "light-blue",height = "650",solidHeader = T,align = "middle",
        h1("Section: 1"),
        h3("The predicted next word"),
        h3(tableOutput("mytable1"))
        ),
      box(width = 20, background = "purple",height = "200",solidHeader = T,align = "middle",  
          h1("Section: 2"),
          h3("What you have entered:"),
          h3(textOutput("caption"))
      ),
      box(width = 20, background = "blue",height = "200",solidHeader = T,align = "middle", 
          h1("Section: 3"),    
          h3(textInput("inText", "1: Please enter your text here:"))
      ))
  )
)
