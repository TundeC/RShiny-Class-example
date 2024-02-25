library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinya11y)
library(plotly)

# UI
ui <- dashboardPage(

  dashboardHeader(title = span(icon("dove"),"Chicks")),
  
  dashboardSidebar(
    
  ),
  
  dashboardBody(
    use_tota11y(),
    tags$html(lang="en"),
    tags$head(
      tags$title("Chicks"),
      tags$style(
        HTML(
          "#text {
            color: #007f00;
          },
          #hist img {
            border: 5px solid black;
          }
          .skin-blue .main-header .logo {
            background-color: #317da8;
          }
          .skin-blue .main-header .navbar {
            background-color: #007fb0;
          }
          .alert-info, .bg-aqua, .callout.callout-info, .label-info, .modal-info .modal-body {
            background-color: #0081a1!important;
          }
          a {
            color: #0075a3;
          }
          "
        )
      )
    ),
    tags$h1("Analysis results"),
    fluidRow(
      column(
        tags$img(src="https://images.pexels.com/photos/2695703/pexels-photo-2695703.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=1", 
                 height=200,
                 alt="A chick standing on some wood planks"),
        width=4
      ),
      column(
        infoBox(title = "No. of diets:",
                value = 4,
                icon = icon("wheat-awn"),
                color = "aqua",
                width = 4,
                fill = TRUE),
        width=8
      )
    ),
    selectInput("col","Select dataset column",choices=list("weight","Time","Chick","Diet")),
    plotOutput("hist"),
    plotlyOutput("bar"),
    textOutput("text"),
    tags$h2("Further information"),
    p("Further information can be found on ", tags$a("Google.",href="https://www.google.com"))
  )
  
)

# Server
server <- function(input, output) {
  output$hist <- renderPlot({
    validate(
      if (!exists("ChickWeight")) {
        "Could not find dataset."
      } else if (input$col=="Diet") {
        "Cannot produce histogram of the Diet variable. Pick a different variable to see a histogram."
      } else if (input$col=="Chick") {
        "Cannot produce histogram of the Chick variable. Pick a different variable to see a histogram."
      } else NULL)
    ChickWeight %>% ggplot(aes(x=!!sym(input$col))) + geom_histogram()
  })
  
  output$bar <- renderPlotly({
    validate(
      need(input$col %in% c("Chick","Diet"),"Pick a different cariable.")
    )
    ChickWeight %>% count(!!sym(input$col)) %>% plot_ly(x=as.formula(paste0("~",input$col)), y=~n, type="bar")
  })
  
  output$text <- renderText({
    paste0("This is page shows summary information of the variables of the ChickWeight dataset, including: ",paste(colnames(ChickWeight),collapse = ", "),".")
  })
}

# Run the application
shinyApp(ui, server)
