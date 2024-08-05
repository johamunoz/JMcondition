#
library(shiny)
library(dplyr)
library(plotly)


data <- JMcondition:::PKGENVIR$DATA ## read the data from envir
concList <- unique(data$CONCEPT_ID)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Condition count"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput("condition_sel",
                      label = "Select condition ID",
                      choices = concList,
                      multiple = TRUE
          ),

          checkboxInput("month_sel", label = "Displayed by month", value = FALSE),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {


    output$distPlot <- renderPlotly({
       # to avoid error in the displayed graph
      validate(
        need(input$condition_sel, "please select at least one condition ID")
      )
        # filter out the dataset
      data_fil <- subset(data, CONCEPT_ID %in% input$condition_sel)

        # draw the graph
        plotTrend(data=data_fil, byMonth = input$month_sel)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
