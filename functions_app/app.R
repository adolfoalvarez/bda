#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
# library(widgetframe)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Examples of functions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("bins", label="label", choices =c("Square",
                                                          "Square root",
                                                          "Logarithmic",
                                                          "Exponential",
                                                          "Sine"))
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = 10 + 1)

        # draw the histogram with the specified number of bins
        x <- seq(-3,3,0.01)
        y <- case_when(
            input$bins == "Square" ~ x^2,
            input$bins == "Square root" ~ sqrt(x),
            input$bins == "Logarithmic" ~ log(x),
            input$bins == "Exponential" ~ exp(x),
            input$bins == "Sine" ~ sin(x)
        )
        data <- tibble(x, y)
        # f <- ggplot(data, aes(x, y))
        # f <- f + geom_jitter()
        # plot(f)
        l <- plot_ly(data=data, x=~x, y=~y)
        l
        
        # ggplotly(f)
        #hist(x, breaks = bins, col = 'darkgray', border = 'white', main = input$bins)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
