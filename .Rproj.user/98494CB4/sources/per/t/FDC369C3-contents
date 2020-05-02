library(shiny)
library(shinyMatrix)
library(colourpicker)
library(ggplot2)
library(plotly)
library(dplyr)

m <- matrix(c(1, 2), 1, 2, dimnames = list(NULL, c("x", "y")))


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Regressor2000"),
    
    sidebarLayout(
        sidebarPanel(
            
            tabsetPanel(
                tabPanel("Input", 
                         radioButtons('input_method', "Input method", c("CSV File" = "file_input", 
                                                                        "Manual input (WIP)" = 'manual')),
                         conditionalPanel("input.input_method == 'file_input'",
                                          selectInput('separator', "CSV separator", c("Comma" = ",",
                                                                                      "Semicolon" = ";",
                                                                                      "Tab" = "\t",
                                                                                      "Whitespace" = " ")),
                                          selectInput('decimal', "CSV decimal", c(".", ",")),
                                          checkboxInput('headers', "My file includes headers"),
                                          fileInput('file',"File input", accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv"))),
                         conditionalPanel("input.input_method == 'manual'",
                                          matrixInput('matrix',
                                              value = m,
                                              rows = list(extend = TRUE),
                                              cols = list(names = TRUE)))),
                tabPanel("Customization",
                         colourInput('point_colour', "Points colour", 'blue'),
                         colourInput('lm_colour', "Line colour", 'red'),
                         checkboxInput('disable_lm', "Disable regression fit"),
                         sliderInput('point_size', "Points size", 0.1, 5, 2, 0.1),
                         sliderInput('lm_size', "Line size", 0.1, 5, 1, 0.1),
                         sliderInput('point_alpha', "Points transparency", 0.1, 1, 1, 0.1),
                         sliderInput('lm_alpha', "Line transparency", 0.1, 1, 0.8, 0.1)),
                tabPanel("Text",
                         textInput('title', 'Title'),
                         textInput('xaxis', "x-axis label"),
                         textInput('yaxis', 'y-axis label'))
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Plot', plotlyOutput('regres')),
                tabPanel('Table', tableOutput('table'))
            )
        )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        if(input$input_method == 'file_input') {
            data <- read.csv(input$file$datapath, header=input$headers,
                                        sep=input$separator,
                                        dec=input$decimal,
                                        col.names=c('x', 'y'))
        } else {
            data <- as.data.frame(input$matrix)
        }
        data
    })

    
    output$table <- renderTable({
            data()

    })

    output$regres <- renderPlotly({
        data <- data()
        p <- ggplot(data, aes(x, y)) +
                geom_point(size=input$point_size, colour=input$point_colour, alpha=input$point_alpha)
        
        if(!(input$disable_lm)) {
            p <- p + geom_line(stat="smooth", method='lm',formula=y~x, alpha=input$lm_alpha, 
                                                        size=input$lm_size, 
                                                        colour=input$lm_colour)
        }
        p <- p + labs(title=input$title, x=input$xaxis, y=input$yaxis)
            
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
