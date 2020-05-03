library(shiny)
library(shinyMatrix)
library(colourpicker)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinythemes)

m <- matrix(c(1, 2), 1, 2, dimnames = list(NULL, c("x", "y")))


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Regressor2000"),
    
    sidebarLayout(
        sidebarPanel(
            
            tabsetPanel(
                tabPanel("Input", 
                         radioButtons('input_method', "Input method", c("CSV File" = "file_input" 
                                                                        #,"Manual input (WIP)" = 'manual'
                                                                        )),
                         conditionalPanel("input.input_method == 'file_input'",
                                          selectInput('separator', "CSV separator", c("Comma [,]" = ",",
                                                                                      "Semicolon [;]" = ";",
                                                                                      "Tab [\\t]" = "\t",
                                                                                      "Whitespace [ ]" = " ")),
                                          selectInput('decimal', "CSV decimal", c(".", ",")),
                                          checkboxInput('headers', "My file includes headers"),
                                          fileInput('file',"File input", accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv"))),
                         # conditionalPanel("input.input_method == 'manual'",
                         #                  matrixInput('matrix',
                         #                      value = m,
                         #                      rows = list(extend = TRUE),
                         #                      cols = list(names = TRUE)))
                         ),
                tabPanel("Visual Customization",
                         colourInput('point_colour', "Points colour", 'blue'),
                         colourInput('lm_colour', "Line colour", 'red'),
                         checkboxInput('disable_lm', "Disable regression fit"),
                         sliderInput('point_size', "Points size", 0.1, 5, 2, 0.1),
                         sliderInput('lm_size', "Line size", 0.1, 5, 1, 0.1),
                         sliderInput('point_alpha', "Points transparency", 0.1, 1, 1, 0.1),
                         sliderInput('lm_alpha', "Line transparency", 0.1, 1, 0.8, 0.1)),
                tabPanel("Math Customization",
                         radioButtons('formula', "Regression formula", c("Linear" = "y~x",
                                                                         "Polynomial [2 degree]" = "y~poly(x, 2)",
                                                                         "Polynomial [3 degree]" = "y~poly(x, 3)")),
                         numericInput('round', "Number of digits to round to", 2, 1, 5, 1)),
                tabPanel("Text Customization",
                         textInput('title', 'Title'),
                         checkboxInput('lm_as_title', "Use regression equation as a title"),
                         textInput('xaxis', "x-axis label"),
                         textInput('yaxis', 'y-axis label'))
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Plot', plotlyOutput('regres')),
                tabPanel('Table', tableOutput('table')),
                tabPanel('Model', tableOutput('model'))
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
        } 
        # else {
        #     data <- as.data.frame(input$matrix)
        # }
        data
    })
    
    model <- reactive({
        data <- data()
        m <- lm(as.formula(input$formula), data)
        m$coefficients <- round(m$coefficients, input$round)
        m
    })

    
    output$table <- renderTable({
            data()

    })

    output$regres <- renderPlotly({
        data <- data()
        formula <- as.formula(input$formula)
        
        p <- ggplot(data, aes(x, y)) +
                geom_point(size=input$point_size, colour=input$point_colour, alpha=input$point_alpha)
        
        if(!(input$disable_lm)) {
            p <- p + geom_line(stat="smooth", method='lm',formula=formula, alpha=input$lm_alpha, 
                                                        size=input$lm_size, 
                                                        colour=input$lm_colour)
        }
        p <- p + labs(title=input$title, x=input$xaxis, y=input$yaxis)
        
        if(input$lm_as_title) {
            m <- model()
            
            if(input$formula == "y~x") {
                eq <- paste("y = ", m$coefficients[[2]], "x + ", m$coefficients[[1]])
            } else if(input$formula == "y~poly(x, 2)") {
                eq <- paste("y = ", m$coefficients[[3]], "x^2 + ", m$coefficients[[2]], "x + ", m$coefficients[[1]])
            } else {
                eq <- paste("y = ", m$coefficients[[4]], "x^3 + ", m$coefficients[[3]], "x^2 + ", m$coefficients[[2]], "x + ", m$coefficients[[1]])
            }
            
            p <- p + labs(title=eq)
        }
            
        p
    })
    
    output$model <- renderTable({
        m <- model()
        
        print(m$coefficients)
        print(summary(m)$r.squared)
        
        m$coefficients['R squared'] <- summary(m)$r.squared
        parameters <- as.data.frame(m$coefficients)
        colnames(parameters) <- c("Values")
        t(parameters)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
