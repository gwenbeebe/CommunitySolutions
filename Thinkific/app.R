library(shiny)
library(shinyWidgets)
library(ggplot2)

load('images/thinkific.RData')

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(width = 12, offset = 0,
        selectInput(
            inputId = "question_input",
            label = "Label",
            choices = all_questions %>%
                select(question) %>%
                distinct()
        ))
    ),
    fluidRow(
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("bar")
        )
    ),
    fluidRow(
        column(12,
               tableOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    color_scheme = c("#F2E8B3", "#F25D27", "#F28F6B", "#F24141", 
                     "#0D0D0D", "#F2BC8D", "#A64F03", "#F2E8B3")
    
    output$table <- renderTable(
        question_responses %>%
            filter(Question ==
                       as.character(
                           all_questions %>%
                               filter(question == input$question_input) %>%
                               select(hash))
                       )
        )
    
    
    reactive_data = reactive({
        question_responses %>%
            filter(Question ==
                       as.character(
                           all_questions %>%
                               filter(question == input$question_input) %>%
                               select(hash)))
    })
    
    output$bar <- renderPlot({

        reactive_data() %>% 
            ggplot(., aes(y=Answer, fill = Answer)) +
            geom_bar(stat='count') +
            scale_fill_manual(values= color_scheme)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
