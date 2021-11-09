library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(googlesheets4)
library(rdrop2)

`%nin%` = Negate(`%in%`)
drop_auth(rdstoken = "token.rds")

# loaded_data <- read.csv("FreeTextAnswers.csv") %>%
#     filter(!is.na(Answer) &
#                Combined.Questions %nin% c(
#                    "In what zip code do you currently live?",
#                    "In what zip code is your workplace located?",
#                    "What is your job title?",
#                    "Who is your employer?",
#                    "How did you participate in this session?",
#                    "What happens in our brain when we feel judged, devalued, or not accepted?",
#                    "United Front would like to get some feedback from participants to help with planning for 2022. We thank you in advance for your open and honest responses.On a scale of 1 to 10 with 1 being very dissatisfied and 10 being very satisfied, how satisfied are you with the United Front program so far?"
#                ))
# s <- get_nrc_sentiment(iconv(loaded_data$Answer))
# free_responses <- cbind(loaded_data, s)

# free_responses <- read_sheet("https://docs.google.com/spreadsheets/d/1YaAzNMBca6_RU9VD2v1DNTz_HRcq5b3ypwDGlu-C4o8/edit#gid=609288075") %>%
#     filter(Answer != "")

free_responses <- drop_read_csv("CodedResponseTableTrimmed.csv") %>%
    filter(Answer != "")
sentiment_options <- drop_read_csv("sentiment_compare.csv")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("smile"))#,
        # menuItem("Analysis Options", icon = icon("th"), tabName = "widgets",
        #          badgeLabel = "new", badgeColor = "green")
    ),
    pickerInput(
        label = "Module",
        inputId = "module",
        multiple = TRUE,
        choices = unique(free_responses$Module.Name)),
    pickerInput(
        label = "Respondent",
        inputId = "respondent",
        multiple = TRUE,
        choices = unique(free_responses$Respondent.Type),
        options = pickerOptions(
            liveSearch = TRUE, liveSearchStyle = 'contains', actionsBox = TRUE)),
    selectInput("survey", "Survey", choices = NULL),
    selectInput("question", "Question", choices = NULL)
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "sentiment",
                fluidPage(
                    fluidRow(valueBoxOutput("response_count"),
                             downloadButton("download_responses", "Download All")),
                    fluidRow(plotOutput("sentimentPlot")),
                    fluidRow(actionButton("anger", "Anger"),
                             actionButton("anticipation", "Anticipation"),
                             actionButton("disgust", "Disgust"),
                             actionButton("fear", "Fear"),
                             actionButton("joy", "Joy"),
                             actionButton("sadness", "Sadness"),
                             actionButton("surprise", "Surprise"),
                             actionButton("trust", "Trust"),
                             actionButton("negative", "Negative"),
                             actionButton("positive", "Positive"),
                             actionButton("reset", "- Reset -")),
                    tableOutput("test"),
                    tableOutput("data")
                )
        ),
        tabItem(tabName = "widgets",
                h2("Widgets tab content")
        )
    )
)


ui <- dashboardPage(
    dashboardHeader(title = "Free Text Responses"),
    sidebar,
    body,
    skin = "black"
)


server <- function(input, output) {
    
    module <- reactive({
        filter(free_responses, Module.Name %in% input$module)
    })
    observeEvent(module(), {
        choices <- unique(module()$Respondent.Type)
        updateSelectInput(inputId = "respondent", choices = choices) 
    })
    
    respondent <- reactive({
        req(input$respondent)
        filter(module(), Respondent.Type %in% input$respondent)
    })
    observeEvent(respondent(), {
        choices <- unique(respondent()$Survey.Type)
        updateSelectInput(inputId = "survey", choices = choices, selected = NULL)
    })
    
    survey <- reactive({
        req(input$survey)
        filter(respondent(), Survey.Type == input$survey)
    })
    observeEvent(survey(), {
        choices <- unique(survey()$Combined.Questions)
        updateSelectInput(inputId = "question", choices = choices, selected = NULL)
    })
    
    output$response_count <- renderValueBox({
        answered <- survey() %>% 
            filter(Combined.Questions == input$question)
        
        valueBox(
            length(unique(answered$Student.Email)), 
            "Respondents", icon = icon("users", lib = "font-awesome"),
            color = "purple"
        )
    })
    
    output$sentimentPlot <- renderPlot({
        req(input$question)
        
        selected_data <- survey() %>% 
            filter(Combined.Questions == input$question) %>%
            select(anger, anticipation, disgust, fear, joy,
                   sadness, surprise, trust, negative, positive)
        barplot(colSums(selected_data),
                las = 2,
                col = c("#BF3B5E", "#F2F2F2", "#735371", "#30698C", 
                        "#BFAABA", "#BF3F57", "#385C73"),
                ylab = 'Total Score',
                main = 'Sentiment')
    })
    
    observeEvent(input$question,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer)
        },
        colnames = FALSE)
    })
    
    observeEvent(input$anger,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "anger"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$anticipation,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "anticipation"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$disgust,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "disgust"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$fear,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "fear"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$joy,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "joy"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$sadness,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "sadness"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$surprise,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "surprise"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$trust,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "trust"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$negative,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "negative"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$positive,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer[selected_data[colnames(selected_data) == "positive"] > 0])
        },
        colnames = FALSE)
    })
    
    observeEvent(input$reset,{
        output$test <- renderTable({
            selected_data <- survey() %>% 
                filter(Combined.Questions == input$question)
            unique(selected_data$Answer)
        },
        colnames = FALSE)
    })
    
    output$download_responses <- downloadHandler(
        filename = function() {
            paste0(input$module, "-", input$respondent,
                   "-", input$survey, "-responses.csv")},
        content = function(file) {
            write.csv(survey() %>%
                          filter(Combined.Questions == input$question),
                      file)
        }
    )
    
    
}


shinyApp(ui = ui, server = server)
