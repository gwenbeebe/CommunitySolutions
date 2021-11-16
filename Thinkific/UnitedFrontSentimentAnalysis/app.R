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
library(shinyscreenshot)
library(wordcloud2)
library(tm)
library(colourpicker)

`%nin%` = Negate(`%in%`)
drop_auth(rdstoken = "token.rds")
CSPalette <- c("#BF3B5E", "#F2F2F2", "#735371", "#30698C", "#BFAABA", "#BF3F57", "#385C73")

free_responses <- drop_read_csv("CodedResponseTableTrimmed.csv") %>%
    select(-"Participant.Method") %>%
    distinct() %>%
    filter(Answer != "")
sentiment_options <- drop_read_csv("sentiment_compare.csv")

# free_responses <- read.csv("CodedResponseTableTrimmed.csv")
# sentiment_options <- read.csv("sentiment_compare.csv")


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("smile")),
        menuItem("Analysis Options", icon = icon("th"), tabName = "options",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("Word Cloud", icon = icon("cloud"), tabName = "wordcloud",
                 badgeLabel = "new", badgeColor = "green")
    )
    
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "sentiment",
                fluidPage(
                    sidebarLayout(
                        sidebarPanel(
                            selectInput(
                                label = "Module",
                                inputId = "module",
                                multiple = TRUE,
                                choices = unique(free_responses$Module.Name)),
                            selectInput(
                                label = "Respondent",
                                inputId = "respondent",
                                multiple = TRUE,
                                choices = unique(free_responses$Respondent.Type)),
                            selectInput("survey", "Survey", choices = NULL),
                            selectInput("question", "Question", choices = NULL)
                        ),
                        mainPanel(
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
                            tableOutput("test")
                        )
                    )
                )
        ),
        tabItem(tabName = "options",
                fluidPage(
                    fluidRow(
                        box(
                            pickerInput(
                                label = "Question to Evaluate",
                                inputId = "eval_question",
                                choices = unique(sentiment_options$Combined.Questions))),
                        box(
                            textOutput("progress"))),
                    fluidRow(
                        box(title=textOutput("question_text"),
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            p(textOutput('answer_text'))
                        )
                    ),
                    fluidRow(
                        box(title = textOutput("syuzhet_class"),
                            width = 2,
                            actionButton("syuzhet", "Syuzhet")),
                        box(title = textOutput("bing_class"),
                            width = 2,
                            actionButton("bing", "Bing")),
                        box(title = textOutput("afinn_class"),
                            width = 2,
                            actionButton("afinn", "Afinn")),
                        box(title = textOutput("nrc_class"),
                            width = 2,
                            actionButton("nrc", "NRC")),
                        actionButton("next_q", "Next Answer")
                    ),
                    fluidRow(plotOutput("methodPlot")),
                    fluidRow(tableOutput("full_row"))
                )
        ),
        tabItem(tabName = "wordcloud",
                fluidPage(
                    # Create a container for tab panels
                    tabsetPanel(
                        # Create a "Word cloud" tab
                        tabPanel(
                            title = "Word cloud",
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput(
                                        label = "Module",
                                        inputId = "wc_module",
                                        multiple = TRUE,
                                        choices = unique(free_responses$Module.Name)),
                                    selectInput(
                                        label = "Respondent",
                                        inputId = "wc_respondent",
                                        multiple = TRUE,
                                        choices = unique(free_responses$Respondent.Type)),
                                    selectInput("wc_survey", "Survey", choices = NULL),
                                    selectInput("wc_question", "Question", choices = NULL),
                                    hr(),
                                    checkboxInput("remove_words", "Remove specific words?", FALSE),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1",
                                        textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
                                        textAreaInput("words_to_remove2", "", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
                                        textAreaInput("words_to_remove3", "", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
                                        textAreaInput("words_to_remove4", "", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
                                        textAreaInput("words_to_remove5", "", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
                                        textAreaInput("words_to_remove6", "", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
                                        textAreaInput("words_to_remove7", "", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
                                        textAreaInput("words_to_remove8", "", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
                                        textAreaInput("words_to_remove9", "", rows = 1)
                                    ),
                                    conditionalPanel(
                                        condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
                                        textAreaInput("words_to_remove10", "", rows = 1)
                                    ),
                                    hr(),
                                    numericInput("num", "Maximum number of words",
                                                 value = 30, min = 5
                                    )
                                ),
                                mainPanel(
                                    wordcloud2Output("cloud"),
                                    br(),
                                    br()
                                )
                            )
                        ),
                        # Create an "About this app" tab
                        tabPanel(
                            title = "About this app",
                            h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),
                            br(),
                            br(),
                            em("Adapted from DataCamp"),
                            br(),
                        )
                    )))
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
                col = CSPalette,
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
    
    
    sentiments <- reactive({
        filter(sentiment_options, Combined.Questions == input$eval_question)
    })
    
    values <- reactiveValues()
    values$created <- data.frame(matrix(c(0,0,0,0), nrow = 1)) %>%
        `colnames<-`(c("syuzhet", "bing", "afinn", "nrc"))
    values$evaluated <- 0
    values$answer_text <- ""
    values$question_text <- "Question"
    values$sentiment_length <- sample(1:10)
    values$sentiment_count <- 0
    
    observeEvent(input$eval_question,{
        values$sentiment_length <- sample(1:nrow(sentiments()))
        values$evaluated <- 1
        values$sentiment_count <- nrow(sentiments())
        values$question_text <- sentiments()$Combined.Questions[1]
        values$answer_text <- sentiments()$Answer[values$sentiment_length[values$evaluated]]
        values$syuzhet_class <- sentiments()$syuzhet[values$sentiment_length[values$evaluated]]
        values$bing_class <- sentiments()$bing[values$sentiment_length[values$evaluated]]
        values$afinn_class <- sentiments()$afinn[values$sentiment_length[values$evaluated]]
        values$nrc_class <- sentiments()$nrc[values$sentiment_length[values$evaluated]]
    })
    
    observeEvent(input$syuzhet, {values$created$syuzhet <- values$created$syuzhet + 1})
    observeEvent(input$bing, {values$created$bing <- values$created$bing + 1})
    observeEvent(input$afinn, {values$created$afinn <- values$created$afinn + 1})
    observeEvent(input$nrc, {values$created$nrc <- values$created$nrc + 1})
    
    observeEvent(input$next_q, {
        values$evaluated <- values$evaluated + 1
        values$answer_text <- sentiments()$Answer[values$sentiment_length[values$evaluated]]
        values$syuzhet_class <- sentiments()$syuzhet[values$sentiment_length[values$evaluated]]
        values$bing_class <- sentiments()$bing[values$sentiment_length[values$evaluated]]
        values$afinn_class <- sentiments()$afinn[values$sentiment_length[values$evaluated]]
        values$nrc_class <- sentiments()$nrc[values$sentiment_length[values$evaluated]]})
    
    output$methodPlot <- renderPlot({
        ggplot(melt(values$created), aes(x = variable, y = value)) +
            geom_col(fill = "#BF3B5E") +
            geom_text(aes(label = value), vjust = -.5) +
            scale_fill_manual(values = CSPalette)
    })
    
    # output$methodCount <- renderTable({
    #     values$created
    # })
    
    output$question_text <- renderText({values$question_text})
    output$answer_text <- renderText({values$answer_text})
    output$progress <- renderText({
        paste(as.numeric(values$evaluated) - 1, " evaluated / ",
              as.numeric(values$sentiment_count), " total")
    })
    output$syuzhet_class <- renderText({values$syuzhet_class})
    output$bing_class <- renderText({values$bing_class})
    output$afinn_class <- renderText({values$afinn_class})
    output$nrc_class <- renderText({values$nrc_class})
    
    
    
    
    wc_module <- reactive({
        filter(free_responses, Module.Name %in% input$wc_module)
    })
    observeEvent(wc_module(), {
        choices <- unique(wc_module()$Respondent.Type)
        updateSelectInput(inputId = "wc_respondent", choices = choices) 
    })
    
    wc_respondent <- reactive({
        req(input$wc_respondent)
        filter(wc_module(), Respondent.Type %in% input$wc_respondent)
    })
    observeEvent(wc_respondent(), {
        choices <- unique(wc_respondent()$Survey.Type)
        updateSelectInput(inputId = "wc_survey", choices = choices, selected = NULL)
    })
    
    wc_survey <- reactive({
        req(input$wc_survey)
        filter(wc_respondent(), Survey.Type == input$wc_survey)
    })
    observeEvent(wc_survey(), {
        choices <- unique(wc_survey()$Combined.Questions)
        updateSelectInput(inputId = "wc_question", choices = choices, selected = NULL)
    })
    
    data_source <- reactive({
        req(input$wc_question)
        answered <- wc_survey() %>% 
            filter(Combined.Questions == input$wc_question)
        data <- answered$Answer
        return(data)
    })
    
    create_wordcloud <- function(data, num_words = 15, background = "white") {
        req(input$wc_question)
        if (is.character(data)) {
            corpus <- Corpus(VectorSource(data))
            corpus <- tm_map(corpus, tolower)
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, removeNumbers)
            corpus <- tm_map(corpus, removeWords, stopwords(tolower("English")))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
            corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
            tdm <- as.matrix(TermDocumentMatrix(corpus))
            data <- sort(rowSums(tdm), decreasing = TRUE)
            data <- data.frame(word = names(data), freq = as.numeric(data))
        }
        if (!is.numeric(num_words) || num_words < 3) {
            num_words <- 3
        }
        data <- head(data, n = num_words)
        if (nrow(data) == 0) {
            return(NULL)
        }
        wordcloud2(data, 
                   color = sample(c("#BF3B5E", "#735371", "#30698C", "#BFAABA", "#BF3F57", "#385C73"), num_words, replace = TRUE),
                   backgroundColor = background)
    }
    output$cloud <- renderWordcloud2({
        create_wordcloud(data_source(),
                         num_words = input$num
        )
    })
}


shinyApp(ui = ui, server = server)
