library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


file_hold <- file.choose()
free_responses <- read.csv(file_hold) %>%
  filter(!is.na(Answer) &
           Combined.Questions %nin% c(
             "In what zip code do you currently live?",
             "In what zip code is your workplace located?",
             "What is your job title?",
             "Who is your employer?",
             "How did you participate in this session?",
             "What happens in our brain when we feel judged, devalued, or not accepted?",
             "United Front would like to get some feedback from participants to help with planning for 2022. We thank you in advance for your open and honest responses.On a scale of 1 to 10 with 1 being very dissatisfied and 10 being very satisfied, how satisfied are you with the United Front program so far?"
           ))
  
names(free_responses)[1] <- "rownum"

for (i in c("Survey.Type", "Module.Name", "Respondent.Type",
            "Participant.Method", "Combined.Questions")) {
  assign(paste0(i, "s"), 
         free_responses %>%
           select(all_of(i)) %>%
           distinct())
}

test_set <- free_responses %>%
  filter(Module.Name == "Microaggressions" &
           Respondent.Type == "Community" &
           Survey.Type == "Keynote" &
           Participant.Method == "Live" &
           Combined.Questions == "What did you like about this session?")

answers <- iconv(test_set$Answer)
s <- get_sentiment(answers, method = "bing")
test <- data.frame(answers)



test <- read.csv("UnitedFrontSentimentAnalysis\\FreeTextAnswers.csv") %>%
      filter(!is.na(Answer) &
                 Combined.Questions %nin% c(
                     "In what zip code do you currently live?",
                     "In what zip code is your workplace located?",
                     "What is your job title?",
                     "Who is your employer?",
                     "How did you participate in this session?",
                     "What happens in our brain when we feel judged, devalued, or not accepted?",
                     "United Front would like to get some feedback from participants to help with planning for 2022. We thank you in advance for your open and honest responses.On a scale of 1 to 10 with 1 being very dissatisfied and 10 being very satisfied, how satisfied are you with the United Front program so far?"
                 )) %>%
  filter(Answer != "")
answers <- iconv(test$Answer)
test$syuzhet <- get_sentiment(answers, method = "syuzhet")
test$bing <- get_sentiment(answers, method = "bing")
test$afinn <- get_sentiment(answers, method = "afinn")
test$nrc <- get_sentiment(answers, method = "nrc")
test_labelled <- test %>%
  mutate(syuzhet = case_when(syuzhet < 0 ~ "Neg",
                             syuzhet > 0 ~ "Pos",
                             TRUE ~ "Neu"),
         bing = case_when(bing < 0 ~ "Neg",
                          bing > 0 ~ "Pos",
                          TRUE ~ "Neu"),
         afinn = case_when(afinn < 0 ~ "Neg",
                           afinn > 0 ~ "Pos",
                           TRUE ~ "Neu"),
         nrc = case_when(nrc < 0 ~ "Neg",
                         nrc > 0 ~ "Pos",
                         TRUE ~ "Neu"))

test_filtered <- test_labelled %>%
  filter(syuzhet != bing | syuzhet != afinn | syuzhet != nrc |
           bing != afinn | bing != nrc | afinn != nrc) %>%
  select(-c("?..rownum", "Participant.Method")) %>%
  distinct()
write.csv(test_filtered, "sentiment_compare.csv")


s <- get_nrc_sentiment(answers)

new <- cbind(test_set, s)
test_1 <- new %>%
  select(anger, anticipation, disgust, fear, joy,
         sadness, surprise, trust, negative, positive)

barplot(colSums(s),
        las = 2,
        col = c("#BF3B5E", "#F2F2F2", "#735371", "#30698C", 
                "#BFAABA", "#BF3F57", "#385C73"),
        ylab = 'Total Score',
        main = 'Sentiment')

sentiment <- "anticipation"

answers <- iconv(test_set$Answer)
s <- get_nrc_sentiment(answers)
answers[s[colnames(s) == sentiment] > 0]



#####

file_name <- file.choose()
loaded_data <- read.csv(file_name) %>%
  filter(!is.na(Answer) &
           Combined.Questions %nin% c(
             "In what zip code do you currently live?",
             "In what zip code is your workplace located?",
             "What is your job title?",
             "Who is your employer?",
             "How did you participate in this session?",
             "What happens in our brain when we feel judged, devalued, or not accepted?",
             "United Front would like to get some feedback from participants to help with planning for 2022. We thank you in advance for your open and honest responses.On a scale of 1 to 10 with 1 being very dissatisfied and 10 being very satisfied, how satisfied are you with the United Front program so far?"
           ))
s <- get_nrc_sentiment(iconv(free_responses$Answer))
full_table <- cbind(free_responses, s)
write.csv(full_table, file = "CodedResponseTable.csv")


test <- unique(full_table$Answer[full_table[colnames(full_table) == "fear"] > 0])

free_responses <- read.csv("UnitedFrontSentimentAnalysis\\CodedResponseTable.csv") %>%
  filter(Answer != "")

created <- data.frame(matrix(c(0,0,0,0), nrow = 1))
colnames(created) <- c("syuzhet", "bing", "afinn", "nrc")

ggplot(melt(test), aes(x = variable, y = value)) +
  geom_col() +
  geom_text(aes(label = value), vjust = -0.2)


random_order <- sample(1:nrow(free_responses))

free_responses[1,8]
free_responses[1,10]
