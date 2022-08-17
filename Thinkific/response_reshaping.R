library(tidyverse)

response_file <- file.choose()
responses <- read.csv(response_file) %>%
  mutate(FullSurveyTitle = paste(Module.Name, "-", Survey.Type))

students <- responses %>%
  select(Email, Respondent.Type) %>%
  distinct()

module_completion <- responses %>%
  select(Email, Respondent.Type, Module.Name, 
         Survey.Type, Participant.Method, Day.of.Date.Completed) %>%
  distinct()

surveys <- responses %>%
  select(Module.Name, Survey.Type, FullSurveyTitle) %>%
  distinct () %>%
  mutate(
    Module.Name = factor(
      Module.Name, 
      levels = c("Individual Bias", 
                 "Organizational Bias", "Microaggressions", 
                 "Stereotype Threat", "Imposter Syndrome", 
                 "High Context Dependency", "Allyship in Shared Humanity", 
                 "Privilege, Power, and Position", "Actions of an Ally", 
                 "Facing the Fears of Open Dialogue", "Embracing Your Journey")),
    Survey.Type = factor(
      Survey.Type,
      levels = c("Welcome Survey", "Pre Survey", "Keynote", "Discussion Group",
                 "Wrap-Up Survey")
    )) %>%
  arrange(Module.Name, Survey.Type)
  
all_data <- students

for (i in 1:nrow(surveys)) {
  survey_name <- surveys[i,3]
  survey_responses <- responses %>%
    filter(FullSurveyTitle == survey_name)
  
  completion_info <- survey_responses %>%
    select(Email, Respondent.Type, Day.of.Date.Completed, Participant.Method) %>%
    arrange(desc(Day.of.Date.Completed)) %>%
    distinct(Email, Respondent.Type, .keep_all = TRUE)
    
  colnames(completion_info) <- c("Email", "Respondent.Type",
                                 paste(survey_name, "- Day Completed"),
                                 paste(survey_name, "- Method"))
  
  all_data <- all_data %>%
    left_join(completion_info,
              by = c("Email", "Respondent.Type"))
  
  questions <- survey_responses %>%
    select(Question) %>%
    distinct()
  
  for (j in 1:nrow(questions)) {
    column_name <- paste(survey_name, "-", questions[j,1])
    question_responses <- survey_responses %>%
      filter(Question == questions[j,1]) %>%
      select(Email, Respondent.Type, Answer) %>%
      distinct(Email, Respondent.Type, .keep_all = TRUE) %>%
      rename(!!column_name := Answer)
    
    all_data <- all_data %>%
      left_join(question_responses,
                by = c("Email", "Respondent.Type"))
  }
  
}

file <- file.choose()
student_names <- read.csv(file) %>%
  select(Email, First.Name, Last.Name, Last.sign.in, Sign.in.count,
         Enrollments, ) %>%
  distinct(Email, .keep_all = TRUE)

file <- file.choose()
student_employers <- read.csv(file) %>%
  filter(!(Group %in% c("United Front PACE", "NOT REQUIRED - United Front 2020 Content",
                        "Test Course", "United Front Keynotes Only"))) %>%
  mutate(Group = str_remove(Group, " R"), 
         Respondent.Type = str_remove(Group, fixed("."))) %>%
  select(Email, Company.Name, Respondent.Type, enrollment_created_at, percentage_completed,
         percentage_completed_binned) %>%
  distinct(Email, Respondent.Type, .keep_all = TRUE)

file <- file.choose()
employer_info <- read.csv(file)
colnames(employer_info) <- c("Company.Name", "Industry", "Category" )

extra_student_data <- student_names %>%
  full_join(student_employers, by = "Email") %>%
  left_join(employer_info, by = "Company.Name") %>%
  full_join(all_data, by = c("Email", "Respondent.Type"))

file <- file.choose()
survey_monkey <- read_csv(file) %>%
  select(-c("Respondent ID", "Collector ID", "Start Date", "IP Address"))
survey_monkey <- survey_monkey %>%
  `colnames<-` (c("Embracing Your Journey - Wrap-Up Survey - Day Completed",
                 "Email",
                 paste0("Embracing Your Journey - Wrap-Up Survey - ",
                        colnames(survey_monkey)[3:38]))) 

last_survey <- survey_monkey %>%
  union(extra_student_data %>%
          select(colnames(survey_monkey))
        , by = colnames(survey_monkey)) %>%
  group_by(Email) %>%
  slice(1L) %>%
  ungroup()

extra_student_data3 <- extra_student_data %>%
  select(-c(colnames(survey_monkey)[1:1],
            colnames(survey_monkey)[3:length(colnames(survey_monkey))])) %>%
  left_join(last_survey, by = "Email")

write.csv(extra_student_data3, "all_response_data v5.csv", row.names = FALSE)
