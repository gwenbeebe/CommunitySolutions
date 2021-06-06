all_surveys <- list.files(
  paste0(getwd(), "/cleaned_data"),
  full.names = TRUE)

complete_response_table <- data.frame(Course.Name = character(), 
                                      Survey.Name = character(),
                                      Student.Email = character(),
                                      Date.Completed = character(),
                                      Question = character(),
                                      Answer = character())

for (clean_file in all_surveys) {
  responses <- read.csv(clean_file) %>%
    select(-X)
  
  complete_response_table <- complete_response_table %>%
    full_join(responses, 
              by = c("Course.Name", "Survey.Name", "Student.Email",
                     "Date.Completed", "Question", "Answer"))
}


write.csv(complete_response_table, file = "complete_response_table.csv")

