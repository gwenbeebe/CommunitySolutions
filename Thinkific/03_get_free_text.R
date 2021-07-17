##  this code is NOT currently freestanding

keynote_free_text <- question_responses %>%
  filter(Question %in% c(
    "b8e47823a56e28cc44c437dd50246354",
    "4f2828b67671f45c14c4b248c5f31f91",
    "891f986ce6bad8618be7049de09df9b7",
    "9a38b8023834f314c58d0c2855d73a0f"
  ),
  !is.na(Answer)) %>%
  #mutate(Answer = paste("lll", Answer)) %>%
  left_join(combined, by = c("Question" = "hash")) %>%
  select(Survey.Name, Course.Name, question, Answer)


write.csv(keynote_free_text, file = "text_responses.csv")

text_holding <- data.frame(Survey.Name = as.character(),
                           Group = as.character(),
                           Question = as.character(),
                           Answer = as.character())

for (response in 1:nrow(keynote_free_text)){
  chosen_row <- keynote_free_text[response,]
  
  
  for (word in strsplit(gsub("[^[:alnum:] ]", "", chosen_row[[4]], " +")[[1]], " ")) {
    if (length(word) > 0) {
      text_holding <- rbind(text_holding, data.frame(Survey.Name = chosen_row[[1]],
                                                     Group = chosen_row[[2]],
                                                     Question = chosen_row[[3]],
                                                     Answer = tolower(word)))
    }
  }
}

text_to_save <- text_holding %>%
  filter(Answer != "")

write.csv(text_to_save, "free_text.csv")
