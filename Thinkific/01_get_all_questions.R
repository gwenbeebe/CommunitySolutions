# # Copyright (C) 2021  Gwen Beebe
# #
# # This program is free software: you can redistribute it and/or modify
# # it under the terms of the GNU Affero General Public License as published
# # by the Free Software Foundation, either version 3 of the License, or
# # any later version.
# #
# # This program is distributed in the hope that it will be useful,
# # but WITHOUT ANY WARRANTY; without even the implied warranty of
# # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# # GNU Affero General Public License for more details at
# # <https://www.gnu.org/licenses/>.
# 
# 
# ##  read in libraries we need
# library(rlang)
# library(tidyverse)
# library(lubridate)
# library(readxl)
# library(stringr)
# library(lubridate)
# 
# 
# all_surveys <- list.files(
#   paste0(getwd(), "/data/Thinkific Exports"),
#   full.names = TRUE)
# 
# 
# ##  create empty data frame--the loop will fill this up with questions
# all_questions <- data.frame(hash = character(), 
#                             question = character())
# 
# for (get_file in all_surveys) {
# 
#   ##  create prefix for tables
#   {
#     ##  get the actual file name
#     file_name <- sapply(strsplit(get_file,"\\\\"), `[`, 11)
#     ##  capitalize all letters in Pre and Post, if applicable
#     file_name <- gsub("Pre", "PRE", file_name)
#     file_name <- gsub("Post", "POST", file_name)
#     ##  remove all lower case letters
#     uppercase_in_file_name <- strsplit(file_name, "[[:lower:]]*")
#     ##  squish the upper case letters together and save them as a prefix for later
#     prefix <- gsub("[^A-Z]","", uppercase_in_file_name)
#   }
#   
#   
#   ##  create list to store table names
#   table_list <- c()
#   
#   
#   ##  read in file information
#   for(i in 1:length(excel_sheets(get_file))) {
#     sheet_name <- excel_sheets(get_file)[i]
#     if (sheet_name != "Sheet1") {
#       ##  do initial file read
#       file_hold <- read_excel(get_file, sheet = sheet_name)
#       ##  check if there's an extra header row; if there is, read again but ignore the first row 
#       if (colnames(file_hold)[1] == "Column1") {
#         file_hold <- read_excel(get_file, sheet = excel_sheets(get_file)[i], skip = 1)
#       }
#       ##  get the top left cell to identify the group included in this table
#       group <- file_hold[[1,1]]
#       ##  remove any rows that aren't labeled with that row; rare Thinkific quirk
#       file_hold <- file_hold %>%
#         filter(`Course Name` == group)
#       ##  use group to create suffix
#       suffix <- str_squish((str_replace_all(group, regex("\\W+"), "")))
#       ##  join the prefix from earlier and the suffix to create a unique table name
#       table_name <- paste0(prefix, "_", suffix)
#       ##  name the table 
#       assign(table_name, file_hold)
#       
#       ##  store the table name in a list so we can look it up later
#       table_list <- c(table_list, table_name)
#     }
#   }
#   
#   
#   
#   
#   ##  create table holding all questions and their locations in each response table
#   for(i in 1:length(table_list)) {
#     ##  gets the next response table
#     responses <- get(table_list[i])
#     
#     questions <- responses %>% 
#       ##  keep only columns containing questions
#       select(contains("Question ")) %>%
#       ##  question columns are the same all the way down, so we just keep the top row
#       slice(1L)
#     
#     ##  this adds a second row and rotates our table
#     question_matrix <- t(
#       rbind(
#         as.matrix(questions), 
#         ##  This adds a second row that just holds sequential numbers.
#         ##  This will become our position column when rotated
#         c(sprintf(
#           "%d", 
#           seq(1, length(as.matrix(questions)))
#         ))
#       ))  
#     
#     ##  row name are meaningless once questions are joined, so delete them
#     rownames(question_matrix) <- NULL
#     ##  change this back to a data frame 
#     question_df <- as.data.frame(question_matrix)
#     ##  rename our columns; the first one holds our question and the second has locations by response table
#     colnames(question_df) <- c("question", table_list[i])
#     ##  create a code to represent our question for renaming answer columns
#     question_df$hash <- lapply(question_df$question, function(x) {hash(x)})
#     
#     ##  add the questions to the container we created above
#     all_questions <- all_questions %>%
#       ##  if this question is already in the container, update that row; otherwise, add it
#       full_join(question_df <- question_df %>%
#                   mutate(hash = unlist(hash)) %>%
#                   select(hash, question),
#                 ##  we join to the columns that will always be the same between response tables
#                 ##  (the hash and question text), so the only /new/ column is the position
#                 ##  column specific to the response table we're currently processing
#                 by = c("hash", "question"))
#   }
# }
# 
# 
# ## get the additional question information from a separate file and read it in
question_table <- read_excel(file.choose())
question_table$hash <- lapply(question_table$Question, function(x) {hash(x)})

combined <- all_questions %>%
  full_join(question_table%>%
              mutate(hash = unlist(hash))
            , by = "hash") %>%
  distinct(hash, .keep_all = TRUE)

write.csv(combined, file = "joined_questions_10.24.21.csv",
          row.names = FALSE)
