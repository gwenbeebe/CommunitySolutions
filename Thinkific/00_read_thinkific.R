# Copyright (C) 2021  Gwen Beebe
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.


##  read in libraries we need
library(rlang)
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(lubridate)


##  pick file to read in
get_file <- file.choose()


##  create prefix for tables
{
  ##  get the actual file name
  file_name <- sapply(strsplit(get_file,"\\\\"), `[`, 11)
  ##  capitalize all letters in Pre and Post, if applicable
  file_name <- gsub("Pre", "PRE", file_name)
  file_name <- gsub("Post", "POST", file_name)
  ##  remove all lower case letters
  uppercase_in_file_name <- strsplit(file_name, "[[:lower:]]*")
  ##  squish the upper case letters together and save them as a prefix for later
  prefix <- gsub("[^A-Z]","", uppercase_in_file_name)
}


##  create list to store table names
table_list <- c()


##  read in file information
for(i in 1:length(excel_sheets(get_file))) {
  ##  do initial file read
  file_hold <- read_excel(get_file, sheet = excel_sheets(get_file)[i])
  ##  check if there's an extra header row; if there is, read again but ignore the first row 
  if (colnames(file_hold)[1] == "Column1") {
    file_hold <- read_excel(get_file, sheet = excel_sheets(get_file)[i], skip = 1)
  }
  
  ##  get the top left cell to identify the group included in this table to use as suffix
  suffix <- str_squish((str_replace_all(file_hold[[1,1]], regex("\\W+"), "")))
  ##  join the prefix from earlier and the suffix to create a unique table name
  table_name <- paste0(prefix, "_", suffix)
  ##  name the table 
  assign(table_name, file_hold)
  
  ##  store the table name in a list so we can look it up later
  table_list <- c(table_list, table_name)
}


##  create empty data frame--the next loop will fill this up with questions
all_questions <- data.frame(hash = character(), 
                        question = character())


##  create table holding all questions and their locations in each response table
for(i in 1:length(table_list)) {
  ##  gets the next response table
  responses <- get(table_list[i])
  
  questions <- responses %>% 
    ##  keep only columns containing questions
    select(contains("Question ")) %>%
    ##  question columns are the same all the way down, so we just keep the top row
    slice(1L)
  
  ##  this adds a second row and rotates our table
  question_matrix <- t(
    rbind(
      as.matrix(questions), 
      ##  This adds a second row that just holds sequential numbers.
      ##  This will become our position column when rotated
      c(sprintf(
        "%d", 
        seq(1, length(as.matrix(questions)))
      ))
    ))  
  
  ##  row name are meaningless once questions are joined, so delete them
  rownames(question_matrix) <- NULL
  ##  change this back to a data frame 
  question_df <- as.data.frame(question_matrix)
  ##  rename our columns; the first one holds our question and the second has locations by response table
  colnames(question_df) <- c("question", table_list[i])
  ##  create a code to represent our question for renaming answer columns
  question_df$hash <- lapply(question_df$question, function(x) {hash(x)})
  
  ##  add the questions to the container we created above
  all_questions <- all_questions %>%
    ##  if this question is already in the container, update that row; otherwise, add it
    full_join(question_df <- question_df %>%
                mutate(hash = unlist(hash)) %>%
                select(hash, question, table_list[i]),
              ##  we join to the columns that will always be the same between response tables
              ##  (the hash and question text), so the only /new/ column is the position
              ##  column specific to the response table we're currently processing
              by = c("hash", "question"))
}


##  now that we have codes for every question, rename answer headers to match
for(i in 1:length(table_list)) {
  ##  go grab the response table we're working with on this loop and save it
  response_holding <- get(table_list[i]) %>% 
    ##  keep only columns not containing questions
    select(-contains("Question "))
  
  ##  we need to get the hash code for each question, based on response table and position
  get_hashes <- all_questions %>%
    ##  keep only the columns with the code and the relevant position
    select(hash, table_list[[i]]) %>%
    ##  remove any rows for questions that weren't asked of this group
    na.omit() %>%
    ##  positions are stored as text, change them to numbers so we can put them in order
    mutate(position = as.numeric(.[[2]])) %>%
    ##  put codes in numeric order by position
    arrange(position)
  
  ##  update the column names in the response table we're working with 
  colnames(response_holding) <- c(
    ##  columns prior to the questions keep their original names
    colnames(response_holding)[1:which(colnames(response_holding) == "Total Number of Questions")],
    ##  question columns are renamed with the ordered codes from the previous step
    get_hashes$hash)
  
  ##  replace the original table we grabbed with the one we just updated
  assign(table_list[i], response_holding)
}


##  remove all extra variables created in our loops
rm(list = ls()[!(ls() %in% c(
  ls(pattern = prefix),
  "all_questions",
  "question_table"
))])


## get the additional question information from a separate file and read it in
# question_table <- read_excel(file.choose())
# question_table$hash <- lapply(question_table$Question, function(x) {hash(x)})
# 
# combined <- all_questions %>%
#   full_join(question_table%>%
#               mutate(hash = unlist(hash))
#             , by = "hash")
# 
# write.csv(combined, file = "qs.csv")


##  create an empty frame to hold all the response information
question_responses <- data.frame('Course Name' = character(), 
                                 'Survey Name' = character(),
                                 'Student Email' = character(),
                                 # 'Date Completed' = as.Date(character()),
                                 ## storing as character for now
                                 ## some files have dates that cannot be easily parsed
                                 'Date Completed' = character(),
                                 'Question' = character(),
                                 'Answer' = character())
##  R doesn't like column names with spaces, so we force them here
colnames(question_responses) <- c(
  'Course Name', 'Survey Name', 'Student Email', 'Date Completed', 'Question', 'Answer')


for (i in 1:length(all_questions)) {
  ##  pick question to get responses for
  isolated_question <- all_questions[i, ]
  ##  create a list of all the groups that were asked that question
  responding_groups <- isolated_question[ , colSums(is.na(isolated_question)) < nrow(isolated_question)]
  ##  grab the code that represents that question
  question <- responding_groups[[1]]
  
  ##  the first two columns are the question code and the question text, so we skip those
  for (j in 3:length(responding_groups)) {
    ##  grab every column besides those two and go look up the responses
    group_responses <- get(colnames(responding_groups)[j]) %>%
      mutate(Question = question,
             ## remove this one once date issue is solved
             'Date Completed' = as.character('Date Completed')) %>%
      select('Course Name',
             'Survey Name', 
             'Student Email',
             'Date Completed',
             'Question',
             all_of(question)
      )
    
    colnames(group_responses) <- c(
      'Course Name', 'Survey Name', 'Student Email', 'Date Completed', 'Question', 'Answer')
    
    ##  add these responses to the holding table
    question_responses <- question_responses %>%
      full_join(group_responses,
                by = colnames(question_responses))
  }
  
}

rm(list = ls()[!(ls() %in% c(
  "question_responses"
))])


