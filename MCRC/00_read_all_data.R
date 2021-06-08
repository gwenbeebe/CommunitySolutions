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
# library(rlang)
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(janitor)


##  pick arrests file to read in
arrest_file <- file.choose(); arrest_data <- read_excel(arrest_file)

##  pick releases file to read in
release_file <- file.choose(); release_data <- read_excel(release_file)

##  pick admissions file to read in
admission_file <- file.choose(); admission_data <- read_excel(admission_file)

arrest_columns <- as.data.frame(colnames(arrest_data))
release_columns <- as.data.frame(colnames(release_data))
admission_columns <- as.data.frame(colnames(admission_data))


# add key columns and remove duplicates
arrest_data_clean <- arrest_data %>%
  mutate(person_id = paste0(LN3, 
                            FN3, 
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         arrest_row_id = row_number()) %>%
  distinct(person_id, BookingDate, .keep_all = TRUE)

admission_data_clean <- admission_data %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         admit_row_id = row_number()) %>%
  distinct(person_id, INTKDT, .keep_all = TRUE)

release_data_clean <- release_data %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         release_row_id = row_number()) %>%
  distinct(person_id, FACRLD, .keep_all = TRUE)


# create recidivism summary
recidivism <- release_data_clean %>%
  select(person_id, FACRLD) %>%
  left_join(admission_data_clean %>%
              select(person_id, INTKDT),
            by = "person_id") %>%
  mutate(return_flag = case_when(
    FACRLD < INTKDT 
      & (FACRLD + years(2)) >= INTKDT
      ~ TRUE,
    TRUE ~ FALSE)) %>%
  arrange(desc(return_flag)) %>%
  select(-INTKDT) %>%
  group_by(person_id, FACRLD) %>%
  slice(1L) %>%
  ungroup()


# create summary text table
summary_table <- recidivism %>%
  mutate(month_of_release = floor_date(FACRLD, "month")) %>%
  tabyl(month_of_release, return_flag) %>%
  mutate(month_of_release = paste(month(month_of_release, label = TRUE), year(month_of_release))) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(0) %>%
  adorn_ns()


# create draft chart for exploration
chart_table <- recidivism %>%
  mutate(month_of_release = floor_date(FACRLD, "month")) %>%
  group_by(month_of_release, return_flag) %>%
  summarise(people = n()) %>%
  arrange(month_of_release) %>%
  mutate(month_of_release = paste(month(month_of_release, label = TRUE), year(month_of_release)))

ggplot(chart_table, aes(fill=return_flag, y=people, x=month_of_release)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip()


# splitting out individual charges (in progress)
release_test <- release_data %>%
  mutate(row_id = row_number())

release_charges <- release_test %>%
  select(starts_with("A")) %>%
  `colnames<-`(substring(names(.), 2)) %>% 
  remove_empty("rows") #%>%
  inner_join(release_test %>%
               select(starts_with("B")) %>%
               `colnames<-`(substring(names(.), 2))
               )  %>% 
  remove_empty("rows")
  
release_charges_1 <- release_test %>%
  select(starts_with("A")) %>%
  `colnames<-`(substring(names(.), 2)) %>% 
  remove_empty("rows")
  
release_charges_2 <- release_test %>%
  select(starts_with("B")) %>%
  `colnames<-`(substring(names(.), 2)) %>% 
  remove_empty("rows")


