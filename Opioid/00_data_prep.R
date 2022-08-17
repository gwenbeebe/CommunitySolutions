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
# library(stringr)
# library(janitor)

source("00_functions.R")
#################################################

all_months <- list.files(
  paste0(getwd(), "/data/SU - Client Tracking Reports"),
  full.names = TRUE)

file_names <- list.files(
  paste0(getwd(), "/data/SU - Client Tracking Reports"))

##  create list to store table names
table_list <- c()


for (file_num in 1:length(all_months)) {
  cols_to_keep <- c("encounter_date", "length_of_service",
                    "last_name", "first_name", "date_of_birth",
                    "location_of_service", "role_of_staff_member",
                    "type_of_service")
  
  file_hold <- read_xlsx(all_months[file_num], sheet = "Encounter Data") %>%
    `colnames<-`(clean_names(colnames(.))) %>%
    {if("location" %in% names(.)) rename(., location_of_service = location) else .} %>%
    {if("role" %in% names(.)) rename(., role_of_staff_member = role) else .} %>%
    select(cols_to_keep)
  
  file_name <- str_replace_all(file_names[file_num], regex("\\W+"), "")
  file_name <- substring(file_name, 0, nchar(file_name) - 4)

  
  if (exists("combined_files")) {
    combined_files <- combined_files %>%
      full_join(file_hold, by = cols_to_keep)
  } else {
    combined_files <- file_hold
  }
  
}


combined_files_clean <- combined_files %>%
  distinct() %>%
  mutate(time_amount = str_replace_all(length_of_service, " 1/2", ".5"),
         time_amount = as.numeric(gsub("[^[:digit:].]", "", time_amount)),
         time_type = str_detect(length_of_service, "hour"),
         # time_type = if_else(time_type, "hour", "min"),
         service_minutes = if_else(time_type, time_amount * 60, time_amount),
         person_id = paste0(substr(toupper(last_name), 0, 3),
                            substr(toupper(first_name), 0, 3),
                            str_replace_all(date_of_birth, "[^[:alnum:]]", ""))) %>%
  select(-c(length_of_service, time_amount, time_type,
            last_name, first_name, date_of_birth))


write.csv(combined_files_clean, file = "services_provided 1.31.22 v2.csv", row.names = FALSE)
