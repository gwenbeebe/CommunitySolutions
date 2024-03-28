# library(rlang)
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(digest)

`%nin%` = Negate(`%in%`)

matchColClasses <- function(df1, df2) {
  
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  
  return(df2)
}


all_files <- list.files(
  paste0(getwd(), "/Arrest Files"),
  full.names = TRUE,
  # recursive = TRUE)
  recursive = FALSE)

arrest_release_files <- all_files[grepl("rel", tolower(all_files))]
arrest_booking_files <- all_files[grepl("boo", tolower(all_files))]


all_file_names <- list.files(
  paste0(getwd(), "/Arrest Files"),
  # recursive = TRUE)
  recursive = FALSE)

arrest_release_file_names <- all_file_names[grepl("rel", tolower(all_files))]
arrest_booking_file_names <- all_file_names[grepl("boo", tolower(all_files))]
file_length <- matrix(ncol = 2)

# test_arrest_release_files <- arrest_release_files[1:5]
# test_arrest_release_files <- arrest_release_files[length(arrest_release_files) - 10:length(arrest_release_files)]

#####################################
############ arrest_releaseS ############### 
#####################################

# for checking whether all columns are shared
# for (i in 1:length(arrest_booking_files)) {
#   if (exists("R_C_N")) {
#     R_C_N <- R_C_N %>%
#       rbind(colnames(read_csv(arrest_release_files[i])))
#   } else {
#     R_C_N <- colnames(read_csv(arrest_release_files[i]))
#   }
# }

for (i in 1:length(arrest_release_files)) {
    
  file <- arrest_release_files[i]
  
  print(paste0(file, "  (i = ", i, ")"))
  
  if (grepl("csv", tolower(file))) {
    arrest_releases <- read_csv(file,
                         show_col_types = FALSE)
  } else {
    arrest_releases <- readxl::read_excel(file)
  }
  
  arrest_releases <- arrest_releases[, 1:23]
  
  if(!exists("arrest_release_col_names")){
    arrest_release_col_names <- colnames(arrest_releases)
  } else {
    colnames(arrest_releases) <- arrest_release_col_names
  }
  
  if (length(class(arrest_releases$`Book Date`)) == 2) {
    
  }
  
  selected_arrest_releases <- arrest_releases %>%
    mutate(FileName = arrest_release_file_names[i],
           across(contains("Date") &
                    !where(is.POSIXct), ~  as.Date(parse_date_time2(., orders = c('mdyHM', 'mdYHM', 'ymdHMS')))),
           across(where(is.POSIXct), ~ as.Date(.)),
           across(everything(), as.character),
           GALLERY = str_remove(gsub("[^0-9.-]", "", GALLERY), "^0+"))
  
  if (exists("combined_arrest_releases")) {
    combined_arrest_releases <- combined_arrest_releases %>%
      union(selected_arrest_releases)
  } else {
    combined_arrest_releases <- selected_arrest_releases
  }
  
  file_length <- rbind(file_length, c(
    arrest_release_file_names[i], nrow(combined_arrest_releases)))
  
  print(paste(ncol(combined_arrest_releases)))
}


#####################################
########### ADMISSIONS ############## 
#####################################

for (i in 1:length(arrest_booking_files)) {
  
  file <- arrest_booking_files[i]
  
  print(paste0(file, "  (i = ", i, ")"))
  
  if (grepl("csv", tolower(file))) {
    arrest_bookings <- read_csv(file,
                         show_col_types = FALSE)
  } else {
    arrest_bookings <- readxl::read_excel(file)
  }
  
  arrest_bookings <- arrest_bookings[, 1:23]
  
  if(!exists("arrest_booking_col_names")){
    arrest_booking_col_names <- colnames(arrest_bookings)
  } else {
    colnames(arrest_bookings) <- arrest_booking_col_names
  }
  
  selected_arrest_bookings <- arrest_bookings %>%
    mutate(FileName = arrest_booking_file_names[i],
           across(contains("Date") &
                    !where(is.POSIXct), ~  as.Date(parse_date_time2(., orders = c('mdyHM', 'mdYHM', 'ymdHMS')))),
           across(where(is.POSIXct), ~ as.Date(.)),
           across(everything(), as.character),
           GALLERY = str_remove(gsub("[^0-9.-]", "", GALLERY), "^0+"))
  
  if (exists("combined_arrest_bookings")) {
    combined_arrest_bookings <- combined_arrest_bookings %>%
      union(selected_arrest_bookings)
  } else {
    combined_arrest_bookings <- selected_arrest_bookings
  }
  
  file_length <- rbind(file_length, c(
    arrest_booking_file_names[i], nrow(combined_arrest_bookings)))
  
  print(paste(ncol(combined_arrest_bookings)))
}

demographics <- combined_arrest_releases %>%
  union(combined_arrest_bookings) %>%
  group_by(GALLERY) %>%
  slice(1L) %>%
  select(GALLERY, DOB, GENDER, `Hispanic / Latino`, RACE)

deduped_arrest_releases <- combined_arrest_releases %>%
  select(-FileName) %>%
  mutate(UniqueID = paste0(GALLERY, "-", `Release Date`),
         DOB = parse_date_time(DOB, orders = c('mdy', 'ymd'))) %>%
  group_by(UniqueID) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-c(UniqueID, `Inmate Last, First, Middle`,
         DOB, GENDER, `Hispanic / Latino`, RACE))

deduped_arrest_bookings <- combined_arrest_bookings %>%
  select(-FileName) %>%
  mutate(UniqueID = paste0(GALLERY, "-", `Book Date`),
         DOB = parse_date_time(DOB, orders = c('mdy', 'ymd'))) %>%
  group_by(UniqueID) %>%
  # slice(1L) %>%
  ungroup() %>%
  select(-c(UniqueID, `Inmate Last, First, Middle`,
         DOB, GENDER, `Hispanic / Latino`, RACE))

#write.csv(demographics, file = "jail_demographics_1.17.22.csv", row.names = FALSE)
#write.csv(deduped_arrest_releases, file = "jail_arrest_releases_1.17.23.csv", row.names = FALSE)
#write.csv(deduped_arrest_bookings, file = "jail_arrest_bookings_1.17.23.csv", row.names = FALSE)


big_arrest_table <- combined_arrest_releases %>%
  union(combined_arrest_bookings) %>%
  select(-FileName) %>%
  separate(`Inmate Last, First, Middle`, 
           into = c("LastName", "FirstName"), sep = ", ") %>%
  mutate(DOB = parse_date_time(DOB, orders = c('mdy', 'ymd')),
         person_id = paste0(substr(LastName, 0, 3),
                            substr(FirstName, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         `Book Date` = ymd(`Book Date`)) 

arrest_demographics <- big_arrest_table %>%
  group_by(person_id) %>%
  slice(1L) %>%
  ungroup() %>%
  select(person_id, LastName, FirstName, DOB, GENDER, RACE, `Book Date`)

all_arrests <- big_arrest_table %>%
  mutate(UniqueID = paste0(GALLERY, "-", `Book Date`)) %>%
  group_by(UniqueID) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-c(UniqueID, DOB, GENDER, `Hispanic / Latino`, RACE))



# write.csv(all_arrests, file = "all_arrest_data_4.4.23.csv", row.names = FALSE)


lookback_stop_date <- max(all_arrests$`Book Date`)

##  for daily bookings file
mini_arrest_data <- all_arrests %>%
  # filter(`Book Date` >= mdy("1/1/22") &
  #          `Book Date` <= mdy("12/31/22")) %>%
  filter(`Book Date` >= lookback_stop_date %m-% years(1)) %>%
  group_by(GALLERY) %>%
  summarise(num_arrests = n_distinct(`Book Date`, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(num_arrests >= 3)

# write.csv(mini_arrest_data, file = "mini_arrest_data.2.6.24.csv", row.names = FALSE)


  
