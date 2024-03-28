# library(rlang)
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(digest)

# setwd(paste0(getwd(), "/temp"))

`%nin%` = Negate(`%in%`)

matchColClasses <- function(df1, df2) {
  
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  
  return(df2)
}

try_to_read_csv <- function(filename) {
  file_contents <- try(read.csv(filename, stringsAsFactors = FALSE, skipNul = TRUE))
  if("try-error" %in% class(file_contents) | 
     isTRUE(colnames(file_contents)[1] %nin% c("LASTNAME", "Obs"))) {
    file_contents <- try(read.csv(filename, skip = 1, stringsAsFactors = FALSE, skipNul = TRUE))
  }
  if("try-error" %in% class(file_contents) | 
     isTRUE(colnames(file_contents)[1] %nin% c("LASTNAME", "Obs"))) { 
    file_contents <- try(read.csv(filename, skip = 3, stringsAsFactors = FALSE, skipNul = TRUE))
  }
  if("try-error" %in% class(file_contents) | 
     isTRUE(colnames(file_contents)[1] %nin% c("LASTNAME", "Obs"))) { 
    file_contents <- try(read.csv(filename, skip = 4, stringsAsFactors = FALSE, skipNul = TRUE))
  }
  file_contents
}

directory <- 
  # "2022 Data Files"
  # "Calendar Year Data Files 2015 - 2020 and partial 2021"
  "OneDrive_2_12-1-2021"
  # "All IDOC Files"

new_directory <- "New IDOC"

all_files <- union(
  list.files(
    paste0(getwd(), "/", directory),
    full.names = TRUE,
    recursive = TRUE),
  # recursive = FALSE),
  list.files(
    paste0(getwd(), "/", new_directory),
    full.names = TRUE,
    recursive = TRUE))

release_files <- all_files[grepl("rel", tolower(all_files))]
admit_files <- all_files[grepl("adm", tolower(all_files))]


all_file_names <- union(
  list.files(
    paste0(getwd(), "/", directory),
    recursive = TRUE),
  list.files(
    paste0(getwd(), "/", new_directory),
    recursive = TRUE))

release_file_names <- all_file_names[grepl("rel", tolower(all_files))]
admit_file_names <- all_file_names[grepl("adm", tolower(all_files))]
file_length <- matrix(ncol = 2)

# test_release_files <- release_files[1:5]
# test_release_files <- release_files[length(release_files) - 10:length(release_files)]

#####################################
############ RELEASES ############### 
#####################################

for (i in 1:length(release_files)) {
  file <- release_files[i]
  
  print(paste0(file, "  (i = ", i, ")"))
  
  if (grepl("csv", tolower(file))) {
    # uncomment when possible
    # releases <- read_csv(file)
    
    # for old files
    releases <- try_to_read_csv(file)
    
  } else {
    releases <- readxl::read_excel(file)
  }
  
  if(str_detect(file, new_directory)) {
    releases <- releases %>%
      rename(
        LASTNAME = LAST_NAME,
        FIRSTNAM = FIRST_NAME,
        SEX = GENDER,
        COUNTY = COUNTY_OF_COMMIT,
        FACRLD = DISCHARGE_DATE,
        DOCNUM = DOC_NUMBER
      ) %>%
      filter(COUNTY == "Marion")
  } else {
    releases <- releases %>%
      filter(COUNTY == 49 &
               INTKSTCD %nin% c(17, 18) &
               RELFROM %nin% c("XAD", "XZZ") &
               RELTYPCD %nin% c("D", "M", "E")) %>%
      mutate(ETHNICITY = case_when(
        RACE == "H" ~ "Hispanic"
      ))
  }
  
  if (exists("release_columns")) {
    release_columns <- intersect(release_columns, colnames(releases))
  } else {
    release_columns <- colnames(releases)
  }
  
  selected_releases <- releases %>%
    select(all_of(release_columns)) %>%
    mutate(FileName = release_file_names[i],
           across(everything(), as.character),
           FACRLD = parse_date_time(FACRLD, orders = c('mdy', 'ymd', 'ymd H:M:S')))
  
  if (exists("combined_releases")) {
    
    combined_releases <- combined_releases %>%
      select(c(all_of(release_columns), FileName)) %>%
      union(selected_releases)
    
  } else {
    combined_releases <- selected_releases
  }
  
  file_length <- rbind(file_length, c(
    release_file_names[i], nrow(combined_releases)))
  
  print(paste(ncol(combined_releases), length(release_columns)))
}


# write.csv(file_length, file = "file_length.csv")
# write.csv(combined_releases, file = "combined_releases 1.31.22.csv")

#### Tableau Export ####
deduped_releases <- combined_releases %>%
  # filter(COUNTY == 49 &
  #          INTKSTCD %nin% c(17, 18) &
  #          RELFROM %nin% c("XAD", "XZZ") &
  #          RELTYPCD %nin% c("D", "M", "E")) %>%
  select(-FileName) %>%
  mutate(UniqueID = paste0(DOCNUM, "-", FACRLD),
         DOB = parse_date_time(DOB, orders = c('ymd', 'mdy')),
         FACRLD = as.Date(parse_date_time(FACRLD, orders = c("ymd", "ymdHMS"), truncated = 1))) %>%
  group_by(UniqueID) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(UniqueID = row_number())
# 
# release_columns <- as.data.frame(colnames(deduped_releases))%>%
#   `colnames<-`(c("Columns"))
# 
# for (letter in c("A", "B", "C", "D", "E", "F")) {
#   if (exists("offense_columns")) {
#     offense_columns <- inner_join(offense_columns, 
#                                   release_columns %>%
#                                     filter(grepl(paste0("^", letter), Columns)) %>%
#                                     mutate(Columns = sub('.', '', Columns))
#                                   , by = "Columns")
#   } else {
#     offense_columns <- release_columns %>%
#       filter(grepl(paste0("^", letter), Columns)) %>%
#       mutate(Columns = sub('.', '', Columns))
#   }
# }
# 
# select_offense_columns <- c()
# 
# for (letter in c("A", "B", "C", "D", "E", "F")) {
#   
#   hold <- deduped_releases %>%
#     select(c(UniqueID, paste0(letter, as.vector(unlist(offense_columns))))) %>%
#     `colnames<-`(c("UniqueID", as.vector(unlist(offense_columns)))) %>%
#     filter(!is.na(COUNTY))
#   
#   if (exists("release_offenses")) {
#     release_offenses <- dplyr::union(release_offenses, hold)
#   } else {
#     release_offenses <- hold
#   }
#   
#   deduped_releases <- deduped_releases %>%
#     select(-c(paste0(letter, as.vector(unlist(offense_columns)))))
# }

# write.csv(deduped_releases, file = "tableau_releases_4.4.23.csv", row.names = FALSE)
# write.csv(release_offenses, file = "tableau_release_offenses_4.4.23.csv", row.names = FALSE)

#####################################
########### ADMISSIONS ############## 
#####################################

file_length <- matrix(ncol = 2)

for (i in 1:length(admit_files)) {
  
  file <- admit_files[i]
  
  print(paste0(file, "  (i = ", i, ")"))
  
  if (grepl("csv", tolower(file))) {
    # uncomment when possible
    # admits <- read_csv(file)
    
    # for old files
    admits <- try_to_read_csv(file)
    
  } else {
    admits <- readxl::read_excel(file)
  }
  
  if(str_detect(file, new_directory)) {
    admits <- admits %>%
      rename(
        LASTNAME = LAST_NAME,
        FIRSTNAM = FIRST_NAME,
        SEX = GENDER,
        COUNTY = COUNTY_OF_COMMIT,
        RECVDT = DELTA_RECEIVE_DT,
        DOCNUM = DOC_NUMBER
      ) %>%
      mutate(DOB = NA) %>%
      filter(COUNTY == "Marion")
  } else {
    admits <- admits %>%
      filter(RECFAC %nin% c("PD1", "PD2", "PD3", "P4A", "P4B", "PD5",
                            "PD6", "PD7", "PD8", "PD9", "PD0", "XAD") &
               RECVCD %nin% c("15", "16", "17", "18", "10", "11",
                              "43", "79", "99")) %>%
      mutate(ETHNICITY = case_when(
        RACE == "H" ~ "Hispanic"
      ))
  }
  
  if (exists("admit_columns")) {
    admit_columns <- intersect(admit_columns, colnames(admits))
  } else {
    admit_columns <- colnames(admits)
  }
  
  "FACRLD" %in% admit_columns
  
  selected_admits <- admits %>%
    select(all_of(admit_columns)) %>%
    mutate(FileName = admit_file_names[i],
           across(everything(), as.character),
           RECVDT = parse_date_time(RECVDT, orders = c('ymd', 'mdy')))
  
  if (exists("combined_admits")) {
    
    combined_admits <- combined_admits %>%
      select(c(all_of(admit_columns), FileName)) %>%
      union(selected_admits)
    
  } else {
    combined_admits <- selected_admits
  }
  
  file_length <- rbind(file_length, c(
    admit_file_names[i], nrow(combined_admits)))
  
  print(paste(ncol(combined_admits), length(admit_columns)))
}

# write.csv(file_length, file = "file_length.csv")
# write.csv(combined_admits, file = "combined_admits 1.31.22.csv")

#### Tableau Export ####
deduped_admits <- combined_admits %>%
  # filter(RECFAC %nin% c("PD1", "PD2", "PD3", "P4A", "P4B", "PD5",
  #                       "PD6", "PD7", "PD8", "PD9", "PD0", "XAD") &
  #          RECVCD %nin% c("15", "16", "17", "18", "10", "11",
  #                         "43", "79", "99")) %>%
  select(-FileName) %>%
  mutate(UniqueID = paste0(DOCNUM, "-", RECVDT),
         # DOB = parse_date_time(DOB, orders = c('ymd', 'mdy'))) %>%
         DOB = case_when(
           !is.na(DOB) ~ parse_date_time(DOB, orders = c('ymd', 'mdy')))) %>%
  group_by(UniqueID) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(UniqueID = row_number())

# admit_columns <- as.data.frame(colnames(deduped_admits))%>%
#   `colnames<-`(c("Columns"))
# 
# for (letter in c("A", "B", "C", "D", "E", "F")) {
#   if (exists("a_offense_columns")) {
#     a_offense_columns <- inner_join(a_offense_columns, 
#                                     admit_columns %>%
#                                       filter(grepl(paste0("^", letter), Columns)) %>%
#                                       mutate(Columns = sub('.', '', Columns))
#                                     , by = "Columns")
#   } else {
#     a_offense_columns <- admit_columns %>%
#       filter(grepl(paste0("^", letter), Columns)) %>%
#       mutate(Columns = sub('.', '', Columns))
#   }
# }
# 
# select_offense_columns <- c()
# 
# for (letter in c("A", "B", "C", "D", "E", "F")) {
#   
#   hold <- deduped_admits %>%
#     select(c(UniqueID, paste0(letter, as.vector(unlist(a_offense_columns))))) %>%
#     `colnames<-`(c("UniqueID", as.vector(unlist(a_offense_columns)))) %>%
#     filter(!is.na(COUNTY)) %>%
#     mutate_if(is.logical, as.character)
#   
#   if (exists("admit_offenses")) {
#     
#     # for (n in names(admit_offenses)) {
#     #   class(hold[, n]) <- sapply(admit_offenses, class)[n]
#     # }
#     
#     admit_offenses <- dplyr::union(admit_offenses, hold)
#   } else {
#     admit_offenses <- hold
#   }
#   
#   deduped_admits <- deduped_admits %>%
#     select(-c(paste0(letter, as.vector(unlist(a_offense_columns)))))
# }

# write.csv(deduped_admits, file = "tableau_admits_1.31.22.csv", row.names = FALSE)
# write.csv(admit_offenses, file = "tableau_admit_offenses_1.31.22.csv", row.names = FALSE)

#################################################
##  update arrests data
#################################################

##  uncomment the below if not using the arrest_combination.R file for some reason
# ##  pick arrests file to read in
# # arrest_file <- file.choose(); arrest_data <- read_excel(arrest_file)
# arrest_file <- file.choose()
# arrest_data <- read_csv(arrest_file,
#                         show_col_types = FALSE)
# 
# arrest_columns <- as.data.frame(colnames(arrest_data))
# 
# extra_file <- file.choose()
# ##  remove extra rows 
# extra_data <- read_excel(extra_file)
# extra_data <- read_excel(extra_file,  
#                          skip = which(extra_data == "Gallery", arr.ind=TRUE)[1]) %>%
#   separate("Inmate Name", c('LastName', 'FirstName'), sep = ", ") %>%
#   mutate(LN3 = substr(LastName, 0, 3),
#          FN3 = substr(FirstName, 0, 3)) %>%
#   rename("GALLERY" = "Gallery"
#          , "BookingDate" = "Book Date"
#          , "Booking#" = "Book NO."
#          # , "Booking#" = "Book No"
#          # , "XXX" = "Inmate Name"
#          , "SEX" = "Gender"
#          , "HISPANICLATINO" = "Hispanic/Latino"
#          , "RACE" = "Race"
#          , "AdmissionType" = "Admission Type"
#          , "ADMITDOCUMENTATIONTYPE" = "Admit Documention Type"
#          , "JAILLOCATION" = "Jail"
#          # , "JAILLOCATION" = "Jail Location"
#          # , "XXX" = "Degree"
#          , "GradeClass" = "Grade"
#          , "OffenseDescription" = "Offense Description"
#          , "SenYN" = "Sen Y/N"
#          , "CaseSenStatus" = "Case Sen Status"
#          , "DetainerHOLDCharge" = "Detainer Hold Charge"
#          , "DetainerIssuingAgency" = "Detainer Agency"
#          , "ReleaseType" = "Release Type"
#          # , "XXX" = "Temp Rel. Date"
#          , "ReleaseDate" = "Rel. Date"
#   ) %>%
#   select(LN3, FN3, DOB, BookingDate, AdmissionType)
# 
# arrest_data <- arrest_data %>%
#   full_join(extra_data, by = c("LN3", "FN3", "DOB", "BookingDate", "AdmissionType"))
# 

# add key columns and remove duplicates
# arrest_data_clean <- arrest_data %>%


arrest_data_clean <- big_arrest_table %>%
  mutate(person_id = paste0(toupper(substr(LastName, 0, 3)),
                            toupper(substr(FirstName, 0, 3)),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         arrest_row_id = row_number()) %>%
  # distinct(person_id, BookingDate, .keep_all = TRUE)
  distinct(person_id, `Book Date`, .keep_all = TRUE)

small_arrests <- arrest_data_clean %>%
  filter(`Admission Type` %in% c("OUTRIGHT ONLY", "OUTRIGHT WITH HOLD",
                              "OUTRIGHT WITH WARRANTS")) %>%
  # select("person_id", "BookingDate", "AdmissionType", "ReleaseDate",
  select("person_id", `Book Date`, `Admission Type`, `Release Date`,
         "Offense", `Release Type`, "GALLERY", "Hispanic / Latino", RACE)

# write.csv(small_arrests, file = "small_arrests_5.16.22.csv", row.names = FALSE)

#############################################################################
#############################################################################
#############################################################################


# get_tableau_releases <- file.choose(); get_tableau_releases <- read.csv(get_tableau_releases)
get_tableau_releases <- deduped_releases
# add key columns and remove duplicates
# small_releases <- get_tableau_releases %>%
#   mutate(person_id = paste0(substr(LASTNAME, 0, 3),
#                             substr(FIRSTNAM, 0, 3),
#                             str_replace_all(DOB, "[^[:alnum:]]", "")),
#          release_row_id = row_number()) %>%
#   distinct(person_id, FACRLD, .keep_all = TRUE) %>%
#   filter(COUNTY == 49) %>%
#   # select(person_id, RELFROM, COUNTY, FACRLD)
#   select(person_id, RELFROM, COUNTY, FACRLD, INTKSTCD, RELTYPCD)

# write.csv(small_releases, file = "small_releases_4.4.23.csv", row.names = FALSE)

# get_tableau_admits <- file.choose(); get_tableau_admits <- read.csv(get_tableau_admits)
get_tableau_admits <- deduped_admits
# add key columns and remove duplicates
# small_admits <- get_tableau_admits %>%
#   mutate(person_id = paste0(substr(LASTNAME, 0, 3),
#                             substr(FIRSTNAM, 0, 3),
#                             str_replace_all(DOB, "[^[:alnum:]]", "")),
#          admit_row_id = row_number()) %>%
#   # filter(INTKSTCD %nin% c(10, 11, 15, 16, 17, 18, 43)) %>%
#   # distinct(person_id, INTKDT, .keep_all = TRUE)
#   distinct(person_id, INTKDT, RECVDT, .keep_all = TRUE) %>%
#   select(person_id, COUNTY, INTKDT, INTKSTCD, DOCNUM, RECVDT, RECFAC, RECVCD)

# write.csv(small_admits, file = "small_admits_4.4.23.csv", row.names = FALSE)


demographics_columns <- c("person_id", "last_name", "first_name",
                          "dob", "sex", "race", "information_date")

small_demographics <- deduped_admits %>%
  mutate(person_id = paste0(toupper(substr(LASTNAME, 0, 3)),
                            toupper(substr(FIRSTNAM, 0, 3)),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
  #        INTKDT = as.Date(INTKDT, "%m/%d/%Y")) %>%
  # select(person_id, LASTNAME, FIRSTNAM, DOB, SEX, RACE, INTKDT) %>%
  RECVDT = as.Date(RECVDT, "%m/%d/%Y")) %>%
  select(person_id, LASTNAME, FIRSTNAM, DOB, SEX, RACE, RECVDT) %>%
  `colnames<-`(demographics_columns) %>%
  mutate(origin = "admissions") %>%
  union_all(arrest_demographics %>%
              # arrest_data %>%
              # filter(AdmissionType %in% c("OUTRIGHT ONLY", "OUTRIGHT WITH HOLD",
              #                             "OUTRIGHT WITH WARRANTS")) %>%
              # mutate(DOB = 
              #          # as.Date(DOB, "%m/%d/%Y"),
              #          ymd(str_sub(person_id, 7)),
              #        BookingDate = as.Date(`Book Date`, "%m/%d/%Y")) %>%
              # select(person_id, LastName, FirstName, DOB, SEX, RACE, BookingDate) %>%
              `colnames<-`(demographics_columns) %>%
              mutate(origin = "arrests")) %>%
  union_all(deduped_releases %>%
              mutate(person_id = paste0(toupper(substr(LASTNAME, 0, 3)),
                                        toupper(substr(FIRSTNAM, 0, 3)),
                                        str_replace_all(DOB, "[^[:alnum:]]", ""))) %>%
              # filter(COUNTY == 49) %>%
              select(person_id, LASTNAME, FIRSTNAM, DOB, SEX, RACE, FACRLD) %>%
              `colnames<-`(demographics_columns) %>%
              mutate(origin = "releases")) %>%
  arrange(race, desc(information_date)) %>%
  distinct(person_id, #last_name, first_name, dob, sex, race, 
           .keep_all = TRUE) %>%
  select(-information_date)

# write.csv(small_demographics, file = "small_demographics_4.4.23.csv", row.names = FALSE)

#############################################################################
#############################################################################
#############################################################################

# table_for_recidivism <- deduped_releases %>%
#   mutate(person_id = paste0(substr(LASTNAME, 0, 3),
#                             substr(FIRSTNAM, 0, 3),
#                             str_replace_all(DOB, "[^[:alnum:]]", ""))) %>%
#   select(DOCNUM, person_id, FACRLD, UniqueID) %>%
#   left_join(deduped_admits %>%
#               select(DOCNUM, RECVDT),
#             by = "DOCNUM",
#             relationship = "many-to-many") %>%
#   left_join(small_arrests %>%
#               select(person_id, `Book Date`),
#             by = "person_id",
#             relationship = "many-to-many")

table_for_recidivism <- deduped_releases %>%
  mutate(person_id = paste0(toupper(substr(LASTNAME, 0, 3)),
                            toupper(substr(FIRSTNAM, 0, 3)),
                            str_replace_all(DOB, "[^[:alnum:]]", ""))) %>%
  select(DOCNUM, person_id, FACRLD, UniqueID) %>%
  left_join(deduped_admits %>%
              select(DOCNUM, RECVDT),
            join_by(DOCNUM, closest(FACRLD < RECVDT))) %>%
  left_join(small_arrests %>%
              select(person_id, `Book Date`),
            join_by(person_id, closest(FACRLD < `Book Date`)))

recidivism <- table_for_recidivism %>%
  mutate(IDOC_return_flag_12 = case_when(
    FACRLD < RECVDT
    & (FACRLD + days(366)) >= RECVDT ~ TRUE,
    TRUE ~ FALSE),
    IDOC_return_flag_6 = case_when(
      FACRLD < RECVDT
      & (FACRLD + days(183)) >= RECVDT ~ TRUE,
      TRUE ~ FALSE),
    arrest_return_flag_12 = case_when(
      FACRLD < `Book Date`
      & (FACRLD + days(366)) >= `Book Date` ~ TRUE,
      TRUE ~ FALSE),
    arrest_return_flag_6 = case_when(
      FACRLD < `Book Date`
      & (FACRLD + days(183)) >= `Book Date` ~ TRUE,
      TRUE ~ FALSE)) %>%
  select(UniqueID, IDOC_return_flag_12, IDOC_return_flag_6, 
         arrest_return_flag_12, arrest_return_flag_6) %>%
  group_by(UniqueID) %>%
  summarise(across(where(is.logical), ~ max(.x, na.rm = TRUE))) %>%
  ungroup()

dashboard_table <- deduped_releases %>%
  left_join(recidivism,
            by = "UniqueID") %>%
  mutate(cohort_of_release = paste(year(FACRLD), if_else(month(FACRLD) <= 6, "A", "B"))) %>%
  select(-c(LASTNAME, FIRSTNAM
            # , SSN1, SSN2, SSN3, SSN4
            ))
  
summary_table <- dashboard_table %>%
  select(cohort_of_release, DOCNUM, IDOC_return_flag_12, IDOC_return_flag_6,
         arrest_return_flag_12, arrest_return_flag_6) %>%
  mutate(DOCNUM = as.character(DOCNUM)) %>%
  group_by(cohort_of_release, DOCNUM) %>%
  summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE))) %>%
  group_by(cohort_of_release) %>%
  summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE) * 100, 1))) 
  
# write_csv(dashboard_table, "IDOC_dashboard_data_2.21.24.csv")


test <- combined_releases  %>%
  mutate(month = month(FACRLD),
         year = year(FACRLD)) %>%
  group_by(month, year) %>%
  summarise(rows = n()) %>%
  arrange(year, month)



##  for Connecting to Community participant monitoring

ctc_idoc_data <- small_admits %>%
  # select(person_id, DOCNUM, RECVDT) %>%
  filter(RECVDT >= mdy("4/1/18")) %>%
  left_join(get_tableau_releases %>%
              mutate(RECVDT = parse_date_time(RECVDT, orders = c('ymd', 'mdy'))) %>%
              distinct(DOCNUM, RECVDT, .keep_all = TRUE) %>%
              select(DOCNUM, RECVDT, FACRLD),
            by = c("DOCNUM", "RECVDT")) %>%
  rowwise() %>%
  mutate(person_id = digest(person_id)) %>%
  ungroup()

ctc_previous_idoc_data <- ctc_idoc_data %>%
  group_by(person_id) %>%
  arrange(desc(RECVDT)) %>%
  mutate(admit_count = row_number()) %>%
  ungroup() %>%
  filter(admit_count == 1 |
           RECVDT >= mdy("4/1/23"))

ctc_arrest_data <- arrest_data_clean %>%
  filter(`Book Date` >= mdy("4/1/18")) %>%
  select(person_id, GALLERY, `Book Date`, `Admission Type`, Offense,
         `Hispanic / Latino`, RACE, GENDER, `Release Date`) %>%
  rowwise() %>%
  mutate(person_id = digest(person_id)) %>%
  ungroup()

ctc_previous_arrest_data <- ctc_arrest_data %>%
  group_by(person_id) %>%
  arrange(desc(`Book Date`)) %>%
  mutate(arrest_count = row_number()) %>%
  ungroup() %>%
  filter(arrest_count == 1 |
           `Book Date` >= mdy("4/1/23"))

# ctc_arrest_data_smushed <- ctc_arrest_data %>%
#   group_by(person_id, GALLERY) %>%
#   summarise(BookDates = toString(`Book Date`),
#             Offenses = toString(Offense))

write.csv(ctc_previous_idoc_data, 
          file = "ctc_previous_idoc_data.csv", row.names = FALSE)
write.csv(ctc_previous_arrest_data, 
          file = "ctc_previous_arrest_data.csv", row.names = FALSE)

write.csv(ctc_idoc_data, 
          file = "ctc_idoc_data.csv", row.names = FALSE)
write.csv(ctc_arrest_data, 
          file = "ctc_arrest_data.csv", row.names = FALSE)


################################################################################
######################## parsing new file type #################################
################################################################################

new_admits <- file.choose(); new_admits <- read_xlsx(new_admits)
new_releases <- file.choose(); new_releases <- read_xlsx(new_releases)

hold <- new_releases %>%
  group_by(DOC_NUMBER, DISCHARGE_DATE) %>%
  summarise(across(everything(), ~ n_distinct(.)))

hold_2 <- hold %>%
  mutate(total = rowSums(across(all_of(colnames(hold)[3:length(colnames(hold))]))))

for (column in colnames(hold)[3:length(colnames(hold))]) {
  if (max(hold[, column]) > 1) {
    print(column)
  }
}

new_releases_compare <- new_releases %>%
  filter(
    DISCHARGE_DATE >= ymd("2023-1-1") &
      DISCHARGE_DATE <= ymd("2023-4-30") &
      SENTENCE_RANK == 1 &
      COUNTY == "Marion" &
      # INTKSTCD %nin% c(17, 18) &
      DISCHARGE_FACILITY %nin% c("XAD", "XZZ") 
      # RELTYPCD %nin% c("D", "M", "E")
    
    )

# INTKDT = ADMISSION_DATE
# COUNTY = COUNTY (spelled out now)
# RELFROM = DISCHARGE_FACILITY
# RELTYPCD = ? maybe RELEASE_CATEGORY spelled out?
# INTKSTCD = ?
# DOCNUM = DOC_NUMBER
# no change to DOB
# RACE = RACE (spelled out now)
# ETHNICITY is new
# FACRLD = DISCHARGE_DATE

CNTYREL = COUNTYREL


hold <- deduped_releases %>%
  select(DOCNUM, INTKDT, FACRLD, RELTYPCD, INTKSTCD) %>%
  mutate(DOCNUM = as.double(DOCNUM),
         INTKDT = ymd(INTKDT),
         FACRLD = ymd(FACRLD)) %>%
  full_join(new_releases_compare %>%
              mutate(ADMISSION_DATE = ymd(ADMISSION_DATE),
                     DISCHARGE_DATE = as.Date(DISCHARGE_DATE)),
            by = c("DOCNUM" = "DOC_NUMBER",
                   # "INTKDT" = "ADMISSION_DATE",
                   "FACRLD" = "DISCHARGE_DATE"))

matches <- hold %>%
  filter(!is.na(INTKDT) &
           !is.na(ADMISSION_DATE))

mismatches <- hold %>%
  filter(is.na(INTKDT) |
           is.na(ADMISSION_DATE))

# variable identification
matches %>% 
  select(RELTYPCD, MOVEMENT_CODE) %>%
  distinct()

compare_to <- "RELTYPCD"

for (old_column in colnames(matches)) {
  
  number_of_values <- n_distinct(matches[, old_column])
  
  start <- match(old_column, colnames(matches)) + 1
  end <- length(colnames(matches))
  
  if (number_of_values %in% c(2:20) & 
      start < end &
      !grepl("DATE", old_column)) {
    for (new_column in colnames(matches)[start:end]) {
      number_of_combined_values <- nrow(unique(matches[, c(old_column, new_column)]))
      
      if (number_of_values == number_of_combined_values &
          number_of_combined_values == n_distinct(matches[, new_column]) &
          !grepl("DATE", new_column)) {
        print(paste(old_column, "may align with", new_column))
      }
    }
  }
}



# deduped_releases <- combined_releases %>%
#   filter(COUNTY == 49 &
#            INTKSTCD %nin% c(17, 18) &
#            RELFROM %nin% c("XAD", "XZZ") &
#            RELTYPCD %nin% c("D", "M", "E")) %>%
#   select(-FileName) %>%
#   mutate(UniqueID = paste0(DOCNUM, "-", FACRLD),
#          DOB = parse_date_time(DOB, orders = c('ymd', 'mdy'))) %>%
#   group_by(UniqueID) %>%
#   slice(1L) %>%
#   ungroup() %>%
#   mutate(UniqueID = row_number())
