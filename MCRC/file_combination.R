# library(rlang)
library(tidyverse)
library(lubridate)
# library(readxl)
library(stringr)

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
  paste0(getwd(), "/data/OneDrive_2_12-1-2021"),
  full.names = TRUE,
  recursive = TRUE)

release_files <- all_files[grepl("REL", all_files)]
admit_files <- all_files[grepl("ADM", all_files)]


all_file_names <- list.files(
  paste0(getwd(), "/data/OneDrive_2_12-1-2021"),
  recursive = TRUE)

release_file_names <- all_file_names[grepl("REL", all_file_names)]
admit_file_names <- all_file_names[grepl("ADM", all_file_names)]
file_length <- matrix(ncol = 2)

# test_release_files <- release_files[1:5]
# test_release_files <- release_files[length(release_files) - 10:length(release_files)]

#####################################
############ RELEASES ############### 
#####################################

for (i in 1:length(release_files)) {
  file <- release_files[i]
  
  print(file)
  
  file_top <- as.data.frame(
    read.csv(file,
             header = FALSE,
             skipNul = TRUE,
             nrows = 5))
  # file_top <- read.table(file, 
  #                        nrow = 5, stringsAsFactors = FALSE, sep = ",")
  # 
  releases <- read.csv(file, 
                         # skip = which.max(file_top[,1] %in% c("Obs", "LASTNAME")) - max(file_top[,1] == ""), 
                       skip = which.max(file_top[,1] %in% c("Obs", "LASTNAME")) - 1, 
                       skipNul = TRUE)
  hold <- file_top
  if (exists("release_columns")) {
    release_columns <- intersect(release_columns, colnames(releases))
  } else {
    release_columns <- colnames(releases)
  }
  
  selected_releases <- releases %>%
    select(all_of(release_columns)) %>%
    mutate(FileName = release_file_names[i])
  
  # assign(release_file_names[i], selected_releases)

  if (exists("combined_releases")) {
    combined_releases <- combined_releases %>%
      select(c(all_of(release_columns), FileName))
    
    attempt <- try(combined_releases %>%
               union(selected_releases),
               silent = TRUE)
    print("try-error" %in% class(attempt))
    if ("try-error" %in% class(attempt)) {
      selected_releases <- matchColClasses(combined_releases, selected_releases)
      combined_releases <- combined_releases %>%
        union(selected_releases)
    }
    else {
      combined_releases <- attempt
    }
    
  } else {
    combined_releases <- selected_releases
  }
  
  file_length <- rbind(file_length, c(
    release_file_names[i], nrow(combined_releases)))
  
  print(paste(ncol(combined_releases), length(release_columns)))
}

# write.csv(file_length, file = "file_length.csv")
# write.csv(combined_releases, file = "combined_releases.csv")

#### Tableau Export ####
deduped_releases <- combined_releases %>%
  select(-FileName) %>%
  mutate(UniqueID = paste0(DOCNUM, "-", FACRLD)) %>%
  # mutate(UniqueID = paste0(DOCNUM, "-", INTKDT)) %>%
  group_by(UniqueID) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(UniqueID = row_number())

release_columns <- as.data.frame(colnames(deduped_releases))%>%
  `colnames<-`(c("Columns"))

for (letter in c("A", "B", "C", "D", "E", "F")) {
  if (exists("offense_columns")) {
    offense_columns <- inner_join(offense_columns, 
                                  release_columns %>%
                                    filter(grepl(paste0("^", letter), Columns)) %>%
                                    mutate(Columns = sub('.', '', Columns))
                                  , by = "Columns")
  } else {
    offense_columns <- release_columns %>%
      filter(grepl(paste0("^", letter), Columns)) %>%
      mutate(Columns = sub('.', '', Columns))
  }
}

select_offense_columns <- c()

for (letter in c("A", "B", "C", "D", "E", "F")) {
  
  hold <- deduped_releases %>%
    select(c(UniqueID, paste0(letter, as.vector(unlist(offense_columns))))) %>%
    `colnames<-`(c("UniqueID", as.vector(unlist(offense_columns)))) %>%
    filter(!is.na(COUNTY))
  
  if (exists("release_offenses")) {
    release_offenses <- dplyr::union(release_offenses, hold)
  } else {
    release_offenses <- hold
  }
  
  deduped_releases <- deduped_releases %>%
    select(-c(paste0(letter, as.vector(unlist(offense_columns)))))
}

write.csv(deduped_releases, file = "tableau_releases_5.16.22.csv", row.names = FALSE)
write.csv(release_offenses, file = "tableau_release_offenses_5.16.22.csv", row.names = FALSE)

#####################################
########### ADMISSIONS ############## 
#####################################

for (i in 1:length(admit_files)) {
  file <- admit_files[i]
  
  print(file)
  
  file_top <- as.data.frame(
    read.csv(file,
             header = FALSE,
             skipNul = TRUE,
             nrows = 5))
  # file_top <- read.table(file, 
  #                        nrow = 5, stringsAsFactors = FALSE, sep = ",")
  # 
  admits <- read.csv(file, 
                       # skip = which.max(file_top[,1] %in% c("Obs", "LASTNAME")) - max(file_top[,1] == ""), 
                       skip = which.max(file_top[,1] %in% c("Obs", "LASTNAME")) - 1, 
                       skipNul = TRUE)
  hold <- file_top
  if (exists("admit_columns")) {
    admit_columns <- intersect(admit_columns, colnames(admits))
  } else {
    admit_columns <- colnames(admits)
  }
  
  selected_admits <- admits %>%
    select(all_of(admit_columns)) %>%
    mutate(FileName = admit_file_names[i])
  
  # assign(admit_file_names[i], selected_admits)
  
  if (exists("combined_admits")) {
    combined_admits <- combined_admits %>%
      select(c(all_of(admit_columns), FileName))
      # select(all_of(admit_columns))
    
    attempt <- try(combined_admits %>%
                     union(selected_admits),
                   silent = TRUE)
    print("try-error" %in% class(attempt))
    if ("try-error" %in% class(attempt)) {
      selected_admits <- matchColClasses(combined_admits, selected_admits)
      combined_admits <- combined_admits %>%
        union(selected_admits)
    }
    else {
      combined_admits <- attempt
    }
    
  } else {
    combined_admits <- selected_admits
  }
  
  file_length <- rbind(file_length, c(
    admit_file_names[i], nrow(combined_admits)))
}

# write.csv(file_length, file = "file_length.csv")
# write.csv(combined_admits, file = "combined_admits.csv")

#### Tableau Export ####
deduped_admits <- combined_admits %>%
  select(-FileName) %>%
  # mutate(UniqueID = paste0(DOCNUM, "-", INTKDT)) %>%
  # use below for intake/receive date swap
  mutate(UniqueID = paste0(DOCNUM, "-", INTKDT, "-", RECVDT)) %>%
  group_by(UniqueID) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(UniqueID = row_number())

admit_columns <- as.data.frame(colnames(deduped_admits))%>%
  `colnames<-`(c("Columns"))

for (letter in c("A", "B", "C", "D", "E", "F")) {
  if (exists("a_offense_columns")) {
    a_offense_columns <- inner_join(a_offense_columns, 
                                  admit_columns %>%
                                    filter(grepl(paste0("^", letter), Columns)) %>%
                                    mutate(Columns = sub('.', '', Columns))
                                  , by = "Columns")
  } else {
    a_offense_columns <- admit_columns %>%
      filter(grepl(paste0("^", letter), Columns)) %>%
      mutate(Columns = sub('.', '', Columns))
  }
}

select_offense_columns <- c()

for (letter in c("A", "B", "C", "D", "E", "F")) {
  
  hold <- deduped_admits %>%
    select(c(UniqueID, paste0(letter, as.vector(unlist(a_offense_columns))))) %>%
    `colnames<-`(c("UniqueID", as.vector(unlist(a_offense_columns)))) %>%
    filter(!is.na(COUNTY)) %>%
    mutate_if(is.logical, as.character)
  
  if (exists("admit_offenses")) {
    
    # for (n in names(admit_offenses)) {
    #   class(hold[, n]) <- sapply(admit_offenses, class)[n]
    # }
    
    admit_offenses <- dplyr::union(admit_offenses, hold)
  } else {
    admit_offenses <- hold
  }
  
  deduped_admits <- deduped_admits %>%
    select(-c(paste0(letter, as.vector(unlist(a_offense_columns)))))
}

write.csv(deduped_admits, file = "tableau_admits_5.16.22.csv", row.names = FALSE)
write.csv(admit_offenses, file = "tableau_admit_offenses_5.16.22.csv", row.names = FALSE)

#################################################
##  update arrests data
#################################################
##  pick arrests file to read in
arrest_file <- file.choose(); arrest_data <- read_excel(arrest_file)

arrest_columns <- as.data.frame(colnames(arrest_data))

extra_file <- file.choose()
##  remove extra rows 
extra_data <- read_excel(extra_file)
extra_data <- read_excel(extra_file,  
                         skip = which(extra_data == "Gallery", arr.ind=TRUE)[1]) %>%
  separate("Inmate Name", c('LastName', 'FirstName'), sep = ", ") %>%
  mutate(LN3 = substr(LastName, 0, 3),
         FN3 = substr(FirstName, 0, 3)) %>%
  rename("GALLERY" = "Gallery"
         , "BookingDate" = "Book Date"
         , "Booking#" = "Book NO."
         # , "Booking#" = "Book No"
         # , "XXX" = "Inmate Name"
         , "SEX" = "Gender"
         , "HISPANICLATINO" = "Hispanic/Latino"
         , "RACE" = "Race"
         , "AdmissionType" = "Admission Type"
         , "ADMITDOCUMENTATIONTYPE" = "Admit Documention Type"
         , "JAILLOCATION" = "Jail"
         # , "JAILLOCATION" = "Jail Location"
         # , "XXX" = "Degree"
         , "GradeClass" = "Grade"
         , "OffenseDescription" = "Offense Description"
         , "SenYN" = "Sen Y/N"
         , "CaseSenStatus" = "Case Sen Status"
         , "DetainerHOLDCharge" = "Detainer Hold Charge"
         , "DetainerIssuingAgency" = "Detainer Agency"
         , "ReleaseType" = "Release Type"
         # , "XXX" = "Temp Rel. Date"
         , "ReleaseDate" = "Rel. Date"
  ) %>%
  select(LN3, FN3, DOB, BookingDate, AdmissionType)

arrest_data <- arrest_data %>%
  full_join(extra_data, by = c("LN3", "FN3", "DOB", "BookingDate", "AdmissionType"))


# add key columns and remove duplicates
arrest_data_clean <- arrest_data %>%
  mutate(person_id = paste0(LN3, 
                            FN3, 
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         arrest_row_id = row_number()) %>%
  distinct(person_id, BookingDate, .keep_all = TRUE)

small_arrests <- arrest_data_clean %>%
  # filter(AdmissionType %in% c("OUTRIGHT ONLY", "OUTRIGHT WITH HOLD",
  #                             "OUTRIGHT WITH WARRANTS")) %>%
  select("person_id", "BookingDate", "AdmissionType", "ReleaseDate",
         "OffenseDescription", "ReleaseType", "GALLERY")

write.csv(small_arrests, file = "small_arrests_5.16.22.csv", row.names = FALSE)

#############################################################################
#############################################################################
#############################################################################

# get_tableau_releases <- file.choose(); get_tableau_releases <- read.csv(get_tableau_releases)
get_tableau_releases <- deduped_releases
# add key columns and remove duplicates
small_releases <- get_tableau_releases %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(as.Date(DOB, "%m/%d/%Y"), "[^[:alnum:]]", "")),
         release_row_id = row_number()) %>%
  distinct(person_id, FACRLD, .keep_all = TRUE) %>%
  filter(COUNTY == 49) %>%
  # select(person_id, RELFROM, COUNTY, FACRLD)
  select(person_id, RELFROM, COUNTY, FACRLD, INTKSTCD, RELFROM, RELTYPCD)

write.csv(small_releases, file = "medium_releases_5.16.22.csv", row.names = FALSE)

# get_tableau_admits <- file.choose(); get_tableau_admits <- read.csv(get_tableau_admits)
get_tableau_admits <- deduped_admits
# add key columns and remove duplicates
small_admits <- get_tableau_admits %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(as.Date(DOB, "%m/%d/%Y"), "[^[:alnum:]]", "")),
         admit_row_id = row_number()) %>%
  # filter(INTKSTCD %nin% c(10, 11, 15, 16, 17, 18, 43)) %>%
  # distinct(person_id, INTKDT, .keep_all = TRUE)
  distinct(person_id, INTKDT, RECVDT, .keep_all = TRUE) %>%
  select(person_id, COUNTY, INTKDT, INTKSTCD, DOCNUM, RECVDT, RECFAC, RECVCD)

write.csv(small_admits, file = "medium_admits_5.16.22.csv", row.names = FALSE)



demographics_columns <- c("person_id", "last_name", "first_name",
                          "dob", "sex", "race", "information_date")
small_demographics <- deduped_admits %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(as.Date(DOB, "%m/%d/%Y"), "[^[:alnum:]]", "")),
         DOB = as.Date(DOB, "%m/%d/%Y"), 
         INTKDT = as.Date(INTKDT, "%m/%d/%Y")) %>%
  select(person_id, LASTNAME, FIRSTNAM, DOB, SEX, RACE, INTKDT) %>%
  `colnames<-`(demographics_columns) %>%
  mutate(origin = "admissions") %>%
  union_all(arrest_data_clean %>%
              # filter(AdmissionType %in% c("OUTRIGHT ONLY", "OUTRIGHT WITH HOLD",
              #                             "OUTRIGHT WITH WARRANTS")) %>%
              mutate(DOB = as.Date(DOB, "%m/%d/%Y"), 
                     BookingDate = as.Date(BookingDate, "%m/%d/%Y")) %>%
              select(person_id, LastName, FirstName, DOB, SEX, RACE, BookingDate) %>%
              `colnames<-`(demographics_columns) %>%
              mutate(origin = "arrests")) %>%
  union_all(deduped_releases %>%
              mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                                        substr(FIRSTNAM, 0, 3),
                                        str_replace_all(as.Date(DOB, "%m/%d/%Y"), "[^[:alnum:]]", "")),
                     DOB = as.Date(DOB, "%m/%d/%Y"), 
                     FACRLD = as.Date(FACRLD, "%m/%d/%Y")) %>%
              filter(COUNTY == 49) %>%
              select(person_id, LASTNAME, FIRSTNAM, DOB, SEX, RACE, FACRLD) %>%
              `colnames<-`(demographics_columns) %>%
              mutate(origin = "releases")) %>%
  arrange(race, desc(information_date)) %>%
  distinct(person_id, #last_name, first_name, dob, sex, race, 
           .keep_all = TRUE) %>%
  select(-information_date)

write.csv(small_demographics, file = "small_demographics_5.16.22.csv", row.names = FALSE)
