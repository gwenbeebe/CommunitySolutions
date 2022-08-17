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

`%nin%` = Negate(`%in%`)

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

write.csv(small_arrests, file = "small_arrests_2.9.22 v2.csv", row.names = FALSE)

#################################################
##  update release data
#################################################
##  pick releases file to read in
# release_file <- file.choose(); release_data <- read_excel(release_file)
release_file <- file.choose(); release_data <- read.csv(release_file)

##  when there's extra rows
release_data <- read_excel(release_file,  
                         skip = which(release_data == "Obs", arr.ind=TRUE)[1])

release_columns <- as.data.frame(colnames(release_data))

# add key columns and remove duplicates
release_data_clean <- release_data %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         release_row_id = row_number()) %>%
  distinct(person_id, FACRLD, .keep_all = TRUE)

small_releases <- release_data_clean %>%
  filter(COUNTY == 49) %>%
  select(person_id, RELFROM, COUNTY, FACRLD)

write.csv(small_releases, file = "small_releases_1.31.22.csv", row.names = FALSE)


#################################################
##  update admissions data
#################################################
##  pick admissions file to read in
# admission_file <- file.choose(); admission_data <- read_excel(admission_file)
admission_file <- file.choose(); admission_data <- read.csv(admission_file)

##  when there's extra rows
admission_data <- read_excel(admission_file,  
                           skip = which(admission_data == "Obs", arr.ind=TRUE)[1])

admission_columns <- as.data.frame(colnames(admission_data))

# add key columns and remove duplicates
admission_data_clean <- admission_data %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         admit_row_id = row_number()) %>%
  # filter(INTKSTCD %nin% c(10, 11, 15, 16, 17, 18, 43)) %>%
  # distinct(person_id, INTKDT, .keep_all = TRUE)
  distinct(person_id, RECVDT, .keep_all = TRUE)

# write base csvs
small_admits <- admission_data_clean %>%
  select(person_id, COUNTY, INTKDT, INTKSTCD, DOCNUM)

write.csv(small_admits, file = "small_admits_1.31.22.csv", row.names = FALSE)


#################################################
##  update demographics data
#################################################
demographics_columns <- c("person_id", "last_name", "first_name",
                          "dob", "sex", "race", "information_date")
small_demographics <- admission_data_clean %>%
  select(person_id, LASTNAME, FIRSTNAM, DOB, SEX, RACE, INTKDT) %>%
  `colnames<-`(demographics_columns) %>%
  mutate(origin = "admissions") %>%
  union_all(arrest_data_clean %>%
              filter(AdmissionType %in% c("OUTRIGHT ONLY", "OUTRIGHT WITH HOLD",
                                          "OUTRIGHT WITH WARRANTS")) %>%
              select(person_id, LastName, FirstName, DOB, SEX, RACE, BookingDate) %>%
              `colnames<-`(demographics_columns) %>%
              mutate(origin = "arrests")) %>%
  union_all(release_data_clean %>%
              filter(COUNTY == 49) %>%
              select(person_id, LASTNAME, FIRSTNAM, DOB, SEX, RACE, FACRLD) %>%
              `colnames<-`(demographics_columns) %>%
              mutate(origin = "releases")) %>%
  arrange(race, desc(information_date)) %>%
  distinct(person_id, #last_name, first_name, dob, sex, race, 
           .keep_all = TRUE) %>%
  select(-information_date)

write.csv(small_demographics, file = "small_demographics.csv", row.names = FALSE)



##  summaries

# create recidivism summary
recidivism <- release_data_clean %>%
  filter(COUNTY == 49) %>%
  select(person_id, FACRLD) %>%
  # select(DOCNUM, FACRLD) %>%
  left_join(arrest_data_clean %>%
              filter(AdmissionType %in% c("OUTRIGHT ONLY", "OUTRIGHT WITH HOLD",
                                          "OUTRIGHT WITH WARRANTS")) %>%
              select(person_id, BookingDate),
            # select(DOCNUM, INTKDT),
            by = "person_id") %>%
  mutate(return_flag = case_when(
    FACRLD < BookingDate
    # & (FACRLD + days(364)) >= BookingDate ~ TRUE,
    & (FACRLD + days(182)) >= BookingDate ~ TRUE,
    TRUE ~ FALSE)) %>%
  arrange(desc(return_flag)) %>%
  select(-BookingDate) %>%
  mutate(cohort_of_release = paste(year(FACRLD), if_else(month(FACRLD) <= 6, "A", "B"))) %>%
  group_by(person_id, cohort_of_release) %>%
  slice(1L) %>%
  ungroup()


# create summary text table
summary_table <- recidivism %>%
  tabyl(cohort_of_release, return_flag) %>%
  # mutate(month_of_release = paste(month(month_of_release, label = TRUE), year(month_of_release))) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(0) %>%
  adorn_ns()

# create draft chart for exploration
chart_table <- recidivism %>%
  filter(FACRLD >= (today() - years(2))) %>%
  mutate(month_of_release = floor_date(FACRLD, "month")) %>%
  group_by(month_of_release, return_flag) %>%
  summarise(people = n()) %>%
  arrange(month_of_release) %>%
  mutate(month_of_release = paste(month(month_of_release, label = TRUE), year(month_of_release)))

ggplot(chart_table, aes(fill=return_flag, y=people, x=month_of_release)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip()


# used this for spot checking the demographics table for uniqueness
# identify_flaws <- small_demographics  %>%
#   group_by(person_id) %>%
#   mutate(differences = n()) %>%
#   ungroup() %>%
#   filter(differences > 1) %>%
#   arrange(person_id, origin, information_date)

# splitting out individual charges from the release data
for (letter in c("A", "B", "C", "D", "E", "F")) {
  
  r_letter_charges <- release_data_clean %>%
    filter(COUNTY == 49)  %>%
    select(c("Obs", starts_with(letter))) %>%
    `colnames<-`(c("Obs", (substring(names(.)[2:length(.)], 2))))
  
  if (exists("r_columns_to_keep")) {
    r_columns_to_keep <- intersect(colnames(r_all_charges), colnames(r_letter_charges))
  } else {
    r_columns_to_keep <- colnames(r_letter_charges)
  }
  
  if (exists("r_all_charges")) {
    r_all_charges <- r_all_charges %>%
      left_join(r_letter_charges %>% 
                  remove_empty("rows") %>%
                  filter(!is.na(COURT))
                , by = r_columns_to_keep) %>%
      select(r_columns_to_keep)
  } else {
    r_all_charges <- r_letter_charges %>% 
      remove_empty("rows") %>%
      filter(!is.na(COURT))
  }
}

write.csv(r_all_charges, file = "all_release_charges.csv", row.names = FALSE)



# splitting out individual charges from the admit data
for (letter in c("A", "B", "C", "D", "E", "F")) {
  
  ad_letter_charges <- admission_data %>%
    select(c("Obs", starts_with(letter))) %>%
    `colnames<-`(c("Obs", (substring(names(.)[2:length(.)], 2))))
  
  if (exists("ad_columns_to_keep")) {
    ad_columns_to_keep <- intersect(colnames(ad_all_charges), colnames(ad_letter_charges))
  } else {
    ad_columns_to_keep <- colnames(ad_letter_charges)
  }
  
  if (exists("ad_all_charges")) {
    ad_all_charges <- ad_all_charges %>%
      full_join(ad_letter_charges %>% 
                  remove_empty("rows") %>%
                  filter(!is.na(COURT))
                , by = ad_columns_to_keep) %>%
      select(ad_columns_to_keep)
  } else {
    ad_all_charges <- ad_letter_charges %>% 
      remove_empty("rows") %>%
      filter(!is.na(COURT))
  }
}

write.csv(ad_all_charges, file = "all_admit_charges.csv", row.names = FALSE)



# get admits for incarceration dashboard

for (letter in c("A", "B", "C", "D", "E", "F")) {
  if (exists("pivoted_ad_cols")) {
    pivoted_ad_cols <- c(pivoted_ad_cols, 
                      paste0(letter, colnames(ad_all_charges)[2:length(colnames(ad_all_charges))]))
  } else {
    pivoted_ad_cols <- paste0(letter, colnames(ad_all_charges)[2:length(colnames(ad_all_charges))])
  }
}

incarceration_admits <- admission_data %>%
  filter(COUNTY == 49) %>%
  select(-pivoted_ad_cols)
write.csv(incarceration_admits, file = "incarceration_admits.csv", row.names = FALSE)



# get releases for incarceration dashboard

for (letter in c("A", "B", "C", "D", "E", "F")) {
  if (exists("pivoted_r_cols")) {
    pivoted_r_cols <- c(pivoted_r_cols, 
                         paste0(letter, colnames(r_all_charges)[2:length(colnames(r_all_charges))]))
  } else {
    pivoted_r_cols <- paste0(letter, colnames(r_all_charges)[2:length(colnames(r_all_charges))])
  }
}

incarceration_releases <- release_data %>%
  filter(COUNTY == 49) %>%
  select(-pivoted_r_cols) %>%
  distinct()
write.csv(incarceration_releases, file = "incarceration_releases.csv", row.names = FALSE)
