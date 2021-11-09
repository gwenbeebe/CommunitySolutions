library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(janitor)

`%nin%` = Negate(`%in%`)

##  RELEASES
##  pick releases file to read in
release_file <- file.choose(); release_data <- read_excel(release_file)
##  when there's extra rows
release_data <- read_excel(release_file,  
                           skip = which(release_data == "Obs", arr.ind=TRUE)[1])

##  OLD
# add key columns and remove duplicates
release_data_old <- release_data %>%
  filter(COUNTY == 49) %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", ""))) %>%
  distinct(person_id, FACRLD, .keep_all = TRUE)

##  NEW
release_data_new <- release_data %>%
  filter(COUNTY == 49 &
           INTKSTCD %nin% c(17, 18) &
           RELFROM %nin% c("XAD", "XZZ") &
           RELTYPCD %nin% c("D", "M", "E")) %>%
  distinct(DOCNUM, FACRLD, .keep_all = TRUE)

##  ADMISSIONS
##  pick admissions file to read in
admission_file <- file.choose(); admission_data <- read_excel(admission_file)
##  when there's extra rows
admission_data <- read_excel(admission_file,  
                             skip = which(admission_data == "Obs", arr.ind=TRUE)[1])

##  OLD
# add key columns and remove duplicates
admission_data_old <- admission_data %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         admit_row_id = row_number()) %>%
  # filter(INTKSTCD %nin% c(10, 11, 15, 16, 17, 18, 43)) %>%
  filter(INTKSTCD %nin% c(3, 5, 13, 20, 33, 42, 50, 52, 54, 56, 58)) %>%
  distinct(person_id, RECVDT, .keep_all = TRUE)
  # distinct(person_id, RECVDT, .keep_all = TRUE)

##  NEW
admission_data_new <- admission_data %>%
  filter(RECFAC %nin% c("PD1", "PD2", "PD3", "P4A", "P4B", "PD5",
                        "PD6", "PD7", "PD8", "PD9", "PD0", "XAD") &
           RECVCD %nin% c("15", "16", "17", "18", "10", "11",
                          "43", "79", "99")) %>%
  distinct(DOCNUM, INTKDT, .keep_all = TRUE)
  # distinct(DOCNUM, RECVDT, .keep_all = TRUE)



##  SUMMARIES
# create recidivism summary
recidivism <- release_data_new %>%
  # select(person_id, FACRLD) %>%
  select(DOCNUM, FACRLD) %>%
  left_join(admission_data_new %>%
              select(DOCNUM, INTKDT),
            by = "DOCNUM") %>%
  mutate(return_flag = case_when(
    FACRLD < INTKDT
    & (FACRLD + days(366)) >= INTKDT ~ TRUE,
    # & (FACRLD + days(183)) >= RECVDT ~ TRUE,
    TRUE ~ FALSE)) %>%
  arrange(desc(return_flag)) %>%
  select(-INTKDT) %>%
  mutate(cohort_of_release = paste(year(FACRLD))) %>%#, if_else(month(FACRLD) <= 6, "A", "B"))) %>%
  group_by(DOCNUM, cohort_of_release) %>%
  slice(1L) %>%
  ungroup()

# create summary text table
summary_table <- recidivism %>%
  tabyl(cohort_of_release, return_flag) %>%
  # mutate(month_of_release = paste(month(month_of_release, label = TRUE), year(month_of_release))) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(1) %>%
  adorn_ns()

write.table(summary_table, "clipboard", sep="\t", row.names=FALSE)
