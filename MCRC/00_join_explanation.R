##  these add in some tools we use for the analysis--they're basically giving
##  access to additional functions
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(janitor)

##  this creates a function I can use to say "include everything NOT in the following"
`%nin%` = Negate(`%in%`)

##  RELEASES
##  pick releases file to read in
release_file <- file.choose(); release_data <- read_excel(release_file)
##  when there's extra rows, skip any blanks. If there are no empty rows, this has no effect
release_data <- read_excel(release_file,  
                           skip = which(release_data == "Obs", arr.ind=TRUE)[1])

##  create new data frame with cleaned data, so we don't have to 
##  re-load the data every time we change something
release_data_new <- release_data %>%
  ##  keep just Marion County
  filter(COUNTY == 49 &
           ## remove intakes with INTKSTCD 17 or INTKSTCD 18
           INTKSTCD %nin% c(17, 18) &
           ## removes intakes with RELFROM XAD or RELFROM XZZ
           RELFROM %nin% c("XAD", "XZZ") &
           ## removes intakes with RELTYPCD D, RELTYPCD M, or RELTYPCD E
           RELTYPCD %nin% c("D", "M", "E")) %>%
  ##  keep one row for every unique pair of DOCNUM and FACRLD,
  ##  to avoid counting people more than once if they have multiple
  ##  release rows for a single day
  distinct(DOCNUM, FACRLD, .keep_all = TRUE)


##  ADMISSIONS
##  pick admissions file to read in
admission_file <- file.choose(); admission_data <- read_excel(admission_file)
##  when there's extra rows, skip any blanks. If there are no empty rows, this has no effect
admission_data <- read_excel(admission_file,  
                             skip = which(admission_data == "Obs", arr.ind=TRUE)[1])

##  create new data frame with cleaned data, so we don't have to 
##  re-load the data every time we change something
admission_data_new <- admission_data %>%
  ##  removes admits with RECFAC in this list
  filter(RECFAC %nin% c("PD1", "PD2", "PD3", "P4A", "P4B", "PD5",
                        "PD6", "PD7", "PD8", "PD9", "PD0", "XAD") &
           ## removes admits with RECVCD in this list
           RECVCD %nin% c("15", "16", "17", "18", "10", "11",
                          "43", "79", "99")) %>%
  ##  keep one row for every unique pair of DOCNUM and INTKDT,
  ##  to avoid counting people more than once if they have multiple
  ##  admit rows for a single day
  distinct(DOCNUM, INTKDT, .keep_all = TRUE)



##  SUMMARIES
# create recidivism summary
recidivism <- release_data_new %>%
  ##  keep just the DOC and FACRLD from the release data
  select(DOCNUM, FACRLD) %>%
  ##  join all DOCNUM and INTKDT pairs from the admit data based 
  ##  on matching DOCNUMs
  left_join(admission_data_new %>%
              select(DOCNUM, INTKDT),
            by = "DOCNUM") %>%
  ##  create a T/F flag variable that marks whether a given release data
  ##  row has a qualifying return event
  mutate(return_flag = case_when(
    ##  to qualify, the release date must be earlier than the intake date
    FACRLD < INTKDT
    ##  for one year returns, the intake date must be less than or equal
    ##  to the release date + 366 days
    & (FACRLD + days(366)) >= INTKDT ~ TRUE,
    ##  for six month returns, the intake date must be less than or equal
    ##  to the release date + 183 days
    # & (FACRLD + days(183)) >= RECVDT ~ TRUE,
    ##  if the date conditions are not met, mark that particular pair
    ##  as not having a return match
    TRUE ~ FALSE)) %>%
  ##  bring all pairs flagged as returns to the top
  arrange(desc(return_flag)) %>%
  ##  remove the INTKDT variable, because we don't need it now that we've
  ##  identified the returns
  select(-INTKDT) %>%
  ##  create a variable to assign each row to a cohort based on their release
  ##  date. Cohorts are the release year + A for January-June and B for July-December
  mutate(cohort_of_release = paste(year(FACRLD), if_else(month(FACRLD) <= 6, "A", "B"))) %>%
  ##  group all rows for each individual in each cohort together (i.e. someone with 
  ##  two releases in the first half of 2020 and one in the second half of 2020 would
  ##  two groups, one with the first two releases and the second with that single 
  ##  release from the second half of the year)
  group_by(DOCNUM, cohort_of_release) %>%
  ##  keep only the top pair in each group. We can do this because above we put all
  ##  the returns at the top, so we know if a perosn has a return in a cohort period 
  ##  we will keep that row
  slice(1L) %>%
  ##  remove the grouping, because now we have one row per person per cohort
  ##  they appear in
  ungroup()

# create summary text table
summary_table <- recidivism %>%
  ##  assign the cohort to the rows and create two columns for the return flag
  tabyl(cohort_of_release, return_flag) %>%
  ##  show a total column
  adorn_totals("col") %>%
  ##  show percentages in addition to raw counts
  adorn_percentages("row") %>%
  ##  round percentages to the nearest whole percent
  adorn_pct_formatting(1) %>%
  ##  show raw counts
  adorn_ns()

##  optional--saves table of results to your clipboard so you can paste it
write.table(summary_table, "clipboard", sep="\t", row.names=FALSE)
