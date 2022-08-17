library(janitor)
library(tidyverse)
library(lubridate)
library(stringr)

# `%nin%` = Negate(`%in%`)

matchColClasses <- function(df1, df2) {
  
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  
  return(df2)
}



all_files <- list.files(
  paste0(getwd(), "/data/IMPD Arrest Data Files"),
  full.names = TRUE,
  # recursive = TRUE)
  recursive = FALSE)

release_files <- all_files[endsWith(all_files, ".csv")]
# admit_files <- all_files[grepl("ADM", all_files)]

all_file_names <- list.files(
  paste0(getwd(), "/data/IMPD Arrest Data Files"),
  # recursive = TRUE)
  recursive = FALSE)

release_file_names <- all_files[endsWith(all_files, "MCSO.csv")]
# admit_file_names <- all_file_names[grepl("ADM", all_file_names)]
file_length <- matrix(ncol = 2)


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

deduped_releases <- combined_releases %>%
  # mutate(last_name = strsplit(Inmate.Last..First..Middle, split = ",")[1])
  separate(Inmate.Last..First..Middle, c("LASTNAME", "FIRSTNAME"), sep = ", ") %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAME, 0, 3),
                            str_replace_all(as.Date(DOB, "%m/%d/%Y"), "[^[:alnum:]]", "")),
         Book.Date = mdy(gsub( " .*$", "", Book.Date)),
         Release.Date = mdy(gsub( " .*$", "", Release.Date))) %>%
  distinct(person_id, Book.Date, Release.Date, .keep_all = TRUE)

write.csv(deduped_releases, file = "all_arrest_data_6.23.22.csv", row.names = FALSE)
# write.csv(release_offenses, file = "tableau_release_offenses_3.19.22.csv", row.names = FALSE)



#################################################
# test <- deduped_releases %>%
#   group_by(Release.Type) %>%
#   summarise(inmate = max(Booking.No.), 
#             releases = n(), 
#             last_used = max(as.Date(Release.Date, "%m/%d/%Y"))) %>% 
#   arrange(releases)
# 
# test
# 
# file <- file.choose(); release_charges <- read.csv(file)
# file <- file.choose(); admit_charges <- read.csv(file)
# 
# charge_columns <- c("relevant_date", "conviction_class", 
#                     "offense", "offense_type", "origin")
# small_charges <- release_charges %>%
#   select(EDS, CNVCLS, OFF1, TYPE1) %>%
#   mutate(origin = "Release") %>%
#   `colnames<-`(charge_columns) %>%
#   union(release_charges %>%
#           select(EDS, CNVCLS, OFF2, TYPE2) %>%
#           mutate(origin = "Release") %>%
#           filter(!is.na(OFF2)) %>%
#           `colnames<-`(charge_columns)) %>%
#   union(admit_charges %>%
#           select(EDS, CNVCLS, OFF1, TYPE1) %>%
#           mutate(origin = "Admit") %>%
#           `colnames<-`(charge_columns)) %>%
#   union(release_charges %>%
#           select(EDS, CNVCLS, OFF2, TYPE2) %>%
#           mutate(origin = "Admit") %>%
#           filter(!is.na(OFF2)) %>%
#           `colnames<-`(charge_columns)) %>%
#   union(deduped_releases %>%
#           select(Offense.Date, DEGREE, Offense) %>%
#           mutate(type = "", origin = "Arrest Releases") %>%
#           filter(!is.na(Offense)) %>%
#           `colnames<-`(charge_columns))
# 
# write.csv(small_charges, file = "all_offenses_5.17.22.csv")
#################################################

##  summaries

# create recidivism summary
recidivism <- deduped_releases %>%
  filter(!is.na(Release.Date)) %>%
  select(person_id, Release.Date) %>%
  left_join(deduped_releases %>%
              # filter(AdmissionType %in% c("OUTRIGHT ONLY", "OUTRIGHT WITH HOLD",
              #                             "OUTRIGHT WITH WARRANTS")) %>%
              select(person_id, Book.Date),
            by = "person_id") %>%
  mutate(return_flag = case_when(
    Release.Date < Book.Date
    # & (Release.Date + days(364)) >= Book.Date ~ TRUE,
    & (Release.Date + days(182)) >= Book.Date ~ TRUE,
    TRUE ~ FALSE)) %>%
  arrange(desc(return_flag)) %>%
  select(-Book.Date) %>%
  mutate(cohort_of_release = paste(year(Release.Date), if_else(month(Release.Date) <= 6, "A", "B"))) %>%
  # mutate(cohort_of_release = paste(year(Release.Date), if_else(month(Release.Date) <= 3, "A", "B"))) %>%
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

write.table(summary_table, "clipboard", sep="\t")

# # create draft chart for exploration
# chart_table <- recidivism %>%
#   filter(FACRLD >= (today() - years(2))) %>%
#   mutate(month_of_release = floor_date(FACRLD, "month")) %>%
#   group_by(month_of_release, return_flag) %>%
#   summarise(people = n()) %>%
#   arrange(month_of_release) %>%
#   mutate(month_of_release = paste(month(month_of_release, label = TRUE), year(month_of_release)))
# 
# ggplot(chart_table, aes(fill=return_flag, y=people, x=month_of_release)) + 
#   geom_bar(position="fill", stat="identity") +
#  coord_flip()
