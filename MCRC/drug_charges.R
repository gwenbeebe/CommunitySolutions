library(dplyr)


##  RELEASES
##  pick releases file to read in
release_file <- file.choose()
#excel
release_data <- read_excel(release_file)
##  when there's extra rows, skip any blanks. If there are no empty rows, this has no effect
release_data <- read_excel(release_file,  
                           skip = which(release_data == "Obs", arr.ind=TRUE)[1])
#csv
release_data <- read.csv(release_file, skipNul = TRUE)

##  ADMISSIONS
##  pick admissions file to read in
admission_file <- file.choose()
#excel
admission_data <- read_excel(admission_file)
##  when there's extra rows, skip any blanks. If there are no empty rows, this has no effect
admission_data <- read_excel(admission_file,  
                             skip = which(admission_data == "Obs", arr.ind=TRUE)[1])
#csv
admission_data <- read.csv(admission_file, skipNul = TRUE)


arrest_file <- file.choose(); arrest_data <- read_excel(arrest_file)
arrest_columns <- as.data.frame(colnames(arrest_data))

{extra_file <- file.choose()
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
}


drug_related <- file.choose()
drug_related_data <- read_excel(drug_related)



test <- admission_data %>%
  select(DOCNUM, RECVDT, EPRDT, MRD, PRD, SEX, RACE, HIGRADE, 
         ARMEDFORCES, FIRSTTIME) #%>%
  mutate(RECVDT = as.POSIXct(as.numeric(as.character(RECVDT)),origin="1970-01-01"))

has_admits <- drug_related_data %>%
  left_join(admission_data %>%
              select(DOCNUM, RECVDT, INTKDT, EPRDT, MRD, PRD, SEX, RACE, HIGRADE, 
                     ARMEDFORCES, FIRSTTIME, DOB) %>%
              # mutate(RECVDT = as.POSIXct(RECVDT)) %>%
              `colnames<-`(c("DOCNUM", "RECVDT", "ADM_INTKDT", "ADM_EPRDT", "ADM_MRD", "ADM_PRD",
                             "ADM_SEX", "ADM_RACE", "ADM_HIGRADE", 
                             "ADM_ARMEDFORCES", "ADM_FIRSTTIME", "ADM_DOB")),
            by = c("DOCNUM", "RECVDT"))



combined <- has_admits %>%
  left_join(release_data %>%
              select(DOCNUM, FACRLD, INTKDT, RELFROM, RELTYPCD, RELTO, MRD, PRD,
                     SEX, RACE, HIGRADE, ARMEDFORCES, FIRSTTIME) %>%
              filter(FACRLD >= as.Date("05/19/21", "%m/%d/%y")) %>%
              `colnames<-`(c("DOCNUM", "REL_FACRLD", "REL_INTKDT", "REL_RELFROM", 
                             "REL_RELTYPCD", "REL_RELTO", "REL_MRD", "REL_PRD", 
                             "REL_SEX", "REL_RACE", "REL_HIGRADE", 
                             "REL_ARMEDFORCES", "REL_FIRSTTIME")), 
            by = "DOCNUM") %>%
  arrange(desc(REL_FACRLD)) %>%
  group_by(DOCNUM) %>%
  slice(1L) %>%
  ungroup() 
  # filter(REL_FACRLD >= as.Date("05/19/21", "%m/%d/%y"))

green_data <- file.choose(); green_data <- read_excel(green_data)

has_green <- combined %>%
  left_join(green_data, by = c("DOCNUM" = "DOC#")) %>%
  mutate(combined_result = case_when(
    is.na(`Cause No.`) & is.na(REL_FACRLD) ~ "Not Recommended, Not Released",
    !is.na(`Cause No.`) & is.na(REL_FACRLD) ~ "Recommended, Not Released",
    is.na(`Cause No.`) & !is.na(REL_FACRLD) ~ "Not Recommended, Released",
    !is.na(`Cause No.`) & !is.na(REL_FACRLD) ~ "Recommended, Released"
  ),
  INTKDT = if_else(is.na(REL_INTKDT), ADM_INTKDT, REL_INTKDT)) %>%
  select(!c(ADM_INTKDT, REL_INTKDT))

# write.csv(combined, "Released Since 5.19.21 V2.csv", na = "")
write.csv(has_green, "DrugChargesDueForRelease.csv", na = "")














##################

new_data <- file.choose(); new_data <- read_excel(new_data)
new_has_admits <- new_data %>%
  left_join(admission_data %>%
              arrange(desc(RECVDT)) %>%
              group_by(DOCNUM) %>%
              slice(1L) %>%
              ungroup() %>%
              select(DOCNUM, DOB) %>%
              # mutate(RECVDT = as.POSIXct(RECVDT)) %>%
              `colnames<-`(c("DOCNUM", "ADM_DOB")),
            by = c("DOCNUM"))

new_combined <- new_has_admits %>%
  left_join(release_data %>%
              select(DOCNUM, FACRLD, DOB) %>%
              # filter(FACRLD >= as.Date("05/19/21", "%m/%d/%y")) %>%
              `colnames<-`(c("DOCNUM", "REL_FACRLD", "REL_DOB")), 
            by = "DOCNUM") %>%
  arrange(desc(REL_FACRLD)) %>%
  group_by(DOCNUM) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(Combined_DOB = if_else(is.na(REL_DOB), ADM_DOB, REL_DOB)) %>%
  select(-c(REL_FACRLD, REL_DOB, ADM_DOB))
