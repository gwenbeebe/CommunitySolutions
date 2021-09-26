## NEW

release_data_clean <- release_data %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         release_row_id = row_number()) %>%
  filter(COUNTY == 49 &
           INTKSTCD %nin% c(17, 18) &
           RELFROM %nin% c("XAD", "XZZ") &
           RELTYPCD %nin% c("D", "M", "E")) %>%
  distinct(DOCNUM, FACRLD, .keep_all = TRUE)


admission_data_clean <- admission_data %>%
  mutate(person_id = paste0(substr(LASTNAME, 0, 3),
                            substr(FIRSTNAM, 0, 3),
                            str_replace_all(DOB, "[^[:alnum:]]", "")),
         admit_row_id = row_number()) %>%
  filter(RECFAC %nin% c("PD1", "PD2", "PD3", "P4A", "P4B", "PD5",
                        "PD6", "PD7", "PD8", "PD9", "PD0", "XAD") &
           RECVCD %nin% c("15", "16", "17", "18", "10", "11",
                          "43", "79", "99")) %>%
  # distinct(person_id, INTKDT, .keep_all = TRUE)
  distinct(DOCNUM, INTKDT, .keep_all = TRUE)





## OLD

small_releases <- release_data_clean %>%
  # filter(COUNTY == 49) %>%
  mutate(cohort_of_release = paste(year(FACRLD), if_else(month(FACRLD) <= 6, "A", "B"))) %>%
  select(DOCNUM, cohort_of_release, FACRLD)

small_admits <- admission_data_clean %>%
  select(DOCNUM, INTKDT)

small_arrests <- arrest_data_clean %>%
  filter(AdmissionType %in% c("OUTRIGHT ONLY", "OUTRIGHT WITH HOLD",
                              "OUTRIGHT WITH WARRANTS")) %>%
  select("person_id", "BookingDate", "AdmissionType")


# create admit recidivism summary
recidivism <- small_releases %>%
  left_join(small_admits,
            by = "DOCNUM") %>%
  mutate(return_flag = case_when(
    FACRLD < INTKDT
    # & (FACRLD + days(366)) >= INTKDT ~ TRUE,
    & (FACRLD + days(183)) >= INTKDT ~ TRUE,
    TRUE ~ FALSE)) %>%
  arrange(desc(return_flag)) %>%
  group_by(DOCNUM, cohort_of_release) %>%
  slice(1L) %>%
  ungroup()

# create arrest recidivism summary
recidivism <- release_data_clean %>%
  left_join(small_arrests,
            by = "person_id") %>%
  mutate(return_flag = case_when(
    FACRLD < BookingDate
    # & (FACRLD + days(365)) >= BookingDate ~ TRUE,
    & (FACRLD + days(180)) >= BookingDate ~ TRUE,
    TRUE ~ FALSE),
    cohort_of_release = paste(year(FACRLD), if_else(month(FACRLD) <= 6, "A", "B"))
  ) %>%
  arrange(desc(return_flag)) %>%
  group_by(person_id, cohort_of_release) %>%
  slice(1L) %>%
  ungroup()


# create summary text table
summary_table <- recidivism %>%
  tabyl(cohort_of_release, return_flag) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() %>%
  adorn_title("combined", row_name = "INTKDT in Six Months", col_name = "")
