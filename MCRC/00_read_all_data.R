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

##  pick file to read in
get_file <- file.choose()

##  do initial file read
file_hold <- read_excel(get_file)

##  do second file read
arrest_data <- read_excel(get_file, skip = which(file_hold[,1] == "Gallery"))


equity_check <- arrest_data %>%
  group_by(`Inmate Name`, DOB, `Book Date`) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup() %>%
  mutate(Race = if_else(Race %in% c("White", "WHITE"),
                        paste("White, ", `Hispanic/Latino`),
                        Race)) %>%
  group_by(Race) %>%
  summarise(count = n()) %>%
  mutate(percent = round((count / sum(count) * 100), 1))
