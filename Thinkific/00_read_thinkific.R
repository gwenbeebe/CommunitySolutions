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
library(tidyverse)
library(lubridate)
library(readxl)


##  pick file to read in
get_file <- file.choose()


##  create prefix for tables`
{
  file_name <- sapply(strsplit(get_file,"\\\\"), `[`, 11)
  file_name <- gsub("Pre", "PRE", file_name)
  file_name <- gsub("Post", "POST", file_name)
  uppercase_in_file_name <- strsplit(file_name, "[[:lower:]]*")
  prefix <- gsub("[^A-Z]","", uppercase_in_file_name)
}


##  read in file information
for(i in 1:length(excel_sheets(get_file))) {
  file_hold <- read_excel(get_file, sheet = excel_sheets(get_file)[i])
  if (colnames(file_hold)[1] == "Column1") {
    file_hold <- read_excel(get_file, sheet = excel_sheets(get_file)[i], skip = 1)
  }
  assign(paste0(prefix, "_", i), file_hold)
}

