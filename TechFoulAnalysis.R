library(data.table)
library(dplyr)
library(tibble)

# Read in Play-By-Play (PBP) file(s)
ncaam_1819<- tibble(fread("NatStat-MBB2019-Play-by-Play-2021-01-17-h15.csv"))

# Add row_id
ncaam_1819 <- rowid_to_column(ncaam_1819, "row_id")

free_throws <- ncaam_1819 %>%
  filter(grepl("free throw", Description))

techs <- ncaam_1819 %>%
  filter(grepl("technical|Technical", Description))

check <- ncaam_1819[9832:9839,]
  
  ncaam_1819[techs$row_id[1]:techs$row_id[1]+5,]
