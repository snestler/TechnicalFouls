library(data.table)
library(dplyr)
library(tibble)

# Read in Play-By-Play (PBP) file(s)
ncaam_1819<- tibble(fread("NatStat-MBB2019-Play-by-Play-2021-01-17-h15.csv"))

# Add row_id
ncaam_1819 <- rowid_to_column(ncaam_1819, "row_id")

# Remove unnecessary columns
keeps <- c("row_id", "TeamID", "OppID", "Description", "ScoringPlay", "Points")
ncaam_1819 <- ncaam_1819[keeps]

# Find all free throws
free_throws <- ncaam_1819 %>%
  filter(grepl("free throw", Description))

# Find free throws made
made <- free_throws %>%
  filter(grepl("made|Made", Description))

# Find free throws missed
missed <- free_throws %>%
  filter(grepl("missed|miss", Description))

# Check if all are accounted for
nrow(made)+nrow(missed) == nrow(free_throws)
nrow(made)+nrow(missed)
nrow(free_throws)
nrow(free_throws) - (nrow(made) + nrow(missed))
# Missing 4951 FTs somewhere
the_missing <- free_throws %>%
  filter(!grepl("made|Made|missed|miss", Description))
# So these are the PFs with number of FTs awarded (1, 2, or 3)
# Don't really need those.

# Find all technical fouls
techs <- ncaam_1819 %>%
  filter(grepl("technical|Technical", Description))

# Combine the technical fouls and free throws
tf_ft <- rbind(techs, made, missed)

# Reorder by row number
tf_ft <- tf_ft %>%
  arrange(row_id)

# Keep only free throws within 6 (?) rows of a technical foul
result <- techs %>%
  select(row_id) %>% 
  map( ~ c(.x + 1:6)) %>% 
  flatten_dbl()

ft_after_tf <- tf_ft %>%
  filter(row_id %in% result)

# Find free throws made
made_tf <- ft_after_tf %>%
  filter(grepl("made|Made", Description))

# Find free throws missed
missed_tf <- ft_after_tf %>%
  filter(grepl("missed|miss", Description))

# Calculate the FT % for FTs after TFs.
tfFTperc <- nrow(made_tf) / (nrow(made_tf) + nrow(missed_tf))

# Separate out FTs NOT after Tfs.
non_techs <- tf_ft %>%
  filter(!(row_id %in% result))

# Check if all are accounted for
nrow(techs)
nrow(non_techs)
nrow(techs)+nrow(non_techs)
nrow(made)+nrow(missed)
