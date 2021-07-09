library(data.table)
library(dplyr)
library(tibble)
library(purrr)
library(stringr)

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

# Determine the shooter
free_throws <- free_throws %>%
  mutate(Shooter = sapply(str_split(Description, " made "), function(x) x[1])) %>%
  mutate(Shooter = sapply(str_split(Shooter, " Made "), function(x) x[1])) %>%
  mutate(Shooter = sapply(str_split(Shooter, " missed "), function(x) x[1])) %>%
  mutate(Shooter = sapply(str_split(Shooter, " Missed "), function(x) x[1]))

# Get a list of all FT shooters
allShooters <- unique(free_throws$Shooter)

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
# Don't really need those, so remove them.

keepers <- free_throws %>%
  filter (!(row_id %in% the_missing$row_id))

# Check counts again
nrow(made)+nrow(missed) == nrow(keepers)

# Find all technical fouls
techs <- ncaam_1819 %>%
  filter(grepl("technical|Technical", Description)) %>%
  mutate(Shooter = "None")

# Combine the technical fouls and free throws
tf_ft <- rbind(techs, made, missed)

# Reorder by row number
tf_ft <- tf_ft %>%
  arrange(row_id)

# Keep only free throws within 5 (?) rows of a technical foul
# What about a double technical foul?  Look for example.

result1 <- techs$row_id +1
result2 <- techs$row_id +2
result3 <- techs$row_id +3
result4 <- techs$row_id +4
result5 <- techs$row_id +5
result <- sort(c(techs$row_id,result1, result2, result3,  result4,  result5))

# There's got to be a better way.  Maybe something like this?
# results <- techs %>%
#  select(row_id) %>%
#  map( ~ c(.x + 1:6)) %>%
#  flatten_dbl()

ft_after_tf <- tf_ft %>%
  filter(row_id %in% result) %>%
  filter(grepl("made|Made|missed|miss", Description))

# Find free throws made
made_tf <- ft_after_tf %>%
  filter(grepl("made|Made", Description))

# Find free throws missed
missed_tf <- ft_after_tf %>%
  filter(grepl("missed|miss", Description))

# Calculate the FT % for FTs after TFs.
tfFTperc <- nrow(made_tf) / (nrow(ft_after_tf))

# Separate out FTs NOT after Tfs.
non_techs <- keepers %>%
  filter(!(row_id %in% result))
non_tech_made <- non_techs %>%
  filter(grepl("made|Made", Description))

# Calculate the FT % for FTs NOT after TFs.
FTperc <- nrow(non_tech_made) / nrow(non_techs)

# Make a list of all FT shooters who shot at least 1 TF shot
TFshooters <- unique(ft_after_tf$Shooter)

# Get all NON TF shots by shooters who shot at least 1 TF shot
non_tech_by_TF <- non_techs %>%
  filter(Shooter %in% TFshooters)
made_by_TF <- non_tech_made %>%
  filter(Shooter %in% TFshooters)

# Calculate the FT % for FTs NOT after TFs by shooters who shot at least 1 TF
FTperc_TF_shoot <- nrow(made_by_TF) / nrow(non_tech_by_TF)

# Check if all are accounted for
nrow(ft_after_tf)
nrow(non_techs)
nrow(ft_after_tf)+nrow(non_techs)
nrow(made)+nrow(missed)
(nrow(ft_after_tf)+nrow(non_techs))-(nrow(made)+nrow(missed))
#missing 828.  (But that's the number of technical fouls!)

team_bench <- techs %>%
  filter(grepl("team|Team|bench|Bench|admin|Admin", Description))

