# =============================================================================
# Build relational database from coded qualitative data
#
# This script processes coded survey data and associated metadata to build a 
# tidy, relational database. The main goal is to map themes to individual papers
# (identified by DOI), and generate lookup tables for journals, publishers, 
# authors, and metadata themes (barriers, opportunities, etc).
#
# INPUT:
#  - Cleaned Qualtrics-coded data (cleandata.csv)
#  - Raw coder export (for DOI list)
#  - Metadata CSVs (theme tables, barriers, etc.)
#  - CrossRef API metadata
#
# OUTPUT:
#  - paper_theme_tbl.csv (DOI ↔ themeID)
#  - author_tbl.csv
#  - author_paper_tbl.csv
# =============================================================================


# ---- Load packages and set reproducibility seed ---------------------------- #

library(tidyverse)      # Data manipulation
library(rcrossref)      # Fetch DOI metadata
set.seed(123)           # Ensure consistent random suffix assignment


# ---- Load raw coder export and remove headers ----------------------------- #

dat <- read_csv("data/Claim+Analysis_June+18,+2025_09.53.csv")
data <- dat[3:nrow(dat), ]  # Skip Qualtrics metadata and prompts
rm(dat)


# ---- Load cleaned long-format values -------------------------------------- #

clean <- read_csv("out/tables/cleandata.csv") |>
  mutate(
    value = ifelse(variable == "distrust.sources" & value == "Non-specified concerns", 
                   "Non-specified Distrust", value),
    value = ifelse(variable == "distrust.sources" & value == "Other (write)", 
                   "other distrust", value),
    value = ifelse(variable == "env.opps" & value == "Non-specified", 
                   "Non-specified Env", value),
    value = ifelse(variable == "Health.opps" & value == "Non-specified", 
                   "Non-specified Health", value),
    value = ifelse(variable == "development.category" & value == "other", 
                   "other Dev", value),
    value = ifelse(variable == "consequences" & value == "other", 
                   "other consequence", value)
  ) |>
  separate_longer_delim(value, ",") |>
  distinct()


# ---- Load metadata tables for BLOC categories ----------------------------- #

bloc_tbl <- read_csv("data/bloc_tbl.csv")
barrierType_tbl <- read_csv("data/barrierType_tbl.csv")
developmentBarrier_tbl <- read_csv("data/developmentBarrier_tbl.csv")
adoptionBarrier_tbl <- read_csv("data/adoptionBarrier_tbl.csv")
distrustBarrier_tbl <- read.csv("data/distrustBarrier_tbl.csv")
sensesDistrusted_tbl <- read.csv("data/sensesDistrusted_tbl.csv")
opportunityType_tbl <- read.csv("data/opportunityType_tbl.csv")
environmentalOpportunity_tbl <- read_csv("data/environmentOpportunity_tbl.csv")
pollutionOpportunity_tbl <- read.csv("data/pollutionOpportunity_tbl.csv")
humanOpportunity_tbl <- read_csv("data/humanOpportunity_tbl.csv")
healthOpportunity_tbl <- read_csv("data/healthOpportunity_tbl.csv")
safetyOpportunity_tbl <- read_csv("data/safetyOpportunity_tbl.csv")
limitation_tbl <- read_csv("data/limitation_tbl.csv")
consequence_tbl <- read_csv("data/consequence_tbl.csv")


# ---- Load section and aim metadata ---------------------------------------- #

aim_tbl <- read.csv("data/AIM_tbl.csv") |>
  arrange(aimID) |>
  mutate(aimID = factor(aimID))

section_tbl <- read_csv("data/Section_tbl.csv") |>
  mutate(sectionID = factor(sectionID))


# ---- Fetch DOI metadata using CrossRef ----------------------------------- #

DOIs <- data |>
  select(doi = `paper-doi_1`) |>
  distinct()

doi_data <- cr_works(DOIs$doi)$data
rm(DOIs)

paper_tbl <- doi_data |>
  select(
    doi,
    publisherID = publisher,
    journalID = container.title,
    dateCreated = created,
    references = reference.count,
    referencedBy = is.referenced.by.count
  )


# ---- Reference statistics ------------------------------------------------- #

mean_paperReferencedBy <- mean(na.omit(paper_tbl$referencedBy))
mean_yearsAgo <- mean(2025 - as.numeric(na.omit(year(paper_tbl$dateCreated))))
meanReferencesPerYear <- mean_paperReferencedBy / mean_yearsAgo


# ---- Create lookup tables: publishers, journals --------------------------- #

publisher_tbl <- doi_data |>
  select(publisherID = publisher) |>
  distinct() |>
  arrange(publisherID)

journal_tbl <- doi_data |>
  select(journalID = container.title) |>
  distinct() |>
  mutate(journalID = str_replace_all(journalID, "amp;", "")) |>
  arrange(journalID)


# ---- Extract author metadata ---------------------------------------------- #

get_orcid_suffix <- function(orcid) {
  if (!is.na(orcid) && str_detect(orcid, "\\d{4}$")) {
    str_sub(orcid, -4)
  } else {
    NA_character_
  }
}

author_data <- doi_data |>
  select(doi, author) |>
  filter(!map_lgl(author, is.null)) |>
  unnest(author) |>
  mutate(
    given_clean = str_replace_all(given, "\\b([A-Z])\\b(?!\\.)", "\\1."),
    name = paste(given_clean, family),
    orcid = if_else(is.na(ORCID), NA_character_, ORCID),
    orcid_suffix = map_chr(orcid, get_orcid_suffix)
  )

needs_random <- which(is.na(author_data$orcid_suffix))
n_needed <- length(needs_random)
existing_suffixes <- na.omit(author_data$orcid_suffix)
possible_suffixes <- sprintf("%04d", 0:9999)
available_suffixes <- setdiff(possible_suffixes, existing_suffixes)
set.seed(42)
random_suffixes <- sample(available_suffixes, n_needed)
author_data$orcid_suffix[needs_random] <- random_suffixes

author_tbl <- author_data |>
  mutate(authorID = paste0(str_to_lower(family), orcid_suffix)) |>
  select(authorID, authorName = name, authorOrcid = ORCID) |>
  distinct() |>
  arrange(authorID)

write_csv(author_tbl, file = "out/tables/author_tbl.csv")

author_paper_tbl <- author_data |>
  mutate(authorID = paste0(str_to_lower(family), orcid_suffix)) |>
  select(doi, authorID) |>
  distinct()

write_csv(author_paper_tbl, file = "out/tables/author_paper_tbl.csv")
rm(author_data, author_tbl, author_paper_tbl)


# ---- Build unified BLOC metadata table ----------------------------------- #

limits <- limitation_tbl |> select(blocID, themeID = limitationID)
consequence <- consequence_tbl |>
  select(blocID, themeID = consequenceID) |>
  mutate(themeID = ifelse(themeID == "other", "other consequence", themeID))
adoption <- adoptionBarrier_tbl |> select(blocID, type = barrierTypeID, themeID = adoptionBarrierID)
distrust <- distrustBarrier_tbl |>
  select(blocID, type = barrierTypeID, subtype = adoptionBarrierID, themeID = distrustBarrierID) |>
  mutate(themeID = ifelse(themeID == "Non-specified concerns", "Non-specified Distrust", themeID),
         themeID = ifelse(themeID == "Other (write)", "other distrust", themeID))
develop <- developmentBarrier_tbl |>
  select(blocID, type = barrierTypeID, themeID = developmentBarrierID) |>
  mutate(themeID = ifelse(themeID == "other", "other Dev", themeID))
opp <- opportunityType_tbl |> select(blocID, themeID = opportunityTypeID)
env <- environmentalOpportunity_tbl |>
  select(blocID, type = opportunityType, themeID = environmentOpportunityID) |>
  mutate(themeID = ifelse(themeID == "Non-specified", "Non-specified Env", themeID))
human <- humanOpportunity_tbl |> select(blocID, type = opportunityType, themeID = humanOpportunityID)
health <- healthOpportunity_tbl |>
  select(blocID, type = opportunityType, subtype = humanOpportunityID, themeID = healthOpportunityID) |>
  mutate(themeID = ifelse(themeID == "Non-specified", "Non-specified Health", themeID))

bloc <- limits |>
  full_join(consequence) |>
  full_join(adoption) |>
  full_join(develop) |>
  full_join(distrust) |>
  full_join(opp) |>
  full_join(env) |>
  full_join(human) |>
  full_join(health) |>
  select(blocID, type, subtype, themeID) |>
  arrange(themeID)

write_csv(bloc, "out/tables/blocMetadata.csv")


# ---- Match DOIs to themeIDs ---------------------------------------------- #

paper_theme_tbl <- tibble(doi = character(), value = character())

for (i in 1:nrow(bloc)) {
  theme <- as.character(bloc[i, "themeID", drop = TRUE])
  doilist <- clean |>
    select(doi = paper.doi_1, value) |>
    filter(value == theme) |>
    distinct()
  if (nrow(doilist) > 0) {
    paper_theme_tbl <- bind_rows(paper_theme_tbl, doilist)
  } else {
    message(paste0(theme, " is not yet present in the dataset."))
  }
}

write_csv(paper_theme_tbl, "out/tables/paper_theme_tbl.csv")


# ---- Final cleanup ------------------------------------------------------- #

rm(limits, consequence, adoption, develop, distrust, opp, env, human, health)
rm(paper_theme_tbl, bloc, clean, paper_tbl, doi_data)
