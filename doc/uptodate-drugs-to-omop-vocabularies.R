## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE
)

## ----setup--------------------------------------------------------------------
library(amphora)
library(uptodate)
library(tidyverse)

## -----------------------------------------------------------------------------
UPTODATE <- 
  UTD_PDF_INVENTORY %>%
  left_join(UTD_CSV_INVENTORY, by = "pdf") %>%
  left_join(UTD_DRUG_DATA, by = "csv") %>%
  filter(header == "Pharmacologic Category") %>%
  select(drug, 
         pharmacologic_category = header_text) %>%
  separate_rows(pharmacologic_category,
                sep = "[;]{1} ")
rMarkedDown::print_dt(UPTODATE)

## -----------------------------------------------------------------------------
CONCEPT_A <-
  UPTODATE %>%
  distinct(drug) %>%
  transmute(concept_id = "",
    concept_name = drug,
    domain_id = "Drug",
    vocabulary_id = "UpToDate",
    concept_class_id = "Drug",
    standard_concept = NA_character_,
    concept_code = 1:length(unique(UPTODATE$drug)),
    valid_start_date = Sys.Date(),
    valid_end_date = as.Date(NA_character_),
    invalid_reason = NA_character_)

## -----------------------------------------------------------------------------
CONCEPT_B <-
  UPTODATE %>%
  distinct(pharmacologic_category) %>%
  transmute(concept_id = "",
    concept_name = pharmacologic_category,
    domain_id = "Drug",
    vocabulary_id = "UpToDate",
    concept_class_id = "Pharmacologic Category",
    standard_concept = "C",
    concept_code = 1:length(unique(UPTODATE$pharmacologic_category)),
    valid_start_date = Sys.Date(),
    valid_end_date = as.Date(NA_character_),
    invalid_reason = NA_character_)

## -----------------------------------------------------------------------------
CONCEPT <-
  bind_rows(CONCEPT_A,
            CONCEPT_B)

## -----------------------------------------------------------------------------
CONCEPT$concept_id <- uuid::UUIDgenerate(use.time = TRUE, n = nrow(CONCEPT))

## -----------------------------------------------------------------------------
CONCEPT_RELATIONSHIP <-
UPTODATE %>%
  inner_join(CONCEPT,
             by = c("drug" = "concept_name")) %>%
  select(concept_id_1 = concept_id,
         pharmacologic_category) %>%
  inner_join(CONCEPT,
             by = c("pharmacologic_category" = "concept_name")) %>%
  transmute(concept_id_1,
            relationship_id = "Is a",
            concept_id_2 = concept_id,
            valid_start_date = Sys.Date(),
            valid_end_date = as.Date(NA_character_),
            invalid_reason = NA_character_)


## -----------------------------------------------------------------------------
CONCEPT_RELATIONSHIP_INVERSE <-
  CONCEPT_RELATIONSHIP %>%
  transmute(concept_id_1 = concept_id_2,
            relationship_id = "Subsumes",
            concept_id_2 = concept_id_1,
            valid_start_date,
            valid_end_date,
            invalid_reason)

## -----------------------------------------------------------------------------
CONCEPT_RELATIONSHIP <-
  bind_rows(CONCEPT_RELATIONSHIP,
            CONCEPT_RELATIONSHIP_INVERSE)

## -----------------------------------------------------------------------------
UPTODATE_TABLES <-
  list(CONCEPT = CONCEPT,
       CONCEPT_RELATIONSHIP = CONCEPT_RELATIONSHIP) 

file <- file.path("data-raw", "UPTODATE_TABLES.xlsx")
if (!file.exists(file)) {
  broca::write_full_excel(x = UPTODATE_TABLES,
                          file = file)
}

