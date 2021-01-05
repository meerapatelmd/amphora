## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE
)

## ----setup--------------------------------------------------------------------
library(amphora)
library(mOMOP)
library(tidyverse)

## -----------------------------------------------------------------------------
MCODE_CLASS_HIERARCHY

## -----------------------------------------------------------------------------
MCODE_CLASS_HIERARCHY2 <-
  MCODE_CLASS_HIERARCHY %>%
  mutate_at(vars(class), stringr::str_replace_all, "_", " ") %>%
  mutate_at(vars(class), stringr::str_to_title)
unique(MCODE_CLASS_HIERARCHY2$class)

## -----------------------------------------------------------------------------
CONCEPT_A <-
  MCODE_CLASS_HIERARCHY2 %>%
  select(class) %>%
  distinct() %>%
  mutate(concept_id = uuid::UUIDgenerate(n = length(unique(MCODE_CLASS_HIERARCHY2$class))),
         concept_code = length(unique(MCODE_CLASS_HIERARCHY2$class))) %>%
  transmute(
    concept_id,
    concept_name = class,
    domain_id = "Cancer Modifier",
    vocabulary_id = "mOMOP",
    concept_class_id = "Class",
    standard_concept = "C",
    concept_code,
    valid_start_date = Sys.Date(),
    valid_end_date = as.Date(NA_character_),
    invalid_reason = NA_character_)
rMarkedDown::print_dt(CONCEPT_A)

## -----------------------------------------------------------------------------
CONCEPT_RELATIONSHIP <-
CONCEPT_A %>%
  transmute(concept_id_1 = concept_id,
            class = concept_name) %>%
  inner_join(MCODE_CLASS_HIERARCHY2,
            by = "class") %>%
  chariot::unmerge_strip(strip_col = concept,
                         remove = FALSE) %>%
  mutate(concept_id_2 = coalesce(concept_id, concept)) %>%
  transmute(concept_id_1,
             relationship_id = "Subsumes",
             concept_id_2,
             valid_start_date = Sys.Date(),
             valid_end_date = as.Date(NA_character_),
             invalid_reason = NA_character_)

## -----------------------------------------------------------------------------
CONCEPT_RELATIONSHIP_INVERSE <-
  CONCEPT_RELATIONSHIP %>%
  transmute(concept_id_1 = concept_id_2,
            relationship_id = "Is a",
            concept_id_2 = concept_id_1,
            valid_start_date,
            valid_end_date,
            invalid_reason)

## ----message='hide'-----------------------------------------------------------
CONCEPT_B <-
chariot::join_on_concept_id(
  data = CONCEPT_RELATIONSHIP %>%
            mutate_at(vars(concept_id_2), as.integer),
  column = "concept_id_2"
) %>%
  select(concept_id:last_col())

## -----------------------------------------------------------------------------
CONCEPT <-
  bind_rows(CONCEPT_A %>%
              mutate_all(as.character),
            CONCEPT_B %>%
              mutate_all(as.character))

## -----------------------------------------------------------------------------
CONCEPT_RELATIONSHIP <-
  bind_rows(CONCEPT_RELATIONSHIP,
            CONCEPT_RELATIONSHIP_INVERSE)

## -----------------------------------------------------------------------------
MOMOP_TABLES <-
  list(CONCEPT = CONCEPT,
       CONCEPT_RELATIONSHIP = CONCEPT_RELATIONSHIP) 

file <- file.path("data-raw", "MOMOP_TABLES.xlsx")
if (!file.exists(file)) {
  broca::write_full_excel(x = MOMOP_TABLES,
                          file = file)
}

