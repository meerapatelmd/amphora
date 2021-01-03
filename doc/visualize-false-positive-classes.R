## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "##",
                      tidy = TRUE, #`styler` to use styler:style_text() to reformat code
                      tidy.opts = list(blank = FALSE, width.cutoff = 60),
                      echo = TRUE,
                      eval = TRUE,
                      cache = TRUE,
                      # cache.path = "reports/cache/",
                      child = NULL, #file/s to knit and then include,
                      collapse = FALSE, #collapse all output into a single block,
                      error = TRUE, #display error messages in doc. FALSE stops render when error is thrown
                      include = TRUE, #include chunk?
                      message = FALSE, #display code messages?
                      tidy = TRUE, #tidy code 
                      warning = FALSE, #include warnings?
                      results = "markup"
                        # "asis": passthrough results
                        # "hide": do not display results 
                        # "hold": put all results below all code
                      )

## ----libraries----------------------------------------------------------------
library(tidyverse)
library(collapsibleTree)
library(pg13)
library(chariot)
library(amphora)

## ----results='hide'-----------------------------------------------------------
conn <- connectAthena()

## ----false_positive_concepts0,echo=FALSE,results='hide'-----------------------
false_positive_ingrs <-
tibble::tribble(~rxnorm_ingredient_name,
        "filgrastim",
        "benralizumab",
        "cyclosporine",
        "dihematoporphyrin ether",
        "eculizumab",
        "ethoglucid",
        "fosfestrol",
        "fostamatinib",
        "herpesvirus 1, human",
        "mesna",
        "pravastatin",
        "ravulizumab",
        "ruxolitinib",
        "tranexamic acid",
        "valproate",
        "levamisole",
        "methyl 5-aminolevulinate",
        "phenytoin",
        "zidovudine",
        "mepolizumab",
        "mycophenolate mofetil",
        "ketoconazole",
        "pirfenidone",
        "sargramostim",
        "octreotide",
        "quinine",
        "tacrolimus",
        "megestrol",
        "miltefosine",
        "medroxyprogesterone",
        "methylprednisolone",
        "omeprazole",
        "tocilizumab",
        "leucovorin",
        "levoleucovorin",
        "hydrocortisone",
        "cortisone",
        "danazol",
        "doxycycline",
        "estradiol",
        "dexamethasone",
        "denosumab",
        "amoxicillin",
        "aspirin",
        "azathioprine",
        "sirolimus",
        "isotretinoin",
        "diethylstilbestrol",
        "dutasteride",
        "prednisolone",
        "prednisone",
)

false_positive_concepts1 <-
join_on_concept_synonym_name(
  data = false_positive_ingrs,
  column = "rxnorm_ingredient_name",
  conn = conn
) %>%
  rename(match_concept_id = concept_id)

false_positive_concepts <-
  join_on_concept_id(
    data = false_positive_concepts1,
    column = "match_concept_id",
    where_in_concept_field = "vocabulary_id",
    where_in_concept_field_value = c("RxNorm", "RxNorm Extension"),
    where_is_null_concept_field = "invalid_reason",
    conn = conn
  ) %>%
  select(concept_id:invalid_reason)

## ----false_positive_concepts--------------------------------------------------
false_positive_concepts

## ----results='hide'-----------------------------------------------------------
false_positive_ancestors <-
join_for_ancestors(
  data = false_positive_concepts,
  descendant_id_column = "concept_id",
  where_in_concept_ancestor_field = "max_levels_of_separation",
  where_in_concept_ancestor_field_value = 1,
  conn = conn
  )

## ----false_positive_parents---------------------------------------------------
false_positive_parents <-
  false_positive_ancestors %>%
  rename_all(str_replace_all, "ancestor_", "parent_") %>%
  filter(
    parent_vocabulary_id %in% c("ATC", "HemOnc"),
    parent_standard_concept %in% c("C")
    )  %>%
  filter(!(parent_concept_class_id %in% c("ATC 5th")))
false_positive_parents

## ----atc----------------------------------------------------------------------
atc <-
false_positive_parents %>%
  rubix::split_by(col = parent_vocabulary_id) %>%
  pluck("ATC") %>%
  unite(col = false_positive_atc_class,
        parent_concept_id,
        parent_concept_name,
        sep = " ") %>%
  transmute(false_positive_atc_class,
            color = "red")
atc

## ----results='hide'-----------------------------------------------------------
atc_descendants <-
  join_for_descendants(data = tibble::tibble(top_atc_class = 21601386)) %>%
  filter(descendant_vocabulary_id %in% "ATC") %>%
  filter(!(descendant_concept_class_id %in% "ATC 5th")) %>%
  arrange(descendant_concept_class_id) %>%
  unite(col = descendant,
        descendant_concept_id,
        descendant_concept_name,
        sep = " ",
        remove = FALSE) %>%
  pivot_wider(id_cols = top_atc_class,
              names_from = descendant_concept_class_id,
              values_from = descendant,
              values_fn = list(descendant = ~ paste(unique(.), collapse = "|"))) %>%
  rubix::recode_value(col = top_atc_class,
                      "21601386 ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS" = "21601386")

## ----atc_descendants----------------------------------------------------------
atc_descendants

## -----------------------------------------------------------------------------
parent_child_fields <- 
  list(
    c("top_atc_class", "ATC 1st"),
    c("ATC 1st", "ATC 2nd"),
    c("ATC 2nd", "ATC 3rd"),
    c("ATC 3rd", "ATC 4th")
  )

output <- list()
for (i in seq_along(parent_child_fields)) {
  x <-
  atc_descendants %>%
    select(all_of(parent_child_fields[[i]])) 
  colnames(x) <- c("parent", "child")
  
  output[[i]] <- x
  
}
atc_data_tree <-
  bind_rows(output) %>%
  separate_rows(parent, sep = "[|]{1}") %>%
   separate_rows(child, sep = "[|]{1}") %>%
  mutate(color = "blue") %>%
  distinct()
atc_data_tree

## -----------------------------------------------------------------------------
atc_data_tree2 <-
atc_data_tree %>%
  left_join(atc, 
            by = c("child" = "false_positive_atc_class"),
            suffix = c(".dt", ".fp")) %>%
  transmute(parent,
            child,
            color = coalesce(color.fp, color.dt))
atc_data_tree2

## -----------------------------------------------------------------------------
atc_data_tree2$parent[1]<- NA_character_
atc_data_tree2

## ----cache=FALSE--------------------------------------------------------------
collapsibleTreeNetwork(df = atc_data_tree2,
                       fill = "color",
                       collapsed = FALSE)

## ----results='hide'-----------------------------------------------------------
dcAthena(conn = conn)

