## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(amphora)
library(tidyverse)
conn <- pg13::local_connect()

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21600001)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21600959)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21601237)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21601386)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21601907)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21602359)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21602681)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21602795)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21603550)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21603931)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21604180)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21604847)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21605007)

## -----------------------------------------------------------------------------
 plot_atc_1st_class(conn = conn, vocab_schema = 'omop_vocabulary', atc_1st_concept = 21605212)

## -----------------------------------------------------------------------------
pg13::dc(conn = conn)

