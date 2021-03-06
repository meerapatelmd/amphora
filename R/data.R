#' @title Antineoplastics Annotation
#' @description
#' Notes on the `False Positive` status of RxNorm Antineoplastic ingredients and
#' additional comments from OHDSI Oncologists.
#' @format A data frame with 298 rows and 6 variables:
#' \describe{
#'   \item{\code{concept_id}}{double}
#'   \item{\code{concept_name}}{character}
#'   \item{\code{vocabulary_id}}{character}
#'   \item{\code{concept_class_id}}{character}
#'   \item{\code{false_positive}}{logical}
#'   \item{\code{comment}}{character}
#'}
"antineoplastics_annotation"

#' @title mOMOP Tables
#' @description
#' mOMOP in the OMOP Vocabulary format to reflect the
#' contrived class hierarchy.
"MOMOP_TABLES"

#' @title UpToDate Drug Tables
#' @description
#' Drugs in UpToDate in the OMOP Vocabulary format, classified based on
#' their assigned Pharmacologic Category in Lexicomp.
"UPTODATE_TABLES"
