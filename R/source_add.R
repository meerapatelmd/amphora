#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param source_concept_name PARAM_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION, Default: 'Unspecified'
#' @param concept_class_id PARAM_DESCRIPTION, Default: 'Unspecified'
#' @param standard_library_schema PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param render_only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[pg13]{c("query", "query")}},\code{\link[pg13]{append_table}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[uuid]{UUIDgenerate}}
#'  \code{\link[tibble]{tibble}}
#' @rdname source_add_concept
#' @export 
#' @importFrom pg13 query append_table
#' @importFrom SqlRender render
#' @importFrom uuid UUIDgenerate
#' @importFrom tibble tibble
source_add_concept <-
        function(conn,
                 source_concept_name,
                 vocabulary_id = "Unspecified",
                 concept_class_id = "Unspecified",
                 standard_library_schema,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                resultset <-
                        pg13::query(conn = conn,
                                    sql_statement =
                                            SqlRender::render("SELECT concept_id
                                                      FROM @standard_library_schema.concept
                                                      WHERE invalid_reason IS NULL
                                                        AND domain_id = 'Source'
                                                        AND vocabulary_id = '@vocabulary_id'
                                                        AND concept_class_id = '@concept_class_id'
                                                        AND LOWER(concept_name) = LOWER('@source_concept_name);",
                                                              standard_library_schema = standard_library_schema,
                                                              vocabulary_id = vocabulary_id,
                                                              concept_class_id = concept_class_id,
                                                              source_concept_name = source_concept_name),
                                    verbose = verbose,
                                    render_sql = render_sql,
                                    render_only = render_only)

                if (nrow(resultset) == 1) {
                        stop(sprintf("source concept '%s' already exists.", source_concept_name))
                }

                if (nrow(resultset) > 1) {
                        stop(sprintf("multiple source concept '%s' exists.", source_concept_name))
                }


                concept_id <- uuid::UUIDgenerate()
                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept",
                                   data = tibble::tibble(concept_id = concept_id,
                                                         concept_name = source_concept_name,
                                                         domain_id = "Source",
                                                         vocabulary_id = vocabulary_id,
                                                         concept_class_id = concept_class_id,
                                                         standard_concept = NA,
                                                         concept_code = as.character(Sys.time()),
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param standard_library_schema PARAM_DESCRIPTION
#' @param source_concept_obj PARAM_DESCRIPTION
#' @param target_concept_obj PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param render_only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[pg13]{append_table}}
#'  \code{\link[tibble]{tibble}}
#' @rdname source_add_map_to_target
#' @export 
#' @importFrom pg13 append_table
#' @importFrom tibble tibble
source_add_map_to_target <-
        function(conn,
                 standard_library_schema,
                 source_concept_obj,
                 target_concept_obj,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                source_id <- source_concept_obj@concept_id
                target_id <- target_concept_obj@concept_id


                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept_relationship",
                                   data = tibble::tibble(concept_id_1 = source_id,
                                                         concept_id_2 = target_id,
                                                         relationship_id	= "Maps to",
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)

                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept_relationship",
                                   data = tibble::tibble(concept_id_1 = target_id,
                                                         concept_id_2 = source_id,
                                                         relationship_id	= "Mapped from",
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)


        }
