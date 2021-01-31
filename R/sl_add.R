#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param class_hierarchy PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
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
#' @rdname sl_add_classification
#' @export 
#' @importFrom pg13 query append_table
#' @importFrom SqlRender render
#' @importFrom uuid UUIDgenerate
#' @importFrom tibble tibble
sl_add_classification <-
        function(class_hierarchy,
                 conn,
                 standard_library_schema,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                for (i in seq_along(class_hierarchy)) {
                        class <- names(class_hierarchy)[i]
                        subClasses <- class_hierarchy[[i]]

                        class_resultset <-
                                pg13::query(conn = conn,
                                            sql_statement = SqlRender::render("SELECT * FROM @standard_library_schema.concept WHERE invalid_reason IS NULL AND standard_concept = 'C' AND concept_class_id = 'Class' AND LOWER(concept_name) = LOWER('@class');", standard_library_schema = standard_library_schema,
                                                                              class = class))


                        if (nrow(class_resultset) == 0) {

                                class_concept_id <- uuid::UUIDgenerate()
                                pg13::append_table(conn = conn,
                                                   schema = standard_library_schema,
                                                   table = "concept",
                                                   data = tibble::tibble(concept_id = class_concept_id,
                                                                         concept_name = class,
                                                                         domain_id = "Standard Library",
                                                                         vocabulary_id = "Amphora",
                                                                         concept_class_id = "Class",
                                                                         standard_concept = "C",
                                                                         concept_code = sprintf("00%s",i),
                                                                         valid_start_date = Sys.Date(),
                                                                         valid_end_date = as.Date("2099-12-31"),
                                                                         invalid_reason = NA),
                                                   verbose = verbose,
                                                   render_sql = render_sql,
                                                   render_only = render_only)

                                pg13::append_table(conn = conn,
                                                   schema = standard_library_schema,
                                                   table = "concept_ancestor",
                                                   data = tibble::tibble(ancestor_concept_id = class_concept_id,
                                                                         descendant_concept_id = class_concept_id,
                                                                         min_levels_of_separation = 0,
                                                                         max_levels_of_separation = 0),
                                                   verbose = verbose,
                                                   render_sql = render_sql,
                                                   render_only = render_only)

                        } else if (nrow(class_resultset) > 1) {
                                stop(sprintf("%s '%s' classes found and there can only be one", nrow(class_resultset), class))
                        } else {
                                class_concept_id <- class_resultset$concept_id
                        }


                        for (j in seq_along(subClasses)) {
                                subClass <- subClasses[j]

                                subclass_resultset <-
                                        pg13::query(conn = conn,
                                                    sql_statement = SqlRender::render("SELECT * FROM @standard_library_schema.concept WHERE invalid_reason IS NULL AND standard_concept = 'C' AND concept_class_id = 'Class' AND LOWER(concept_name) = LOWER('@subClass');", standard_library_schema = standard_library_schema,
                                                                                      subClass = subClass))
                                if (nrow(subclass_resultset) == 0) {
                                        subclass_concept_id <- uuid::UUIDgenerate()
                                        pg13::append_table(conn = conn,
                                                           schema = standard_library_schema,
                                                           table = "concept",
                                                           data = tibble::tibble(concept_id = subclass_concept_id,
                                                                                 concept_name = subClass,
                                                                                 domain_id = "Standard Library",
                                                                                 vocabulary_id = "Amphora",
                                                                                 concept_class_id = "subClass",
                                                                                 standard_concept = "C",
                                                                                 concept_code = sprintf("00%s_00%s",i, j),
                                                                                 valid_start_date = Sys.Date(),
                                                                                 valid_end_date = as.Date("2099-12-31"),
                                                                                 invalid_reason = NA),
                                                           verbose = verbose,
                                                           render_sql = render_sql,
                                                           render_only = render_only)
                                } else if (nrow(subclass_resultset) > 1) {
                                        stop(sprintf("%s '%s' subClass found and there can only be one", nrow(subclass_resultset), subclass))
                                } else {
                                        subclass_concept_id <- subclass_resultset$concept_id
                                }

                                pg13::append_table(conn = conn,
                                                   schema = standard_library_schema,
                                                   table = "concept_relationship",
                                                   data = tibble::tibble(concept_id_1 = class_concept_id,
                                                                         concept_id_2 = subclass_concept_id,
                                                                         relationship_id	= "Subsumes",
                                                                         valid_start_date = Sys.Date(),
                                                                         valid_end_date = as.Date("2099-12-31"),
                                                                         invalid_reason = NA),
                                                   verbose = verbose,
                                                   render_sql = render_sql,
                                                   render_only = render_only)


                                pg13::append_table(conn = conn,
                                                   schema = standard_library_schema,
                                                   table = "concept_relationship",
                                                   data = tibble::tibble(concept_id_1 = subclass_concept_id,
                                                                         concept_id_2 = class_concept_id,
                                                                         relationship_id	= "Is a",
                                                                         valid_start_date = Sys.Date(),
                                                                         valid_end_date = as.Date("2099-12-31"),
                                                                         invalid_reason = NA),
                                                   verbose = verbose,
                                                   render_sql = render_sql,
                                                   render_only = render_only)

                                pg13::append_table(conn = conn,
                                                   schema = standard_library_schema,
                                                   table = "concept_ancestor",
                                                   data = tibble::tibble(ancestor_concept_id = class_concept_id,
                                                                         descendant_concept_id = subclass_concept_id,
                                                                         min_levels_of_separation = 1,
                                                                         max_levels_of_separation = 1),
                                                   verbose = verbose,
                                                   render_sql = render_sql,
                                                   render_only = render_only)
                        }

                }


        }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param concept_obj PARAM_DESCRIPTION
#' @param subclass PARAM_DESCRIPTION
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
#'  \code{\link[tibble]{tibble}}
#' @rdname sl_add_omop_concept
#' @export 
#' @importFrom pg13 query append_table
#' @importFrom SqlRender render
#' @importFrom tibble tibble
sl_add_omop_concept <-
        function(conn,
                 concept_obj,
                 subclass,
                 standard_library_schema,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                subclass_resultset <-
                        pg13::query(conn = conn,
                                    sql_statement =
                                            SqlRender::render("SELECT concept_id
                                                      FROM @standard_library_schema.concept
                                                      WHERE invalid_reason IS NULL
                                                        AND standard_concept = 'C'
                                                        AND concept_class_id = 'subClass'
                                                        AND LOWER(concept_name) = LOWER('@subclass');",
                                                              standard_library_schema = standard_library_schema,
                                                              subclass = subclass),
                                    verbose = verbose,
                                    render_sql = render_sql,
                                    render_only = render_only)

                if (nrow(subclass_resultset) == 0) {
                        stop(sprintf("subclass '%s' does not exist.", subclass))
                }


                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept",
                                   data = tibble::tibble(concept_id = concept_obj@concept_id,
                                                         concept_name = concept_obj@concept_name,
                                                         domain_id = "Standard Library",
                                                         vocabulary_id = concept_obj@vocabulary_id,
                                                         concept_class_id = concept_obj@concept_class_id,
                                                         standard_concept = concept_obj@standard_concept,
                                                         concept_code = concept_obj@concept_code,
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)


                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept_relationship",
                                   data = tibble::tibble(concept_id_1 = subclass_resultset$concept_id,
                                                         concept_id_2 = concept_obj@concept_id,
                                                         relationship_id	= "Has concept",
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)

                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept_relationship",
                                   data = tibble::tibble(concept_id_1 = concept_obj@concept_id,
                                                         concept_id_2 = subclass_resultset$concept_id,
                                                         relationship_id	= "Concept of",
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
#' @param concept_name PARAM_DESCRIPTION
#' @param subclass PARAM_DESCRIPTION
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
#' @rdname sl_add_new_concept
#' @export 
#' @importFrom pg13 query append_table
#' @importFrom SqlRender render
#' @importFrom uuid UUIDgenerate
#' @importFrom tibble tibble
sl_add_new_concept <-
        function(conn,
                 concept_name,
                 subclass,
                 standard_library_schema,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                subclass_resultset <-
                        pg13::query(conn = conn,
                                    sql_statement =
                                            SqlRender::render("SELECT concept_id
                                                      FROM @standard_library_schema.concept
                                                      WHERE invalid_reason IS NULL
                                                        AND standard_concept = 'C'
                                                        AND concept_class_id = 'subClass'
                                                        AND LOWER(concept_name) = LOWER('@subclass');",
                                                              standard_library_schema = standard_library_schema,
                                                              subclass = subclass),
                                    verbose = verbose,
                                    render_sql = render_sql,
                                    render_only = render_only)

                if (nrow(subclass_resultset) == 0) {
                        stop(sprintf("subclass '%s' does not exist.", subclass))
                }

                concept_id <- uuid::UUIDgenerate()
                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept",
                                   data = tibble::tibble(concept_id = concept_id,
                                                         concept_name = concept_name,
                                                         domain_id = "Standard Library",
                                                         vocabulary_id = "Amphora",
                                                         concept_class_id = "Concept",
                                                         standard_concept = NA,
                                                         concept_code = as.character(Sys.time()),
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)


                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept_relationship",
                                   data = tibble::tibble(concept_id_1 = subclass_resultset$concept_id,
                                                         concept_id_2 = concept_id,
                                                         relationship_id	= "Has concept",
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)

                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept_relationship",
                                   data = tibble::tibble(concept_id_1 = concept_id,
                                                         concept_id_2 = subclass_resultset$concept_id,
                                                         relationship_id	= "Concept of",
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)
        }
