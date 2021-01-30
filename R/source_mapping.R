







add_source_concept <-
        function(conn,
                 source_concept_name,
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
                                                        AND vocabulary_id = 'Source'
                                                        AND concept_class_id = 'Concept'
                                                        AND LOWER(concept_name) = LOWER('@source_concept_name);",
                                                              standard_library_schema = standard_library_schema,
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
                                                         vocabulary_id = "Source",
                                                         concept_class_id = "Concept",
                                                         standard_concept = NA,
                                                         concept_code = as.character(Sys.time()),
                                                         valid_start_date = Sys.Date(),
                                                         valid_end_date = as.Date("2099-12-31"),
                                                         invalid_reason = NA),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)
        }


add_source_to_target_mapping <-
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
