remove_from_library <-
        function(conn,
                 concept_obj,
                 standard_library_schema) {


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
                                                         valid_start_date = concept_obj@valid_start_date,
                                                         valid_end_date = Sys.Date(),
                                                         invalid_reason = "R"),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)


                pg13::send(conn = conn,
                           sql_statement = SqlRender::render(
                                   "DELETE FROM @standard_library_schema.concept
                                   WHERE concept_id = '@concept_id' AND invalid_reason IS NULL",
                                   standard_library_schema = standard_library_schema,
                                   concept_id = concept_obj@concept_id))

        }


remove_relationship <-
        function(conn,
                 concept_obj1,
                 concept_obj2,
                 relationship_id,
                 inverse_relationship_id,
                 standard_library_schema) {

                valid_start_date <-
                        pg13::query(conn = conn,
                                    sql_statement =
                                            SqlRender::render("SELECT valid_start_date
                                                              FROM @standard_library_schema.concept_relationship
                                                              WHERE concept_id_1 = '@concept_id_1'
                                                                AND concept_id_2 = '@concept_id_2'
                                                                AND relationship_id = '@relationship_id'
                                                                AND invalid_reason IS NULL",
                                                              concept_id_1 = concept_obj1@concept_id,
                                                              concept_id_2 = concept_obj2@concept_id,
                                                              relationship_id = relationship_id))
                valid_start_date <- unlist(valid_start_date)

                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept_relationship",
                                   data = tibble::tibble(concept_id_1 = concept_obj1@concept_id,
                                                         concept_id_2 = concept_obj2@concept_id,
                                                         relationship_id	= relationship_id,
                                                         valid_start_date = valid_start_date,
                                                         valid_end_date = Sys.Date(),
                                                         invalid_reason = "R"),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)

                pg13::send(conn = conn,
                           sql_statement = SqlRender::render(
                                   "DELETE FROM @standard_library_schema.concept_relationship
                                   WHERE concept_id_1 = '@concept_id_1' AND concept_id_2 = '@concept_id_2' AND invalid_reason IS NULL AND relationship_id = '@relationship_id'",
                                   standard_library_schema = standard_library_schema,
                                   concept_id_1 = concept_obj1@concept_id,
                                   concept_id_2 = concept_obj2@concept_id,
                                   relationship_id = relationship_id))

                pg13::append_table(conn = conn,
                                   schema = standard_library_schema,
                                   table = "concept_relationship",
                                   data = tibble::tibble(concept_id_1 = concept_obj2@concept_id,
                                                         concept_id_2 = concept_obj1@concept_id,
                                                         relationship_id	= inverse_relationship_id,
                                                         valid_start_date = valid_start_date,
                                                         valid_end_date = Sys.Date(),
                                                         invalid_reason = "R"),
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)

                pg13::send(conn = conn,
                           sql_statement = SqlRender::render(
                                   "DELETE FROM @standard_library_schema.concept_relationship
                                   WHERE concept_id_1 = '@concept_id_1' AND concept_id_2 = '@concept_id_2' AND invalid_reason IS NULL AND relationship_id = '@relationship_id'",
                                   standard_library_schema = standard_library_schema,
                                   concept_id_1 = concept_obj2@concept_id,
                                   concept_id_2 = concept_obj1@concept_id,
                                   relationship_id = inverse_relationship_id))

        }
