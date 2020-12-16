#' @title
#' Lookup a Concept Id
#' @export
#' @rdname lookup_concept_id


lookup_concept_id <-
        function(concept_id,
                 vocab_schema,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql <-
                        SqlRender::render("SELECT *
                                                        FROM @vocab_schema.concept c
                                                        WHERE c.concept_id IN (@concept_id)
                                                      ",
                                          vocab_schema = vocab_schema,
                                          concept_id = concept_id)

                queryAthena(sql_statement = sql,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }


#' @title
#' Lookup Synonyms
#' @export
#' @rdname lookup_synonyms


lookup_synonyms <-
        function(concept_id,
                 vocab_schema,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                sql <-
                        SqlRender::render("WITH con AS (
                                                        SELECT *
                                                        FROM @vocab_schema.concept c
                                                        WHERE c.concept_id IN (@concept_id)
                                                )

                                            SELECT DISTINCT cs.concept_synonym_name
                                            FROM @vocab_schema.concept_synonym cs
                                            INNER JOIN con c
                                            ON c.concept_id = cs.concept_id
                                            WHERE c.concept_name <> cs.concept_synonym_name
                                                AND cs.language_concept_id = 4180186
                                                      ",
                                          vocab_schema = vocab_schema,
                                          concept_id = concept_id)

                queryAthena(sql_statement = sql,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }
