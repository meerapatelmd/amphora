#' @title
#' Lookup a Concept Id
#'
#' @return
#' Concept Class object
#'
#' @importFrom SqlRender render
#' @export
#' @rdname get_concept

get_concept <-
        function(concept_id,
                 vocab_schema,
                 conn,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                # conn <- chariot::connectAthena()
                # concept_ids <- 1112807
                # cache_only <- FALSE
                # skip_cache <- FALSE
                # override_cache <- FALSE
                # render_sql <- FALSE
                # verbose <- FALSE
                # sleepTime <- 1
                # sql <-
                # pg13::buildQuery(schema = schema,
                #                  tableName = "concept",
                #                  whereInField = "concept_id",
                #                  whereInVector = concept_ids,
                #                  caseInsensitive = FALSE)

                df <- lookup_concept_id(concept_id = concept_id,
                                        vocab_schema = vocab_schema,
                                        conn = conn,
                                        cache_only = cache_only,
                                        skip_cache = skip_cache,
                                        override_cache = override_cache,
                                        render_sql = render_sql,
                                        verbose = verbose,
                                        sleepTime = sleepTime)


                df2 <- lookup_synonyms(concept_id = concept_id,
                                       vocab_schema = vocab_schema,
                                       conn = conn,
                                       cache_only = cache_only,
                                       skip_cache = skip_cache,
                                       override_cache = override_cache,
                                       render_sql = render_sql,
                                       verbose = verbose,
                                       sleepTime = sleepTime)


                check_concept_id(concept_id = df$concept_id)

                new(Class = "concept",
                    concept_id = df$concept_id,
                    concept_name = df$concept_name,
                    concept_synonym_names = paste(df2$concept_synonym_name, collapse = "|"),
                    domain_id = df$domain_id,
                    vocabulary_id = df$vocabulary_id,
                    concept_class_id = df$concept_class_id,
                    standard_concept = df$standard_concept,
                    concept_code = df$concept_code,
                    valid_start_date = df$valid_start_date,
                    valid_end_date = df$valid_end_date,
                    invalid_reason = df$invalid_reason)

        }
