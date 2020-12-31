#' @title
#' Print Class Hierarchy for Protege
#'
#' @description
#' Print the HemOnc Class Hierarchy to the console to copy-and-paste into Protege's "owl:Thing". To write an Excel to import using Cellphie instead, see \code{\link{excel_hemonc_class_hierarchy}}.
#'
#' @importFrom pg13 query
#' @importFrom tidyr pivot_wider unite
#' @importFrom dplyr left_join
#' @export
#' @rdname print_hemonc_class_hierarchy

print_hemonc_class_hierarchy <-
        function(conn,
                 conn_fun) {

                hemonc_class_ancestry <-
                pg13::query(conn = conn,
                           conn_fun = conn_fun,
                           sql_statement =
                                   "SELECT DISTINCT
                                                        c.concept_id AS hemonc_ancestor_id,
                                                        c.concept_name AS hemonc_ancestor_name,
                                                        ca.max_levels_of_separation AS level_of_separation,
                                                        c2.concept_id AS hemonc_descendant_id,
                                                        c2.concept_name AS hemonc_descendant_name
                                                FROM omop_vocabulary.concept_ancestor ca
                                                INNER JOIN omop_vocabulary.concept c
                                                ON c.concept_id = ca.ancestor_concept_id
                                                INNER JOIN omop_vocabulary.concept c2
                                                ON c2.concept_id = ca.descendant_concept_id
                                                WHERE  c.invalid_reason IS NULL
                                                        AND c.vocabulary_id = 'HemOnc'
                                                        AND c2.invalid_reason IS NULL
                                                        AND c2.vocabulary_id = 'HemOnc'
                                                        --AND c.standard_concept = 'C'
                                                        --AND c2.standard_concept = 'C'
                                                        AND c.domain_id = 'Drug'
                                                        AND c2.domain_id = 'Drug'
                                                        AND c.concept_name <> c2.concept_name
                                            ;
                                                ") %>%
                        tidyr::unite(col = "hemonc_ancestor",
                                     hemonc_ancestor_id,
                                     hemonc_ancestor_name,
                                     sep = " ") %>%
                        tidyr::unite(col = "hemonc_descendant",
                                     hemonc_descendant_id,
                                     hemonc_descendant_name,
                                     sep = " ") %>%
                        tidyr::pivot_wider(id_cols = hemonc_ancestor,
                                           names_from = level_of_separation,
                                           values_from = hemonc_descendant,
                                           values_fn = list(hemonc_descendant = ~paste(unique(.), collapse = "|")))

                hemonc_top_classes <-
                pg13::query(
                        conn = conn,
                        conn_fun = conn_fun,
                        sql_statement =
                                SqlRender::render(
                                        "
                                WITH ancestry AS (
                                    SELECT DISTINCT ca.ancestor_concept_id, ca.descendant_concept_id
                                    FROM @vocab_schema.concept c
                                    INNER JOIN @vocab_schema.concept_ancestor ca
                                    ON ca.ancestor_concept_id = c.concept_id
                                    INNER JOIN @vocab_schema.concept c2
                                    ON ca.descendant_concept_id = c2.concept_id
                                    WHERE
                                    c.vocabulary_id IN ('@vocabulary_id')
                                    AND c.standard_concept = 'C'
                                    AND c.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                    AND c2.standard_concept = 'C'
                                    AND c2.vocabulary_id IN ('@vocabulary_id')
                                    AND c.domain_id = '@domain_id'
                                    AND c2.domain_id = '@domain_id'
                                    AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                    AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                )

                            SELECT DISTINCT c.concept_id AS hemonc_ancestor_id, c.concept_name AS hemonc_ancestor_name
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.ancestor_concept_id NOT IN (
                                SELECT a2.descendant_concept_id
                                FROM ancestry a2);",
                                        vocab_schema = "omop_vocabulary",
                                        vocabulary_id = "HemOnc",
                                        domain_id = "Drug")) %>%
                        tidyr::unite(col = "hemonc_ancestor",
                                     hemonc_ancestor_id,
                                     hemonc_ancestor_name,
                                     sep = " ")

                output <-
                        hemonc_top_classes %>%
                        dplyr::left_join(hemonc_class_ancestry,
                                         by = "hemonc_ancestor")


                for (i in 1:nrow(output)) {
                        cat(sprintf("%s\n", output$hemonc_ancestor[i]))
                        cat(sprintf("\t%s\n", unlist(strsplit(x = output$`1`[i], split = "[|]{1}"))))
                        cat(sprintf("\t\t%s\n", unlist(strsplit(x = output$`2`[i], split = "[|]{1}"))))
                        cat(sprintf("\t\t\t%s\n", unlist(strsplit(x = output$`3`[i], split = "[|]{1}"))))
                        cat(sprintf("\t\t\t\t%s\n", unlist(strsplit(x = output$`4`[i], split = "[|]{1}"))))
                        #cat(sprintf("\t\t\t\t\t%s\n", unlist(strsplit(x = output$`5`[i], split = "[|]{1}"))))
                }
        }


#' @title
#' Write Excel of HemOnc's Class Hierarchy for Protege
#'
#' @description
#' To print the HemOnc Class Hierarchy to the console to copy-and-paste into Protege's "owl:Thing", see \code{\link{print_hemonc_class_hierarchy}}.
#'
#' @importFrom pg13 query
#' @importFrom tidyr unite
#' @importFrom openxlsx write.xlsx
#' @export
#' @rdname excel_hemonc_class_hierarchy

excel_hemonc_class_hierarchy <-
        function(conn,
                 conn_fun,
                 file = "hemonc_class_hierarchy.xlsx") {

                hemonc_class_ancestry <-
                        pg13::query(conn = conn,
                                    conn_fun = conn_fun,
                                    sql_statement =
                                            "SELECT DISTINCT
                                                        c.concept_id AS hemonc_ancestor_id,
                                                        c.concept_name AS hemonc_ancestor_name,
                                                        --ca.max_levels_of_separation AS level_of_separation,
                                                        c2.concept_id AS hemonc_descendant_id,
                                                        c2.concept_name AS hemonc_descendant_name
                                                FROM omop_vocabulary.concept_ancestor ca
                                                INNER JOIN omop_vocabulary.concept c
                                                ON c.concept_id = ca.ancestor_concept_id
                                                INNER JOIN omop_vocabulary.concept c2
                                                ON c2.concept_id = ca.descendant_concept_id
                                                WHERE  c.invalid_reason IS NULL
                                                        AND c.vocabulary_id = 'HemOnc'
                                                        AND c2.invalid_reason IS NULL
                                                        AND c2.vocabulary_id = 'HemOnc'
                                                        --AND c.standard_concept = 'C'
                                                        --AND c2.standard_concept = 'C'
                                                        AND c.domain_id = 'Drug'
                                                        AND c2.domain_id = 'Drug'
                                                        AND c.concept_name <> c2.concept_name
                                                        AND ca.max_levels_of_separation = 1
                                            ;
                                                ") %>%
                        tidyr::unite(col = "hemonc_ancestor",
                                     hemonc_ancestor_id,
                                     hemonc_ancestor_name,
                                     sep = " ") %>%
                        tidyr::unite(col = "hemonc_descendant",
                                     hemonc_descendant_id,
                                     hemonc_descendant_name,
                                     sep = " ")


                openxlsx::write.xlsx(x = hemonc_class_ancestry,
                                     file = file)
        }






#' @title
#' Convert OMOP HemOnc to Triplets
#'
#' @export

hemonc_triples <-
        function(conn,
                 conn_fun,
                 write_schema = "hemonc",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                pg13::drop_cascade(conn = conn,
                                  conn_fun = conn_fun,
                                  schema = write_schema)

                pg13::create_schema(conn = conn,
                                    conn_fun = conn_fun,
                                    schema = write_schema,
                                    verbose = verbose,
                                    render_sql = render_sql,
                                    render_only = render_only)



                 pg13::send(conn = conn,
                                    conn_fun = conn_fun,
                                    sql_statement =
                                            "
                                            DROP TABLE IF EXISTS hemonc.hemonc;

                                            CREATE TABLE hemonc.hemonc AS (
                                                SELECT DISTINCT c.concept_name AS hemonc_name_1,
                                                        cr.relationship_id AS relationship_id,
                                                        c2.concept_name AS hemonc_name_2
                                                FROM omop_vocabulary.concept_relationship cr
                                                INNER JOIN omop_vocabulary.concept c
                                                ON c.concept_id = cr.concept_id_1
                                                INNER JOIN omop_vocabulary.concept c2
                                                ON c2.concept_id = cr.concept_id_2
                                                WHERE cr.invalid_reason IS NULL
                                                        AND c.invalid_reason IS NULL
                                                        AND c.vocabulary_id = 'HemOnc'
                                                        AND c2.invalid_reason IS NULL
                                                        AND c2.vocabulary_id = 'HemOnc'
                                            );
                                                ")


                 hemonc <-
                         pg13::read_table(conn = conn,
                                          conn_fun = conn_fun,
                                          schema = "hemonc",
                                          table = "hemonc",
                                          verbose = verbose,
                                          render_sql = render_sql,
                                          render_only = render_only) %>%
                         dplyr::mutate_all(stringr::str_replace_all, " ", "_")

                 base <- "http://www.semanticweb.org/meerapatel/ontologies/2020/11/hemonc"
                 hemonc_rdf <- rdflib::rdf()
                 for (i in 1:nrow(hemonc)) {
                         hemonc_rdf %>%
                                 rdflib::rdf_add(
                                         subject = sprintf("%s#%s", base, hemonc$hemonc_name_1[i]),
                                         predicate = sprintf("%s#%s", base, hemonc$relationship_id[i]),
                                         object = hemonc$hemonc_name_2[i]
                                 )
                 }

                 rdflib::rdf_serialize(rdf = hemonc_rdf,
                                       format = "turtle",
                                       doc = "test.ttl",
                                       base = base)


                 test_input <- rdf_parse("test.rdf")

                 hemonc_pivoted <-
                        pg13::read_table(conn = conn,
                                         conn_fun = conn_fun,
                                         schema = "hemonc",
                                         table = "hemonc",
                                         verbose = verbose,
                                         render_sql = render_sql,
                                         render_only = render_only) %>%
                         tidyr::pivot_wider(id_cols = hemonc_name_1,
                                            names_from = relationship_id,
                                            values_from = hemonc_name_2,
                                            values_fn = list(hemonc_name_2 = ~paste(unique(.), collapse = "|"))) %>%
                         rubix::format_colnames()

                 fields <- colnames(hemonc_pivoted)
                 fields <- fields[!(fields %in% "hemonc_name_1")]
                 output <- list()
                 for (i in seq_along(fields)) {
                         output[[i]] <-
                         tidyr::separate_rows(hemonc_pivoted,
                                              fields[i],
                                              sep = "[|]{1}") %>%
                                 dplyr::select(hemonc_name_1, dplyr::all_of(fields[i])) %>%
                                 dplyr::distinct()
                         names(output)[i] <- fields[i]

                 }

                 for (i in seq_along(output)) {

                         pg13::write_table(conn = conn,
                                           conn_fun = conn_fun,
                                           schema = "hemonc",
                                           table = names(output)[i],
                                           data = output[[i]])
                 }

                 pg13::send(conn = conn,
                            conn_fun = conn_fun,
                            sql_statement =
                                    "DROP TABLE IF EXISTS hemonc.hemonc;")



                base <- "http://www.semanticweb.org/patelm9/ontologies/2020/1/hemonc#"

                relationships2 <-
                        relationships %>%
                        dplyr::transmute(subject = sprintf("%s%s", base, hemonc_name_1),
                                         predicate = sprintf("%s%s", base, stringr::str_replace_all(relationship_id,
                                                                                                     pattern = "[ ]{1}",
                                                                                                     replacement = ".")),
                                         object = hemonc_name_2)

                library(rdflib)
                hemonc_owl <- rdf()

                for (i in 1:nrow(relationships2)) {

                        hemonc_owl %>%
                                rdf_add(
                                        subject = relationships2$subject[i],
                                        predicate = relationships2$predicate[i],
                                        object = relationships2$object[i]
                                )


                }

        }
