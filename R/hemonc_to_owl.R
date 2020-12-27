#' @title
#' Convert OMOP HemOnc to Triplets
#'
#' @export

hemonc_triples <-
        function(conn,
                 conn_fun) {

                relationships <-
                        pg13::query(conn = conn,
                                    conn_fun = conn_fun,
                                    sql_statement =
                                            "
                                            WITH hemonc AS (
                                                        SELECT DISTINCT concept_id AS hemonc_id, concept_name AS hemonc_name
                                                        FROM omop_vocabulary.concept c
                                                        WHERE c.invalid_reason IS NULL
                                                                AND c.vocabulary_id = 'HemOnc'
                                                )

                                                SELECT ho.hemonc_name AS hemonc_name_1,
                                                        cr.relationship_id,
                                                        ho2.hemonc_name AS hemonc_name_2
                                                FROM omop_vocabulary.concept_relationship cr
                                                INNER JOIN hemonc ho
                                                ON ho.hemonc_id = cr.concept_id_1
                                                INNER JOIN hemonc ho2
                                                ON ho.hemonc_id = cr.concept_id_2
                                                WHERE cr.invalid_reason IS NULL
                                                ")
        }
