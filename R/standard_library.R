#' @title
#' Creating the Target Standard Library from OMOP Concepts
#'
#' @description
#' Filter the current OMOP vocabulary for the elements most
#' relevant to the RWD domain.
#'
#' @details
#' 1. Identify a valid Concept in the vocabulary schema. A hash
#' will be created for concepts that do not presently exist.
#' 2. Provide the class it belongs to. If the Class does not
#' currently exist, it will be made and added to the Concept table.
#' 3. Copy the Concept to the Standard Library.
#' 4. Add the "Has concept" relationship to the Class
#' 5. Add that "Concept of" relationship to the Concept






instantiate_standard_library <-
        function(conn,
                 standard_library_schema,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                if (!(pg13::schema_exists(conn = conn,
                                    schema = standard_library_schema))) {

                pg13::create_schema(conn = conn,
                                    schema = standard_library_schema,
                                    verbose = verbose,
                                    render_sql = render_sql,
                                    render_only = render_only)

                pg13::send(conn = conn,
                           sql_statement =
                                   SqlRender::render(
                                           "
                                        --HINT DISTRIBUTE ON RANDOM
                                        CREATE TABLE @schema.concept (
                                          concept_id			TEXT			NOT NULL ,
                                          concept_name			VARCHAR(255)	NOT NULL ,
                                          domain_id				VARCHAR(20)		NOT NULL ,
                                          vocabulary_id			VARCHAR(20)		NOT NULL ,
                                          concept_class_id		VARCHAR(20)		NOT NULL ,
                                          standard_concept		VARCHAR(1)		NULL ,
                                          concept_code			VARCHAR(50)		NOT NULL ,
                                          valid_start_date		DATE			NOT NULL ,
                                          valid_end_date		DATE			NOT NULL ,
                                          invalid_reason		VARCHAR(1)		NULL
                                        )
                                        ;


                                        --HINT DISTRIBUTE ON RANDOM
                                        CREATE TABLE @schema.concept_relationship (
                                          concept_id_1			TEXT			NOT NULL,
                                          concept_id_2			TEXT			NOT NULL,
                                          relationship_id		VARCHAR(20)		NOT NULL,
                                          valid_start_date		DATE			NOT NULL,
                                          valid_end_date		DATE			NOT NULL,
                                          invalid_reason		VARCHAR(1)		NULL
                                          )
                                        ;


                                        --HINT DISTRIBUTE ON RANDOM
                                        CREATE TABLE @schema.concept_ancestor (
                                          ancestor_concept_id		TEXT		NOT NULL,
                                          descendant_concept_id		TEXT		NOT NULL,
                                          min_levels_of_separation	INTEGER		NOT NULL,
                                          max_levels_of_separation	INTEGER		NOT NULL
                                        )
                                        ;

                                        --HINT DISTRIBUTE ON RANDOM
                                        CREATE TABLE @schema.source_to_concept_map (
                                          source_code				VARCHAR(50)		NOT NULL,
                                          source_concept_id			INTEGER			NOT NULL,
                                          source_vocabulary_id		VARCHAR(20)		NOT NULL,
                                          source_code_description	VARCHAR(255)	NULL,
                                          target_concept_id			INTEGER			NOT NULL,
                                          target_vocabulary_id		VARCHAR(20)		NOT NULL,
                                          valid_start_date			DATE			NOT NULL,
                                          valid_end_date			DATE			NOT NULL,
                                          invalid_reason			VARCHAR(1)		NULL
                                        )
                                        ;

                                        ",
                                           schema = standard_library_schema
                                   ))

                if (verbose) {
                        secretary::typewrite("Tables created.")
                }


                }
        }

create_classification <-
        function(class_hierarchy = list('CANCER ANATOMY' = c('NAACCR_Laterality'),
                             'CANCER CLINICAL PERFORMANCE' = c('ICDO3_Behavior'),
                             'CANCER GRADES' = c('CNS_Tumors'),
                             'CANCER STAGES' = c('AnnArbor_HodgkinLymphoma', 'Lugano_NonHodgkinLymphoma', 'NAACCR_Clinical_Stage', 'NAACCR_Path_Stage'),
                             'CANCER TYPE' = c('ICD10', 'ICDO3_Histology', 'ICDO3_Top_Topography', 'ICDO3_Topography'),
                             'DRUGS' = c('BY RXNORM INGREDIENT/GLUCOCORTICOID', 'BY RXNORM INGREDIENT/IMMUNOMODULATOR_IMIDE', 'BY RXNORM INGREDIENT/IMMUNOMODULATOR_TNFa_INHIBITOR', 'BY RXNORM INGREDIENT/IMMUNOMODULATOR', 'BY RXNORM INGREDIENT/NSAID', 'BY RXNORM INGREDIENT/STATIN', 'BY RXNORM INGREDIENT/VASOPRESSOR'),
                             'LABS' = c('BLEEDING_TIME', 'BLOOD_GASES', 'BLOOD_TYPE', 'CBC', 'CREATINE_KINASE', 'DIFFERENTIAL', 'IMMUNOGLOBULIN_PANEL', 'INFECTIOUS_DISEASE_DNA', 'INFECTIOUS_DISEASE_RNA', 'LACTATE_DEHYDROGENASE', 'LIPID_PANEL', 'METABOLIC_PANEL', 'SINGLE_ORDER_LABS', 'SPECIMEN_TYPE')),
                 conn,
                 standard_library_schema,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                for (i in seq_along(class_hierarchy)) {
                        class <- names(class_hierarchy)[i]
                        subClasses <- class_hierarchy[[i]]

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


                        for (j in seq_along(subClasses)) {
                                subClass <- subClasses[j]

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

add_omop_concept_to_library <-
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
