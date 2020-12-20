#' @title
#' Plot ATC 1st
#'
#' @description
#' Plot the hierarchy of an ATC 1st Class.
#'
#' @export
#' @rdname plot_atc_1st_class

plot_atc_1st_class <-
        function(conn,
                 vocab_schema = "omop_vocabulary",
                 atc_1st_concept) {

                if (class(atc_1st_concept) != "concept") {

                        atc_class_concept_obj <-
                        get_concept(concept_id = atc_1st_concept,
                                    vocab_schema = vocab_schema,
                                    conn = conn)

                } else {

                        atc_class_concept_obj <- atc_1st_concept
                }

                if (!(atc_class_concept_obj@vocabulary_id %in% "ATC")) {

                        stop("source vocabulary is not ATC")

                }

                if (!(atc_class_concept_obj@concept_class_id %in% "ATC 1st")) {

                        stop("concept class is not 'ATC 1st'")

                }


                atc_class_id <- atc_class_concept_obj@concept_id

                atc_classification <-
                queryAthena(
                        sql_statement =
                        SqlRender::render(
                        "
                        WITH atc AS (
                                SELECT *
                                FROM omop_vocabulary.concept c
                                WHERE c.vocabulary_id = 'ATC'
                                        AND c.invalid_reason IS NULL
                                        AND c.standard_concept = 'C'
                        )

                        SELECT
                                ca.max_levels_of_separation AS level_of_separation,
                                atc.concept_id AS ancestor_concept_id,
                                atc.concept_name AS ancestor_concept_name,
                                atc.domain_id AS ancestor_domain_id,
                                atc.vocabulary_id AS ancestor_vocabulary_id,
                                atc.concept_class_id AS ancestor_concept_class_id,
                                atc.standard_concept AS ancestor_standard_concept,
                                atc.concept_code AS ancestor_concept_code,
                                atc.valid_start_date AS ancestor_valid_start_date,
                                atc.valid_end_date AS ancestor_valid_end_date,
                                atc.invalid_reason AS ancestor_invalid_reason,
                                a2.concept_id AS descendant_concept_id,
                                a2.concept_name AS descendant_concept_name,
                                a2.domain_id AS descendant_domain_id,
                                a2.vocabulary_id AS descendant_vocabulary_id,
                                a2.concept_class_id AS descendant_concept_class_id,
                                a2.standard_concept AS descendant_standard_concept,
                                a2.concept_code AS descendant_concept_code,
                                a2.valid_start_date AS descendant_valid_start_date,
                                a2.valid_end_date AS descendant_valid_end_date,
                                a2.invalid_reason AS descendant_invalid_reason
                        FROM omop_vocabulary.concept_ancestor ca
                        INNER JOIN atc
                        ON atc.concept_id = ca.ancestor_concept_id
                        INNER JOIN atc a2
                        ON a2.concept_id = ca.descendant_concept_id
                        WHERE atc.concept_class_id = 'ATC 1st'
                                AND atc.concept_id = @atc_class_id
                        ", atc_class_id = atc_class_id
                        )
                )


                rxnorm_ingredients <-
                        queryAthena(
                                sql_statement =
                                        SqlRender::render(
                                        "
                        WITH atc AS (
                                SELECT *
                                FROM omop_vocabulary.concept c
                                WHERE c.vocabulary_id = 'ATC'
                                        AND c.invalid_reason IS NULL
                                        AND c.standard_concept = 'C'
                                        AND c.concept_class_id = 'ATC 1st'
                                        AND c.concept_id = @atc_class_id
                        ),
                        rxnorm AS (
                                SELECT *
                                FROM omop_vocabulary.concept c
                                WHERE c.vocabulary_id IN ('RxNorm', 'RxNorm Extension')
                                        AND c.invalid_reason IS NULL
                                        AND c.concept_class_id = 'Ingredient'
                        )

                        SELECT
                                5 AS level_of_separation,
                                atc.concept_id AS ancestor_concept_id,
                                atc.concept_name AS ancestor_concept_name,
                                atc.domain_id AS ancestor_domain_id,
                                atc.vocabulary_id AS ancestor_vocabulary_id,
                                atc.concept_class_id AS ancestor_concept_class_id,
                                atc.standard_concept AS ancestor_standard_concept,
                                atc.concept_code AS ancestor_concept_code,
                                atc.valid_start_date AS ancestor_valid_start_date,
                                atc.valid_end_date AS ancestor_valid_end_date,
                                atc.invalid_reason AS ancestor_invalid_reason,
                                rx.concept_id AS descendant_concept_id,
                                rx.concept_name AS descendant_concept_name,
                                rx.domain_id AS descendant_domain_id,
                                rx.vocabulary_id AS descendant_vocabulary_id,
                                rx.concept_class_id AS descendant_concept_class_id,
                                rx.standard_concept AS descendant_standard_concept,
                                rx.concept_code AS descendant_concept_code,
                                rx.valid_start_date AS descendant_valid_start_date,
                                rx.valid_end_date AS descendant_valid_end_date,
                                rx.invalid_reason AS descendant_invalid_reason
                        FROM omop_vocabulary.concept_ancestor ca
                        INNER JOIN atc
                        ON atc.concept_id = ca.ancestor_concept_id
                        INNER JOIN rxnorm rx
                        ON rx.concept_id = ca.descendant_concept_id
                        ",
                                        atc_class_id = atc_class_id
                        )
                        )

                final <-
                        dplyr::bind_rows(atc_classification,
                                         rxnorm_ingredients) %>%
                        chariot::merge_strip(into = "ancestor",
                                             prefix = "ancestor_") %>%
                        chariot::merge_strip(into = "descendant",
                                             prefix = "descendant_") %>%
                        dplyr::filter(level_of_separation != 0) %>%
                        dplyr::distinct()


                final2 <-
                        final %>%
                        tidyr::pivot_wider(id_cols = ancestor,
                                           names_from = level_of_separation,
                                           values_from = descendant,
                                           values_fn = function(x) paste(x, collapse = "|")) %>%
                        dplyr::select(ancestor,
                                      dplyr::all_of(as.character(1:3)))


                final3 <-
                        final2 %>%
                        tidyr::separate_rows(`1`, sep = "[|]{1}")


                final4 <-
                        final3 %>%
                        tidyr::separate_rows(`2`, sep = "[|]{1}")


                final5 <-
                        final4 %>%
                        tidyr::separate_rows(`3`, sep = "[|]{1}") %>%
                        dplyr::distinct()


                collapsibleTree::collapsibleTree(df = final5,
                                                 hierarchy = c("ancestor",
                                                               "1",
                                                               "2",
                                                               "3"),
                                                 linkLength = 1000,
                                                 collapsed = FALSE,
                                                 zoomable = TRUE,
                                                 width = 15000,
                                                 height = 20000)

        }




#' @title
#' Preview an ATC Class's Descendants
#'
#' @description
#' The ATC Classification includes the interface between the ATC 5th Concept Class and the RxNorm Ingredient Concept Class. While all 5 concept classes belonging to ATC have a level of separation of 1, the ATC 5th Concept Class and RxNorm Ingredient have a level of separation of 0, but it is treated as 1 for this use case.
#'
#' Due to the high amount of records, used to determine the appropriate range based on row counts per level to supply the optional `range` argument for \code{\link{plot_atc_classification}}, where this function is called again and can be optionally filtered on a numeric range before plotting.
#'
#' @export
#' @rdname preview_atc_classification

preview_atc_classification <-
        function(conn,
                 concept_class_obj,
                 vocab_schema = "omop_vocabulary",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1) {


                if (is.concept(concept_class_obj)) {

                        if (concept_class_obj@standard_concept %in% "C" &&
                            concept_class_obj@vocabulary_id %in% "ATC" &&
                            is.na(concept_class_obj@invalid_reason)) {

                        concept_id <- concept_class_obj@concept_id
                        } else {
                                stop("`concept_class_obj` is not a valid ATC Class")
                        }

                } else {

                        stop("`concept_class_obj` must be a concept class object")

                }

                domain_id <- "Drug"
                vocabulary_id <- "ATC"
                child <- domain_id



                level_1 <-
                        queryAthena(sql_statement =
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

                            SELECT DISTINCT c.*
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.ancestor_concept_id IN (@concept_id);",
                                                    vocab_schema = vocab_schema,
                                                    vocabulary_id = vocabulary_id,
                                                    domain_id = domain_id,
                                                    concept_id = concept_id),
                                    conn = conn,
                                    skip_cache = TRUE,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime
                        )

                stopifnot(nrow(level_1) > 0)

                level_1 <-
                        level_1 %>%
                        dplyr::mutate(parent = child) %>%
                        tidyr::unite(col = child,
                                     concept_id,
                                     concept_name,
                                     sep = " ",
                                     na.rm = TRUE,
                                     remove = FALSE) %>%
                        dplyr::select(parent,
                                      child,
                                      dplyr::everything())

                range_output <- list()
                range_output[[1]] <- level_1


                range <- 1:100
                for (i in 2:max(range)) {


                        new_parents <-
                                range_output[[i-1]] %>%
                                dplyr::select(concept_id) %>%
                                dplyr::distinct() %>%
                                unlist() %>%
                                as.integer()

                        level_n <-
                                chariot::queryAthena(sql_statement =
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
                                        --AND c2.standard_concept = 'C'
                                        AND c2.vocabulary_id IN ('@vocabulary_id')
                                        AND c.domain_id = '@domain_id'
                                        AND c2.domain_id = '@domain_id'
                                        AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                        AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                    )

                                    SELECT DISTINCT
                                        CONCAT(parent.concept_id, ' ', parent.concept_name) AS parent,
                                        CONCAT(child.concept_id, ' ', child.concept_name) AS child,
                                        child.*
                                    FROM ancestry a
                                    LEFT JOIN @vocab_schema.concept parent
                                    ON a.ancestor_concept_id = parent.concept_id
                                    LEFT JOIN @vocab_schema.concept child
                                    ON a.descendant_concept_id = child.concept_id
                                    WHERE a.ancestor_concept_id IN (@new_parents)
                                    ;",
                                                            vocab_schema = vocab_schema,
                                                            vocabulary_id = vocabulary_id,
                                                            domain_id = domain_id,
                                                            new_parents = new_parents),
                                            conn = conn,
                                            verbose = verbose,
                                            render_sql = render_sql
                                )


                        if (nrow(level_n) == 0) {

                                break()

                        } else {
                                range_output[[i]] <- level_n
                        }

                }

                # Get ATC 5th to RxNorm Ingredient
                atc_5th <-
                        dplyr::bind_rows(range_output) %>%
                        dplyr::filter(concept_class_id %in% "ATC 5th")


                last_level <-
                        queryAthena(sql_statement =
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
                                        c.vocabulary_id IN ('ATC')
                                        AND c.concept_class_id IN ('ATC 5th')
                                        AND c.standard_concept = 'C'
                                        AND c.invalid_reason IS NULL
                                        AND c2.invalid_reason IS NULL
                                        --AND c2.standard_concept = 'C'
                                        AND c2.vocabulary_id IN ('RxNorm', 'RxNorm Extension')
                                        AND c2.concept_class_id IN ('Ingredient')
                                        AND c.domain_id = 'Drug'
                                        AND c2.domain_id = 'Drug'
                                        AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                        AND ca.min_levels_of_separation = 0 AND ca.max_levels_of_separation = 0
                                    )

                                    SELECT DISTINCT
                                        CONCAT(parent.concept_id, ' ', parent.concept_name) AS parent,
                                        CONCAT(child.concept_id, ' ', child.concept_name) AS child,
                                        child.*
                                    FROM ancestry a
                                    INNER JOIN @vocab_schema.concept parent
                                    ON a.ancestor_concept_id = parent.concept_id
                                    INNER JOIN @vocab_schema.concept child
                                    ON a.descendant_concept_id = child.concept_id
                                    ;",
                                                             vocab_schema = vocab_schema),
                                             conn = conn,
                                             verbose = verbose,
                                             render_sql = render_sql
                        )


                range_output[[length(range_output)+1]] <-
                        last_level %>%
                        dplyr::inner_join(atc_5th %>%
                                                  dplyr::select(parent = child),
                                          by = "parent")

                secretary::typewrite("There are", length(range_output), "levels below", secretary::inside_out(sprintf('%s %s', concept_class_obj@concept_id, concept_class_obj@concept_name)))
                secretary::typewrite("Row counts:")
                1:length(range_output) %>%
                        purrr::map2(range_output,
                                    function(x,y)
                                            secretary::typewrite(sprintf("%s: %s", x, nrow(y)), tabs = 4, timepunched = FALSE)
                        )

                range_output


        }



#' @title
#' Plot an ATC Preview as a Tree Diagram
#'
#' @description
#' Due to the high amount of records, use \code{\link{preview_atc_classification}} to determine the appropriate range based on row counts per level to supply the optional `range` argument. The final output is either 1. the data.tree-style dataframe if the `skip_plot` argument is set to true, 2. a plot in the Viewer, or 3. a plot saved to an html file as named by the `file` argument if one is provided.
#'
#' @param skip_plot If true, returns the dataframe before it is plotted and plotting is not done. This is an option to troubleshoot or customize a plot beyond what is available within the function.
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{unite}}
#'  \code{\link[colorspace]{rainbow_hcl}}
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{press_enter}}
#'  \code{\link[collapsibleTree]{collapsibleTreeNetwork}}
#' @rdname plot_atc_classification
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate_all distinct group_by summarize_at vars ungroup select left_join
#' @importFrom tidyr pivot_longer unite
#' @importFrom colorspace terrain_hcl
#' @importFrom secretary typewrite press_enter
#' @importFrom collapsibleTree collapsibleTreeNetwork
#' @importFrom htmlwidgets saveWidget

plot_atc_classification <-
        function(conn,
                 concept_class_obj,
                 range,
                 file,
                 vocab_schema = "omop_vocabulary",
                 color_by = "standard_concept",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1,
                 skip_plot = FALSE) {


                # if (is.concept(concept_class_obj)) {
                #
                #         concept_id <- concept_class_obj@concept_id
                #
                # } else {
                #
                #         stop("`concept_class_obj` must be a concept class object")
                #
                # }

                domain_id <- "Measurement"
                vocabulary_id <- "LOINC"
                child <- domain_id
                root <-
                        tibble::tibble(parent = NA_character_,
                                       child = child)


                range_output <- preview_atc_classification(conn = conn,
                                                        concept_class_obj = concept_class_obj,
                                                        vocab_schema = vocab_schema,
                                                        verbose = verbose,
                                                        render_sql = render_sql,
                                                        sleep_time = sleep_time)

                if (!missing(range)) {

                        range_output <- range_output[range]
                }

                df <- dplyr::bind_rows(root,
                                       range_output)

                color <- unlist(df[,color_by])
                color[is.na(color)] <- "NA"
                df$color <- factor(color)
                levels(df$color) <- colorspace::diverging_hcl(n = length(levels(df$color)))
                df$color <- as.character(df$color)

                tooltip <-
                        df %>%
                        dplyr::mutate_all(as.character) %>%
                        tidyr::pivot_longer(cols = !c(parent,child),
                                            names_to = "attribute",
                                            values_to = "attribute_value",
                                            values_drop_na = TRUE) %>%
                        tidyr::unite(col = tooltip,
                                     attribute,
                                     attribute_value,
                                     sep = ": ",
                                     remove = TRUE,
                                     na.rm = TRUE) %>%
                        dplyr::distinct() %>%
                        dplyr::group_by(child) %>%
                        dplyr::summarize_at(dplyr::vars(tooltip), ~paste(., collapse = "<br>")) %>%
                        dplyr::ungroup() %>%
                        dplyr::distinct()

                df <-
                        df %>%
                        dplyr::select(parent, child, color) %>%
                        dplyr::left_join(tooltip) %>%
                        dplyr::distinct()

                if (skip_plot) {

                        df

                } else {
                secretary::typewrite("There are", nrow(df), "rows in the data tree. Plotting...")

                if (missing(file)) {

                        collapsibleTree::collapsibleTreeNetwork(df = df,
                                                                tooltipHtml = "tooltip",
                                                                fill = "color")

                } else {

                        p <- collapsibleTree::collapsibleTreeNetwork(df = df,
                                                                tooltipHtml = "tooltip",
                                                                fill = "color")

                        htmlwidgets::saveWidget(widget = p,
                                                file = file)
                }
                }

        }
