#' @title
#' Preview a HemOnc Class's Descendants
#'
#'
#' @export
#' @rdname preview_hemonc_classification

preview_hemonc_classification <-
        function(conn,
                 concept_class_obj,
                 vocab_schema = "omop_vocabulary",
                 verbose = TRUE,
                 render_sql = TRUE,
                 sleep_time = 1) {


                if (is.concept(concept_class_obj)) {

                        if (concept_class_obj@standard_concept %in% "C" &&
                            concept_class_obj@vocabulary_id %in% "HemOnc" &&
                            is.na(concept_class_obj@invalid_reason)) {

                        concept_id <- concept_class_obj@concept_id
                        } else {
                                stop("`concept_class_obj` is not a valid HemOnc Class")
                        }

                } else {

                        stop("`concept_class_obj` must be a concept class object")

                }

                domain_id <- "Drug"
                vocabulary_id <- "HemOnc"
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
                                    -- AND c2.standard_concept = 'C'
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
                                        c.vocabulary_id IN ('HemOnc')
                                        AND c.concept_class_id IN ('Component Class')
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
                        last_level

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
