#' @title
#' Create Class Hierarchy Cellfie
#'
#' @description
#' Write an Excel file of a class hierarchy from a dataframe to import into
#' Protege using the
#' [Cellfie plugin](https://github.com/protegeproject/cellfie-plugin).
#'
#' @details
#' Create an
#' The triplets to to create a single class are:
#' \itemize{
#'   \item {Subject: IRI#Class}
#'   \item {Predicate: <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>}
#'   \item {Object: <http://www.w3.org/2002/07/owl#Class>}
#' }
#'
#'  A subClass of an existing class has 2 lines:
#'  \enumerate{
#'    \item Add subClass as Class
#'      \itemize{
#'        \item {Subject: IRI#subClass}
#'        \item {Predicate: <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>}
#'        \item {Object: <http://www.w3.org/2002/07/owl#Class>}
#'      }
#'
#'    \item Add subClassof
#'      \itemize{
#'        \item {Subject: IRI#subClass}
#'        \item {Predicate: <http://www.w3.org/2000/01/rdf-schema#subClassOf>}
#'        \item {Object: IRI#Class}
#'      }
#'  }
#'
#'  @rdname create_class_hierarchy_cellfie
#'  @export

create_class_hierarchy_cellfie <-
        function(data,
                 Class,
                 subClass,
                 file) {

                readr::write_csv(x =
                                data %>%
                                        dplyr::select({{ Class }}, {{ subClass}}),
                                file = file)

        }
