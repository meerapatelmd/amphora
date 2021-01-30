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
