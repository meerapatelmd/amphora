% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_cellfie.R
\name{create_class_hierarchy_cellfie}
\alias{create_class_hierarchy_cellfie}
\title{Create Class Hierarchy Cellfie}
\usage{
create_class_hierarchy_cellfie(data, Class, subClass, file)
}
\description{
Write an Excel file of a class hierarchy from a dataframe to import into
Protege using the
\href{https://github.com/protegeproject/cellfie-plugin}{Cellfie plugin}.
}
\details{
Create an
The triplets to to create a single class are:
\itemize{
\item {Subject: IRI#Class}
\item {Predicate: \url{http://www.w3.org/1999/02/22-rdf-syntax-ns#type}}
\item {Object: \url{http://www.w3.org/2002/07/owl#Class}}
}

A subClass of an existing class has 2 lines:
\enumerate{
\item Add subClass as Class
\itemize{
\item {Subject: IRI#subClass}
\item {Predicate: \url{http://www.w3.org/1999/02/22-rdf-syntax-ns#type}}
\item {Object: \url{http://www.w3.org/2002/07/owl#Class}}
}

\item Add subClassof
\itemize{
\item {Subject: IRI#subClass}
\item {Predicate: \url{http://www.w3.org/2000/01/rdf-schema#subClassOf}}
\item {Object: IRI#Class}
}
}

@rdname create_class_hierarchy_cellfie
@export
}
