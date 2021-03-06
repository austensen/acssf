% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acs_load.R
\name{acs_load}
\alias{acs_load}
\title{Load all files created by acs_transform() into database table}
\usage{
acs_load(conn, table_name, acs_dir = ".")
}
\arguments{
\item{conn}{A \code{\link[DBI:DBIConnection-class]{DBI::DBIConnection}} object, as returned by
\code{\link[DBI:dbConnect]{DBI::dbConnect()}}.}

\item{table_name}{A character string specifying the unquoted database table name.}

\item{acs_dir}{The root directory in which all the ACS
data has been downloaded with \code{\link[=acs_download]{acs_download()}}. Defaults to current working directory.}
}
\description{
Load all files created by acs_transform() into database table
}
