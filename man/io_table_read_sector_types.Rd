% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-read.R
\name{io_table_read_sector_types}
\alias{io_table_read_sector_types}
\title{Create columns of sector types in an input-output table}
\usage{
io_table_read_sector_types(
  cells,
  competitive_import,
  industry_pattern,
  import_pattern,
  value_added_pattern,
  final_demand_pattern,
  export_pattern,
  total_pattern
)
}
\arguments{
\item{cells}{A data frame containing the cell contents of an input-output
table.}

\item{competitive_import}{A scalar logical indicating whether the
input-output table is a competitive import type.}

\item{industry_pattern, import_pattern, value_added_pattern, final_demand_pattern, export_pattern, total_pattern}{A scalar character specifying the pattern for sector types.}
}
\value{
A data frame containing the sector types of an input-output table.
}
\description{
`io_table_read_sector_types()` creates columns of sector types in an
input-output table.
}
