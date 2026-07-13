

<!-- README.md is generated from README.qmd. Please edit that file -->

# econioread

<!-- badges: start -->

<!-- badges: end -->

econioread provides a step-by-step interface for reading input-output
tables from `csv`, `xls`, and `xlsx` files into
[econio](https://github.com/UchidaMizuki/econio) input-output table
objects. It handles the messy parts of importing published input-output
tables: locating and importing raw spreadsheet cells, building headers,
and identifying regions, sector names, and sector types, for both
regional and multiregional tables.

## Installation

You can install the development version of econioread from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("UchidaMizuki/econioread")
```

## Example

`io_table_reader()` builds a step-by-step reader for a single
input-output table file. Each step is applied in turn, transforming the
raw cells of the file into a finished `econio` input-output table:

``` r
library(econioread)

io_table_reader("path/to/input-output-table.xlsx") |>
  io_table_read_cells(
    sheets = "Sheet1",
    rows_range = c(1, 100),
    cols_range = c(1, 100)
  ) |>
  io_table_read_headers(
    input_names = c(up = "input_sector_code"),
    output_names = c(left = "output_sector_code")
  ) |>
  io_table_read_sector_names(
    input_sector_name_glue = "{input_sector_code}",
    output_sector_name_glue = "{output_sector_code}"
  ) |>
  io_table_read_sector_types(
    competitive_import = FALSE,
    industry_pattern = "^\\d",
    import_pattern = "Imports",
    value_added_pattern = "Value added",
    final_demand_pattern = "Final demand",
    export_pattern = "Exports",
    total_pattern = "Total"
  ) |>
  io_table_read_data(value_scale = 1e6)
```

For multiregional tables, pass `region_type = "multiregional"` to
`io_table_reader()`, which adds an `io_table_read_regions()` step to
extract input and output regions before the sector types are identified.
