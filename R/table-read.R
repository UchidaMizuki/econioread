#' Input-output table reader
#'
#' `io_table_reader()` provides a step-by-step process to read an input-output
#' table from a file.
#'
#' @param file Path to the input-output table file.
#' @param region_type Type of the input-output table. Either `"regional"` or
#' `"multiregional"`, by default `"regional"`.
#'
#' @return A function that takes a file path and returns a step-by-step
#' input-output table reader.
#'
#' @export
io_table_reader <- function(
  file,
  region_type = c("regional", "multiregional")
) {
  region_type <- rlang::arg_match(region_type, c("regional", "multiregional"))
  steps <- c(
    io_table_read_cells = "Import cell contents of an input-output table",
    io_table_read_headers = "Create headers of an input-output table",
    io_table_read_regions = "Create columns of regions in an input-output table",
    io_table_read_sector_names = "Create columns of sector names in an input-output table",
    io_table_read_sector_types = "Create columns of sector types in an input-output table",
    io_table_read_data = "Create data of an input-output table"
  )
  steps <- switch(
    region_type,
    regional = steps[!names(steps) %in% "io_table_read_regions"],
    multiregional = steps
  )
  adverbial::step_by_step(steps)(file)
}

#' Import cell contents of an input-output table
#'
#' `io_table_read_cells()` reads the cell contents of an input-output table.
#'
#' @param file Path to the input-output table file.
#' @param sheets A character vector of sheet names to read from. Passed to
#' [tidyxl::xlsx_cells()].
#' @param rows_range,cols_range A numeric vector of length 2 specifying the
#' range of rows and columns to read from.
#' @param rows_exclude,cols_exclude A numeric vector of row and column numbers
#' to exclude.
#'
#' @return A data frame containing the cell contents of an input-output table.
#'
#' @export
io_table_read_cells <- function(
  file,
  sheets = NULL,
  rows_range = c(1, Inf),
  rows_exclude = integer(),
  cols_range = c(1, Inf),
  cols_exclude = integer()
) {
  f <- function(
    file,
    sheets,
    rows_range,
    rows_exclude,
    cols_range,
    cols_exclude
  ) {
    vctrs::vec_check_size(rows_range, 2)
    vctrs::vec_check_size(cols_range, 2)

    switch(
      fs::path_ext(file),
      csv = readr::read_csv(
        file,
        col_names = FALSE,
        col_types = "c",
        name_repair = "minimal",
        skip_empty_rows = FALSE,
      ) |>
        unpivotr::as_cells(),
      xls = readxl::read_xls(
        file,
        sheet = sheets,
        col_names = FALSE,
        col_types = "text",
        .name_repair = "minimal",
      ) |>
        unpivotr::as_cells(),
      xlsx = tidyxl::xlsx_cells(file, sheets = sheets %||% NA)
    ) |>
      unpivotr::pack() |>
      dplyr::select("row", "col", "value") |>
      unpivotr::unpack() |>
      dplyr::filter(
        dplyr::between(.data$row, rows_range[[1]], rows_range[[2]]),
        !.data$row %in% .env$rows_exclude,
        dplyr::between(.data$col, cols_range[[1]], cols_range[[2]]),
        !.data$col %in% .env$cols_exclude
      )
  }
  adverbial::as_step(f, "io_table_read_cells")(
    file,
    sheets = sheets,
    rows_range = rows_range,
    rows_exclude = rows_exclude,
    cols_range = cols_range,
    cols_exclude = cols_exclude
  )
}

#' Create headers of an input-output table
#'
#' `io_table_read_headers()` creates headers of an input-output table.
#'
#' @param cells A data frame containing the cell contents of an input-output
#' table.
#' @param input_names,output_names A character vector of input and output names.
#'
#' @return A data frame containing the headers of an input-output table.
#'
#' @export
io_table_read_headers <- function(
  cells,
  input_names,
  output_names
) {
  f <- function(cells, input_names, output_names) {
    for (i in seq_along(input_names)) {
      input_name_direction <- names(input_names)[[i]] %||% "left"
      input_name <- input_names[[i]]
      cells <- cells |>
        unpivotr::behead(input_name_direction, !!input_name)
    }
    for (i in seq_along(output_names)) {
      output_name_direction <- names(output_names)[[i]] %||% "up"
      output_name <- output_names[[i]]
      cells <- cells |>
        unpivotr::behead(output_name_direction, !!output_name)
    }
    cells |>
      unpivotr::pack() |>
      dplyr::select(dplyr::all_of(c(input_names, output_names)), "value")
  }
  adverbial::as_step(f, "io_table_read_headers")(
    cells,
    input_names = input_names,
    output_names = output_names
  )
}

#' Create columns of sector names in an input-output table
#'
#' `io_table_read_sector_names()` creates columns of sector names in an
#' input-output table.
#'
#' @param cells A data frame containing the cell contents of an input-output
#' table.
#' @param input_sector_name_glue,output_sector_name_glue A scalar character
#' speecifying the glue pattern for input and output sector names.
#'
#' @return A data frame containing the sector names of an input-output table.
#'
#' @export
io_table_read_sector_names <- function(
  cells,
  input_sector_name_glue,
  output_sector_name_glue
) {
  f <- function(cells, input_sector_name_glue, output_sector_name_glue) {
    cells |>
      dplyr::mutate(
        input_sector_name = stringr::str_glue(input_sector_name_glue),
        output_sector_name = stringr::str_glue(output_sector_name_glue),
        .keep = "unused",
        .before = "value"
      )
  }
  adverbial::as_step(f, "io_table_read_sector_names")(
    cells,
    input_sector_name_glue = input_sector_name_glue,
    output_sector_name_glue = output_sector_name_glue
  )
}

#' Create columns of regions in an input-output table
#'
#' `io_table_read_regions()` creates columns of regions in an input-output
#' table.
#'
#' @param cells A data frame containing the cell contents of an input-output
#' table.
#' @param input_region_glue,output_region_glue A scalar character string
#' specifying the glue pattern for input and output regions.
#'
#' @return A data frame containing the regions of an input-output table.
#'
#' @export
io_table_read_regions <- function(
  cells,
  input_region_glue,
  output_region_glue
) {
  f <- function(cells, input_region_glue, output_region_glue) {
    cells |>
      dplyr::mutate(
        input_region = stringr::str_glue(input_region_glue),
        output_region = stringr::str_glue(output_region_glue),
        .keep = "unused"
      )
  }
  adverbial::as_step(f, "io_table_read_regions")(
    cells,
    input_region_glue = input_region_glue,
    output_region_glue = output_region_glue
  )
}

#' Create columns of sector types in an input-output table
#'
#' `io_table_read_sector_types()` creates columns of sector types in an
#' input-output table.
#'
#' @param cells A data frame containing the cell contents of an input-output
#' table.
#' @param competitive_import A scalar logical indicating whether the
#' input-output table is a competitive import type.
#' @param industry_pattern,import_pattern,value_added_pattern,final_demand_pattern,export_pattern,total_pattern
#' A scalar character specifying the pattern for sector types.
#'
#' @return A data frame containing the sector types of an input-output table.
#'
#' @export
io_table_read_sector_types <- function(
  cells,
  competitive_import,
  industry_pattern,
  import_pattern,
  value_added_pattern,
  final_demand_pattern,
  export_pattern,
  total_pattern
) {
  f <- function(
    cells,
    competitive_import,
    industry_pattern,
    import_pattern,
    value_added_pattern,
    final_demand_pattern,
    export_pattern,
    total_pattern
  ) {
    competitive_import <- vctrs::vec_cast(competitive_import, logical())
    vctrs::vec_check_size(competitive_import, 1)

    input_sector_types <- if (competitive_import) {
      c("industry", "value_added", "total")
    } else {
      c("industry", "import", "value_added", "total")
    }
    output_sector_types <- if (competitive_import) {
      c("industry", "final_demand", "export", "import", "total")
    } else {
      c("industry", "final_demand", "export", "total")
    }

    get_cases_sector_types <- function(sector_types, col_sector_name) {
      purrr::map(sector_types, function(sector_type) {
        rlang::expr(
          stringr::str_detect(
            .data[[!!col_sector_name]],
            .env[[!!paste(sector_type, "pattern", sep = "_")]]
          ) ~
            !!sector_type
        )
      })
    }
    cases_input_sector_types <- get_cases_sector_types(
      input_sector_types,
      "input_sector_name"
    )
    cases_output_sector_types <- get_cases_sector_types(
      output_sector_types,
      "output_sector_name"
    )
    cells |>
      dplyr::mutate(
        input_sector_type = dplyr::case_when(!!!cases_input_sector_types),
        output_sector_type = dplyr::case_when(!!!cases_output_sector_types)
      ) |>
      tidyr::drop_na("input_sector_type", "output_sector_type")
  }
  adverbial::as_step(f, "io_table_read_sector_types")(
    cells,
    competitive_import = competitive_import,
    industry_pattern = industry_pattern,
    import_pattern = import_pattern,
    value_added_pattern = value_added_pattern,
    final_demand_pattern = final_demand_pattern,
    export_pattern = export_pattern,
    total_pattern = total_pattern
  )
}

#' Create data of an input-output table
#'
#' `io_table_read_data()` creates data of an input-output table.
#'
#' @param cells A data frame containing the cell contents of an input-output
#' table.
#' @param value_scale A scalar numeric specifying the scale for the numeric values.
#' @param value_na A character vector of strings to be treated as NA values.
#' @param total_tolerance Passed to [econio::io_table_multiregional()] or
#' [econio::io_table_regional()]. By default, `.Machine$double.eps^0.5`.
#'
#' @return An input-output table object.
#'
#' @export
io_table_read_data <- function(
  cells,
  value_scale,
  value_na = c("", "NA"),
  total_tolerance = .Machine$double.eps^0.5
) {
  f <- function(cells, scale, total_tolerance) {
    cells <- cells |>
      dplyr::mutate(
        value = .data$value |>
          purrr::map_dbl(\(x) readr::parse_number(x, na = value_na)),
        value = .data$value * .env$value_scale,
      )

    competitive_import <- "import" %in% cells$output_sector_type
    if (all(c("input_region", "output_region") %in% names(cells))) {
      econio::io_table_multiregional(
        cells,
        competitive_import = competitive_import,
        total_tolerance = total_tolerance
      )
    } else {
      econio::io_table_regional(
        cells,
        competitive_import = competitive_import,
        total_tolerance = total_tolerance
      )
    }
  }
  adverbial::as_step(f, "io_table_read_data")(
    cells,
    scale = scale,
    total_tolerance = total_tolerance
  )
}
