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
#' A scalar character specifying the pattern for sector names.
#' @param industry_total_pattern,import_total_pattern,value_added_total_pattern,final_demand_total_pattern,export_total_pattern
#' A scalar character specifying the pattern for total sector names.
#'
#' @return A data frame containing the sector types of an input-output table.
#'
#' @export
io_table_read_sector_types <- function(
  cells,
  competitive_import,
  industry_pattern = NULL,
  industry_total_pattern = NULL,
  import_pattern = NULL,
  import_total_pattern = NULL,
  value_added_pattern = NULL,
  value_added_total_pattern = NULL,
  final_demand_pattern = NULL,
  final_demand_total_pattern = NULL,
  export_pattern = NULL,
  export_total_pattern = NULL,
  total_pattern = NULL
) {
  f <- function(
    cells,
    competitive_import,
    industry_pattern,
    industry_total_pattern,
    import_pattern,
    import_total_pattern,
    value_added_pattern,
    value_added_total_pattern,
    final_demand_pattern,
    final_demand_total_pattern,
    export_pattern,
    export_total_pattern,
    total_pattern
  ) {
    competitive_import <- vctrs::vec_cast(competitive_import, logical())
    vctrs::vec_check_size(competitive_import, 1)

    sector_patterns <- list(
      industry = industry_pattern,
      import = import_pattern,
      value_added = value_added_pattern,
      final_demand = final_demand_pattern,
      export = export_pattern,
      total = total_pattern
    )
    sector_total_patterns <- list(
      industry = industry_total_pattern,
      import = import_total_pattern,
      value_added = value_added_total_pattern,
      final_demand = final_demand_total_pattern,
      export = export_total_pattern
    )

    sector_types <- if (competitive_import) {
      list(
        input = c("industry", "value_added", "total"),
        output = c("industry", "final_demand", "export", "import", "total")
      )
    } else {
      list(
        input = c("industry", "import", "value_added", "total"),
        output = c("industry", "final_demand", "export", "total")
      )
    }

    sector_names <- vctrs::vec_init(list(), 2) |>
      rlang::set_names(c("input", "output"))
    for (axis in c("input", "output")) {
      sector_names_todo <- cells[[paste(axis, "sector_name", sep = "_")]] |>
        vctrs::vec_unique()
      sector_names_done <- vctrs::vec_init_along(
        list(),
        sector_types[[axis]]
      ) |>
        rlang::set_names(sector_types[[axis]])

      for (sector_type in sector_types[[axis]]) {
        sector_pattern <- sector_patterns[[sector_type]]
        sector_total_pattern <- sector_total_patterns[[sector_type]]

        if (is.null(sector_total_pattern)) {
          loc_sector <- stringr::str_which(
            sector_names_todo,
            sector_pattern
          )
          sector_names_done[[sector_type]] <- sector_names_todo |>
            vctrs::vec_slice(loc_sector)
          sector_names_todo <- sector_names_todo |>
            vctrs::vec_slice(-loc_sector)
        } else {
          # Extract those that match `sector_total_pattern`
          loc_sector_total <- stringr::str_which(
            sector_names_todo,
            sector_total_pattern
          )
          if (vctrs::vec_size(loc_sector_total) == 0) {
            cli::cli_abort(
              "{.str {sector_total_pattern}} does not match any sector name for {.var {sector_type}}."
            )
          } else if (vctrs::vec_size(loc_sector_total) > 1) {
            cli::cli_abort(c(
              "{.str {sector_total_pattern}} matches multiple sector names for {.var {sector_type}}.",
              "i" = "Matched sector name{?s}: {.str {sector_names_todo[loc_sector_total]}}"
            ))
          }

          loc_sector <- seq_len(loc_sector_total - 1)
          sector_names_done[[sector_type]] <- sector_names_todo |>
            vctrs::vec_slice(loc_sector)
          sector_names_todo <- sector_names_todo[
            -c(loc_sector, loc_sector_total)
          ]

          # Extract those that match `sector_pattern`
          if (!is.null(sector_pattern)) {
            loc_sector <- stringr::str_which(
              sector_names_done[[sector_type]],
              sector_pattern
            )
            sector_names_done[[sector_type]] <- sector_names_done[[
              sector_type
            ]] |>
              vctrs::vec_slice(loc_sector)
          }
        }
      }
      sector_names[[axis]] <- sector_names_done
    }

    get_cases_sector_types <- function(sector_types, axis) {
      purrr::map(sector_types[[axis]], function(sector_type) {
        rlang::expr(
          !!sector_names[[axis]][[sector_type]] ~ !!sector_type
        )
      })
    }

    cases_input_sector_types <- get_cases_sector_types(
      sector_types,
      "input"
    )
    cases_output_sector_types <- get_cases_sector_types(
      sector_types,
      "output"
    )
    cells |>
      dplyr::mutate(
        input_sector_type = dplyr::case_match(
          .data$input_sector_name,
          !!!cases_input_sector_types
        ),
        .before = "input_sector_name"
      ) |>
      dplyr::mutate(
        output_sector_type = dplyr::case_match(
          .data$output_sector_name,
          !!!cases_output_sector_types
        ),
        .before = "output_sector_name"
      ) |>
      tidyr::drop_na("input_sector_type", "output_sector_type")
  }
  adverbial::as_step(f, "io_table_read_sector_types")(
    cells,
    competitive_import = competitive_import,
    industry_pattern = industry_pattern,
    industry_total_pattern = industry_total_pattern,
    import_pattern = import_pattern,
    import_total_pattern = import_total_pattern,
    value_added_pattern = value_added_pattern,
    value_added_total_pattern = value_added_total_pattern,
    final_demand_pattern = final_demand_pattern,
    final_demand_total_pattern = final_demand_total_pattern,
    export_pattern = export_pattern,
    export_total_pattern = export_total_pattern,
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
#' @param check_axes Passed to [econio::io_table_multiregional()] or
#' [econio::io_table_regional()]. By default, `TRUE`.
#'
#' @return An input-output table object.
#'
#' @export
io_table_read_data <- function(
  cells,
  value_scale,
  value_na = c("", "NA"),
  total_tolerance = .Machine$double.eps^0.5,
  check_axes = TRUE
) {
  f <- function(cells, scale, total_tolerance) {
    cells <- cells |>
      tidyr::unnest("value") |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of("value") & dplyr::where(is.numeric),
          as.character
        ),
        value = readr::parse_number(.data$value, na = value_na) *
          .env$value_scale,
      ) |>
      tidyr::drop_na("value")

    competitive_import <- "import" %in% cells$output_sector_type
    if (all(c("input_region", "output_region") %in% names(cells))) {
      econio::io_table_multiregional(
        cells,
        competitive_import = competitive_import,
        total_tolerance = total_tolerance,
        check_axes = check_axes
      )
    } else {
      econio::io_table_regional(
        cells,
        competitive_import = competitive_import,
        total_tolerance = total_tolerance,
        check_axes = check_axes
      )
    }
  }
  adverbial::as_step(f, "io_table_read_data")(
    cells,
    scale = scale,
    total_tolerance = total_tolerance
  )
}
