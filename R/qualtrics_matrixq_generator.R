#' Generate Qualtrics matrix question text from a codebook
#'
#' Build Qualtrics matrix question text and item lists from a codebook-style
#' input file. The function supports files in `.csv`, `.xlsx`/`.xls`, and
#' LibreOffice `.ods` formats and can parse two item naming conventions:
#' items encoded as `scale_itemnumber` (e.g., `stress_1`) or a `stemonly`
#' format where the scale name is repeated for each item.
#'
#' @param inpfile Path to the input codebook file.
#' @param sheet Sheet name or index for spreadsheet inputs (`.xlsx`, `.xls`,
#'   `.ods`). Ignored for `.csv` inputs.
#' @param varcol,itemcol Column name or index of the variable/scale label and
#'   item text columns, respectively.
#' @param itemformat Item naming format. Use `"_number"` (default) for
#'   `scale_itemnumber` labels or `"stemonly"` for repeated scale names.
#' @param responses Logical; if `TRUE` and `itemformat = "stemonly"`, extract
#'   optional lead-in text and response labels from the same sheet.
#' @param leadin Column name or index containing lead-in/instruction text (only
#'   used when `responses = TRUE`).
#' @param resp Column name or index containing response codes (optional, only
#'   used when `responses = TRUE`).
#' @param resplabel Column name or index containing response labels (only used
#'   when `responses = TRUE`).
#'
#' @return A list with elements:
#' - `scalenames`: character vector of scale names.
#' - `itemtext_list`: named list of item texts per scale.
#' - `text`: character vector of assembled Qualtrics matrix question text.
#' - `items`: tibble with `scale`, `itemno`, and `itemtext` columns.
#' - `responses_leadin_list`: named list of lead-ins and responses (or `NULL`).
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' dat <- data.frame(
#'   var = c("scale_1", "scale_2", "scale_3"),
#'   item = c("Item one", "Item two", "Item three"),
#'   stringsAsFactors = FALSE
#' )
#' utils::write.csv(dat, tmp, row.names = FALSE)
#' out <- qualtrics_matrixq_generator(tmp, varcol = "var", itemcol = "item")
#' str(out$text)
#' @export
qualtrics_matrixq_generator <- function(
  inpfile,
  sheet = NULL,
  varcol,
  itemcol,
  itemformat = c("_number", "stemonly"),
  responses = FALSE,
  leadin = NULL,
  resp = NULL,
  resplabel = NULL
) {
  itemformat <- match.arg(itemformat)
  x <- .qm_read_input(inpfile, sheet = sheet)

  varcol <- .qm_resolve_col(x, varcol, "varcol")
  itemcol <- .qm_resolve_col(x, itemcol, "itemcol")

  items_raw <- NULL
  if (itemformat == "_number") {
    varnames <- as.character(x[[varcol]])
    itemidx <- !is.na(varnames) & stringr::str_detect(varnames, "^[^_]+_.+$")
    if (!any(itemidx)) {
      rlang::abort("No items detected for itemformat = '_number'.")
    }
    split_mat <- stringr::str_split_fixed(varnames, "_", 2)
    items <- tibble::tibble(
      scale = split_mat[itemidx, 1],
      itemno = split_mat[itemidx, 2],
      itemtext = x[[itemcol]][itemidx]
    )
  } else {
    itemidx <- !is.na(x[[varcol]]) & !is.na(x[[itemcol]])
    items_raw <- x[itemidx, , drop = FALSE]
    if (nrow(items_raw) == 0) {
      rlang::abort("No items detected for itemformat = 'stemonly'.")
    }
    scalenames <- unique(items_raw[[varcol]])
    items <- purrr::map_dfr(scalenames, function(scale_name) {
      if (.qm_check_non_consecutive(items_raw[[varcol]], scale_name)) {
        rlang::abort(
          "The scale label is used non-consecutively. Please check the codebook."
        )
      }
      items_of_scale <- items_raw[items_raw[[varcol]] == scale_name, , drop = FALSE]
      tibble::tibble(
        scale = items_of_scale[[varcol]],
        itemno = seq_len(nrow(items_of_scale)),
        itemtext = items_of_scale[[itemcol]]
      )
    })
  }

  if (!exists("scalenames", inherits = FALSE)) {
    scalenames <- unique(items$scale)
  }

  itemtext_list <- purrr::map(scalenames, function(scale_name) {
    itemtext <- items$itemtext[items$scale == scale_name]
    as.character(itemtext)
  })
  names(itemtext_list) <- scalenames

  text <- purrr::map2_chr(scalenames, seq_along(scalenames), function(scale_name, i) {
    stringr::str_c(
      stringr::str_c(i, ".", scale_name),
      "",
      stringr::str_c(itemtext_list[[scale_name]], collapse = "\n"),
      "",
      "[Antwortoptionen einfuegen]",
      "",
      sep = "\n"
    )
  })

  responses_leadin_list <- NULL
  if (responses) {
    if (itemformat != "stemonly") {
      warning(
        "`responses = TRUE` is only supported for itemformat = 'stemonly'.",
        call. = FALSE
      )
    } else {
      if (is.null(resplabel)) {
        rlang::abort("`resplabel` must be provided when responses = TRUE.")
      }
      leadin_col <- if (is.null(leadin)) NULL else .qm_resolve_col(x, leadin, "leadin")
      resplabel_col <- .qm_resolve_col(x, resplabel, "resplabel")
      resp_col <- if (is.null(resp)) NULL else .qm_resolve_col(x, resp, "resp")

      responses_leadin_list <- purrr::map(scalenames, function(scale_name) {
        leadin_vals <- character(0)
        if (!is.null(leadin_col)) {
          leadidx <- !is.na(x[[varcol]]) & !is.na(x[[leadin_col]])
          leadraw <- x[leadidx, , drop = FALSE]
          leadin_vals <- leadraw[[leadin_col]][leadraw[[varcol]] == scale_name]
          leadin_vals <- leadin_vals[!is.na(leadin_vals)]
        }

        respidx <- !is.na(x[[varcol]]) & !is.na(x[[resplabel_col]])
        respraw <- x[respidx, , drop = FALSE]
        respraw <- respraw[respraw[[varcol]] == scale_name, , drop = FALSE]

        if (is.null(resp_col)) {
          resp_vals <- respraw[[resplabel_col]]
          resp_vals <- resp_vals[!is.na(resp_vals)]
          list(leadin = leadin_vals, responses = resp_vals)
        } else {
          resp_tbl <- tibble::tibble(
            code = respraw[[resp_col]],
            label = respraw[[resplabel_col]]
          )
          resp_tbl <- resp_tbl[!is.na(resp_tbl$label), , drop = FALSE]
          list(leadin = leadin_vals, responses = resp_tbl)
        }
      })
      names(responses_leadin_list) <- scalenames
    }
  }

  list(
    scalenames = scalenames,
    itemtext_list = itemtext_list,
    text = text,
    items = items,
    responses_leadin_list = responses_leadin_list
  )
}

# # # todo: integrate the output returned from qualtrics_matrixq_generator function as x
qualtrics_matrixq_responsescales <- function(
  inpfile,
  sheet = NULL,
  varcol,
  leadin,
  resp,
  resplabel,
  itemformat = c("_number", "stemonly")
) {
  itemformat <- match.arg(itemformat)
  x <- .qm_read_input(inpfile, sheet = sheet)

  varcol <- .qm_resolve_col(x, varcol, "varcol")
  leadin <- .qm_resolve_col(x, leadin, "leadin")
  resp <- .qm_resolve_col(x, resp, "resp")
  resplabel <- .qm_resolve_col(x, resplabel, "resplabel")

  pattern <- "(?=(?:.*_.*))_"
  varnames <- as.character(x[[varcol]])
  split_list <- stringr::str_split(varnames, pattern)
  itemidx <- purrr::map_lgl(split_list, function(x) {
    !all(is.na(x)) && length(x) %in% c(1, 2)
  })

  items_raw <- x[itemidx, , drop = FALSE]
  split_mat <- stringr::str_split(items_raw[[varcol]], pattern, simplify = TRUE)
  items <- tibble::as_tibble(split_mat, .name_repair = "universal")
  items <- dplyr::rename(items, scale = ...1, no = ...2)
  items <- dplyr::mutate(
    items,
    no = items_raw[[resp]],
    itemtext = items_raw[[resplabel]]
  )
  items <- dplyr::filter(items, !is.na(rlang::.data$itemtext))

  scalenames <- unique(items$scale)
  leadin_out <- purrr::map(scalenames, function(name) {
    pattern <- paste0("^", name, "(_\\d+)?$")
    rows <- x[stringr::str_detect(x[[varcol]], pattern), leadin, drop = TRUE]
    rows <- as.character(rows)
    rows[!is.na(rows)]
  })

  list(responses = items, leadin = leadin_out)
}

write_qualtricsgen <- function(text, outfile) {
  # Write the assembled text to the output file.
  cat(text, sep = "\n", file = outfile)
}

.qm_read_input <- function(inpfile, sheet = NULL) {
  if (!file.exists(inpfile)) {
    rlang::abort("`inpfile` does not exist.")
  }
  ext <- tolower(tools::file_ext(inpfile))

  if (ext == "csv") {
    if (!is.null(sheet)) {
      warning("`sheet` is ignored for .csv inputs.", call. = FALSE)
    }
    return(readr::read_csv(inpfile, show_col_types = FALSE))
  }

  if (ext %in% c("xls", "xlsx")) {
    if (is.null(sheet)) {
      return(readxl::read_excel(inpfile))
    }
    return(readxl::read_excel(inpfile, sheet = sheet))
  }

  if (ext == "ods") {
    if (is.null(sheet)) {
      return(readODS::read_ods(inpfile))
    }
    return(readODS::read_ods(inpfile, sheet = sheet))
  }

  rlang::abort("Unsupported file type. Use .csv, .xls/.xlsx, or .ods.")
}

.qm_resolve_col <- function(data, col, arg) {
  if (is.null(col)) {
    rlang::abort(sprintf("`%s` must be provided.", arg))
  }

  if (is.numeric(col)) {
    if (length(col) != 1L) {
      rlang::abort(sprintf("`%s` must be a single column index.", arg))
    }
    if (col < 1L || col > ncol(data)) {
      rlang::abort(sprintf("`%s` is out of range.", arg))
    }
    return(names(data)[[col]])
  }

  if (is.character(col)) {
    if (length(col) != 1L) {
      rlang::abort(sprintf("`%s` must be a single column name.", arg))
    }
    if (!col %in% names(data)) {
      rlang::abort(sprintf("`%s` was not found in the input data.", arg))
    }
    return(col)
  }

  rlang::abort(sprintf("`%s` must be a column name or index.", arg))
}

.qm_check_non_consecutive <- function(vec, target) {
  vec <- as.character(vec)
  idx <- which(vec == target)
  length(idx) > 1 && any(diff(idx) != 1)
}
