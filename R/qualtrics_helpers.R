#' Generate Qualtrics matrix question text from a codebook
#'
#' Build Qualtrics matrix question text and item lists from a codebook-style
#' input file. To write a Qualtrics `.txt` file, call
#' `cat(out$text, sep = "\\n", file = "your_file.txt")` on the result.
#' The function supports files in `.csv`, `.xlsx`/`.xls`, and LibreOffice
#' `.ods` formats and can parse two item naming conventions: items encoded as
#' `scale_itemnumber` (e.g., `stress_1`) or a `stemonly` format where the scale
#' name is repeated for each item.
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
#' @param build_text Logical; if `TRUE`, assemble Qualtrics-ready text blocks.
#' @param clean_text A function applied to each item text vector when
#'   `build_text = TRUE` (defaults to [stringr::str_squish]). Set to `NULL` to
#'   skip cleaning.
#' @param keep_order Logical; if `TRUE`, preserve the first-appearance order of
#'   scales in the input file when assembling text.
#'
#' @return A list with elements:
#' - `scalenames`: character vector of scale names.
#' - `itemtext_list`: named list of item texts per scale.
#' - `text`: character vector of assembled Qualtrics matrix question text or
#'   `NULL` when `build_text = FALSE`.
#' - `items`: tibble with `scale`, `itemno`, and `itemtext` columns.
#' - `responses_leadin_list`: named list of lead-ins and responses (or `NULL`).
#' - `leadins`: named vector of lead-in text.
#' - `response_labels`: named list of response labels.
#'
#' @examples
#' codebook <- data.frame(
#'   scale = rep(c("submarine", "lunar"), each = 5),
#'   item = c(
#'     "I am licensed to operate nuclear submarines.",
#'     "I can safely dock a submarine in a pineapple.",
#'     "I navigate by the stars while underwater.",
#'     "I have befriended at least one dolphin crew member.",
#'     "I can calculate torpedo trajectories in my head.",
#'     "I have walked on the Moon during lunch.",
#'     "I can fix a rover with duct tape.",
#'     "I keep a spare spacesuit in my closet.",
#'     "I speak fluent Martian.",
#'     "I have a favorite crater."
#'   ),
#'   instruction = c(
#'     "How much do you agree with these statements?",
#'     rep(NA_character_, 4),
#'     "How true are these statements about you?",
#'     rep(NA_character_, 4)
#'   ),
#'   response = rep(1:5, 2),
#'   response_label = rep(
#'     c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
#'     2
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(codebook, tmp, row.names = FALSE)
#' out <- qmatrix_generator(
#'   inpfile = tmp,
#'   varcol = "scale",
#'   itemcol = "item",
#'   itemformat = "stemonly",
#'   responses = TRUE,
#'   leadin = "instruction",
#'   resp = "response",
#'   resplabel = "response_label"
#' )
#' out$scalenames
#' tmp_out <- tempfile(fileext = ".txt")
#' cat(out$text, sep = "\n", file = tmp_out)
#' # Not run: typical output path
#' # \dontrun{
#' # dir.create("output", showWarnings = FALSE)
#' # cat(out$text, sep = "\n", file = "output/qualtrics_export.txt")
#' # }
#' @export
qmatrix_generator <- function(
  inpfile,
  sheet = NULL,
  varcol,
  itemcol,
  itemformat = c("_number", "stemonly"),
  responses = FALSE,
  leadin = NULL,
  resp = NULL,
  resplabel = NULL,
  build_text = TRUE,
  clean_text = stringr::str_squish,
  keep_order = TRUE
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

  scalenames_out <- scalenames
  if (keep_order) {
    scalenames_out <- unique(stats::na.omit(x[[varcol]]))
    scalenames_out <- scalenames_out[scalenames_out %in% scalenames]
  }

  leadins <- stats::setNames(character(0), character(0))
  response_labels <- stats::setNames(vector("list", 0), character(0))
  if (!is.null(responses_leadin_list)) {
    leadins <- purrr::map_chr(responses_leadin_list, function(x) {
      leadin_vals <- x$leadin
      if (length(leadin_vals) == 0) {
        ""
      } else {
        as.character(leadin_vals[1])
      }
    })
    names(leadins) <- scalenames

    response_labels <- purrr::map(responses_leadin_list, function(x) {
      r <- x$responses
      if (is.null(r)) {
        return(character(0))
      }
      if (is.data.frame(r)) {
        if (all(c("code", "label") %in% names(r))) {
          return(paste(r$code, r$label))
        }
        return(as.character(r[[1]]))
      }
      as.character(r)
    })
    names(response_labels) <- scalenames
  }

  text <- NULL
  if (build_text) {
    itemtext_list_out <- itemtext_list
    if (!is.null(clean_text)) {
      itemtext_list_out <- purrr::map(itemtext_list_out, clean_text)
    }

    text <- purrr::map_chr(scalenames_out, function(scale_name) {
      leadin_text <- leadins[[scale_name]]
      if (is.null(leadin_text)) {
        leadin_text <- ""
      }
      heading <- if (nzchar(leadin_text)) {
        stringr::str_c(scale_name, ". ", leadin_text)
      } else {
        stringr::str_c(scale_name, ".")
      }

      item_lines <- itemtext_list_out[[scale_name]]
      if (is.null(item_lines)) {
        item_lines <- character(0)
      }
      resp_lines <- response_labels[[scale_name]]
      if (is.null(resp_lines)) {
        resp_lines <- character(0)
      }

      stringr::str_c(
        heading,
        "",
        stringr::str_c(item_lines, collapse = "\n"),
        "",
        stringr::str_c(resp_lines, collapse = "\n"),
        "",
        sep = "\n",
        collapse = "\n"
      )
    })
  }

  list(
    scalenames = scalenames_out,
    itemtext_list = itemtext_list,
    text = text,
    items = items,
    responses_leadin_list = responses_leadin_list,
    leadins = leadins,
    response_labels = response_labels
  )
}

# # # todo: integrate the output returned from qmatrix_generator function as x
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
