#' Table footnote
#'
#' Include tables footnotes and symbols for kables in pandoc format
#'
#' @param table kable output in pandoc format.
#' @param notes footnotes for the table.
#' @param label label for start the footnote.
#' @param notation notation for the symbols and footnotes (default = "number"). See details.
#'
#' @details
#'
#' You should use the pandoc format \code{kable(format = "pipe")}.
#' You can add the footnote symbol using \code{{-}} in your table.
#' \code{notation} could be use: "alphabet", "number", "symbol", "none".
#'
#' @return Table with footnotes for word and html documents
#'
#' @source
#'
#' \url{https://github.com/haozhu233/kableExtra/blob/master/R/add_footnote.R}
#'
#' @importFrom utils read.csv
#' @importFrom stringr str_count str_extract str_extract_all
#'
#' @export

footnotes <- function(table
                      , notes = NULL
                      , label = "Note:"
                      , notation = "alphabet"
) {

  notation <- match.arg(notation, c("alphabet", "number", "none"))

  ids.ops <- read.csv(system.file("files/symbols.csv", package = "inti"))

  if (notation == "none") {

    ids <- ids.ops[["alphabet"]]

  } else {

    ids <- ids.ops[[notation]]

    }

  ids.intable <- gsub("\\*", "\\\\*", ids)

  # Count the number of items in note and in table notation

  count.intablenote <- sum(str_count(table, "\\{-\\}"))
  count.note <- length(notes)

  if (count.intablenote != 0 & count.note != count.intablenote) {
    warning(paste("You entered", count.note, "notes but you put",
                  count.intablenote, "{notes} in your table."))
  }

  # Find out if there are any extra in-table notations needed to be corrected

  extra.notation <- unique(as.numeric(
    str_extract(
      str_extract_all(
        paste0(table, collapse = ""), "\\{-[0-9]{1,2}\\}"
      )[[1]],
      "[0-9]{1,2}")))

# pandoc docs -------------------------------------------------------------
# -------------------------------------------------------------------------

  if (!attr(table, "format") %in% c("html", "latex")) {

# In table notation -------------------------------------------------------
# -------------------------------------------------------------------------

    if (count.intablenote != 0) {

      for (i in 1:count.intablenote) {

        replace_note <- paste0("^", ids.intable[i], "^",
                               paste0(rep(" ", 4 - ceiling(i/5)), collapse = ""))

        table[which(str_detect(table, "\\{-\\}"))[1]] <-
          sub("\\{-\\}", replace_note,
              table[which(str_detect(table, "\\{-\\}"))[1]])

      }

    }

# footnote label ----------------------------------------------------------
# -------------------------------------------------------------------------

    for (i in extra.notation) {
      table <- gsub(paste0("\\{-", i, "\\}"),
                    paste0("^", ids.intable[i], "^",
                           paste0(rep(" ", 4 - ceiling(i/5)), collapse = "")),
                    table)
    }

    table[length(table) + 1] <- ""
    table[length(table) + 1] <- label

# number in the notes -----------------------------------------------------
# -------------------------------------------------------------------------

    if ( notation == "none" ) {

      table[length(table) + 1] <- paste0("<small>",
        paste0("", "", "", notes), collapse = " ",
        "</small>"
        )

    } else {

      table[length(table) + 1] <- paste0("<small>",
        paste0("^", ids[1:length(notes)], "^ ", notes), collapse = " ",
       "</small>"
       )

    }

  }

  attr(table, "kable_meta") <- NULL

# result ------------------------------------------------------------------
# -------------------------------------------------------------------------

  return(table)

}
