# -------------------------------------------------------------------------
# Tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2026-08-05
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Packages and application-only helpers -----------------------------------
# -------------------------------------------------------------------------

suppressPackageStartupMessages({
  source("pkgs.R")
})

helper_candidates <- c(
  file.path(getwd(), "helpers.R"),
  system.file("tarpuy", "helpers.R", package = "inti")
)

helper_path <- helper_candidates[
  nzchar(helper_candidates) & file.exists(helper_candidates)
][1L]

if(is.na(helper_path) || !nzchar(helper_path)) {
  stop("TARPUY could not find inst/tarpuy/helpers.R.", call. = FALSE)
}

source(helper_path, local = TRUE)

options(
  "googleAuthR.scopes.selected" = c(
    "https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/userinfo.email"
  )
)

options(gargle_oob_default = TRUE)
options(shiny.port = 1221)

if(file.exists("www/cloud.json")) {
  gar_set_client(
    web_json = "www/cloud.json",
    activate = "web"
  )
}

# -------------------------------------------------------------------------
# Server ------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  # -----------------------------------------------------------------------
  # Internal utilities -----------------------------------------------------
  # -----------------------------------------------------------------------

  normalize_design_type_app <- function(x) {
    tryCatch(
      {
        normalizer <- getFromNamespace(
          "normalize_tarpuy_design_type",
          "inti"
        )
        normalizer(x)
      },
      error = function(e) {
        value <- tolower(trimws(as.character(x)[1L]))
        value <- gsub("[[:space:]_]+", "-", value)
        aliases <- c(
          "dca" = "crd",
          "dbca" = "rcbd",
          "splitplot-rcbd" = "split-rcbd",
          "split-plot-rcbd" = "split-rcbd"
        )
        if(value %in% names(aliases)) unname(aliases[[value]]) else value
      }
    )
  }

  nonempty_scalar <- function(x, default = NULL) {
    if(
      is.null(x) ||
      length(x) == 0L ||
      is.na(x[[1L]]) ||
      !nzchar(trimws(as.character(x[[1L]])))
    ) {
      return(default)
    }

    trimws(as.character(x[[1L]]))
  }

  input_or <- function(id, default = NULL) {
    value <- input[[id]]

    if(
      is.null(value) ||
      length(value) == 0L ||
      (length(value) == 1L && is.na(value)) ||
      (length(value) == 1L && is.character(value) && !nzchar(trimws(value)))
    ) {
      return(default)
    }

    value
  }

  design_display_name <- function(x) {
    design_type <- normalize_design_type_app(x)

    switch(
      design_type,
      "crd" = "CRD",
      "rcbd" = "RCBD",
      "augmented" = "Augmented",
      "split-rcbd" = "Splitplot-RCBD",
      as.character(x)[1L]
    )
  }

  sheet_names_safe <- function() {
    googlesheets4::sheet_names(gs())
  }

  notify_error <- function(error, duration = 10) {
    showNotification(
      conditionMessage(error),
      type = "error",
      duration = duration
    )
  }


  # -----------------------------------------------------------------------
  # Protect the structural design column in generated fieldbooks ----------
  # -----------------------------------------------------------------------

  tarpuy_design_protection_prefix <- function() {
    "TARPUY structural column: design"
  }

  get_sheet_protected_ranges <- function(ssid, sheet_id) {
    request <- googlesheets4::request_generate(
      "sheets.spreadsheets.get",
      params = list(
        spreadsheetId = ssid,
        fields = paste0(
          "sheets(properties(sheetId,title),",
          "protectedRanges(protectedRangeId,description,range))"
        )
      )
    )

    response <- googlesheets4::request_make(request)
    metadata <- gargle::response_process(response)
    sheets <- metadata$sheets

    if(is.null(sheets) || length(sheets) == 0L) {
      return(list())
    }

    matching_sheet <- Filter(
      function(sheet) {
        properties <- sheet$properties
        !is.null(properties) &&
          !is.null(properties$sheetId) &&
          identical(as.integer(properties$sheetId), as.integer(sheet_id))
      },
      sheets
    )

    if(length(matching_sheet) == 0L) {
      return(list())
    }

    protected_ranges <- matching_sheet[[1L]]$protectedRanges
    if(is.null(protected_ranges)) list() else protected_ranges
  }

  protect_fieldbook_design_column <- function(data, sheet_name) {
    if(!is.data.frame(data) || !"design" %in% names(data)) {
      stop(
        "The fieldbook does not contain the structural 'design' column.",
        call. = FALSE
      )
    }

    spreadsheet_id <- googlesheets4::as_sheets_id(gs())
    spreadsheet <- googlesheets4::gs4_get(spreadsheet_id)
    sheet_position <- match(sheet_name, spreadsheet$sheets$name)

    if(is.na(sheet_position)) {
      stop("Fieldbook sheet not found: ", sheet_name, call. = FALSE)
    }

    sheet_id <- as.integer(spreadsheet$sheets$id[[sheet_position]])
    design_start <- as.integer(match("design", names(data)) - 1L)
    design_end <- design_start + 1L
    protection_prefix <- tarpuy_design_protection_prefix()
    protection_description <- paste0(
      protection_prefix,
      " — generated by TARPUY; do not edit"
    )

    editor_email <- tryCatch(
      suppressMessages(googlesheets4::gs4_user()),
      error = function(e) NULL
    )
    editor_email <- as.character(editor_email)
    editor_email <- editor_email[
      !is.na(editor_email) & nzchar(trimws(editor_email))
    ]

    if(length(editor_email) == 0L) {
      stop(
        "TARPUY could not identify the authenticated Google account required to protect the 'design' column.",
        call. = FALSE
      )
    }

    existing_ranges <- get_sheet_protected_ranges(
      ssid = spreadsheet_id,
      sheet_id = sheet_id
    )

    tarpuy_ranges <- Filter(
      function(protected_range) {
        description <- protected_range$description
        range <- protected_range$range

        !is.null(description) &&
          startsWith(as.character(description), protection_prefix) &&
          !is.null(range) &&
          !is.null(range$sheetId) &&
          identical(as.integer(range$sheetId), sheet_id)
      },
      existing_ranges
    )

    requests <- list()

    # Remove an earlier TARPUY protection and ensure its former column remains
    # visible. This is necessary when the number of experimental factors changes
    # and therefore moves the 'design' column to another position.
    for(protected_range in tarpuy_ranges) {
      old_range <- protected_range$range
      old_start <- old_range$startColumnIndex
      old_end <- old_range$endColumnIndex

      if(
        !is.null(old_start) &&
        !is.null(old_end) &&
        is.finite(as.numeric(old_start)) &&
        is.finite(as.numeric(old_end))
      ) {
        requests[[length(requests) + 1L]] <- list(
          updateDimensionProperties = list(
            range = list(
              sheetId = sheet_id,
              dimension = "COLUMNS",
              startIndex = as.integer(old_start),
              endIndex = as.integer(old_end)
            ),
            properties = list(hiddenByUser = FALSE),
            fields = "hiddenByUser"
          )
        )
      }

      if(!is.null(protected_range$protectedRangeId)) {
        requests[[length(requests) + 1L]] <- list(
          deleteProtectedRange = list(
            protectedRangeId = as.integer(
              protected_range$protectedRangeId
            )
          )
        )
      }
    }

    # Keep the current 'design' column visible. Only its editing permissions
    # are restricted; users can still inspect the design type in Google Sheets.
    requests[[length(requests) + 1L]] <- list(
      updateDimensionProperties = list(
        range = list(
          sheetId = sheet_id,
          dimension = "COLUMNS",
          startIndex = design_start,
          endIndex = design_end
        ),
        properties = list(hiddenByUser = FALSE),
        fields = "hiddenByUser"
      )
    )

    # Only the account currently running TARPUY can edit this structural
    # column. Other spreadsheet collaborators cannot change it accidentally.
    requests[[length(requests) + 1L]] <- list(
      addProtectedRange = list(
        protectedRange = list(
          range = list(
            sheetId = sheet_id,
            startColumnIndex = design_start,
            endColumnIndex = design_end
          ),
          description = protection_description,
          warningOnly = FALSE,
          editors = list(
            users = editor_email,
            domainUsersCanEdit = FALSE
          )
        )
      )
    )

    request <- googlesheets4::request_generate(
      "sheets.spreadsheets.batchUpdate",
      params = list(
        spreadsheetId = spreadsheet_id,
        requests = requests,
        responseIncludeGridData = FALSE
      )
    )

    response <- googlesheets4::request_make(request)
    gargle::response_process(response)

    invisible(TRUE)
  }


  # -----------------------------------------------------------------------
  # Internal Trait identity and metadata ----------------------------------
  # -----------------------------------------------------------------------

  tarpuy_trait_metadata_sheet <- function() {
    "_tarpuy_traits_meta"
  }

  tarpuy_trait_id_protection_prefix <- function() {
    "TARPUY internal column: _trait_id"
  }

  tarpuy_trait_metadata_protection_prefix <- function() {
    "TARPUY internal sheet: _tarpuy_traits_meta"
  }

  tarpuy_column_letter <- function(index) {
    index <- as.integer(index)

    if(length(index) != 1L || is.na(index) || index < 1L) {
      stop("Column index must be one positive integer.", call. = FALSE)
    }

    letters <- character(0)

    while(index > 0L) {
      remainder <- (index - 1L) %% 26L
      letters <- c(LETTERS[[remainder + 1L]], letters)
      index <- (index - 1L) %/% 26L
    }

    paste0(letters, collapse = "")
  }

  tarpuy_authenticated_editor <- function() {
    editor <- tryCatch(
      suppressMessages(googlesheets4::gs4_user()),
      error = function(e) NULL
    )
    editor <- trimws(as.character(editor))
    editor <- editor[!is.na(editor) & nzchar(editor)]

    if(length(editor) == 0L) {
      stop(
        "TARPUY could not identify the authenticated Google account.",
        call. = FALSE
      )
    }

    editor[[1L]]
  }

  tarpuy_sheet_id <- function(sheet_name) {
    spreadsheet <- googlesheets4::gs4_get(gs())
    position <- match(sheet_name, spreadsheet$sheets$name)

    if(is.na(position)) {
      stop("Sheet not found: ", sheet_name, call. = FALSE)
    }

    as.integer(spreadsheet$sheets$id[[position]])
  }


  ensure_sheet_grid_capacity <- function(
      sheet_name,
      min_rows = 1L,
      min_columns = 1L
  ) {
    min_rows <- suppressWarnings(as.integer(min_rows))
    min_columns <- suppressWarnings(as.integer(min_columns))

    if(
      length(min_rows) != 1L ||
      is.na(min_rows) ||
      min_rows < 1L ||
      length(min_columns) != 1L ||
      is.na(min_columns) ||
      min_columns < 1L
    ) {
      stop(
        "Minimum sheet rows and columns must be positive integers.",
        call. = FALSE
      )
    }

    spreadsheet_id <- googlesheets4::as_sheets_id(gs())
    sheet_id <- tarpuy_sheet_id(sheet_name)

    request <- googlesheets4::request_generate(
      "sheets.spreadsheets.get",
      params = list(
        spreadsheetId = spreadsheet_id,
        fields = paste0(
          "sheets(properties(sheetId,title,",
          "gridProperties(rowCount,columnCount)))"
        )
      )
    )

    response <- googlesheets4::request_make(request)
    metadata <- gargle::response_process(response)
    sheets <- metadata$sheets

    matching <- Filter(
      function(sheet) {
        properties <- sheet$properties
        !is.null(properties) &&
          !is.null(properties$sheetId) &&
          identical(as.integer(properties$sheetId), sheet_id)
      },
      sheets
    )

    if(length(matching) == 0L) {
      stop("Sheet grid properties were not found: ", sheet_name, call. = FALSE)
    }

    grid <- matching[[1L]]$properties$gridProperties
    current_rows <- suppressWarnings(as.integer(grid$rowCount))
    current_columns <- suppressWarnings(as.integer(grid$columnCount))

    if(length(current_rows) == 0L || is.na(current_rows)) {
      current_rows <- 0L
    }

    if(length(current_columns) == 0L || is.na(current_columns)) {
      current_columns <- 0L
    }

    requests <- list()

    if(current_rows < min_rows) {
      requests[[length(requests) + 1L]] <- list(
        appendDimension = list(
          sheetId = sheet_id,
          dimension = "ROWS",
          length = as.integer(min_rows - current_rows)
        )
      )
    }

    if(current_columns < min_columns) {
      requests[[length(requests) + 1L]] <- list(
        appendDimension = list(
          sheetId = sheet_id,
          dimension = "COLUMNS",
          length = as.integer(min_columns - current_columns)
        )
      )
    }

    if(length(requests) == 0L) {
      return(invisible(FALSE))
    }

    request <- googlesheets4::request_generate(
      "sheets.spreadsheets.batchUpdate",
      params = list(
        spreadsheetId = spreadsheet_id,
        requests = requests,
        responseIncludeGridData = FALSE
      )
    )

    response <- googlesheets4::request_make(request)
    gargle::response_process(response)
    invisible(TRUE)
  }


  remove_internal_trait_id_protection <- function(sheet_name) {
    spreadsheet_id <- googlesheets4::as_sheets_id(gs())
    sheet_id <- tarpuy_sheet_id(sheet_name)
    prefix <- tarpuy_trait_id_protection_prefix()
    existing_ranges <- get_sheet_protected_ranges(
      ssid = spreadsheet_id,
      sheet_id = sheet_id
    )

    matching <- Filter(
      function(protected_range) {
        description <- protected_range$description
        !is.null(description) && startsWith(as.character(description), prefix)
      },
      existing_ranges
    )

    requests <- lapply(
      matching,
      function(protected_range) {
        if(is.null(protected_range$protectedRangeId)) {
          return(NULL)
        }

        list(
          deleteProtectedRange = list(
            protectedRangeId = as.integer(protected_range$protectedRangeId)
          )
        )
      }
    )
    requests <- Filter(Negate(is.null), requests)

    if(length(requests) == 0L) {
      return(invisible(FALSE))
    }

    request <- googlesheets4::request_generate(
      "sheets.spreadsheets.batchUpdate",
      params = list(
        spreadsheetId = spreadsheet_id,
        requests = requests,
        responseIncludeGridData = FALSE
      )
    )

    response <- googlesheets4::request_make(request)
    gargle::response_process(response)
    invisible(TRUE)
  }

  protect_internal_trait_id_column <- function(
      sheet_name,
      column_index
  ) {
    spreadsheet_id <- googlesheets4::as_sheets_id(gs())
    sheet_id <- tarpuy_sheet_id(sheet_name)
    start_index <- as.integer(column_index - 1L)
    end_index <- start_index + 1L
    prefix <- tarpuy_trait_id_protection_prefix()
    editor <- tarpuy_authenticated_editor()

    existing_ranges <- get_sheet_protected_ranges(
      ssid = spreadsheet_id,
      sheet_id = sheet_id
    )

    matching <- Filter(
      function(protected_range) {
        description <- protected_range$description
        !is.null(description) && startsWith(as.character(description), prefix)
      },
      existing_ranges
    )

    requests <- list()

    for(protected_range in matching) {
      if(!is.null(protected_range$protectedRangeId)) {
        requests[[length(requests) + 1L]] <- list(
          deleteProtectedRange = list(
            protectedRangeId = as.integer(protected_range$protectedRangeId)
          )
        )
      }
    }

    requests[[length(requests) + 1L]] <- list(
      updateDimensionProperties = list(
        range = list(
          sheetId = sheet_id,
          dimension = "COLUMNS",
          startIndex = start_index,
          endIndex = end_index
        ),
        properties = list(hiddenByUser = TRUE),
        fields = "hiddenByUser"
      )
    )

    requests[[length(requests) + 1L]] <- list(
      addProtectedRange = list(
        protectedRange = list(
          range = list(
            sheetId = sheet_id,
            startColumnIndex = start_index,
            endColumnIndex = end_index
          ),
          description = paste0(prefix, " — managed automatically by TARPUY"),
          warningOnly = FALSE,
          editors = list(
            users = editor,
            domainUsersCanEdit = FALSE
          )
        )
      )
    )

    request <- googlesheets4::request_generate(
      "sheets.spreadsheets.batchUpdate",
      params = list(
        spreadsheetId = spreadsheet_id,
        requests = requests,
        responseIncludeGridData = FALSE
      )
    )

    response <- googlesheets4::request_make(request)
    gargle::response_process(response)
    invisible(TRUE)
  }

  write_trait_ids_to_sheet <- function(data, sheet_name, column_index) {
    remove_internal_trait_id_protection(sheet_name)

    # The standard Traits template contains eight columns. The technical
    # `_trait_id` column is normally written as the ninth column. Google Sheets
    # rejects a write outside the current grid, so extend the worksheet before
    # clearing or writing that column. Rows are also extended when the Traits
    # table grows beyond the sheet's current row capacity.
    ensure_sheet_grid_capacity(
      sheet_name = sheet_name,
      min_rows = max(1L, nrow(data) + 1L),
      min_columns = as.integer(column_index)
    )

    column_letter <- tarpuy_column_letter(column_index)
    column_range <- paste0(column_letter, "1:", column_letter)

    googlesheets4::range_clear(
      ss = gs(),
      sheet = sheet_name,
      range = column_range
    )

    id_data <- data.frame(
      `_trait_id` = as.character(data[["_trait_id"]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    googlesheets4::range_write(
      ss = gs(),
      data = id_data,
      sheet = sheet_name,
      range = paste0(column_letter, "1"),
      col_names = TRUE,
      reformat = FALSE
    )

    protect_internal_trait_id_column(
      sheet_name = sheet_name,
      column_index = column_index
    )

    invisible(TRUE)
  }

  prepare_traits_sheet_tracking <- function(data, sheet_name) {
    prepared <- tarpuy_prepare_trait_ids(data)
    prepared$sheet_name <- sheet_name
    prepared
  }

  read_trait_metadata <- function() {
    sheet_name <- tarpuy_trait_metadata_sheet()

    if(!sheet_name %in% sheet_names_safe()) {
      return(tarpuy_empty_trait_metadata())
    }

    metadata <- tryCatch(
      googlesheets4::range_read(
        ss = gs(),
        sheet = sheet_name,
        col_types = "c"
      ),
      error = function(e) {
        stop(
          "TARPUY could not read its internal Trait metadata sheet: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    if(ncol(metadata) == 0L) {
      return(tarpuy_empty_trait_metadata())
    }

    required_columns <- names(tarpuy_empty_trait_metadata())
    if(!all(required_columns %in% names(metadata))) {
      stop(
        "The reserved sheet '_tarpuy_traits_meta' already exists but does not contain valid TARPUY metadata. Rename or remove that sheet before generating the fieldbook.",
        call. = FALSE
      )
    }

    tarpuy_normalize_trait_metadata(metadata)
  }

  protect_trait_metadata_sheet <- function(sheet_name) {
    spreadsheet_id <- googlesheets4::as_sheets_id(gs())
    sheet_id <- tarpuy_sheet_id(sheet_name)
    prefix <- tarpuy_trait_metadata_protection_prefix()
    editor <- tarpuy_authenticated_editor()
    existing_ranges <- get_sheet_protected_ranges(
      ssid = spreadsheet_id,
      sheet_id = sheet_id
    )

    matching <- Filter(
      function(protected_range) {
        description <- protected_range$description
        !is.null(description) && startsWith(as.character(description), prefix)
      },
      existing_ranges
    )

    requests <- list()

    for(protected_range in matching) {
      if(!is.null(protected_range$protectedRangeId)) {
        requests[[length(requests) + 1L]] <- list(
          deleteProtectedRange = list(
            protectedRangeId = as.integer(protected_range$protectedRangeId)
          )
        )
      }
    }

    requests[[length(requests) + 1L]] <- list(
      updateSheetProperties = list(
        properties = list(
          sheetId = sheet_id,
          hidden = TRUE
        ),
        fields = "hidden"
      )
    )

    requests[[length(requests) + 1L]] <- list(
      addProtectedRange = list(
        protectedRange = list(
          range = list(sheetId = sheet_id),
          description = paste0(prefix, " — managed automatically by TARPUY"),
          warningOnly = FALSE,
          editors = list(
            users = editor,
            domainUsersCanEdit = FALSE
          )
        )
      )
    )

    request <- googlesheets4::request_generate(
      "sheets.spreadsheets.batchUpdate",
      params = list(
        spreadsheetId = spreadsheet_id,
        requests = requests,
        responseIncludeGridData = FALSE
      )
    )

    response <- googlesheets4::request_make(request)
    gargle::response_process(response)
    invisible(TRUE)
  }

  write_trait_metadata <- function(metadata) {
    sheet_name <- tarpuy_trait_metadata_sheet()
    metadata <- tarpuy_normalize_trait_metadata(metadata)

    if(!sheet_name %in% sheet_names_safe()) {
      googlesheets4::sheet_add(
        ss = gs(),
        sheet = sheet_name
      )
    }

    # Hide and protect the technical worksheet before writing metadata, so a
    # failed write never leaves an internal table exposed as a normal sheet.
    protect_trait_metadata_sheet(sheet_name)

    googlesheets4::write_sheet(
      data = metadata,
      ss = gs(),
      sheet = sheet_name
    )

    invisible(metadata)
  }

  scope_candidate_trait_metadata <- function(
      metadata,
      fieldbook_sheet,
      traits_sheet
  ) {
    metadata <- as.data.frame(
      metadata,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if(nrow(metadata) == 0L) {
      return(tarpuy_empty_trait_metadata())
    }

    out <- data.frame(
      fieldbook_sheet = rep(fieldbook_sheet, nrow(metadata)),
      traits_sheet = rep(if(is.null(traits_sheet)) "" else traits_sheet, nrow(metadata)),
      trait_id = as.character(metadata$trait_id),
      generated_column = as.character(metadata$generated_column),
      generated_index = as.integer(metadata$generated_index),
      status = rep("active", nrow(metadata)),
      updated_at = rep(format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), nrow(metadata)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    tarpuy_normalize_trait_metadata(out)
  }

  fieldbook_trait_metadata <- function(metadata, sheet_name) {
    metadata <- tarpuy_normalize_trait_metadata(metadata)
    metadata[
      metadata$fieldbook_sheet == sheet_name,
      ,
      drop = FALSE
    ]
  }

  replace_fieldbook_trait_metadata <- function(sheet_name, scoped_metadata) {
    all_metadata <- read_trait_metadata()
    retained <- all_metadata[
      all_metadata$fieldbook_sheet != sheet_name,
      ,
      drop = FALSE
    ]

    scoped_metadata <- tarpuy_normalize_trait_metadata(scoped_metadata)
    combined <- rbind(retained, scoped_metadata)
    write_trait_metadata(combined)
  }

  # -----------------------------------------------------------------------
  # Close local session automatically -------------------------------------
  # -----------------------------------------------------------------------

  observe({
    if(Sys.getenv("SHINY_PORT") == "") {
      session$onSessionEnded(stopApp)
    }
  })

  # -----------------------------------------------------------------------
  # Authentication ---------------------------------------------------------
  # -----------------------------------------------------------------------

  source("www/auth.R")

  if(file.exists("www/analytics.r")) {
    source("www/analytics.r", local = TRUE)
  }

  gar_shiny_auth(session)

  access_token <- moduleServer(
    id = "js_token",
    module = googleAuth_js
  )

  output$login <- renderUI({
    if(file.exists("www/cloud.json")) {
      googleAuth_jsUI(
        "js_token",
        login_text = "LogIn",
        logout_text = "LogOut"
      )
    } else {
      actionButton(
        inputId = "local_user",
        label = "Local",
        class = "btn-success"
      )
    }
  })

  # -----------------------------------------------------------------------
  # Google Sheet URL and connection ---------------------------------------
  # -----------------------------------------------------------------------

  fieldbook_url <- reactive({
    validate(
      need(input$fieldbook_url, "LogIn and insert a url")
    )

    input$fieldbook_url
  })

  gs <- reactive({
    if(Sys.getenv("SHINY_PORT") == "") {
      gs4_auth(TRUE)
    } else {
      gs4_auth(
        scopes = "https://www.googleapis.com/auth/spreadsheets",
        cache = FALSE,
        use_oob = TRUE,
        token = access_token()
      )
    }

    validate(
      need(gs4_has_token(), "LogIn and insert a url")
    )

    as_sheets_id(fieldbook_url())
  })

  # -----------------------------------------------------------------------
  # Create and open Google Sheet ------------------------------------------
  # -----------------------------------------------------------------------

  gs_created <- NULL
  makeReactiveBinding("gs_created")

  observeEvent(input$create_sheet, {
    tryCatch(
      {
        if(Sys.getenv("SHINY_PORT") == "") {
          gs4_auth(TRUE)
        } else {
          gs4_auth(
            scopes = "https://www.googleapis.com/auth/spreadsheets",
            cache = FALSE,
            use_oob = TRUE,
            token = access_token()
          )
        }

        validate(
          need(gs4_has_token(), "LogIn and insert a url")
        )

        gs_created <<- gs4_create(
          name = paste("Tarpuy", format(Sys.time(), "%Y-%m-%d  %H:%M")),
          sheets = "tarpuy",
          locale = "en_US"
        )

        sheet_url <- paste0(
          "https://docs.google.com/spreadsheets/d/",
          gs_created %>% purrr::pluck(1)
        )

        updateTextInput(
          session = session,
          inputId = "fieldbook_url",
          value = sheet_url
        )
      },
      error = notify_error
    )
  })

  output$open_url <- renderUI({
    link <- nonempty_scalar(
      input$fieldbook_url,
      "https://docs.google.com/spreadsheets/u/0/"
    )

    tags$a(
      href = link,
      target = "_blank",
      rel = "noopener noreferrer",
      class = "btn btn-success",
      style = "width:80%;",
      role = "button",
      "Open"
    )
  })

  # -----------------------------------------------------------------------
  # PLEX module ------------------------------------------------------------
  # -----------------------------------------------------------------------

  # Transitional defaults. ui.R will define the same defaults in the next
  # phase, but these updates keep the current frontend consistent meanwhile.
  session$onFlushed(
    function() {
      advanced_fields <- c(
        "institutions",
        "researchers",
        "altitude",
        "georeferencing",
        "project",
        "album"
      )

      current_fields <- isolate(input$plex_fields)
      if(is.null(current_fields)) {
        current_fields <- c(
          "manager",
          "location",
          "dates",
          "environment",
          "repository",
          "manuscript"
        )
      }

      updateCheckboxGroupInput(
        session,
        "plex_fields",
        selected = unique(c(current_fields, advanced_fields))
      )

      current_sheets <- isolate(input$plex_sheets)
      if(is.null(current_sheets)) {
        current_sheets <- c("logbook", "matrix", "budget", "credit")
      }

      updateCheckboxGroupInput(
        session,
        "plex_sheets",
        selected = setdiff(current_sheets, "budget")
      )
    },
    once = TRUE
  )

  output$plex_factor_selector <- renderUI({
    numericInput(
      inputId = "plex_nfactors",
      label = "Factors number",
      value = 1,
      min = 1,
      max = 5,
      step = 1,
      width = "100%"
    )
  })

  output$plex_design_selector <- renderUI({
    req(input$plex_nfactors)

    design_choices <- if(input$plex_nfactors == 1) {
      c(
        "CRD" = "crd",
        "RCBD" = "rcbd"
      )
    } else if(input$plex_nfactors == 2) {
      c(
        "CRD" = "crd",
        "RCBD" = "rcbd",
        "Augmented" = "augmented",
        "Splitplot-RCBD" = "split-rcbd"
      )
    } else {
      c(
        "CRD" = "crd",
        "RCBD" = "rcbd"
      )
    }

    selectizeInput(
      inputId = "plex_design",
      label = "Design type",
      choices = design_choices,
      selected = design_choices[[1L]],
      multiple = FALSE,
      width = "100%"
    )
  })

  output$plex_design_parameters <- renderUI({
    req(input$plex_design)

    tagList(
      if(input$plex_design != "augmented") {
        numericInput(
          inputId = "plex_rep",
          label = "Replications",
          value = 3,
          min = 1,
          step = 1,
          width = "100%"
        )
      },
      if(input$plex_design == "augmented") {
        tagList(
          numericInput(
            inputId = "plex_eu_block",
            label = "Experimental units by block",
            value = NA,
            min = 1,
            step = 1,
            width = "100%"
          ),
          selectInput(
            inputId = "plex_random",
            label = "Randomization",
            choices = c("TRUE", "FALSE"),
            selected = "TRUE",
            width = "100%"
          )
        )
      }
    )
  })

  design_type <- reactive({
    req(input$plex_design)
    normalize_design_type_app(input$plex_design)
  })

  is_augmented <- reactive({
    identical(design_type(), "augmented")
  })

  plex_dates <- reactive({
    values <- input$plex_dates

    if(is.null(values) || length(values) < 2L) {
      return(c(as.Date(NA), as.Date(NA)))
    }

    start <- suppressWarnings(as.Date(values[[1L]]))
    end <- suppressWarnings(as.Date(values[[2L]]))

    validate(
      need(
        is.na(start) || is.na(end) || end >= start,
        "The end date must be equal to or later than the start date."
      )
    )

    c(start, end)
  })

  plex <- reactive({
    req(input$plex_design)

    dates <- plex_dates()

    # QR structure is an internal TARPUY setting. It is intentionally not
    # exposed in PLEX because users should not need to edit the template.
    qrcode_value <- "{project}{plots}"

    common_args <- list(
      data = NULL,
      title = input$plex_title,
      short_title = input$plex_short_title,
      objective = input$plex_objective,
      references = input$plex_references,
      plan = input_or("plex_plan", NULL),
      institutions = input$plex_institutions,
      researchers = input$plex_researchers,
      manager = input$plex_manager,
      location = input$plex_location,
      altitude = input$plex_altitude,
      georeferencing = input$plex_georeferencing,
      environment = input$plex_environment,
      start = dates[[1L]],
      end = dates[[2L]],
      album = input$plex_album,
      project = input$plex_project,
      repository = input$plex_repository,
      manuscript = input$plex_manuscript,
      design = design_type(),
      serie = input$plex_serie,
      seed = input$plex_seed,
      zigzag = as.logical(input$plex_zigzag),
      qrcode = qrcode_value
    )

    if(is_augmented()) {
      do.call(
        tarpuy_plex,
        c(
          common_args,
          list(
            nfactor = input$plex_nfactors,
            rep = NA,
            nrows = NA,
            aug_blocks = input_or("plex_blocks", NA),
            aug_eu_block = input$plex_eu_block,
            aug_random = as.logical(input$plex_random)
          )
        )
      )
    } else {
      do.call(
        tarpuy_plex,
        c(
          common_args,
          list(
            nfactor = input$plex_nfactors,
            rep = input$plex_rep,
            nrows = NA
          )
        )
      )
    }
  })

  sheets_refresh <- reactiveVal(0L)

  plex_core_sheet_names <- reactive({
    c(
      info = sanitize_sheet_name(input$gsheet_info, fallback = "info"),
      traits = sanitize_sheet_name(input$gsheet_varlist, fallback = "traits"),
      design = sanitize_sheet_name(input$gsheet_design, fallback = "design")
    )
  })

  output$plex_sheets2create <- renderUI({
    sheets <- plex_core_sheet_names()

    checkboxGroupInput(
      inputId = "plex_sheet2create",
      label = NULL,
      choices = c(
        "Information" = unname(sheets[["info"]]),
        "Traits" = unname(sheets[["traits"]]),
        "Design" = unname(sheets[["design"]])
      ),
      selected = unname(sheets),
      inline = TRUE
    )
  })

  create_plex_sheet <- function(ss, sheet_name, data) {
    existing <- googlesheets4::sheet_names(ss)

    if(sheet_name %in% existing) {
      return("exists")
    }

    googlesheets4::sheet_add(
      ss = ss,
      sheet = sheet_name
    )

    tryCatch(
      {
        googlesheets4::sheet_write(
          data = data,
          ss = ss,
          sheet = sheet_name
        )
      },
      error = function(e) {
        try(
          googlesheets4::sheet_delete(
            ss = ss,
            sheet = sheet_name
          ),
          silent = TRUE
        )
        stop(e)
      }
    )

    "created"
  }

  observeEvent(input$plex_generate, {
    tryCatch(
      {
        validate(
          need(input$fieldbook_url, "LogIn and create or insert a url")
        )

        plex_object <- plex()
        core_names <- plex_core_sheet_names()

        validate(
          need(
            !anyDuplicated(unname(core_names)),
            "Information, Traits and Design must use different sheet names."
          )
        )

        selected_core <- input$plex_sheet2create
        selected_core <- if(is.null(selected_core)) character(0) else selected_core

        selected_names <- c(
          selected_core,
          input$plex_sheets
        )
        selected_names <- selected_names[
          !is.na(selected_names) & nzchar(trimws(selected_names))
        ]

        validate(
          need(
            !anyDuplicated(selected_names),
            "Each selected PLEX sheet must have a unique name."
          )
        )

        jobs <- list()

        if(unname(core_names[["info"]]) %in% selected_core) {
          jobs[[length(jobs) + 1L]] <- list(
            name = unname(core_names[["info"]]),
            data = plex_object$plex
          )
        }

        if(unname(core_names[["traits"]]) %in% selected_core) {
          jobs[[length(jobs) + 1L]] <- list(
            name = unname(core_names[["traits"]]),
            data = plex_object$variables
          )
        }

        if(unname(core_names[["design"]]) %in% selected_core) {
          jobs[[length(jobs) + 1L]] <- list(
            name = unname(core_names[["design"]]),
            data = plex_object$design
          )
        }

        extra_objects <- list(
          logbook = plex_object$logbook,
          timetable = plex_object$timetable,
          budget = plex_object$budget,
          matrix = plex_object$matrix,
          credit = plex_object$credit
        )

        selected_extra <- input$plex_sheets
        selected_extra <- if(is.null(selected_extra)) character(0) else selected_extra

        for(extra_name in names(extra_objects)) {
          if(extra_name %in% selected_extra) {
            jobs[[length(jobs) + 1L]] <- list(
              name = sanitize_sheet_name(extra_name, fallback = extra_name),
              data = extra_objects[[extra_name]]
            )
          }
        }

        validate(
          need(length(jobs) > 0L, "Select at least one sheet to create.")
        )

        results <- character(0)

        for(job in jobs) {
          status <- create_plex_sheet(
            ss = gs(),
            sheet_name = job$name,
            data = job$data
          )
          results[[job$name]] <- status
        }

        current_sheets <- googlesheets4::sheet_names(gs())
        if(
          "tarpuy" %in% current_sheets &&
          length(setdiff(current_sheets, "tarpuy")) > 0L
        ) {
          googlesheets4::sheet_delete(
            ss = gs(),
            sheet = "tarpuy"
          )
        }

        sheets_refresh(sheets_refresh() + 1L)

        created <- names(results)[results == "created"]
        existing <- names(results)[results == "exists"]

        message <- c(
          if(length(created) > 0L) {
            paste("Created:", paste(created, collapse = ", "))
          },
          if(length(existing) > 0L) {
            paste("Already existed:", paste(existing, collapse = ", "))
          }
        )

        showNotification(
          paste(message, collapse = ". "),
          type = "message",
          duration = 8
        )
      },
      error = notify_error
    )
  })

  # -----------------------------------------------------------------------
  # Fieldbook module -------------------------------------------------------
  # -----------------------------------------------------------------------

  fieldbook_generated <- reactiveVal(NULL)
  fieldbook_preview_data <- reactiveVal(NULL)
  fieldbook_detected_sheet <- reactiveVal(NULL)
  fieldbook_warning <- reactiveVal(NULL)
  pending_fieldbook_write <- reactiveVal(NULL)
  pending_trait_reconciliation <- reactiveVal(NULL)
  design_preview_refresh <- reactiveVal(0L)

  # Explicit versions force Shiny to rebuild outputs after the user requests
  # a fresh read from Google Sheets, even when the selected sheet name remains
  # unchanged.
  fieldbook_preview_version <- reactiveVal(0L)
  sketch_refresh_version <- reactiveVal(0L)

  fieldbook_sheet_name <- reactive({
    sanitize_sheet_name(input$fb2export, fallback = "fb")
  })

  fieldbook_sheet_name_debounced <- debounce(
    fieldbook_sheet_name,
    millis = 400
  )

  set_fieldbook_state <- function(data = NULL,
                                  sheet_name = NULL,
                                  warning = NULL) {
    fieldbook_generated(data)
    fieldbook_preview_data(data)
    fieldbook_detected_sheet(sheet_name)
    fieldbook_warning(warning)
  }

  load_fieldbook_sheet <- function(sheet_name) {
    fb_existing <- googlesheets4::range_read(
      ss = gs(),
      sheet = sheet_name
    )

    if(!is_valid_fieldbook_sheet(fb_existing)) {
      set_fieldbook_state(
        warning = paste0(
          "Sheet '", sheet_name,
          "' does not have a valid TARPUY fieldbook structure."
        )
      )
      return(NULL)
    }

    set_fieldbook_state(
      data = fb_existing,
      sheet_name = sheet_name,
      warning = NULL
    )

    fieldbook_preview_version(fieldbook_preview_version() + 1L)

    fb_existing
  }

  observeEvent(
    list(
      input$fieldbook_url,
      fieldbook_sheet_name_debounced(),
      sheets_refresh()
    ),
    {
      if(
        is.null(input$fieldbook_url) ||
        !nzchar(trimws(input$fieldbook_url))
      ) {
        return(invisible(NULL))
      }

      tryCatch(
        {
          sheet_export <- fieldbook_sheet_name_debounced()
          current_sheets <- sheet_names_safe()

          if(sheet_export %in% current_sheets) {
            load_fieldbook_sheet(sheet_export)
          } else {
            set_fieldbook_state(
              warning = paste0(
                "Sheet '", sheet_export,
                "' was not found. Generate a fieldbook or verify Sheet export."
              )
            )
          }
        },
        error = function(e) {
          set_fieldbook_state(warning = conditionMessage(e))
        }
      )
    },
    ignoreInit = FALSE
  )

  observeEvent(input$refresh_fieldbook_preview, {
    tryCatch(
      {
        req(input$fieldbook_url)
        sheet_export <- fieldbook_sheet_name()

        if(sheet_export %in% sheet_names_safe()) {
          load_fieldbook_sheet(sheet_export)
          showNotification(
            paste0("Fieldbook Preview updated from sheet '", sheet_export, "'."),
            type = "message",
            duration = 4
          )
        } else {
          set_fieldbook_state(
            warning = paste0(
              "Sheet '", sheet_export,
              "' was not found. Verify Sheet export."
            )
          )
        }
      },
      error = function(e) {
        set_fieldbook_state(warning = conditionMessage(e))
      }
    )
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Design sheet preview ---------------------------------------------------
  # -----------------------------------------------------------------------

  gsheet_design <- reactive({
    sheets_refresh()
    design_preview_refresh()

    validate(
      need(input$fieldbook_url, "LogIn and create or insert a url")
    )

    info <- googlesheets4::gs4_get(gs())
    design_sheet <- sanitize_sheet_name(
      input$gsheet_design,
      fallback = "design"
    )

    validate(
      need(
        design_sheet %in% info$sheets$name,
        paste("Sheet not found:", design_sheet)
      )
    )

    id <- info$sheets %>%
      dplyr::filter(.data$name %in% design_sheet) %>%
      purrr::pluck("id")

    paste0(
      info$spreadsheet_url,
      "#gid=",
      id,
      "&refresh=",
      design_preview_refresh()
    )
  })

  output$gsheet_preview_design <- renderUI({
    tags$div(
      class = "gsheet-preview-wrapper gsheet-preview-wrapper--fieldbook",
      tags$iframe(
        src = gsheet_design(),
        class = "gsheet-preview-frame",
        title = "Design sheet preview"
      )
    )
  })

  # -----------------------------------------------------------------------
  # Build and write fieldbook ---------------------------------------------
  # -----------------------------------------------------------------------

  build_fieldbook_candidate <- function() {
    design_sheet <- sanitize_sheet_name(
      input$gsheet_design,
      fallback = "design"
    )
    traits_sheet <- sanitize_sheet_name(
      input$gsheet_varlist,
      fallback = "traits"
    )

    current_sheets <- sheet_names_safe()

    if(!design_sheet %in% current_sheets) {
      stop("Design sheet not found: ", design_sheet, call. = FALSE)
    }

    design_data <- googlesheets4::range_read(
      ss = gs(),
      sheet = design_sheet
    )

    if(!is.data.frame(design_data) || nrow(design_data) == 0L) {
      stop("The design sheet is empty.", call. = FALSE)
    }

    base_fieldbook <- tarpuy_design(design_data)

    if(!is_valid_fieldbook_sheet(base_fieldbook)) {
      stop(
        "The design sheet did not generate a valid TARPUY fieldbook.",
        call. = FALSE
      )
    }

    variables <- NULL
    traits_tracking <- NULL

    if(traits_sheet %in% current_sheets) {
      traits_data <- googlesheets4::range_read(
        ss = gs(),
        sheet = traits_sheet,
        col_types = "c"
      )

      if(nrow(traits_data) > 0L) {
        if(!is_valid_traits_sheet(traits_data)) {
          stop(
            "The Traits sheet must contain variable, {trait}, {when}, ",
            "{samples}, {format}, units, details and categories as applicable.",
            call. = FALSE
          )
        }

        traits_tracking <- prepare_traits_sheet_tracking(
          data = traits_data,
          sheet_name = traits_sheet
        )
        variables <- traits_tracking$data
      }
    }

    result <- tarpuy_traits(
      fieldbook = base_fieldbook,
      last_factor = NULL,
      traits = variables
    )

    if(!is_valid_fieldbook_sheet(result$fieldbook)) {
      stop(
        "The generated fieldbook failed structural validation.",
        call. = FALSE
      )
    }

    list(
      base = base_fieldbook,
      full = result$fieldbook,
      traits = result$traits,
      mobile = result$fb,
      trait_metadata = result$metadata,
      traits_tracking = traits_tracking,
      design_sheet = design_sheet,
      traits_sheet = if(traits_sheet %in% current_sheets) traits_sheet else NULL
    )
  }

  ensure_sketch_sheet <- function(after_sheet) {
    current_sheets <- sheet_names_safe()

    if("sketch" %in% current_sheets) {
      return(invisible(FALSE))
    }

    if(!after_sheet %in% current_sheets) {
      return(invisible(FALSE))
    }

    created <- tryCatch(
      {
        googlesheets4::sheet_add(
          ss = gs(),
          sheet = "sketch",
          .after = after_sheet
        )
        TRUE
      },
      error = function(e) {
        showNotification(
          paste0(
            "The fieldbook was generated, but the 'sketch' sheet could not be created: ",
            conditionMessage(e)
          ),
          type = "warning",
          duration = 10
        )
        FALSE
      }
    )

    invisible(created)
  }

  commit_fieldbook <- function(
      data,
      sheet_name,
      message,
      trait_metadata = NULL,
      traits_tracking = NULL
  ) {
    session$sendCustomMessage(
      "tarpuy:set-loading",
      list(selector = "#fieldbook_preview", loading = TRUE)
    )

    on.exit(
      session$sendCustomMessage(
        "tarpuy:set-loading",
        list(selector = "#fieldbook_preview", loading = FALSE)
      ),
      add = TRUE
    )

    if(!is.null(traits_tracking)) {
      write_trait_ids_to_sheet(
        data = traits_tracking$data,
        sheet_name = traits_tracking$sheet_name,
        column_index = traits_tracking$column_index
      )
    }

    googlesheets4::write_sheet(
      data = data,
      ss = gs(),
      sheet = sheet_name
    )

    protection_applied <- tryCatch(
      {
        protect_fieldbook_design_column(
          data = data,
          sheet_name = sheet_name
        )
        TRUE
      },
      error = function(e) {
        showNotification(
          paste0(
            "The fieldbook was written, but the structural 'design' column could not be protected: ",
            conditionMessage(e)
          ),
          type = "warning",
          duration = 12
        )
        FALSE
      }
    )

    if(!is.null(trait_metadata)) {
      tryCatch(
        replace_fieldbook_trait_metadata(
          sheet_name = sheet_name,
          scoped_metadata = trait_metadata
        ),
        error = function(e) {
          showNotification(
            paste0(
              "The fieldbook was written, but TARPUY could not update its internal Trait metadata: ",
              conditionMessage(e)
            ),
            type = "warning",
            duration = 14
          )
        }
      )
    }

    set_fieldbook_state(
      data = data,
      sheet_name = sheet_name,
      warning = NULL
    )

    ensure_sketch_sheet(after_sheet = sheet_name)

    design_preview_refresh(design_preview_refresh() + 1L)
    sheets_refresh(sheets_refresh() + 1L)

    showNotification(
      message,
      type = "message",
      duration = 8
    )

    invisible(data)
  }

  show_destructive_fieldbook_modal <- function(
      sheet_name,
      existing,
      candidate,
      trait_metadata,
      traits_tracking
  ) {
    structural <- detect_structural_columns(existing)
    extra_columns <- setdiff(names(existing), structural)
    populated_columns <- extra_columns[vapply(
      extra_columns,
      function(column_name) {
        has_recorded_data(existing, columns = column_name)
      },
      logical(1L)
    )]

    pending_fieldbook_write(
      list(
        sheet = sheet_name,
        data = candidate,
        trait_metadata = trait_metadata,
        traits_tracking = traits_tracking
      )
    )

    showModal(
      modalDialog(
        title = tagList(
          icon("triangle-exclamation"),
          " Experimental design changes detected"
        ),
        class = "tarpuy-destructive-modal",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            "cancel_fieldbook_overwrite",
            "Cancel",
            class = "btn btn-default"
          ),
          actionButton(
            "confirm_fieldbook_overwrite",
            "Overwrite and delete data",
            class = "btn btn-danger"
          )
        ),
        tags$div(
          class = "tarpuy-warning tarpuy-warning-danger",
          tags$p(
            tags$strong("The experimental design is not the same as the existing fieldbook.")
          ),
          tags$p(
            "Changing factors, levels, treatments, blocks, replications, seed-based allocation or design type requires replacing the complete fieldbook."
          ),
          tags$p(
            "The current sheet contains recorded values or manually created columns. Overwriting will permanently remove those data from this sheet."
          ),
          if(length(populated_columns) > 0L) {
            tags$p(
              tags$strong("Columns with data: "),
              paste(populated_columns, collapse = ", ")
            )
          },
          tags$p(
            "Recommended action: cancel and enter another name in Sheet export to generate the modified design in a new sheet."
          )
        )
      )
    )
  }


  show_trait_reconciliation_modal <- function(
      sheet_name,
      existing,
      candidate,
      old_metadata,
      new_metadata,
      plan
  ) {
    rename_ids <- character(nrow(plan$renames))
    rename_items <- vector("list", nrow(plan$renames))

    if(nrow(plan$renames) > 0L) {
      for(i in seq_len(nrow(plan$renames))) {
        item <- plan$renames[i, , drop = FALSE]
        count_text <- if(item$value_count[[1L]] == 0L) {
          "empty column"
        } else {
          paste0(item$value_count[[1L]], " recorded values")
        }

        if(isTRUE(item$conflict[[1L]])) {
          rename_items[[i]] <- tags$div(
            class = "tarpuy-warning tarpuy-warning-danger",
            tags$strong("Column-name conflict: "),
            tags$code(item$old_column[[1L]]),
            " cannot be renamed to ",
            tags$code(item$new_column[[1L]]),
            " because the target column already exists in the fieldbook."
          )
          next
        }

        input_id <- paste0("trait_rename_", i)
        rename_ids[[i]] <- input_id
        rename_items[[i]] <- checkboxInput(
          inputId = input_id,
          value = TRUE,
          label = tagList(
            tags$code(item$old_column[[1L]]),
            " → ",
            tags$code(item$new_column[[1L]]),
            tags$span(
              class = "tarpuy-help-text",
              paste0(" (", count_text, "; values will be preserved)")
            )
          )
        )
      }
    }

    obsolete_ids <- character(nrow(plan$obsolete))
    obsolete_items <- vector("list", nrow(plan$obsolete))

    if(nrow(plan$obsolete) > 0L) {
      for(i in seq_len(nrow(plan$obsolete))) {
        item <- plan$obsolete[i, , drop = FALSE]
        has_values <- item$value_count[[1L]] > 0L
        reason_text <- if(identical(item$reason[[1L]], "deleted_trait")) {
          "Trait removed from the Traits sheet"
        } else {
          "column no longer generated after changing moments or samples"
        }

        if(isTRUE(item$conflict[[1L]])) {
          obsolete_items[[i]] <- tags$div(
            class = "tarpuy-warning tarpuy-warning-danger",
            tags$strong("Column-name conflict: "),
            tags$code(item$old_column[[1L]]),
            " belongs to a removed or reduced Trait but is also requested by a different active Trait."
          )
          next
        }

        input_id <- paste0("trait_delete_", i)
        obsolete_ids[[i]] <- input_id
        obsolete_items[[i]] <- checkboxInput(
          inputId = input_id,
          value = !has_values,
          label = tagList(
            if(has_values) {
              icon("triangle-exclamation")
            },
            tags$code(item$old_column[[1L]]),
            tags$span(
              class = "tarpuy-help-text",
              paste0(
                " — ", reason_text, "; ",
                if(has_values) {
                  paste0(item$value_count[[1L]], " recorded values. Unchecked by default.")
                } else {
                  "empty. Checked for deletion by default."
                }
              )
            )
          )
        )
      }
    }

    has_conflicts <- (nrow(plan$renames) > 0L && any(plan$renames$conflict)) ||
      (nrow(plan$obsolete) > 0L && any(plan$obsolete$conflict))

    pending_trait_reconciliation(
      list(
        sheet = sheet_name,
        existing = existing,
        candidate = candidate,
        old_metadata = old_metadata,
        new_metadata = new_metadata,
        plan = plan,
        rename_ids = rename_ids,
        obsolete_ids = obsolete_ids,
        has_conflicts = has_conflicts
      )
    )

    footer <- if(has_conflicts) {
      tagList(
        actionButton(
          "cancel_trait_reconciliation",
          "Cancel",
          class = "btn btn-default"
        )
      )
    } else {
      tagList(
        actionButton(
          "cancel_trait_reconciliation",
          "Cancel",
          class = "btn btn-default"
        ),
        actionButton(
          "keep_historical_trait_columns",
          "Keep historical columns",
          class = "btn btn-warning"
        ),
        actionButton(
          "apply_trait_reconciliation",
          "Apply selected changes",
          class = "btn btn-primary"
        )
      )
    }

    showModal(
      modalDialog(
        title = tagList(
          icon("table"),
          " Trait changes detected"
        ),
        size = "l",
        easyClose = FALSE,
        footer = footer,
        tags$p(
          "TARPUY identified these changes through the internal Trait ID. ",
          "Manually created columns are not included and will be preserved."
        ),
        if(length(rename_items) > 0L) {
          tagList(
            tags$h4("Renamed Trait columns"),
            tags$p(
              class = "tarpuy-help-text",
              "Checked items will be renamed and their values will remain attached to the same qrcode."
            ),
            tagList(rename_items)
          )
        },
        if(length(obsolete_items) > 0L) {
          tagList(
            tags$hr(),
            tags$h4("Columns no longer generated"),
            tags$p(
              class = "tarpuy-help-text",
              "Checked items will be deleted. Unchecked items will remain in the fieldbook as historical columns."
            ),
            tagList(obsolete_items)
          )
        },
        if(has_conflicts) {
          tagList(
            tags$hr(),
            tags$p(
              tags$strong("No update was performed. "),
              "Resolve the target-column conflict in the fieldbook or Traits sheet and generate again."
            )
          )
        }
      )
    )
  }

  apply_trait_reconciliation_decisions <- function(
      pending,
      rename_selected,
      delete_selected,
      message
  ) {
    plan <- pending$plan

    rename_rows <- if(length(rename_selected) == 0L) {
      integer(0)
    } else {
      which(rename_selected)
    }
    delete_rows <- if(length(delete_selected) == 0L) {
      integer(0)
    } else {
      which(delete_selected)
    }

    rename_map <- character(0)
    renamed_sources <- character(0)

    if(length(rename_rows) > 0L) {
      selected <- plan$renames[rename_rows, , drop = FALSE]
      selected <- selected[!selected$conflict, , drop = FALSE]

      if(nrow(selected) > 0L) {
        rename_map <- selected$new_column
        names(rename_map) <- selected$old_column
        renamed_sources <- selected$old_column
      }
    }

    deleted_columns <- if(length(delete_rows) > 0L) {
      plan$obsolete$old_column[delete_rows]
    } else {
      character(0)
    }

    synchronized <- tarpuy_reconcile_trait_columns(
      existing = pending$existing,
      new = pending$candidate$full,
      rename_map = rename_map,
      delete_columns = deleted_columns
    )

    # Verify the selected rename transaction before writing either the
    # fieldbook or the internal metadata. This prevents TARPUY from reporting a
    # successful rename when the old columns were actually retained and the new
    # columns merely appended.
    if(length(rename_map) > 0L) {
      retained_sources <- intersect(names(rename_map), names(synchronized))
      missing_targets <- setdiff(unname(rename_map), names(synchronized))

      if(length(retained_sources) > 0L || length(missing_targets) > 0L) {
        details <- c(
          if(length(retained_sources) > 0L) {
            paste0(
              "old columns still present: ",
              paste(retained_sources, collapse = ", ")
            )
          },
          if(length(missing_targets) > 0L) {
            paste0(
              "new columns missing: ",
              paste(missing_targets, collapse = ", ")
            )
          }
        )

        stop(
          "Trait rename verification failed (",
          paste(details, collapse = "; "),
          "). No fieldbook or metadata changes were written.",
          call. = FALSE
        )
      }
    }

    final_metadata <- tarpuy_finalize_trait_metadata(
      old_metadata = pending$old_metadata,
      new_metadata = pending$new_metadata,
      renamed_sources = renamed_sources,
      deleted_columns = deleted_columns
    )

    commit_fieldbook(
      data = synchronized,
      sheet_name = pending$sheet,
      message = message,
      trait_metadata = final_metadata,
      traits_tracking = pending$candidate$traits_tracking
    )

    invisible(synchronized)
  }

  observeEvent(input$export_design, {
    pending_fieldbook_write(NULL)
    pending_trait_reconciliation(NULL)

    tryCatch(
      {
        validate(
          need(input$fieldbook_url, "LogIn and create or insert a url")
        )

        candidate <- build_fieldbook_candidate()
        sheet_export <- fieldbook_sheet_name()
        candidate_metadata <- scope_candidate_trait_metadata(
          metadata = candidate$trait_metadata,
          fieldbook_sheet = sheet_export,
          traits_sheet = candidate$traits_sheet
        )

        source_sheets <- unique(c(
          sanitize_sheet_name(input$gsheet_info, fallback = "info"),
          candidate$design_sheet,
          candidate$traits_sheet
        ))
        source_sheets <- source_sheets[
          !is.na(source_sheets) & nzchar(source_sheets)
        ]

        validate(
          need(
            !sheet_export %in% source_sheets,
            "Sheet export must be different from the Information, Design and Traits sheet names."
          )
        )

        current_sheets <- sheet_names_safe()
        sheet_exists <- sheet_export %in% current_sheets
        overwrite <- identical(input$export_design_overwrite, "yes")

        if(!sheet_exists) {
          commit_fieldbook(
            data = candidate$full,
            sheet_name = sheet_export,
            message = paste0("Fieldbook created in sheet '", sheet_export, "'."),
            trait_metadata = candidate_metadata,
            traits_tracking = candidate$traits_tracking
          )
          return(invisible(NULL))
        }

        existing <- googlesheets4::range_read(
          ss = gs(),
          sheet = sheet_export
        )

        if(!overwrite) {
          if(is_valid_fieldbook_sheet(existing)) {
            set_fieldbook_state(
              data = existing,
              sheet_name = sheet_export,
              warning = paste0(
                "Sheet '", sheet_export,
                "' already exists and Overwrite is set to no. No changes were applied."
              )
            )
            tryCatch(
              protect_fieldbook_design_column(
                data = existing,
                sheet_name = sheet_export
              ),
              error = function(e) {
                showNotification(
                  paste0(
                    "The existing fieldbook was loaded, but the structural 'design' column could not be protected: ",
                    conditionMessage(e)
                  ),
                  type = "warning",
                  duration = 12
                )
              }
            )
            ensure_sketch_sheet(after_sheet = sheet_export)
            sheets_refresh(sheets_refresh() + 1L)
          } else {
            set_fieldbook_state(
              warning = paste0(
                "Sheet '", sheet_export,
                "' already exists but is not a valid TARPUY fieldbook. No changes were applied."
              )
            )
          }
          return(invisible(NULL))
        }

        all_metadata <- read_trait_metadata()
        old_metadata <- fieldbook_trait_metadata(
          metadata = all_metadata,
          sheet_name = sheet_export
        )

        if(same_tarpuy_design(existing, candidate$full)) {
          plan <- tarpuy_trait_change_plan(
            old_metadata = old_metadata,
            new_metadata = candidate_metadata,
            existing = existing
          )

          if(isTRUE(plan$has_changes)) {
            show_trait_reconciliation_modal(
              sheet_name = sheet_export,
              existing = existing,
              candidate = candidate,
              old_metadata = old_metadata,
              new_metadata = candidate_metadata,
              plan = plan
            )
            return(invisible(NULL))
          }

          synchronized <- tarpuy_reconcile_trait_columns(
            existing = existing,
            new = candidate$full
          )

          final_metadata <- tarpuy_finalize_trait_metadata(
            old_metadata = old_metadata,
            new_metadata = candidate_metadata
          )

          metadata_message <- if(nrow(old_metadata) == 0L && nrow(candidate_metadata) > 0L) {
            " Internal Trait tracking was initialized; existing extra columns were preserved."
          } else {
            ""
          }

          commit_fieldbook(
            data = synchronized,
            sheet_name = sheet_export,
            message = paste0(
              "Fieldbook '", sheet_export,
              "' updated. Existing observations and manual columns were preserved.",
              metadata_message
            ),
            trait_metadata = final_metadata,
            traits_tracking = candidate$traits_tracking
          )
          return(invisible(NULL))
        }

        if(has_recorded_data(existing)) {
          show_destructive_fieldbook_modal(
            sheet_name = sheet_export,
            existing = existing,
            candidate = candidate$full,
            trait_metadata = candidate_metadata,
            traits_tracking = candidate$traits_tracking
          )
          return(invisible(NULL))
        }

        commit_fieldbook(
          data = candidate$full,
          sheet_name = sheet_export,
          message = paste0(
            "The design changed and fieldbook '", sheet_export,
            "' was regenerated because no recorded data were found."
          ),
          trait_metadata = candidate_metadata,
          traits_tracking = candidate$traits_tracking
        )
      },
      error = notify_error
    )
  })

  observeEvent(input$confirm_fieldbook_overwrite, {
    pending <- pending_fieldbook_write()
    req(pending)

    removeModal()

    tryCatch(
      {
        commit_fieldbook(
          data = pending$data,
          sheet_name = pending$sheet,
          message = paste0(
            "Fieldbook '", pending$sheet,
            "' was replaced after confirmation."
          ),
          trait_metadata = pending$trait_metadata,
          traits_tracking = pending$traits_tracking
        )
        pending_fieldbook_write(NULL)
      },
      error = notify_error
    )
  })

  observeEvent(input$cancel_fieldbook_overwrite, {
    pending_fieldbook_write(NULL)
    removeModal()
    showNotification(
      "Fieldbook replacement cancelled. No data were changed.",
      type = "warning",
      duration = 6
    )
  })


  observeEvent(input$apply_trait_reconciliation, {
    pending <- pending_trait_reconciliation()
    req(pending)

    if(isTRUE(pending$has_conflicts)) {
      showNotification(
        "Trait changes were not applied because a target-column conflict exists.",
        type = "error",
        duration = 10
      )
      return(invisible(NULL))
    }

    rename_selected <- if(length(pending$rename_ids) == 0L) {
      logical(0)
    } else {
      vapply(
        pending$rename_ids,
        function(id) nzchar(id) && isTRUE(input[[id]]),
        logical(1L)
      )
    }

    delete_selected <- if(length(pending$obsolete_ids) == 0L) {
      logical(0)
    } else {
      vapply(
        pending$obsolete_ids,
        function(id) nzchar(id) && isTRUE(input[[id]]),
        logical(1L)
      )
    }

    removeModal()

    tryCatch(
      {
        apply_trait_reconciliation_decisions(
          pending = pending,
          rename_selected = rename_selected,
          delete_selected = delete_selected,
          message = paste0(
            "Fieldbook '", pending$sheet,
            "' updated with the selected Trait changes. Manual columns were preserved."
          )
        )
        pending_trait_reconciliation(NULL)
      },
      error = notify_error
    )
  })

  observeEvent(input$keep_historical_trait_columns, {
    pending <- pending_trait_reconciliation()
    req(pending)

    if(isTRUE(pending$has_conflicts)) {
      return(invisible(NULL))
    }

    removeModal()

    tryCatch(
      {
        apply_trait_reconciliation_decisions(
          pending = pending,
          rename_selected = rep(FALSE, nrow(pending$plan$renames)),
          delete_selected = rep(FALSE, nrow(pending$plan$obsolete)),
          message = paste0(
            "Fieldbook '", pending$sheet,
            "' updated. Previous Trait columns were retained as historical columns and new columns were added."
          )
        )
        pending_trait_reconciliation(NULL)
      },
      error = notify_error
    )
  })

  observeEvent(input$cancel_trait_reconciliation, {
    pending_trait_reconciliation(NULL)
    removeModal()
    showNotification(
      "Trait update cancelled. No fieldbook columns were changed.",
      type = "warning",
      duration = 7
    )
  })

  # -----------------------------------------------------------------------
  # Fieldbook status, preview and dynamic summary -------------------------
  # -----------------------------------------------------------------------

  output$fieldbook_status <- renderUI({
    if(!is.null(fieldbook_warning())) {
      return(
        tags$div(
          class = "tarpuy-status tarpuy-status-warning",
          icon("triangle-exclamation"),
          " ",
          fieldbook_warning()
        )
      )
    }

    fb <- fieldbook_generated()

    if(is.null(fb)) {
      return(tags$p("No fieldbook generated yet."))
    }

    tagList(
      tags$div(
        class = "fieldbook-status-available",
        icon("check"),
        tags$span("Fieldbook available")
      ),
      tags$div(
        class = "fieldbook-status-metrics",
        tags$div(
          class = "fieldbook-status-metric",
          tags$span(class = "fieldbook-status-label", "Rows"),
          tags$strong(class = "fieldbook-status-value", nrow(fb))
        ),
        tags$div(
          class = "fieldbook-status-metric",
          tags$span(class = "fieldbook-status-label", "Columns"),
          tags$strong(class = "fieldbook-status-value", ncol(fb))
        ),
        tags$div(
          class = "fieldbook-status-metric",
          tags$span(class = "fieldbook-status-label", "Sheet"),
          tags$strong(
            class = "fieldbook-status-value",
            fieldbook_detected_sheet()
          )
        )
      )
    )
  })

  output$fieldbook_preview <- DT::renderDT({
    fieldbook_preview_version()
    preview_data <- fieldbook_preview_data()
    req(preview_data)

    # Preserve exactly the same column order returned by Google Sheets or by
    # the newly generated fieldbook. Experimental factors, structural columns,
    # Traits and manually added columns therefore appear in the preview in the
    # same sequence as in the source worksheet.

    preview_widths <- c(
      qrcode = 300L,
      plots = 90L,
      ntreat = 90L,
      entry = 220L,
      type = 90L,
      checks = 90L
    )

    width_columns <- intersect(names(preview_widths), names(preview_data))
    column_defs <- lapply(
      width_columns,
      function(column_name) {
        list(
          width = paste0(preview_widths[[column_name]], "px"),
          targets = match(column_name, names(preview_data)) - 1L
        )
      }
    )

    DT::datatable(
      preview_data,
      rownames = FALSE,
      class = "stripe hover compact nowrap",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollCollapse = FALSE,
        autoWidth = TRUE,
        columnDefs = c(
          column_defs,
          list(list(width = "110px", targets = "_all"))
        )
      )
    )
  })

  output$fieldbook_summary <- renderUI({
    fb <- fieldbook_preview_data()

    if(is.null(fb)) {
      return(tags$p("Generate the fieldbook to view layout summary."))
    }

    summary_data <- build_layout_summary(fb)

    if(!is.data.frame(summary_data) || nrow(summary_data) == 0L) {
      return(tags$p("The layout summary could not be calculated."))
    }

    header_cells <- lapply(
      names(summary_data),
      function(column_name) tags$th(column_name, scope = "col")
    )

    summary_values <- unname(as.list(summary_data[1L, , drop = FALSE]))
    value_cells <- lapply(
      summary_values,
      function(value) tags$td(as.character(value[[1L]]))
    )

    header_row <- tags$tr(htmltools::tagList(header_cells))
    value_row <- tags$tr(htmltools::tagList(value_cells))

    tags$div(
      class = "tarpuy-summary-scroll",
      tabindex = "0",
      `aria-label` = "Fieldbook layout summary",
      tags$table(
        class = "tarpuy-summary-table",
        tags$thead(header_row),
        tags$tbody(value_row)
      )
    )
  })

  # -----------------------------------------------------------------------
  # Sheet catalog used by Sketch and Mobile -------------------------------
  # -----------------------------------------------------------------------

  sheet_catalog <- reactiveVal(
    list(
      fieldbooks = character(0),
      traits = character(0),
      errors = character(0)
    )
  )

  refresh_sheet_catalog <- function() {
    names_available <- googlesheets4::sheet_names(gs())
    fieldbook_sheets <- character(0)
    traits_sheets <- character(0)
    errors <- character(0)

    for(sheet_name in names_available) {
      data <- tryCatch(
        googlesheets4::range_read(
          ss = gs(),
          sheet = sheet_name,
          col_types = "c"
        ),
        error = function(e) e
      )

      if(inherits(data, "error")) {
        errors[[sheet_name]] <- conditionMessage(data)
        next
      }

      if(is_valid_fieldbook_sheet(data)) {
        fieldbook_sheets <- c(fieldbook_sheets, sheet_name)
      }

      if(is_valid_traits_sheet(data)) {
        traits_sheets <- c(traits_sheets, sheet_name)
      }
    }

    sheet_catalog(
      list(
        fieldbooks = unique(fieldbook_sheets),
        traits = unique(traits_sheets),
        errors = errors
      )
    )
  }

  fieldbook_url_debounced <- debounce(
    reactive(input$fieldbook_url),
    millis = 600
  )

  observeEvent(
    list(
      fieldbook_url_debounced(),
      input$update_sketch,
      sheets_refresh()
    ),
    {
      # Force a new read of the selected fieldbook and a complete redraw of
      # Sketch whenever Refresh is pressed or the workbook source changes.
      sketch_refresh_version(sketch_refresh_version() + 1L)

      if(
        is.null(input$fieldbook_url) ||
        !nzchar(trimws(input$fieldbook_url))
      ) {
        sheet_catalog(
          list(
            fieldbooks = character(0),
            traits = character(0),
            errors = character(0)
          )
        )
        return(invisible(NULL))
      }

      tryCatch(
        refresh_sheet_catalog(),
        error = function(e) {
          sheet_catalog(
            list(
              fieldbooks = character(0),
              traits = character(0),
              errors = conditionMessage(e)
            )
          )
        }
      )
    },
    ignoreInit = FALSE
  )

  # -----------------------------------------------------------------------
  # Sketch module ----------------------------------------------------------
  # -----------------------------------------------------------------------

  output$sketch_sheets <- renderUI({
    choices <- sheet_catalog()$fieldbooks
    preferred <- fieldbook_sheet_name()
    current <- isolate(input$sketch_sheets)

    selected <- if(preferred %in% choices) {
      preferred
    } else if(!is.null(current) && current %in% choices) {
      current
    } else if(length(choices) > 0L) {
      choices[[1L]]
    } else {
      ""
    }

    selectizeInput(
      inputId = "sketch_sheets",
      label = "Fieldbook",
      choices = c("choose" = "", choices),
      selected = selected,
      multiple = FALSE,
      width = "100%"
    )
  })

  fb_sketch <- reactive({
    sketch_refresh_version()
    req(input$sketch_sheets)

    validate(
      need(
        input$sketch_sheets %in% sheet_catalog()$fieldbooks,
        "Select a valid fieldbook sheet."
      )
    )

    data <- googlesheets4::range_read(
      ss = gs(),
      sheet = input$sketch_sheets
    )

    validate(
      need(
        is_valid_fieldbook_sheet(data),
        "The selected sheet is not a valid TARPUY fieldbook."
      )
    )

    data
  })

  gsheet_fb <- reactive({
    sketch_refresh_version()
    req(input$sketch_sheets)

    info <- googlesheets4::gs4_get(gs())

    validate(
      need(
        input$sketch_sheets %in% info$sheets$name,
        paste("Sheet not found:", input$sketch_sheets)
      )
    )

    id <- info$sheets %>%
      dplyr::filter(.data$name %in% input$sketch_sheets) %>%
      purrr::pluck("id")

    paste0(
      info$spreadsheet_url,
      "#gid=",
      id,
      "&tarpuy_refresh=",
      sketch_refresh_version()
    )
  })

  output$gsheet_preview_sketch <- renderUI({
    if(is.null(input$sketch_sheets) || !nzchar(input$sketch_sheets)) {
      return(tags$p("Select a fieldbook to preview."))
    }

    tags$div(
      class = "gsheet-preview-wrapper",
      tags$iframe(
        src = gsheet_fb(),
        class = "gsheet-preview-frame",
        title = "Selected fieldbook sheet preview"
      )
    )
  })

  output$sketch_options <- renderUI({
    if(is.null(input$sketch_sheets) || !nzchar(input$sketch_sheets)) {
      return(NULL)
    }

    fb <- fb_sketch()
    design <- normalize_design_type_app(unique(fb$design)[1L])

    excluded <- c(
      "qrcode",
      "sort",
      "rows",
      "cols",
      "design"
    )

    choices <- setdiff(names(fb), excluded)
    choices <- choices[!grepl("^alt", choices, ignore.case = TRUE)]

    validate(
      need(length(choices) > 0L, "No valid columns are available for Sketch.")
    )

    factor_columns <- detect_factor_columns(fb)
    default_factor <- default_sketch_color(fb)

    if(is.null(default_factor) || !default_factor %in% choices) {
      default_factor <- choices[[1L]]
    }

    current_factor <- isolate(input$sketch_factor)
    selected_factor <- if(
      !is.null(current_factor) &&
      length(current_factor) == 1L &&
      current_factor %in% choices
    ) {
      current_factor
    } else {
      default_factor
    }

    default_fill <- if(identical(design, "augmented")) {
      if(all(c("plots", "entry") %in% choices)) {
        c("plots", "entry")
      } else if(all(c("plots", "ntreat") %in% choices)) {
        c("plots", "ntreat")
      } else {
        intersect(c("plots", "entry", "ntreat"), choices)
      }
    } else if(identical(design, "split-rcbd")) {
      subplot_factor <- if(length(factor_columns) >= 2L) {
        factor_columns[[2L]]
      } else {
        "wp_sp"
      }

      candidate <- c("plots", subplot_factor)
      candidate[candidate %in% choices]
    } else {
      candidate <- c(
        "plots",
        if(length(factor_columns) > 0L) factor_columns[[1L]] else "ntreat"
      )
      candidate[candidate %in% choices]
    }

    if(length(default_fill) == 0L) {
      default_fill <- choices[[1L]]
    }

    current_fill <- isolate(input$sketch_fill)
    current_fill <- as.character(current_fill)
    current_fill <- current_fill[
      !is.na(current_fill) & current_fill %in% choices
    ]
    selected_fill <- if(length(current_fill) > 0L) {
      unique(current_fill)
    } else {
      default_fill
    }

    tagList(
      selectizeInput(
        inputId = "sketch_factor",
        label = "Color by",
        multiple = FALSE,
        choices = choices,
        selected = selected_factor,
        width = "100%"
      ),
      selectizeInput(
        inputId = "sketch_fill",
        label = "Label",
        multiple = TRUE,
        choices = choices,
        selected = selected_fill,
        width = "100%"
      )
    )
  })

  output$sketch_text_options <- renderUI({
    if(is.null(input$sketch_sheets) || !nzchar(input$sketch_sheets)) {
      return(NULL)
    }

    tagList(
      tags$hr(),
      tags$div(
        class = "sketch-options-title",
        icon("font"),
        " Text size"
      ),
      selectizeInput(
        inputId = "sketch_font_size",
        label = "Font size (pt)",
        choices = c(
          6, 7, 8, 8.5, 9, 10, 11, 12, 14, 16,
          18, 20, 22, 24, 26, 28, 36, 48, 72
        ),
        selected = 8.5,
        multiple = FALSE,
        options = list(
          create = TRUE,
          persist = FALSE,
          maxItems = 1
        ),
        width = "100%"
      ),
      helpText(
        "Long labels and line wrapping are adjusted automatically to the plot cells."
      )
    )
  })

  sketch_text_size <- reactive({
    value <- suppressWarnings(as.numeric(input$sketch_font_size))

    if(length(value) == 0L || is.na(value)) {
      value <- 8.5
    }

    validate(
      need(
        length(value) == 1L &&
          is.finite(value) &&
          value >= 4 &&
          value <= 72,
        "Font size must be a number between 4 and 72 pt."
      )
    )

    value
  })

  sketch_dimensions <- reactive({
    dpi <- suppressWarnings(as.numeric(input$sketch_dpi))
    width_cm <- suppressWarnings(as.numeric(input$sketch_width))
    height_cm <- suppressWarnings(as.numeric(input$sketch_height))

    recommended <- recommended_sketch_dimensions(fb_sketch())

    if(length(dpi) == 0L || is.na(dpi)) dpi <- 300
    if(length(width_cm) == 0L || is.na(width_cm)) {
      width_cm <- recommended$width_cm
    }
    if(length(height_cm) == 0L || is.na(height_cm)) {
      height_cm <- recommended$height_cm
    }

    validate(
      need(
        is.finite(dpi) && dpi >= 72 && dpi <= 600,
        "Resolution must be between 72 and 600 dpi."
      ),
      need(
        is.finite(width_cm) && width_cm >= 5 && width_cm <= 200,
        "Width must be between 5 and 200 cm."
      ),
      need(
        is.finite(height_cm) && height_cm >= 5 && height_cm <= 200,
        "Height must be between 5 and 200 cm."
      )
    )

    list(
      dpi = dpi,
      width_cm = width_cm,
      height_cm = height_cm,
      width_in = width_cm / 2.54,
      height_in = height_cm / 2.54
    )
  })

  calculate_sketch_wrap_width <- function(
      fb,
      width_cm,
      height_cm,
      font_size_pt,
      label_columns
  ) {
    validate(
      need("cols" %in% names(fb), "The fieldbook does not contain 'cols'."),
      need("rows" %in% names(fb), "The fieldbook does not contain 'rows'.")
    )

    label_columns <- as.character(label_columns)
    label_columns <- label_columns[
      !is.na(label_columns) &
        nzchar(trimws(label_columns)) &
        label_columns %in% names(fb)
    ]

    if(length(label_columns) == 0L) {
      return(8L)
    }

    geometry <- sketch_layout_geometry(fb)
    number_cols <- max(1L, geometry$effective_columns)
    number_rows <- max(1L, geometry$effective_rows)

    # Reserve room for axes, legend, facet strips and plot margins. The
    # calculation is based on the target device size, not on PNG pixel density.
    panel_width_cm <- width_cm * 0.88
    panel_height_cm <- height_cm * 0.76
    cell_width_cm <- panel_width_cm / number_cols
    cell_height_cm <- panel_height_cm / number_rows

    average_character_cm <- font_size_pt * 0.0352778 * 0.52
    line_height_cm <- font_size_pt * 0.0352778 * 1.02

    characters_by_width <- floor(
      (cell_width_cm / average_character_cm) * 0.82
    )

    available_lines <- max(
      1L,
      floor((cell_height_cm / line_height_cm) * 0.88)
    )
    lines_per_label <- max(
      1L,
      floor(available_lines / max(length(label_columns), 1L))
    )

    label_lengths <- unlist(
      lapply(
        label_columns,
        function(column_name) {
          values <- as.character(fb[[column_name]])
          values[is.na(values)] <- ""

          if(identical(column_name, "ntreat")) {
            values <- ifelse(nzchar(values), paste0("T", values), "")
          }

          values <- gsub("_", " ", values, fixed = TRUE)
          nchar(values, type = "width", allowNA = FALSE)
        }
      ),
      use.names = FALSE
    )

    longest_label <- max(c(label_lengths, 1L), na.rm = TRUE)
    width_needed_for_height <- ceiling(longest_label / lines_per_label)

    calculated <- min(
      max(characters_by_width, width_needed_for_height),
      characters_by_width
    )

    calculated <- max(4L, min(80L, as.integer(calculated)))
    min(calculated, as.integer(longest_label))
  }

  build_sketch_plot <- function(fb, target_dimensions) {
    validate(
      need(input$sketch_factor, "Select a color factor."),
      need(input$sketch_fill, "Select at least one label."),
      need(
        input$sketch_factor %in% names(fb),
        paste("Selected color factor was not found:", input$sketch_factor)
      ),
      need(
        all(input$sketch_fill %in% names(fb)),
        paste(
          "Selected label columns were not found:",
          paste(setdiff(input$sketch_fill, names(fb)), collapse = ", ")
        )
      )
    )

    wrap_width <- calculate_sketch_wrap_width(
      fb = fb,
      width_cm = target_dimensions$width_cm,
      height_cm = target_dimensions$height_cm,
      font_size_pt = sketch_text_size(),
      label_columns = input$sketch_fill
    )

    tarpuy_plotdesign(
      data = fb,
      factor = input$sketch_factor,
      fill = input$sketch_fill,
      text_size = sketch_text_size(),
      wrap_width = wrap_width,
      font_family = "Open Sans",
      font_face = "plain"
    )
  }

  preview_sketch_dimensions <- reactive({
    dimensions <- sketch_dimensions()

    sketch_preview_dimensions(
      fieldbook = fb_sketch(),
      width_cm = dimensions$width_cm,
      height_cm = dimensions$height_cm,
      dpi = 100L
    )
  })

  preview_sketch_plot <- reactive({
    fb <- fb_sketch()
    dimensions <- preview_sketch_dimensions()
    build_sketch_plot(fb, dimensions)
  })

  write_sketch_file <- function(file, format) {
    dimensions <- sketch_dimensions()
    plot <- build_sketch_plot(fb_sketch(), dimensions)
    device_open <- FALSE

    if(identical(format, "png")) {
      grDevices::png(
        filename = file,
        width = dimensions$width_cm,
        height = dimensions$height_cm,
        units = "cm",
        res = dimensions$dpi
      )
    } else if(identical(format, "svg")) {
      grDevices::svg(
        filename = file,
        width = dimensions$width_in,
        height = dimensions$height_in,
        onefile = TRUE
      )
    } else if(identical(format, "pdf")) {
      grDevices::pdf(
        file = file,
        width = dimensions$width_in,
        height = dimensions$height_in,
        onefile = TRUE,
        useDingbats = FALSE
      )
    } else {
      stop("Unsupported sketch format: ", format, call. = FALSE)
    }

    device_open <- TRUE
    on.exit({
      if(device_open) {
        grDevices::dev.off()
      }
    }, add = TRUE)

    print(plot)
    grDevices::dev.off()
    device_open <- FALSE

    invisible(file)
  }

  output$plot_sketch <- renderImage({
    dimensions <- preview_sketch_dimensions()
    outfile <- tempfile(fileext = ".png")
    device_open <- FALSE

    grDevices::png(
      filename = outfile,
      width = dimensions$width_px,
      height = dimensions$height_px,
      units = "px",
      res = dimensions$dpi
    )

    device_open <- TRUE
    on.exit({
      if(device_open) {
        grDevices::dev.off()
      }
    }, add = TRUE)

    print(preview_sketch_plot())
    grDevices::dev.off()
    device_open <- FALSE

    list(
      src = outfile,
      contentType = "image/png",
      width = dimensions$width_px,
      height = dimensions$height_px,
      alt = "Experimental design sketch preview"
    )
  }, deleteFile = TRUE)

  output$sketch_download_png <- downloadHandler(
    filename = function() paste0("tarpuy-sketch-", Sys.Date(), ".png"),
    content = function(file) write_sketch_file(file, "png")
  )

  output$sketch_download_svg <- downloadHandler(
    filename = function() paste0("tarpuy-sketch-", Sys.Date(), ".svg"),
    content = function(file) write_sketch_file(file, "svg")
  )

  output$sketch_download_pdf <- downloadHandler(
    filename = function() paste0("tarpuy-sketch-", Sys.Date(), ".pdf"),
    content = function(file) write_sketch_file(file, "pdf")
  )

  output$sketch_modules <- renderUI({
    if(is.null(input$sketch_sheets) || !nzchar(input$sketch_sheets)) {
      return(
        tags$div(
          class = "tarpuy-empty-state",
          icon("table"),
          tags$p("Select a valid fieldbook sheet to generate the Sketch.")
        )
      )
    }

    if(identical(input$sketch_preview_opt, "Gsheet")) {
      return(uiOutput("gsheet_preview_sketch"))
    }

    recommended <- recommended_sketch_dimensions(fb_sketch())

    tagList(
      tags$div(
        class = "sketch-size-controls",
        tags$div(
          class = "sketch-size-control",
          numericInput(
            inputId = "sketch_width",
            label = "Width (cm)",
            value = recommended$width_cm,
            step = 1,
            min = 5,
            max = 200,
            width = "100%"
          )
        ),
        tags$div(
          class = "sketch-size-control",
          numericInput(
            inputId = "sketch_height",
            label = "Height (cm)",
            value = recommended$height_cm,
            step = 1,
            min = 5,
            max = 200,
            width = "100%"
          )
        ),
        tags$div(
          class = "sketch-size-control",
          numericInput(
            inputId = "sketch_dpi",
            label = "Resolution (dpi)",
            value = 300,
            step = 50,
            min = 72,
            max = 600,
            width = "100%"
          )
        )
      ),
      tags$div(
        class = "sketch-export-help",
        icon("circle-info"),
        tags$span(
          paste0(
            "Recommended size for this design: ",
            recommended$width_cm,
            " × ",
            recommended$height_cm,
            " cm. Width and height update the preview and all downloads; Resolution affects PNG only."
          )
        )
      ),
      tags$div(
        class = "sketch-downloads",
        downloadButton(
          "sketch_download_png",
          "PNG",
          icon = icon("download")
        ),
        downloadButton(
          "sketch_download_svg",
          "SVG",
          icon = icon("download")
        ),
        downloadButton(
          "sketch_download_pdf",
          "PDF",
          icon = icon("download")
        )
      ),
      tags$div(
        class = "sketch-preview-image sketch-preview-image--dimensioned",
        imageOutput(
          "plot_sketch",
          width = "auto",
          height = "auto"
        ),
        align = "center"
      )
    )
  })

  # -----------------------------------------------------------------------
  # Mobile connection module ----------------------------------------------
  # -----------------------------------------------------------------------

  output$connection_sheet_fieldbook <- renderUI({
    choices <- sheet_catalog()$fieldbooks
    preferred <- fieldbook_sheet_name()
    current <- isolate(input$connection_sheet_fieldbook)

    selected <- if(preferred %in% choices) {
      preferred
    } else if(!is.null(current) && current %in% choices) {
      current
    } else if(length(choices) > 0L) {
      choices[[1L]]
    } else {
      ""
    }

    selectInput(
      inputId = "connection_sheet_fieldbook",
      label = NULL,
      choices = c("choose" = "", choices),
      selected = selected
    )
  })

  output$connection_sheet_traits <- renderUI({
    choices <- sheet_catalog()$traits
    preferred <- sanitize_sheet_name(
      input$gsheet_varlist,
      fallback = "traits"
    )
    current <- isolate(input$connection_sheet_traits)

    selected <- if(preferred %in% choices) {
      preferred
    } else if(!is.null(current) && current %in% choices) {
      current
    } else if(length(choices) > 0L) {
      choices[[1L]]
    } else {
      ""
    }

    selectInput(
      inputId = "connection_sheet_traits",
      label = NULL,
      choices = c("choose" = "", choices),
      selected = selected
    )
  })

  mobile_traits <- reactive({
    req(input$connection_sheet_traits)

    validate(
      need(
        input$connection_sheet_traits %in% sheet_catalog()$traits,
        "Select a valid Traits sheet."
      )
    )

    data <- googlesheets4::range_read(
      ss = gs(),
      sheet = input$connection_sheet_traits,
      col_types = "c"
    )

    validate(
      need(is_valid_traits_sheet(data), "The selected Traits sheet is invalid.")
    )

    data
  })

  mobile_fieldbook <- reactive({
    req(input$connection_sheet_fieldbook)

    validate(
      need(
        input$connection_sheet_fieldbook %in% sheet_catalog()$fieldbooks,
        "Select a valid fieldbook sheet."
      )
    )

    data <- googlesheets4::range_read(
      ss = gs(),
      sheet = input$connection_sheet_fieldbook
    )

    validate(
      need(
        is_valid_fieldbook_sheet(data),
        "The selected fieldbook sheet is invalid."
      )
    )

    data
  })

  mobile_base_result <- reactive({
    fb <- mobile_fieldbook()
    base_columns <- build_mobile_columns(fb)

    validate(
      need(length(base_columns) > 0L, "No valid Mobile columns were detected.")
    )

    base <- fb[, base_columns, drop = FALSE]

    trait_result <- tarpuy_traits(
      fieldbook = base,
      last_factor = NULL,
      traits = mobile_traits()
    )

    list(
      base = base,
      base_columns = base_columns,
      trait_result = trait_result
    )
  })

  output$connection_fieldbook_lastfactor <- renderUI({
    if(
      is.null(input$connection_sheet_fieldbook) ||
      !nzchar(input$connection_sheet_fieldbook) ||
      is.null(input$connection_sheet_traits) ||
      !nzchar(input$connection_sheet_traits)
    ) {
      return(NULL)
    }

    fb <- mobile_fieldbook()
    mobile_result <- mobile_base_result()
    generated_traits <- as.character(mobile_result$trait_result$traits$trait)
    generated_traits <- generated_traits[
      !is.na(generated_traits) & nzchar(trimws(generated_traits))
    ]

    additional_choices <- setdiff(
      names(fb),
      c(mobile_result$base_columns, generated_traits)
    )

    selectizeInput(
      inputId = "connection_fieldbook_additional",
      label = "Additional columns (optional)",
      choices = additional_choices,
      selected = character(0),
      multiple = TRUE,
      width = "100%"
    )
  })

  fbapp <- reactive({
    fb <- mobile_fieldbook()
    result <- mobile_base_result()

    generated_traits <- as.character(result$trait_result$traits$trait)
    generated_traits <- generated_traits[
      !is.na(generated_traits) & nzchar(trimws(generated_traits))
    ]

    additional <- input$connection_fieldbook_additional
    additional <- if(is.null(additional)) character(0) else additional
    additional <- intersect(additional, names(fb))
    additional <- setdiff(additional, generated_traits)
    additional <- setdiff(additional, result$base_columns)

    csv_columns <- unique(c(result$base_columns, additional))
    csv_fieldbook <- fb[, csv_columns, drop = FALSE]

    list(
      fieldbook = result$trait_result$fieldbook,
      traits = result$trait_result$traits,
      fb = csv_fieldbook
    )
  })

  connection_sheet_preview_url <- reactive({
    validate(
      need(input$fieldbook_url, "LogIn and create or insert a url")
    )

    info <- googlesheets4::gs4_get(gs())

    selected_sheet <- if(identical(input$connection_sheet_preview, "Traits")) {
      input$connection_sheet_traits
    } else {
      input$connection_sheet_fieldbook
    }

    validate(
      need(selected_sheet, "Select a sheet to preview"),
      need(
        selected_sheet %in% info$sheets$name,
        paste("Sheet not found:", selected_sheet)
      )
    )

    id <- info$sheets %>%
      dplyr::filter(.data$name %in% selected_sheet) %>%
      purrr::pluck("id")

    paste0(info$spreadsheet_url, "#gid=", id)
  })

  connection_preview_tag <- reactive({
    tags$div(
      class = "gsheet-preview-wrapper",
      tags$iframe(
        src = connection_sheet_preview_url(),
        class = "gsheet-preview-frame",
        title = "Mobile source sheet preview"
      )
    )
  })

  # New, non-duplicated output ID for the corrected ui.R.
  output$connection_sheet_preview_ui <- renderUI({
    connection_preview_tag()
  })

  # Backward-compatible output for the current ui.R. The next phase will
  # replace uiOutput("connection_sheet_preview") with the new ID above.
  output$connection_sheet_preview <- renderUI({
    connection_preview_tag()
  })

  output$connection_traits_trt <- downloadHandler(
    filename = function() {
      paste0("traits-", Sys.Date(), ".trt")
    },
    content = function(con) {
      readr::write_delim(
        fbapp()$traits,
        file = con,
        delim = ",",
        quote = "all",
        na = ""
      )
    }
  )

  output$connection_fieldbook_csv <- downloadHandler(
    filename = function() {
      paste0("fieldbook-", Sys.Date(), ".csv")
    },
    content = function(con) {
      utils::write.csv(
        fbapp()$fb,
        file = con,
        row.names = FALSE,
        na = "",
        fileEncoding = "UTF-8"
      )
    }
  )

  output$connection_traits_download <- renderUI({
    validate(
      need(input$connection_sheet_traits, ""),
      need(input$connection_sheet_fieldbook, "")
    )

    downloadButton(
      outputId = "connection_traits_trt",
      label = h6("Traits"),
      icon = icon("download", "fa-2x")
    )
  })

  output$connection_fieldbook_download <- renderUI({
    validate(
      need(input$connection_sheet_traits, ""),
      need(input$connection_sheet_fieldbook, "")
    )

    downloadButton(
      outputId = "connection_fieldbook_csv",
      label = h6("FieldBook"),
      icon = icon("download", "fa-2x")
    )
  })

  # -----------------------------------------------------------------------
  # End app ---------------------------------------------------------------
  # -----------------------------------------------------------------------

})
