# -------------------------------------------------------------------------
# Tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2026-06-14
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

suppressPackageStartupMessages({
  source("pkgs.R")
})

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
  # Close local session automatically
  # -----------------------------------------------------------------------
  
  observe({
    
    if(Sys.getenv("SHINY_PORT") == "") {
      session$onSessionEnded(stopApp)
    }
    
  })
  
  # -----------------------------------------------------------------------
  # Authentication
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
  
  # -----------------------------------------------------------------------
  # Login button
  # -----------------------------------------------------------------------
  
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
  # Google Sheet URL
  # -----------------------------------------------------------------------
  
  fieldbook_url <- reactive({
    
    validate(
      need(input$fieldbook_url, "LogIn and insert a url")
    )
    
    if(input$fieldbook_url != "") {
      input$fieldbook_url
    }
    
  })
  
  # -----------------------------------------------------------------------
  # Google Sheet connection 
  # -----------------------------------------------------------------------
  
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
  # Create new Google Sheet 
  # -----------------------------------------------------------------------
  
  gs_created <- NULL
  makeReactiveBinding("gs_created")
  
  observeEvent(input$create_sheet, {
    
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
    
  })
  
  # -----------------------------------------------------------------------
  # PLEX module ------------------------------------------------------------
  # -----------------------------------------------------------------------
  
  # -----------------------------------------------------------------------
  # Factor number selector
  # -----------------------------------------------------------------------
  
  output$plex_factor_selector <- renderUI({
    
    fluidRow(
      column(
        width = 7,
        numericInput(
          inputId = "plex_nfactors",
          label = "Factors number",
          value = 1,
          max = 5,
          min = 1
        )
      )
    )
    
  })
  
  # -----------------------------------------------------------------------
  # Design selector by number of factors
  # -----------------------------------------------------------------------
  
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
        "Split-RCBD" = "split-rcbd"
        # "Strip-plot" = "strip-plot"
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
      selected = design_choices[[1]],
      multiple = FALSE
    )
    
  })
  
  # -----------------------------------------------------------------------
  # Design-specific parameters
  # -----------------------------------------------------------------------
  
  output$plex_design_parameters <- renderUI({
    
    req(input$plex_design)
    
    tagList(
      
      # Standard and split-RCBD designs use replications.
      if(input$plex_design != "augmented") {
        
        numericInput(
          inputId = "plex_rep",
          label = "Replications",
          value = 3,
          min = 1
        )
        
      },
      
      # Augmented design uses experimental units per block.
      if(input$plex_design == "augmented") {
        
        tagList(
          
          numericInput(
            inputId = "plex_block_size",
            label = "Experimental units/block",
            value = NA,
            min = 1
          ),
          
          selectInput(
            inputId = "plex_random",
            label = "Randomization",
            choices = c("TRUE", "FALSE"),
            selected = "TRUE"
          )
          
        )
        
      }
      
    )
    
  })
  
  # -----------------------------------------------------------------------
  # PLEX helpers
  # -----------------------------------------------------------------------
  
  design_type <- reactive({
    
    req(input$plex_design)
    
    input$plex_design
    
  })
  
  is_augmented <- reactive({
    
    req(input$plex_design)
    
    input$plex_design == "augmented"
    
  })
  
  # -----------------------------------------------------------------------
  # Build PLEX object
  # -----------------------------------------------------------------------
  
  plex <- reactive({
    
    req(input$plex_design)
    
    common_args <- list(
      data = NULL,
      title = input$plex_title,
      short_title = input$plex_short_title,
      objective = input$plex_objective,
      references = input$plex_references,
      plan = input$plex_plan,
      institutions = input$plex_institutions,
      researchers = input$plex_researchers,
      manager = input$plex_manager,
      location = input$plex_location,
      altitude = input$plex_altitude,
      georeferencing = input$plex_georeferencing,
      environment = input$plex_environment,
      start = input$plex_dates[1],
      end = input$plex_dates[2],
      album = input$plex_album,
      project = input$plex_project,
      repository = input$plex_repository,
      manuscript = input$plex_manuscript,
      design = input$plex_design,
      serie = input$plex_serie,
      seed = input$plex_seed,
      zigzag = as.logical(input$plex_zigzag)
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
            aug_blocks = NA,
            aug_block_size = input$plex_block_size,
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
  
  # Refresh Google Sheets-dependent previews
  sheets_refresh <- reactiveVal(0)
  
  # -----------------------------------------------------------------------
  # Sheets to create from PLEX 
  # -----------------------------------------------------------------------
  
  output$plex_sheets2create <- renderUI({
    
    sheets <- c(
      input$gsheet_info,
      input$gsheet_design,
      input$gsheet_varlist
    )
    
    checkboxGroupInput(
      inputId = "plex_sheet2create",
      label = NULL,
      choices = sheets,
      selected = sheets,
      inline = TRUE
    )
    
  })
  
  # -----------------------------------------------------------------------
  # Create PLEX sheets 
  # -----------------------------------------------------------------------
  
  observeEvent(input$plex_generate, {
    
    validate(
      need(input$fieldbook_url, "LogIn and create or insert a url")
    )
  
    # ---------------------------------------------------------------------
    # Info sheet 
    # ---------------------------------------------------------------------
    
    if(!input$gsheet_info %in% googlesheets4::sheet_names(gs()) &&
       input$gsheet_info %in% input$plex_sheet2create) {
      
      googlesheets4::sheet_add(
        ss = gs(),
        sheet = input$gsheet_info
      )
      
      plex()$plex %>%
        googlesheets4::sheet_write(
          ss = gs(),
          sheet = input$gsheet_info
        )
      
      print("sheet created: info")
      
    } else {
      
      print("sheet already exist: info")
      
    }
    
    
    # ---------------------------------------------------------------------
    # Traits sheet 
    # ---------------------------------------------------------------------
    
    if(!input$gsheet_varlist %in% googlesheets4::sheet_names(gs()) &&
       input$gsheet_varlist %in% input$plex_sheet2create) {
      
      googlesheets4::sheet_add(
        ss = gs(),
        sheet = input$gsheet_varlist,
        .after = input$gsheet_info
      )
      
      plex()$variables %>%
        googlesheets4::sheet_write(
          ss = gs(),
          sheet = input$gsheet_varlist
        )
      
      print("sheet created: traits")
      
    } else {
      
      print("sheet already exist: traits")
      
    }
    
    
    # ---------------------------------------------------------------------
    # Design sheet 
    # ---------------------------------------------------------------------
    
    if(!input$gsheet_design %in% googlesheets4::sheet_names(gs()) &&
       input$gsheet_design %in% input$plex_sheet2create) {
      
      googlesheets4::sheet_add(
        ss = gs(),
        sheet = input$gsheet_design,
        .after = input$gsheet_varlist
      )
      
      plex()$design %>%
        googlesheets4::sheet_write(
          ss = gs(),
          sheet = input$gsheet_design
        )
      
      print("sheet created: design")
      
    } else {
      
      print("sheet already exist: design")
      
    }
    
    
    # ---------------------------------------------------------------------
    # Remove default temporary sheet 
    # ---------------------------------------------------------------------
    
    if("tarpuy" %in% googlesheets4::sheet_names(gs())) {
      
      gs() %>%
        googlesheets4::sheet_delete(sheet = "tarpuy")
      
    }
    
    # ---------------------------------------------------------------------
    # Optional PLEX sheets 
    # ---------------------------------------------------------------------
    
    extra_sheets <- list(
      logbook   = plex()$logbook,
      timetable = plex()$timetable,
      budget    = plex()$budget,
      matrix    = plex()$matrix,
      credit    = plex()$credit
    )
    
    for(sheet_name in names(extra_sheets)) {
      
      if(sheet_name %in% input$plex_sheets &&
         !sheet_name %in% googlesheets4::sheet_names(gs())) {
        
        googlesheets4::sheet_add(
          ss = gs(),
          sheet = sheet_name
        )
        
        extra_sheets[[sheet_name]] %>%
          googlesheets4::sheet_write(
            ss = gs(),
            sheet = sheet_name
          )
        
        paste("sheet created:", sheet_name) %>%
          print()
        
      } else {
        
        paste("sheet already exist:", sheet_name) %>%
          print()
        
      }
      
    }
    
    sheets_refresh(sheets_refresh() + 1)
    
  })
  
  # -------------------------------------------------------------------------
  # Fieldbook module ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  fieldbook_generated <- reactiveVal(NULL)      # status + summary
  fieldbook_preview_data <- reactiveVal(NULL)   # table only
  fieldbook_detected_sheet <- reactiveVal(NULL)
  fieldbook_warning <- reactiveVal(NULL)
  
  design_preview_refresh <- reactiveVal(0)
  
  
  # -------------------------------------------------------------------------
  # Fieldbook helpers 
  # -------------------------------------------------------------------------
  
  is_fieldbook_sheet <- function(data) {
    
    "plots" %in% names(data)
    
  }
  
  
  same_fieldbook <- function(x, y) {
    
    if(is.null(x) || is.null(y)) {
      return(FALSE)
    }
    
    identical(
      list(names = names(x), nrow = nrow(x), ncol = ncol(x)),
      list(names = names(y), nrow = nrow(y), ncol = ncol(y))
    )
    
  }
  
  
  load_fieldbook_sheet <- function(sheet_name) {
    
    fb_existing <- gs() %>%
      googlesheets4::range_read(sheet = sheet_name)
    
    if(!is_fieldbook_sheet(fb_existing)) {
      
      fieldbook_warning(
        paste0(
          "Sheet '", sheet_name,
          "' does not look like a fieldbook. Check the sheet name in Sheet export."
        )
      )
      
      fieldbook_generated(NULL)
      fieldbook_preview_data(NULL)
      fieldbook_detected_sheet(NULL)
      
      return(NULL)
    }
    
    fieldbook_generated(fb_existing)
    fieldbook_preview_data(fb_existing)
    fieldbook_detected_sheet(sheet_name)
    fieldbook_warning(NULL)
    
  }
  
  
  # -------------------------------------------------------------------------
  # Existing fieldbook loader
  # -------------------------------------------------------------------------
  
  observeEvent(
    list(input$fieldbook_url, input$fb2export, sheets_refresh()),
    {
      
      req(input$fieldbook_url)
      req(input$fb2export)
      
      sheet_export <- input$fb2export %>%
        gsub("[[:space:]]", "_", .)
      
      if(sheet_export %in% googlesheets4::sheet_names(gs())) {
        
        load_fieldbook_sheet(sheet_export)
        
      } else {
        
        fieldbook_warning(
          paste0(
            "Sheet '", sheet_export,
            "' was not found. Generate a fieldbook or enter the correct fieldbook sheet name in Sheet export."
          )
        )
        
        fieldbook_generated(NULL)
        fieldbook_preview_data(NULL)
        fieldbook_detected_sheet(NULL)
        
      }
        
      
    },
    ignoreInit = FALSE
  )
  
  
  # -------------------------------------------------------------------------
  # Auto refresh fieldbook preview
  # -------------------------------------------------------------------------
  
  # auto_refresh_fieldbook <- reactiveTimer(30000, session = session)
  # 
  # observe({
  #   
  #   auto_refresh_fieldbook()
  #   
  #   req(input$fieldbook_url)
  #   req(fieldbook_detected_sheet())
  #   
  #   sheet_name <- fieldbook_detected_sheet()
  #   
  #   if(sheet_name %in% googlesheets4::sheet_names(gs())) {
  #     
  #     fb_existing <- gs() %>%
  #       googlesheets4::range_read(sheet = sheet_name)
  #     
  #     if(!same_fieldbook(fieldbook_preview_data(), fb_existing)) {
  #       
  #       fieldbook_preview_data(fb_existing)
  #       
  #     }
  #     
  #   }
  #   
  # })
  # 
  # -------------------------------------------------------------------------
  # Manual refresh fieldbook preview
  # -------------------------------------------------------------------------
  
  observeEvent(input$refresh_fieldbook_preview, {
    
    req(input$fieldbook_url)
    req(input$fb2export)
    
    sheet_export <- input$fb2export %>%
      gsub("[[:space:]]", "_", .)
    
    if(sheet_export %in% googlesheets4::sheet_names(gs())) {
      
      load_fieldbook_sheet(sheet_export)
      
    } else {
      
      fieldbook_warning(
        paste0(
          "Sheet '", sheet_export,
          "' was not found. Enter the correct fieldbook sheet name in Sheet export."
        )
      )
      
      fieldbook_generated(NULL)
      fieldbook_preview_data(NULL)
      fieldbook_detected_sheet(NULL)
      
    }
    
  })
  
  # -------------------------------------------------------------------------
  # Design sheet preview
  # -------------------------------------------------------------------------
  
  gsheet_design <- reactive({
    
    sheets_refresh()
    design_preview_refresh()
    
    validate(
      need(input$fieldbook_url, "LogIn and create or insert a url")
    )
    
    info <- googlesheets4::gs4_get(gs())
    
    validate(
      need(
        input$gsheet_design %in% info$sheets$name,
        paste("Sheet not found:", input$gsheet_design)
      )
    )
    
    url <- info$spreadsheet_url
    
    id <- info$sheets %>%
      dplyr::filter(.data$name %in% input$gsheet_design) %>%
      purrr::pluck("id")
    
    paste0(url, "#gid=", id, "&refresh=", design_preview_refresh())
    
  })
  
  
  # output$gsheet_preview_design <- renderUI({
  #   
  #   tags$iframe(
  #     src = gsheet_design(),
  #     style = "
  #     height:600px;
  #     width:100%;
  #     border:0;
  #     border-radius:8px;
  #   "
  #   )
  #   
  # })
  
  output$gsheet_preview_design <- renderUI({
    
    tags$div(
      class = "gsheet-preview-wrapper",
      
      tags$iframe(
        src = gsheet_design(),
        class = "gsheet-preview-frame"
      )
    )
    
  })
  
  
  
  # -------------------------------------------------------------------------
  # Generate and export fieldbook
  # -------------------------------------------------------------------------
  
  observeEvent(input$export_design, {
    
    validate(
      need(input$fieldbook_url, "LogIn and create or insert a url")
    )
    
    # Read design sheet ------------------------------------------------------
    
    validate(
      need(
        input$gsheet_design %in% googlesheets4::sheet_names(gs()),
        paste("Sheet not found:", input$gsheet_design)
      )
    )
    
    design_data <- gs() %>%
      googlesheets4::range_read(input$gsheet_design)
    
    
    # Read traits sheet ------------------------------------------------------
    
    variables <- NULL
    
    if(input$gsheet_varlist %in% googlesheets4::sheet_names(gs())) {
      
      variables <- gs() %>%
        googlesheets4::range_read(
          sheet = input$gsheet_varlist,
          col_types = "c"
        )
      
    }
    
    
    # Build fieldbook from the design sheet ---------------------------------
    
    fieldbook <- design_data %>%
      tarpuy_design()
    
    
    # Prepare export sheet name ---------------------------------------------
    
    sheet_export <- input$fb2export %>%
      gsub("[[:space:]]", "_", .)
    
    
    # Add traits columns and export fieldbook -------------------------------
    
    if(!is.null(fieldbook)) {
      
      fbds <- tarpuy_traits(
        fieldbook = fieldbook,
        last_factor = NULL,
        traits = variables
      )
      
      sheet_exists <- sheet_export %in% googlesheets4::sheet_names(gs())
      
      if(input$export_design_overwrite == "no" && !sheet_exists) {
        
        fbds$fieldbook %>%
          googlesheets4::write_sheet(
            ss = gs(),
            sheet = sheet_export
          )
        
        fieldbook_generated(fbds$fieldbook)
        fieldbook_preview_data(fbds$fieldbook)
        fieldbook_detected_sheet(sheet_export)
        fieldbook_warning(NULL)
        
      } else if(input$export_design_overwrite == "yes") {
        
        fbds$fieldbook %>%
          googlesheets4::write_sheet(
            ss = gs(),
            sheet = sheet_export
          )
        
        fieldbook_generated(fbds$fieldbook)
        fieldbook_preview_data(fbds$fieldbook)
        fieldbook_detected_sheet(sheet_export)
        fieldbook_warning(NULL)
        
      } else {
        
        print("sheet already exists; preview loaded from Google Sheets")
        
        fb_existing <- gs() %>%
          googlesheets4::range_read(sheet = sheet_export)
        
        fieldbook_generated(fb_existing)
        fieldbook_preview_data(fb_existing)
        fieldbook_detected_sheet(sheet_export)
        fieldbook_warning(NULL)
        
      }
      
    } else {
      
      print("Insert factor levels")
      
    }
    
    
    # Create sketch sheet if needed -----------------------------------------
    
    if(!"sketch" %in% googlesheets4::sheet_names(gs()) &&
       sheet_export %in% googlesheets4::sheet_names(gs())) {
      
      googlesheets4::sheet_add(
        ss = gs(),
        sheet = "sketch",
        .after = sheet_export
      )
      
    }
    design_preview_refresh(design_preview_refresh() + 1)
    
  })
  
  
  # -------------------------------------------------------------------------
  # Fieldbook status
  # -------------------------------------------------------------------------
  
  output$fieldbook_status <- renderUI({
    
    if(!is.null(fieldbook_warning())) {
      return(
        tags$div(
          style = "color:#b45309; font-weight:600;",
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
      tags$p(icon("check"), " Fieldbook generated"),
      tags$p(tags$strong("Rows: "), nrow(fb)),
      tags$p(tags$strong("Columns: "), ncol(fb)),
      tags$p(tags$strong("Sheet: "), fieldbook_detected_sheet())
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Fieldbook table preview
  # -------------------------------------------------------------------------
  
  output$fieldbook_preview <- DT::renderDT({
    
    req(fieldbook_preview_data())
    
    DT::datatable(
      fieldbook_preview_data(),
      rownames = FALSE,
      extensions = c("FixedColumns"),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        fixedColumns = list(leftColumns = 2),
        columnDefs = list(
          list(width = "220px", targets = 0),
          list(width = "90px", targets = 1),
          list(width = "100px", targets = "_all")
        )
      )
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Fieldbook layout summary
  # -------------------------------------------------------------------------
  
  output$fieldbook_summary <- renderUI({
    
    fb <- fieldbook_preview_data()
    
    if(is.null(fb)) {
      return(tags$p("Generate the fieldbook to view layout summary."))
    }
    
    design_name <- if("design" %in% names(fb)) unique(fb$design)[1] else NA
    nplots <- if("plots" %in% names(fb)) length(unique(fb$plots)) else nrow(fb)
    nblocks <- if("block" %in% names(fb)) length(unique(fb$block)) else NA
    ncols <- if("cols" %in% names(fb)) length(unique(fb$cols)) else NA
    
    tags$table(
      style = "width:100%;",
      
      tags$tr(
        tags$th("Design"),
        tags$th("Plots"),
        tags$th("Blocks"),
        tags$th("Cols")
      ),
      
      tags$tr(
        tags$td(design_name),
        tags$td(nplots),
        tags$td(nblocks),
        tags$td(ncols)
      )
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Sketch module ------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------
  # Google Sheet preview for selected fieldbook
  # -------------------------------------------------------------------------
  
  gsheet_fb <- reactive({
    
    validate(
      need(input$fieldbook_url, "LogIn and create or insert a url")
    )
    
    validate(
      need(input$sketch_sheets, "Insert your fieldbook")
    )
    
    info <- googlesheets4::gs4_get(gs())
    
    validate(
      need(
        input$sketch_sheets %in% info$sheets$name,
        paste("Sheet not found:", input$sketch_sheets)
      )
    )
    
    url <- info$spreadsheet_url
    
    id <- info$sheets %>%
      dplyr::filter(.data$name %in% input$sketch_sheets) %>%
      purrr::pluck("id")
    
    paste(url, id, sep = "#gid=")
    
  })
  
  
  output$gsheet_preview_sketch <- renderUI({
    
    tags$iframe(
      src = gsheet_fb(),
      style = "height:580px; width:100%; scrolling=no"
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Available fieldbook sheets
  # -------------------------------------------------------------------------
  
  sketch_sheets <- eventReactive(input$update_sketch, {
    
    googlesheets4::sheet_names(gs())
    
  })
  
  
  output$sketch_sheets <- renderUI({
    
    selectizeInput(
      inputId = "sketch_sheets",
      label = "Fieldbook",
      choices = c(
        "choose" = "",
        sketch_sheets()
      ),
      multiple = FALSE
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Read selected fieldbook
  # -------------------------------------------------------------------------
  
  fb_sketch <- reactive({
    
    validate(
      need(input$sketch_sheets, "Insert your fieldbook")
    )
    
    validate(
      need(
        input$sketch_sheets %in% googlesheets4::sheet_names(gs()),
        paste("Sheet not found:", input$sketch_sheets)
      )
    )
    
    gs() %>%
      googlesheets4::range_read(input$sketch_sheets)
    
  })
  
  
  # -------------------------------------------------------------------------
  # Sketch options
  # -------------------------------------------------------------------------
  
  output$sketch_options <- renderUI({
    
    validate(
      need(fb_sketch(), "Insert your fieldbook")
    )
    
    fb <- fb_sketch()
    
    design_type <- if("design" %in% names(fb)) {
      tolower(trimws(as.character(unique(fb$design)[1])))
    } else {
      NA_character_
    }
    
    exclude_cols <- c(
      "qrcode",
      "sort",
      "rows",
      "cols",
      "design"
    )
    
    choices <- names(fb)
    choices <- choices[!choices %in% exclude_cols]
    choices <- choices[!grepl("^alt", choices)]
    
    validate(
      need(length(choices) > 0, "No valid columns available for sketch.")
    )
    
    # Defaults by design type
    # -----------------------------------------------------------------------
    
    if(design_type == "augmented") {
      
      default_factor <- if("type" %in% choices) {
        "type"
      } else if("checks" %in% choices) {
        "checks"
      } else {
        choices[1]
      }
      
      default_fill <- if(all(c("plots", "entry") %in% choices)) {
        c("plots", "entry")
      } else if(all(c("plots", "ntreat") %in% choices)) {
        c("plots", "ntreat")
      } else if("plots" %in% choices) {
        "plots"
      } else {
        choices[1]
      }
      
    } else if(design_type == "split-rcbd") {
      
      # In split-plot RCBD, factor names are usually between ntreat and wp_sp.
      # Example: plots, ntreat, Soil, Fertilizer, wp_sp, block...
      
      factor_candidates <- choices[
        !choices %in% c("plots", "ntreat", "wp_sp", "block")
      ]
      
      default_factor <- if(length(factor_candidates) > 0) {
        factor_candidates[1]  # whole-plot factor by fieldbook order
      } else if("wp_sp" %in% choices) {
        "wp_sp"
      } else if("ntreat" %in% choices) {
        "ntreat"
      } else {
        choices[1]
      }
      
      default_fill <- if(length(factor_candidates) >= 2 &&
                         "plots" %in% choices) {
        c("plots", factor_candidates[2]) # subplot factor
      } else if("wp_sp" %in% choices &&
                "plots" %in% choices) {
        c("plots", "wp_sp")
      } else if(all(c("plots", "ntreat") %in% choices)) {
        c("plots", "ntreat")
      } else if("plots" %in% choices) {
        "plots"
      } else {
        choices[1]
      }
      
    } else {
      
      # Standard designs: crd, rcbd, sorted, unsorted, lsd
      
      default_factor <- if("ntreat" %in% choices) {
        "ntreat"
      } else {
        choices[1]
      }
      
      default_fill <- if(all(c("plots", "ntreat") %in% choices)) {
        c("plots", "ntreat")
      } else if("plots" %in% choices) {
        "plots"
      } else {
        choices[1]
      }
      
    }
    
    tagList(
      
      selectizeInput(
        inputId = "sketch_factor",
        label = "Color by",
        multiple = FALSE,
        choices = choices,
        selected = default_factor,
        width = "100%"
      ),
      
      selectizeInput(
        inputId = "sketch_fill",
        label = "Label",
        multiple = TRUE,
        choices = choices,
        selected = default_fill,
        width = "100%"
      )
      
    )
    
  })
  
  # -------------------------------------------------------------------------
  # Build sketch plot 
  # -------------------------------------------------------------------------
  
  plot_sketch <- reactive({
    
    validate(
      need(input$fieldbook_url, "LogIn and create or insert a url")
    )
    
    validate(
      need(input$sketch_sheets, "Insert your fieldbook")
    )
    
    fb <- fb_sketch()
    
    validate(
      need(input$sketch_factor, "Select color factor")
    )
    
    validate(
      need(input$sketch_fill, "Select label")
    )
    
    validate(
      need(
        input$sketch_factor %in% names(fb),
        paste("Selected color factor was not found:", input$sketch_factor)
      )
    )
    
    validate(
      need(
        all(input$sketch_fill %in% names(fb)),
        paste(
          "Selected label columns were not found:",
          paste(setdiff(input$sketch_fill, names(fb)), collapse = ", ")
        )
      )
    )
    
    tarpuy_plotdesign(
      data = fb,
      factor = input$sketch_factor,
      fill = input$sketch_fill
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Render sketch image
  # -------------------------------------------------------------------------
  
  output$plot_sketch <- renderImage({
    
    dpi <- input$sketch_dpi
    width_cm <- input$sketch_width
    height_cm <- input$sketch_height
    
    outfile <- tempfile(fileext = ".png")
    
    png(
      filename = outfile,
      width = width_cm,
      height = height_cm,
      units = "cm",
      res = dpi
    )
    
    print(plot_sketch())
    
    dev.off()
    
    list(src = outfile)
    
  }, deleteFile = TRUE)
  
  
  # -------------------------------------------------------------------------
  # Sketch preview module ----------------------------------------------------
  # -------------------------------------------------------------------------
  
  output$sketch_modules <- renderUI({
    
    if(input$sketch_preview_opt == "Gsheet") {
      
      uiOutput("gsheet_preview_sketch")
      
    } else if(input$sketch_preview_opt == "Sketch") {
      
      tagList(
        
        fluidRow(
          
          box(
            width = 4,
            numericInput(
              inputId = "sketch_width",
              label = "Width (cm)",
              value = 20,
              step = 5,
              min = 5
            )
          ),
          
          box(
            width = 4,
            numericInput(
              inputId = "sketch_height",
              label = "Height (cm)",
              value = 10,
              step = 5,
              min = 5
            )
          ),
          
          box(
            width = 4,
            numericInput(
              inputId = "sketch_dpi",
              label = "Resolution",
              value = 100,
              step = 50,
              min = 100
            )
          )
          
        ),
        
        div(
          imageOutput("plot_sketch"),
          align = "center"
        )
        
      )
      
    }
    
  })
  
  # -------------------------------------------------------------------------
  # Mobile connection module -------------------------------------------------
  # -------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------
  # Fieldbook sheet selector -------------------------------------------------
  # -------------------------------------------------------------------------
  
  output$connection_sheet_fieldbook <- renderUI({
    
    validate(
      need(fieldbook_url(), "LogIn and insert a url")
    )
    
    info <- googlesheets4::gs4_get(gs())
    
    sheet_names <- info$sheets$name
    
    selectInput(
      inputId = "connection_sheet_fieldbook",
      label = NULL,
      choices = c(
        "choose" = "",
        sheet_names
      )
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Traits sheet selector ----------------------------------------------------
  # -------------------------------------------------------------------------
  
  output$connection_sheet_traits <- renderUI({
    
    validate(
      need(fieldbook_url(), "LogIn and insert a url")
    )
    
    info <- googlesheets4::gs4_get(gs())
    
    sheet_names <- info$sheets$name
    
    selectInput(
      inputId = "connection_sheet_traits",
      label = NULL,
      choices = c(
        "choose" = "",
        sheet_names
      )
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Google Sheet preview -----------------------------------------------------
  # -------------------------------------------------------------------------
  
  connection_sheet_preview <- reactive({
    
    validate(
      need(input$fieldbook_url, "LogIn and create or insert a url")
    )
    
    info <- googlesheets4::gs4_get(gs())
    
    url <- info$spreadsheet_url
    
    selected_sheet <- if(input$connection_sheet_preview == "Traits") {
      
      input$connection_sheet_traits
      
    } else {
      
      input$connection_sheet_fieldbook
      
    }
    
    validate(
      need(selected_sheet, "Select a sheet to preview")
    )
    
    validate(
      need(
        selected_sheet %in% info$sheets$name,
        paste("Sheet not found:", selected_sheet)
      )
    )
    
    id <- info$sheets %>%
      dplyr::filter(.data$name %in% selected_sheet) %>%
      purrr::pluck("id")
    
    paste(url, id, sep = "#gid=")
    
  })
  
  
  output$connection_sheet_preview <- renderUI({
    
    tags$iframe(
      src = connection_sheet_preview(),
      style = "height:580px; width:100%; scrolling=no"
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Read selected traits sheet ----------------------------------------------
  # -------------------------------------------------------------------------
  
  traits <- reactive({
    
    validate(
      need(input$connection_sheet_traits, "Need table with traits")
    )
    
    validate(
      need(
        input$connection_sheet_traits %in% googlesheets4::sheet_names(gs()),
        paste("Sheet not found:", input$connection_sheet_traits)
      )
    )
    
    gs() %>%
      googlesheets4::range_read(
        sheet = input$connection_sheet_traits,
        col_types = "c"
      )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Read selected fieldbook sheet -------------------------------------------
  # -------------------------------------------------------------------------
  
  fieldbook <- reactive({
    
    validate(
      need(input$connection_sheet_fieldbook, "Need field book table")
    )
    
    validate(
      need(
        input$connection_sheet_fieldbook %in% googlesheets4::sheet_names(gs()),
        paste("Sheet not found:", input$connection_sheet_fieldbook)
      )
    )
    
    gs() %>%
      googlesheets4::range_read(
        sheet = input$connection_sheet_fieldbook
      )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Last factor selector -----------------------------------------------------
  # -------------------------------------------------------------------------
  
  output$connection_fieldbook_lastfactor <- renderUI({
    
    validate(
      need(fieldbook(), "LogIn and insert a url")
    )
    
    fieldbook_names <- fieldbook() %>%
      names()
    
    selectInput(
      inputId = "connection_fieldbook_lastfactor",
      label = "Last Factor",
      choices = c(
        "choose" = "",
        fieldbook_names
      )
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Build Field Book app files ----------------------------------------------
  # -------------------------------------------------------------------------
  
  fbapp <- reactive({
    
    tarpuy_traits(
      fieldbook = fieldbook(),
      last_factor = input$connection_fieldbook_lastfactor,
      traits = traits()
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Download traits file -----------------------------------------------------
  # -------------------------------------------------------------------------
  
  output$connection_traits_trt <- downloadHandler(
    
    filename = function() {
      paste("traits-", Sys.Date(), ".trt", sep = "")
    },
    
    content = function(con) {
      
      fbapp()$traits %>%
        readr::write_delim(
          file = con,
          delim = ",",
          quote = "all",
          na = '""'
        )
      
    }
    
  )
  
  
  # -------------------------------------------------------------------------
  # Download fieldbook CSV ---------------------------------------------------
  # -------------------------------------------------------------------------
  
  output$connection_fieldbook_csv <- downloadHandler(
    
    filename = function() {
      paste("fieldbook-", Sys.Date(), ".csv", sep = "")
    },
    
    content = function(con) {
      
      fbapp()$fb %>%
        utils::write.csv(file = con)
      
    }
    
  )
  
  
  # -------------------------------------------------------------------------
  # Traits download button ---------------------------------------------------
  # -------------------------------------------------------------------------
  
  output$connection_traits_download <- renderUI({
    
    validate(
      need(input$connection_fieldbook_lastfactor, "")
    )
    
    validate(
      need(input$connection_sheet_traits, "")
    )
    
    downloadButton(
      outputId = "connection_traits_trt",
      label = h6("Traits"),
      icon = icon("download", "fa-2x")
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # Fieldbook download button ------------------------------------------------
  # -------------------------------------------------------------------------
  
  output$connection_fieldbook_download <- renderUI({
    
    validate(
      need(input$connection_fieldbook_lastfactor, "")
    )
    
    validate(
      need(input$connection_sheet_traits, "")
    )
    
    downloadButton(
      outputId = "connection_fieldbook_csv",
      label = h6("FieldBook"),
      icon = icon("download", "fa-2x")
    )
    
  })
  
  
  # -------------------------------------------------------------------------
  # End app -----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
})


