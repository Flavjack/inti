# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupanapro/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-09-30
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

suppressPackageStartupMessages({source("pkgs.R")})

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/spreadsheets"
                                          , "https://www.googleapis.com/auth/userinfo.email"
                                          ))

options(gargle_oob_default = TRUE)
options(shiny.port = 1221)

if (file.exists("www/cloud.json")) gar_set_client(web_json = "www/cloud.json", activate = "web")

# -------------------------------------------------------------------------
# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  

# close auto local session ------------------------------------------------

observe({

  if(Sys.getenv('SHINY_PORT') == "") {

    session$onSessionEnded(stopApp)

  }

})
  
# auth --------------------------------------------------------------------

  gar_shiny_auth(session)

# longin vs local ---------------------------------------------------------

  access_token <- moduleServer(id = "js_token"
                               , module = googleAuth_js)
  
# -------------------------------------------------------------------------

  output$login <- renderUI({

    if (file.exists("www/cloud.json")) {

      googleAuth_jsUI("js_token"
                      , login_text = "LogIn"
                      , logout_text = "LogOut"
                      )

      } else {

      actionButton("local_user", "Local", class = "btn-success")

    }

  })

  gs <- reactive({

    if(Sys.getenv('SHINY_PORT') == "") {

      gs4_auth(email = TRUE)

    } else {

    gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets"
             , cache = FALSE
             , use_oob = TRUE
             , token = access_token()
             )

    }

    validate( need( gs4_has_token(), "LogIn and insert a url" ) )

    as_sheets_id( fieldbook_url() )

  })
  
if(file.exists("www/analytics.r")) {source("www/analytics.r", local = T)}
  
# -------------------------------------------------------------------------

  fieldbook_url <- reactive({

    validate( need( input$fieldbook_url, "LogIn and insert a url" ) )

    if ( input$fieldbook_url != "" ) {

      fieldbook_url <- input$fieldbook_url

    }

  })
  
  
  output$fieldbook_gsheet <- renderUI({
    
    validate(need(fieldbook_url(), "LogIn and insert a url") )
    
    info <- gs4_get(gs())
    
    names <- info$sheets$name 
    
    selectInput(inputId = "fieldbook_gsheet"
                , label = NULL
                , choices = c("choose" = ""
                              , names)
    )
  })

# open url ----------------------------------------------------------------

  output$open_url <- renderUI({

    if ( input$fieldbook_url == "" ) {

      link <- "https://docs.google.com/spreadsheets/u/0/"

    } else {

      link <- fieldbook_url()

    }

    open <- paste0("window.open('", link, "', '_blank')")

    actionButton(inputId = "open_sheet"
                 , label = "Open"
                 , class = "btn btn-success"
                 , onclick = open
                 , width = "100%"
    )

  })

# -------------------------------------------------------------------------

  fb_url <- reactive({

    info <- gs4_get(gs())

    url <-  info$spreadsheet_url

    id <- info$sheets %>%
      filter(name == input$fieldbook_gsheet) %>%
      pluck("id")

    fb_url <- paste(url, id, sep = "#gid=")

  })

# -------------------------------------------------------------------------
  
  fieldbook <- reactive({
    
    validate(need(input$fieldbook_gsheet, "Choose you fb sheet"))
    
    gs() %>%
      range_read(input$fieldbook_gsheet) %>% 
      select(!starts_with("[") | !ends_with("]")) 
    
    })
  

# -------------------------------------------------------------------------

  output$fb_last_factor <- renderUI({
    
    validate(need(fieldbook(), "LogIn and insert a url") )
    
    names <- fieldbook() %>% names()
    
    selectInput(inputId = "fb_last_factor"
                , label = NULL
                , choices = c("choose" = ""
                              , names)
    )
  })
  
  
# Yupana: Exploratory -----------------------------------------------------
# -------------------------------------------------------------------------
  
  output$raw_response <- renderUI({
    
    validate(need(fieldbook(), "LogIn and create or insert a url"))
    
    variable_names <- fieldbook() %>%
      names()
    
    selectInput(
      inputId = "raw_y"
      , label = "Response variable"
      , choices = c("choose" = ""
                    , variable_names)
    )
    
  })
  
  output$raw_x <- renderUI({
    
    validate(need(fieldbook(), "LogIn and create or insert a url"))
    
    factor_names <- fieldbook() %>%
      names()
    
    selectInput(
      inputId = "raw_x"
      , label = "Axis X"
      , choices = c("choose" = ""
                    , factor_names)
    )
    
  })
  
  output$raw_group <- renderUI({
    
    validate(need(fieldbook(), "LogIn and create or insert a url"))
    
    factor_names <- fieldbook() %>%
      names()
    
    selectInput(
      inputId = "raw_group"
      , label = "Grouped"
      , choices = c("choose" = ""
                    , factor_names)
    )
    
  })
  
  plotraw <- reactive({
    
    validate(need(fieldbook(), "LogIn and create or insert a url"))
    validate(need(input$raw_x, "Choose your parameters"))
    validate(need(input$raw_y, "Choose your parameters"))
    
    y <- if(input$raw_y == "") NULL else input$raw_y
    x <- if(input$raw_x == "") NULL else input$raw_x
    
    model <-  y ~ x
    
    raw_xrotation <- input$raw_xrotation %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    raw_ylimits <- input$raw_ylimits %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    raw_gtext <- input$raw_gtext %>% 
      strsplit(., ",") %>% 
      pluck(1) %>% as.character()
    
    raw_xtext <- input$raw_xtext %>% 
      strsplit(., ",") %>% 
      pluck(1) %>% as.character()
    
    fieldbook() %>% 
      plot_raw(type = input$raw_type
               , x = x
               , y = y
               , group = if(input$raw_group == "") NULL else input$raw_group
               , xlab = if(input$raw_xlab == "") NULL else input$raw_xlab
               , ylab = if(input$raw_ylab == "") NULL else input$raw_ylab
               , glab = if(input$raw_glab == "") NULL else input$raw_glab
               , ylimits = if(input$raw_ylimits == "") NULL else raw_ylimits
               , xrotation = if(input$raw_xrotation == "") NULL else raw_xrotation
               , legend = input$raw_legend
               , color = if(input$raw_color == "yes") TRUE else FALSE
               , opt = if(input$raw_opt == "") NULL else input$raw_opt
               , xtext = if(input$raw_xtext == "") NULL else raw_xtext
               , gtext = if(input$raw_gtext == "") NULL else raw_gtext
               ) +
      {if(input$raw_type == "scatterplot") {
        
        ggpmisc::stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")))
        
        } }
    })
  
  output$plotraw <- renderImage({
    
    validate(need(fieldbook(), "LogIn and create or insert a url"))

    dim <- input$raw_dimension %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    if(!is.na(dim[1])) { ancho <- dim[1] } else {ancho <- input$graph_width}
    if(!is.na(dim[2])) { alto <- dim[2] } else {alto <- input$graph_height}
    if(!is.na(dim[3])) { dpi <- dim[3] } else {dpi <- input$graph_dpi}
    
    outfile <- tempfile(fileext = ".png")
    
    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)
    print(plotraw())
    dev.off()
    
    list(src = outfile)
    
  }, deleteFile = TRUE)
  
# ------------------------------------------------------------------------- 
  
  output$plot_raw <- renderUI({
    
    validate(need(plotraw(), "Choose your parameters"))

    tagList( div(imageOutput("plotraw"), align = "center") ) 
    
    })

# Yupana: Fieldbook -------------------------------------------------------
# -------------------------------------------------------------------------

  output$fieldbook_preview <- renderUI({

    tags$iframe(src = fb_url()
                , style="height:580px; width:100%; scrolling=no")

  })

# reshape module ----------------------------------------------------------

  output$last_factor_rs <- renderUI({

    if ( !is.null(fieldbook()) ) {

      fieldbook_names <- fieldbook() %>%
        names()

      selectInput(inputId = "last_factor_rs"
                  , label = "Last factor"
                  , choices = c("choose" = ""
                                , fieldbook_names)
      )

    } else { print ("Insert sheet name") }

  })

  output$from_var_rs <- renderUI({
    
    validate( need( input$last_factor_rs, "Insert last factor" ) )

    if ( !is.null(fieldbook()) && input$last_factor_rs != "" ) {

      fieldbook_varnames <- fieldbook() %>%
        select( !c(1:input$last_factor_rs)  ) %>%
        names()

      selectInput(inputId = "from_var_rs"
                  , label = "From variable (optional)"
                  , choices = c("choose" = ""
                                , fieldbook_varnames)
      )

    } else { print ("Insert last factor") }

  })

  output$to_var_rs <- renderUI({
    
    validate( need( input$last_factor_rs, "Insert last factor" ) )
    
    if ( !is.null(fieldbook()) && input$last_factor_rs != "" ) {

      fieldbook_varnames <- fieldbook() %>%
        select( !c(1:input$last_factor_rs)  ) %>%
        names()

      selectInput(inputId = "to_var_rs"
                  , label = "To variable (optional)"
                  , choices = c("choose" = ""
                                , fieldbook_varnames)
      )

    } else { print ("Insert last factor") }

  })

  output$exc_fact_rs <- renderUI({
    
    validate( need( input$last_factor_rs, "Insert last factor" ) )

    if ( !is.null( fieldbook() ) && input$last_factor_rs != ""  ) {

      exc_fact_rs <- fieldbook() %>%
        select( 1:input$last_factor_rs ) %>%
        names()

      selectInput(inputId = "exc_fact_rs"
                  , label = "Exclude Factors (optional)"
                  , multiple = TRUE
                  , choices = c("choose" = ""
                                , exc_fact_rs)
      )

    } else { print("Insert last factor") }

  })

# -------------------------------------------------------------------------

  output$fb_rsp <- renderUI({

    tagList(

      uiOutput("last_factor_rs"),

      textInput(inputId = "fbrs_sep"
                , label = "Separator"
                , value = ""
                , placeholder = "e.g: var_flw --> '_'"
      ),

      textInput(inputId = "fbrs_newcol"
                , label = "New column"
                , value = ""
                , placeholder = "Column name"
      ),

      uiOutput("from_var_rs"),

      uiOutput("to_var_rs"),

      uiOutput("exc_fact_rs"),

      actionButton(inputId = "fbrs_generate"
                   , label = "Generate"
                   , class = "btn btn-warning"
      )
    )

  })

  # -------------------------------------------------------------------------

  observeEvent(input$fbrs_generate, {

    validate( need( input$fieldbook_url, "LogIn and insert a url" ) )

    if ( !is.null( fieldbook() ) && input$last_factor_rs != "" )  {

      fbrs <- yupana_reshape(data = fieldbook()
                            , last_factor = input$last_factor_rs
                            , sep = input$fbrs_sep
                            , new_colname = input$fbrs_newcol
                            , from_var = input$from_var_rs
                            , to_var = input$to_var_rs
                            , exc_factors = input$exc_fact_rs
                             )

      if ( !"fbrs" %in% sheet_names(gs()) ) {

        sheet_add(ss = gs(), .after = input$fieldbook_gsheet, sheet = "fbrs")

        fbrs %>% sheet_write(ss = gs(), sheet = "fbrs")

      } else { print ("sheet already exist") }

    }

  })

# modules linked ----------------------------------------------------------

  output$fb_modules <- renderUI({ uiOutput("fb_rsp") })
  

# Yupana: Analysis --------------------------------------------------------
# -------------------------------------------------------------------------
  
  output$analysis_last_factor <- renderUI({
    
    validate(need(fieldbook(), "LogIn and insert a url") )
    
    names <- fieldbook() %>% names()
    
    selectInput(inputId = "analysis_last_factor"
                , label = "Last factor"
                , selected = input$fb_last_factor
                , choices = c("choose" = ""
                              , names)
                )
    })
  
  output$analysis_response <- renderUI({
    
    validate(need(input$analysis_last_factor, "Select your last factor") )
    
    names <- fieldbook() %>% 
      select(!1:input$analysis_last_factor) %>% 
      names()
    
    selectInput(inputId = "analysis_response"
                , label = "Response variable"
                , choices = c("choose" = ""
                              , names)
    )
  })
  
  output$analysis_comparison <- renderUI({
    
    validate(need(input$analysis_last_factor, "Select your last factor") )
    
    names <- fieldbook() %>% 
      select(1:input$analysis_last_factor) %>% 
      names()
    
    selectInput(inputId = "analysis_comparison"
                , label = "Factor comparison"
                , multiple = TRUE
                , choices = c("choose" = ""
                              , names)
                )
    })
  
  output$analysis_model_factors <- renderUI({
    
    textInput(
      inputId = "analysis_model_factors"
      , label = "Model factors"
      , value = input$fb_model_factors
      , placeholder = "e.g. block + factor1*factor2"
      , width = "100%"
        
        )
    
    })
  
  observe({
    
    comparison <- input$analysis_comparison %>% 
      paste(., collapse = " + ")
    
    if( input$fb_model_factors == "" ) {
      
      updateTextInput(session
                      , inputId = "analysis_model_factors"
                      , value = comparison)
    } 

    
  })
  
# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  analysis <- reactive({
    
    validate(need(input$analysis_response, "Choose your variable"))
    validate(need(input$analysis_model_factors, "Include your model factors"))
    validate(need(input$analysis_comparison, "Include your model comparison"))
    validate(need(input$analysis_last_factor, "Select your last factor") )
    
    rslt <- yupana_analysis(data = fieldbook()
                            , response = input$analysis_response
                            , last_factor = input$analysis_last_factor
                            , model_factors = input$analysis_model_factors
                            , comparison = input$analysis_comparison
                            , test_comp = input$analysis_test_comparison
                            , sig_level = input$analysis_sig_level
                            , plot_dist = "boxplot"
                            , plot_diag = FALSE
                            , digits = input$analysis_digits
                            )
    })
  
  # -------------------------------------------------------------------------

  output$anova <- renderPrint({ anova(analysis()$anova) })
  
  output$plotdiag <- renderPlot({
    
    diag <- analysis()$plotdiag 
    plot_grid(plotlist = diag, ncol = 2)
    
    })

  output$plotdist <- renderPlot({ analysis()$plotdist })

  # -------------------------------------------------------------------------

  output$meancomp <- DT::renderDataTable(server = FALSE, {

      inti::web_table(data = analysis()$meancomp
                      , digits = input$analysis_digits
                      , file_name = input$analysis_response
                      )
    
    })

  output$smrstats <- DT::renderDataTable(server = FALSE, {

    mc <- analysis()$stats %>%
      inti::web_table(buttons = "copy", scrolly = "15vh")

  })


  # -------------------------------------------------------------------------

  output$analysis_preview <- renderUI({
    
    if ( input$analysis_preview_opt == "Gsheet" ) {

      tags$iframe(src = fb_url(),
                  style="height:580px; width:100%; scrolling=no")
      
      } else if ( input$analysis_preview_opt == "Model" ) {

      validate( need( input$analysis_model_factors, "Choose your variable") )
      
      tagList(

        fluidRow(

          column(width = 5,

                 HTML('<h4><strong>ANOVA</strong></h4>'),

                 verbatimTextOutput("anova"),
                 
                 br(),

                 HTML('<h4><strong>Statistics</strong></h4>'),

                 DT::dataTableOutput("smrstats")

          ),

          column(width = 7,

                 HTML('<h4><strong>Mean Comparison</strong></h4>'),

                 DT::dataTableOutput("meancomp")
                 
                 )
          )
      )

    } else if ( input$analysis_preview_opt == "Diagnostic" ) {

      tagList(

        fluidRow(

          column(width = 5,

                 HTML('<h4><strong>Model Diagnostic</strong></h4>'),

                 plotOutput("plotdiag", width =  "auto", height = "500px")

          ),

          column(width = 7,

                 HTML('<h4><strong>Variable Distribution</strong></h4>'),

                 plotOutput("plotdist", width =  "auto", height = "500px")

          )
        )
      )
    } 
    
  })

# Yupana: Graphics --------------------------------------------------------
# -------------------------------------------------------------------------
  
# load --------------------------------------------------------------------
# -------------------------------------------------------------------------

observeEvent(input$graph_smr_load, {
  
  names <- gs() %>% sheet_names()
  
  import <- modalDialog(size = "s", easyClose = T, 
    
    title = div(h3("Select your sheet", icon("cloud-upload-alt"))
                , align = "left"),
    
    selectInput(
      inputId = "smr_load_sheet"
      , label = NULL
      , choices = c("choose" = ""
                    , names)
      ), 
    
    footer = tagList(
      actionButton("import_sheet", "Import", class = "btn-success")
      )
    
    )
  
  showModal(import)
  
})
  
observeEvent(input$import_sheet, { removeModal() })
  
gropt <- NULL
makeReactiveBinding("gropt")

observeEvent(input$import_sheet, {
  
  validate(need(input$smr_load_sheet, "Select your sheet"))
  
  mc <- gs() %>% range_read(input$smr_load_sheet)
  
  gropt <<- mc %>% yupana_import_smr()
  
})

observeEvent(input$analysis_response, {
  
  gropt <<- NULL
  
})

grdt <- reactive({
  
  if(!is.null(gropt)) {

    list(data = gropt$data
        , type = gropt$type
        , y = gropt$y
        , x = gropt$x
        , group = gropt$group
        , xlab = gropt$xlab
        , ylab = gropt$ylab
        , glab = gropt$glab
        , ylimits = if(is.na(gropt$ylimits)) NA else paste(gropt$ylimits, collapse = "*")
        , xrotation = if(is.na(gropt$xrotation)) NA else paste(gropt$xrotation, collapse = "*")
        , xtext = if(is.na(gropt$xtext)) NA else paste(gropt$xtext, collapse = ",")
        , gtext = if(is.na(gropt$gtext)) NA else paste(gropt$gtext, collapse = ",")
        , legend = gropt$legend
        , sig = gropt$sig
        , error = gropt$error
        , color = if(length(gropt$color) == 0) TRUE else gropt$color
        , opt = gropt$opt
        , dimension = gropt$dimension
        , model = gropt$model
        , factors = gropt$factors
        , tabvar = gropt$tabvar
        , comparison = gropt$comparison
        , test_comp = gropt$test_comp
        , sig_level = gropt$sig_level
        )  
    
  } else if (is.null(gropt)) {
    
    list(data = analysis()$meancomp
        , y = analysis()$response
        , x = analysis()$comparison[1]
        , group = analysis()$comparison[2]
        #>
        , type = NA
        , xlab = NA 
        , ylab = NA 
        , glab = NA 
        , ylimits = NA 
        , xrotation = NA 
        , legend = NA 
        , color = NA 
        , opt = NA 
        , xtext = NA 
        , gtext = NA 
        , sig = "sig" 
        , error = "ste"
        , dimension = NA 
        #>
        , model = analysis()$model
        , factors = analysis()$factors
        , tabvar = analysis()$tabvar
        , comparison = analysis()$comparison
        , test_comp = input$analysis_test_comparison
        )
    }
  
})

output$smr_type <- renderUI({ 
  
  grdt <- grdt()
  
  opts <- c("bar", "line")
  selection <- grdt$type
  
  selectInput(
    inputId = "smr_type"
    , label = "Type"
    , choices = opts
    , selected = selection
    )
  
})

output$smr_response <- renderUI({ 
  
  grdt <- grdt()
  
  opts <- grdt$y

selectInput(
  inputId = "smr_response"
  , label = "Response variable"
  , choices = opts
  )

})

output$smr_x <- renderUI({ 
  
  grdt <- grdt()

  opts <- grdt$factors
  selection <- grdt$x

selectInput(
  inputId = "smr_x"
  , label = "Axis X"
  , choices = opts
  , selected = selection
  )

})

output$smr_group <- renderUI({ 
  
  grdt <- grdt()
  
  opts <- grdt$factors
  selection <- grdt$group
  
selectInput(
  inputId = "smr_group"
  , label = "Grouped"
  , choices = opts
  , selected = selection
  )

})

output$smr_sig <- renderUI({ 
  
  grdt <- grdt()
  
  opts <- c(grdt$factors, grdt$tabvar, "none")
  
  selection <- if(all(is.na(grdt$data$ste))) {"none"} else {grdt$sig}
  
selectInput(
  inputId = "smr_sig"
  , label = "Significance"
  , choices = opts
  , selected = selection
  )

})

output$smr_error <- renderUI({ 
  
  grdt <- grdt()
  
  opts <- c("ste", "std", "none")
  
  selection <- if(all(is.na(grdt$data$ste))) {"none"} else {grdt$error}
  
  selectInput(
    inputId = "smr_error"
    , label = "Error bar"
    , choices = opts
    , selected = selection
    )
  
})

output$plot_error <- renderUI({ 
  
  grdt <- grdt()
  
  opts <- c("top", "bottom", "left", "right", "none")
  selection <- grdt$legend
  
  selectInput(
    inputId = "smr_legend"
    , label = "Legend"
    , choices = opts
    , selected = selection
  )
  
})

output$smr_ylimits <- renderUI({ 
  
  grdt <- grdt()
  
  selection <- if(is.na(grdt$ylimits)) "" else grdt$ylimits
  
  textInput(
    inputId ="smr_ylimits"
    , label = "Y limits"
    , placeholder = "0*100*20"
    , value = selection
    )
  
})

output$smr_xrotation <- renderUI({ 
  
  grdt <- grdt()
  
  selection <- if(is.na(grdt$xrotation)) "0*0.5*0.5" else grdt$xrotation
  
  textInput(
    inputId ="smr_xrotation"
    , label = "X rotation"
    , placeholder = "angle*h*v"
    , value = selection
    )
  
})

output$smr_dimension <- renderUI({ 
  
  grdt <- grdt()
  
  selection <- grdt$dimension
  
  selection <- if(is.na(grdt$xrotation)) "20*10*100" else paste(selection, collapse = "*")
  
  textInput(
    inputId = "smr_dimension"
    , label = "Dimensions (W*H*dpi)"
    , placeholder = "W*H*dpi"
    , value = selection
  )
  
})

output$analysis_model <- renderText({ 
  
  grdt <- grdt()
  
  grdt$model
  
})

output$plot_ylab <- renderUI({ 
  
  grdt <- grdt()
  selection <- if(is.na(grdt$ylab)) "" else grdt$ylab
  
  textInput(
    inputId ="smr_ylab"
    , label = "Y label"
    , value = selection
  )
  
})

output$plot_xlab <- renderUI({ 
  
  grdt <- grdt()
  selection <- if(is.na(grdt$xlab)) "" else grdt$xlab
  
  textInput(
    inputId ="smr_xlab"
    , label = "X label"
    , value = selection
    )
  
})

output$plot_glab <- renderUI({ 
  
  grdt <- grdt()
  selection <- if(is.na(grdt$glab)) "" else grdt$glab
  
  textInput(
    inputId ="smr_glab"
    , label = "Group label"
    , value = selection
  )
  
})

output$plot_gtext <- renderUI({ 
  
  grdt <- grdt()
  selection <- if(is.na(grdt$gtext)) "" else grdt$gtext
  
  textInput(
    inputId ="smr_gtext"
    , label = "Group brake labels (,)"
    , value = selection
  )
  
})

output$plot_xtext <- renderUI({ 
  
  grdt <- grdt()
  selection <- if(is.na(grdt$xtext)) "" else grdt$xtext

  textInput(
    inputId ="smr_xtext"
    , label = "X brake labels (,)"
    , value = selection
  )
  
})

output$plot_opt <- renderUI({ 
  
  grdt <- grdt()
  
  groups <- grdt$comparison
  
  if(length(groups) == 3) {

    selection <- paste0("facet_grid(. ~", groups[3], ")")
    
  } else {
    
    selection <- if(is.na(grdt$opt)) "" else grdt$opt
    
  }
  
  textInput(
    inputId ="smr_opt"
    , label = "Opt"
    , placeholder = "extra layers"
    , value = selection
    )
  
})

output$plot_color <- renderUI({
  
  if(is.null(gropt)) {
    
    opts <- c("yes", "no")
    
  } else {
    
    opts <- c("manual")
    
  }
  
  selectInput(
    inputId ="smr_color"
    , label = "Color"
    , choices = opts
    )
  
})


# plot --------------------------------------------------------------------
# -------------------------------------------------------------------------

  plotsmr <- reactive({ 
    
    grdt <- grdt()
    
    ylimits <- if(input$smr_ylimits == "") NULL else {
      
      input$smr_ylimits %>% 
        strsplit(., "[*]") %>% 
        unlist() %>% as.numeric()
      
      }
    
    xrotation <- if(input$smr_xrotation == "") NULL else {
      
      input$smr_xrotation %>% 
        strsplit(., "[*]") %>% 
        unlist() %>% as.numeric()
      
      }

    xlab <- if(input$smr_xlab == "") NULL else input$smr_xlab
    ylab <- if(input$smr_ylab == "") NULL else input$smr_ylab
    glab <- if(input$smr_glab == "") NULL else input$smr_glab
    xtext <- if(input$smr_xtext == "") NULL else input$smr_xtext %>% strsplit(., ",") %>% unlist()
    gtext <- if(input$smr_gtext == "") NULL else input$smr_gtext %>% strsplit(., ",") %>% unlist()
    
    opt <- if(input$smr_opt == "") NULL else input$smr_opt
    sig <- if(input$smr_sig == "none") NULL else input$smr_sig
    error <- if(input$smr_error == "none") NULL else input$smr_error
    
    color <- if(input$smr_color == "yes") { TRUE
    } else if (input$smr_color == "no") { FALSE
    } else if (input$smr_color == "manual"){ grdt$color }
    
    plot_smr(data = grdt$data
             , type = input$smr_type
             , y = input$smr_response
             , x = input$smr_x
             , group = input$smr_group
             , xlab = xlab
             , ylab = ylab
             , glab = glab
             , ylimits = ylimits
             , xrotation = xrotation
             , error = error
             , sig = sig
             , legend = input$smr_legend
             , opt = opt
             , gtext = gtext 
             , xtext = xtext
             , color = color
             ) 
    })
  
# -------------------------------------------------------------------------
  
  output$plotsmr <- renderImage({
    
    validate(need(plotsmr(), "Choose your parameters") )
    
    dim <- input$smr_dimension %>% 
      strsplit(., "[*]") %>% 
      unlist() %>% as.numeric()
    
    if(!is.na(dim[1])) { ancho <- dim[1] } else {ancho <- input$graph_width}
    if(!is.na(dim[2])) { alto <- dim[2] } else {alto <- input$graph_height}
    if(!is.na(dim[3])) { dpi <- dim[3] } else {dpi <- input$graph_dpi}
    
    outfile <- tempfile(fileext = ".png")
    
    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)
    print(plotsmr())
    dev.off()
    
    list(src = outfile)
    
  }, deleteFile = TRUE)
  
# -------------------------------------------------------------------------
  
  graph_url <- reactive({

    sheet_name <- input$graph_smr_name %>% gsub("[[:space:]]", "_", .)

    info <- gs4_get(gs())

    url <- info$spreadsheet_url

    id <- info$sheets %>%
      filter(name %in% sheet_name) %>%
      pluck("id")

    url_preview <- paste(url, id, sep = "#gid=")

  })
  
  
  
  output$plot_smr <- renderUI({
    
    if ( input$smr_preview_opt == "Gsheet" ) {
      
      tags$iframe(src = graph_url(),
                  style="height:580px; width:100%; scrolling=no")
      
    } else if (input$smr_preview_opt == "Plots" ) {
      
      tagList(
        
        div(imageOutput("plotsmr"), align = "center")
        
      )
    }
  })
  

# export graph options ---------------------------------------------
  
  output$graph_sheet_save <- renderUI({
    
    if(is.null(gropt)) {
      
      selection <- input$analysis_response 
      
    } else if (!is.null(gropt)) {
      
      selection <- input$smr_load_sheet
        
    }
    
    textInput(inputId = "graph_smr_name"
              , label = "Sheet export"
              , value = selection
              )
    })
  
  graph_save_info <- reactive({
    
    # validate(need(grdt(), "Some paremeter are missing") )

    grdt <- grdt()
    
    color <- if(input$smr_color == "yes") { TRUE
    }  else if (input$smr_color == "nos") { FALSE 
        } else { grdt$color }
    
    yupana_export_smr(data = grdt$data
                      #> reactive
                     , type = input$smr_type
                     , xlab = input$smr_xlab
                     , ylab = input$smr_ylab
                     , glab = input$smr_glab
                     , ylimits = input$smr_ylimits
                     , xrotation = input$smr_xrotation
                     , xtext = input$smr_xtext
                     , gtext = input$smr_gtext
                     , legend = input$smr_legend
                     , sig = input$smr_sig
                     , error = input$smr_error
                     , opt = input$smr_opt
                     , dimension = input$smr_dimension
                     #> fixed/reactive
                     , response = grdt$y
                     , comparison = grdt$comparison
                     , model = grdt$model
                     , test_comp = grdt$test_comp
                     , sig_level = grdt$sig_level
                     , color = color
                     )
    
    }) 
  

  observeEvent(input$graph_smr_save, {
    
    validate(need(graph_save_info(), "Some paremeter are missing") )
    
    sheet_export <- input$graph_smr_name %>% gsub("[[:space:]]", "_", .)
    
    if ( input$graph_smr_overwrite == "no" & !sheet_export %in% sheet_names(gs())) {
      
      sheet_add(ss = gs(), sheet = sheet_export)
      
      graph_save_info() %>% 
        sheet_write(ss = gs(), sheet = sheet_export )
      
    } else if(input$graph_smr_overwrite == "yes") {
      
      # sheet_add(ss = gs(), sheet = sheet_export)
      
      graph_save_info() %>% 
        sheet_write(ss = gs(), sheet = sheet_export )
      
    } else {  print ("sheet already exist") }
  
    
  })
  
  
# Yupana: Multivariate-----------------------------------------------------
# -------------------------------------------------------------------------

  output$mvr_last_factor <- renderUI({
    
    validate(need(fieldbook(), "LogIn and insert a url"))
    
    last_factor <- fieldbook() %>% names()
    
    
    selectInput(inputId = "mvr_last_factor"
                , label = "Last Factor"
                , selected = input$fb_last_factor
                , choices = c("choose" = ""
                              , last_factor)
                )
    })
  
  output$mvr_facts <- renderUI({

    validate(need(input$mvr_last_factor, "Choose your factors"))
    
    mvr_factors <- fieldbook() %>%
      select(1:input$mvr_last_factor) %>% 
      names()

      selectInput(inputId = "mvr_factors"
                  , label = "Factors"
                  , multiple = T
                  , choices = c("choose" = ""
                                , mvr_factors)
      )

  })

# -------------------------------------------------------------------------

  output$mvr_groups <- renderUI({
    
    validate(need(input$mvr_factors, "Insert group factor"))
    
    selectInput(inputId = "mvr_groups"
                , label = "Groups"
                , choices = input$mvr_factors
                )
    
    })
  
  
  output$mvr_variables <- renderUI({
    
    validate(need(input$mvr_last_factor, "Insert variables"))
    
    mvr_variables <- fieldbook() %>%
      select(!1:input$mvr_last_factor) %>% 
      names()
    
    selectInput(inputId = "mvr_variables"
                , label = "Variables"
                , multiple = TRUE
                , choices = c("all", mvr_variables)
    )
    
  })

# -------------------------------------------------------------------------
  
  mvr <- reactive({
    
    validate(need(input$mvr_variables, "Choose your variables"))

    n <- fieldbook() %>%
      select(input$mvr_factors)  %>% 
      select(!starts_with("{") | !ends_with("}")) %>% 
      as.list() %>% 
      map(discard, is.na) %>% 
      lengths() %>% 
      prod()
    
    validate(need(n > 2, "The factors should have more than 2 levels"))
    
    yupana_mvr(
      data = fieldbook()
      , last_factor = input$mvr_last_factor
      , summary_by = input$mvr_factors
      , groups = input$mvr_groups
      , variables = if(input$mvr_variables == "all") NULL else input$mvr_variables
      )
    
  })

# -------------------------------------------------------------------------

  output$pca_var <- renderImage({
    
    validate(need(mvr(), "Choose your factors"))
    
    dim <- input$mvr_dimension_pca_var %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    if(!is.na(dim[1])) { ancho <- dim[1] } else {ancho <- input$graph_width}
    if(!is.na(dim[2])) { alto <- dim[2] } else {alto <- input$graph_height}
    if(!is.na(dim[3])) { dpi <- dim[3] } else {dpi <- input$graph_dpi}

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

    plot.PCA(x = mvr()$pca
             , choix = "var"
             , autoLab = "auto"
             , shadowtext = T
    ) %>% print()

    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # -------------------------------------------------------------------------

  output$pca_ind <- renderImage({
    
    validate(need(mvr(), "Choose your factors"))

    dim <- input$mvr_dimension_pca_ind %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    if(!is.na(dim[1])) { ancho <- dim[1]} else {ancho <- input$graph_width}
    if(!is.na(dim[2])) { alto <- dim[2]} else {alto <- input$graph_height}
    if(!is.na(dim[3])) { dpi <- dim[3]} else {dpi <- input$graph_dpi}

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

    plot <- plot.PCA(x = mvr()$pca
             , choice = "ind"
             , habillage = mvr()$param$groups_n
             , invisible = "quali"
             , autoLab = "auto"
             , shadowtext = T
             , graph.type = "ggplot"
             ) +
      theme(legend.position = "bottom")
    
    plot %>% print()

    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)


  # -------------------------------------------------------------------------

  output$hcpc_tree <- renderImage({
    
    validate(need(mvr(), "Choose your factors"))

    dim <- input$mvr_dimension_hcp_tree %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    if(!is.na(dim[1])) { ancho <- dim[1] } else {ancho <- input$graph_width}
    if(!is.na(dim[2])) { alto <- dim[2] } else {alto <- input$graph_height}
    if(!is.na(dim[3])) { dpi <- dim[3] } else {dpi <- input$graph_dpi}

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

    plot.HCPC(x = mvr()$hcpc, choice = "tree")

    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # -------------------------------------------------------------------------

  output$hcpc_map <- renderImage({
    
    validate(need(mvr(), "Choose your factors"))

    dim <- input$mvr_dimension_hcp_map %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    if(!is.na(dim[1])) { ancho <- dim[1]} else {ancho <- input$graph_width}
    if(!is.na(dim[2])) { alto <- dim[2]} else {alto <- input$graph_height}
    if(!is.na(dim[3])) { dpi <- dim[3]} else {dpi <- input$graph_dpi}

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

    plot.HCPC(x = mvr()$hcpc
              , choice = "map"
              , legend = list(x = "topright"
                              , cex = 0.6
                              , inset = 0.001
                              , box.lty=0
                              )
              , draw.tree = F
              )
    
    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # -------------------------------------------------------------------------

  output$correlation <- renderImage({
    
    validate(need(mvr(), "Choose your factors"))
    
    dim <- input$mvr_dimension_cor %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    if(!is.na(dim[1])) { ancho <- dim[1]} else {ancho <- input$graph_width}
    if(!is.na(dim[2])) { alto <- dim[2]} else {alto <- input$graph_height}
    if(!is.na(dim[3])) { dpi <- dim[3]} else {dpi <- input$graph_dpi}

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi, pointsize = 9)

    corrplot(mvr()$corr$correlation
             , method = "number"
             , tl.col = "black"
             , tl.srt = 45
             , 
             )

    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # -------------------------------------------------------------------------

  output$mvr_preview <- renderUI({
    
    validate(need(mvr(), "Choose your factors"))
    
    if ( input$mvr_module == "PCA" ) {

      tagList(

        fluidRow(

          box(width = 6,

              div(imageOutput("pca_var"), align = "center"),

          ),

          box(width = 6,

              div(imageOutput("pca_ind"), align = "center")

          )

        )

      )

    } else if ( input$mvr_module == "HCPC" ) {

      tagList(

        fluidRow(

          box(width = 6,

              div(imageOutput("hcpc_tree", width = "100%"), align = "center"),

          ),

          box(width = 6,

              div(imageOutput("hcpc_map", width = "100%"), align = "center")

          )

        )

      )

    } else if (input$mvr_module == "CORR" ) {

      tagList(

        fluidRow(

          box(width = 12,

              div(imageOutput("correlation", width = "100%"), align = "center")

          )

        )

      )

    }
  })

# end yupana --------------------------------------------------------------
# -------------------------------------------------------------------------

})
