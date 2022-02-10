# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupanapro/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2022-02-10
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
  
  source("auth.R")
  if(file.exists("www/analytics.r")) {source("www/analytics.r", local = T)}
  
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
    
    fieldbook() %>% 
      plot_raw(type = input$raw_type
               , x = input$raw_x
               , y = input$raw_y
               , group = input$raw_group
               , xlab = if(input$raw_xlab == "") input$raw_x else input$raw_xlab
               , ylab = if(input$raw_ylab == "") input$raw_y else input$raw_ylab
               , glab = if(input$raw_glab == "") NULL else input$raw_glab
               , ylimits = input$raw_ylimits
               , xrotation = input$raw_xrotation
               , legend = input$raw_legend
               , color = input$raw_color
               , opt = input$raw_opt
               , xtext = input$raw_xtext
               , gtext = input$raw_gtext
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
    
    
    factors <- input$analysis_model_factors %>% 
      gsub("[[:space:]]", "", .)
    
    model_factors <- all.vars(parse(text= factors)) %>% as.vector()
    
    validate(need(all(model_factors %in% names(fieldbook()))
                  , "Factors not found in dataframe"))
    
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

  output$anova <- renderPrint({

    anova(analysis()$anova)

    })

  output$plotdiag <- renderPlot({

    diag <- analysis()$plotdiag
    plot_grid(plotlist = diag, ncol = 2)


    })

  output$plotdist <- renderPlot({

    analysis()$plotdist

    })

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

imp_opt <- NULL
makeReactiveBinding("imp_opt")

observeEvent(input$import_sheet, {

  validate(need(input$smr_load_sheet, "Select your sheet"))

  dt <- gs() %>% range_read(input$smr_load_sheet)

  imp_opt <<- dt

})

observeEvent(input$analysis_response, {

  imp_opt <<- NULL

})

plot_opt <- reactive({

  if(is.null(imp_opt)) {

    yupana_import(analysis())

  } else if(!is.null(imp_opt)) {

    yupana_import(imp_opt)

  }

})

output$smr_type <- renderUI({

  plot_opt <- plot_opt()

  opts <- c("bar", "line")
  selection <- plot_opt$plot_args$type

  selectInput(
    inputId = "smr_type"
    , label = "Type"
    , choices = opts
    , selected = selection
    )
  })


output$smr_response <- renderUI({

  plot_opt <- plot_opt()

  opts <- plot_opt$plot_args$y

selectInput(
  inputId = "smr_response"
  , label = "Response variable"
  , choices = opts
  )

})

output$smr_x <- renderUI({

  plot_opt <- plot_opt()

  opts <- plot_opt$factors
  selection <- plot_opt$plot_args$x

selectInput(
  inputId = "smr_x"
  , label = "Axis X"
  , choices = opts
  , selected = selection
  )

})

output$smr_group <- renderUI({

  plot_opt <- plot_opt()

  opts <- plot_opt$factors
  selection <- plot_opt$plot_args$group

selectInput(
  inputId = "smr_group"
  , label = "Grouped"
  , choices = opts
  , selected = selection
  )

})

output$smr_sig <- renderUI({

  plot_opt <- plot_opt()

  opts <- c(plot_opt$factors, plot_opt$tabvar, "none")

  selection <- if(all(is.na(plot_opt$smr$sig))) {"none"} else {plot_opt$plot_args$sig}

selectInput(
  inputId = "smr_sig"
  , label = "Significance"
  , choices = opts
  , selected = selection
  )

})

output$smr_error <- renderUI({

  plot_opt <- plot_opt()

  opts <- c("ste", "std", "none")

  selection <- if(all(is.na(plot_opt$smr$ste))) {"none"} else {plot_opt$plot_args$error}

  selectInput(
    inputId = "smr_error"
    , label = "Error bar"
    , choices = opts
    , selected = selection
    )

})

output$plot_error <- renderUI({

  plot_opt <- plot_opt()

  opts <- c("top", "bottom", "left", "right", "none")
  selection <- plot_opt$plot_args$legend

  selectInput(
    inputId = "smr_legend"
    , label = "Legend"
    , choices = opts
    , selected = selection
  )

})

output$smr_ylimits <- renderUI({

  plot_opt <- plot_opt()

  selection <- if(is.na(plot_opt$plot_args$ylimits)) "" else plot_opt$plot_args$ylimits

  textInput(
    inputId ="smr_ylimits"
    , label = "Y limits"
    , placeholder = "0*100*20"
    , value = selection
    )

})

output$smr_xrotation <- renderUI({

  plot_opt <- plot_opt()

  selection <- ifelse(is.na(plot_opt$plot_args$xrotation)
                      , "0*0.5*0.5"
                      , paste(plot_opt$plot_args$xrotation, collapse = "*")
                      ) %>% pluck(1)
  
  textInput(
    inputId ="smr_xrotation"
    , label = "X rotation"
    , placeholder = "angle*h*v"
    , value = selection
    )

})

output$smr_dimension <- renderUI({

  plot_opt <- plot_opt()
  
  selection <- ifelse(is.na(plot_opt$plot_args$dimension)
                      , "20*10*100"
                      , paste(plot_opt$plot_args$dimension, collapse = "*")
                      ) %>% 
    pluck(1)

  textInput(
    inputId = "smr_dimension"
    , label = "Dimensions (W*H*dpi)"
    , placeholder = "W*H*dpi"
    , value = selection
  )

})

output$analysis_model <- renderText({

  plot_opt <- plot_opt()

  plot_opt$stats_args$model

})

output$plot_ylab <- renderUI({

  plot_opt <- plot_opt()

  selection <- if(is.na(plot_opt$plot_args$ylab)) "" else plot_opt$plot_args$ylab

  textInput(
    inputId ="smr_ylab"
    , label = "Y label"
    , value = selection
  )

})

output$plot_xlab <- renderUI({

  plot_opt <- plot_opt()

  selection <- if(is.na(plot_opt$plot_args$xlab)) "" else plot_opt$plot_args$xlab

  textInput(
    inputId ="smr_xlab"
    , label = "X label"
    , value = selection
    )

})

output$plot_glab <- renderUI({

  plot_opt <- plot_opt()

  selection <- if(is.na(plot_opt$plot_args$glab)) "" else plot_opt$plot_args$glab

  textInput(
    inputId ="smr_glab"
    , label = "Group label"
    , value = selection
  )

})

output$plot_gtext <- renderUI({

  plot_opt <- plot_opt()

  selection <- if(is.na(plot_opt$plot_args$gtext)) "" else plot_opt$plot_args$gtext

  textInput(
    inputId ="smr_gtext"
    , label = "Group brake labels (,)"
    , value = selection
  )

})

output$plot_xtext <- renderUI({

  plot_opt <- plot_opt()

  selection <- if(is.na(plot_opt$plot_args$xtext)) "" else plot_opt$plot_args$xtext

  textInput(
    inputId ="smr_xtext"
    , label = "X brake labels (,)"
    , value = selection
  )

})

output$plot_opt <- renderUI({

  plot_opt <- plot_opt()

  # check!

  groups <- plot_opt$stats_args$comparison %>%
    strsplit(., "[*]")

  if(length(groups) == 3) {

    selection <- paste0("facet_grid(. ~", groups[3], ")")

  } else {

    selection <- if(is.na(plot_opt$plot_args$opt)) "" else plot_opt$plot_args$opt

  }

  textInput(
    inputId ="smr_opt"
    , label = "Opt"
    , placeholder = "extra layers"
    , value = selection
    )

})

output$plot_color <- renderUI({

  if(is.null(imp_opt)) {

    opts <- c("yes", "no")

  } else {

    opts <- c("manual", "yes", "no")

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

    plot_opt <- plot_opt()

    color <- if(input$smr_color == "yes") { TRUE
    } else if (input$smr_color == "no") { FALSE
        } else if (input$smr_color == "manual"){ plot_opt$plot_args$color }

    plot_smr(data = plot_opt$smr
             , type = input$smr_type
             , x = input$smr_x
             , y = input$smr_response
             , group = input$smr_group
             , xlab = if(input$smr_xlab == "") input$smr_x else input$smr_xlab
             , ylab = if(input$smr_ylab == "") input$smr_response else input$smr_ylab
             , glab = if(input$smr_glab == "") NULL else input$smr_glab
             , ylimits = input$smr_ylimits
             , xrotation = input$smr_xrotation
             , error = input$smr_error
             , sig = input$smr_sig
             , legend = input$smr_legend
             , opt = input$smr_opt
             , gtext = input$smr_gtext
             , xtext = input$smr_xtext
             , color = color
             )
    })

# -------------------------------------------------------------------------

  output$plotsmr <- renderImage({

    validate(need(plotsmr(), "Choose your parameters"))

    dim <- input$smr_dimension %>%
      strsplit(., "[*]") %>%
      unlist() %>%
      as.numeric()

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

    if(is.null(imp_opt)) {

      selection <- input$analysis_response

    } else if (!is.null(imp_opt)) {

      selection <- input$smr_load_sheet

    }
    

      textInput(inputId = "graph_smr_name"
                , label = "Sheet export"
                , value = selection
                , width = "100%"
                )


    })

# -------------------------------------------------------------------------

  graph_save_info <- reactive({

    if(is.null(imp_opt)) {

      dt <- analysis()

    } else if(!is.null(imp_opt)) {

      dt <- plot_opt()

    }

    color <- if(input$smr_color == "yes") { TRUE
      }  else if (input$smr_color == "no") { FALSE
      } else { imp_opt$plot$color }

    yupana_export(data = dt
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
                  #>
                  , color = color
                  )
    })

# -------------------------------------------------------------------------

  observeEvent(input$graph_smr_save, {

    validate(need(graph_save_info(), "Some paremeter are missing") )

    sheet_export <- input$graph_smr_name %>% gsub("[[:space:]]", "_", .)

    if ( input$graph_smr_overwrite == "no" & !sheet_export %in% sheet_names(gs())) {

      sheet_add(ss = gs(), sheet = sheet_export)

      graph_save_info() %>%
        sheet_write(ss = gs(), sheet = sheet_export )

    } else if(input$graph_smr_overwrite == "yes") {

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
