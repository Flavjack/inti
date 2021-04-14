# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupanapro/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-03-12
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

source("pkgs.R")

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

      gs4_auth(T)

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

  fbsm_url <- reactive({

    info <- gs4_get(gs())

    url <- info$spreadsheet_url

    id <- info$sheets %>%
      filter(name == input$fbsmrvars_gsheet) %>%
      pluck("id")

    fbsm_url  <- paste(url, id, sep = "#gid=")

  })

# -------------------------------------------------------------------------
  
  fieldbook <- reactive({

    if ( input$fieldbook_gsheet %in% sheet_names(gs()) ) {

      gs() %>%
        range_read( input$fieldbook_gsheet ) %>% 
        as.data.frame()

    } else { fieldbook <- NULL }

  })
  
  refresh <- reactive({ list(input$fbsm_refresh, input$mvr_refresh) })

  fbsmrvar <- eventReactive( refresh(), {
    
      validate( need( input$fieldbook_url, "LogIn and create or insert a url" ) )

      if ( input$fbsmrvars_gsheet %in% sheet_names(gs()) ) {

        fbsmrvar <- gs() %>%
          range_read( input$fbsmrvars_gsheet ) %>%
          as.data.frame()

      } else { fbsmrvar <- NULL }
    
  })

# Yupana: Fieldbook -------------------------------------------------------
# -------------------------------------------------------------------------

  output$fieldbook_preview <- renderUI({

    tags$iframe(src = fb_url()
                , style="height:580px; width:100%; scrolling=no")

  })

  # summary module ----------------------------------------------------------

  output$last_factor <- renderUI({

    if ( !is.null(fieldbook()) ) {

      fieldbook_names <- fieldbook() %>%
        names()

      selectInput(inputId = "last_factor"
                  , label = "Last factor"
                  , choices = c("choose" = ""
                                , fieldbook_names)
      )

    } else { print ("Insert sheet name") }

  })

  # -------------------------------------------------------------------------

  output$comp_facts <- renderUI({
    
    validate( need( fieldbook(), "LogIn and insert a url" ) )
    validate( need( input$last_factor, "Insert last factor" ) )

    if ( !is.null( fieldbook() ) && input$last_factor != ""  ) {

      fieldbook_fctr_names <- fieldbook() %>%
        select( 1:input$last_factor ) %>%
        names()

      selectInput(inputId = "comp_facts"
                  , label = "Comparison factors"
                  , multiple = TRUE
                  , choices = c("choose" = ""
                                , fieldbook_fctr_names)
      )

    } else { print("Insert last factor") }

  })

  # -------------------------------------------------------------------------

  output$fb_smr <- renderUI({

    tagList(

      uiOutput("last_factor"),

      textInput(inputId = "model_facts"
                , label = "Model factors"
                , value = ""
                , placeholder = "block + factor1*factor2"
      ),

      uiOutput("comp_facts"),

      selectInput(inputId = "test_comp"
                  , label = "Mean comparison test"
                  , choices = c("SNK", "TUKEY", "DUNCAN")
      ),

      numericInput(inputId = "sig_level"
                   , label = "Significance level"
                   , value = 0.05
                   , step = 0.01
                   , min = 0
      ),

      actionButton(inputId = "fbsmr_generate"
                   , label = "Generate"
                   , class = "btn btn-warning"
      )

    )

  })

  # -------------------------------------------------------------------------

  observeEvent(input$fbsmr_generate, {

    validate( need( input$fieldbook_url, "LogIn and insert a url" ) )

    if ( !is.null( fieldbook() ) && input$last_factor != "" )  {

      fbsmr <- fieldbook_summary(data = fieldbook()
                                 , last_factor = input$last_factor
                                 , model_facts = input$model_facts
                                 , comp_facts = paste0(input$comp_facts
                                                       , collapse = "*")
                                 , test_comp = input$test_comp
                                 , sig_level = input$sig_level
      )

      if ( !input$fbsmrvars_gsheet %in% sheet_names(gs()) ) {

        sheet_add(ss = gs(), .after = input$fieldbook_gsheet
                  , sheet = input$fbsmrvars_gsheet)

        fbsmr %>% sheet_write(ss = gs(), sheet = input$fbsmrvars_gsheet)

      } else { print ("sheet already exist") }

    }

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

      fbrs <- fieldbook_reshape(data = fieldbook()
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

  output$fb_modules <- renderUI({

    if ( input$fb_preview_opt == "Summary" ) {

      uiOutput("fb_smr")

    } else if ( input$fb_preview_opt == "Reshape" ) {

      uiOutput("fb_rsp")

    }

  })

  # Yupana: Analysis --------------------------------------------------------
  # -------------------------------------------------------------------------

  output$rpt_variable <- renderUI({

    if ( !is.null( fbsmrvar() ) ) {

      rpt_variable_names <- fbsmrvar() %>%
        filter(!type %in% c("factor", "factores", "factors")) %>%
        select(variables) %>%
        deframe()

      selectInput(inputId = "rpt_variable"
                  , label = "Variable"
                  , choices = c("choose" = ""
                                , rpt_variable_names)
      )

    } else { print ("Insert fieldbook summary") }

  })
  

  # -------------------------------------------------------------------------
  
  output$sheet_export <- renderUI({
    
    validate( need( input$rpt_variable, "Choose your variable") )
    
    textInput(inputId = "sheet_export"
            , label = "Sheet export"
            , value = input$rpt_variable
            )
    
    })

  if (file.exists("www/analytics.r")) { source("www/analytics.r", local = T) }

  # -------------------------------------------------------------------------

  output$rpt_dotplot_groups <- renderUI({

    if ( !is.null( fbsmrvar() ) ) {

      rpt_dotplot_groups_names <- fbsmrvar() %>%
        filter(type %in% c("factor", "factores", "factors")) %>%
        select(variables) %>%
        deframe()

      selectInput(inputId = "rpt_dotplot_groups"
                  , label = "Dotplot"
                  , choices = c("choose" = ""
                                , rpt_dotplot_groups_names)
      )

    } else { print ("Insert fieldbook summary") }

  })

  # -------------------------------------------------------------------------

  report <- reactive({

    validate( need( input$rpt_variable, "Choose your variable") )

    if ( !is.null( fieldbook() ) & !is.null( fbsmrvar() ) )  {

      rslt <- fieldbook_report(data = fieldbook()
                               , fb_smr = fbsmrvar()
                               , variable = input$rpt_variable
                               , dotplot_groups = input$rpt_dotplot_groups
                               , model_diag = FALSE
      )
    }

  })

  output$anova <- renderPrint({ anova(report()$anova) })
  
  output$dfreq <- renderPlot({
    
    diag <- report()$diagplot 
    plot_grid(plotlist = diag, ncol = 2)

  })

  output$dotplot <- renderPlot({

    validate( need( input$rpt_dotplot_groups, "Choose your groups") )

    report()$dotplot

  })

  # -------------------------------------------------------------------------
  
  output$rpt_digits <- renderUI({
    
    validate( need( input$rpt_variable, "Choose your variable") )
    
    numericInput(inputId = "rpt_digits"
                 , label = "Table digits"
                 , value = 2
                 , step = 1
                 , min = 0
                 , max = 6
    )
  })
  
  # -------------------------------------------------------------------------

  mean_comp <- reactive({

    validate( need( input$rpt_variable, "Choose your variable") )

    if ( !is.null( fieldbook() ) & !is.null( fbsmrvar() ) )  {

      mc <- mean_comparison(data = fieldbook()
                            , fb_smr = fbsmrvar()
                            , variable = input$rpt_variable
                            , graph_opts = T
                            , digits = input$rpt_digits
                            )
    }

  })

  output$mc_table <- DT::renderDataTable(server = FALSE, {

    mc <- mean_comp()$comparison %>%
      select(!c("{colors}", "{arguments}", "{values}")) %>%
      inti::web_table(digits = input$rpt_digits)

  })

  output$mc_stats <- DT::renderDataTable(server = FALSE, {

    mc <- mean_comp()$stats %>%
      select(!c(name.t, MSerror, Df)) %>%
      select(!intersect(names(.), c("StudentizedRange", "MSD"))) %>%
      inti::web_table(buttons = "copy")

  })

  # export meancomparison table ---------------------------------------------

  observeEvent(input$export_mctab, {
    
    sheet_export <- input$sheet_export %>% gsub("[[:space:]]", "_", .)

    if ( !sheet_export %in% sheet_names(gs()) ) {

      sheet_add( ss = gs(), sheet = sheet_export )

      mean_comp()$comparison %>% 
        sheet_write(ss = gs(), sheet = sheet_export )

    } else { print ("sheet already exist") }

  })
  
  # -------------------------------------------------------------------------

  output$rpt_preview <- renderUI({
    
    if ( input$rpt_preview_opt == "Gsheet" ) {

      tags$iframe(src = fbsm_url(),
                  style="height:580px; width:100%; scrolling=no")

    } else if ( input$rpt_preview_opt == "Model" & !is.null(input$rpt_variable) ) {

      validate( need( input$rpt_variable, "Choose your variable") )
      
      tagList(

        fluidRow(

          column(width = 5,

                 HTML('<h4><strong>ANOVA</strong></h4>'),

                 verbatimTextOutput("anova"),
                 
                 br(),

                 HTML('<h4><strong>Statistics</strong></h4>'),

                 DT::dataTableOutput("mc_stats")

          ),

          column(width = 7,

                 HTML('<h4><strong>Mean Comparison</strong></h4>'),

                 DT::dataTableOutput("mc_table"),

                 br(),

                 actionButton(inputId = "export_mctab"
                              , label = "Export table"
                              , class = "btn btn-warning"
                 )
          )
        )
      )

    } else if ( input$rpt_preview_opt == "Plots" & !is.null(input$rpt_variable) ) {

      tagList(

        fluidRow(

          column(width = 6,

                 HTML('<h4><strong>Model Diagnostic</strong></h4>'),

                 plotOutput("dfreq", width =  "auto", height = "500px")

          ),

          column(width = 6,

                 HTML('<h4><strong>Variable Distribution</strong></h4>'),

                 plotOutput("dotplot", width =  "auto", height = "500px")

          )
        )
      )
    }
  })

  # Yupana: Graphics --------------------------------------------------------
  # -------------------------------------------------------------------------

  sheet_grp <- eventReactive(input$graph_refresh, {
    
    fbinfo <- c(input$fieldbook_gsheet, input$fbsmrvars_gsheet)
    
    gs() %>% sheet_names() %>% setdiff(., fbinfo)
    
    })

  output$graph_sheets <- renderUI({

    selectInput(inputId = "graph_sheets"
                , label = "Graph sheet"
                , choices = c("choose" = ""
                              , sheet_grp()
                              )
                )
    })

  # -------------------------------------------------------------------------

  plot_url <- reactive({

    info <- gs4_get(gs())

    url <- info$spreadsheet_url

    id <- info$sheets %>%
      filter(name %in% input$graph_sheets) %>%
      pluck("id")

    plot_url  <- paste(url, id, sep = "#gid=")

  })

  # -------------------------------------------------------------------------

  
  
  plotdt <- eventReactive(input$graph_create, {
    
    validate( need( input$graph_sheets, "Refresh and choose a sheet") )
    
    if ( input$graph_sheets %in% sheet_names(gs()) ) {
      
      plottb <- gs() %>%
        range_read( input$graph_sheets )
      
    } else {"Choose a summary table"}
    
    })
  
  plotgr <- reactive({ plot_smr(plotdt()) })

  # -------------------------------------------------------------------------

  output$plotgr <- renderImage({
    
    validate( need( input$graph_sheets, "Refresh and choose a sheet") )
    
    dim <- plotdt() %>% 
      select('{arguments}',	'{values}') %>% 
      tibble::deframe() %>% 
      .["dimensions"] %>% 
      strsplit(., "[*]") %>% 
      pluck(1) %>% as.numeric()
    
    if(!is.na(dim[1])) { ancho <- dim[1] } else {ancho <- input$graph_width}
    if(!is.na(dim[2])) { alto <- dim[2] } else {alto <- input$graph_height}
    if(!is.na(dim[3])) { dpi <- dim[3] } else {dpi <- input$graph_dpi}
    
    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)
    print(plotgr())
    dev.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # -------------------------------------------------------------------------

  output$graph_preview <- renderUI({

    if ( input$grp_preview_opt == "Gsheet" ) {

      tags$iframe(src = plot_url(),
                  style="height:580px; width:100%; scrolling=no")

    } else if ( input$grp_preview_opt == "Plots" ) {

      tagList(

        div(imageOutput("plotgr"), align = "center")

      )
    }
  })

  # Yupana: Multivariate-----------------------------------------------------
  # -------------------------------------------------------------------------

  output$mvr_facts <- renderUI({

    if ( !is.null( fieldbook() ) & !is.null( fbsmrvar() ) ) {

      mvr_variable_names <- fbsmrvar() %>%
        filter(type %in% c("factor", "factores", "factors")) %>%
        select(variables) %>%
        deframe()

      selectInput(inputId = "mvr_facts"
                  , label = "Factors"
                  , multiple = T
                  , choices = c("choose" = ""
                                , mvr_variable_names)
      )

    } else { print ("Insert fieldbook summary") }

  })

  # -------------------------------------------------------------------------

  output$mvr_groups <- renderUI({
    
    validate(need(input$mvr_facts, "Insert summary factors"))

    if ( !is.null(input$mvr_facts) ) {

      selectInput(inputId = "mvr_groups"
                  , label = "Groups"
                  , choices = c(input$mvr_facts)
      )

    } else { print ("Insert summary factors") }

  })

  # -------------------------------------------------------------------------

  mvr <- reactive({
    
    n <- fieldbook() %>%
      select(input$mvr_facts)  %>% 
      unlist() %>% 
      unique() %>% 
      length()
    
    validate(need(n>2, "The factors should have more than 2 levels"))
    
    mvr <- fieldbook_mvr(data = fieldbook()
                         , fb_smr = fbsmrvar()
                         , summary_by = input$mvr_facts
                         , groups = input$mvr_groups)
  })

  # -------------------------------------------------------------------------

  output$pca_var <- renderImage({
    
    validate(need(mvr()$pca, "Insert summary factors"))
    
    dpi <- input$mvr_dpi
    ancho <- input$mvr_width
    alto <- input$mvr_height

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
    
    validate(need(mvr()$pca, "Insert summary factors"))

    dpi <- input$mvr_dpi
    ancho <- input$mvr_width
    alto <- input$mvr_height

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

    plot.PCA(x = mvr()$pca
             , choice = "ind"
             , habillage = mvr()$param$groups_n
             , invisible = "quali"
             , autoLab = "auto"
             , shadowtext = T
    ) %>% print()

    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)


  # -------------------------------------------------------------------------

  output$hcpc_tree <- renderImage({
    
    validate(need(mvr()$pca, "Insert summary factors"))

    dpi <- input$mvr_dpi
    ancho <- input$mvr_width
    alto <- input$mvr_height

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

    plot.HCPC(x = mvr()$hcpc, choice = "tree")

    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # -------------------------------------------------------------------------

  output$hcpc_map <- renderImage({
    
    validate(need(mvr()$pca, "Insert summary factors"))

    dpi <- input$mvr_dpi
    ancho <- input$mvr_width
    alto <- input$mvr_height

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

    plot.HCPC(x = mvr()$hcpc
              , choice = "map"
              , legend = list(bty = "y"
                              , x = "topright")
              , draw.tree = F
    )

    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # -------------------------------------------------------------------------

  output$correlation <- renderImage({
    
    validate(need(mvr()$pca, "Insert summary factors"))

    dpi <- input$mvr_dpi
    ancho <- input$mvr_width + 5
    alto <- input$mvr_height + 5

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

    corrplot(mvr()$corr$correlation
             , method="number"
             , tl.col="black"
             , tl.srt=45
    )

    graphics.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # -------------------------------------------------------------------------

  output$mvr_preview <- renderUI({
    
    validate(need(input$mvr_facts, "Insert summary factors"))
    
    if ( input$mvr_module == "PCA" & !is.null(input$mvr_facts) ) {

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

    } else if ( input$mvr_module == "HCPC" & !is.null(input$mvr_facts) ) {

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

    } else if (input$mvr_module == "CORR" & !is.null(input$mvr_facts) ) {

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
