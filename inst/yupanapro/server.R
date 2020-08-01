# yupanapro----------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://flavjack.shinyapps.io/yupanapro/
# runApp('inst/tarpuy', port = 1221)
# browseURL("http://localhost:1221/")

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/spreadsheets"))

if (file.exists("setup.r")) { source("setup.r") }

library(shiny)
library(inti)
library(metathis)
library(tidyverse)
library(googlesheets4)
library(googleAuthR)
library(bootstraplib)
library(shinydashboard)
library(ggpubr)
library(FactoMineR)

gar_set_client(web_json = "www/yupanapro.json")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

# auth --------------------------------------------------------------------
# -------------------------------------------------------------------------

gar_shiny_auth(session)

access_token <- callModule(googleAuth_js, "js_token")

gs <- reactive({

  if(Sys.getenv('SHINY_PORT') == "") {

    gs4_auth(T)

  } else {

    gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets"
             , cache = FALSE
             , use_oob = TRUE
             , token = access_token())
    }

  as_sheets_id(input$fieldbook_url)

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

# Yupana: Fieldbook -------------------------------------------------------
# -------------------------------------------------------------------------

observe({

  cat("Fieldbook --------------------------------------------------\n")

  cat("fieldbook_gsheet")
  print(input$fieldbook_gsheet)

  cat("fbsmrvars_gsheet")
  print(input$fbsmrvars_gsheet)

  cat("last_factor")
  print(input$last_factor)

  cat("model_facts")
  print(input$model_facts)

  cat("comp_facts")
  print(input$comp_facts)

  cat("test_comp")
  print(input$test_comp)

  cat("sig_level")
  print(input$sig_level)

})

# -------------------------------------------------------------------------

output$fieldbook_preview <- renderUI({

  tags$iframe(src = fb_url()
              , style="height:450px; width:100%; scrolling=no; zoom:1.2")

})

# -------------------------------------------------------------------------

fieldbook <- reactive({

  if ( input$fieldbook_gsheet %in% sheet_names(gs()) ) {

    gs() %>%
      range_read( input$fieldbook_gsheet )

    } else { fieldbook <- NULL }

  })

refresh <- reactive({
  list(input$fbsm_refresh, input$mvr_refresh)
})

# make reactive!

fbsmrvar <- NULL
makeReactiveBinding("fbsmrvar")

observeEvent( refresh(), {

  if ( input$fbsmrvars_gsheet %in% sheet_names(gs()) ) {

    fbsmrvar <<- gs() %>%
      range_read( input$fbsmrvars_gsheet )

  } else { fbsmrvar <<- NULL }

})

# -------------------------------------------------------------------------

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

observeEvent(input$fbsmr_generate, {

  if ( !is.null( fieldbook() ) && input$last_factor != "" )  {

  fbsmr <- fieldbook_summary(data = fieldbook()
                             , last_factor = input$last_factor
                             , model_facts = input$model_facts
                             , comp_facts = paste0(input$comp_facts, collapse = ":")
                             , test_comp = input$test_comp
                             , sig_level = input$sig_level
                             )


  if ( !"fbsm" %in% sheet_names(gs()) ) {

    sheet_add(ss = gs(), .after = input$fieldbook_gsheet, sheet = "fbsm")

    fbsmr %>% sheet_write(ss = gs(), sheet = "fbsm")

  } else { print ("sheet already exist") }

  }

  })

# Yupana: Analysis --------------------------------------------------------
# -------------------------------------------------------------------------

observe({

  cat("Analysis --------------------------------------------------\n")

  cat("rpt_variable")
  print(input$rpt_variable)

  cat("rpt_dotplot_groups")
  print(input$rpt_dotplot_groups)

  cat("rpt_preview_opt")
  print(input$rpt_preview_opt)

})

# -------------------------------------------------------------------------

output$rpt_variable <- renderUI({

  if ( !is.null( fbsmrvar ) ) {

    rpt_variable_names <- fbsmrvar %>%
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

output$rpt_dotplot_groups <- renderUI({

  if ( !is.null( fbsmrvar ) ) {

    rpt_dotplot_groups_names <- fbsmrvar %>%
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

  if ( !is.null( fieldbook() ) & !is.null( fbsmrvar ) )  {

  rslt <- fieldbook_report(data = fieldbook()
                           , fb_smr = fbsmrvar
                           , variable = input$rpt_variable
                           , dotplot_groups = input$rpt_dotplot_groups
                           , model_diag = FALSE
                           )

  }

})

output$anova <- renderPrint({ summary(report()$anova) })

output$dfreq <- renderPlot({

  p1 <- report()$diagplot$freq
  p2 <- report()$diagplot$qqnorm
  p3 <- report()$diagplot$resid
  p4 <- report()$diagplot$sresid

  ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

})

output$dotplot <- renderPlot({ report()$dotplot })

# -------------------------------------------------------------------------

mean_comp <- reactive({

  if ( !is.null( fieldbook() ) & !is.null( fbsmrvar ) )  {

    mc <- mean_comparison(data = fieldbook()
                          , fb_smr = fbsmrvar
                          , variable = input$rpt_variable
                          , graph_opts = T
                          )
  }

})

output$mc_table <- DT::renderDataTable(server = FALSE, {

  mc <- mean_comp()$comparison %>%
    select(!c("{colors}", "{arguments}", "{values}")) %>%
    inti::web_table()

})

output$mc_stats <- renderTable({

  mc <- mean_comp()$stats %>%
    select(!c(name.t, MSerror, Df)) %>%
    select(!intersect(names(.), c("StudentizedRange", "MSD")))

})

# export meancomparison table ---------------------------------------------

observeEvent(input$export_mctab, {

  if ( !input$rpt_variable %in% sheet_names(gs()) ) {

    sheet_add(ss = gs()
              , .after = input$fbsmrvars_gsheet
              , sheet = input$rpt_variable)

    mean_comp()$comparison %>% sheet_write(ss = gs(), sheet = input$rpt_variable)

  } else { print ("sheet already exist") }


})

# -------------------------------------------------------------------------

output$rpt_preview <- renderUI({

  if ( input$rpt_preview_opt == "Gsheet" ) {

      tags$iframe(src = fbsm_url(),
            style="height:450px; width:100%; scrolling=no; zoom:1.2")

  } else if ( input$rpt_preview_opt == "Model" ) {

    tagList(

      fluidRow(


      column(width = 5,

             HTML('<h4><strong>Analysis of Variance</strong></h4>'),

             verbatimTextOutput("anova"),

             br(),

             HTML('<h4><strong>Statistics</strong></h4>'),

             tableOutput("mc_stats")

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

  } else if ( input$rpt_preview_opt == "Plots" ) {

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

observe({

  cat("Graphics --------------------------------------------------\n")

  cat("graph_sheets")
  print(input$graph_sheets)

  cat("graph_width")
  print(input$graph_width)

  cat("graph_height")
  print(input$graph_height)

  cat("graph_dpi")
  print(input$graph_dpi)

  cat("grp_format")
  print(input$grp_format)

})

# -------------------------------------------------------------------------

output$graph_sheets <- renderUI({

  sheet_names <- sheet_names(gs())
  sheet_exclude <- c(input$fieldbook_gsheet, input$fbsmrvars_gsheet)
  sheet_names <- sheet_names[!(sheet_names %in% sheet_exclude)]

  selectInput(inputId = "graph_sheets"
              , label = "Graph sheet"
              , choices = c(sheet_names)
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

plotgr <- NULL
makeReactiveBinding("plotgr")

observeEvent(input$graph_create, {

  if ( input$graph_sheets %in% sheet_names(gs()) ) {

    plot_table <- gs() %>%
      range_read( input$graph_sheets )

  }

  plotgr <<- plot_smr(plot_table)

})

# -------------------------------------------------------------------------

output$plotgr <- renderImage({

  dpi <- input$graph_dpi
  ancho <- input$graph_width
  alto <- input$graph_height

  outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)
    print(plotgr)
    dev.off()

    list(src = outfile)

}, deleteFile = TRUE)

# -------------------------------------------------------------------------

output$graph_preview <- renderUI({

  if ( input$grp_preview_opt == "Gsheet" ) {

      tags$iframe(src = plot_url(),
                  style="height:450px; width:100%; scrolling=no; zoom:1.2")

  } else if ( input$grp_preview_opt == "Plots" ) {

    tagList(

      div(imageOutput("plotgr"), align = "center")

    )
  }
})

# Yupana: Multivariate-----------------------------------------------------
# -------------------------------------------------------------------------

observe({

  cat("Multivariate --------------------------------------------------\n")

  cat("mvr_width")
  print(input$mvr_width)

  cat("mvr_height")
  print(input$mvr_height)

  cat("mvr_dpi")
  print(input$mvr_dpi)

  cat("mvr_facts")
  print(input$mvr_facts)

})

# -------------------------------------------------------------------------

output$mvr_facts <- renderUI({

  if ( !is.null( fieldbook() ) & !is.null( fbsmrvar ) ) {

    mvr_variable_names <- fbsmrvar %>%
      filter(type %in% c("factor", "factores", "factors")) %>%
      select(variables) %>%
      deframe()

    selectInput(inputId = "mvr_facts"
                , label = "Variable"
                , multiple = T
                , choices = c("choose" = ""
                              , mvr_variable_names)
    )

  } else { print ("Insert fieldbook summary") }

})

# -------------------------------------------------------------------------

mvr <- reactive({

  quali <- input$mvr_facts

 mvr <- fieldbook_mvr(data = fieldbook()
                       , fb_smr = fbsmrvar
                       , quali_sup = quali
                       )

  })


# -------------------------------------------------------------------------

output$pca_var <- renderImage({

  dpi <- input$mvr_dpi
  ancho <- input$mvr_width
  alto <- input$mvr_height

  outfile <- tempfile(fileext = ".png")

  png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

  plot.PCA(x = mvr()$pca
           , choix = "var"
           ) %>% print()

  graphics.off()

  list(src = outfile)

}, deleteFile = TRUE)

# -------------------------------------------------------------------------

output$pca_ind <- renderImage({

  dpi <- input$mvr_dpi
  ancho <- input$mvr_width
  alto <- input$mvr_height

  outfile <- tempfile(fileext = ".png")

  png(outfile, width = ancho, height = alto, units = "cm", res = dpi)

  plot.PCA(x = mvr()$pca
           , choice = "ind"
           , habillage = 1
           ) %>% print()

  graphics.off()

  list(src = outfile)

}, deleteFile = TRUE)


# -------------------------------------------------------------------------

output$hcpc_tree <- renderImage({

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

output$mvr_preview <- renderUI({

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

  }
})

# end yupana --------------------------------------------------------------
# -------------------------------------------------------------------------

})


