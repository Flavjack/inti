# fieldbook -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(sapiens)
library(agricolae)
library(dplyr)
library(tibble)
library(DT)
library(ggplot2)



shinyServer(function(input, output) {


# import data -----------------------------------------------------------

data_fb <-  eventReactive(input$reload, {


  validate(

    need( input$fbdt, message = "Insert a Google spreadsheet URL or xlsx file" )

    )


  if ( !is.null(input$impdata) ) {

    xls <- input$impdata

    file.rename(xls$datapath, paste(xls$datapath, ".xlsx", sep = ""))

    sapiens::getData(dir = paste(xls$datapath, ".xlsx", sep = ""), sheet = input$sheetdt)


  } else {

    url <- input$fbdt

    sapiens::getData(dir = url, sheet = input$sheetdt)

  }



    }, ignoreNULL = FALSE)



output$fbook <- renderUI({

  gss <- tags$iframe(src = input$fbdt,
    style="height:450px; width:100%; scrolling=no")

  print(gss)

})


# Filter ------------------------------------------------------------------

output$filter_01 <- renderUI({

  file <- data_fb()
  fbn <- names(file)

  selectInput(
    inputId = "filter_nm01",
    label = "Factor",
    choices = c("choose" = "", fbn)
  )

})

output$filter_fact01 <- renderUI({

  validate(

    need( input$filter_nm01, "Select your levels")

  )

  file <- data_fb()
  fl <- file[, input$filter_nm01]

  selectInput(
    inputId = "filter_ft01",
    label = "Levels",
    choices = c("choose" = "", fl),
    multiple = TRUE
  )

})

output$filter_02 <- renderUI({

  file <- data_fb()
  fbn <- names(file)

  selectInput(
    inputId = "filter_nm02",
    label = "Factor",
    choices = c("choose" = "", fbn)
  )

})

output$filter_fact02 <- renderUI({

  validate(

    need( input$filter_nm02, "Select your levels")

  )

  file <- data_fb()
  fl <- file[, input$filter_nm02]

  selectInput(
    inputId = "filter_ft02",
    label = "Levels",
    choices = c("choose" = "", fl),
    multiple = TRUE
  )

})


# Data analisys -----------------------------------------------------------


fb <- reactive({


  file <- data_fb()

  fc1 <- input$filter_nm01
  lv1 <- input$filter_ft01

  fc2 <- input$filter_nm02
  lv2 <- input$filter_ft02


  if( fc1 == "" && fc2 == "" ){


  dt <- file


  } else if (fc1 != "" && fc2 != ""){

    dt <- file %>%
      subset( eval(parse(text = fc1)) %in% lv1 & eval(parse(text = fc2)) %in% lv2 )


        if ( length(lv1) == 1){

          dt[, fc1] <- NULL
        }

        if ( length(lv2) == 1){

          dt[, fc2] <- NULL
        }

  } else if (fc1 != "" && fc2 == "" ){


    dt <- file %>%
      subset( eval(parse(text = fc1)) %in% lv1 )


          if ( length(lv1) == 1){

            dt[, fc1] <- NULL
          }

  } else if (fc1 == "" && fc2 != "" ){

    dt <- file %>%
      subset( eval(parse(text = fc2)) %in% lv2 )


          if ( length(lv2) == 1){

            dt[, fc2] <- NULL
          }

  }


  dt


})


# boxplot -----------------------------------------------------------------

output$bpx <- renderUI({


  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "xbp",
    label = "Axis X",
    choices = c("choose" = "", fbn)
  )

})

output$bpy <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "ybp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})


output$bpz <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "zbp",
    label = "Grouped",
    choices = c("choose" = "", fbn)
  )

})


output$boxplot <- renderPlot({

  validate(

    need( input$ybp, "Select your response variable"),
    need( input$xbp, "Select your X axis variable" ),
    need( input$zbp, "Select your grouped variable")

  )



  file <- fb()


  variable <- input$ybp
  fx <-  input$xbp
  fz <-  input$zbp
  gply <- input$bply
  gplx <- input$bplx
  gplz <- input$bplz
  brk <- input$bpbrk

  # Title axis --------------------------------------------------------------

  if ( gply == ""){

    gply <- NULL

  }

  if ( gplx == ""){

    gplx <- NULL

  }


  if ( gplz == ""){

    gplz <- NULL

  }


  if(is.na(brk)){

    brks <- NULL

  } else { brks <- brk}


  boxp <- sapiens::plot_box(

    data = file,
    y = variable,
    x = fx,
    z = fz,
    xlab = gplx,
    ylab = gply,
    lgl =  gplz,
    lgd = "top",
    font = input$bpsize,
    brk = brks

  )

  boxp


})


# multivariate ------------------------------------------------------------

output$crpt <- renderPlot({

  file <- fb()

  sapiens::plot_correlation(
    data = file,
    sig = input$corsig,
    color = input$corcol,
    font = input$cor_font)

})


output$pca <- renderPlot({

  file <- fb()


  if( is.na(input$pcaqs) ){

    qs <- NULL

  } else {

    qs <- input$pcaqs

  }


  if( input$pcalbl == "" ){

    lbl <- NULL

  } else {

    lbl <- input$pcalbl

  }




  sapiens::plot_PCA(
    data = file,
    type = input$pcatype,
    quali.sup = qs,
    lgl = lbl
    )


})


# statistics --------------------------------------------------------------

# Select factors


output$stat_response <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "stat_rsp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})


output$stat_factor <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "stat_fact",
    label = "Factors",
    choices = c("choose" = "", fbn),
    multiple = TRUE
  )

})



output$stat_block <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "stat_blk",
    label = "Block",
    choices = c("choose" = "", fbn),
    multiple = TRUE
  )

})

# ANOVA


av <- reactive({

  validate(

    need( input$stat_rsp, "Select your response variable" ),
    need( input$stat_fact, "Select your factors")

  )

    file <- fb()

    variable <- input$stat_rsp

    factor <- input$stat_fact %>% paste0() %>%  paste(collapse= " * ")

    block <- input$stat_blk %>% paste0() %>% paste(collapse= " + ")

    file <- file %>% dplyr::mutate_each_(funs(factor(.)), input$stat_fact)


    if ( block == "" ){

      formula <- as.formula(paste( variable , factor, sep = " ~ "))


    } else {

      formula <- as.formula(paste( variable , paste(block, factor, sep = " + "), sep = " ~ "))

    }


    av <- aov(formula, data = file)
    av



})


# ANOVA table

output$tbav = renderPrint({

  file <- av()

  summary(file)


})


# comparison test


comp <- reactive({


  file <- av()
  test <- input$stmc
  sig <- input$stsig
  factor <- input$stat_fact
  variable <- input$stat_rsp


  if( length(factor) == 1 && !(variable == '') )

  {

    rs <- sapiens::test_comparison(
      aov = file,
      comp = factor[1],
      type = test,
      sig = sig)


  }


  else if( length(factor) >= 2 && !(variable == '') )

  {

    rs <- sapiens::test_comparison(
      aov = file,
      comp = c( factor[1], factor[2] ),
      type = test,
      sig = sig)


  }


  rs


})



# Mean comparison table

output$mnc = DT::renderDataTable({

  file <- comp()

  file <- file %>% format(digits = 3, nsmall = 3)


  DT::datatable(file,

    filter = 'top',
    extensions = c('Buttons', 'Scroller'),
    rownames = FALSE,

    options = list(

      searchHighlight = TRUE,
      searching = TRUE,

      dom = 'Bfrtip',
      buttons = list(
        'copy',
        list(extend = 'csv', filename = input$stat_rsp),
        list(extend = 'excel', filename = input$stat_rsp)
        ),

      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets ="_all")),
      deferRender=TRUE,
      scrollY = 400,
      scrollX = TRUE,
      scroller = TRUE,

      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))

})


# graphics ----------------------------------------------------------------

stat_plot <- reactive({

df <- comp()

factor <- input$stat_fact
variable <- input$stat_rsp

gtype <- input$gtype
gcolor <- input$gcolor

gply <- input$gply
gplx <- input$gplx
gplz <- input$gplz

gerbr <- input$gerbr
gsig <- input$gsig
gfont <- input$gfont
glabel <- input$glabel

limits <- input$glmti * input$glmtf
brakes <- input$gbrakes

xbl <- input$gp_xbk
zbl <- input$gp_zbk


# Title axis --------------------------------------------------------------

if ( gply == ""){

  gply <- variable

} else {

  gply <- input$gply

}

if ( gplx == ""){

  gplx <- NULL

}

if ( gplz == ""){

  gplz <- NULL

}


# Color -------------------------------------------------------------------

if ( gcolor == "yes" ){

  gcolor <- TRUE

} else {

  gcolor <- FALSE

}



# Label brake axis --------------------------------------------------------


if ( xbl == ""){

  xbl <- NULL

} else {

  xbl <- input$gp_xbk

}

if ( zbl == ""){

  zbl <- NULL

} else {

  zbl <- input$gp_zbk

}

# limits & brake ----------------------------------------------------------

if(is.na(limits)) {

  glimits <- NULL

} else {

  glimits <- c(input$glmti, input$glmtf)

}


if(is.na(brakes)) {

  gbrakes <- NULL

} else {

  gbrakes <- brakes

  }


# Error & significance ----------------------------------------------------

if(gerbr == "yes"){

  gerbr <- TRUE

}

if (gerbr == "no"){

  gerbr <-  FALSE

  }


if(gsig == "yes"){

  gsig <- "sg"

}

if (gsig == "no"){

  gsig <-  NULL

  }



# body graph --------------------------------------------------------------


if( length(factor) == 1 && !(variable == '') ){

         pt <- sapiens::plot_brln(data = df, type = gtype,
                             x = factor[1],
                             y = "mean",
                             z = factor[1],
                             ylab = gply,
                             xlab = gplx,
                             lgl = gplz,
                             lgd = glabel,
                             erb = gerbr,
                             sig = gsig,
                             font = gfont,
                             lmt = glimits,
                             brk = gbrakes,
                             xbl = xbl,
                             zbl = zbl,
                             color = gcolor)


}


else if( length(factor) >= 2  && !(variable == ''))

{


  pt <- sapiens::plot_brln(data = df, type = gtype,
    x = factor[1],
    y = "mean",
    z = factor[2],
    ylab = gply,
    xlab = gplx,
    lgl = gplz,
    lgd = glabel,
    erb = gerbr,
    sig = gsig,
    font = gfont,
    lmt = glimits,
    brk = gbrakes,
    xbl = xbl,
    zbl = zbl,
    color = gcolor
  )


}


pt


})



# plot output -------------------------------------------------------------

output$stplot <- renderPlot({

  plot <-  stat_plot()
  plot

})

# download plot -----------------------------------------------------------

output$download_plot <- downloadHandler(
  file = function(){ paste( "plot_", input$stat_rsp, '.tiff', sep = '')},
  content = function(file){
    ggplot2::ggsave(file, plot = stat_plot(), device = "tiff", dpi = 300, width = input$plot_W, height = input$plot_H, units = "mm" )

  }
)


# fieldbook design --------------------------------------------------------



fdbk <- reactive({

  validate(
    need( input$tool_f1, "Insert levels for your experiment")
  )

  trt1 <- input$tool_f1
  trt2 <- input$tool_f2
  dsg <-  input$tool_dsg
  lbl1 <- input$tool_lb1
  lbl2 <- input$tool_lb2
  r <- input$tool_rep
  int <- input$tool_eva


  if( trt2 == "" ){

    trt2 <- NULL

  } else {

    trt2 <- input$tool_f2

  }

  if( trt1 == "" ){

    trt1 <- NULL

  } else {

    trt1 <- input$tool_f1

  }


  if( input$tool_rep == "" ){

    r <- NULL

  } else {

    r <- input$tool_rep

  }


  if( input$tool_var == "" ){

    vars <- NULL

  } else {

    vars <- input$tool_var

  }


  sapiens::design_fieldbook(
    treat1 = trt1,
    treat2 = trt2,
    rep = r,
    design = dsg,
    lbl_treat1 = lbl1,
    lbl_treat2 = lbl2,
    variables = vars,
    intime = int
    )


})



# Fieldbook table ---------------------------------------------------------

output$fbdsg = DT::renderDataTable({


file <- fdbk()

DT::datatable(file,

  filter = 'top',
  extensions = c('Buttons', 'Scroller'),
  rownames = FALSE,

  options = list(

    searchHighlight = TRUE,
    searching = TRUE,

    dom = 'Bfrtip',
    buttons = list(
      'copy',
      list(extend = 'csv', filename = "FieldBook"),
      list(extend = 'excel', filename = "FieldBook")
      ),

    autoWidth = TRUE,
    columnDefs = list(list(className = 'dt-center', targets ="_all")),
    deferRender=TRUE,
    scrollY = 400,
    scrollX = TRUE,
    scroller = TRUE,

    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ))


})



# Lineal regression -------------------------------------------------------

output$lrg_variable1 <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "lrg_var1",
    label = "Variable",
    choices = c("choose" = "", fbn)
  )

})


output$lrg_variable2 <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "lrg_var2",
    label = "Variable",
    choices = c("choose" = "", fbn)
  )

})



output$lrg_grouped <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "lrg_group",
    label = "Grouped",
    choices = c("choose" = "", fbn)
  )

})


plot_lr <- reactive({

  validate(

    need( input$lrg_var1, "Select your numeric variable"),
    need( input$lrg_var2, "Select your numeric variable" )

  )


  file <- fb()
  xvr <- input$lrg_var1
  yvr <- input$lrg_var2
  zvr <- input$lrg_group
  sfn <- input$lr_font
  col <- input$lr_color
  lgp <- input$lr_label
  xlab <- input$lr_lbv1
  ylab <- input$lr_lbv2
  lgl <- input$lr_lbgp
  xbk  <- input$lr_brk1
  ybk <- input$lr_brk2
  lvl <- input$lr_lglv


  if ( col == "yes" ){

    col <- TRUE

  } else {

    col <- FALSE

  }

  if ( zvr == "" ){

    zvr <- NULL

  }

  if ( ylab == "" ){

    ylab <- NULL

  }

  if ( xlab == "" ){

    xlab <- NULL

  }


  if ( lgl == "" ){

    lgl <- NULL

  }

  if ( lvl == "" ){

    lvl <- NULL

  }

  if ( is.na(ybk) ){

    ybk <- NULL

  }

  if ( is.na(xbk) ){

    xbk <- NULL

  }


  sapiens::plot_linereg(
    data = file,
    y = yvr,
    x = xvr,
    z = zvr,
    lgd = lgp,
    color = col,
    ylab = ylab,
    xlab =  xlab,
    lgl = lgl,
    xbrk = xbk,
    ybrk = ybk,
    zbl = lvl,
    font = sfn
  )
})



output$plot_regression <- renderPlot({

  plot <-  plot_lr()
  plot

})


# download reg plot -----------------------------------------------------------

output$download_plot_lr <- downloadHandler(
  file = function(){ paste( "plot_", input$lrg_var2, "_" ,  input$lrg_var1, '.tiff', sep = '')},
  content = function(file){
    ggplot2::ggsave(file, plot = plot_lr(), device = "tiff", dpi = 300, width = input$lr_plot_W, height = input$lr_plot_H, units = "mm" )

  }
)

})
