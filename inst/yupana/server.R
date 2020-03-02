# yupana ------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(agricolae)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rhandsontable)
library(yupana)
library(stringr)
library(shinyBS)
library(openxlsx)

options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, session) {


# User Manual ------------------------------------------------------------

output$usm <- renderUI({

    getPage <- function() {

    HTML('<iframe src="https://flavjack.github.io/yupana-usm/" style="border: 0; position:fixed; top:50px; left:0; right:0; bottom:50px; width:100%; height:92%">')

    }

    getPage()

  })


# fieldbook  --------------------------------------------------------

fdbk <- reactive({

    #print(input$tool_design)
    print(input$tool_layout)
    #print(input$input$tool_sp_import)

    # When radio button selection is Standard

    if(input$tool_layout == 'Standard'){

      validate(
        need( input$tool_f1, "Insert levels for your experiment")
      )

      trt1 <- input$tool_f1
      trt2 <- input$tool_f2
      # dsg <-  input$tool_dsg
      lbl1 <- input$tool_lb1
      lbl2 <- input$tool_lb2

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

    }

    # When radio button selection is Special

    if(input$tool_layout == 'Template'){
      #else{

      validate(
        need( input$tool_template_upload, "Please upload your template")
      )

      fbook_xlsx_file <- input$tool_template_upload
      print(fbook_xlsx_file)
      print(fbook_xlsx_file$datapath)

      if (is.null(fbook_xlsx_file)) {
        return()
      } else {

        xlsx <- input$tool_template_upload
        file.rename(xlsx$datapath, paste(xlsx$datapath, ".xlsx", sep = ""))
        fb_xlsx <- try(fieldbook::getData(dir = paste(xlsx$datapath, ".xlsx", sep = ""), sheet = 1))


        #fb_xlsx <- try(readxl::read_excel(path = fbook_csv_file$datapath,sheet = 1 ))

      }

      trt1 <- stringr::str_trim(fb_xlsx[,1], side = "both")
      #print(trt1)
      trt1 <- trt1[!is.na(trt1)]
      print(trt1)
      #In case trt2 is empty in csv template (trt2 vacio)
      if(ncol(fb_xlsx)>=2){
       trt2 <- stringr::str_trim(fb_xlsx[,2], side = "both")
       trt2 <- trt2[!is.na(trt2)]
       print(trt2)
      }

      if(input$tool_dsg  == 'f2crd' ||  input$tool_dsg  == 'f2rcbd' || input$tool_dsg  == 'f2lsd'){

            if(length(trt2)==0){
                 trt2 <- NULL

                 validate(
                   need( !is.null(trt2), "Please fill the second column treatment (trt2) in your template")
                 )


            } else {
               trt2 <- trt2
            }
      } else{
            trt2 <- NULL
      }

      #print(trt2)
      cln <- colnames(fb_xlsx)
      lbl1 <- cln[1]
      lbl2 <- cln[2]

    }


    dsg <-  input$tool_dsg
    r <- input$tool_rep
    smp <- input$tool_smp
    int <- input$tool_eva


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

    # validate(
    #   need( length(trt1)>1, "Please fill vars")
    # )


    #Creation of fieldbook reactive expression
    fieldbook::design_fieldbook(
      treat1 = trt1,
      treat2 = trt2,
      rep = r,
      design = dsg,
      lbl_treat1 = lbl1,
      lbl_treat2 = lbl2,
      variables = vars,
      sample = smp,
      intime = int
    )


  })

##
observe({

  if(input$tool_layout=="Template"){

  if(is.null(input$tool_template_upload)  || length(input$tool_template_upload)==0) {


  } else {

      dtp <-   try(fdbk())
      class_dtp <- class(dtp)
      nr <- nrow(dtp)

     if(class_dtp == "try-error"){

      # print("case true")
        shinysky::showshinyalert(session, "alert_tool_fbmsg",
                                 paste("ERROR: Your fieldbook template is empty. Please fill it."),
                                 styleclass = "danger")
    #}
     } else if(length(dt)==0){

       shinysky::showshinyalert(session, "alert_tool_fbmsg",
                                paste("ERROR: Your fieldbook template is empty. Please fill it."),
                                styleclass = "danger")

     } else {

        shinysky::showshinyalert(session, "alert_tool_fbmsg",
                                 paste("Fantastic: Your fieldbook template has been successfully uploaded."),
                                 styleclass = "success")
     }
  }
#print("omar")

  }
})


# Fieldbook table ---------------------------------------------------------

output$fbdsg = DT::renderDataTable( server = FALSE, {


    file <- fdbk()

    DT::datatable(file,

                  filter = 'top',
                  extensions = c('Buttons', 'Scroller'),
                  rownames = FALSE,

                  options = list(
                    pageLength =  nrow(file),
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

shiny::observeEvent(input$tool_reset_page,{
  session$reload()
})


# import data -----------------------------------------------------------

data_fb <-  reactive({

    validate(
      need( input$fbdt, message = "Insert a Google spreadsheet URL or xlsx file" )
    )

    #print(input$fb_import)

    if( input$fb_import=="Local"){

      if(is.null(input$impdata)){return()}
      if(!is.null(input$impdata)){

        xls <- input$impdata
        file.rename(xls$datapath, paste(xls$datapath, ".xlsx", sep = ""))
        out <- fieldbook::getData(dir = paste(xls$datapath, ".xlsx", sep = ""), sheet = input$sheetdt)

      }
    }

    if(input$fb_import=="Google"){

      url <- input$fbdt
      if(is.null(url)){return()}
      out <- fieldbook::getData(dir = url)

    }
out
#print(out)


  })

output$fbook <- renderUI({

  gss <- tags$iframe(src = input$fbdt,
    style="height:450px; width:100%; scrolling=no")

  print(gss)

})

output$fbook_excel <- renderRHandsontable({

  req(input$impdata)
  req(input$fbdt)


  #print(data_fb())
  rhandsontable(data_fb(), width = 600, height = 500)


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


# Data analysis -----------------------------------------------------------

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
addTooltip(session, "bpx", title = "Select your explanatory variable",  trigger = 'hover')


output$bpy <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "ybp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})
addTooltip(session, "bpy", title = "Select your response variable",  trigger = 'hover')


output$bpz <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "zbp",
    label = "Grouped",
    choices = c("choose" = "", fbn)
  )

})
addTooltip(session, "bpz", title = "Select variable to group observations.",  trigger = 'hover')



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


  boxp <- fieldbook::plot_box(

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

  fieldbook::plot_correlation(
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




  fieldbook::plot_PCA(
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
addTooltip(session, "stat_response", title = "Select the response variable",  trigger = 'hover')



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
addTooltip(session, "stat_factor", title = "Select the factor variable",  trigger = 'hover')


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
addTooltip(session, "stat_block", title = "Select this option if your design has Blocks. Example: Randomized Block Design",  trigger = 'hover')


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

    file <- file %>% dplyr::mutate_each_(funs(factor(.)), c(input$stat_fact, input$stat_blk))


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


# descriptive Statistics

output$stat_summary = renderTable({

  file <- av()
  dt <-  fb()

  stat_sm(modelo = file, data = dt)

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

    rs <- fieldbook::test_comparison(
      aov = file,
      comp = factor[1],
      type = test,
      sig = sig)


  }


  else if( length(factor) >= 2 && !(variable == '') )

  {

    rs <- fieldbook::test_comparison(
      aov = file,
      comp = c( factor[1], factor[2] ),
      type = test,
      sig = sig)


  }


  rs


})



# Mean comparison table

output$mnc = DT::renderDataTable(server = FALSE, {

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

# Assumptions -------------------------------------------------------------


assuption_homvar <- reactive({

  plot(av(),
       main = "Homogeneity of Variance",
       # caption = "caption",
       sub.caption = "",
       which = 1)

})

output$assuption_plot01  <- renderPlot({

  assuption_homvar()

})


assuption_norm <- reactive({

  plot(av(),
       main = "Normal distribution plot",
       # caption = "caption",
       sub.caption = "",
       which = 2)


})

output$assuption_plot02  <- renderPlot({

  assuption_norm()

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

         pt <- fieldbook::plot_brln(data = df, type = gtype,
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


  pt <- fieldbook::plot_brln(data = df, type = gtype,
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



# Lineal regression -------------------------------------------------------

output$lrg_variable1 <- renderUI({

  file <- fb()
  fbn <- names(file)
  selectInput(inputId = "lrg_var1", label = "Variable", choices = c("choose" = "", fbn))

})
addTooltip(session, "lrg_variable1", title = "Select your independent variable",  trigger = 'hover')



output$lrg_variable2 <- renderUI({

  file <- fb()
  fbn <- names(file)
  selectInput(inputId = "lrg_var2", label = "Variable", choices = c("choose" = "", fbn)
  )

})
addTooltip(session, "lrg_variable2", title = "Select your dependent variable",  trigger = 'hover')


output$lrg_grouped <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "lrg_group",
    label = "Grouped",
    choices = c("choose" = "", fbn)
  )

})
addTooltip(session, "lrg_grouped", title = "Select variable to group regression lines",  trigger = 'hover')




plot_lr <- reactive({

  validate(

    need( input$lrg_var1, "Select your independent variable"),
    need( input$lrg_var2, "Select your depedent variable" )

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
  rlx <- input$lr_eq_x
  rly <- input$lr_eq_y


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

  if ( is.na(rlx) ){

    rlx <- NULL

  }

  if ( is.na(rly) ){

    rly <- NULL

  }

  fieldbook::plot_linereg(
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
    font = sfn,
    rlx = rlx,
    rly = rly
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


output$tool_template_download <- downloadHandler(
  filename = function() {
    paste("fb_template", '.xlsx', sep='')
  },
  content = function(file) {
#print("omar")

#shiny::observeEvent(input$tool_template_download,{

    hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=11,
                      fontName="Arial Narrow", fgFill = "#4F80BD")

    template <- fb_template
    #file_xlsx_tp <- "qfb_template.xlsx"
    #file_temp_XLSX <- system.file("exdata", "qfb_template.xlsx", package = "fieldbook")
    #try(system(paste("open", file_temp_XLSX)))
    openxlsx::write.xlsx(x = template, file, headerStyle = hs, sheetName = "fb")
  #  try(system("open", file_temp_XLSX))

  }
)


})