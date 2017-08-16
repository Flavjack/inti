dtr_choices <- c("log(y)"="logy",
                 "log(y+1)"="logy1",
                 "Square root (y)"="sqrty",
                 "Square root (y+0.5)"="sqrty1",
                 "Arc-sine"="arcsin"
)

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Info boxes"),
  dashboardSidebar(),
  dashboardBody(
    # infoBoxes with fill=FALSE
    fluidRow(


      shiny::fileInput(inputId = "dtr_fileInput", label = "Upload Fieldbook", multiple = FALSE,
                       accept = ".xlsx", placeholder = "Please select file"),


      div(style="display: inline-block;vertical-align:top; width: 150px;",
      uiOutput("trait_dtr_sel")),

      div(style="display: inline-block;vertical-align:top; width: 150px;",
      shiny::selectInput(inputId = "dtr_type", label = "Type of transformation",
                         choices = dtr_choices, selected = 1, multiple = FALSE)),

      #Tabla de transformacion de variables.
      DT::dataTableOutput("fbdt_tempdtr")#,

    )#,


  )
)

server <- function(input, output) {


  fbdtr <- reactive({

    #req(input$dtr_fileInput)
    if(is.null(input$dtr_fileInput) || input$dtr_fileInput=="") {return(NULL)}
    else{
    dtr_xlsx <- input$dtr_fileInput
    file.rename(from = dtr_xlsx$datapath, to = paste(dtr_xlsx$datapath, ".xlsx", sep = ""))
    fb_xlsx <- try(fieldbook::getData(dir = paste(dtr_xlsx$datapath, ".xlsx", sep = ""),
                                      sheet = 1))

    fb_xlsx}

  })

  output$trait_dtr_sel <- renderUI({
    trait_selection <- names(fbdtr())
    selectInput(inputId = "trait_dtr", label = "Select Trait", choices = trait_selection, selected = 1, multiple = FALSE)
  })

  fb_tempdtr <- shiny::reactive({

    req(input$dtr_fileInput)
    fieldbook <- as.data.frame(fbdtr())
    trait_dtr_input <- input$trait_dtr
    fieldbook <- st4gi::dtr(trait = trait_dtr_input, type = input$dtr_type, base = 10, data = fieldbook)
  })

  output$fbdt_dtr = DT::renderDataTable( server = FALSE, {


    table_dtr <- fbdtr()

    DT::datatable(table_dtr,

                  filter = 'top',
                  extensions = c('Buttons', 'Scroller'),
                  rownames = FALSE,

                  options = list(
                    pageLength =  nrow(table_dtr),
                    searchHighlight = TRUE,
                    searching = TRUE,

                    dom = 'Bfrtip',
                    buttons = list(
                      'copy',
                      list(extend = 'csv', filename = "FieldBook"),
                      list(extend = 'excel', filename = "FieldBook")
                    ),

                    #buttons = c('copy', 'csv', 'excel'),

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

  output$fbdt_tempdtr = DT::renderDataTable( server = FALSE, {


    table_tempdtr <- fb_tempdtr()

    DT::datatable(table_tempdtr,

                  filter = 'top',
                  extensions = c('Buttons', 'Scroller'),
                  rownames = FALSE,

                  options = list(
                    pageLength =  nrow(table_tempdtr),
                    searchHighlight = TRUE,
                    searching = TRUE,

                    dom = 'Bfrtip',
                    buttons = list(
                      'copy',
                      list(extend = 'csv', filename = "FieldBook"),
                      list(extend = 'excel', filename = "FieldBook")
                    ),

                    #buttons = c('copy', 'csv', 'excel'),

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





}

shinyApp(ui, server)
