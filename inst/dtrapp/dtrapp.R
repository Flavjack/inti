dtr_choices <- c("Logarithmic transformation log(y)"="logy",
                 "Logarithmic transformation log(y + 1)"="logy1",
                 "Square root transformation sqrt(y)"="sqrty",
                 "Square root transformation sqrt(y + 0.5)"="sqrty1",
                 "Arc-sine transformation arcsin"="arcsin"
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

      uiOutput("trait_dtr_sel"),

      shiny::selectInput(inputId = "dtr_type", label = "Type of transformation", choices = dtr_choices, selected = 1, multiple = FALSE),

      #DT::dataTableOutput("fbdt_dtr"),
      DT::dataTableOutput("fbdt_tempdtr")#,

      # A static infoBox
      #infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
      # Dynamic infoBoxes
      #infoBoxOutput("progressBox"),
      #infoBoxOutput("approvalBox")
    )#,

    # infoBoxes with fill=TRUE
    # fluidRow(
    #   infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
    #   infoBoxOutput("progressBox2"),
    #   infoBoxOutput("approvalBox2")
    # ),
    #
    # fluidRow(
    #   # Clicking this will increment the progress amount
    #   box(width = 4, actionButton("count", "Increment progress"))
    # )
  )
)

server <- function(input, output) {


  fbdtr <- reactive({

    req(input$dtr_fileInput)

    dtr_xlsx <- input$dtr_fileInput
    file.rename(from = dtr_xlsx$datapath, to = paste(dtr_xlsx$datapath, ".xlsx", sep = ""))
    fb_xlsx <- try(fieldbook::getData(dir = paste(dtr_xlsx$datapath, ".xlsx", sep = ""),
                                      sheet = 1))
    fb_xlsx

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



  # output$progressBox <- renderInfoBox({
  #   infoBox(
  #     "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  # output$approvalBox <- renderInfoBox({
  #   infoBox(
  #     "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
  #     color = "yellow"
  #   )
  # })

  # Same as above, but with fill=TRUE
  # output$progressBox2 <- renderInfoBox({
  #   infoBox(
  #     "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
  #     color = "purple", fill = TRUE
  #   )
  # })
  # output$approvalBox2 <- renderInfoBox({
  #   infoBox(
  #     "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
  #     color = "yellow", fill = TRUE
  #   )
  # })
}

shinyApp(ui, server)
