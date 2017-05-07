design_choices <- c(
  "Completely Randomized Design (CRD)" = "crd",
  "Randomized Complete Block Design (RCBD)" = "rcbd",
  "Factorial Two-Way Design in CRD (F2CRD)" = "f2crd",
  "Factorial Two-Way Design in RCBD (F2RCBD)" = "f2rcbd",
  "Latin Square Design (LSD)" = "lsd"
)

#TODO: Agregar import fieldbook
#      Separar por comas los valores ingresados en treatment y variables.
#      Y si tienen espacios en blanco, agregar underscore.

# fieldbook -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(agricolae)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rhandsontable)
library(ggpubr)
library(fieldbook)
library(shinyBS)

shinyUI(dashboardPage(skin = "green",


    dashboardHeader(title = "FIELDBOOK"),

# Sider -------------------------------------------------------------------

    dashboardSidebar(

      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About", tabName = "about", icon = icon("home")),
        menuItem("User Manual", tabName = "usm", icon = icon("book")),
        menuItem("FieldBook", tabName = "fieldbook", icon = icon("wrench")),
        menuItem("Import data", tabName = "import", icon = icon("leaf")),
        menuItem("Outliers", tabName = "outlier", icon = icon("search")),
        menuItem("Multivariate", tabName = "multv", icon = icon("paperclip")),
        menuItem("Regression", tabName = "regression", icon = icon("random")),
        menuItem("Statistics", tabName = "stat", icon = icon("pie-chart")),
        menuItem("Graphics", tabName = "graph", icon = icon("tint"))#,
        # menuItem("Report", tabName = "report", icon = icon("book"))
      )


    ),


# Iconos :: http://getbootstrap.com/components/#glyphicons-glyphs


    dashboardBody(

      tabItems(

        tabItem(tabName = "usm",
                htmlOutput("usm")
        ),

# dashboard

######### dashboard #####
tabItem(tabName = "dashboard",

        #br(h2("Highly Interactive Data Analysis Platform")),
        #br( p(class = "text-muted", style="text-align:right", "Highly Interactive Data Analysis Platform")),

        # br(),
        #img(src="potato.jpg", width = "100%"),
        img(src="portada_fieldbook3.png", width = "100%"),

        # h3("HIDAP v.1.0"),
        # p(class = "text-muted", style="text-align:justify",
        #   #paste("HiDAP is a Highly Interactive Data Analysis Platform originally meant to support clonal crop breeders at the <a href='http://www.cipotato.org' target='_new'>International Potato Center</a>. It is part of a continuous institutional effort to improve data collection, data quality, data analysis and open access publication. The recent iteration simultaneously also represents efforts to unify best practices from experiences in breeding data management of over 10 years, specifically with DataCollector and CloneSelector for potato and sweetpotato breeding, to address new demands for open access publishing and continue to improve integration with both corporate and community databases (such as biomart and sweetpotatobase) and platforms such as the <a href='https://research.cip.cgiar.org/gtdms/' target='_new'> Global Trial Data Management System (GTDMS)</a> at CIP. </br> One of the main new characteristics of the current software development platform established over the last two years is the web-based interface which provides also a highly interactive environment. It could be used both online and offline and on desktop as well as tablets and laptops. Key features include support for data capture, creation of field books, upload field books from and to accudatalogger, data access from breeding databases (e.g., <a href = 'http://germplasmdb.cip.cgiar.org/' target='_new'>CIP BioMart</a>, <a href='http://www.sweetpotatobase.org' target='_new'>sweetpotatobase</a> via <a href='http://docs.brapi.apiary.io/' target='_new'>breeding API</a>), data quality checks, single and multi-environmental data analysis, selection indices, and report generations. For users of DataCollector or CloneSelector many of the features are known but have been improved upon. Novel features include list management of breeding families, connection with the institutional pedigree database, interactive and linked graphs as well as reproducible reports. With the first full release by end of November 2016 we will include all characteristics from both DataCollector and CloneSelector. HIDAP, with additional support from <a href='https://sweetpotatogenomics.cals.ncsu.edu/' target='_new'>GT4SP</a>, <a href='http://www.rtb.cgiar.org/' target='_new'>RTB</a>, USAID, and <a href='http://cipotato.org/research/partnerships-and-special-projects/sasha-program/' target='_new'>SASHA</a>, is aimed to support the broader research community working on all aspects with primary focus on breeding, genetics, biotechnology, physiology and agronomy.")
        #   shiny::includeHTML("www/about_hidap.txt")
        # ),
        #
        # br(),
        # br(),
        #
        # fluidRow(
        #   box(
        #     width = 2, style="background-color = #fff", height = "128px",
        #     solidHeader = TRUE,
        #     br(),
        #     div(img(src="CIPlogo_RGB.png", width = "150px"), style="text-align: center;")
        #   ),
        #   box(
        #     width = 2, style="background-color = #fff", height = "128px",
        #     solidHeader = TRUE,
        #     div(img(src="gt4sp.png", height = "108px"), style="text-align: center;")
        #   ),
        #   box(
        #     width = 2, style="background-color = #fff", height = "128px",
        #     solidHeader = TRUE,
        #     br(),
        #     div(img(src="usaid.png", width = "150px"), style="text-align: center;")
        #   ),
        #   box(
        #     width = 2, style="background-color = #fff", height = "128px",
        #     solidHeader = TRUE,
        #     div(img(src="sasha.png"), style="text-align: center;")
        #   ),
        #   box(
        #     width = 2, style="background-color = #fff", height = "128px",
        #     solidHeader = TRUE,
        #     br(),
        #     div(img(src="rtb.png", width = "150px"), style="text-align: center;")
        #   )
        # ),
        br()
),


# presentacion ------------------------------------------------------------

        tabItem(tabName = "about",


                box(
                  title = "Presentacion",
                  width = 4,
                  status = "primary",
                  solidHeader = T,

                p( strong(em("Fieldbook")),"is a interactive application for exploratory data analisys and graphics for experimnetal designs"),

                  img(src = "agrinka.jpg",  width = "100%"),


                HTML('<p style="text-align: right;"><span style="font-size:14px;"><span style="font-family:comic sans ms,cursive;">Powered by <span style="font-size:16px;"><span style="font-family:lucida sans unicode,lucida grande,sans-serif;"><strong><span style="color:#ff0000;">Q</span><span style="color:#a52a2a;">u</span><span style="color:#008000;">i</span><span style="color:#008080;">p</span><span style="color:#0000ff;">o</span></strong></span></span></span></span></p>')


                ),


                box(
                  title = "Characteristics",
                  width = 4,
                  status = "danger",
                  solidHeader = T,

                  p("- Import data from excel files and google spreadsheet documents"),

                  p("- Detection of outliers"),

                  p("- Statistical analisys for experimental designs"),

                  p("- Colored and gray scale graphics for publication"),

                  p("- Multivariate analisys: PCA and correlation"),

                  p("- Field book design generator")


                ),

                box(
                  title = "Contributors",
                  width = 4,
                  status = "success",
                  solidHeader = T,

                  p(
                    strong("Flavio Lozano Isla "),
                    br(),
                    a("< flavjack@gmail.com >"),
                    br(),
                    code("Universidad Nacional Agraria la Molina, Lima, Perú")
                    ),

                  p(
                    strong("Omar Benites Alfaro"),
                    br(),
                    a("< obacc07@gmail.com >"),
                    br(),
                    code("Centro Internacional de la Papa (CIP)")
                  ),

                  p(
                    strong("Jimmy R. Gomez Carrion"),
                    br(),
                    a("< purmacana@gmail.com >"),
                    br(),
                    code("Universidad Nacional Agraria la Molina, Lima, Perú")
                  ),


                  p(
                    strong("Felipe de Mendiburu"),
                    br(),
                    a("<  fmendiburu@lamolina.edu.pe >"),
                    br(),
                    code("Universidad Nacional Agraria la Molina, Lima, Perú")
                  ),
                  hr(),

                  p(strong("If you have any question, comment or sugestion you can write a email for us, enjoy FIELDBOOK!!"))

                )

        ),


# fieldbook -------------------------------------------------------------------

tabItem(tabName = "fieldbook",


        shiny::fluidRow(




          box(title = "FieldBook Design",
              status = "info",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,


           #   list(


                column(width = 12,

                       radioButtons("tool_layout", label = h4("Type of layout", style = "font-family: 'Arial', cursive;
                                                              font-weight: 1000; line-height: 1.1"),
                                    choices = c("Standard", "Special"),inline = TRUE, selected = "Standard"),

                       #bsTooltip("tool_layout", "Press Standard to upload file from your computer. Press Google for connecting to Google SpreedShet", options = list(container = "body")),

                       shiny::selectInput("tool_dsg", "Design", design_choices, selected = "crd", multiple = FALSE)#,




                       ),

                conditionalPanel(

                  condition = "input.tool_layout == 'Standard'",


                  shiny::conditionalPanel(
                    "input.tool_dsg == 'rcbd'  |
                    input.tool_dsg ==  'crd'   |
                    input.tool_dsg ==  'f2crd' |
                    input.tool_dsg ==  'f2rcbd'|
                    input.tool_dsg ==  'lsd'",


                    column(width = 8,
                           textInput("tool_f1", label = "Factor levels (separated by commas (,))", value = ""),
                           bsTooltip("tool_f1", "Enter your factors separated by commas [,]. Example: d0mg, d50mg, d100mg. Whitespaces are filled with underscore (_)", options = list(container = "body"))

                    ),

                    column(width = 4,
                           textInput("tool_lb1", label = "Header", value = "Label"),
                           bsTooltip("tool_lb1", "Enter the header of your factor. Example: Fertilizer. Whitespaces are filled with underscore (_)", options = list(container = "body"))

                    )#,

                  ),

                  shiny::conditionalPanel(
                    "input.tool_dsg == 'f2crd'  |
                    input.tool_dsg == 'f2rcbd'",

                    column(width = 8,
                           textInput("tool_f2", label = "Factor levels (separated by commas (,))", value = ""),
                           bsTooltip("tool_f2", "Enter your factor levels separated by commas (,). Example: normal, deficit. Whitespaces are filled with underscore (_)", options = list(container = "body"))

                    ),

                    column(width = 4,
                           textInput("tool_lb2", label = "Header", value = "Label"),
                           bsTooltip("tool_lb2", "Enter the header of your Factor. Example: Irrigation. Whitespaces are filled with underscore (_)", options = list(container = "body"))
                    ) #,

                  ), #end conditional panel

                  shiny::conditionalPanel(
                    "input.tool_dsg == 'rcbd'  |
                    input.tool_dsg  == 'crd'   |
                    input.tool_dsg  == 'f2crd' |
                    input.tool_dsg  == 'f2rcbd'|
                    input.tool_dsg  == 'lsd'",

                    #column(width = 12,

                    column(width = 8,
                           textInput("tool_var", label = "Variables (separated by commas (,))", value = ""),
                           bsTooltip("tool_var", "Enter your variables separated by commas (,): height, weight, leafArea ", options = list(container = "body"))

                    ),

                    fluidRow(
                      column(width = 2,
                             numericInput("tool_rep",label = "Replications", value = 3, min = 2),
                             bsTooltip("tool_rep", "Enter the number of replications.", options = list(container = "body"))
                      ),

                      column(width = 2,
                             numericInput("tool_eva",label = "Intime", value = 1, min = 1),
                             bsTooltip("tool_eva", "Enter the evaluation in time", options = list(container = "body"))
                      )#,
                    )
                    #)#,

                  )

                )#,



              #)

        ) #end tabBox
), #end fluidRow



shiny::fluidRow(#Begin fluidRow

  box(title = "Fieldbook Preview",
      status = "primary",
      height = 900,
      #width = NULL,
      solidHeader = TRUE,
      width = 12, collapsible = TRUE,

      #width = 6,

      DT::dataTableOutput("fbdsg")

  )
)





),


# import data -------------------------------------------------------------



tabItem(tabName = "import",



# Begin fluidRow: fieldbook-select Importation -------
#TODO: Fix the importation of data in case of selecting 'local' option.

shiny::fluidRow(

        box( #begin box (fieldbook tab)
            status = "success",
            width = 12,
            #collapsible = TRUE,
            #solidHeader = TRUE,
            background = "black",

            fluidRow(

              column(2,


            radioButtons("fb_import", label = h4("Select Importation", style = "font-family: 'Arial', cursive;
                                                 font-weight: 1000; line-height: 1.1"),
                         choices = c("Local", "Google"),
                         selected = "Local"),




            br()#,

),

            conditionalPanel(
              condition = "input.fb_import == 'Google'",

            column(width = 10, #offset = 1,

             h4(icon("book"), "Google SpreadSheet (URL)", width = "100%"),
             textInput("fbdt",
               label = NULL,
               width = "100%",
               value = "https://docs.google.com/spreadsheets/d/14sO81N50Zx1al5O3Iu3IPaz1_5CVncvtsx-_JRqJ_qE/edit#gid=172957346")

            )#,

          ),
          conditionalPanel(
            condition = "input.fb_import == 'Local'",

            column(width = 9, #offset = 1,
              h4(icon("book"), "Excel file (.xlsx)", width = "100%"),
              fileInput('impdata',
                label = NULL,
                accept = c(".xlsx"))
            ),

            column(width = 1,
                   h4("Sheet", width = "100%"),
                   numericInput("sheetdt", label = NULL, value = 1, step = 1, min = 1)

            )#,

          ),

        shiny::conditionalPanel(
          "input.fb_import == 'Local' |
               input.fb_import == 'Google'"

            # column(width = 1,
            #   h4( "Update", width = "100%"),
            #   actionButton(inputId = "reload", label = "", icon("refresh"), width = "100%")
            #
            # )
         )

            )#,
        )#, #end box (fieldbook tab)

), #end fluidRow: fieldbook-select Importation


shiny::fluidRow(

  box(title = "",

      status = "success",
      width = 12,
      collapsible = TRUE,
      solidHeader = TRUE,


  conditionalPanel(
    condition = "input.fb_import == 'Google'",

        box(

          status = "danger",
          solidHeader = T,
          width = 10,

        # DT::dataTableOutput('fbook')
        htmlOutput("fbook"),
        br()#,

        )#,
  ),

  conditionalPanel(
    condition = "input.fb_import == 'Local'",
    box(

      status = "danger",
      solidHeader = T,
      width = 10,

      # DT::dataTableOutput('fbook')
      rHandsontableOutput("fbook_excel"),
      br()#,

    )#,

  ),


# filter ------------------------------------------------------------------

  box(

    status = "danger",
    solidHeader = T,
    width = 2,
    title = 'Filter',

    uiOutput("filter_01"),

    uiOutput("filter_fact01"),

    br(),
    uiOutput("filter_02"),

    uiOutput("filter_fact02")

  )
# End filter ---------------------------------------------------------------

) #end Box


) # end of fluidRow: fieldbook,


# filter ------------------------------------------------------------------

        # box(
        #
        #   status = "danger",
        #   solidHeader = T,
        #   width = 2,
        #   title = 'Filter',
        #
        #   uiOutput("filter_01"),
        #
        #   uiOutput("filter_fact01"),
        #
        #   br(),
        #   uiOutput("filter_02"),
        #
        #   uiOutput("filter_fact02")
        #
        # )

  ),
# end TabItem: Fieldbook

# outliers ----------------------------------------------------------------

        tabItem(tabName = "outlier",

          box(width = 10, background = "black",

                    column(width = 4,
                      uiOutput("bpy")
                    ),

                    column(width = 4,
                      uiOutput("bpx")
                    ),

                    column(width = 4,
                      uiOutput("bpz")
                    ),

                    column(width = 4,
                      textInput(inputId ="bply", label = "Y label", value = ""),
                      bsTooltip("bply", "Enter the label of the response variable", options = list(container = "body"))
                    ),



                    column(width = 4,
                      textInput(inputId ="bplx", label = "X label", value = ""),
                      bsTooltip("bplx", "Enter the label of the explanatory variable", options = list(container = "body"))
                    ),



                    column(width = 4,
                      textInput(inputId ="bplz", label = "Legend label", value = ""),
                      bsTooltip("bplz", "Enter the label fo the group variable", options = list(container = "body"))
                    )
          ),


          box(width = 2, background = "black",


            column(width = 12,

              numericInput(inputId ="bpbrk", label = "Axis brake", value = NA),
              bsTooltip("bpbrk", "Enter the number of breaks", options = list(container = "body"))
            ),


            column(width = 12,
              numericInput(inputId ="bpsize", label = "Size", value = 2, min = 0, step = 0.1)
            )

          ),


          shiny::fluidRow(
          box(width = 12, height = NULL,
            plotOutput("boxplot"),
            br()#,
          )#,
          ),

          br(),
          br()#,

        ),


# multivariate ------------------------------------------------------------

        tabItem(tabName = "multv",

               shiny::fluidRow(

         box(width = 6,

           column(width = 3,

             h5(icon("book"), "Correlation", width = "100%")

           ),


           column(width = 2,

            numericInput("corsig",
               label = "Significance",
              value = 0.05,
              min = 0,
              max = 5,
              step = 0.01)

           ),


           column(width = 2,

             numericInput("cor_font",
               label = "Font",
               value = 1,
               min = 0,
               step = 0.1)


           ),

            column(width = 5,

              textInput("corcol",
                label = "Color",
                value = "#DD5143 #F38A78 #68C7EC #00A0DC"
               )

            )


          ),


          box(width = 6,

            column(width = 2,

              h5(icon("book"), "PCA", width = "100%")

            ),


            column(width = 3,

              selectInput("pcatype",
                label = "Type",
                choices = c("ind", "var", "biplot"),
                selected = "biplot")

            ),

            column(width = 2,

              numericInput("pcaqs",
                label = "Variable",
                value = NA,
                min = 1,
                step = 1
              )

            ),

            column(width = 5,

              textInput("pcalbl",
                label = "Label",
                value =  ""
              )

            )
          ),


          box(width = 6,

            plotOutput("crpt", width = "580px", height = "520px")

          ),

          box(width = 6,


            plotOutput("pca", width = "580px", height = "520px")


          )
         )
        ),


# statistics -------------------------------------------------------------

        tabItem(tabName = "stat",

         shiny::fluidRow(
          box(width = 5, background = "black",


            column(width = 12,

                   uiOutput("stat_factor")

            ),



            column(width = 6,

              uiOutput("stat_response")

            ),


            column(width = 6,

              uiOutput("stat_block")


            ),


            column(width = 6,

              numericInput("stsig",
                label = "Significance",
                value = 0.05,
                min = 0,
                max = 5,
                step = 0.01),
              bsTooltip("stsig", "The significance level (alpha)", options = list(container = "body"))

            ),

            column(width = 6,

              selectInput("stmc",
                label = "Type",
                choices = c("tukey", "duncan", "snk"),
                selected = "snk"),
              bsTooltip("stmc", "The type of test. There are three test: Tukey, Ducan and SNK", options = list(container = "body"))


            ),


              column(width = 12,

                verbatimTextOutput("tbav")

              )
          ),

          #shiny::fluidRow(
            box(width = 7,
              DT::dataTableOutput("mnc")
            )
          #)

          )
        ),

# graphics ----------------------------------------------------------------


        tabItem(tabName = "graph",

                shiny::fluidRow(

          box( width = 10,

            box(width = 5, title = NULL, background = "blue",

                      column(width = 12,

                        textInput(
                          inputId ="gply",
                          label = "Y label",
                          value = "")
                      ),

                      column(width = 4,


                        numericInput(
                          inputId ="gbrakes",
                          label = "Brakes",
                          value = NA,
                          min = 0
                        )

                      ),


                      column(width = 4,
                        numericInput(
                          inputId ="glmti",
                          label = "Limit (i)",
                          value = NA
                        )

                      ),


                      column(width = 4,


                        numericInput(
                          inputId ="glmtf",
                          label = "Limit (f)",
                          value = NA
                        )

                      )




              ),



            box(width = 4, title = NULL, background = "green",




                  column(width = 12,

                    textInput(inputId ="gplx", label = "X label", value = "")


                  ),


                  column(width = 12,

                    textInput(inputId ="gp_xbk", label = "Brake Text", value = "")


                  )



            ),


            box(width = 3, background = "red",

                  column(width = 12,

                    textInput(inputId ="gplz", label = "Legend", value = "")


                  ),


                column(width = 12,

                  textInput(inputId ="gp_zbk", label = "Brake Text", value = "")


                )

            ),


          box(width = 12,


                plotOutput("stplot")



          )


      ),

          box(width = 2,

            column(width = 12,


              numericInput(
                inputId ="gfont",
                label = "Size",
                value = 2,
                min = 0,
                step = 0.1
              )

            ),


            column(width = 12,


              radioButtons(
                inputId ="gtype",
                label = "Type",
                choices = c("bar", "line"),
                selected = "bar",
                inline = TRUE)
            ),


            column(width = 12,


              radioButtons(
                inputId ="gcolor",
                label = "Color",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE)
            ),


            column(width = 12,


              radioButtons(
                inputId ="gsig",
                label = "Significance",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE)
            ),

            column(width = 12,


              radioButtons(
                inputId ="gerbr",
                label = "Error",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE)
            ),


            column(width = 12,


              radioButtons(
                inputId ="glabel",
                label = "Legend",
                choices = c("none", "left", "right", "top", "bottom"),
                selected = "top",
                inline = TRUE)
            ),



            column(width = 12,

              numericInput('plot_H', 'Height (mm)',
                value = 75,
                min = 0,
                step = 5)

             ),


            column(width = 12,


              numericInput('plot_W', 'Width (mm)',
                value = 105,
                min = 0,
                step = 5)


            ),


            column(width = 12,

              downloadButton('download_plot', ' TIFF (300 dpi)')

            )





          )

        )

        ),



# Linear Regression -------------------------------------------------------

        tabItem(tabName = "regression",


          box( width = 10,


            box(width = 4, title = NULL, background = "blue",


              column(width = 12,

                uiOutput("lrg_variable2")


              ),

              column(width = 8,

                textInput("lr_lbv2", label = "Label", value = ""),
                bsTooltip("lr_lbv2", "Enter the label of the dependent variable", options = list(container = "body"))

              ),


              column(width = 4,

                numericInput("lr_brk2", label = "Brakes", value = NA, min = 0),
                bsTooltip("lr_brk2", "Enter the number of breaks", options = list(container = "body"))

              )


            ),



            box(width = 4, title = NULL, background = "green",




              column(width = 12,

                uiOutput("lrg_variable1")


              ),

              column(width = 8,

                textInput("lr_lbv1", label = "Label", value = ""),
                bsTooltip("lr_lbv1", "Enter the label of the independent variable", options = list(container = "body"))


              ),

              column(width = 4,

                numericInput("lr_brk1", label = "Brakes", value = NA, min = 0),
                bsTooltip("lr_brk1", "Enter the number of breaks", options = list(container = "body"))

              )


            ),


            box(width = 4, background = "red",

              column(width = 12,

                uiOutput("lrg_grouped")


              ),


              column(width = 6,

                textInput("lr_lbgp", label = "Legend", value = ""),
                bsTooltip("lr_lbgp", "Enter name of group variable", options = list(container = "body"))


              ),


              column(width = 6,

                textInput("lr_lglv", label = "Levels", value = "")


              )


            ),


            box(width = 12,


              plotOutput("plot_regression")



            )


          ),

          shiny::fluidRow(

          box(width = 2, #begin second box

            column(width = 12,


              numericInput(
                inputId ="lr_font",
                label = "Size",
                value = 2,
                min = 0,
                step = 0.1
              ),
              bsTooltip("lr_font", "Enter the font size of the graphic", options = list(container = "body"))


            ),


            column(width = 12,

              radioButtons(
                inputId ="lr_color",
                label = "Color",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE),
              bsTooltip("lr_color", "Select (YES) for colored graphics. (NO) for white and black", options = list(container = "body"))



            ),


            column(width = 12,


              radioButtons(
                inputId ="lr_label",
                label = "Legend",
                choices = c("none", "left", "right", "top", "bottom"),
                selected = "top",
                inline = TRUE)
            ),


            column(width = 12,

               numericInput('lr_eq_x', 'Eq. x',
                            value = NA),

               numericInput('lr_eq_y', 'Eq. y',
                            value = NA)

            ),




            column(width = 12,

              numericInput('lr_plot_H', 'Height (mm)',
                value = 75,
                min = 0,
                step = 5)

            ),


            column(width = 12,


              numericInput('lr_plot_W', 'Width (mm)',
                value = 105,
                min = 0,
                step = 5)


            ),


            column(width = 12,

              downloadButton('download_plot_lr', ' TIFF (300 dpi)')

            )

          ) #end second box
         )

        )


      ),
      br(),
      br(),
      br()

    )
  )
)

















