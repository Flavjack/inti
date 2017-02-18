# fieldbook -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(sapiens)
library(agricolae)
library(dplyr)
library(tibble)
library(DT)
library(ggplot2)


shinyUI(dashboardPage(skin = "green",


    dashboardHeader(title = "FIELDBOOK"),

# Sider -------------------------------------------------------------------

    dashboardSidebar(

      sidebarMenu(
        menuItem("Presentacion", tabName = "intro", icon = icon("home")),
        menuItem("Tools", tabName = "tools", icon = icon("wrench")),
        menuItem("Fieldbook", tabName = "fieldbook", icon = icon("leaf")),
        menuItem("Outliers", tabName = "outlier", icon = icon("search")),
        menuItem("Multivariate", tabName = "multv", icon = icon("paperclip")),
        menuItem("Regression", tabName = "regression", icon = icon("random")),
        menuItem("Statistics", tabName = "stat", icon = icon("pie-chart")),
        menuItem("Graphics", tabName = "graph", icon = icon("tint"))#,
        # menuItem("Report", tabName = "report", icon = icon("book")),
        # menuItem("About", tabName = "info", icon = icon("book"))
      )


    ),


# Iconos :: http://getbootstrap.com/components/#glyphicons-glyphs


    dashboardBody(


      tabItems(


# presentacion ------------------------------------------------------------


        tabItem(tabName = "intro",


                box(
                  title = "Presentacion",
                  width = 6,
                  status = "primary",
                  solidHeader = T,

                p( strong(em("FieldBook")),"is a interactive application for exploratory data analisys and graphics for experimnetal designs"),

                  img(src = "agrinka.jpg",  width = "100%")

                ),


                box(
                  title = "Characteristics",
                  width = 6,
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
                  width = 6,
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


                  hr(),

                  p(strong("If you have any question, commment or sugestion you can write a email for us, enjoy FIELDBOOK!!"))



                )



        ),



# fieldbook -------------------------------------------------------------


        tabItem(tabName = "fieldbook",


        box(

          status = "info",
          width = 12,
          background = "black",


          column(width = 6,

           h4(icon("book"), "Google SpreadSheet (URL)", width = "100%"),

           textInput("fbdt",
             label = NULL ,
             width = "100%",
             value = "https://docs.google.com/spreadsheets/d/14sO81N50Zx1al5O3Iu3IPaz1_5CVncvtsx-_JRqJ_qE/edit#gid=172957346")


          ),


          column(width = 4,

            h4(icon("book"), "Excel file (.xlsx)", width = "100%"),

            fileInput('impdata',
              label = NULL,
              accept = c(".xlsx"))

          ),

          column(width = 1,

            h4("Sheet", width = "100%"),

            numericInput("sheetdt", label = NULL, value = 1, step = 1, min = 1)

          ),

          column(width = 1,

            h4( "Update", width = "100%"),

            actionButton(inputId = "reload", label = "", icon("refresh"), width = "100%")

          )


        ),


        box(

          status = "danger",
          solidHeader = T,
          width = 10,

        # DT::dataTableOutput('fbook')
        htmlOutput("fbook")


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


        ),


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

                      textInput(inputId ="bply", label = "Y label", value = "")


                    ),


                    column(width = 4,


                      textInput(inputId ="bplx", label = "X label", value = "")


                    ),


                    column(width = 4,

                      textInput(
                        inputId ="bplz",
                        label = "Legend label",
                        value = "")


                    )

          ),


          box(width = 2, background = "black",


            column(width = 12,

              numericInput(
                inputId ="bpbrk",
                label = "Axis brake",
                value = NA)

            ),


            column(width = 12,

              numericInput(
                inputId ="bpsize",
                label = "Size",
                value = 2,
                min = 0,
                step = 0.1)


            )


            ),


          box(width = 12,

          plotOutput("boxplot")


          )



        ),


# multivariate ------------------------------------------------------------

        tabItem(tabName = "multv",

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

        ),


# statistics -------------------------------------------------------------

        tabItem(tabName = "stat",


          box(width = 5, background = "black",

            column(width = 6,

              uiOutput("stat_response")

            ),


            column(width = 6,

              uiOutput("stat_block")


            ),

            column(width = 12,

              uiOutput("stat_factor")

            ),



            column(width = 6,

              numericInput("stsig",
                label = "Significance",
                value = 0.05,
                min = 0,
                max = 5,
                step = 0.01)


            ),

            column(width = 6,

              selectInput("stmc",
                label = "Type",
                choices = c("tukey", "duncan", "snk"),
                selected = "snk")


            ),


              column(width = 12,

                verbatimTextOutput("tbav")

              )




          ),



          box(width = 7,


            DT::dataTableOutput("mnc")


          )


        ),

# graphics ----------------------------------------------------------------


        tabItem(tabName = "graph",

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



        ),


# tools -------------------------------------------------------------------

        tabItem(tabName = "tools",


                  box(title = "My FieldBook", status = "info", width = 6,

                    column(width = 8,

                      textInput("tool_f1", label = "Treatment 1", value = "")

                    ),

                    column(width = 4,

                      textInput("tool_lb1", label = "Label", value = "treat1")

                    ),


                    column(width = 8,

                      textInput("tool_f2", label = "Treatment 2", value = "")

                    ),

                    column(width = 4,

                      textInput("tool_lb2", label = "Label", value = "treat2")

                    ),

                    column(width = 12,

                      textInput("tool_var", label = "Variables", value = "")

                    ),

                    column(width = 2,

                      numericInput("tool_rep",label = "Repetitions", value = 3, min = 2)

                    ),


                    column(width = 2,

                      numericInput("tool_eva",label = "Intime", value = 1, min = 1)

                    ),

                    column(width = 5,

                      radioButtons("tool_dsg", label = "Design",
                        choices = c("crd", "rcbd", "lsd"), selected = "crd", inline = TRUE)


                    )


                  ),


                  box(width = 6,


                    DT::dataTableOutput("fbdsg")


                  )



          ),


# Lineal Regression -------------------------------------------------------

        tabItem(tabName = "regression",

          box( width = 10,


            box(width = 4, title = NULL, background = "blue",


              column(width = 12,

                uiOutput("lrg_variable2")


              ),

              column(width = 8,

                textInput("lr_lbv2", label = "Label", value = "")


              ),


              column(width = 4,

                numericInput("lr_brk2", label = "Brakes", value = NA, min = 0)


              )


            ),



            box(width = 4, title = NULL, background = "green",




              column(width = 12,

                uiOutput("lrg_variable1")


              ),

              column(width = 8,

                textInput("lr_lbv1", label = "Label", value = "")


              ),

              column(width = 4,

                numericInput("lr_brk1", label = "Brakes", value = NA, min = 0)

              )


            ),


            box(width = 4, background = "red",

              column(width = 12,

                uiOutput("lrg_grouped")


              ),


              column(width = 6,

                textInput("lr_lbgp", label = "Legend", value = "")


              ),


              column(width = 6,

                textInput("lr_lglv", label = "Levels", value = "")


              )


            ),


            box(width = 12,


              plotOutput("plot_regression")



            )


          ),

          box(width = 2,

            column(width = 12,


              numericInput(
                inputId ="lr_font",
                label = "Size",
                value = 2,
                min = 0,
                step = 0.1
              )

            ),


            column(width = 12,


              radioButtons(
                inputId ="lr_color",
                label = "Color",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE)
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





          )



        ),


# information -------------------------------------------------------------


        tabItem(tabName = "info"






        )


      )



    )

  )
)



