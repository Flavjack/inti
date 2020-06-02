# elisios -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(tibble)
library(DT)
library(leaps)
library(scatterplot3d)
library(flashClust)
library(lubridate)


shinyUI(dashboardPage(skin = "green",


# Head --------------------------------------------------------------------


    dashboardHeader(title = "ELISIOS"),


# Sider -------------------------------------------------------------------

    dashboardSidebar(

      sidebarMenu(
        menuItem("Presentacion", tabName = "intro", icon = icon("home")),
        menuItem("Crop", tabName = "crop", icon = icon("leaf")),
        menuItem("Soil", tabName = "soil", icon = icon("cog")),
        menuItem("Weather", tabName = "clima", icon = icon("cloud")),
        menuItem("Graphics", tabName = "graph", icon = icon("edit")),
        menuItem("Irrigation", tabName = "riego", icon = icon("tint"))
      )


    ),


# Iconos :: http://getbootstrap.com/components/#glyphicons-glyphs


# Body --------------------------------------------------------------------


    dashboardBody(


      tabItems(


# presentacion ------------------------------------------------------------


        tabItem(tabName = "intro",


                box(
                  title = "Presentacion",
                  width = 4,
                  status = "primary",
                  solidHeader = T,

                p( strong(em("ELISIOS")),"is a interactive application for calculte the irrigation requirements based in the metereological data information"),

                  img(src = "agrinka.jpg",  width = "100%"),

                HTML('<p style="text-align: right;"><span style="font-size:14px;"><span style="font-family:comic sans ms,cursive;">Powered by <span style="font-size:16px;"><span style="font-family:lucida sans unicode,lucida grande,sans-serif;"><strong><span style="color:#ff0000;">Q</span><span style="color:#a52a2a;">u</span><span style="color:#008000;">i</span><span style="color:#008080;">p</span><span style="color:#0000ff;">o</span></strong></span></span></span></span></p>')

                ),


                box(
                  title = "Charateristics",
                  width = 4,
                  status = "danger",
                  solidHeader = T,

                    p("- Calculate the dialy evapotranspiration"),

                    p("- Calculate the crop irrigation schedule"),

                    p("- Optimized for arduino metereological estation"),

                    p("- Remote sensing app"),

                    p("- Free open source software")


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
                    strong("Jimmy R. Gomez Carrion"),
                    br(),
                    a("< purmacana@gmail.com >"),
                    br(),
                    code("Universidad Nacional Agraria la Molina, Lima, Perú")
                  ),

                  # p(
                  #   strong("Omar Benites Alfaro"),
                  #   br(),
                  #   a("< obacc07@gmail.com >"),
                  #   br(),
                  #   code("Centro Internacional de la Papa (CIP)")
                  # ),
                  #

                  p(
                    strong("Kevin Arthur Lara Jauregui"),
                    br(),
                    a("< kevin.lara@pucp.edu.pe >"),
                    br(),
                    code("Pontificia Universidad Católica del Perú")
                  ),


                  hr(),

                  p(strong("If you have any question, commment or sugestion you can write a email for us, enjoy ELISIOS!!"))



                )



        ),


# crop --------------------------------------------------------------------


        tabItem(tabName = "crop",


               box(title = "Information",
                   status = "success",
                   solidHeader = T,
                   width = 12,


                   column(width = 3,

                          textInput("crop",
                                    label = ("Crop name"),
                                    value = "rabanito")

                   ),



                   column(width = 3,

                          numericInput("area",
                                       label = ("Area (ha)"),
                                       min = 0,
                                       value = 1)

                   ),


                   column(width = 3,

                          dateInput("pdate",
                                    label = ("Planting date"),
                                    format = "yyyy-mm-dd",
                                    value =  "2016-07-05")


                   ),


                   column(width = 3,

                          numericInput("height",
                                       label = ("Crop height maximun (m)"),
                                       min = 0, step = 10,
                                       value = 0.2)
                   )



                   ),


               box(title = "Stages (days)",
                   status = "warning" ,
                   solidHeader = T,
                   width = 3,


                   column(width = 12,

                          numericInput("st1",
                                       label = ("Initial"),
                                       min = 0, step = 5,
                                       value = "5")

                   ),


                   column(width = 12,

                          numericInput("st2",
                                       label = ("Develop"),
                                       min = 0, step = 5,
                                       value = "10")

                   ),

                   column(width = 12,

                          numericInput("st3",
                                       label = ("Mid"),
                                       min = 0,  step = 5,
                                       value = "15")

                   ),

                   column(width = 12,

                          numericInput("st4",
                                       label = ("End"),
                                       min = 0, step = 5,
                                       value = "5")

                   )


               ),


               box(title = "Kc values (fraction)",
                   status = "primary",
                   solidHeader = T,
                   width = 3,

                   column(width = 12,

                          numericInput("kc1",
                                       label = ("Initial"),
                                       min = 0, max = 2, step = 0.2,
                                       value = "0.3")

                   ),


                   column(width = 12,

                          numericInput("kc2",
                                       label = ("Develop"),
                                       min = 0, max = 2, step = 0.2,
                                       value = "0.5")

                   ),

                   column(width = 12,

                          numericInput("kc3",
                                       label = ("Mid"),
                                       min = 0, max = 2, step = 0.2,
                                       value = "0.7")

                   ),

                   column(width = 12,

                          numericInput("kc4",
                                       label = ("End"),
                                       min = 0, max = 2, step = 0.2,
                                       value = "0.4")

                   )





               ),




               box(title = "Critical depletion (%)",
                   status = "danger",
                   solidHeader = T,
                   width = 3,


                   column(width = 12,

                          numericInput("cd1",
                                       label = ("Initial"),
                                       min = 0, max = 100, step = 5,
                                       value = "10")

                   ),


                   column(width = 12,

                          numericInput("cd2",
                                       label = ("Develop"),
                                       min = 0, max = 100, step = 5,
                                       value = "5")

                   ),

                   column(width = 12,

                          numericInput("cd3",
                                       label = ("Mid"),
                                       min = 0, max = 100, step = 5,
                                       value = "10")

                   ),

                   column(width = 12,

                          numericInput("cd4",
                                       label = ("End"),
                                       min = 0, max = 100, step = 5,
                                       value = "30")

                   )


               ),


               box(title = "Irrigation depth (cm)",
                   status = "info",
                   solidHeader = T,
                   width = 3,


                   column(width = 12,

                          numericInput("id1",
                                       label = ("Initial"),
                                       min = 0, step = 5,
                                       value = "10")

                   ),


                   column(width = 12,

                          numericInput("id2",
                                       label = ("Develop"),
                                       min = 0, step = 5,
                                       value = "10")

                   ),

                   column(width = 12,

                          numericInput("id3",
                                       label = ("Mid"),
                                       min = 0, step = 5,
                                       value = "15")

                   ),

                   column(width = 12,

                          numericInput("id4",
                                       label = ("End"),
                                       min = 0, step = 5,
                                       value = "10")

                   )


               )





        ),



# soil --------------------------------------------------------------------


        tabItem(tabName = "soil",


                box(title = "Soil",
                    status = "danger",
                    solidHeader = T,
                    width = 4,


                    column(width = 12,

                           textInput("sname",
                                     label = ("Soil name"),
                                     value = "desierto")

                    ),


                    column(width = 12,

                           textInput("stex",
                                     label = ("Soil texture"),
                                     value = "arena")

                    ),

                    column(width = 12,


                           numericInput("sden",
                                        label = ("Soil density"),
                                        min = 0, max = 5, step = 0.1,
                                        value = "1.75")

                    )


                ),


                box(title = "Profile",
                    status = "warning",
                    solidHeader = T,
                    width = 4,

                    column(width = 12,

                           numericInput("fc",
                                        label = ("Field capacity (%)"),
                                        min = 0, step = 1,
                                        value = "8")

                    ),

                    column(width = 12,

                           numericInput("wp",
                                        label = ("Wild point (%)"),
                                        min = 0, step = 10,
                                        value = "3")

                    ),

                    column(width = 12,

                           numericInput("inf",
                                        label = ("Infiltration (mm/hr)"),
                                        min = 0, step = 10,
                                        value = "25")

                    ),


                    column(width = 12,

                           numericInput("soilm",
                                        label = ("Initial soil moisture (%)"),
                                        min = 0, step = 10,
                                        value = "0")

                    )

                ),


                box(title = "Irrigation",
                    status = "primary",
                    solidHeader = T,
                    width = 4,


                    column(width = 12,

                           numericInput("ire",
                                     label = ("Irrigation efficiency (%)"),
                                     min = 0, max = 100, step = 10,
                                     value = "80")

                    ),


                    column(width = 12,

                           numericInput("irs",
                                        label = ("Irrigation surface (%)"),
                                        min = 0, max = 100, step = 10,
                                        value = "50")

                    )



                )


        ),



# soil --------------------------------------------------------------------


        tabItem(tabName = "clima",


        box(

          status = "info",
          width = 12,
          background = "blue",

          column(width = 3,


                 h4("Google spreadsheet (URL)", icon("book"), width = "100%")


          ),

          column(width = 7,

                 textInput("wtdt", label = NULL, width = "100%",
                           value = "https://docs.google.com/spreadsheets/d/14sO81N50Zx1al5O3Iu3IPaz1_5CVncvtsx-_JRqJ_qE/edit#gid=801368065")
          ),

          column(width = 2,

                 actionButton(inputId = "reload", label = "update", icon("refresh"), width = "100%")

          )


        ),


        box(

          status = "danger",
          solidHeader = T,
          width = 12,


        htmlOutput("gss")


        )


        ),



# Graphics ----------------------------------------------------------------


        tabItem(tabName = "graph",


        box(

          status = "danger",
          solidHeader = T,
          width = 12,

          plotly::plotlyOutput("wtplot", height = "auto")

        )


        ),




# irrigation -----------------------------------------------------------------


        tabItem(tabName = "riego",


                DT::dataTableOutput('crop')


        )



      )



    )

  )
)

