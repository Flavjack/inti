# wanuy -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

shinyUI(dashboardPage(skin = "green",


# Head --------------------------------------------------------------------

    dashboardHeader(title = "WANUY"),



# Sider -------------------------------------------------------------------

    dashboardSidebar(

      sidebarMenu(
        menuItem("presentacion", tabName = "intro", icon = icon("home")),
        menuItem("unidades", tabName = "und", icon = icon("cog")),
        menuItem("cultivo", tabName = "req", icon = icon("leaf")),
        menuItem("fertilizantes", tabName = "fert", icon = icon("tint")),
        menuItem("resumen", tabName = "res", icon = icon("book"))
      )


    ),



# Body --------------------------------------------------------------------


    dashboardBody(


      tabItems(

        # presentacion

        tabItem(tabName = "intro",


                box(
                  title = "Presentacion",
                  width = 4,
                  status = "primary",
                  solidHeader = T,

                  p( strong(em("wanuy")),"es una aplicacion web gratuita para el calculo y manejo de fertilizacion, de manera facil y precisa, garantizando cubrir los
                  requerimientos de tus cultivos. Cuenta tambien con una base de datos de los fertilizantes mas usado y realiza las recomendaciones para
                     tu cultivo de acuerdo a la informacion ingresada"),


                  img(src = "agrinka.jpg",  width = "100%")

                ),


                box(
                  title = "Characteristics",
                  width = 4,
                  status = "danger",
                  solidHeader = T,

                  p("- calculo de los costos por aplicacion de fertilizante"),

                  p("- posibilidad de adicionar tu propio fertilizante"),

                  p("- posiblidad de adicionar tu analisis de suelo")


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

                  p(
                    strong("Omar Benites Alfaro"),
                    br(),
                    a("< obacc07@gmail.com >"),
                    br(),
                    code("Centro Internacional de la Papa (CIP)")
                  ),

                  hr(),

                  p(strong("If you have any question, commment or sugestion you can write a email for us, enjoy WANUY!!"))

                  )

        ),


        # unidades

        tabItem(tabName = "und",


               box(title = "moneda", footer = h3("1 $."),
                   status = "primary",
                   solidHeader = T,
                   width = 4,


                   column(width = 6,

                          numericInput("moneda1", label = h4(""), value = 3, min = 0)

                          ),

                   column(width = 6,

                          textInput("moneda2", label = h4(""), value = "S/.")

                          )

                   ),


               box(title = "area", footer = h4("1 ha"),
                   status = "primary",
                   solidHeader = T,
                   width = 4,

                   column(width = 6,

                          numericInput("area1", label = h4(""), value = 1, min = 0)

                   ),

                   column(width = 6,

                          textInput("area2", label = h4(""), value = "ha")

                   )





               ),


               box(title = "peso", footer = h4("1 kg"),
                   status = "primary",
                   solidHeader = T,
                   width = 4,


                   column(width = 6,

                          numericInput("peso1", label = h4(""), value = 1, min = 0)

                   ),

                   column(width = 6,

                          textInput("peso2", label = h4(""), value = "kg")

                   )



               )



        ),


        # cultivo

        tabItem(tabName = "req",


                box(title = "Planta",
                    status = "success",
                    solidHeader = T,
                    width = 4,


                box(title = "macronutrientes",
                    status = "danger",
                    solidHeader = T,
                    width = 12,


                    column(width = 12,

                           numericInput("nitro", label = h4("nitrogeno"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("fosf", label = h4("fosforo"), min = 0, value = ""  )

                    ),

                    column(width = 12,

                           numericInput("pota", label = h4("potasio"), min = 0, value = "")

                    )

                ),


                box(title = "micronutientes",
                    status = "info",
                    solidHeader = T,
                    width = 12,
                    collapsible = T, collapsed = T,


                    column(width = 12,

                           numericInput("cal", label = h4("calcio"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("mg", label = h4("magnesio"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("fe", label = h4("fierro"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("cu", label = h4("cobre"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("sf", label = h4("azufre"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("br", label = h4("boro"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("mn", label = h4("manganeso"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("zn", label = h4("zin"), min = 0, value = "")

                    )




                )


                ),


            box(title = "Analisis de Suelo",
                status = "warning",
                solidHeader = T,
                width = 4,
                collapsible = T,
                collapsed = T,


             box(title = "macronutrientes",
                    status = "danger",
                    solidHeader = T,
                    width = 12,


                    column(width = 12,

                           numericInput("nitro", label = h4("nitrogeno"), min = 0, value = "")

                    ),

                    column(width = 12,

                           numericInput("fosf", label = h4("fosforo"), min = 0, value = ""  )

                    ),

                    column(width = 12,

                           numericInput("pota", label = h4("potasio"), min = 0, value = "")

                    )

                )



                ),


            box(title = "unidades",
                status = "info",
                solidHeader = T,
                width = 4,
                collapsible = T,
                collapsed = T,


                    box(title = "moneda (1 $)",
                        status = "primary",
                        solidHeader = T,
                        width = 12,


                        column(width = 6,

                               numericInput("moneda1", label = h4(""), value = 3, min = 0)

                        ),

                        column(width = 6,

                               textInput("moneda2", label = h4(""), value = "S/.")

                        )

                    ),


                    box(title = "area (1 ha)",
                        status = "primary",
                        solidHeader = T,
                        width = 12,

                        column(width = 6,

                               numericInput("area1", label = h4(""), value = 1, min = 0)

                        ),

                        column(width = 6,

                               textInput("area2", label = h4(""), value = "ha")

                        )





                    ),


                    box(title = "peso (1 kg)",
                        status = "primary",
                        solidHeader = T,
                        width = 12,


                        column(width = 6,

                               numericInput("peso1", label = h4(""), value = 1, min = 0)

                        ),

                        column(width = 6,

                               textInput("peso2", label = h4(""), value = "kg")

                        )



                    )


                )




        ),


        # fertilizantes

        tabItem(tabName = "fert",


        box(

          title = "fertilizantes selecionados",
          status = "danger",
          solidHeader = T,
          width = 12,

          verbatimTextOutput('sfrt')

        ),

        box(

          title = "lista de fertilizantes",
          status = "primary",
          solidHeader = T,
          width = 12,

        DT::dataTableOutput('frt')


        )


        ),

        # resumen

        tabItem(tabName = "res",

                h2("resultados, comentarios, sugerencias")

        )



      )



    )

  )
)



