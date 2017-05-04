#' Conditional Panels for statistical design
#' @author quipo.org
#' @description Differents types of statistical designs are related to customized inputs.
#' @importFrom shiny selectInput conditionalPanel textInput br icon
#' @importFrom shinydashboard infoBox
#' @export


design_conditional_panels <- function(){


  list(



  shiny::fluidRow(




  box(title = "FieldBook Design",
      status = "info",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,




    column(width = 12,

        radioButtons("tool_layout", label = h4("Select Importation", style = "font-family: 'Arial', cursive;
                                              font-weight: 1000; line-height: 1.1"),
                   choices = c("Standard", "Special"),inline = TRUE, selected = "Standard"),

        shiny::selectInput("tool_dsg", "Design", design_choices, selected = "crd", multiple = FALSE)#,
      #
    ),

    conditionalPanel(

      condition = "input.tool_layout == 'Standard'",


      shiny::conditionalPanel(
        "input.tool_dsg == 'rcbd' |
        input.tool_dsg == 'crd'   |
        input.tool_dsg == 'f2crd' |
        input.tool_dsg == 'f2rcbd'|
        input.tool_dsg == 'lsd'",


        column(width = 8,
               textInput("tool_f1", label = "Factor levels", value = "")
        ),

        column(width = 4,
               textInput("tool_lb1", label = "Header", value = "Label")
        )#,

      ),

    shiny::conditionalPanel(
      "input.tool_dsg == 'f2crd'  |
       input.tool_dsg == 'f2rcbd'",

        column(width = 8,
               textInput("tool_f2", label = "Factor levels", value = "")
        ),

        column(width = 4,
               textInput("tool_lb2", label = "Header", value = "Label")
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
                      textInput("tool_var", label = "Variables", value = "")
               ),

        fluidRow(
               column(width = 2,
                      numericInput("tool_rep",label = "Repetitions", value = 3, min = 2)
               ),

               column(width = 2,
                      numericInput("tool_eva",label = "Intime", value = 1, min = 1)
               )#,
            )
        #)#,

      )

    )#,


#  )

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

)


}
