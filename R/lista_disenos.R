#' Conditional Panels for statistical design
#' @author quipo.org
#' @description Differents types of statistical designs are related to customized inputs.
#' @importFrom shiny selectInput conditionalPanel textInput br icon
#' @importFrom shinydashboard infoBox box
#' @importFrom shiny column radioButtons h4 fluidRow numericInput
#' @importFrom shinyBS bsTooltip addTooltip
#' @export


design_conditional_panels <- function(){

  design_choices <- NULL

  list(

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

)


}




# Requiered packages ------------------------------------------------------


