library(shiny)
library(shinyjs)
library(shinyURL)
library(shinydashboard)

#pass: 123
#credentials <- list("fieldbook" = "202cb962ac59075b964b07152d234b70")

#pass: quipo07
credentials <- list("fieldbook" = "32a04706ce87ebef04a0d871e3874ad8")


shinyServer(function(input, output) {
  shinyURL.server()

  USER <- reactiveValues(Logged = FALSE)

  observeEvent(input$.login, {
    if (isTRUE(credentials[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })

  output$app = renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
                      wellPanel(id = "login",
                                titlePanel("User and Password"),
                                textInput(".username", "Username:"),
                                passwordInput(".password", "Password:"),
                                div(actionButton(".login", "Log in"), style="text-align: center;")
                      ),
                      textOutput("message")
      ))
    } else {
      # Sidebar with a slider input for number of bins
      #sidebarLayout(
        # sidebarPanel(
        #   # sliderInput("bins",
        #   #             "Number of bins:",
        #   #             min = 1,
        #   #             max = 50,
        #   #             value = 30),
        # ),

        # Show a plot of the generated distribution
       # mainPanel(
          #plotOutput("distPlot")
      fluidRow(
        column(12,
          htmlOutput("usm")
        )
)

        #)
    #  )

    }

  )


  output$usm <- renderUI({

    # getPage <- function() {
    #
    #
    #
    #   HTML('<iframe src="http://ec2-54-186-118-40.us-west-2.compute.amazonaws.com:3838/sample-apps/fieldbook" style="border: 0; position:fixed; top:50px; left:0; right:0; bottom:50px; width:100%; height:100%">')
    #
    # }
    shinydashboard::box(title = "Fieldbook", width = 12, height = 12,
    background = "green",
    a("Click in FieldBook Link", href="http://ec2-54-186-118-40.us-west-2.compute.amazonaws.com:3838/sample-apps/fieldbook", target="_blank")

    )
    #getPage()

  })



  # output$distPlot <- renderPlot({
  #
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #
  # })


})
