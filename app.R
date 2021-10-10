library(shiny)
library(shinythemes)

donkeys <- readRDS("d_train.rds")
model <- step(lm(Weight~(BCS+ Age+ Sex+ Length+ Girth+ Height)^2, data = donkeys))

ui <- fluidPage(
    theme = shinytheme("flatly"),
    navbarPage("Dosing Donkeys"),
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red;
                      font-size: 20px;
                      }
                      "))),
    sidebarLayout(
        sidebarPanel(
            numericInput("length",
                         label = "Enter the LENGTH of the donkey (centimeters):",
                         value = 100,
                         min = 0, max = 200),
            numericInput("girth",
                         label = "Enter the GIRTH of the donkey (centimeters):",
                         value = 100,
                         min = 0, max = 200),
            numericInput("height",
                         label = "Enter the HEIGHT of the donkey (centimeters):",
                         value = 100,
                         min = 0, max = 200),
            selectInput("bcs",
                        label = "Select the Body Condition Score (BCS) of the donkey:",
                        choices = c("1.5", "2", "2.5", "3", "3.5", "4")),
            selectInput("age",
                        label = "Select the age of the donkey:",
                        choices = c("<2", "2-5", ">5")),
            actionButton("goButton", "Go!", style = "font-weight: bold; color = #FFFFFF; background-color: #F0312E ", width = 100)
        ),

        mainPanel(
            h4("Enter the length, girth, height, Body Condition Score (BCS) and age of the donkey to get an estimate of its weight."),
            h2("The estimated weight of the donkey is:"),
            textOutput("warning"),
            textOutput("weight"),
            tags$style("#weight{color: #1A5276;
                                 font-size: 80px;
                                 font-style: bold;
                                 text-align: center;
                                 }"),
            br(),
            dataTableOutput("confidence"),
            br(),
            h4("*If you are afraid of overdosing your donkey with the weight estimation, refer to the 95% confidence interval above and use the weight of the lower bound (lwr). ", 
               style = "color:#14AECC; font-weight: bold;"),
            br(),
           img(src = "smiling-donkey.png", style = "border-radius: 80%; display: block; margin-left: auto; margin-right: auto; width: 70%;")
        )
    )
)


server <- function(input, output) {
    lengthRe <- eventReactive(input$goButton,
                              {input$length})
    girthRe <- eventReactive(input$goButton,
                             {input$girth})
    heightRe <- eventReactive(input$goButton,
                             {input$height})
    bcsRe <- eventReactive(input$goButton,
                             {switch(input$bcs,
                                     "1.5" = -11.64172,
                                     "2" = -7.23305,
                                     "2.5" = -5.16656,
                                     "3" = +0,
                                     "3.5" = +6.27816,
                                     "4" = +13.08229)})
    ageRe <- eventReactive(input$goButton,
                             {switch(input$age,
                                     "<2" = -9.55086,
                                     "2-5" = -5.508682,
                                     ">5" = +0)})
    ageRe2 <- eventReactive(input$goButton,
                            {input$age})
    bcsRe2 <- eventReactive(input$goButton,
                            {input$bcs})
    
    output$warning <- renderText({
      validate(
        need(lengthRe() > 60 & lengthRe() < 120, "Warning: the length of the donkey is outside of the normal range. Please check for errors in measurement!"),
        need(girthRe() > 85 & girthRe() < 140, "Warning: the girth of the donkey is outside of the normal range. Please check for errors in measurement!"),
        need(heightRe() > 80 & heightRe() < 120, "Warning: the height of the donkey is outside of the normal range. Please check for errors in measurement!")
      )
    })
    
    output$weight <- renderText({
        paste(
        round(46.70719 + 1.791154*girthRe() - 1.96295*lengthRe() - 1.84277*heightRe() + 0.02831*lengthRe()*heightRe() + ageRe() + bcsRe(), digits = 3),
        " kg"
        )
    })
    
    output$confidence <- renderDataTable({
      user_input <- data.frame(
        BCS = bcsRe2(),
        Age = ageRe2(),
        Length = lengthRe(),
        Girth = girthRe(),
        Height = heightRe()
      )
      ci <- predict(model, user_input, interval = "confidence")
    })

}


shinyApp(ui = ui, server = server)
