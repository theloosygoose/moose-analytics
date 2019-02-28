library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

ui <- fluidPage(
  tags$h1("moose analytics"),
  
  textInput(
    inputId = "team_numSearch", label = "Choose a team"),
  
  tabsetPanel(
  tabPanel("Team Numerical Data",
      fluidRow(
        
        column(4,
          
        
          tags$h2("Teams in Database:"),
          verbatimTextOutput("teamNum")
        ),
        column(4,
               
          tags$h2("Match Number Played:"),
          verbatimTextOutput("teamMatches")
        ),
          
        column(4,
    
          tags$h2("Hatches On Rocket:"),
          verbatimTextOutput("teamHatchOnRocket")
        )
      
      )
  ),



 tabPanel( "Charts and Graphs",
      fluidRow(
        column(3,
  
          plotOutput("hatchesLine", width = "400px", height = "400px")
        ),
        column(3,
                 
          plotOutput("teamRocketBallPlot", width = "400px", height = "400px")
        ),
        column(3,
                 
          plotOutput("team_CargoBallPlot", width = "400px", height = "400px")
        ),
        column(3,
                 
          plotOutput("team_linReg", width = "400px", height = "400px")
        )
      
      )

    )
  )
)


server <- function(input, output){

  df <-read.csv(file = "sample.csv", header = TRUE,
                colClasses = c("Team.Number" = "character")
                )

  output$teamNum <- renderPrint({
    df[grep(input$team_numSearch, df$Team.Number),1]
  })

  output$teamMatches <- renderPrint({

    df[grep(input$team_numSearch, df$Team.Number),2]

  })

  output$teamHatchOnRocket <- renderPrint({

    df[grep(input$team_numSearch, df$Team.Number),c(2,4)]
  })
##HISTOGRAM FOR ROCKET BALL
  output$hatchesLine <- renderPlot({

    hatchdata <- df[grep(input$team_numSearch, df$Team.Number),4]

    hist(hatchdata)

  })
##GRAPH FOR ROCKET BALLS X=MATCH NUMBER Y=CARGO ON ROCKET SHIP
  output$teamRocketBallPlot <- renderPlot({
    rocketBallData <- df[grep(input$team_numSearch, df$Team.Number),c(2,6)]
    ggplot(data = rocketBallData, aes(x=Match.Number, y=cargoOnRocket, ymin=0, ymax=10,group = 1)) + geom_line() + geom_point()
  })

##GRAPH FOR CARGO BALLS X=MATCH NUMBER Y=CARGO ON CARGO SHIP
  output$team_CargoBallPlot <- renderPlot({
    cargoBallData <- df[grep(input$team_numSearch, df$Team.Number), c(2,7)]

    ggplot(data = cargoBallData, aes(x=Match.Number, y=cargoOnCargoship, ymin=0, ymax=10)) +geom_line() +geom_point()
  })
  
  output$team_linReg <- renderPlot({
    team_linRegData <- df[grep(input$team_numSearch, df$Team.Number), c(6,7)]
    
    ggplot(data = team_linRegData, aes(x=cargoOnRocket, y=cargoOnCargoship)) + geom_point (shape=1) + geom_smooth(method=lm) 
    
  })
  
 #output$team_linReg <- renderText({
 #   team_linReg <
 # })


}


shinyApp(ui = ui, server = server)
