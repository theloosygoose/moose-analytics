library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

ui <- fluidPage(
  tags$h1("moose analytics"),
  
  textInput(
    inputId ="robot_numSearch", label = "Choose a team"),
  
  tabsetPanel(
    tabPanel("Robot Numerical Data",
      fluidRow(
        tags$h1("Misc."),
        column(4,
          tags$h2("ScoutName"),
          verbatimTextOutput("scoutName")
          
        ),
        column(4,
          tags$h2("Match Number Played:"),
          verbatimTextOutput("teamMatches")
        ),
        column(4,
          tags$h2("Starting Items:"),
          verbatimTextOutput("startingitem_Text")
        ),
        column(4,
          tags$h2("Defence Per 10 sec"),
          verbatimTextOutput("defence_Text")
        ),
        column(4,
          tags$h2("Ending Points:"),
          verbatimTextOutput("endpoint_Text")
        )
      ),
      
      fluidRow(
        tags$h1("Sandstorm:"),
        
        column(4,
          tags$h2("Cargo on CargoShip"),
          verbatimTextOutput("A_cargoShip_Cargo_Text")
        ),
        column(4,
          tags$h2("Hatches on CargoShip"),
          verbatimTextOutput("A_cargoShip_Hatch_Text")
        )
      ),
      
      fluidRow(
        tags$h1("Tele-op:"),
        
        column(5,
          tags$h2("Total Cargo in Rocket:"),
          verbatimTextOutput("rocket_Cargo_Text_total")
        ),
        
        column(5,
          tags$h2("Total Hatch in Rocket:"),
          verbatimTextOutput("rocket_Hatch_Text_total")
        ),
        column(5,
          tags$h2("Total Cargo in Cargoship:"),
          verbatimTextOutput("cargoship_Cargo_Text_total")
        ),
        column(5,
          tags$h2("Total Hatch in Cargoship:"),
          verbatimTextOutput("cargoship_Hatch_Text_total")
        )
      ),
      
      fluidRow(
        tags$h1("Cargo Pickup"),
      
        column(3,
          tags$h3("Depot"),
          verbatimTextOutput("depot_Cargo_Text")
        ),
        column(3,
          tags$h3("Ground"),
          verbatimTextOutput("ground_Cargo_Text")
        ),
        column(3,
          tags$h3("Loading Station"),
          verbatimTextOutput("LS_Cargo_Text")
        )
       ),
      fluidRow(
        tags$h1("Hatch Pickup"),
        
        column(3,
          tags$h3("Ground"),
          verbatimTextOutput("ground_Hatch_Text")
        ),
        
        column(3,
          tags$h3("Loading Station"),
          verbatimTextOutput("LS_Hatch_Text")
        )
      )
      
      
    ),


    tabPanel( "Charts and Graphs",
      # GRAPHS FOR AUTONOMOUS ALL Are 3/12 columns wide and are 400x400px
        tags$h1("Sandstorm"),
        
        fluidRow(
          
          column(3,
            plotOutput("A_cargoShip_Hatch_Line", width = "400px", height = "400px")
          ),
          column(3,
            plotOutput("A_cargoShip_Cargo_Line", width = "400px", height = "400px")
          )
          
        )
    ),
    tabPanel("Frequencies"
    
    
    )
  )
)


server <- function(input, output){

  df <-read.csv(file = "2019_03_02_HatboroEvent.csv", header = TRUE, colClasses = c("Team_Num" = "character"))
  
  rockettotal_Teleop <- df$C_on_3rd_Lev + df$C_on_2nd_Lev + df$C_on_1st_Lev
  
  print(rockettotal_Teleop)
  
  ################################
## MISC. INFORMATION ABOUT ROBOT ##
  ###############################
  
##Server-side for STARTING ITEM :: STARTING ITEM
  output$startingitem_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,31)]
  })
  
##Server-side for DEFENCE PER 10SEC
  output$defence_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,30)]
  })
##Server-side for ENDING POINT
  output$endpoint_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,29)]
    
  })
  
  

#Server-Side for Finding who scouted the robot and at what match number
  output$scoutName <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(1,3)]
    
  })
#Server-Side for Finding starting points and match number
  output$teamMatches <- renderPrint({
    row.names(df[grep(input$robot_numSearch, df$Robot_Num),c(3,5)]) <- NULL
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,5)]

  })

  
  ####################################
## SANDSTORM INFORMATION ABOUT ROBOT ##
 #################################### 
  
#Server-Side for Sandstorm #Cargo# on CargoShip w/ match Number
  output$A_cargoShip_Cargo_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,13)]
  })
  
  #PLOT
  output$A_cargoShip_Cargo_Line <- renderPlot({
    A_cargoShip_Cargo_Line_data <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,13)]
    ggplot(data = A_cargoShip_Cargo_Line_data, aes(x=Match_Num, y=A_CS_C, ymin=0, ymax=3,group = 1)) + geom_line() + geom_point()
    
  })
  
  
##Server-side for Sandstrom #Hatches# on CargoShip w/ match Number
  output$A_cargoShip_Hatch_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,14)]
  })
  
  #PLOT
  output$A_cargoShip_Hatch_Line <- renderPlot({
    
    A_cargoShip_Hatch_Line_data <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,14)]
    ggplot(data = A_cargoShip_Hatch_Line_data, aes(x=Match_Num, y=A_CS_H, ymin=0, ymax=3,group = 1)) + geom_line() + geom_point()
    
  })
  ##################################
## TELE-OP INFORMATION ABOUT ROBOT ##
  #################################
  
##Server-side for TOTAL ROCKET DATA w/ match Number
  #CARGO
  output$rocket_Cargo_Text_total <- renderPrint({
    newdf <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,15,16,17)]
    newdf$totalCargo <- newdf$C_on_3rd_Lev +newdf$C_on_2nd_Lev +newdf$C_on_1st_Lev 
    newdf
  })
  #HATCH
  output$rocket_Hatch_Text_total <- renderPrint({
    newdf <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,18,19,20)]
    newdf$totalHatch <- newdf$H_on_3rd_Lev +newdf$H_on_2nd_Lev +newdf$H_on_1st_Lev
    newdf
  })
  
##Server-side for TOTAL CARGO SHIP DATA w/ match Number
  #HATCH
  output$cargoship_Hatch_Text_total <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,21)]
  })
  #CARGO
  output$cargoship_Cargo_Text_total <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,22)]
  })
  
##Server-side for PICKUPS AREAS
  ##CARGO FROM LOADING STATION
  output$LS_Cargo_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,23)]
  })
  ##CARGO FROM GROUND
  output$ground_Cargo_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,24)]
  })
  ##CARGP FROM DEPOT
  output$depot_Cargo_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,25)]
  })
  ##HATCH FROM LOADING STATION
  output$LS_Hatch_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,26)]
  })
  
  ##HATCH FROM GROUND
  output$ground_Hatch_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,27)]
  })
  

}


shinyApp(ui = ui, server = server)
