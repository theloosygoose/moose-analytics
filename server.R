library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

server <- function(input, output){
df <-read.csv(file = "2019_03_03_HatboroEvent.csv", header = TRUE, colClasses = c("Team_Num" = "character"))
 
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
    newdf$totalCargo <- newdf$C_on_3_Lev +newdf$C_on_2_Lev +newdf$C_on_1_Lev
    newdf
  })
  #HATCH
  output$rocket_Hatch_Text_total <- renderPrint({
    newdf <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,18,19,20)]
    newdf$totalHatch <- newdf$H_on_3_Lev +newdf$H_on_2_Lev +newdf$H_on_1_Lev
    newdf
  })

##Server-side for TOTAL CARGO SHIP DATA w/ match Number
  #HATCH
  output$cargoship_Hatch_Text_total <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,22)]
  })
  #CARGO
  output$cargoship_Cargo_Text_total <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,21)]
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
##Frequencies FOR pickup HA
  
#Frequencies for pickup CARGO over Total
  output$Cargo_over_total_freq <- renderPrint({
    newdf <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,23,24,25,26,27)]
    newdf$total_All <- rowSums(newdf[,c(2,3,4,5,6)])
    newdf$total_Cargo <- rowSums(newdf[,c(2,3,4)])
    newdf$frequency <- newdf$total_Cargo / newdf$total_All
    finaldata <- select (newdf, -c(2,3,4,5,6,7,8))
    finaldata
  })
#Frequenvies for pick HATCH over Total
  output$Hatch_over_total_freq <- renderPrint({
    newdf <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,23,24,25,26,27)]
    newdf$total_All <- rowSums(newdf[,c(2,3,4,5,6)])
    newdf$total_Hatches <- rowSums(newdf[,c(5,6)])
    newdf$frequency <- newdf$total_Hatches / newdf$total_All
    finaldata <- select (newdf, -c(2,3,4,5,6,7,8))
    finaldata  
  })
  output$Start_Location <- renderPrint ({
    tblofstrt <- df[grep(input$robot_numSearch, df$Robot_Num), c(3,5)]
    r1 <- length(which(tblofstrt == "1R"))
    c1 <- length(which(tblofstrt == "1C"))
    l1 <- length(which(tblofstrt == "1L"))
    l2 <- length(which(tblofstrt == "2L"))
    r2 <- length(which(tblofstrt == "2R"))
    matchesLen <- r1 + c1 + l1 + l2 + r2
    paste(c("L1: "), c(round((l1/matchesLen)*100, digits = 2)) , c("%. C1: ") , c(round((c1/matchesLen)*100, digits = 2)) , c("%. R1: ") , c(round((r1/matchesLen)*100, digits = 2)), c("%. L2: ") , c(round((l2/matchesLen)*100, digits = 2)) , c("%. R2: "), c(round((r2/matchesLen)*100, digits = 2)), c("%."), sep = "")
  })
  




}
