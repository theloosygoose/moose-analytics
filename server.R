library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(scales)
library(tibble)
library(fmsb)
library(rsconnect)

server <- function(input, output){

df <-read.csv(file = "event_data/2019_03_03_HatboroEvent.csv", header = TRUE)

df[df=="?"] <- 0

##MAKING NEW COLUMNS IN THE DATAFRAME
  ##TOTAL HATCHES 32
  df$H_total <- df$H_on_3_Lev + df$H_on_2_Lev + df$H_on_1_Lev + df$CS_H
  #TOTAL CARGO 33
  df$C_total <- df$C_on_3_Lev + df$C_on_2_Lev + df$C_on_1_Lev + df$CS_C


  #Taking Means of Total Cargo
  summary_df <- aggregate(cbind(df$C_total, df$H_total,df$Defense, df$End_Point), by=list(Category=df$Robot_Num), FUN=mean)
  #Making summary_df for EVENT TAB AND TEAM TAB
  names(summary_df) <- c("Team", "Cargo_Avg", "Hatch_Avg", "Defense_Avg","Endgame_Avg")

  #There is an Error where summary_df$Endgame_Avg added 4 to itself but this works to fix it
  summary_df$Endgame_Avg <- summary_df$Endgame_Avg - 4

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
          plotdata <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,13,28)]
          plotdata$Match_Num <- as.character(plotdata$Match_Num)
          ggplot(data = plotdata, aes(x=Match_Num, y=A_CS_C, fill =W_L)) + geom_bar(stat="identity") + ylim(0,3) + scale_fill_gradient(low="red", high="lightgreen")
        })
##Server-side for Sandstrom #Hatches on CargoShip w/ match Number
  output$A_cargoShip_Hatch_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,14)]
  })
        #PLOT
        output$A_cargoShip_Hatch_Line <- renderPlot({
          plotdata <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,14,28)]
          plotdata$Match_Num <- as.character(plotdata$Match_Num)
          ggplot(data = plotdata, aes(x=Match_Num, y=A_CS_H, fill=W_L)) + geom_bar(stat="identity")  + ylim(0,3) + scale_fill_gradient(low="red", high="lightgreen")
        })
  ##################################
## TELE-OP INFORMATION ABOUT ROBOT ##
  #################################

##Server-side for TOTAL ROCKET DATA w/ match Number
  #CARGO
  output$rocket_Cargo_Text_total <- renderPrint({
    cargo_df <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,15,16,17)]
    cargo_df$total_cargo_rocket <-  cargo_df$C_on_3_Lev + cargo_df$C_on_2_Lev + cargo_df$C_on_1_Lev
    cargo_df
  })
        #PLOT
        output$rocket_Cargo_Plot_total <- renderPlot({
          plotdata <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,15,16,17,28)]

          plotdata$rocket_C_total <- plotdata$C_on_3_Lev + plotdata$C_on_2_Lev + plotdata$C_on_1_Lev
          plotdata$Match_Num <- as.character(plotdata$Match_Num)
          ggplot(data = plotdata, aes(x=Match_Num, y=rocket_C_total, fill=W_L)) + geom_bar(stat="identity") + ylim(0,6)+ scale_fill_gradient(low="red", high="lightgreen")
        })

  #HATCH
  output$rocket_Hatch_Text_total <- renderPrint({
    hatch_df <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,18,19,20)]

    hatch_df$total_hatch_rocket <-  hatch_df$H_on_3_Lev + hatch_df$H_on_2_Lev + hatch_df$H_on_1_Lev
    hatch_df
  })
        #PLOT
        output$rocket_Hatch_Plot_total <- renderPlot({
          plotdata <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,18,19,20,28)]
          plotdata$rocket_H_total <- plotdata$H_on_3_Lev + plotdata$H_on_2_Lev + plotdata$H_on_1_Lev
          plotdata$Match_Num <- as.character(plotdata$Match_Num)
          ggplot(data = plotdata, aes(x=Match_Num, y=rocket_H_total, fill=W_L)) + geom_bar(stat="identity") + ylim(0,6) + scale_fill_gradient(low="red", high="lightgreen")
        })

##Server-side for TOTAL CARGO SHIP DATA w/ match Number
  #HATCH 22
  output$cargoship_Hatch_Text_total <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,22)]
  })
      #PLOT
      output$cargoship_Hatch_Plot_total <- renderPlot({
        plotdata <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,22,28)]
        plotdata$Match_Num <- as.character(plotdata$Match_Num)
        ggplot(data = plotdata, aes(x=Match_Num, y=CS_H, fill=W_L)) + geom_bar(stat="identity") + ylim(0,6) + scale_fill_gradient(low="red", high="lightgreen")
      })
  #CARGO 21
  output$cargoship_Cargo_Text_total <- renderPrint({
    df[grep(input$robot_numSearch, df$Robot_Num),c(3,21)]
  })
      #PLOT
      output$cargoship_Cargo_Plot_total <- renderPlot({
        plotdata <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,21,28)]
        plotdata$Match_Num <- as.character(plotdata$Match_Num)
        ggplot(data = plotdata, aes(x=Match_Num, y=CS_C, fill=W_L)) + geom_bar(stat="identity") + ylim(0,6) + scale_fill_gradient(low="red", high="lightgreen")
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

  #################
## Frequencies TAB ###
  #################

#Frequencies for pickup CARGO over Total
  output$Cargo_over_total_freq <- renderPrint({
    newdf <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,23,24,25,26,27)]
    newdf$total_All <- rowSums(newdf[,c(2,3,4,5,6)])
    newdf$total_Cargo <- rowSums(newdf[,c(2,3,4)])
    newdf$frequency <- newdf$total_Cargo / newdf$total_All
    newdf$percentage <- with(newdf, round(newdf$frequency * 100, digits = 2))
    finaldata <- select (newdf, -c(2,3,4,5,6,7,8,9))
    finaldata
  })

#Frequenvies for pick HATCH over Total
  output$Hatch_over_total_freq <- renderPrint({
    newdf <- df[grep(input$robot_numSearch, df$Robot_Num),c(3,23,24,25,26,27)]
    newdf$total_All <- rowSums(newdf[,c(2,3,4,5,6)])
    newdf$total_Hatches <- rowSums(newdf[,c(5,6)])
    newdf$frequency <- newdf$total_Hatches / newdf$total_All
    newdf$percentage <- with(newdf, round(newdf$frequency * 100, digits = 2))
    finaldata <- select(newdf, -c(2,3,4,5,6,7,8,9))
    finaldata
  })
#Starting location Freq
  output$Start_Location <- renderPrint ({
    tblofstrt <- df[grep(input$robot_numSearch, df$Robot_Num), c(3,5)]
    r1 <- length(which(tblofstrt == "1R"))
    c1 <- length(which(tblofstrt == "1C"))
    l1 <- length(which(tblofstrt == "1L"))
    l2 <- length(which(tblofstrt == "2L"))
    r2 <- length(which(tblofstrt == "2R"))
    matchesLen <- r1 + c1 + l1 + l2 + r2
    paste(c("L1: "), c(round((l1/matchesLen)*100, digits = 2)) , c("% || C1: ") , c(round((c1/matchesLen)*100, digits = 2)) , c("% || R1: ") , c(round((r1/matchesLen)*100, digits = 2)), c("% || L2: ") , c(round((l2/matchesLen)*100, digits = 2)) , c("% || R2: "), c(round((r2/matchesLen)*100, digits = 2)), c("%."), sep = "")
  })

#endlocaltion Freq
  output$End_Location <- renderPrint ({
    endloc_df <- df[grep(input$robot_numSearch, df$Robot_Num), c(29)]
    lv_0 <- length(which(endloc_df == 0))
    lv_1 <- length(which(endloc_df == 1))
    lv_2 <- length(which(endloc_df == 2))
    lv_3 <- length(which(endloc_df == 3))
    matchesLen <- lv_0 + lv_1 +lv_2 + lv_3

    paste(c("Level 0: "), c(round((lv_0/matchesLen)*100, digits = 2)) , c("% || Level 1: ") , c(round((lv_1/matchesLen)*100, digits = 2)) , c("% || Level 2: ") , c(round((lv_2/matchesLen)*100, digits = 2)), c("% || Level 3: ") , c(round((lv_3/matchesLen)*100, digits = 2)), c("%."), sep = "")
})




  ###################
## ROBOT SUMMARY TAB ##
  ###################

  output$robot_num <- renderText({
   if (!is.element(input$robot_numSearch, df$Robot_Num)) {
      "Overall"
    } else {
      paste("Team", input$robot_numSearch)
    }
  })
  #THE RADAR CHARTS OF GODS
  output$robot_skills_radar <- renderPlot({
    newdf <- summary_df[grep(input$robot_numSearch, summary_df$Team),]
    newdf$Team <- NULL
    newdf <- rbind(c(0,0,0,0), newdf)
    newdf <- rbind(c(5,4,4,4), newdf)
    radarchart(newdf, axistype = 2,
               pcol='brown3', pfcol='brown3', plwd = 3,
               cglcol="grey", cglty=1, axislabcol="grey", cglwd=2)

  })

#OUTPUT FOR SHOWING THE ROBOT Category
  output$robot_category <- renderText ({
    newdf <- df[grep(input$robot_numSearch, df$Robot_Num),c(15,16,17,18,19,20,21,22,30)]
    ##TOTAL HATCHES 32
    H_total <- newdf$H_on_3_Lev + newdf$H_on_2_Lev + newdf$H_on_1_Lev + newdf$CS_H
    #TOTAL CARGO 33
    C_total <- newdf$C_on_3_Lev + newdf$C_on_2_Lev + newdf$C_on_1_Lev + newdf$CS_C
    hatches <- sum(H_total)
    cargo <- sum(C_total)
    defense <- sum(newdf$Defense) / 2
    
    if (!is.element(input$robot_numSearch, df$Robot_Num)) {
      ""
    } else if (defense > cargo && defense > hatches) {
      "Defensive Robot"
    }else if (cargo > defense && cargo > hatches) {
        "Cargo-Style Robot"
    } else if (hatches > cargo && hatches > defense) {
      "Hatch-Style Robot"
    }

  })


  ####################
## EVENT SUMMARY TAB ##
  ###################
  # Plain output of the summary_df that was made at the top of the code
  output$event_skill_summary <- renderPrint({
    summary_df
  })
  
  #Scatter plot based on summary_df
  output$event_skill_summary_plot <- renderPlot({
    ggplot(summary_df, aes(x=Cargo_Avg, y=Hatch_Avg, label=Team, color=Endgame_Avg)) + geom_point(aes(size=Endgame_Avg)) +geom_text(color ="darkgreen", aes(label=Team),hjust=0, vjust=0) + scale_color_gradient(low="red", high="lightgreen")

  })

    ################
  ## PREDICTION TAB ##
    ################

  #SHOWING PERCENT WIN with inputs on UI.R
  output$total_w_l_corr <- renderPrint ({
    mylogit <- glm(W_L ~ C_on_3_Lev + C_on_2_Lev + C_on_1_Lev + CS_C + CS_H + A_H_on_1_Lev + A_CS_H + H_on_3_Lev + H_on_2_Lev + H_on_1_Lev + Defense, data = df, family = "binomial")
    c3 <- as.integer(input$cargo_3lvl_in)
    c2 <- as.integer(input$cargo_2lvl_in)
    c1 <- as.integer(input$cargo_1lvl_in)

    x <- data.frame(C_on_3_Lev = c3, C_on_2_Lev = c2, C_on_1_Lev = c1, CS_C = as.integer(input$cargo_cs_in), CS_H = as.integer(input$hatch_cs_in), A_H_on_1_Lev = as.integer(input$ahatch_1lvl_in), A_CS_H = as.integer(input$ahatch_cs_in), H_on_3_Lev = as.integer(input$hatch_3lvl_in), H_on_2_Lev = as.integer(input$hatch_2lvl_in), H_on_1_Lev = as.integer(input$hatch_1lvl_in), Defense = as.integer(input$defense_) )
    p<- predict(mylogit,x)
    paste(round(p*100, digits = 2), "% chance of winning", sep = "")
       # c(input$cargo_3lvl_in, input$cargo_2lvl_in, input$cargo_1lvl_in, input$cargo_cs_in, input$hatch_cs_in, input$ahatch_1lvl_in, input$ahatch_cs_in, input$hatch_3lvl_in, input$hatch_2lvl_in, input$hatch_1lvl_in, input$defense_)
  })
  #SUMMARY FOR LINEAR REGRESSION
  output$lin_reg_summ <- renderPrint({
    mylogit <- glm(W_L ~ C_on_3_Lev + C_on_2_Lev + C_on_1_Lev + CS_C + CS_H + A_H_on_1_Lev + A_CS_H + H_on_3_Lev + H_on_2_Lev + H_on_1_Lev + Defense, data = df, family = "binomial")
    summary(mylogit)
  })

}
