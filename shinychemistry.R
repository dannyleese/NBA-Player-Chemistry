library("DT")
library("shiny")
library("shinydashboard")
library("rsconnect")
library("dplyr")
library("data.table")
library("rvest")
library("paletteer")
library("gt")
library("ggiraphExtra")
library("ggplot2")
library("tibble")
library("scales")


data<-read.csv("https://raw.githubusercontent.com/dannyleese/NBA-teams-best-lineup/master/perpossandadv.csv", stringsAsFactors=FALSE)
data[,sapply(data,is.character)] <- sapply(
  data[,sapply(data,is.character)],
  iconv,"WINDOWS-1252","UTF-8")
names(data)[c(4:9,11,14,15:20,23,44,45,47:54)]<-c("Player","year","Pos","Age","Tm","G","MP","FG%","3P","3PA","3P%","2P","2PA","2P%","FT%","TS%","3PAr","ORB%","DRB%","TRB%","AST%","STL%","BLK%","TOV%","USG%")


lineups<-read.csv("https://raw.githubusercontent.com/dannyleese/NBA-teams-best-lineup/master/lineups.csv",stringsAsFactors=FALSE)
lineups[,sapply(lineups,is.character)] <- sapply(
  lineups[,sapply(lineups,is.character)],
  iconv,"WINDOWS-1252","UTF-8")
lineups$placeholder<- paste(lineups$Lineup1,lineups$Lineup2,lineups$Lineup3,lineups$Lineup4,lineups$Lineup5, sep = " | ")
lineups$Lineup<-lineups$placeholder
lineups$placeholder<- NULL
colnames66 <- names(data[,c(5,9:34,43:62)])
teamlist <- c("ATL","BOS","CHO","CHI","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","BRK","NYK","ORL","PHI","PHO","POR","SAS","SAC","OKC","TOR","UTA","NOP","WAS")


ui <- fluidPage(
  titlePanel("NBA Player Chemistry - Danny Leese"),
             
  
  sidebarLayout(
    sidebarPanel(h1("Overview"),
                 a("Danny Leese Twitter", 
                   href = "https://twitter.com/dannyleese"),
                 
                 p("This tool allows any user to quantify the chemistry of NBA lineups based on customized attributes. ",h4("Click through the tabs in the ORDERED sequence to complete the process.")," The approach will be to first group all similar players in the NBA using K-Means Clustering, a machine learning algorithm. For example, there will be a group of 3-point shooters, a group of defensive big men, and a group of pass-first guards, etc. We will then look at all 5-player lineup combinations over the past 20 years and see which groups made up each lineup. By determining the performance level of a 5-group combination, we can quantify the chemistry of that group mixture. Finally, we will fit today's NBA rosters into the best lineup combinations to see the optimal player personnel grouping on each team based on chemistry."),
                 p("For a complete explanation check out ",
                   a("my article", 
                     href = "https://medium.com/the-sports-scientist/quantifying-nba-player-chemistry-d6c4fa8f016e?source=friends_link&sk=ef4ae776a0174ed513634885f4ca938d")),
                 h2("Instructions For Each Tab"),
                 h3("Graph:"),
                 p("This graph plots the total within-cluster sum of squares based on the number of clusters. This chart will help you determine the number of clusters you want. The more clusters you have, the more specific each cluster will be."),
                 h3("Selections:"),
                 p("Choose your desired K-Means Clustering characteristics based off the number of clusters and customizable categories."),
                 h3("Player Cluster Output:"),
                 p("This table displays the cluster classification for each player along with their season statistics."),
                 h3("Cluster Averages:"),
                 p("This table displays the cluster mean statistics."),
                 h3("Lineups:"),
                 p("This table displays the per 100 possessions statistics of every lineup since 2000 (minimum 10 minutes)."),
                 h3("Lineup Grouping:"),
                 p("This table displays the per 100 possessions statistics of each cumulative lineup grouping. To view which lineups make up this five-cluster grouping, search the ID on the Lineups tab."),
                 h3("Team Specific Lineups:"),
                 p("This table displays the lineup projections for the chosen team. Current NBA rosters are fit to the best performing cluster groupings (in terms of Net Points). The cluster grouping will be filtered based on the minutes minimum chosen."),
                 h3("Lineup Customization:"),
                 p("This table displays the lineup projections based on the clusters of the chosen players.")
                 
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Cover",
                           h1("NBA Player Chemistry Tool"),
                           h3("Read instructions on the left and click through the tabs"),
                           uiOutput("mycover")),
                  
                  tabPanel("Graph",
                           uiOutput("myImage")),
        tabPanel("Selections",
                 
                 fluidRow(
                   
                   column(4,
                          numericInput("cluster_number", 
                                      h3("Select Number of Clusters (2:100)"), 
                                      value = 20, min = 0, max = 100)),
                 h3("Choose Categories To Create Clusters:"),
                   
                    column(
                     width = 2,
                     checkboxGroupInput(
                       inputId = "checka",
                       label = "Per 100 Possession Statistics",
                       choices = colnames66[1:27],
                       selected = colnames66[c(9,10,12,13,15,16,24)]
                     )
                   ),
                   column(
                     width = 2,
                     checkboxGroupInput(
                       inputId = "checkb",
                       label = "Advanced Statistics",
                       choices = colnames66[28:47],
                       selected = colnames66[c(31,32,33,35,36,37,38)]
                       
                     )
                   ))),
              
        tabPanel("Player Cluster Output",
                 h3( "Note: Wait up to 6 secods for the table to load or reload"),
                 fluidRow( tags$head(tags$style(
                   HTML("input[type='search']:disabled {visibility:hidden}")
                 ))),
                 fluidRow(
                   DT::dataTableOutput("mytable1")
                 )),
        tabPanel("Cluster Averages",
                 h3( "Note: With several clusters, this table will take a few seconds to load"),
                 fluidRow(
                   #DT::dataTableOutput("mytable2")
                   column(8,
                          gt_output(outputId = "mytable2")),
                   column(4,
                          uiOutput("secondSelection"),
                          fluidRow( plotOutput("plot2")))
                         
                 )),
        tabPanel("Lineups",
                 #h3("This table displays the per 100 possessions statistics of each lineup"),
                 fluidRow(
                   DT::dataTableOutput("mytable3")
                 )),
        tabPanel("Lineup Grouping",
                 #h3("This table displays the per 100 possessions statistics of each cumulative lineup grouping"),
                 fluidRow(column(
                   DT::dataTableOutput("mytable4"), width = 10)
                   
                 )),
        tabPanel("Team Specific Lineups",
                
                 fluidRow(
                   column(4,
                          numericInput("Lineup_minutes", 
                                       h5("Select Minutes Minimum for a lineup Grouping"), 
                                       value = 500, min = 0, max = 1000)),
                   column(4,
                          numericInput("Player_minutes", 
                                       h5("Select Minutes Minimum for players on current roster"), 
                                       value = 100, min = 0, max = 2000)),
                   column(4,
                          selectInput("select", h5("Select Team"), 
                                      choices = teamlist, selected = "TOR"))),
                 fluidRow(
                   
                   #h3("This table displays the lineup projections for the chosen team"),
                   DT::dataTableOutput("mytable5")
                 )),
        tabPanel("Lineup Projection",
                 h5("Note: If this five player cluster combination has never been on the court together, no data will be shown."),
                 h5("Note: The five clusters are not in the same order as the players"),
                 


                 fluidRow(
                   column(2,
                          selectInput("player1", h5("Player 1:"),
                                      choices = data[data$year==2020,"Player"], selected = 1)),
                   column(2,
                          selectInput("player2", h5("Player 2:"),
                                      choices = data[data$year==2020,"Player"], selected = "Chris Paul")),
                   column(2,
                          selectInput("player3", h5("Player 3:"),
                                      choices = data[data$year==2020,"Player"], selected = "Kyle Lowry")),
                   column(2,
                          selectInput("player4", h5("Player 4:"),
                                      choices = data[data$year==2020,"Player"], selected = "Kristaps Porzingis")),
                   column(2,
                          selectInput("player5", h5("Player 5:"),
                                      choices = data[data$year==2020,"Player"], selected = "RJ Barrett"))),
                 fluidRow(
                   column(2,
                          uiOutput("img1")),
                   column(2,
                          uiOutput("img2")),
                   column(2,
                          uiOutput("img3")),
                   column(2,
                          uiOutput("img4")),
                   column(2,
                          uiOutput("img5"))),

                 fluidRow(column(
                   DT::dataTableOutput("mytable6"), width = 9))
        )
      ))
  )
)
server <- function(input, output) {
  
  output$myImage <- renderUI({
    img(src ="https://raw.githubusercontent.com/dannyleese/NBA-teams-best-lineup/master/Withinss.png")
  })
  output$mycover <- renderUI({
    img(src ="https://clutchpoints.com/wp-content/uploads/2019/08/THUMBNAIL_077-3.jpg", height="90%", width="90%")
  })
  
  output$img1 <- renderUI({
    img(src =data[data$Player==input$player1 & data$year==2020,"url" ])
  })
  output$img2 <- renderUI({
    img(src =data[data$Player==input$player2 & data$year==2020,"url" ])
  })
  output$img3 <- renderUI({
    img(src =data[data$Player==input$player3 & data$year==2020,"url" ])
  })
  output$img4 <- renderUI({
    img(src =data[data$Player==input$player4 & data$year==2020,"url" ])
  })
  output$img5 <- renderUI({
    img(src =data[data$Player==input$player5 & data$year==2020,"url" ])
  })
    
  

    output$mytable1 <- DT::renderDataTable({
      
      normalize <- function(x) {
        return ((x - min(x)) / (max(x) - min(x)))
      }  

      scaled.df<-data[,c(input$checka,input$checkb)]
      
      scaled.dfreal <- as.data.frame(lapply(scaled.df, normalize))
      fits <- kmeans(scaled.dfreal,input$cluster_number) 
      #create new dataframe with just chosen columns *** (all with *** mean addded for this purpose)
      cols<-c(input$checka,input$checkb)
      FINALCLUSTERexact<-cbind(fits$cluster,data[,c(4,8,5,7)])
      FINALCLUSTERexact<- cbind(FINALCLUSTERexact,data[,cols])
      names(FINALCLUSTERexact)[1]<- "V2"
      #***
      FINALCLUSTER<- cbind(fits$cluster,data)
      FINALCLUSTER[,c(2:4,36:43,64:67,69:70)]<-NULL
      names(FINALCLUSTER)[1]<- "V2"
      #create unique cluster number so when I add 5, those 5 will have a unique sum
      df2 <- FINALCLUSTER[,c("V2","year")]
      df2<- df2[!duplicated(df2$V2),]
      df2$uniquecluster <- runif(nrow(df2), min = 0, max = 100)
      FINALCLUSTER<- merge(FINALCLUSTER, df2[,c(1,3)], by= "V2", all= TRUE)
      FINALCLUSTER<-FINALCLUSTER[order(FINALCLUSTER$V2,FINALCLUSTER$Player),]
      #create new dataframe with just chosen columns ***
      FINALCLUSTERexact<- merge(FINALCLUSTERexact, df2[,c(1,3)], by= "V2", all= TRUE)
      FINALCLUSTERexact<-FINALCLUSTERexact[order(FINALCLUSTERexact$V2),]
      ##***

      #output$mytable2 <- DT::renderDataTable({
        #meancluster<-aggregate(FINALCLUSTER[,c(7,9:52)], list(FINALCLUSTER[,1]), mean)
        #meancluster$count<-table(FINALCLUSTER[,1])
        meancluster<-aggregate(FINALCLUSTERexact[,c(-2,-3,-4,-5)], list(FINALCLUSTERexact[,1]), mean)
        meancluster$uniquecluster<-NULL
        meancluster$V2<-NULL
        meancluster <- meancluster[, !duplicated(colnames(meancluster))]
        meancluster$count<-table(FINALCLUSTERexact[,1])
        meancluster <- meancluster %>%
          mutate_if(is.numeric, round, digits = 2)
        colnames(meancluster)[1]<-"Cluster"
        
        # DT::datatable(meancluster,rownames = FALSE,
        #               extensions = "FixedColumns",
        #               options = list(
        #                 paging = TRUE, searching = TRUE, info = FALSE, fixedHeader=FALSE,
        #                 sort = TRUE, scrollX = TRUE,scrollY = "400px", fixedColumns = list(leftColumns = 1),
        #                 pageLength = 100, autoWidth = TRUE))%>%
        #})
       
        output$mytable2 <-render_gt({
              meancluster  %>% 
                gt() %>% 
                data_color(columns=c(2:(ncol(meancluster)-1)),
                           colors = scales::col_numeric(
                             palette = c(
                               "red", "white", "green"),
                             domain = NULL))
            })
          
              # ),width = px(700))
        
        output$secondSelection <- renderUI({
          selectInput("cluster_select", "Select Cluster:",
                      choices=as.numeric(meancluster[,"Cluster"]))
        })
      
      
        output$plot2<-renderPlot({
          meanclusterscaled <- as.data.frame(lapply(meancluster[,c(2:ncol(meancluster))], normalize))
          meanclusterscaled <- cbind(meancluster[,1],meanclusterscaled)
          colnames(meanclusterscaled)[c(1:(ncol(meanclusterscaled)-1))]<- names(meancluster[c(1:(ncol(meancluster)-1))])
          colnames(meanclusterscaled)[ncol(meanclusterscaled)]<-"Count"
          meanclusterscaled2<-meanclusterscaled[input$cluster_select,]
          ggRadar(meanclusterscaled2, aes(group = Cluster), rescale = FALSE)+
            ggtitle(paste("Cluster", input$cluster_select, "Radar PLot")) +
          theme(
            legend.position = 'none')

          })
      
      #create new dataframe with each cluster and new Unique ID
      #create this dataframe 5 times and merge all dataframes with main dataframe
      NewIDandCluster<- FINALCLUSTER[,c(53,1,54,60)]
      
      NewIDandCluster2<- FINALCLUSTER[,c(53,1,54,60)]
      colnames(NewIDandCluster2)[3] <- "player2"
      
      NewIDandCluster3<- FINALCLUSTER[,c(53,1,54,60)]
      colnames(NewIDandCluster3)[3] <- "player3"
      
      NewIDandCluster4<- FINALCLUSTER[,c(53,1,54,60)]
      colnames(NewIDandCluster4)[3] <- "player4"
      
      NewIDandCluster5<- FINALCLUSTER[,c(53,1,54,60)]
      colnames(NewIDandCluster5)[3] <- "player5"
      
      
      
      #Merge dataframe to give in player ID a cluster (mergedata5 will be the final dataframe)
      mergeData<- merge(NewIDandCluster,lineups,by="player1")
      mergeData2<- merge(NewIDandCluster2,mergeData,by="player2")
      mergeData3<- merge(NewIDandCluster3,mergeData2,by="player3")
      mergeData4<- merge(NewIDandCluster4,mergeData3,by="player4")
      colnames(mergeData4)[c(2:4,6:8)]<- c("score.xx","V2.xx","uniquecluster.xx","score.yy","V2.yy","uniquecluster.yy")
      mergeData5<- merge(NewIDandCluster5,mergeData4,by="player5")
      
      #SUM unique cluster numbers
      mergeData5$clustersum<- mergeData5$uniquecluster+mergeData5$uniquecluster.x+mergeData5$uniquecluster.y+mergeData5$uniquecluster.xx+mergeData5$uniquecluster.yy
      
      #sum points based on sumcluster
      
      #find out total  for each stats within each cluster group sum in order to complete weighted average of all variables
      lineupsum2 <- aggregate(cbind(MP) ~ clustersum, mergeData5, sum)
      mergeData5<- merge(mergeData5, lineupsum2, by="clustersum")
      mergeData5$weight<- mergeData5$MP.x/mergeData5$MP.y
      
      #create finals values by mutliplying each lineup by weight (some values I left the same ie. games, minues, )
      mergeData5$finalG<- mergeData5$G
      mergeData5$finalMP<- mergeData5$MP.x
      mergeData5$finalPace<- mergeData5$Pace * mergeData5$weight
      mergeData5$finalFG<- mergeData5$FG * mergeData5$weight
      mergeData5$finalFGA<- mergeData5$FGA * mergeData5$weight
      mergeData5$finalFGpercent <- mergeData5$FG. * mergeData5$weight
      mergeData5$`final3P`<- mergeData5$X3P * mergeData5$weight
      mergeData5$`final3PA`<- mergeData5$X3PA * mergeData5$weight
      mergeData5$`final3Ppercent` <- mergeData5$X3P. * mergeData5$weight
      mergeData5$finalefgpercent<- mergeData5$eFG. * mergeData5$weight
      mergeData5$finalFT<- mergeData5$FT * mergeData5$weight
      mergeData5$finalFTA<- mergeData5$FTA * mergeData5$weight
      mergeData5$finalFTpercent<- mergeData5$FT. * mergeData5$weight
      mergeData5$finalPTS<- mergeData5$PTS * mergeData5$weight
      
      #sum all cluster groups together
      lineupsum <- aggregate(cbind(finalG,finalMP,finalPace,finalFG,finalFGA,finalFGpercent,`final3P`,`final3PA`,`final3Ppercent`,finalefgpercent,finalFT,finalFTA,finalFTpercent,finalPTS) ~ clustersum, mergeData5, sum)
      
      #create column with all 5 cluster
      mergeData5$fiveclusters<- paste(mergeData5$V2.y,mergeData5$V2.x,mergeData5$V2.yy,mergeData5$V2.xx,mergeData5$V2)
      mergeData5<-mergeData5[order(-mergeData5$PTS),]
      
      
      #create dataframe that I will merge with lineupsum. THis data frame will only have the clustersum and the 5 cluster shown
      df66<- mergeData5[,c(1,4,8,12,16,20,65)]
      
      #merge the lineups information with the 5 cluster
      lineupsum<- merge(lineupsum,df66, by= "clustersum")
      
      #remove  all the duplicates because the five clusters can be given in diffent orders which ruins it
      lineupsum<- lineupsum[!duplicated(lineupsum$clustersum),]
      #sort by net points
      lineupsum<-lineupsum[order(-lineupsum$finalPTS),]
      
      
      output$mytable3 <- DT::renderDataTable({
        colnames(mergeData5)[c(1,28,42)]<-c("ID","MP","Net PTS")
        # mergeData5 <- mergeData5 %>%
        #   mutate_if(is.numeric, round, digits = 5)
        DT::datatable(mergeData5[,c(1,25,24,65,43,42,27,28)],rownames = FALSE,options = list(
          scrollY = "400px", fixedColumns = list(leftColumns = 2), columnDefs = list(list(className = 'dt-center', targets = 0:4)),
          pageLength = 1000, autoWidth = TRUE))
      })
      
     
      output$mytable4 <- DT::renderDataTable({
        # lineupsum <- lineupsum %>%
        #   mutate_if(is.numeric, round, digits = 5)
        colnames(lineupsum)[c(1,3,15,2)]<-c("ID","MP","Net PTS","G")
        DT::datatable(lineupsum[,c(1,21,15,2,3)],rownames = FALSE,options = list(
          scrollY = "400px",columnDefs = list(list(className = 'dt-center', targets = 0:4)),
          pageLength = 1000, autoWidth = TRUE))%>% formatRound(c(3), 2)
      })
      
      
      
      
      output$mytable5 <- DT::renderDataTable({
        
        resultclean2<-data.frame()
        resultclean<-data.frame()
        
        #run code for all teams
        
        # create list with just team specified players !!!!!!!!!!!!!! CHANGE TO WHATEVER TEAM YOU WISH !!!!!!!!!!!!!!!!
        Team <- subset(FINALCLUSTER, FINALCLUSTER$year == 2020 & FINALCLUSTER$Tm == input$select & FINALCLUSTER$MP> input$Player_minutes)
        #sort by Win shares per 48
        Team <-Team[order(-Team$score),]
        
        #first get updated rosters to accout for traded player this year
        updatedrosters<- data.table()
        
        
        url <-paste0("https://www.basketball-reference.com/teams/",input$select,"/2020.html", sep="")
        url <-read_html(url)
        statsbyteam<- data.frame(url %>% html_nodes(xpath='//*[@id="roster"]') %>% html_table())
        updatedrosters<- rbind(updatedrosters,statsbyteam)
        updatedrosters$Player<-gsub("\\s*\\([^\\)]+\\)","",as.character(updatedrosters$Player))
        updatedrosters<-updatedrosters[,2]
        
        #only take active players
        Team<-merge(updatedrosters, Team, by = "Player")
        
        #create random number for each player
        Team$randoms <- runif(nrow(Team), min=0, max=1)
        
        ###############
        #merge team in the five spots on each lineup
        ##########!!!!!!!!!!!min cluster grouping total mins over 100 mins!!!!!!!!!!!!!
        lineupsum100min<- subset(lineupsum,lineupsum$finalMP>input$Lineup_minutes)
        teambestlineup <- merge(lineupsum100min,Team[,c(1,2,53,61)], by.x = "V2", by.y = "V2", all = TRUE)
        colnames(teambestlineup)[1] <- "player1cluster"
        teambestlineup1<- merge(teambestlineup, Team[,c(1,2,53,61)], by.x = "V2.xx", by.y = "V2", all = TRUE)
        colnames(teambestlineup1)[1] <- "player2cluster"
        teambestlineup2<- merge(teambestlineup1, Team[,c(1,2,53,61)], by.x = "V2.yy", by.y = "V2", all = TRUE)
        colnames(teambestlineup2)[1] <- "player3cluster"
        teambestlineup3<- merge(teambestlineup2, Team[,c(1,2,53,61)], by.x = "V2.x", by.y = "V2", all = TRUE)
        colnames(teambestlineup3)[c(1,28:33)] <- c("player4cluster","player.xxx","score.xxx","random.xxx","player.xyx","score.yyy","random.yyy")
        teambestlineup4<- merge(teambestlineup3, Team[,c(1,2,53,6,61)], by.x = "V2.y", by.y = "V2", all = TRUE)
        colnames(teambestlineup4)[1] <- "player5cluster"
        
        
        # remove rows with duplicate entries
        result = teambestlineup4[apply(teambestlineup4[,c(22,25,28,31,34)], MARGIN =  1, FUN = function(x) !any(duplicated(x))), ]
        #delete rows with missing player
        result<- result[complete.cases(result), ]
        #sum WS/48 for each lineup
        colnames(result)[23] <- "scorep1"
        colnames(result)[26] <- "scorep2"
        colnames(result)[29] <- "scorep3"
        colnames(result)[32] <- "scorep4"
        colnames(result)[35] <- "scorep5"
        colnames(result)[22] <- "player1"
        colnames(result)[25] <- "player2"
        colnames(result)[28] <- "player3"
        colnames(result)[31] <- "player4"
        colnames(result)[34] <- "player5"
        colnames(result)[24] <- "rand1"
        colnames(result)[27] <- "rand2"
        colnames(result)[30] <- "rand3"
        colnames(result)[33] <- "rand4"
        colnames(result)[37] <- "rand5"
        
        result$scoretotal<- result$scorep1+ result$scorep2+ result$scorep3 + result$scorep4 + result$scorep5
        #sort by points
        result<- result %>% arrange(-finalPTS, -scoretotal)
        
        #sumrandoms
        result$randsum<- result$rand1 +result$rand2+result$rand3  +result$rand4 +result$rand5
        # remove rows with duplicate entries
        result<- distinct(result,randsum, .keep_all= TRUE)
        
        #clean results datafram
        resultclean<- subset(result[,c(1:6,7,20,21,22,25,28,31,34,36,38)])
        resultclean$clusters<-  paste(resultclean$player1cluster, resultclean$player2cluster, resultclean$player3cluster, resultclean$player4cluster, resultclean$player5cluster)
        
        resultclean2<- rbind(resultclean2,resultclean)
        
        # resultclean2 <- resultclean2 %>%
        #   mutate_if(is.numeric, round, digits = 5)
        colnames(resultclean2)[c(6,8)]<-c("ID","Net PTS")
        DT::datatable(resultclean2[,c(6,17,8,10:14)],rownames = FALSE,options = list(
            scrollY = "400px",columnDefs = list(list(className = 'dt-center', targets = 0:4)),
            pageLength = 1000, autoWidth = TRUE))%>% formatRound(c(3), 2)
      })
      
      output$mytable6 <- DT::renderDataTable({
        FINALCLUSTER2020<-FINALCLUSTER[order(FINALCLUSTER$Player,FINALCLUSTER$year, -FINALCLUSTER$MP),]
        FINALCLUSTER2020$nameyear<- paste(FINALCLUSTER2020$Player,FINALCLUSTER2020$year)
        FINALCLUSTER2020<- FINALCLUSTER2020[!duplicated(FINALCLUSTER2020$nameyear),]
        value<- FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player1 & FINALCLUSTER2020$year==2020,"uniquecluster"]+FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player2 & FINALCLUSTER2020$year==2020,"uniquecluster"]+FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player3 & FINALCLUSTER2020$year==2020,"uniquecluster"]+FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player4 & FINALCLUSTER2020$year==2020,"uniquecluster"]+FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player5 & FINALCLUSTER2020$year==2020,"uniquecluster"]
        colnames(lineupsum)[1]<-"ID"
        
        output$img1 <- renderUI({
          img(src =FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player1 & FINALCLUSTER2020$year==2020,"url" ])
        })
        output$img2 <- renderUI({
          img(src =FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player2 & FINALCLUSTER2020$year==2020,"url" ])
        })
        output$img3 <- renderUI({
          img(src =FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player3 & FINALCLUSTER2020$year==2020,"url" ])
        })
        output$img4 <- renderUI({
          img(src =FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player4 & FINALCLUSTER2020$year==2020,"url" ])
        })
        output$img5 <- renderUI({
          img(src =FINALCLUSTER2020[FINALCLUSTER2020$Player==input$player5 & FINALCLUSTER2020$year==2020,"url" ])
        })
        
        colnames(lineupsum)[c(15,4,2,3)]<-c("Net PTS", "Pace","G","MP")
        DT::datatable(lineupsum[lineupsum$ID==value,c(1,21,15,4,2,3)],rownames = FALSE,options = list(
          autoWidth = TRUE,dom = 't', columnDefs = list(list(className = 'dt-center', targets = 0:4))))%>% formatRound(c(3,4), 2)
      })

      #FINALCLUSTERclean<-FINALCLUSTER %>% select(1:2,4,6, everything())
      #colnames(FINALCLUSTERclean) [1]<-"Cluster"
      colnames(FINALCLUSTERexact) [1]<-"Cluster"
      FINALCLUSTERexact$uniquecluster<-NULL
      FINALCLUSTERexact <- FINALCLUSTERexact[, !duplicated(colnames(FINALCLUSTERexact))]
      #DT::datatable(FINALCLUSTERclean[,c(-53,-54,-55,-56,-57,-58,-59,-60)],rownames = FALSE, filter = "top",
      DT::datatable(FINALCLUSTERexact,rownames = FALSE, filter = "top",
                    options = list(
                      paging = TRUE, searching = TRUE, fixedHeader=FALSE,
                      sort = TRUE, scrollX = TRUE, scrolly="200px", columnDefs = list(list(
                        targets = c(5:(ncol(FINALCLUSTERexact)-1)), searchable = FALSE
                      )), pageLength = 100, autoWidth = TRUE))
      
   })

}

shinyApp(ui, server)
