#install.packages("rvest")
library("rvest")
library("plyr")
library("dplyr")
library("xml2")
library("tidyr")
library("ggplot2")
library("data.table")
library("magrittr")
#########
#scrape all per 100 possessions stats from basketball reffernece for the 2020 season
url <- "https://www.basketball-reference.com/leagues/NBA_2020_per_poss.html"
perpossstats2020 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="per_poss_stats"]') %>%
  html_table()

perpossstats2020 <- perpossstats2020[[1]]

#add a column that has the year 2020 for all players
perpossstats2020$year <- rep(2020,nrow(perpossstats2020))

#scrape all per 100 possessions stats from basketball reffernece for the 2019 season
url <- "https://www.basketball-reference.com/leagues/NBA_2019_per_poss.html"
perpossstats2019 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="per_poss_stats"]') %>%
  html_table()

perpossstats2019 <- perpossstats2019[[1]]

#add a column that has the year 2019 for all players
perpossstats2019$year <- rep(2019,nrow(perpossstats2019))

#scrape all per 100 possessions stats from basketball reffernece for the 2018 season
url <- "https://www.basketball-reference.com/leagues/NBA_2018_per_poss.html"
perpossstats2018 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="per_poss_stats"]') %>%
  html_table()

perpossstats2018 <- perpossstats2018[[1]]

#add a column that has the year 2018 for all players
perpossstats2018$year <- rep(2018,nrow(perpossstats2018))

#combine all three seasons of per game stats
perpossstats2018to2020 <- rbind(perpossstats2018,perpossstats2019,perpossstats2020)

#remove the titles rows
perpossstats2018to2020<-perpossstats2018to2020[!perpossstats2018to2020$Rk == "Rk", ]

########

#scrape all advanced stats from basketball reffernece for the 2020 season
url <- "https://www.basketball-reference.com/leagues/NBA_2020_advanced.html"
advancedstats2020 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="advanced_stats"]') %>%
  html_table()

advancedstats2020 <- advancedstats2020[[1]]

#add a column that has the year 2020 for all players
advancedstats2020$year <- rep(2020,nrow(advancedstats2020))

#scrape all advanced stats from basketball reffernece for the 2019 season
url <- "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html"
advancedstats2019 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="advanced_stats"]') %>%
  html_table()

advancedstats2019 <- advancedstats2019[[1]]

#add a column that has the year 2019 for all players
advancedstats2019$year <- rep(2019,nrow(advancedstats2019))

#scrape all advanced stats from basketball reffernece for the 2018 season
url <- "https://www.basketball-reference.com/leagues/NBA_2018_advanced.html"
advancedstats2018 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="advanced_stats"]') %>%
  html_table()

advancedstats2018 <- advancedstats2018[[1]]

#add a column that has the year 2018 for all players
advancedstats2018$year <- rep(2018,nrow(advancedstats2018))

#combine all three seasons of advanced stats
advancedstats2018to2020 <- rbind(advancedstats2018,advancedstats2019,advancedstats2020)

#remove the titles rows
advancedstats2018to2020<-advancedstats2018to2020[!advancedstats2018to2020$Rk == "Rk", ]
#######

#creae unqiue ID in the advanced stats and per game stats of name, year, and team in order to match columns

perpossstats2018to2020$ID<- paste(perpossstats2018to2020$Player,perpossstats2018to2020$Tm,perpossstats2018to2020$year)
advancedstats2018to2020$ID<- paste(advancedstats2018to2020$Player,advancedstats2018to2020$Tm,advancedstats2018to2020$year)

#combine per poss and advanced stats based on unique ID
perpossandadvancedstats <- merge(perpossstats2018to2020,advancedstats2018to2020,by="ID")

#########
# K Means Clustering

#Since K-Means is a distance based model, we need to normalize the data to get meaningful results


df<- perpossandadvancedstats[c(14,15,17,18,29,21,45:47,49:52)]

df[] <- lapply(df, function(x) as.numeric(as.character(x)))

str(df)

#convert all NA's to 0 in dataset
df[is.na(df)] <- 0

# Normalize All the Attributes
scaled.df <-  as.data.frame(scale(df))


# check that we get mean of 0 and sd of 1
colMeans(scaled.df)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.df, 2, sd)


#create 30 clusters as test

fit <- kmeans(scaled.df, 30) # 2 cluster solution


#The quality of the clustering is assessed based on a measure called "withinss", which is the sum of the differences in the distances of the data that are in the same cluster.

totalWithnss <- sum(fit$withinss)
totalWithnss




#change the number of clusters in a loop
totalWithnss = c()
for(clusters in 2:300){
  fit <- kmeans(scaled.df, clusters) 
  totalWithnss[clusters] <- sum(fit$withinss)
}
plot(totalWithnss)
axis(1, at=seq(0, 300, by=25), labels = TRUE)


#It seems this dataset can be partitioned into about 30 distinct clusters. I am going to use 15 so for practical reasons.


fits <- kmeans(scaled.df,15) 
totalWithnss <- sum(fits$withinss)
totalWithnss

fits$cluster
library("cluster")
library("fpc")
plotcluster(scaled.df,fits$cluster)

head(scaled.df)

FINALCLUSTER<- cbind(fits$cluster,perpossandadvancedstats$ID,scaled.df,perpossandadvancedstats$Player.x,perpossandadvancedstats$Tm.x,perpossandadvancedstats$year.x)
head(FINALCLUSTER)
str(FINALCLUSTER)

#un-normalize all data
FINALCLUSTER$`3PA`<- (FINALCLUSTER$`3PA`*sd(df$`3PA`))+mean(df$`3PA`)
FINALCLUSTER$`3P%`<- (FINALCLUSTER$`3P%`*sd(df$`3P%`))+mean(df$`3P%`)
FINALCLUSTER$`2PA`<- (FINALCLUSTER$`2PA`*sd(df$`2PA`))+mean(df$`2PA`)
FINALCLUSTER$`2P%`<- (FINALCLUSTER$`2P%`*sd(df$`2P%`))+mean(df$`2P%`)
FINALCLUSTER$PF<- (FINALCLUSTER$PF*sd(df$PF))+mean(df$PF)
FINALCLUSTER$`FT%`<- (FINALCLUSTER$`FT%`*sd(df$`FT%`))+mean(df$`FT%`)
FINALCLUSTER$FTr<- (FINALCLUSTER$FTr*sd(df$FTr))+mean(df$FTr)
FINALCLUSTER$`ORB%`<- (FINALCLUSTER$`ORB%`*sd(df$`ORB%`))+mean(df$`ORB%`)
FINALCLUSTER$`DRB%`<- (FINALCLUSTER$`DRB%`*sd(df$`DRB%`))+mean(df$`DRB%`)
FINALCLUSTER$`AST%`<- (FINALCLUSTER$`AST%`*sd(df$`AST%`))+mean(df$`AST%`)
FINALCLUSTER$`STL%`<- (FINALCLUSTER$`STL%`*sd(df$`STL%`))+mean(df$`STL%`)
FINALCLUSTER$`BLK%`<- (FINALCLUSTER$`BLK%`*sd(df$`BLK%`))+mean(df$`BLK%`)
FINALCLUSTER$`TOV%`<- (FINALCLUSTER$`TOV%`*sd(df$`TOV%`))+mean(df$`TOV%`)



head(FINALCLUSTER)

#show mean of each category per cluster
meancluster<-aggregate(FINALCLUSTER[, c(3:15)], list(FINALCLUSTER$`fits$cluster`), mean)

par(mfrow = c(1,1))

#create histograms for each category per clsuter
ggplot(FINALCLUSTER,aes(x=FINALCLUSTER[,3]))+geom_histogram(binwidth=2)+facet_grid(~FINALCLUSTER$`fits$cluster`)+theme_bw()

#count the number of players in ecah cluster of the past 3 seasons
table(FINALCLUSTER$`fits$cluster`)

write.csv(FINALCLUSTER, file = "30clusters.csv")

#########

#scrape lineup stats from basketball reffernece for the 2018 season over 24 mins
alllineups2018 <- data.frame(status = NULL)

urlcount <- c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)


for(j in urlcount){
url <-paste0("https://www.basketball-reference.com/play-index/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=per_poss&year_id=2018&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=mp&c1comp=ge&c1val=24&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&order_by=diff_pts&order_by_asc=&offset=",j,"", sep="")
url <-read_html(url)

set100lines<- data.frame(url %>% html_nodes(xpath='//*[@id="stats"]') %>% html_table())
  alllineups2018<- rbind(alllineups2018,set100lines)
}

#change row 1 to be the headers and then delete the original row 1
colnames(alllineups2018) <- as.character(unlist(alllineups2018[1,]))
alllineups2018 = alllineups2018[-1, ]

#romove the header rows thrughout the dataframe
alllineups2018<-alllineups2018[!alllineups2018$Rk == "Rk", ]
alllineups2018<-alllineups2018[!alllineups2018$Rk == "", ]

#add column of year
alllineups2018$year <- rep(2018,nrow(alllineups2018))

#scrape lineup stats from basketball reffernece for the 2019 season over 24 mins
alllineups2019 <- data.frame(status = NULL)

for(j in urlcount){
  url <-paste0("https://www.basketball-reference.com/play-index/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=per_poss&year_id=2019&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=mp&c1comp=ge&c1val=24&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&order_by=diff_pts&order_by_asc=&offset=",j,"", sep="")
  url <-read_html(url)
  
  set100lines<- data.frame(url %>% html_nodes(xpath='//*[@id="stats"]') %>% html_table())
  alllineups2019<- rbind(alllineups2019,set100lines)
}

#change row 1 to be the headers and then delete the original row 1
colnames(alllineups2019) <- as.character(unlist(alllineups2019[1,]))
alllineups2019 = alllineups2019[-1, ]

#romove the header rows thrughout the dataframe
alllineups2019<-alllineups2019[!alllineups2019$Rk == "Rk", ]
alllineups2019<-alllineups2019[!alllineups2019$Rk == "", ]

#add column of year
alllineups2019$year <- rep(2019,nrow(alllineups2019))

######### can be updated
#scrape lineup stats from basketball reffernece for the 2020 season over 24 mins
alllineups2020 <- data.frame(status = NULL)

for(j in urlcount){
  url <-paste0("https://www.basketball-reference.com/play-index/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=per_poss&year_id=2020&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=mp&c1comp=ge&c1val=24&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&order_by=diff_pts&order_by_asc=&offset=",j,"", sep="")
  url <-read_html(url)
  
  set100lines<- data.frame(url %>% html_nodes(xpath='//*[@id="stats"]') %>% html_table())
  alllineups2020<- rbind(alllineups2020,set100lines)
}

#change row 1 to be the headers and then delete the original row 1
colnames(alllineups2020) <- as.character(unlist(alllineups2020[1,]))
alllineups2020 = alllineups2020[-1, ]

#romove the header rows thrughout the dataframe
alllineups2020<-alllineups2020[!alllineups2020$Rk == "Rk", ]
alllineups2020<-alllineups2020[!alllineups2020$Rk == "", ]

#add column of year
alllineups2020$year <- rep(2020,nrow(alllineups2020))

#combine lineups from past three years
lineups2018to2020<- rbind(alllineups2018,alllineups2019,alllineups2020)

#put players in their own columns
setDT(lineups2018to2020)[, paste0("Lineup", 1:5) := tstrsplit(Lineup, '\\|')]

#remove any white space from player names
lineups2018to2020$Lineup1 <- gsub("(^\\s+)|(\\s+$)", "", lineups2018to2020$Lineup1)
lineups2018to2020$Lineup2 <- gsub("(^\\s+)|(\\s+$)", "", lineups2018to2020$Lineup2)
lineups2018to2020$Lineup3 <- gsub("(^\\s+)|(\\s+$)", "", lineups2018to2020$Lineup3)
lineups2018to2020$Lineup4 <- gsub("(^\\s+)|(\\s+$)", "", lineups2018to2020$Lineup4)
lineups2018to2020$Lineup5 <- gsub("(^\\s+)|(\\s+$)", "", lineups2018to2020$Lineup5)

#create unique ideas for each player within each lineup
lineups2018to2020$player1<- paste(lineups2018to2020$Lineup1,lineups2018to2020$Tm,lineups2018to2020$year)
lineups2018to2020$player2<- paste(lineups2018to2020$Lineup2,lineups2018to2020$Tm,lineups2018to2020$year)
lineups2018to2020$player3<- paste(lineups2018to2020$Lineup3,lineups2018to2020$Tm,lineups2018to2020$year)
lineups2018to2020$player4<- paste(lineups2018to2020$Lineup4,lineups2018to2020$Tm,lineups2018to2020$year)
lineups2018to2020$player5<- paste(lineups2018to2020$Lineup5,lineups2018to2020$Tm,lineups2018to2020$year)

#Remove mid spaces from each player ID
lineups2018to2020$player1<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lineups2018to2020$player1, perl=TRUE)
lineups2018to2020$player2<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lineups2018to2020$player2, perl=TRUE)
lineups2018to2020$player3<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lineups2018to2020$player3, perl=TRUE)
lineups2018to2020$player4<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lineups2018to2020$player4, perl=TRUE)
lineups2018to2020$player5<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lineups2018to2020$player5, perl=TRUE)


#create new data frame of just players ID's and their cluster
IDandCluster<- FINALCLUSTER[,1:2]

#I realized that the lineup data had only the first letter of their first name so I needed to fix this
#first take the first letter of each first name
IDandCluster$firstletter<- substr(IDandCluster$`perpossandadvancedstats$ID`, 1, 1)
#put a period after first letter of first name
IDandCluster$firstletter <- paste(IDandCluster$firstletter,".", sep="")
#create a new column of first letter of first name with the period, and add everything after the space in their orginal unique ID
IDandCluster$player1<- paste(IDandCluster$firstletter,sub("^\\S+\\s+", "", IDandCluster$`perpossandadvancedstats$ID`))
#create unique cluster number so when I add 5, those 5 will have a unique sum
IDandCluster$uniquecluster <- mapvalues(IDandCluster$`fits$cluster`, from=c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"), to=c("1.2453647534", "2.93493983", "3.99283922","4.103940209","5.987676584","6.4040493302","7.38484573227","8.667543392","9.193957392","10.3849403339","11.484933009","12.9452985686","13.163353625362","14.969678595","15.29333220203","16.26525242","17.796859568","18.5485748437","19.966829283","20.253626273","21.995849438","22.273832938","23.685949338","24.2493020293","25.3848565849","26.60969594","27.274738329","28.693020485","29.58402848","30.5594939338"))
IDandCluster$uniquecluster<-as.numeric(IDandCluster$uniquecluster)
#create new dataframe with each cluster and new Unique ID
#create this dataframe 5 times and merge all dataframes with main dataframe
NewIDandCluster<- IDandCluster[,c(1,4,5)]

NewIDandCluster2<- IDandCluster[,c(1,4,5)]
colnames(NewIDandCluster2)[2] <- "player2"

NewIDandCluster3<- IDandCluster[,c(1,4,5)]
colnames(NewIDandCluster3)[2] <- "player3"

NewIDandCluster4<- IDandCluster[,c(1,4,5)]
colnames(NewIDandCluster4)[2] <- "player4"

NewIDandCluster5<- IDandCluster[,c(1,4,5)]
colnames(NewIDandCluster5)[2] <- "player5"

#Merge dataframe to give in player ID a cluster (mergedata5 will be the final dataframe)
mergeData<- merge(NewIDandCluster,lineups2018to2020,by="player1")
mergeData2<- merge(NewIDandCluster2,mergeData,by="player2")
mergeData3<- merge(NewIDandCluster3,mergeData2,by="player3")
mergeData4<- merge(NewIDandCluster4,mergeData3,by="player4")
mergeData5<- merge(NewIDandCluster5,mergeData4,by="player5")

#SUM unique cluster numbers
mergeData5$clustersum<- mergeData5$uniquecluster+mergeData5$uniquecluster.x+mergeData5$uniquecluster.y+mergeData5$uniquecluster.x.1+mergeData5$uniquecluster.y.1
#check to see how many unqiue lineups there are
as.data.frame(table(mergeData5$clustersum))

#sum points based on sumcluster
#make data points numeric
mergeData5[,20:36]<-mergeData5[,20:36] %<>% mutate_if(is.character,as.numeric)

#find out total  for each stats within each cluster group sum in order to complete weighted average of all variables
lineupsum2 <- aggregate(cbind(MP) ~ clustersum, mergeData5, sum)
mergeData5<- merge(mergeData5, lineupsum2, by="clustersum")
mergeData5$weight<- mergeData5$MP.x/mergeData5$MP.y

#create finals values by mutliplying each lineup by weight (some values I left the same ie. games, minues, )
str(mergeData5)
mergeData5$finalG<- mergeData5$G
mergeData5$finalMP<- mergeData5$MP.x
mergeData5$finalPace<- mergeData5$Pace * mergeData5$weight
mergeData5$finalFG<- mergeData5$FG * mergeData5$weight
mergeData5$finalFGA<- mergeData5$FGA * mergeData5$weight
mergeData5$finalFGpercent <- mergeData5$`FG%` * mergeData5$weight
mergeData5$`final3P`<- mergeData5$`3P` * mergeData5$weight
mergeData5$`final3PA`<- mergeData5$`3PA` * mergeData5$weight
mergeData5$`final3Ppercent` <- mergeData5$`3P%` * mergeData5$weight
mergeData5$finalefgpercent<- mergeData5$`eFG%` * mergeData5$weight
mergeData5$finalFT<- mergeData5$FT * mergeData5$weight
mergeData5$finalFTA<- mergeData5$FTA * mergeData5$weight
mergeData5$finalFTpercent<- mergeData5$`FT%` * mergeData5$weight
mergeData5$finalPTS<- mergeData5$PTS * mergeData5$weight

#sum all cluster groups together
lineupsum <- aggregate(cbind(finalG,finalMP,finalPace,finalFG,finalFGA,finalFGpercent,`final3P`,`final3PA`,`final3Ppercent`,finalefgpercent,finalFT,finalFTA,finalFTpercent,finalPTS) ~ clustersum, mergeData5, sum)

#create column with all 5 cluster
mergeData5$fiveclusters<- paste(mergeData5$`fits$cluster`,mergeData5$`fits$cluster.x`,mergeData5$`fits$cluster.y`,mergeData5$`fits$cluster.x.1`,mergeData5$`fits$cluster.y.1`)

#create dataframe that I will merge with lineupsum. THis data frame will only have the clustersum and the 5 cluster shown
df<- mergeData5[,c(1,3,6,9,12,15,59)]

#merge the lineups information with the 5 cluster
lineupsum<- merge(lineupsum,df, by= "clustersum")

#remove  all the duplicates because the five clusters can be given in diffent orders which ruins it
lineupsum<- lineupsum[!duplicated(lineupsum$clustersum),]
#sort by net points
lineupsum<-lineupsum[order(-lineupsum$finalPTS),]

#############
#subet players to get players in 2020 on the same team
#change column name in final cluster so I can easily merge data frames
colnames(FINALCLUSTER)[2] <- "ID"
#merge per/48 so i can sort each player on team in the case of a tie break
clustersandWSper<- merge(FINALCLUSTER, advancedstats2018to2020[,c(6:8,24,31)], by= "ID")
clustersandWSper[,19:22]<- clustersandWSper[,19:22] %<>% mutate_if(is.character,as.numeric)

# create list with just team specified players !!!!!!!!!!!!!! CHANGE TO WHATEVER TEAM YOU WISH !!!!!!!!!!!!!!!!
Team <- subset(clustersandWSper, clustersandWSper$`perpossandadvancedstats$year.x` == 2020 & clustersandWSper$`perpossandadvancedstats$Tm.x`== "MIN" & clustersandWSper$G > 10)
#sort by Win shares per 48
Team <-Team[order(-Team$`WS/48`),]

###############
#merge team in the five spots on each lineup
##########!!!!!!!!!!!min cluster grouping total is currently over 100 minutes but can be changed to whatever!!!!!!!!!!!!!
lineupsum100min<- subset(lineupsum,lineupsum$finalMP>100)
teambestlineup <- merge(lineupsum100min,Team[,c(2,16,22)], by = "fits$cluster", all = TRUE)
colnames(teambestlineup)[1] <- "player1cluster"
teambestlineup1<- merge(teambestlineup, Team[,c(2,16,22)], by.x = "fits$cluster.x", by.y = "fits$cluster", all = TRUE)
colnames(teambestlineup1)[1] <- "player2cluster"
teambestlineup2<- merge(teambestlineup1, Team[,c(2,16,22)], by.x = "fits$cluster.y", by.y = "fits$cluster", all = TRUE)
colnames(teambestlineup2)[1] <- "player3cluster"
teambestlineup3<- merge(teambestlineup2, Team[,c(2,16,22)], by.x = "fits$cluster.x.1", by.y = "fits$cluster", all = TRUE)
colnames(teambestlineup3)[1] <- "player4cluster"
teambestlineup4<- merge(teambestlineup3, Team[,c(2,16,22)], by.x = "fits$cluster.y.1", by.y = "fits$cluster", all = TRUE)
colnames(teambestlineup4)[1] <- "player5cluster"


# remove rows with duplicate entries
result = teambestlineup4[apply(teambestlineup4[,c(23,25,27,29,31)], MARGIN =  1, FUN = function(x) !any(duplicated(x))), ]
#delete rows with missing player
result<- result[complete.cases(result), ]
#sum WS/48 for each lineup
colnames(result)[28] <- "WS/48.x.1"
colnames(result)[30] <- "WS/48.y.1"
colnames(result)[23] <- "player1"
colnames(result)[25] <- "player2"
colnames(result)[27] <- "player3"
colnames(result)[29] <- "player4"
colnames(result)[31] <- "player5"

result$WStotal<- result$`WS/48`+result$`WS/48.x`+result$`WS/48.x.1`+result$`WS/48.y`+result$`WS/48.y.1`
#sort by points
result<- result %>% arrange(-finalPTS, -WStotal)
#########
#create one lineup for each cluster grouping
singlelineup<- result[!duplicated(result$clustersum),]
singlelineup<- subset(singlelineup[,c(7:21,23,25,27,29,31)])

#clean results datafram
resultclean<- subset(result[,c(5,4,3,2,1,7:20,23,25,27,29,31,33)])
resultclean$clusters<-  paste(resultclean$player1cluster, resultclean$player2cluster, resultclean$player3cluster, resultclean$player4cluster, resultclean$player5cluster)

#sort cluster data frame
FINALCLUSTER<- FINALCLUSTER %>% arrange(`fits$cluster`)
