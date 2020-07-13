
library("rvest")
library("plyr")
library("dplyr")
library("xml2")
library("tidyr")
library("ggplot2")
library("data.table")
library("magrittr")
library("Rserve")
library("cluster")
library("fpc")


#########
#scrape all per 100 possessions stats from basketball reffernece for the 2000-2020 season
perpossstats2000to2020<- data.table()

for(j in 2000:2020){
  url <-paste0("https://www.basketball-reference.com/leagues/NBA_",j,"_per_poss.html", sep="")
  url <-read_html(url)
  
  statsbyyear<- data.frame(url %>% html_nodes(xpath='//*[@id="per_poss_stats"]') %>% html_table())
  statsbyyear$year <- paste(j)
  perpossstats2000to2020<- rbind(perpossstats2000to2020,statsbyyear, use.names=FALSE)
}

perpossstats2000to20201<-perpossstats2000to2020

perpossstats2000to2020<-perpossstats2000to20201


#########clean data
#remove header columns throughtout dataset
perpossstats2000to2020<-perpossstats2000to2020[!perpossstats2000to2020$Rk == "Rk", ]

#scrape all the ID's of each player
perpossstats2000to2020ID<- data.table()

for(j in 2000:2020){
  for(i in 1){
    url <-paste0("https://www.basketball-reference.com/leagues/NBA_",j,"_per_poss.html", sep="")
    url <-read_html(url)
    statsbyyear<- data.frame(url %>% html_nodes(xpath='//*[@id="per_poss_stats"]/tbody/tr[",i,"]/td[1]/a') %>% html_attr("href"))
    colnames(statsbyyear)[1]<-"ID"
    perpossstats2000to2020ID<- rbind(perpossstats2000to2020ID,statsbyyear)
  }
}

perpossstats2000to2020<-cbind(perpossstats2000to2020,perpossstats2000to2020ID)

#remove columns that are completely empty
perpossstats2000to2020 <- Filter(function(x)!all(is.na(x)), perpossstats2000to2020)

#convert columns to numeric
perpossstats2000to2020[,c(4,6:32)] %<>% mutate_if(is.character,as.numeric)

#move year to column 3
perpossstats2000to2020<-perpossstats2000to2020 %>% select(1:2, year, everything())

#order players based on name and age and MP
perpossstats2000to2020 <-perpossstats2000to2020[order(perpossstats2000to2020$Player,perpossstats2000to2020$Age, -perpossstats2000to2020$G),]

#create column of player name and age in order to delete duplicate seasons if player played on multiple teams in a year
perpossstats2000to2020$nameage<- paste(perpossstats2000to2020$Player, perpossstats2000to2020$Age, perpossstats2000to2020$Tm)

#deleted any duplicate season stats of a player if he was traded only keeping the total
#perpossstats2000to2020 <- perpossstats2000to2020[!duplicated(perpossstats2000to2020$nameage),]


########

#scrape all advanced stats from basketball reffernece for the 2020 season
advancedstats2000to2020<- data.table()

for(j in 2000:2020){
  url <-paste0("https://www.basketball-reference.com/leagues/NBA_",j,"_advanced.html", sep="")
  url <-read_html(url)
  
  statsbyyear<- data.frame(url %>% html_nodes(xpath='//*[@id="advanced_stats"]') %>% html_table())
  statsbyyear$year <- paste(j)
  advancedstats2000to2020<- rbind(advancedstats2000to2020,statsbyyear)
}

advancedstats2000to20201<-advancedstats2000to2020
advancedstats2000to2020<-advancedstats2000to20201
#########clean data
#remove header columns throughtout dataset
advancedstats2000to2020<-advancedstats2000to2020[!advancedstats2000to2020$Rk == "Rk", ]


#remove columns that are completely empty
advancedstats2000to2020 <- Filter(function(x)!all(is.na(x)), advancedstats2000to2020)

#convert columns to numeric
advancedstats2000to2020[,c(4,6:28)] %<>% mutate_if(is.character,as.numeric)

#move year to column 3
advancedstats2000to2020<-advancedstats2000to2020 %>% select(1:2, year, everything())

#order players based on name and age and MP
advancedstats2000to2020 <-advancedstats2000to2020[order(advancedstats2000to2020$Player,advancedstats2000to2020$Age, -advancedstats2000to2020$G),]

#create column of player name and age in order to delete duplicate seasons if player played on multiple teams in a year
advancedstats2000to2020$nameage<- paste(advancedstats2000to2020$Player, advancedstats2000to2020$Age, advancedstats2000to2020$Tm)

#deleted any duplicate season stats of a player if he was traded only keeping the total
#advancedstats2000to2020 <- advancedstats2000to2020[!duplicated(advancedstats2000to2020$nameage),]


#######

#combine per poss and advanced stats based on unique ID
perpossandadvancedstats <- merge(perpossstats2000to2020,advancedstats2000to2020,by="nameage")

#convert all NA's to 0 in dataset
perpossandadvancedstats[is.na(perpossandadvancedstats)] <- 0


#weighted player score calculation
#mean
PERmean<- mean(perpossandadvancedstats$PER)
WSper48mean<-mean(perpossandadvancedstats$WS.48)
BPMmean<-mean(perpossandadvancedstats$BPM)
VORPmean<-mean(perpossandadvancedstats$VORP)

#SD
PERsd<- sd(perpossandadvancedstats$PER)
WSper48sd<-sd(perpossandadvancedstats$WS.48)
BPMsd<-sd(perpossandadvancedstats$BPM)
VORPsd<-sd(perpossandadvancedstats$VORP)

#z-score
perpossandadvancedstats$PERz<- (perpossandadvancedstats$PER- PERmean)/PERsd
perpossandadvancedstats$WSper48z<- (perpossandadvancedstats$WS.48- WSper48mean)/WSper48sd
perpossandadvancedstats$BPMz<-(perpossandadvancedstats$BPM- BPMmean)/BPMsd
perpossandadvancedstats$VORPz<-(perpossandadvancedstats$VORP- VORPmean)/VORPsd

#Weighted Z-Score
perpossandadvancedstats$score<- perpossandadvancedstats$PERz+perpossandadvancedstats$WSper48z+perpossandadvancedstats$BPMz+perpossandadvancedstats$VORPz

#remove *
perpossandadvancedstats$Player.x <- stringr::str_replace(perpossandadvancedstats$Player.x, '\\*', '')

#I realized that the lineup data had only the first letter of their first name so I needed to fix this
#first take the first letter of each first name
perpossandadvancedstats$firstletter<- substr(perpossandadvancedstats$Player.x, 1, 1)
#put a period after first letter of first name
perpossandadvancedstats$firstletter <- paste(perpossandadvancedstats$firstletter,".", sep="")
#last name
perpossandadvancedstats$lastname<- sub("^\\S+\\s+", '', perpossandadvancedstats$Player.x)
#create a new column of first letter of first name with the period, and add everything after the space in their orginal unique ID
perpossandadvancedstats$player1<- paste(perpossandadvancedstats$firstletter,perpossandadvancedstats$lastname,perpossandadvancedstats$Tm.x,perpossandadvancedstats$year.x, sep = " ")

#add player links for pics
perpossandadvancedstats$player2<-perpossandadvancedstats$Player.x
perpossandadvancedstats$player2 <- gsub("\\.", "",perpossandadvancedstats$player2)
perpossandadvancedstats<-separate(perpossandadvancedstats, player2, into = c("first", "last"), sep = " (?=[^ ]+$)")
perpossandadvancedstats$firstletter<-substr(perpossandadvancedstats$last, 1, 1)
perpossandadvancedstats$fiveletter<-substr(perpossandadvancedstats$last, 1, 5)
perpossandadvancedstats<-perpossandadvancedstats %>% select(1:33,35:73,ID)
perpossandadvancedstats$ID = substr(perpossandadvancedstats$ID,1,nchar(perpossandadvancedstats$ID)-4)
substring(perpossandadvancedstats$ID, 9,9 ) <- ""
perpossandadvancedstats$url<- paste0("https://d2cwpp38twqe55.cloudfront.net/req/202005142/images",perpossandadvancedstats$ID,"jpg")
perpossandadvancedstats$url<-lapply(perpossandadvancedstats$url, tolower)
perpossandadvancedstats$url<-as.character(perpossandadvancedstats$url)

write.csv()

###############Lineups##############
#scrape lineup stats from basketball reffernece for the 2018 season over 24 mins
alllineups <- data.frame()
for (i in 2000:2020) {
  for(j in seq(0,5000,100)){
    url <-paste0("https://www.basketball-reference.com/play-index/lineup_finder.cgi?request=1&match=single&player_id=&lineup_type=5-man&output=per_poss&year_id=",i,"&is_playoffs=N&team_id=&opp_id=&game_num_min=0&game_num_max=99&game_month=&game_location=&game_result=&c1stat=mp&c1comp=ge&c1val=10&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&order_by=diff_pts&order_by_asc=&offset=",j,"", sep="")
    url <-read_html(url)
    set100lines<- data.frame(url %>% html_nodes(xpath='//*[@id="stats"]') %>% html_table())
    
    alllineups<- rbind(alllineups,set100lines)
  }
}


#change row 1 to be the headers and then delete the original row 1
colnames(alllineups) <- as.character(unlist(alllineups[1,]))
alllineups = alllineups[-1, ]

#romove the header rows thrughout the dataframe
alllineups<-alllineups[!alllineups$Rk == "Rk", ]
alllineups<-alllineups[!alllineups$Rk == "", ]


#change year column to two numbers
alllineups$year<-gsub('-.*',"",alllineups$Season)

#make numeric
alllineups[,c(5:21)] %<>% mutate_if(is.character,as.numeric)

alllineups$year<- alllineups$year+1
alllineups<-alllineups[-c(1888,1382),]
#put players in their own columns
setDT(alllineups)[, paste0("Lineup", 1:5) := tstrsplit(Lineup, '\\|')]

#remove any white space from player names
alllineups$Lineup1 <- gsub("(^\\s+)|(\\s+$)", "", alllineups$Lineup1)
alllineups$Lineup2 <- gsub("(^\\s+)|(\\s+$)", "", alllineups$Lineup2)
alllineups$Lineup3 <- gsub("(^\\s+)|(\\s+$)", "", alllineups$Lineup3)
alllineups$Lineup4 <- gsub("(^\\s+)|(\\s+$)", "", alllineups$Lineup4)
alllineups$Lineup5 <- gsub("(^\\s+)|(\\s+$)", "", alllineups$Lineup5)


#Remove mid spaces from each player
alllineups$Lineup1<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", alllineups$Lineup1, perl=TRUE)
alllineups$Lineup2<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", alllineups$Lineup2, perl=TRUE)
alllineups$Lineup3<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", alllineups$Lineup3, perl=TRUE)
alllineups$Lineup4<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", alllineups$Lineup4, perl=TRUE)
alllineups$Lineup5<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", alllineups$Lineup5, perl=TRUE)

#create unique ideas for each player within each lineup
alllineups$player1<- paste(alllineups$Lineup1,alllineups$Tm,alllineups$year)
alllineups$player2<- paste(alllineups$Lineup2,alllineups$Tm,alllineups$year)
alllineups$player3<- paste(alllineups$Lineup3,alllineups$Tm,alllineups$year)
alllineups$player4<- paste(alllineups$Lineup4,alllineups$Tm,alllineups$year)
alllineups$player5<- paste(alllineups$Lineup5,alllineups$Tm,alllineups$year)

#rename team possessoins in alllineups
colnames(alllineups)[7]<-"tmposs"

