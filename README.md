# NBA-teams-best-lineup
Predict the best lineup combination for each team based on 15 player clusters and and historical 5-man lineup performance.
1. Created 15 clusters based on player "rate" statistics over the past three seasons
2. corresponded each player cluster to all 5-player lineups over the past three seasons
3. Combined all lineups with the same 5 clusters based on a weighted average of lineup minutes played
4. Project the best lineup for each team based on the best performing (highest net points) 5 player cluster combinations
note: The final data frame is "resultclean2"
