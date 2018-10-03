## Title: Short Title
# This is the code involved with preprocessing the data.
# Inputs:
# Outputs:

setwd("Desktop/stat133-hws-fall18/workout1/code")

library(readr)
library(plyr)
library(dplyr)

nba.data <- read_csv("../data/nba2018.csv", col_types = cols(
  experience= col_character(),
  position = col_factor(c('C', 'PF', 'PG', 'SF', 'SG'))
))

# Replace all `R`'s in `experience` with 0's, then convert to type `integer`:

i = 1
while(i <= length(nba.data$experience)){
  if(nba.data$experience[i] == "R"){
    nba.data$experience[i] <- 0
    i <- i+1
  } else{
    nba.data$experience[i] <- nba.data$experience[i]
    i <- i+1
  }
}
nba.data$experience <- as.integer(nba.data$experience)

# Displaying salaries in millions of dollars:
nba.data$salary <- nba.data$salary / (10^6)

# Relabelling levels of `position`:
nba.data$position <- revalue(nba.data$position, c("C" = "center", "PF" = "power_fwd","PG" = "point_guard","SF" = "small_fwd","SG" = "shoot_guard"))

# Adding new columns:
nba.data$missed_fg = nba.data$field_goals_atts - nba.data$field_goals
nba.data$missed_ft = nba.data$points1_atts - nba.data$points1 
nba.data$rebounds = nba.data$off_rebounds + nba.data$def_rebounds 
nba.data$efficiency = (nba.data$points + nba.data$rebounds + nba.data$assists +  nba.data$steals + nba.data$blocks - nba.data$missed_fg - nba.data$missed_ft - nba.data$turnovers )/nba.data$games 

# Sending the efficiencies away
sink("../output/efficiency-summary.txt", append = FALSE) 
summary(nba.data$efficiency)
sink()

####################################
#### Creating nba2018-teams.csv ####
####################################

team_names <- levels(as.factor(nba.data$team))

exp <- c()
sal <- c()
p3 <- c()
p2 <- c()
p1 <- c()
p <- c()
or <- c()
dr <- c()
as <- c()
st <- c()
bl <- c()
t <- c()
f <- c()
eff <- c()

i = 1
while(i <= length(team_names)){
  exp <- c(exp, sum(nba.data$experience[team == team_names[i]]) )
  sal <- c(sal, sum(nba.data$salary[team == team_names[i]]) )
  p3 <- c(p3, sum(nba.data$points3[team == team_names[i]]) )
  p2 <- c(p2, sum(nba.data$points2[team == team_names[i]]) )
  p1 <- c(p1, sum(nba.data$points1[team == team_names[i]]) )
  p <- c(p, sum(nba.data$points[team == team_names[i]]) )
  or <- c(or, sum(nba.data$off_rebounds[team == team_names[i]]) )
  dr <- c(dr, sum(nba.data$def_rebounds[team == team_names[i]]) )
  as <- c(as, sum(nba.data$assists[team == team_names[i]]) )
  st <- c(st, sum(nba.data$steals[team == team_names[i]]) )
  bl <- c(bl, sum(nba.data$blocks[team == team_names[i]]) )
  t <- c(t, sum(nba.data$turnovers[team == team_names[i]]) )
  f <- c(f, sum(nba.data$fouls[team == team_names[i]]) )
  eff <- c(eff, sum(nba.data$efficiency[team == team_names[i]]) )
  i <- i+1
}

teams <- data.frame(
   team = levels(as.factor(nba.data$team)),
   experience = exp,
   salary = sal,
   points3 = p3,
   points2 = p2,
   points1 = p1,
   points = p,
   off_rebounds = or,
   def_rebounds = dr,
   assists = as,
   steals = st,
   blocks = bl,
   turnovers = t,
   fouls = f,
   efficiency = eff
 )

sink("../output/teams-summary.txt", append = FALSE)
summary(teams)
sink()

# Exporting "teams" to a csv file
write_csv(teams, "../data/nba2018-teams.csv", append = FALSE)