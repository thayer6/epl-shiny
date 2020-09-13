# load libraries
library(rvest)
library(dplyr)

# load webpage
url = "https://www.premierleague.com/tables"
webpage = read_html(url)

# scrape table
#team names
teams_html = html_nodes(webpage, ".long")
teams = html_text(teams_html)

# rankings
rankings_html = html_nodes(webpage, ".value")
rankings = html_text(rankings_html)

# games played
gp_html = html_nodes(webpage, "td:nth-child(4)")
gp = html_text(gp_html)

# wins
wins_html = html_nodes(webpage, "td:nth-child(5)")
wins = html_text(wins_html)

# draws
draws_html = html_nodes(webpage, "td:nth-child(6)")
draws = html_text(draws_html)

# losses
losses_html = html_nodes(webpage, "td:nth-child(7)")
losses = html_text(losses_html)

# goals for
gf_html = html_nodes(webpage, "td:nth-child(8)")
gf = html_text(gf_html)

# goals against
ga_html = html_nodes(webpage, "td:nth-child(9)")
ga = html_text(ga_html)

# goal diff
gd_html = html_nodes(webpage, "td:nth-child(10)")
gd = html_text(gd_html)

# points
points_html = html_nodes(webpage, "td:nth-child(11)")
points = html_text(points_html)

# create table as a df
table = data.frame(rank = rankings,
                   team = teams,
                   games_played = gp,
                   wins = wins,
                   losses = losses,
                   draws = draws,
                   goals_for = gf,
                   goals_against = ga,
                   goal_diff = gd,
                   points = points)
row.names(table) = NULL

# clean up table
table$goal_diff = gsub("\n", "", table$goal_diff)

# drop championship teaams
epl_table = table[-c(21:80),]

# format table
epl_table = epl_table %>% 
  mutate_at(c("rank", "games_played", "wins", "losses", "draws", "goals_for", "goals_against", "goal_diff", "points"), as.character) %>% 
  mutate_at(c("rank", "games_played", "wins", "losses", "draws", "goals_for", "goals_against", "goal_diff", "points"), as.numeric) %>% 
  mutate_at(c("team"), as.character())

# order table by rank
epl_table = epl_table[order(epl_table$rank),]

# draft results
owners = c("Jason", "Casey", "Megan", "CB", "Neo")

# jason teams
jason_teams = c("Manchester City", "Southampton", "Leeds United", "West Bromwich Albion")
jteams_str = paste( unlist(jason_teams), collapse=', ')

# casey teams
casey_teams = c("Liverpool", "Everton", "Sheffield United", "Fulham")
cteams_str = paste( unlist(casey_teams), collapse=', ')

# megan teams
megan_teams = c("Chelsea", "Arsenal", "West Ham United", "Crystal Palace")
mteams_str = paste( unlist(megan_teams), collapse=', ')

# cb teams
cb_teams = c("Manchester United", "Wolverhampton Wanderers", "Burnley", "Brighton and Hove Albion")
cbteams_str = paste( unlist(cb_teams), collapse=', ')

# neo teams
neo_teams = c("Leicester City", "Tottenham Hotspur", "Newcastle United", "Aston Villa")
nteams_str = paste( unlist(neo_teams), collapse=', ')

# create new table
epl_table$owner = ""

# populate table with owner of each team
epl_table[epl_table$team %in% jason_teams,]$owner <- "Jason"
epl_table[epl_table$team %in% casey_teams,]$owner <- "Casey"
epl_table[epl_table$team %in% megan_teams,]$owner <- "Megan"
epl_table[epl_table$team %in% cb_teams,]$owner <- "CB"
epl_table[epl_table$team %in% neo_teams,]$owner <- "Neo ⭐"

# group by owner and sum points to create owner table
otable = epl_table %>% 
  group_by(owner) %>% 
  summarise(points = sum(points))

# order table by points
otable = otable[order(otable$points, decreasing = T),]

# connect to database
library(RMySQL)

# database credentials
db_user = "root"
db_password = "Nuggetsbball2020"
db_name = "scrape_test"
db_table = "epl_table"
db_host = "127.0.0.1"
db_port = 3306

# connect
mydb = dbConnect(MySQL(), user = db_user, password = db_password, dbname = db_name, host = db_host, port = db_port)

# write epl table
dbWriteTable(conn = mydb,
             name = epl_table,
             value = epl_table,
             overwrite = T)
# write owner table
dbWriteTabel(conn = mydb,
             name = owner_table,
             value = otable,
             overwrite = T)