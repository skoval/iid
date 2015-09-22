library(dplyr)
library(lubridate)
library(deuce)
data(point_by_point)
#data(charting_points)

point_by_point <- point_by_point[point_by_point$tour == "ATP",]
point_by_point  <- point_by_point[point_by_point$draw == "Main",]

index1 <- !grepl(";", point_by_point$Set1) & !is.na(point_by_point$Set1)
index2 <- !grepl(";", point_by_point$Set2) & !is.na(point_by_point$Set2)
index3 <- !grepl(";", point_by_point$Set3) & !is.na(point_by_point$Set3)
index4 <- !grepl(";", point_by_point$Set4) & !is.na(point_by_point$Set4)
index5 <- !grepl(";", point_by_point$Set5) & !is.na(point_by_point$Set5)

point_by_point$Set1[index1] <- sapply(point_by_point$Set1[index1], add_semicolon)
point_by_point$Set2[index2] <- sapply(point_by_point$Set2[index2], add_semicolon)
point_by_point$Set3[index3] <- sapply(point_by_point$Set3[index3], add_semicolon)
point_by_point$Set4[index4] <- sapply(point_by_point$Set4[index4], add_semicolon)
point_by_point$Set5[index5] <- sapply(point_by_point$Set5[index5], add_semicolon)

point_by_point$year <- year(point_by_point$tny_date)

atp_points <- do.call("rbind", lapply(1:nrow(point_by_point), function(x) {
	result <- pbp(point_by_point[x,])
	result$match_id <- paste(point_by_point[x,]$tny_name, 
		point_by_point[x,]$server1, 
		point_by_point[x,]$server2, 
		point_by_point[x,]$year, sep = ":")
result
}))


atp_points$tournament <- sub("(.ATP)(.*)(:.*:.*:.*)", "\\2", atp_points$match_id)

# Limit to 250+
data(atp_matches)

tourney_name <- unique(atp_matches$tourney_name[
	year(atp_matches$tourney_start_date) >= 2010 &
	!(atp_matches$tourney_level %in% c("Challenger", "Tour Finals", "Davis Cup"))
])

tournaments <- c(
	"Auckland", 
	"Munich", 
	"Madrid", 
	"Belgrade", 
	"Houston", 
	"Wimbledon", 
	"Estoril", 
	"Montpellier", 
	"Zagreb", 
	"Valencia", 
	"Casablanca", 
	"Nice", 
	"s Hertogenbosch", 
	"US Open", 
	"Bucharest", 
	"St. Petersburg", 
	"Atlanta", 
	"Qatar", 
	"New Haven", 
	"Johannesburg", 
	"Shanghai", 
	"Australian Open", 
	"Buenos Aires", 
	"Stockholm", 
	"Santiago", 
	"Halle", 
	"Paris", 
	"Rogers Cup", 
	"Cincinnati", 
	"Los Angeles", 
	"San Jose", 
	"Barcelona", 
	"Sydney", 
	"Brisbane", 
	"Vienna", 
	"Eastbourne", 
	"Costa do Sauipe", 
	"Beijing", 
	"Umag", 
	"Moscow", 
	"Metz", 
	"Tokyo", 
	"Basel", 
	"Stuttgart", 
	"Acapulco", 
	"Kuala Lumpur", 
	"French Open", 
	"Rotterdam", 
	"Indian Wells", 
	"Memphis", 
	"Miami", 
	"Bangkok", 
	"Bastad", 
	"Gstaad", 
	"Newport", 
	"Queens", 
	"Delray Beach",
	 "Dubai", 
	 "Marseille", 
	 "Chennai", 
	"Monte Carlo", 
	"Hamburg", 
	"Rome", 
	"Washington", 
	"Dusseldorf", 
	"Kitzbuhel", 
	"Winston Salem", 
	"Sao Paulo", 
	"Bogota", 
	"Vina del Mar", 
	"Shenzhen", 
	"Rio de Janeiro", 
	"Istanbul", 
	"Quito", 
	"Miami", 
	"London", 
	"Eastbourne", 
	"Madrid", 
	"Rome",
	"Geneva")
	
matches <- lapply(gsub(" ","",tolower(tournaments)), function(x) 
	grep(x, gsub("-","",tolower(atp_points$match_id))))	

atp_points$tournament <- NA

for(i in 1:length(tournaments)){
	atp_points$tournament[matches[[i]]] <- tournaments[i]
}

atp_points <- atp_points[!is.na(atp_points$tournament),]


# Assign returner
atp_points <- do.call("rbind", lapply(split(atp_points, f = atp_points$match_id), function(obj){
	players <- unique(obj$serve)
	obj$returning <- ifelse(obj$serve == players[1], players[2], players[1])
obj
}))

names(atp_points)[names(atp_points) == "serve"] <- "serving"

points_played <- atp_points %>%
	group_by(serving) %>%
	summarise(
		points = length(serving),
		matches = length(unique(match_id)),
		grand_slam = length(unique(match_id[tournament %in% c("Australian Open","US Open","French Open","Wimbledon")]))
)

matches <- unique(atp_points$match_id[atp_points$serving %in% points_played$serving[points_played$matches < 3]])

# Remove if fewer than 3 matches
atp_points <- atp_points[!(atp_points$match_id %in% matches), ]
			
atp_points <- do.call("rbind", lapply(split(atp_points, f = atp_points$match_id), function(obj){

	obj$before_breakpoint <- c(obj$breakpoint[-1], FALSE)

	obj <- obj %>%
		group_by(Set) %>%
		  mutate(
		  	max_tiebreak = max(c(max(serve_points[tiebreak]), max(return_points[tiebreak])))
		  )
		  
	serve_games <- obj %>% 
		group_by(Set, serving) %>% 
		summarise(games = sum(serve_score == "GM" | serve_score == max_tiebreak))

	return_games <- obj %>% 
		group_by(Set, returning) %>% 
		summarise(games = sum(return_score == "GM" | return_score == max_tiebreak))
	
	serve_games$games <- 	serve_games$games + return_games$games
	player <- serve_games$serving[1]

	player_games <- subset(serve_games, serving == player)
	opponent_games <- subset(serve_games, serving != player)
	
	player_games$games <- player_games$games > opponent_games$games
	opponent_games$games <- !player_games$games

	player_games$cumsets <- cumsum(player_games$games)
	opponent_games$cumsets <- cumsum(opponent_games$games)
	
	obj$set_down <- FALSE
	obj$set_up <- FALSE
	
	for(i in 2:max(player_games$Set)){
		if(player_games$cumsets[i-1] - opponent_games$cumsets[i-1] == 1)
			obj$set_up[obj$serving == player & obj$Set == i] <- TRUE
			
		if(player_games$cumsets[i-1] - opponent_games$cumsets[i-1] == -1)
			obj$set_down[obj$serving == player & obj$Set == i] <- TRUE	
			
		if(player_games$cumsets[i-1] - opponent_games$cumsets[i-1] == 1)
			obj$set_down[obj$serving != player & obj$Set == i] <- TRUE
			
		if(player_games$cumsets[i-1] - opponent_games$cumsets[i-1] == -1)
			obj$set_up[obj$serving != player & obj$Set == i] <- TRUE					
	}
	
obj
}))

atp_points$set_up <- as.numeric(atp_points$set_up)
atp_points$set_down <- as.numeric(atp_points$set_down)

atp_points$max_tiebreak[!is.finite(atp_points$max_tiebreak)] <- NA
rownames(atp_points) <- NULL
save(atp_points, file = "~/Project/tennis/iid/data/atp_points.RData")


# Track cumulative sets and games won
atp_points <- do.call("rbind", lapply(split(atp_points, factor(atp_points$match_id)), function(obj){
	
	
	if(any(obj$tiebreak)){
		obj$serve_score[obj$max_tiebreak == obj$serve_points] <- "GM"
		obj$return_score[obj$max_tiebreak == obj$return_points] <- "GM"
	}
	
	serve_game <- obj %>%
		group_by(serving, Game, Set) %>%
		summarise(
			games = sum(serve_score == "GM")
	)
	
	return_game <- obj %>%
		group_by(returning, Game, Set) %>%
		summarise(
			games = sum(return_score == "GM")
	)
	
	names(return_game)[1] <- "player"
	names(serve_game)[1] <- "player"
	games_won <- rbind(return_game, serve_game)
	
	if(any(return_game$Game == 13)){
		
		winners <- rbind(return_game, serve_game)
		winners <- subset(winners, Game == 13 & games == 1)

		players <- unique(return_game$player)
		losers <- winners
		losers$player <- ifelse(losers$player == players[1], players[2], players[1])
		losers$games <- 0
		
		games_won <- subset(games_won, Game != 13)
		games_won <- rbind(games_won, winners, losers)
	}

	games_won <- games_won[order(games_won$Set, games_won$Game,games_won$player),]
	
	games_won <- games_won %>% 
		group_by(Set, player) %>%
		mutate(
			cumgame = cumsum(games)
	)
	
	# if(any(games_won$Game == 13)){ # Check for tiebreaks
		# games_won$cumgame[games_won$Game == 13]	<- games_won$cumgame[games_won$Game == 13] + games_won$games[games_won$Game == 13]
		
		# games_won$max_game[games_won$Game == 13]	<- games_won$max_game[games_won$Game == 13] + games_won$games[games_won$Game == 13]
	# }
	
	games_won <- games_won %>% 
		group_by(Set) %>%
		mutate(
			max_game = max(cumgame),
			won_set = max_game == cumgame
	)
	
	
	sets_won <- games_won %>% 
		group_by(player, Set) %>%
		summarise(
			sets_won = sum(won_set) 
	)
	
	
	sets_won <- sets_won[order(sets_won$Set, sets_won$player),]
	
	# Set cumulative values to correspond to 
	
	sets_won <- sets_won %>% 
		group_by(player) %>%
		mutate(
			cum_sets_won = c(0, cumsum(sets_won)[1:(length(player) - 1)])
	)
	
	games_won <- games_won %>% 
		group_by(player, Set) %>%
		mutate(
			cumgame = c(0, cumgame[1:(length(player) - 1)])
	)
	
	obj$serving_games_won  <- mapply(function(x, game, set){
		subset(games_won, player == x & Game == game & Set == set)$cumgame 
		}, x = obj$serving, game = obj$Game, set = obj$Set)
	
	obj$returning_games_won  <- mapply(function(x, game, set){
		 filter(games_won, player == x, Game == game, Set == set)$cumgame 
		}, x = obj$returning, game = obj$Game, set = obj$Set)
	
	obj$serving_sets_won  <- mapply(function(x,  set){
		subset(sets_won, player == x & Set == set)$cum_sets_won 
		}, x = obj$serving, set = obj$Set)
	
	obj$returning_sets_won  <- mapply(function(x,  set){
		subset(sets_won, player == x & Set == set)$cum_sets_won 
	}, x = obj$returning, set = obj$Set)
	
	
	obj$serve_points_won <- c(0, obj$serve_points[-nrow(obj)])
	obj$return_points_won <- c(0, obj$return_points[-nrow(obj)])
	
obj
 })
)

save(atp_points, file = "~/Project/tennis/iid/data/atp_points_imp.RData")


# Assign importance value
load(file = "~/Project/tennis/iid/data/importance.RData")
source(file = "~/Project/tennis/iid/code/importance.R")

atp_points$bestof3 <- !(atp_points$tournament %in% c("Australian Open","French Open","US Open","Wimbledon"))

atp_points$point_importance <- mapply(importance,
	point_x = atp_points$serve_points_won,
	point_y = atp_points$return_points_won,
	game_x = atp_points$serving_games_won,
	game_y = atp_points$returning_games_won,	
	set_x = atp_points$serving_sets_won,
	set_y = atp_points$returning_sets_won,
	tiebreak = atp_points$tiebreak,
	bestof3 = atp_points$bestof3
)

#save(atp_points, file = "~/Project/tennis/iid/data/atp_points_imp.RData")
