# Modeling effects
library(lme4)
library(dplyr)
library(ggplot2)

load( file = "~/Project/tennis/iid/data/atp_points_imp.RData")

expit <- function(x) exp(x)/(1+exp(x))

atp_points$serving_match <- paste(atp_points$match_id, atp_points$serving)
atp_points$returning_match <- paste(atp_points$match_id, atp_points$returning)

atp_points$before_breakpoint <- 
	((atp_points$return_points_won == 2 & atp_points$serve_points_won <= 2) |
	(atp_points$return_points_won == 3 & atp_points$serve_points_won == 3)) &
	!atp_points$tiebreak
	
# Need to set point-level variables to be one before point outcome
atp_points <- atp_points %>%
	group_by(match_id, Set, Game) %>%
	dplyr::mutate(
		breakpoint = c(FALSE, breakpoint[-length(Game)])
)

# Test ID
obj <- subset(atp_points, match_id == "Men'sAustralianOpen:Fernando Verdasco:Novak Djokovic:2015")

fit_iid <- glmer(as.numeric(serve_won) ~ 	1 + 
	(1|serving) + (1|returning) + (1|serving_match) + (1|returning_match), 
	data = atp_points, family = binomial)

save(fit_iid, file = "~/Project/tennis/iid/data/model_iid.RData")

fit_logit <- glmer(as.numeric(serve_won) ~ tiebreak + 
	breakpoint + 
	before_breakpoint +
	set_up + 
	set_down +
	(1|serving) + (1|returning) + (1|serving_match) + (1|returning_match), 
	data = atp_points, family = binomial)


save(fit_logit, file = "~/Project/tennis/iid/data/model_noniid.RData")

fit_logit_imp <- glmer(as.numeric(serve_won) ~ tiebreak + 
	breakpoint + 
	before_breakpoint +
	set_up + 
	set_down +
	point_importance + 
	(1|serving) + (1|returning) + (1|serving_match) + (1|returning_match), 
	data = atp_points, family = binomial)


save(fit_logit_imp, file = "~/Project/tennis/iid/data/model_noniid_imp.RData")

# Player effects
fit_logit_player <- glmer(as.numeric(serve_won) ~ tiebreak + 
	breakpoint + 
	before_breakpoint +
	set_up + 
	set_down +
	point_importance + 
	(breakpoint | serving) + 
	(breakpoint | returning) + (1|serving_match) + (1|returning_match), 
	data = atp_points, family = binomial)

save(fit_logit_player, file = "~/Project/tennis/iid/data/model_noniid_player.RData")


# Summary table
logit_summary <- summary(fit_logit)$coef

summary_table <- data.frame(
	odds_ratio = exp(logit_summary[,"Estimate"]),
	absolute_effect = ((expit(logit_summary[,"Estimate"] + logit_summary[1,"Estimate"])) - expit(logit_summary[1,"Estimate"]))*100,
	pvalue = logit_summary[,ncol(logit_summary)]
)


logit_summary <- summary(fit_logit_imp)$coef
logit_summary["point_importance","Estimate"] <- logit_summary["point_importance","Estimate"]/10

summary_table_imp <- data.frame(
	odds_ratio = exp(logit_summary[,"Estimate"]),
	absolute_effect = ((expit(logit_summary[,"Estimate"] + logit_summary[1,"Estimate"])) - expit(logit_summary[1,"Estimate"]))*100,
	pvalue = logit_summary[,ncol(logit_summary)]
)

sum_tab <- cbind(
	rbind(summary_table, summary_table_imp["point_importance",]),
	summary_table_imp
)

# Comparing return and serve performance
plot_data <- data.frame(
    player = rownames(ranef(fit_logit)$serving),
	serve = expit(ranef(fit_logit)$serving[,1]+fixef(fit_logit)[1]) -  expit(fixef(fit_logit)[1]),
	return = expit(ranef(fit_logit)$returning[,1]+fixef(fit_logit)[1]) -  expit(fixef(fit_logit)[1])
)

match_deviations <- cbind(
	ranef(fit_logit)$returning_match,
	ranef(fit_logit)$serving_match
)

names(match_deviations) <- c("Return","Serve")
match_deviations$player <- sub("(.*:20[0-9][0-9] )(.*)","\\2",rownames(match_deviations))
match_deviations <- match_deviations %>%
	group_by(player) %>%
	dplyr::sumarise(
		sd_return = sd(Return),
		sd_serve = sd(Serve),
		matches = length(player)	
)

match_deviations <- merge(plot_data, match_deviations, by = "player")

gg1 <- ggplot(plot_data, aes(y= serve * 100, x = -100 *return, text = player), colour = "green")+
	geom_point(size = 3) +
	geom_hline(yint = 0) +
	geom_vline(xint = 0) +
	geom_text(data = plot_data[plot_data$serve > .05 | plot_data$serve < -.05,], aes(label = player)) +
	scale_y_continuous("SPW Above Average", lim = c(-15, 15)) +
	scale_x_continuous("RPW Above Average", lim = c(-15, 15)) +
	theme_bw() 
	

# Compare points won on serve with their predictions by match as a diagnostic
atp_points$p_noniid <- predict(fit_logit, type = "response")
atp_points$p_iid <- predict(fit_iid, type = "response")
atp_points$p_logit_imp <- atp_points$point_importance
atp_points$p_logit_imp[!is.na(atp_points$p_logit_imp)] <- predict(fit_logit_imp, type = "response")

validity_check <- atp_points %>%
	group_by(match_id) %>%
	dplyr::summarise(
		observed = sum(serve_won),
	    predicted_noniid = sum(p_noniid),
	    predicted_iid = sum(p_iid),
	    predicted_iid_imp = sum(p_iid_imp, na.rm = TRUE)
)

# Output predictions for grand slams
var <- c("tiebreak",
			"breakpoint",
			"before_breakpoint",
			"set_up",
			"set_down",
			"serving",
			"returning",
			"serving_match",
			"returning_match")

predict_data <- atp_points[atp_points$tournament == "US Open" & 
	grepl("2014", atp_points$match_id), var]

predict_data <- unique(predict_data)
predict_data$p_noniid <- predict(fit_logit, type = "response", newdata = predict_data)
predict_data$p_iid <- predict(fit_iid, type = "response", newdata = predict_data)
predict_data <- predict_data[order(predict_data$serving, predict_data$returning),]

save(predict_data, file = "~/Project/tennis/iid/data/atp_points_predicted.RData")
write.table(predict_data, file = "~/Project/tennis/iid/data/atp_points_predicted.csv", 
	row = FALSE, sep = ",")


select_players <- atp_points %>%
	group_by(serving) %>%
	dplyr::summarise(
		matches = length(unique(match_id))
	)

plot_data <- data.frame(
    player = rownames(ranef(fit_logit_player)$serving),
	serve = expit(ranef(fit_logit_player)$serving[,1]+fixef(fit_logit_player)[1]) -  expit(fixef(fit_logit_player)[1]),
	breakpoint = expit(ranef(fit_logit_player)$serving[,2]+fixef(fit_logit_player)[1]) -  expit(fixef(fit_logit_player)[1])
)


plot_data <- plot_data[plot_data$player %in% select_players$serving[select_players$matches > 50],]

gg1 <- ggplot(plot_data, aes(y= serve * 100, x = 100 *breakpoint, text = player), colour = "green")+
	geom_point(size = 3) +
	geom_hline(yint = 0) +
	geom_vline(xint = 0) +
	geom_text(data = plot_data[plot_data$serve > .05 | plot_data$serve < -.05,], aes(label = player)) +
	scale_y_continuous("SPW Above Average", lim = c(-15, 15)) +
	scale_x_continuous("Saving BP Above Average", lim = c(-2, 2)) +
	theme_bw() 
	
	
plot_data <- data.frame(
    player = rownames(ranef(fit_logit_player)$returning),
	return = expit(ranef(fit_logit_player)$returning[,1]+fixef(fit_logit_player)[1]) -  expit(fixef(fit_logit_player)[1]),
	breakpoint = expit(ranef(fit_logit_player)$returning[,2]+fixef(fit_logit_player)[1]) -  expit(fixef(fit_logit_player)[1])
)


plot_data <- plot_data[plot_data$player %in% select_players$serving[select_players$matches > 50],]

gg1 <- ggplot(plot_data, aes(y= return * -100, x = -100 *breakpoint, text = player), 
colour = "green")+
	geom_point(size = 3) +
	geom_hline(yint = 0) +
	geom_vline(xint = 0) +
	geom_text(data = plot_data[plot_data$return > .05 | plot_data$return < -.05,], 
	aes(label = player)) +
	scale_y_continuous("RPW Above Average", lim = c(-15, 15)) +
	scale_x_continuous("BP Above Average", lim = c(-1, 1)) +
	theme_bw() 	