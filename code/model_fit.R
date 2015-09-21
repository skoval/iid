# Modeling effects
library(lme4)

load( file = "~/Project/tennis/iid/data/atp_points.RData")

expit <- function(x) exp(x)/(1+exp(x))

atp_points$serving_match <- paste(atp_points$match_id, atp_points$serving)
atp_points$returning_match <- paste(atp_points$match_id, atp_points$returning)

fit_iid <- glmer(as.numeric(serve_won) ~ 	1 + 
	(1|serving) + (1|returning) + (1|serving_match) + (1|returning_match), 
	data = atp_points, family = binomial)

fit_logit <- glmer(as.numeric(serve_won) ~ tiebreak + 
	breakpoint + 
	before_breakpoint +
	cumpoints + 
	set_up + 
	set_down +
	(1|serving) + (1|returning) + (1|serving_match) + (1|returning_match), 
	data = atp_points, family = binomial)

save(fit_iid, file = "~/Project/tennis/iid/data/model_iid.RData")
save(fit_logit, file = "~/Project/tennis/iid/data/model_noniid.RData")


# Summary table
logit_summary <- summary(fit_logit)$coef
summary_table <- data.frame(
	odds_ratio = exp(logit_summary[,"Estimate"]),
	absolute_effect = ((expit(logit_summary[,"Estimate"] + logit_summary[1,"Estimate"])) - expit(logit_summary[1,"Estimate"]))*100,
	pvalue = logit_summary[,ncol(logit_summary)]
)


# Comparing return and serve performance
plot_data <- data.frame(
    player = rownames(ranef(fit_logit)$serving),
	serve = expit(ranef(fit_logit)$serving[,1]+fixef(fit_logit)[1]) -  expit(fixef(fit_logit)[1]),
	return = expit(ranef(fit_logit)$returning[,1]+fixef(fit_logit)[1]) -  expit(fixef(fit_logit)[1])
)


gg1 <- ggplot(plot_data, aes(y= serve * 100, x = -100 *return, text = player), colour = "green")+
	geom_point(size = 3) +
	geom_hline(yint = 0) +
	geom_vline(xint = 0) +
	geom_text(data = plot_data[plot_data$serve > .05 | plot_data$serve < -.05,], aes(label = player)) +
	scale_y_continuous("SPW Above Average", lim = c(-10, 10)) +
	scale_x_continuous("RPW Above Average", lim = c(-10, 10)) +
	theme_bw() 
	

# Compare points won on serve with their predictions by match as a diagnostic
atp_points$p_noniid <- predict(fit_logit, type = "response")
atp_points$p_iid <- predict(fit_iid, type = "response")

validity_check <- atp_points %>%
	group_by(match_id) %>%
	summarise(
		observed = sum(serve_won),
	    predicted_noniid = sum(p_noniid),
	    predicted_iid = sum(p_iid)
)


save(atp_points, file = "~/Project/tennis/iid/data/atp_points_predicted.RData")
write.table(atp_points, file = "~/Project/tennis/iid/data/atp_points_predicted.csv", 
	row = FALSE, sep = ",")

