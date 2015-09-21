library(dplyr)

load(file = "~/Project/tennis/iid/data/atp_points.RData")

point_summary <- atp_points %>%
	group_by(match_id, serve) %>%
	summarise(
		overall = mean(serve_won),
		set1 = mean(serve_won[Set == 1])	,
		set2 = mean(serve_won[Set == 2])	,
		set3 = mean(serve_won[Set == 3])	,
		set4 = mean(serve_won[Set == 4])	,
		set5 = mean(serve_won[Set == 5])	,
		facing_breakpoint = mean(serve_won[breakpoint]),
		tiebreak = mean(serve_won[tiebreak]),
		total_points = length(serve)
)

diff_overall <- point_summary %>%
	group_by(serve) %>%
	summarise(
		set1 = sum((overall - set1)* total_points, na.rm = TRUE) / sum(total_points[!is.na(set1)]),	
		set2 = sum((overall - set2)* total_points, na.rm = TRUE) / sum(total_points[!is.na(set2)]),
		set3 = sum((overall - set3)* total_points, na.rm = TRUE) / sum(total_points[!is.na(set3)]),
		set4 = sum((overall - set4)* total_points, na.rm = TRUE) / sum(total_points[!is.na(set4)]),
		set5 = sum((overall - set5)* total_points, na.rm = TRUE) / sum(total_points[!is.na(set5)]),	
		facing_breakpoint = sum((overall - facing_breakpoint)* total_points, na.rm = TRUE) / sum(total_points[!is.na(facing_breakpoint)]),	
		tiebreak = sum((overall - tiebreak)* total_points, na.rm = TRUE) / sum(total_points[!is.na(tiebreak)]),
		overall = sum(overall * total_points, na.rm = TRUE) / sum(total_points)
)

