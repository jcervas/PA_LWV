cat(
	"\n
•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
		RUNNING SIMULATIONS . . . . . . . . . . . . . . . . . . . 
•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••\n")
simulation <- function(x, FUN=NULL) 
  {
    if (is.null(FUN)) stop("No Function Found")
    return(do.call(rbind, lapply(x, FUN = FUN )))
  }
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# # SIMULATE 2016 HOUSE FROM COMPOSITE MEASURE
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# n.sims <- 1000
# impute.vote <- default.unc(comp.2016.votes)
# vbar.impute <- mean(impute.vote)
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# vbar.range <- vbar.impute
# sbar.50 <- rep (NA, length(vbar.range))#vector for predicted seats (based on medians)
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# for (j in 1:length(vbar.range)){#loop over intervals of vbar
# 	 vbar <- vbar.range[j]
# 	 sbar <- rep (NA, n.sims) 
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# 	for (i in 1:n.sims){
# 		v.adj2016 <- impute.vote - (house.inc.2016 * incum_advant) - coef(reg.cong.2016)[1]
# 		normvote2016 <- 0.5 + (coef(reg.cong.2016)[2] * (v.adj2016 - 0.5))
# 		locfree2016 <- normvote2016 + (house.inc.2016 * coef(reg.cong.2016)[3])
# 		locfreeNoise2016 <- rnorm(length(locfree2016), locfree2016, sigma) 
# 		withuncs2016 <- default.unc(locfreeNoise2016)
# 		swingfree2016 <- withuncs2016 + vbar.2016 - mean(withuncs2016) #take out swing
# 		v2016 <- swingfree2016 + vbar - mean(swingfree2016)
# 			sbar[i] <- mean(v2016>.5)
# 		}
# sbar.50[j] <- mean(sbar)
# 	}
# sbar.50


# n.sims <- 1000
# house.2016.impute <- default.unc(house.2016.votes)
# vbar.2016 <- mean(house.2016.impute)
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# vbar.range <- round(vbar.2016,2) + seq(-0.1,0.1,0.002)
# sbar.50 <- rep (NA, length(vbar.range))#vector for predicted seats (based on medians)
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# for (j in 1:length(vbar.range)){#loop over intervals of vbar
# 	 vbar <- vbar.range[j]
# 	 sbar <- rep (NA, n.sims) 
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# 	for (i in 1:n.sims){
# 		v.adj2016 <- house.2016.impute - (pahouse.2016$INC * pahouse.2016$incumb_effect)
# 		normvote2016 <- 0.5 + (cong.coef[5,2] * (v.adj2016 - 0.5))
# 		locfree2016 <- normvote2016 + (pahouse.2016$INC * pahouse.2016$incumb_effect)
# 		locfreeNoise2016 <- rnorm(length(locfree2016), locfree2016, sigma) 
# 		withuncs2016 <- default.unc(locfreeNoise2016)
# 		swingfree2016 <- withuncs2016 + vbar.2016 - mean(withuncs2016) #take out swing
# 		v2016 <- swingfree2016 + vbar - mean(swingfree2016)
# 			sbar[i] <- mean(v2016>.5)
# 		}
# sbar.50[j] <- mean(sbar)
# 	}



# Can we make better inferences from precinct level data from past elections to predict future elections -- yes and no.
# There are strong correlations between elections, sometimes as high as 95%, meaning comparisons can be made with high precision.
# Even 5% of error can lead to erroneous predictions, especially in very close elections.
# Simulating that error can help us address the uncertainty that might arsie from changes in turnout levels, incumbency effects, and other varience.
# `How well can we predict?' -- with ample data, we likely can make better inferences. If we were to model election specific characteristics at the precinct level, we could create 
# more precise confidence intervals -- but this may not add much value to making point estimates. If there were very specific patterns are very low levels of aggregate that could well make better predictions, such as demographic makeup, we could almost without doubt improve our projections.
# This is an empirical exercise that won't be tackled in this essay due to the shear magnitude of reserach and data required.
# reg <- lm(default.unc(comp.2016.votes) ~ default.unc(house.2016.votes))
# predict(reg)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••

sim.election <- function(votes=	NULL, center=house.2016.votes, incumbency=NULL, yr=2018, sims=1000, sigma=sigma) {
	if (is.null(sims)) sims <- 1000
			equal.vote <- mean(default.unc(votes)) - mean(default.unc(center))
			sims.year <- new.list(sims)
		for (k in 1:sims)
			{
		sims.year[[k]] <- 
			default.unc(
				rnorm(length(votes), 
					default.unc(votes - equal.vote), 
					sigma))
			}
	return(sims.year)
	}

# SIMULATIONS OF PREVIOUS ELECTIONS TO VALIDATE COMPOSITE MEASURE
house.sims.2016 <- sim.election(votes=house.2016.votes, center=house.2016.votes, yr=2016, sims=1000, sigma=sigma)
# house.sims.2018 <- sim.election(votes=house.2016.votes, center=house.2018.votes, yr=2018, sims=1000, sigma=sigma)
comp.sims.2016 <- sim.election(votes=comp.2016.votes, center=house.2016.votes, yr=2016, sims=1000, sigma=sigma)

comp.sims.2016.seats <- (unlist(lapply(comp.sims.2016, seats)))
comp.sims.2016.votes <- (unlist(lapply(comp.sims.2016, mean)))
# comp.sims.seats.2018 <- (unlist(lapply(comp.sims.2018, seats)))
# comp.sims.votes.2018 <- (unlist(lapply(comp.sims.2018, mean)))
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# CALCULATE GERRYMANDERING STATISTICS FOR HOUSE AND COMP
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
house.sims.2016.list.bias <- lapply(house.sims.2016, seatsvotes)
house.sims.2016.unlist.bias <- do.call(rbind, house.sims.2016.list.bias)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
house.sims.2016.eg <- simulation(house.sims.2016, eg_TP)
house.sims.2016.meanmedian <- simulation(house.sims.2016, meanmedian)
house.sims.2016.declination <- simulation(house.sims.2016, declination)
house.sims.2016.bias <- house.sims.2016.unlist.bias$bias
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
comp.sims.2016.list.bias <- lapply(comp.sims.2016, seatsvotes)
comp.sims.2016.unlist.bias <- do.call(rbind, comp.sims.2016.list.bias)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
comp.sims.2016.eg <- simulation(comp.sims.2016, eg_TP)
comp.sims.2016.meanmedian <- simulation(comp.sims.2016, meanmedian)
comp.sims.2016.declination <- simulation(comp.sims.2016, declination)
comp.sims.2016.bias <- comp.sims.2016.unlist.bias$bias


# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
correct.pred <- function(x) (sum(1 * (find.winner(x) == find.winner(house.2016.votes))) / length(x))
seats.plusminus.one <- function(x) c((seats(x)*length(x)-1)/length(x), seats(x), (seats(x)*length(x)+1)/length(x))
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
		cat(
"Districts Correctly Predicted 2016:
Average number of correctly predicted districts: ",	
			percent(
				mean(do.call(rbind, 
					lapply(comp.sims.2016, correct.pred)))))
	cat(paste("", collapse = "\n"), "\n")
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
		cat(
"Correctly Predicted Partisan Compostion \n",
	percent(sum(1 * (comp.sims.2016.seats %in% seats(house.2016.votes))) / length(comp.sims.2016.seats))
		, "\n")
	cat(paste("", collapse = "\n"), "\n")
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
		cat(
"Simulated 2016 Delegations within one seat of actual \n",
	percent(sum(1 * comp.sims.2016.seats %in% seats.plusminus.one(house.2016.votes)) / length(comp.sims.2016.seats))
		, "\n")
	cat(paste("", collapse = "\n"), "\n")
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# 		cat(
# "Simulated 2018 Delegations Equal Actual \n",
# 	percent(sum(1 * comp.sims.2018.seats %in% seats(house.2018.votes)) / length(comp.sims.2018.seats))
# 		, "\n")
# 	cat(paste("", collapse = "\n"), "\n")
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# 		cat(
# "Simulated 2018 Delegations within one seat of actual \n",
# 	percent(sum(1 * comp.sims.2018.seats %in% seats.plusminus.one(house.2018.votes)) / length(comp.sims.2018.seats))
# 		, "\n")
# 	cat(paste("", collapse = "\n"), "\n")
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# 		cat(
# "Simulated 2018 Mean Seats \n",
# 	percent(mean(comp.sims.2018.seats))
# 		, "\n")
# 	cat(paste("", collapse = "\n"), "\n")
# # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# 		cat(
# "Simulated 2018 Mean Votes \n",
# 	percent(mean(comp.sims.2018.votes))
# 		, "\n")
# 	cat(paste("", collapse = "\n"), "\n")

out.sims.stats <- function(name, x, y) {
return(
	cat("\n",
		" Plan: ", name, "\n",
		"	Mean Seat Share: ", percent(mean(x)), " ", paste0(mean(x)*18, "R-", 18-mean(x)*18, "D"), "\n",
		"	Median Seat Share: ", percent(median(x)), paste0(median(x)*18, "R-", 18-median(x)*18, "D"), "\n",
		"	SD Seat Share: ", percent(sd(x)), "\n",
		"	Mean Vote Share: ", percent(mean(y)), "\n",
		"	SD Vote Share: ", percent(sd(y)), "\n",
		"	Percent of Sims Republican 'Control': ", percent(sum(1 * x>.5)/length(x)), "\n",
		"	Percent of Sims Democratic 'Control': ", percent(sum(1 * x<.5)/length(x)), "\n",
		"	Percent of Sims Tied: ", percent(sum(1 * x==.5)/length(x)), "\n", 
		"•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••", "\n", 
			"\n"))
	}

# SIMULATE MAPS
	head(pa.redist.dta)
	enacted.comp <- composite(pa.redist.dta, "enacted")$COMPOSITE
	court.comp <- composite(pa.redist.dta, "court")$COMPOSITE
	joint.comp <- composite(pa.redist.dta, "joint")$COMPOSITE
	govwolf.comp <- composite(pa.redist.dta, "gov")$COMPOSITE

	enacted.pres <- composite(pa.redist.dta, "enacted")$PRES
	court.pres <- composite(pa.redist.dta, "court")$PRES
	joint.pres <- composite(pa.redist.dta, "joint")$PRES
	govwolf.pres <- composite(pa.redist.dta, "gov")$PRES


enacted.sims.2016 <- sim.election(
	votes=enacted.comp, 
	center=enacted.comp, 
	yr=2016, sims=1000, 
	sigma=sigma)
court.sims.2016 <- sim.election(
	votes=court.comp, 
	center=enacted.comp, 
	yr=2016, sims=1000, 
	sigma=sigma)
joint.sims.2016 <- sim.election(
	votes=joint.comp, 
	center=enacted.comp, 
	yr=2016, 
	sims=1000, 
	sigma=sigma)
govwolf.sims.2016 <- sim.election(
	votes=govwolf.comp, 
	center=enacted.comp, 
	yr=2016, 
	sims=1000, 
	sigma=sigma)

maps.sims.seats.2016 <- new.list(4)
maps.sims.votes.2016 <- new.list(4)
	names(maps.sims.seats.2016) <- c("enacted", "court", "joint", "govwolf")
	names(maps.sims.votes.2016) <- c("enacted", "court", "joint", "govwolf")
		maps.sims.seats.2016$enacted <- (unlist(lapply(enacted.sims.2016, seats)))
		maps.sims.votes.2016$enacted <- (unlist(lapply(enacted.sims.2016, mean)))

		maps.sims.seats.2016$court <- (unlist(lapply(court.sims.2016, seats)))
		maps.sims.votes.2016$court <- (unlist(lapply(court.sims.2016, mean)))

		maps.sims.seats.2016$joint <- (unlist(lapply(joint.sims.2016, seats)))
		maps.sims.votes.2016$joint <- (unlist(lapply(joint.sims.2016, mean)))

		maps.sims.seats.2016$govwolf <- (unlist(lapply(govwolf.sims.2016, seats)))
		maps.sims.votes.2016$govwolf <- (unlist(lapply(govwolf.sims.2016, mean)))


	out.sims.stats("Enacted 2011", maps.sims.seats.2016$enacted, maps.sims.votes.2016$enacted)
	out.sims.stats( "Court court", maps.sims.seats.2016$court, maps.sims.votes.2016$court)
	out.sims.stats( "Joint Legislative", maps.sims.seats.2016$joint, maps.sims.votes.2016$joint)
	out.sims.stats( "Gov. Wolf", maps.sims.seats.2016$govwolf, maps.sims.votes.2016$govwolf)


# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# SIMULATE PLANS USING USING COMPOSITE FOR HYPOTHTICAL TIE -- GERRYMANDERING DATA

# enacted.sims.5050 <- sim.election(
# 	votes=enacted.comp, 
# 	center=0.5, 
# 	yr=5050, 
# 	sims=1000, 
# 	sigma=sigma)
# court.sims.5050 <- sim.election(
# 	votes=court.comp, 
# 	center=0.5, 
# 	yr=5050, 
# 	sims=1000, 
# 	sigma=sigma)
# joint.sims.5050 <- sim.election(
# 	votes=joint.comp, 
# 	center=0.5, 
# 	yr=5050, 
# 	sims=1000, 
# 	sigma=sigma)
# govwolf.sims.5050 <- sim.election(
# 	votes=govwolf.comp, 
# 	center=0.5, 
# 	yr=5050, 
# 	sims=1000, 
# 	sigma=sigma)

# maps.sims.seats.5050 <- new.list(4)
# maps.sims.votes.5050 <- new.list(4)
# 	maps.sims.seats.5050$enacted <- (unlist(lapply(enacted.sims.5050, seats)))
# 	maps.sims.votes.5050$enacted <- (unlist(lapply(enacted.sims.5050, mean)))

# 	maps.sims.seats.5050$court <- (unlist(lapply(court.sims.5050, seats)))
# 	maps.sims.votes.5050$court <- (unlist(lapply(court.sims.5050, mean)))

# 	maps.sims.seats.5050$joint <- (unlist(lapply(joint.sims.5050, seats)))
# 	maps.sims.votes.5050$joint <- (unlist(lapply(joint.sims.5050, mean)))

# 	maps.sims.seats.5050$govwolf <- (unlist(lapply(govwolf.sims.5050, seats)))
# 	maps.sims.votes.5050$govwolf <- (unlist(lapply(govwolf.sims.5050, mean)))




# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# SIMULATE PLANS USING USING COMPOSITE FOR 2016 ELECTION -- GERRYMANDERING DATA
sims.gerry <- function(x) {
	return(list(
	list(simulation(x, eg_TP)),
	list(simulation(x, meanmedian)),
	list(simulation(x, declination)),
	list(lapply(x, bias))
	))
}
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
enacted.sims.2016.list.bias <- lapply(enacted.sims.2016, bias)
enacted.sims.2016.unlist.bias <- do.call(rbind, enacted.sims.2016.list.bias)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
court.sims.2016.list.bias <- lapply(court.sims.2016, bias)
court.sims.2016.unlist.bias <- do.call(rbind, court.sims.2016.list.bias)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
joint.sims.2016.list.bias <- lapply(joint.sims.2016, bias)
joint.sims.2016.unlist.bias <- do.call(rbind, joint.sims.2016.list.bias)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
govwolf.sims.2016.list.bias <- lapply(govwolf.sims.2016, bias)
govwolf.sims.2016.unlist.bias <- do.call(rbind, govwolf.sims.2016.list.bias)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# ENACTED PARTISAN GERRYMANDERING
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
enacted.sims.2016.eg <- simulation(enacted.sims.2016, eg_TP)
enacted.sims.2016.meanmedian <- simulation(enacted.sims.2016, meanmedian)
enacted.sims.2016.declination <- simulation(enacted.sims.2016, declination)
enacted.sims.2016.bias <- enacted.sims.2016.unlist.bias
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# COURT court PARTISAN GERRYMANDERING
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
court.sims.2016.eg <- simulation(court.sims.2016, eg_TP)
court.sims.2016.meanmedian <- simulation(court.sims.2016, meanmedian)
court.sims.2016.declination <- simulation(court.sims.2016, declination)
court.sims.2016.bias <- court.sims.2016.unlist.bias
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# JOINT LEGISLATIVE PARTISAN GERRYMANDERING
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
joint.sims.2016.eg <- simulation(joint.sims.2016, eg_TP)
joint.sims.2016.meanmedian <- simulation(joint.sims.2016, meanmedian)
joint.sims.2016.declination <- simulation(joint.sims.2016, declination)
joint.sims.2016.bias <- joint.sims.2016.unlist.bias
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# GOV. WOLF PARTISAN GERRYMANDERING
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
govwolf.sims.2016.eg <- simulation(govwolf.sims.2016, eg_TP)
govwolf.sims.2016.meanmedian <- simulation(govwolf.sims.2016, meanmedian)
govwolf.sims.2016.declination <- simulation(govwolf.sims.2016, declination)
govwolf.sims.2016.bias <- govwolf.sims.2016.unlist.bias
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••

                               
# gerry.tab <- function(x) {
# 	rbind(
# 		round(mean(get(paste0(x, ".eg"))), d=3),
# 		paste0("(", round(sd(get(paste0(x, ".eg"))), d=3), ")"),
# 		round(mean(get(paste0(x, ".meanmedian"))), d=3),
# 		paste0("(", round(sd(get(paste0(x, ".meanmedian"))), d=3), ")"),
# 		round(mean(get(paste0(x, ".declination"))), d=3),
# 		paste0("(", round(sd(get(paste0(x, ".declination"))), d=3), ")"),
# 		round(mean(get(paste0(x, ".bias"))), d=3),
# 		paste0("(", round(sd(get(paste0(x, ".bias"))), d=3), ")")
# )}

# `quick.sum` <-  function (x) {
#   `qsum` <- 
#     function (set) cbind(
#                       mean(set[!is.na(set)]),
#                       mean(set[!is.na(set)]) - ci(set[!is.na(set)]),
#                       mean(set[!is.na(set)]) + ci(set[!is.na(set)]),
#                       sd(set[!is.na(set)]),
#                       min(set[!is.na(set)]),
#                       quantile(set[!is.na(set)], 0.025),
#                       quantile(set[!is.na(set)], 0.975),                            
#                       max(set[!is.na(set)])
#                       )}


cat("
•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n
	")
