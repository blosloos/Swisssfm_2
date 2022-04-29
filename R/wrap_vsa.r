

if(FALSE){


	# define defaults for debugging:	

	
	STP_id = NULL
	STP_id_next = NULL
	STP_amount_inhabitants = NULL
	STP_local_discharge_river = NULL					
	STP_amount_people_local = NULL
	
	compound_load_total = FALSE

	compound_load_per_hospital_bed_and_day = 0

	compound_excreted = 1
	
	add_columns_from_STP_table = c("ARANEXTNR", "LageX", "LageY")

	overwrite = TRUE	
	
	STP_filter_steps = TRUE
	
	use_sep_csv = ","
	write_csv = TRUE
		
}




wrap_vsa <- function(

	STP_table = NULL,							# Must be a data.frame if provided, overwrites all of the below STP_ arguments
	
	STP_scenario_year = 2021,
	STP_reroute = TRUE,							# Reroute STPs until a given STP_scenario_year
	STP_filter_steps = TRUE,					# Filter STP treatment steps until a given STP_scenario_year
	
	STP_id = NULL,
	STP_id_next = NULL,
	STP_amount_inhabitants = NULL,
	STP_local_discharge_river = NULL,			# discharge in river at STP
	STP_treatment_steps = NULL,
	STP_discharge_per_capita = 400,				# [l / E d]
	STP_amount_people_local = NULL,				# amount of people at STP
	
	compound_name,
	compound_load_total = FALSE, 				# [kg / a]
	compound_load_gramm_per_capita_and_day,		# [g / E d], set to FALSE to ignore
	compound_load_per_hospital_bed_and_day = 0,	# [g / E d], set to FALSE to ignore
	compound_elimination_STP = NULL,			# named dataframe or vector with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
	compound_excreted = 1,						# fraction excreted and discharged, set to 1 to ignore
	
	with_lake_elimination = FALSE,
	add_absolute_load = FALSE,
	
	use_columns_local_discharge = "Q347_L_s_kleinster",
	use_columns_local_discharge_for_fractions = "Q347_L_s_kleinster", 
	add_columns_from_STP_table = c("ARANEXTNR", "LageX", "LageY"),
	path_out = FALSE,							# if FALSE, return data.frame
	overwrite = TRUE,
	write_csv = TRUE,							# else, exports an excel file
	use_sep_csv = " "
	
){

	###############################################
	# -> if several values, check if equal numbers available, if either one or two values, 
	# and if smaller/larger values are set correctly
	if(!identical(
		length(compound_load_gramm_per_capita_and_day),
		length(use_columns_local_discharge),
		nrow(compound_elimination_STP)
	)) stop("compound_load_gramm_per_capita_and_day, use_columns_local_discharge and the number of rows in compound_elimination_STP must be of equal length")	
	if(!(length(compound_load_gramm_per_capita_and_day) %in% c(1, 2))) stop("compound_load_gramm_per_capita_and_day, use_columns_local_discharge and the number of rows in compound_elimination_STP must have either one or two entries")
	if(length(compound_load_gramm_per_capita_and_day) == 2){
		if(compound_load_gramm_per_capita_and_day[1] < compound_load_gramm_per_capita_and_day[2]) stop("compound_load_gramm_per_capita_and_day set incorrectly for range calculation")
		if(any(compound_elimination_STP[1, ] > compound_elimination_STP[2, ])) stop("compound_elimination_STP set incorrectly for range calculation")
		if(any(STP_table[, use_columns_local_discharge[1]] > STP_table[, use_columns_local_discharge[2]], na.rm = TRUE)) stop("use_columns_local_discharge set incorrectly for range calculation")	
	}
	###############################################
	# -> if available, get all inputs from STP_table
	if(!is.numeric(STP_scenario_year)) stop("STP_scenario_year must be numeric")
	if(!is.null(STP_table) & !is.data.frame(STP_table)) stop("STP_table must be either NULL or a dataframe")
	
	###############################################
	if(!is.null(STP_table) & is.data.frame(STP_table)){
			
		# reroute
		if(STP_reroute){
		
			those <- which(
				(STP_table[, "Typ_MV-Behandlung"] == "Umleitung") & 
				(as.numeric(STP_table[, "Inbetriebnahme"]) <= STP_scenario_year)
			)
			if(length(those)){
				for(i in those){ # rewrite angeschlossene_Einwohner_Abgabeliste2021
				
					to_STP <- STP_table[i, "ARA_Nr_Ziel_Umleitung"] 	# per ID
					if(!(to_STP %in% STP_table$ARA_Nr)) stop(paste0("Invalid ARA_Nr_Ziel_Umleitung for STP ", STP_table[i, "ARA_Nr"]))
					to_STP <- which(STP_table[, "ARA_Nr"] == to_STP) 	# per table position
					if(!is.na(STP_table[to_STP, "ARA_Nr_Ziel_Umleitung"])) stop(paste0("Invalid ARA_Nr_Ziel_Umleitung for STP ", STP_table[i, "ARA_Nr"], ": rerouted STP is rerouted itself."))
					has_STP_amount_people_local <- STP_table[i, "angeschlossene_Einwohner_Abgabeliste2021"]
					STP_table[to_STP, "angeschlossene_Einwohner_Abgabeliste2021"] <- STP_table[to_STP, "angeschlossene_Einwohner_Abgabeliste2021"] + has_STP_amount_people_local
				
				}
				
				STP_table_rerouted <- STP_table[those,, drop = FALSE]
				STP_table <- STP_table[-those,, drop = FALSE]
			
			}else STP_table_rerouted <- NULL
		
		}
			
		# extract data from table
		STP_id <- as.character(STP_table$ARA_Nr)
		STP_id_next <- as.character(STP_table$ARANEXTNR)
		STP_amount_inhabitants <- as.numeric(gsub(".", "", as.character(STP_table$angeschlossene_Einwohner_Abgabeliste2021), fixed = TRUE))
		# STP_local_discharge_river <- as.numeric(STP_table[, use_columns_local_discharge_loop]) # -> inside and after loop below
		STP_amount_people_local <- STP_table$angeschlossene_Einwohner_Abgabeliste2021
		
		if(with_lake_elimination){
			if(!("See_Elimination_min" %in% names(STP_table))) stop("Column See_Elimination_min missing in STP_table")
			if(!("See_Elimination_max" %in% names(STP_table))) stop("Column See_Elimination_max missing in STP_table")			
		}
		if(add_absolute_load) if(!("Absolute_Fracht_add" %in% names(STP_table))) stop("Column Absolute_Fracht_add missing in STP_table")
		
		# get & clean treatment steps
		
		miss_col <- which(!(c("Nitrifikation", "Denitrifikation", "Erhoehte_Denitrifikation", "P_Elimination", "Typ_MV-Behandlung", "Inbetriebnahme") %in% names(STP_table)))
		if(length(miss_col)){
			stop(
				paste0("Column(s) withe name ", 
					paste(c("Nitrifikation", "Denitrifikation", "Erhoehte_Denitrifikation", "P_Elimination", "Typ_MV-Behandlung", "Inbetriebnahme"))[miss_col], 
					" missing in STP_table")
			)
		}
		
		STP_treatment_steps <- STP_table[, c("Nitrifikation", "Denitrifikation", "Erhoehte_Denitrifikation", "P_Elimination", "Typ_MV-Behandlung", "Inbetriebnahme"), drop = FALSE]
		STP_treatment_steps[is.na(STP_treatment_steps[, "Nitrifikation"]), "Nitrifikation"] <- "Nein"	
		STP_treatment_steps[is.na(STP_treatment_steps[, "Denitrifikation"]), "Denitrifikation"] <- "Nein"
		STP_treatment_steps[is.na(STP_treatment_steps[, "Erhoehte_Denitrifikation"]), "Erhoehte_Denitrifikation"] <- "Nein"		
		STP_treatment_steps[is.na(STP_treatment_steps[, "P_Elimination"]), "P_Elimination"] <- "Nein"
		STP_treatment_steps[STP_treatment_steps[, "Typ_MV-Behandlung"] %in% c("Umleitung", "Umleitung wahrscheinlich"), "Typ_MV-Behandlung"] <- NA
		if(STP_filter_steps) STP_treatment_steps[which(STP_treatment_steps[, "Inbetriebnahme"] > STP_scenario_year), "Typ_MV-Behandlung"] <- NA
		
	}else{
		
		if(is.null(STP_id)) stop("For STP_table = NULL, STP_id must be provided as function argument.")
		if(is.null(STP_id_next)) stop("For STP_table = NULL, STP_id_next must be provided as function argument.")	
		if(is.null(STP_amount_inhabitants)) stop("For STP_table = NULL, STP_amount_inhabitants must be provided as function argument.")
		if(is.null(STP_treatment_steps)) stop("For STP_table = NULL, STP_treatment_steps must be provided as function argument.")
		if(is.null(STP_local_discharge_river)) stop("For STP_table = NULL, STP_local_discharge_river must be provided as function argument.")
		if(is.null(STP_amount_people_local)) stop("For STP_table = NULL, STP_amount_people_local must be provided as function argument.")
		
		#if(is.null()) stop("For STP_table = NULL,  must be provided as function argument.")
	
	}
	###########################################
	# check inputs & defaults #################
	if(!is.numeric(STP_amount_inhabitants)) stop("Problem in wrap_vsa: STP_amount_inhabitants must be numeric.")
	if(!identical(length(STP_id), length(STP_id_next), length(STP_amount_inhabitants))) stop("Problem in wrap_vsa: STP_id, STP_id_next and STP_amount_inhabitants must be of equal length.")
	if(!overwrite & !is.logical(path_out)) if(file.exists(path_out)) stop("Problem in wrap_vsa: file at path_out already exists; remove it or use overwrite = TRUE.")
	if(!file.exists(path_out)) dir.create(path = path_out)	
	
	
	###########################################
	# calculate topology matrix ###############
	topo_matrix <- make_topology(
	
		STP_id_next = STP_id_next, 					# NA if none available
		STP_id = STP_id,					
		NA_next_ignore = FALSE,						# ara_id_next not in STP_id? -> set to NA as well
		insert_id_in_topo_matrix = FALSE
	
	)
	if(!sum(topo_matrix)) stop("Problem in wrap_vsa: no relations between STPs found.")

	if(!is.logical(path_out)){

		done_write <- try({
		
			topo_matrix_export <- topo_matrix
			# because otherwise the fuck doesn't work and shifts the colnames one cell to the left
			topo_matrix_export <- rbind(rep("", ncol(topo_matrix_export)), topo_matrix_export)		
			topo_matrix_export[1, ] <- colnames(topo_matrix)
			topo_matrix_export <- cbind(rep("", nrow(topo_matrix_export)), topo_matrix_export)			
			topo_matrix_export[, 1] <- c("", rownames(topo_matrix))
		
			write.table(topo_matrix_export, file = file.path(path_out, paste0("topo_matrix", ".csv")), append = FALSE, quote = TRUE, sep = use_sep_csv, 
				row.names = FALSE, 
				col.names = FALSE
			)
			
		})
		if(class(done_write) == "try-error") stop("Export of results to path_out.csv failed. Is this path valid? Is the file open in another software?")
	
	}
	
	###########################################	
	
	store_results <- vector("list", nrow(compound_elimination_STP))
	for(n in 1:nrow(compound_elimination_STP)){
			
		###########################################
		# get inputs per loop
		# all required columns available?
		
		use_columns_local_discharge_loop <- use_columns_local_discharge[n]
		
		cols_required <- c(
			"ARA_Nr", "ARANEXTNR", "angeschlossene_Einwohner_Abgabeliste2021", 
			"Nitrifikation", "Denitrifikation", "Erhoehte_Denitrifikation", "P_Elimination", "Typ_MV-Behandlung", "Inbetriebnahme",
			"ARA_Nr_Ziel_Umleitung", use_columns_local_discharge_loop, "See_Elimination_min", "See_Elimination_max"
		)
		if(any(is.na(match(cols_required, names(STP_table))))){
			these_missing <- paste(cols_required[is.na(match(cols_required, names(STP_table)))], collapse = ",")
			stop(paste0("STP_table is missing these columns: ", these_missing))
		}
		
		STP_local_discharge_river_loop <- as.numeric(STP_table[, use_columns_local_discharge_loop])


		compound_load_gramm_per_capita_and_day_loop <- compound_load_gramm_per_capita_and_day[n]
		
		if(with_lake_elimination){		
			if(n == 1) lake_eliminination_rates_loop <- as.numeric(STP_table$See_Elimination_min)
			if(n == 2) lake_eliminination_rates_loop <- as.numeric(STP_table$See_Elimination_max)
		}else lake_eliminination_rates_loop <- rep(0, nrow(STP_table))
		
		compound_elimination_STP_loop <- compound_elimination_STP[n,, drop = FALSE]
	
		if(add_absolute_load) absolute_loads_loop <- STP_table$Absolute_Fracht_add else absolute_loads_loop <- rep(0, nrow(STP_table))	
		
		
		
		###########################################
		# calculate local and cumulative loads ####
	
		if(FALSE){
		
			inhabitants_total = sum(STP_amount_inhabitants)
			hospital_beds_total = FALSE
			STP_id = STP_id
			STP_fraction_hospital = FALSE
			STP_amount_inhabitants = STP_amount_inhabitants	
			STP_amount_hospital_beds = FALSE
			compound_load_total = FALSE
			compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day_loop
			compound_load_per_hospital_bed_and_day = compound_load_per_hospital_bed_and_day
			compound_elimination_STP = compound_elimination_STP_loop
			compound_excreted = 1
			lake_eliminination_rates = lake_eliminination_rates_loop
			absolute_loads = absolute_loads_loop
			ARANEXTNR = STP_table$ARANEXTNR
	
			
		}
		
		result_table <- run_daily_load(

			inhabitants_total = sum(STP_amount_inhabitants),
			hospital_beds_total = FALSE,								# Set to FALSE to ignore
			STP_id = STP_id,
			STP_treatment_steps = STP_treatment_steps,
			STP_fraction_hospital = FALSE,
			STP_amount_inhabitants = STP_amount_inhabitants,	
			STP_amount_hospital_beds = FALSE,							# Set to FALSE to ignore
			compound_load_total = FALSE, 								# [kg / a]
			compound_load_gramm_per_capita_and_day = compound_load_gramm_per_capita_and_day_loop,		# [g / E d], set to FALSE to ignore
			compound_load_per_hospital_bed_and_day = compound_load_per_hospital_bed_and_day,
			compound_elimination_STP = compound_elimination_STP_loop,	# vector or STP-specific matrix with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
			compound_excreted = 1,										# fraction excreted and discharged, set to 1 to ignore
			
			with_lake_elimination = with_lake_elimination,
			lake_eliminination_rates = lake_eliminination_rates_loop,	

			add_absolute_load = add_absolute_load,
			absolute_loads = absolute_loads_loop,
			
			topo_matrix,
			ARANEXTNR = STP_table$ARANEXTNR
			
		) # [g / d]
		###########################################
		# concentration ###########################
		result_table <- cbind(result_table, 
			"conc_local_ug_L" =  (result_table$load_local / (24 * 60 * 60)) * 1E6 / STP_local_discharge_river_loop, 			# ng / L  , load: g/d  discharge: Q347_L_s_kleinster
			"conc_cumulated_ug_L" = (result_table$load_cumulated / (24 * 60 * 60)) * 1E6 / STP_local_discharge_river_loop
		)
		###########################################		
		store_results[[n]] <- result_table
		
	}

	if(nrow(compound_elimination_STP) == 2){
	
		names(store_results[[1]]) <- c("STP_ID", "load_local_g_d_max", "load_cumulated_g_d_max", "inhabitants_cumulated", "STP_count_cumulated", "conc_local_ug_L_max", "conc_cumulated_ug_L_max")
		names(store_results[[2]]) <- c("STP_ID", "load_local_g_d_min", "load_cumulated_g_d_min", "inhabitants_cumulated", "STP_count_cumulated", "conc_local_ug_L_min", "conc_cumulated_ug_L_min")
	
		result_table <- cbind(
			store_results[[1]][, c("STP_ID", "load_local_g_d_max", "load_cumulated_g_d_max", "inhabitants_cumulated", "STP_count_cumulated", "conc_local_ug_L_max", "conc_cumulated_ug_L_max")],
			store_results[[2]][, c("load_local_g_d_min", "load_cumulated_g_d_min", "conc_local_ug_L_min", "conc_cumulated_ug_L_min")]	
		)
	
	}else result_table <- store_results[[1]]
	
	###############################################
	# STP_discharge_L_s	
	result_table <- cbind(result_table, 
		"STP_discharge_L_s" = (STP_amount_inhabitants * STP_discharge_per_capita / (24 * 60 * 60))
	)
	###############################################
	# fraction STP discharge ######################
	sewage_discharge_local <- STP_amount_people_local * STP_discharge_per_capita / 24 / 60 / 60 				# convert to [l/s]
	STP_amount_people_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local)
	sewage_discharge_cumulated <- STP_amount_people_cumulated * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]
	
	STP_local_discharge_river <- as.numeric(STP_table[, use_columns_local_discharge_for_fractions])
	
	Discharge_ratio_river_to_STP_local <- STP_local_discharge_river / sewage_discharge_local
	Discharge_ratio_river_to_STP_cumulated <- STP_local_discharge_river / sewage_discharge_cumulated
	
	Fraction_STP_discharge_of_river_local <- sewage_discharge_local / ( STP_local_discharge_river + sewage_discharge_local )
	Fraction_STP_discharge_of_river_cumulated <- sewage_discharge_cumulated / ( STP_local_discharge_river + sewage_discharge_local )	
	
	result_table <- cbind(result_table, 
		"Discharge_ratio_river_to_STP_local" = Discharge_ratio_river_to_STP_local,
		"Discharge_ratio_river_to_STP_cumulated" = Discharge_ratio_river_to_STP_cumulated,
		"Fraction_STP_discharge_of_river_local" = Fraction_STP_discharge_of_river_local,
		"Fraction_STP_discharge_of_river_cumulated" = Fraction_STP_discharge_of_river_cumulated
	)
	
	###############################################
	# fraction sewage per upstream treatment step
	# -> Test further below: rowsum für Anteile muss 1 ergeben
	
	classed <- rep(NA, nrow(STP_treatment_steps))
	classed[
		(STP_treatment_steps[, "Nitrifikation"] == "Nein") & (STP_treatment_steps[, "Denitrifikation"] == "Nein") & is.na(STP_treatment_steps[, "Typ_MV-Behandlung"])
	] <- "nur_C_Abbau"
	classed[
		(STP_treatment_steps[, "Nitrifikation"] == "Ja") & (STP_treatment_steps[, "Denitrifikation"] == "Nein") & is.na(STP_treatment_steps[, "Typ_MV-Behandlung"])
	] <- "Nitrifikation"	
	classed[
		(STP_treatment_steps[, "Nitrifikation"] == "Ja") & (STP_treatment_steps[, "Denitrifikation"] == "Ja") & is.na(STP_treatment_steps[, "Typ_MV-Behandlung"])
	] <- "Denitrifikation"		
	classed[
		!is.na(STP_treatment_steps[, "Typ_MV-Behandlung"])
	] <- "MV_Behandlung"			
	classed[is.na(classed)] <- "Sonstige"
	
	# nur_C_Abbau
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "nur_C_Abbau"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_only_C_removal <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	
	# Nitrifikation
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "Nitrifikation"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_nitrification <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	
	# Denitrifikation
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "Denitrifikation"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_denitrification <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)

	# MV_Behandlung
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "MV_Behandlung"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_advanced_treatment <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	
	# Sonstige
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed != "Sonstige"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_Sonstige <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	
	# keine MV_Behandlung
	STP_amount_people_local_classed <- STP_amount_people_local
	STP_amount_people_local_classed[classed == "MV_Behandlung"] <- 0
	STP_amount_people_cumulated_classed <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y, na.rm = TRUE)}, y = STP_amount_people_local_classed)
	sewage_discharge_cumulated_classed <- STP_amount_people_cumulated_classed * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]	
	Fraction_of_wastewater_no_advanced_treatment <- round(sewage_discharge_cumulated_classed / sewage_discharge_cumulated, digits = 3)
	Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated <- round(STP_local_discharge_river / sewage_discharge_cumulated_classed, digits = 3)	
	
	#STP_treatment_steps[is.na(classed), ]
	#STP_id[is.na(classed)]
	
	# -> Test: rowsum für Anteile muss 1 ergeben
	if(FALSE) if(any(rowSums(cbind(
			"Fraction_of_wastewater_only_C_removal" = Fraction_of_wastewater_only_C_removal,
			"Fraction_of_wastewater_nitrification" = Fraction_of_wastewater_nitrification,
			"Fraction_of_wastewater_denitrification" = Fraction_of_wastewater_denitrification,
			"Fraction_of_wastewater_advanced_treatment" = Fraction_of_wastewater_advanced_treatment
		)) != 1)) stop("Missing treatment fractions in wrap_vsa - revise")
	
	result_table <- cbind(result_table, 
		"Fraction_of_wastewater_only_C_removal" = Fraction_of_wastewater_only_C_removal,
		"Fraction_of_wastewater_nitrification" = Fraction_of_wastewater_nitrification,
		"Fraction_of_wastewater_denitrification" = Fraction_of_wastewater_denitrification,
		"Fraction_of_wastewater_advanced_treatment" = Fraction_of_wastewater_advanced_treatment
	)
	
	Fraction_of_river_only_C_removal <- Fraction_of_wastewater_only_C_removal * Fraction_STP_discharge_of_river_cumulated
	Fraction_of_river_nitrification <- Fraction_of_wastewater_nitrification * Fraction_STP_discharge_of_river_cumulated
	Fraction_of_river_denitrification <-  Fraction_of_wastewater_denitrification * Fraction_STP_discharge_of_river_cumulated
	Fraction_of_river_advanced_treatment <- Fraction_of_wastewater_advanced_treatment * Fraction_STP_discharge_of_river_cumulated	
	Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated <- Fraction_of_wastewater_no_advanced_treatment * Fraction_STP_discharge_of_river_cumulated	
	
	
	result_table <- cbind(result_table, 
		"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated" = Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated,
		"Fraction_of_river_only_C_removal" = Fraction_of_river_only_C_removal,
		"Fraction_of_river_nitrification" = Fraction_of_river_nitrification,
		"Fraction_of_river_denitrification" = Fraction_of_river_denitrification,
		"Fraction_of_river_advanced_treatment" = Fraction_of_river_advanced_treatment,
		"Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated" = Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated
	)	
	
	###############################################
	# format, export & return #####################	
	
	if(nrow(compound_elimination_STP) == 2){
	
		# -> reorder table
		result_table <- result_table[, c( 
			"STP_ID",
			"inhabitants_cumulated",
			"STP_discharge_L_s",
			"STP_count_cumulated",
			"Discharge_ratio_river_to_STP_local",
			"Discharge_ratio_river_to_STP_cumulated",
			"Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated",
			"Fraction_STP_discharge_of_river_local",
			"Fraction_STP_discharge_of_river_cumulated",
			"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated",
			"Fraction_of_river_only_C_removal",
			"Fraction_of_river_nitrification",
			"Fraction_of_river_denitrification",
			"Fraction_of_river_advanced_treatment",
			"Fraction_of_wastewater_only_C_removal",
			"Fraction_of_wastewater_nitrification",
			"Fraction_of_wastewater_denitrification",
			"Fraction_of_wastewater_advanced_treatment",
			"load_local_g_d_max",
			"load_local_g_d_min",
			"load_cumulated_g_d_max",
			"load_cumulated_g_d_min",
			"conc_local_ug_L_max",
			"conc_local_ug_L_min",			
			"conc_cumulated_ug_L_max",
			"conc_cumulated_ug_L_min"			
		), drop = FALSE]	
	
	}else{
	
		# -> reorder table
		result_table <- result_table[, c( 
			"STP_ID",
			"inhabitants_cumulated",
			"STP_discharge_L_s",
			"STP_count_cumulated",
			"Discharge_ratio_river_to_STP_local",
			"Discharge_ratio_river_to_STP_cumulated",
			"Discharge_ratio_river_to_STP_without_advanced_treatment_of_river_cumulated",
			"Fraction_STP_discharge_of_river_local",
			"Fraction_STP_discharge_of_river_cumulated",
			"Fraction_STP_discharge_without_advanced_treatment_of_river_cumulated",
			"Fraction_of_river_only_C_removal",
			"Fraction_of_river_nitrification",
			"Fraction_of_river_denitrification",
			"Fraction_of_river_advanced_treatment",
			"Fraction_of_wastewater_only_C_removal",
			"Fraction_of_wastewater_nitrification",
			"Fraction_of_wastewater_denitrification",
			"Fraction_of_wastewater_advanced_treatment",
			"load_local_g_d",
			"load_cumulated_g_d",
			"conc_local_ug_L",
			"conc_cumulated_ug_L"
		), drop = FALSE]
	
	}
	
if(FALSE){ # <- remove this check
	
	
	load_cumulated_g_d_max_check <- result_table$inhabitants_cumulated * compound_load_gramm_per_capita_and_day[1]	
	result_table$STP_ID[result_table$load_cumulated_g_d_max != load_cumulated_g_d_max_check]
	
	
	cbind(result_table$load_cumulated_g_d_max, load_cumulated_g_d_max_check)[
		result_table$load_cumulated_g_d_max != load_cumulated_g_d_max_check
	, ]
	
	
	result_table_inter$load_cumulated_g_d_max != result_table$load_cumulated_g_d_max
	
	ID <- result_table$STP_ID[
		abs(result_table$load_cumulated_g_d_max - load_cumulated_g_d_max_check) > .001
	]
	
	
	
}
	
	if(is.logical(path_out)) return(result_table) else{
		if(file.exists(file.path(path_out, paste0("STP_result_", compound_name, ".csv"))) & !overwrite) stop("File at path_out already exists, and overwrite is set to FALSE")
	
		# add more STP infos to result_table
		use_cols <- match(add_columns_from_STP_table, names(STP_table))
		use_rows <- match(STP_table[, "ARA_Nr"], result_table[, "STP_ID"])
		result_table <- cbind(
			"STP_ID" = result_table[, "STP_ID"], 
			STP_table[use_rows, use_cols], 
			result_table[, names(result_table) != "STP_ID"]
		)
		result_table <- rbind(
			#rep("", ncol(result_table)),
			rep("", ncol(result_table)),
			rep("", ncol(result_table)),
			rep("", ncol(result_table)),
			#rep("", ncol(result_table)),
			#rep("", ncol(result_table)),
			names(result_table),
			result_table
		)
		names(result_table) <- NULL
		
		result_table[2, 2] <- "Compound name:"
		result_table[3, 2] <- compound_name	
		
		result_table[2, 3] <- "Compound load [g/In d]):"
		result_table[3, 3] <- paste(compound_load_gramm_per_capita_and_day, collapse = ", ")
		
		result_table[2, 5] <- "Szenario Jahr:"
		result_table[3, 5] <- STP_scenario_year
		
		result_table[1, 7] <- "Elimitationsraten"
		result_table[2, 7] <- "Nitrifikation:"
		result_table[3, 7] <- paste(compound_elimination_STP$Nitrifikation, collapse = ", ")
		result_table[2, 8] <- "Denitrifikation:"
		result_table[3, 8] <- paste(compound_elimination_STP$Denitrifikation, collapse = ", ")
		result_table[2, 9] <- "P_Elimination:"
		result_table[3, 9] <- paste(compound_elimination_STP$P_Elimination, collapse = ", ")
		result_table[2, 10] <- "GAK:"
		result_table[3, 10] <- paste(compound_elimination_STP$GAK, collapse = ", ")
		result_table[2, 11] <- "Kombi:"
		result_table[3, 11] <- paste(compound_elimination_STP$Kombi, collapse = ", ")
		result_table[2, 12] <- "Ozonung:"
		result_table[3, 12] <- paste(compound_elimination_STP$Ozonung, collapse = ", ")
		result_table[2, 13] <- "PAK:"
		result_table[3, 13] <- paste(compound_elimination_STP$PAK, collapse = ", ")
		result_table[2, 14] <- "Ausbau:"
		result_table[3, 14] <- paste(compound_elimination_STP$Ausbau, collapse = ", ")
		result_table[2, 15] <- "CSB_Abbau"
		result_table[3, 15] <- paste(compound_elimination_STP$CSB_Abbau, collapse = ", ")		
		
		result_table[1, 17] <- "Parameter" 		
		result_table[2, 17] <- "Umleitung aktiv?"
		result_table[3, 17] <- as.character(STP_reroute)
		
		result_table[2, 18] <- "Filterung treatment steps?"
		result_table[3, 18] <- as.character(STP_filter_steps)
		
		result_table[2, 19] <- "Elimination Seen aktiv?"
		result_table[3, 19] <- paste(with_lake_elimination, collapse = ", ")	
		
		result_table[2, 20] <- "use_columns_local_discharge"		
		result_table[3, 20] <- paste(use_columns_local_discharge, collapse = ", ")	
		
		result_table[2, 21] <- "use_columns_local_discharge_for_fractions"		
		result_table[3, 21] <- paste(use_columns_local_discharge_for_fractions, collapse = ", ")			
		

		if(write_csv){
		
			done_write <- try({
			
				write.table(result_table, file = file.path(path_out, paste0("STP_result_", compound_name, ".csv")), append = FALSE, quote = TRUE, sep = use_sep_csv, row.names = FALSE)
			
			})
			if(class(done_write) == "try-error") stop("Export of results to path_out.csv failed. Is this path valid? Is the file open in another software?")
		
		}else{
		
			done_write <- try({
			
				wb <- openxlsx:::createWorkbook()	
				openxlsx:::addWorksheet(wb, compound_name)
				openxlsx:::writeData(wb, compound_name, result_table, startCol = 2, startRow = 3, rowNames = FALSE)
				openxlsx:::saveWorkbook(wb, file = file.path(path_out, paste0("STP_result_", compound_name, ".xlsx")), overwrite = TRUE)
			
			})
			if(class(done_write) == "try-error") stop("Export of results to path_out.xlsx failed. Is this path valid? Is the file open in another software?")
	
		}
	
	}
	###############################################	
	
}



