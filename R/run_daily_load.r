






run_daily_load <- function( # one function run per compound

	inhabitants_total = 8417700,
	hospital_beds_total = FALSE,						# Set to FALSE to ignore
	STP_id,
	STP_treatment_steps,
	STP_fraction_hospital= FALSE,
	STP_amount_inhabitants,	
	STP_amount_hospital_beds = FALSE,					# Set to FALSE to ignore
	compound_load_total, 								# [kg / a], set to FALSE to ignore and then use compound_load_gramm_per_capita_and_day
	compound_load_gramm_per_capita_and_day = FALSE,		# [g / E d], set to FALSE to ignore and then use compound_load_total
	compound_load_per_hospital_bed_and_day = FALSE, 	# [g / E d], set to FALSE to ignore and then use compound_load_total
	compound_elimination_STP,							# named dataframe or vector with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
	compound_elimination_method = "micropollutants",	# "micropollutants" or "nutrients"
	compound_excreted = 1,								# fraction excreted and discharged, set to 1 to ignore
	
	with_lake_elimination = FALSE,
	lake_eliminination_rates,
	
	add_absolute_load = FALSE,
	absolute_loads = FALSE,
	
	topo_matrix,
	ARANEXTNR
	
){

	###############################################
	# check inputs & defaults
	
	if(!is.data.frame(compound_elimination_STP) & is.vector(compound_elimination_STP)) stop("Problem in wrap_vsa, argument compound_elimination_STP is not a dataframe.")
	if(any(!sapply(compound_elimination_STP, is.numeric))) stop("Problem in wrap_vsa, dataframe compound_elimination_STP has non-numeric entries.")
	STP_steps <- c("CSB_Abbau", "Nitrifikation", "Denitrifikation", "P_Elimination", "GAK", "Kombi", "Ozonung", "PAK", "Ausbau")
	not_found <- !(STP_steps %in% names(compound_elimination_STP))
	if(any(not_found)) stop("Problem in wrap_vsa, argument compound_elimination_STP: entry ", compound_elimination_STP[not_found], " is missing.")
	if(any((compound_elimination_STP < 0) & (compound_elimination_STP > 1))) stop("Problem in run_daily_load: compound_elimination_STP not within [0,1]")
	if(!(compound_elimination_method %in% c("micropollutants", "nutrients"))) stop("Problem in run_daily_load: invalid compound_elimination_method, must be either micropollutants or nutrients.")
	
	if(!all(STP_id %in% colnames(topo_matrix))) stop("Problem in run_daily_load: not all STP_id present in topo_matrix")
	if(!all(colnames(topo_matrix) %in% STP_id)) stop("Problem in run_daily_load: not all topo_matrix entries present in STP_id")
	if(is.logical(STP_fraction_hospital)) if(!isTRUE(STP_fraction_hospital)) STP_fraction_hospital <- rep(0, length(STP_id))
	if(!identical(length(STP_id), length(STP_fraction_hospital), length(STP_amount_inhabitants))) stop("Problem in run_daily_load: STP inputs vary in length_1")
	if(is.logical(STP_amount_hospital_beds)) if(!isTRUE(STP_amount_hospital_beds)) STP_amount_hospital_beds <- 0 else if(length(STP_amount_hospital_beds) != length(STP_id)) stop("Problem in run_daily_load: STP inputs vary in length_2")
	if(is.logical(hospital_beds_total)) if(!isTRUE(hospital_beds_total)) hospital_beds_total <- 0
	if(length(compound_load_total) > 1) stop("Problem in run_daily_load: compound_load_total should consist of one value only")
	if(length(compound_load_gramm_per_capita_and_day) > 1) stop("Problem in run_daily_load: compound_load_gramm_per_capita_and_day should consist of one value only")
	if(!is.numeric(compound_load_total) & !is.numeric(compound_load_gramm_per_capita_and_day)) stop("Problem in run_daily_load: either compound_load_total or compound_load_gramm_per_capita_and_day must be defined.")
	topo_matrix[topo_matrix != 0] <- 1 	# in case topo_matrix contains STP id

	if(!all(ARANEXTNR[!is.na(ARANEXTNR)] %in% STP_id)) stop("Invalid ARANEXTNR entries detected.")

	###############################################
	if(!is.numeric(compound_load_gramm_per_capita_and_day)) compound_load_gramm_per_capita_and_day <- compound_load_total * (1 - STP_fraction_hospital) * 1000 / inhabitants_total / 365 		# [kg/a] -> [g/d]
	if(!is.numeric(compound_load_per_hospital_bed_and_day)) compound_load_per_hospital_bed_and_day <- compound_load_total * (STP_fraction_hospital) * 1000 / hospital_beds_total / 365 			# [kg/a] -> [g/d]
	if(is.vector(compound_elimination_STP)) compound_elimination_STP_calc <- prod(1 - compound_elimination_STP) # prod: if several steps combined
	if(is.data.frame(compound_elimination_STP)){
	
		compound_elimination_STP_calc <- rep(0, nrow(STP_treatment_steps))
		for(i in 1:nrow(STP_treatment_steps)){

			#######################################
			if(compound_elimination_method == "micropollutants"){
			
				# STP, See <- all STP_treatment_steps set to "Nein"
			
				compound_elimination_STP_calc[i] <- prod(1 - c(				
					
					if(STP_treatment_steps[i, "Nitrifikation"] == "Ja") compound_elimination_STP$Nitrifikation else compound_elimination_STP$CSB_Abbau,
									
					if(STP_treatment_steps[i, "Erhoehte_Denitrifikation"] == "Ja") compound_elimination_STP$Erhoehte_Denitrifikation else{
						# Denitrifikation should only be available if there is a prior Nitrifikation, too - not further checked
						if(STP_treatment_steps[i, "Denitrifikation"] == "Ja") compound_elimination_STP$Denitrifikation
					},
					
					if(STP_treatment_steps[i, "P_Elimination"] == "Ja") compound_elimination_STP$P_Elimination,		
					
					if(!is.na(STP_treatment_steps[i, "Typ_MV-Behandlung"])){
						compound_elimination_STP[
							names(compound_elimination_STP) == STP_treatment_steps[i, "Typ_MV-Behandlung"]
						][[1]]
					}else 0
					
				))		
				if(is.na(compound_elimination_STP_calc[i])) stop("Problem in run_daily_load: NA for compound_elimination_STP_calc detected.")
			}
			#######################################
			if(compound_elimination_method == "nutrients"){			
			
				compound_elimination_STP_calc[i] <- (1 - c(	
			
					if(STP_treatment_steps[i, "Erhoehte_Denitrifikation"] == "Ja") compound_elimination_STP$Erhoehte_Denitrifikation else{
						if(STP_treatment_steps[i, "Denitrifikation"] == "Ja") compound_elimination_STP$Denitrifikation else{
							if(STP_treatment_steps[i, "Nitrifikation"] == "Ja") compound_elimination_STP$Nitrifikation else{ 
								compound_elimination_STP$CSB_Abbau
							}
						}
					}
			
				))
			
			}
			#######################################
			
		}
		
	}
	if(!(length(compound_elimination_STP_calc) %in% c(1, length(STP_id)))) stop("Problem in run_daily_load: invalid length for compound_elimination_STP_calc.")
	
	input_load_local_g_d <- STP_amount_inhabitants * compound_load_gramm_per_capita_and_day 
	load_local_g_d <- STP_amount_inhabitants * compound_load_gramm_per_capita_and_day * compound_excreted * compound_elimination_STP_calc
	load_local_g_d <- load_local_g_d + STP_amount_hospital_beds * compound_load_per_hospital_bed_and_day * compound_excreted * compound_elimination_STP_calc
	
	load_local_g_d[is.na(load_local_g_d)] <- 0 # for lakes
	
	if(add_absolute_load){
		absolute_loads[is.na(absolute_loads)] <- 0
		load_local_g_d <- load_local_g_d + absolute_loads
	}
	
	if(!with_lake_elimination){
	
		if(any(colnames(topo_matrix) != rownames(topo_matrix))) stop("topo_matrix must be symmetric")
		load_cumulated_g_d <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y)}, y = load_local_g_d) # MARGIN = 2 -> iterates over columns
	
	}else{
	
		lake_eliminination_rates[is.na(as.numeric(lake_eliminination_rates))] <- 0
		if(any(lake_eliminination_rates < 0) | any(lake_eliminination_rates > 1)) stop("lake_eliminination_rate must be within [0, 1]")
	
	
		# init cumulative loads with local loads
		load_cumulated_g_d <- load_local_g_d
	
		not_loop_endless <- 0 # just to be save
		
		ARANEXTNR_iter <- ARANEXTNR
		do_calc_node <- rep(TRUE, length(STP_id))
		while(
			any(do_calc_node) &
			not_loop_endless < 1E5
		){		
		
			for(k in 1:length(STP_id)){ # check through STPs / lakes
		
				# (1) still required to be calculated?
				if(!do_calc_node[k]) next
				
				# (2) still awaiting load input?
				if(STP_id[k] %in% ARANEXTNR_iter) next		
		
				# (3) for lakes: reduce load before adding on
				load_cumulated_g_d[k] <- load_cumulated_g_d[k] * (1 - lake_eliminination_rates[k])		

				# (4) adding on load to next STP or lake
				if(!is.na(ARANEXTNR_iter[k])){
					load_cumulated_g_d[STP_id == ARANEXTNR_iter[k]] <- load_cumulated_g_d[STP_id == ARANEXTNR_iter[k]] + load_cumulated_g_d[k]
					ARANEXTNR_iter[k] <- NA
				}

				# (5) mark that node has been done
				do_calc_node[k] <- FALSE
		
			}
			
			not_loop_endless <- not_loop_endless + 1
			
		}
		if(not_loop_endless >= 1E5) stop("Load routing through ARA and lake network did not finish within expected number of iterations. Debug input data; any circularities?") 
	
		if(FALSE){ # some test -> outcomment above step (3) -> results as derived from topo_matrix sum should be the same
			load_cumulated_g_d_check <- rep(NA, length(load_local_g_d)) 
			for(n in 1:ncol(topo_matrix)) load_cumulated_g_d_check[n] <- sum(topo_matrix[, n] * load_local_g_d)
			identical(round(load_cumulated_g_d_check, digits = 3), round(load_cumulated_g_d, digits = 3))		
			cbind(load_cumulated_g_d_check, load_cumulated_g_d)[load_cumulated_g_d_check != load_cumulated_g_d,]
		}
	
	}
	
	inhabitants_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y)}, y = STP_amount_inhabitants)
	STP_count_cumulated <- apply(topo_matrix, MARGIN = 2, function(x){ sum(x) - 1 })
	###############################################
	result <- data.frame(
		STP_id, 
		as.numeric(input_load_local_g_d),
		as.numeric(load_local_g_d), 
		as.numeric(load_cumulated_g_d), 
		as.numeric(inhabitants_cumulated), 
		as.numeric(STP_count_cumulated), 
		row.names = NULL)
	names(result) <- c("STP_ID", "input_load_local_g_d", "load_local_g_d", "load_cumulated_g_d", "inhabitants_cumulated", "STP_count_cumulated")
	return(result)
	
}














