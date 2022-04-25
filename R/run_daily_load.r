






run_daily_load <- function( # one function run per compound

	inhabitants_total = 8417700,
	hospital_beds_total = FALSE,					# Set to FALSE to ignore
	STP_id, 
	STP_treatment_steps,
	STP_fraction_hospital= FALSE,
	STP_amount_inhabitants,	
	STP_amount_hospital_beds = FALSE,				# Set to FALSE to ignore
	compound_load_total, 							# [kg / a], set to FALSE to ignore and then use compound_load_gramm_per_capita_and_day
	compound_load_gramm_per_capita_and_day = FALSE,	# [g / E d], set to FALSE to ignore and then use compound_load_total
	compound_load_per_hospital_bed_and_day = FALSE, # [g / E d], set to FALSE to ignore and then use compound_load_total
	compound_elimination_STP,						# named dataframe or vector with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
	compound_excreted = 1,							# fraction excreted and discharged, set to 1 to ignore
	
	with_lake_elimination = FALSE,
	lake_eliminination_rate = 0.25,
	
	topo_matrix
	
){

	###############################################
	# check inputs & defaults
	
	if(!is.data.frame(compound_elimination_STP) & is.vector(compound_elimination_STP)) stop("Problem in wrap_vsa, argument compound_elimination_STP is not a dataframe.")
	if(any(!sapply(compound_elimination_STP, is.numeric))) stop("Problem in wrap_vsa, dataframe compound_elimination_STP has non-numeric entries.")
	STP_steps <- c("CSB_Abbau", "Nitrifikation", "Denitrifikation", "P_Elimination", "GAK", "Kombi", "Ozonung", "PAK", "Ausbau")
	not_found <- !(STP_steps %in% names(compound_elimination_STP))
	if(any(not_found)) stop("Problem in wrap_vsa, argument compound_elimination_STP: entry ", compound_elimination_STP[not_found], " is missing.")
	if(any((compound_elimination_STP < 0) & (compound_elimination_STP > 1))) stop("Problem in run_daily_load: compound_elimination_STP not within [0,1]")
	
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

	###############################################
	if(!is.numeric(compound_load_gramm_per_capita_and_day)) compound_load_gramm_per_capita_and_day <- compound_load_total * (1 - STP_fraction_hospital) * 1000 / inhabitants_total / 365 		# [kg/a] -> [g/d]
	if(!is.numeric(compound_load_per_hospital_bed_and_day)) compound_load_per_hospital_bed_and_day <- compound_load_total * (STP_fraction_hospital) * 1000 / hospital_beds_total / 365 			# [kg/a] -> [g/d]
	if(is.vector(compound_elimination_STP)) compound_elimination_STP_calc <- prod(1 - compound_elimination_STP) # prod: if several steps combined
	if(is.data.frame(compound_elimination_STP)){
	
		compound_elimination_STP_calc <- rep(0, nrow(STP_treatment_steps))
		for(i in 1:nrow(STP_treatment_steps)){
		
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
		
	}
	if(!(length(compound_elimination_STP_calc) %in% c(1, length(STP_id)))) stop("Problem in run_daily_load: invalid length for compound_elimination_STP_calc.")
	
	load_local_g_d <- STP_amount_inhabitants * compound_load_gramm_per_capita_and_day * compound_excreted * compound_elimination_STP_calc
	load_local_g_d <- load_local_g_d + STP_amount_hospital_beds * compound_load_per_hospital_bed_and_day * compound_excreted * compound_elimination_STP_calc
	
	if(!with_lake_elimination){
	
		load_cumulated_g_d <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y)}, y = load_local_g_d) # MARGIN = 2 -> iterates over columns
	
	}else{
	
		if(lake_eliminination_rate < 0 | lake_eliminination_rate > 1) stop("lake_eliminination_rate must be within [0, 1]")
	
		load_cumulated_g_d <- rep(NA, ncol(topo_matrix))
		ARA_Nr_nach_See <- c(664301, 296400, 645700, 102400, 94400, 59300, 26101, 160200, 73300, 110400, 420800, 140100, 19301)
		
		except_ARA <- vector("list", 3) # Spezialfälle gemäss Kommentar in Seen.xlsx	
		names(except_ARA) <- c(102400, 26101, 12101)
		except_ARA[[1]] <- c(100900, 106603, 100100, 100402, 100401, 109800)
		except_ARA[[2]] <- c(26102, 130101, 130103, 137000, 170500)		
		except_ARA[[3]] <- c(11701)		
		
		if(any(colnames(topo_matrix) != rownames(topo_matrix))) stop("topo_matrix must be symmetric")
		
		for(n in 1:ncol(topo_matrix)){
	
			has_ARA_nach_See <- rownames(topo_matrix)[topo_matrix[, n] != 0][rownames(topo_matrix)[topo_matrix[, n] != 0] %in% ARA_Nr_nach_See]		
			those <- which(rownames(topo_matrix) %in% has_ARA_nach_See)

			if(length(those)){

				those <- c(those, n) 	# -> INDEX in matrix
				those <- those[order(colSums(topo_matrix[, those, drop = FALSE] != 0), decreasing = FALSE)] # if(length(those) > 1)
				if(those[length(those)] != n) stop("THIS SHOULD NOT HAPPEN") # current STP should be last one when sorted by increasing count of upstream STPs
				
				done_STPs <- c()		# -> INDEX in matrix
				load_cumulated_g_d_loop <- c()
				
				for(m in 1:length(those)){
				
					# Which are the non-nested STPs for this STP?
					has_upstream_STPs <- which(topo_matrix[, those[m]] != 0)
					has_upstream_STPs <- has_upstream_STPs[has_upstream_STPs != those[m]] # STP after lake excluded, and also last STP
					if(rownames(topo_matrix)[those[m]] %in% names(except_ARA)){
							
						this <- which(names(except_ARA) == rownames(topo_matrix)[those[m]])
						these_ARAs_to_exclude <- match(except_ARA[[this]], rownames(topo_matrix))
						has_upstream_STPs <- has_upstream_STPs[!(has_upstream_STPs %in% these_ARAs_to_exclude)]
					
					}
 					has_upstream_STPs <- has_upstream_STPs[!(has_upstream_STPs %in% done_STPs)]
					done_STPs <- unique(c(done_STPs, which(topo_matrix[, those[m]] != 0)) )
					done_STPs <- done_STPs[done_STPs != those[m]] # except has_ARA_nach_See itself
					
					if(length(has_upstream_STPs)) load_cumulated_g_d_loop <- c(load_cumulated_g_d_loop,
						sum(load_local_g_d[has_upstream_STPs]) * ((1 - lake_eliminination_rate) ^ (length(those) - m)) # last section of STPs, after last lake -> lake_eliminination_rate^0 = 1
					)
										
				}
				# add load of last STP 
				load_cumulated_g_d_loop <- c(load_cumulated_g_d_loop, load_local_g_d[those[m]])
				load_cumulated_g_d[n] <- sum(load_cumulated_g_d_loop)
			
			}else load_cumulated_g_d[n] <- sum(topo_matrix[, n] * load_local_g_d)

		}
	
	}
	
	inhabitants_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y)}, y = STP_amount_inhabitants)
	STP_count_cumulated <- apply(topo_matrix, MARGIN = 2, function(x){ sum(x) - 1 })
	###############################################
	result <- data.frame(
		as.numeric(STP_id), 
		as.numeric(load_local_g_d), 
		as.numeric(load_cumulated_g_d), 
		as.numeric(inhabitants_cumulated), 
		as.numeric(STP_count_cumulated), 
		row.names = NULL)
	names(result) <- c("STP_ID", "load_local_g_d", "load_cumulated_g_d", "inhabitants_cumulated", "STP_count_cumulated")
	return(result)
	
}














