






make_topology <- function(
	STP_id_next, 					# NA if none available
	STP_id = FALSE,					
	NA_next_ignore = FALSE,			# ara_id_next not in STP_id? -> set to NA as well
	insert_id_in_topo_matrix = FALSE
){

	###############################################
	# check inputs & defaults 	
	len <- length(STP_id_next)
	if(is.logical(STP_id[1]) & !isTRUE(STP_id[1])) STP_id <- seq(len) else{ 
		if(length(STP_id) != len) stop("Problem in make_topology: STP_id_next and STP_id must be of same length") 
		if(any(is.na(STP_id))) stop("Problem in make_topology: STP_id must not contain NAs") 
	}
	if(NA_next_ignore){
		if(any(!(STP_id_next[!is.na(STP_id_next)] %in% STP_id))) stop("Problem in make_topology: STP_id and STP_id_next mismatching")
	}else STP_id_next[!(STP_id_next %in% STP_id)] <- NA
	###############################################
	bin_link_matrix <- matrix(nrow = len, ncol = len, 0)
	STP_nr_next <- match(STP_id_next, STP_id)					# NAs returned
	for(i in 1:len) bin_link_matrix[i, STP_nr_next[i]] <- 1 	# NAs skipped
	topo_matrix <- solve(diag(len) - bin_link_matrix)			# 0 or 1
	colnames(topo_matrix) <- rownames(topo_matrix) <- STP_id
	if(isTRUE(insert_id_in_topo_matrix)) for(i in ncol(topo_matrix)) topo_matrix[, i] <- topo_matrix[, i] * as.numeric(STP_id) # inserts where 1
	###############################################
	return(topo_matrix) # columns mark upstream STPs of each STP
	
}

