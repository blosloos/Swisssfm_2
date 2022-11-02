

package.skeleton(
	name = "Swisssfm", 
	environment = .GlobalEnv,
	path = "C:/PART_4/MS/R_packages/Swisssfm", 
	force = FALSE, code_files = character(), encoding = "unknown"
)




library(devtools)


unload(pkg = "D:/PART_4/MS/R_packages/Swisssfm/Swisssfm")
clean_dll(pkg = "D:/PART_4/MS/R_packages/Swisssfm/Swisssfm")

compile_dll(pkg = "D:/PART_4/MS/R_packages/Swisssfm/Swisssfm", quiet = FALSE)
load_all(pkg = "D:/PART_4/MS/R_packages/Swisssfm/Swisssfm")

#
unload(pkg = "C:/PART_4/MS/R_packages/Swisssfm/Swisssfm")
clean_dll(pkg = "C:/PART_4/MS/R_packages/Swisssfm/Swisssfm")

compile_dll(pkg = "C:/PART_4/MS/R_packages/Swisssfm/Swisssfm", quiet = FALSE)
load_all(pkg = "C:/PART_4/MS/R_packages/Swisssfm/Swisssfm")



library(devtools)

install_github("blosloos/Swisssfm")

install_github("blosloos/Swisssfm_2")



library(devtools)

install(pkg = "C:/PART_4/MS/R_packages/Swisssfm/Swisssfm")


#################################################################################
# WORK on ARA table 


ARA <- read.csv2(file = "D:/VSA/new_inputs/ARA_input.csv", header = TRUE, sep = ",")

# 
ARA$LageX

ARA$X_Koordinate[is.na(ARA$LageX)]



sum(is.na(ARA$X_Koordinate))





plot(ARA$X_Koordinate[!is.na(ARA$X_Koordinate) & !is.na(ARA$LageX)], ARA$LageX[!is.na(ARA$X_Koordinate) & !is.na(ARA$LageX)])

ARA$X_Koordinate[!is.na(ARA$X_Koordinate) & !is.na(ARA$LageX)] - ARA$LageX[!is.na(ARA$X_Koordinate) & !is.na(ARA$LageX)]


plot(ARA$Y_Koordinate[!is.na(ARA$Y_Koordinate) & !is.na(ARA$LageY)], ARA$LageY[!is.na(ARA$Y_Koordinate) & !is.na(ARA$LageY)])

ARA$Y_Koordinate[!is.na(ARA$Y_Koordinate) & !is.na(ARA$LageY)] - ARA$LageY[!is.na(ARA$Y_Koordinate) & !is.na(ARA$LageY)]


ARA$LageX[!is.na(ARA$X_Koordinate) & is.na(ARA$LageX)] <- ARA$X_Koordinate[!is.na(ARA$X_Koordinate) & is.na(ARA$LageX)] + 2000000

ARA$LageY[!is.na(ARA$Y_Koordinate) & is.na(ARA$LageY)] <- ARA$Y_Koordinate[!is.na(ARA$Y_Koordinate) & is.na(ARA$LageY)] + 1000000


ARA$LageX[is.na(ARA$LageX)]
ARA$LageY[is.na(ARA$LageY)]




Vorfluter_LageX <- ARA$Vorfluter_X_Koordinate + 2000000
Vorfluter_LageY <- ARA$Vorfluter_Y_Koordinate + 1000000

ARA <- cbind(ARA, Vorfluter_LageX, Vorfluter_LageY)



write.csv(ARA, file = "D:/VSA/new_inputs/ARA_input_corrected.csv")

ARA <- read.csv2(file = "D:/VSA/new_inputs/ARA_input_corrected.csv", header = TRUE, sep = ",")

ARA$ARANEXTNR %in% ARA[, 2]


ARA_missing_ARANEXT <- ARA[!is.na(ARA$ARANEXTNR),][!(ARA$ARANEXTNR[!is.na(ARA$ARANEXTNR)] %in% ARA[, 2]),]





ARA_missing_ARANEXT$ARANEXTNR %in% ARA[, 5] 





write.csv(ARA_missing_ARANEXT, file = "D:/VSA/new_inputs/ARA_missing_ARANEXT.csv")


#################################################################################
# run

ARA <- read.csv2(file = "D:/VSA/new_inputs/ARA_input_corrected.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)








