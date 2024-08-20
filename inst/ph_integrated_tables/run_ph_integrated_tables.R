rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::update(prompt = F)
devtools::install_github("impact-initiatives-hppu/humind", force = T)
source("src/init.R")
library(tidyverse)
library(dplyr)


## check if environment exist
if(file.exists("inputs/environment.Rdata")) {
  load("inputs/environment.Rdata")
}

## Set up the date for the excel outputs
out_date <-  stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)

## Upload Dataset
filename.data <- choose.files(caption = "Select the Dataset", multi = F)

mort_collected <- svDialogs::dlg_message("Have you collected Mortality?", type = "yesno")$res
if(mort_collected == "yes"){
  filename.mortality <- choose.files(caption = "Select the related mortality integrated outputed from the descriptive analysis script",
                                     multi = F)
}


## Load Dataset
cat("\n> Loading data for analysis from", filename.data, "...\n")
sheet_names <- readxl::excel_sheets(filename.data)
sheet_names[1] <- paste(sheet_names[1], "(main)")
cat("> Found the following datasheets:", paste(sheet_names, collapse = ", "), "\n")

# the first sheet is always named "main"!!!
sheet_names[1] <- "main"
data.list <- list("main" = readxl::read_excel(filename.data, sheet=1, col_types = "text"))

for(sheet in sheet_names[-1]) {
  data.list[[sheet]] <- readxl::read_excel(filename.data, sheet=sheet, col_types = "text")
}

## Load Mortality
if(mort_collected == "yes"){
  mort_data <- readxl::read_excel(filename.mortality, col_types = "text") %>%
    dplyr::rename("mort" = point.est,
                  "mort_lci"=`95%lci`,
                  "mort_uci"=`95%uci`)
}


if(!file.exists("inputs/environment.Rdata")) {
  ## Detect Admin1 column
  admin1 <- names(data.list$main)[grepl("admin",names(data.list$main))]

  if(length(admin1) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", admin1, "' the correct admin1 column?"), type = "yesno")$res
    if(yes_no == "no"){
      admin1 <- svDialogs::dlg_input(message= "Enter the name of the Admin 1 Column","admin1")$res
    }
  } else if (length(admin1) > 1){
    admin1 <- tcltk::tk_select.list(admin1, title = "Admin 1 Column")
    if(admin1 == "") {
      admin1 <- svDialogs::dlg_input(message= "Enter the name of the Admin 1 Column","admin1")$res
    }
  } else if (length(admin1) == 0) {
    admin1 <- svDialogs::dlg_input(message= "Enter the name of the Admin 1 Column","admin1")$res
  }
  ## FSL Inputs
  FSL_indicators <- tcltk::tk_select.list(c("FCS","rCSI","HHS","LCSI"), title = "FSL indicators", multiple = T)
}

# FSL
if(!file.exists("inputs/environment.Rdata")) {
  if ("FCS" %in% FSL_indicators){
    fsl_fcs_cereal <- names(data.list$main)[grepl("cereal",names(data.list$main))]
    if(length(fsl_fcs_cereal) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_cereal, "' the correct fsl_fcs_cereal column?"), type = "yesno")$res
      if(yes_no == "no"){
        fsl_fcs_cereal <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_cereal","fsl_fcs_cereal")$res
      }
    } else if (length(fsl_fcs_cereal) > 1){
      fsl_fcs_cereal <- tcltk::tk_select.list(fsl_fcs_cereal, title = "FCS Cereal column")
      if(fsl_fcs_cereal == ""){
        fsl_fcs_cereal <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_cereal","fsl_fcs_cereal")$res
      }
    } else if (length(fsl_fcs_cereal) == 0) {
      fsl_fcs_cereal <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_cereal","fsl_fcs_cereal")$res
    }

    fsl_fcs_legumes <- names(data.list$main)[grepl("legume|pulse|bean|nuts|noix",names(data.list$main))]
    if(length(fsl_fcs_legumes) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_legumes, "' the correct fsl_fcs_legumes column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_legumes <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_legumes","fsl_fcs_legumes")$res
      }
    } else if (length(fsl_fcs_legumes) > 1){
      fsl_fcs_legumes <- tcltk::tk_select.list(fsl_fcs_legumes, title = "FCS Legumes/Pulses column")
      if(fsl_fcs_legumes == ""){
        fsl_fcs_legumes <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_legumes","fsl_fcs_legumes")$res
      }
    } else if (length(fsl_fcs_legumes) == 0) {
      fsl_fcs_legumes <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_legumes","fsl_fcs_legumes")$res
    }

    fsl_fcs_veg <- names(data.list$main)[grepl("veg|legume",names(data.list$main))]
    if(length(fsl_fcs_veg) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_veg, "' the correct fsl_fcs_veg column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_veg <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_veg","fsl_fcs_veg")$res
      }
    } else if (length(fsl_fcs_veg) > 1){
      fsl_fcs_veg <- tcltk::tk_select.list(fsl_fcs_veg, title = "FCS Vegetables column")
      if(fsl_fcs_veg == ""){
        fsl_fcs_veg <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_veg","fsl_fcs_veg")$res
      }
    } else if (length(fsl_fcs_veg) == 0) {
      fsl_fcs_veg <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_veg","fsl_fcs_veg")$res
    }

    fsl_fcs_fruit <- names(data.list$main)[grepl("fruit",names(data.list$main))]
    if(length(fsl_fcs_fruit) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_fruit, "' the correct fsl_fcs_fruit column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_fruit <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_fruit","fsl_fcs_fruit")$res
      }
    } else if (length(fsl_fcs_fruit) > 1){
      fsl_fcs_fruit <- tcltk::tk_select.list(fsl_fcs_fruit, title = "FCS Fruits column")
      if(fsl_fcs_fruit == ""){
        fsl_fcs_fruit <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_fruit","fsl_fcs_fruit")$res
      }
    } else if (length(fsl_fcs_fruit) == 0) {
      fsl_fcs_fruit <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_fruit","fsl_fcs_fruit")$res
    }

    fsl_fcs_meat <- names(data.list$main)[grepl("meat|viande",names(data.list$main))]
    if(length(fsl_fcs_meat) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_meat, "' the correct fsl_fcs_meat column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_meat <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_meat","fsl_fcs_meat")$res
      }
    } else if (length(fsl_fcs_meat) > 1){
      fsl_fcs_meat <- tcltk::tk_select.list(fsl_fcs_meat, title = "FCS Meat column")
      if(fsl_fcs_meat == ""){
        fsl_fcs_meat <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_meat","fsl_fcs_meat")$res
      }
    } else if (length(fsl_fcs_meat) == 0) {
      fsl_fcs_meat <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_meat","fsl_fcs_meat")$res
    }

    fsl_fcs_dairy <- names(data.list$main)[grepl("dairy|milk|lait",names(data.list$main))]
    if(length(fsl_fcs_dairy) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_dairy, "' the correct fsl_fcs_dairy column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_dairy <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_dairy","fsl_fcs_dairy")$res
      }
    } else if (length(fsl_fcs_dairy) > 1){
      fsl_fcs_dairy <- tcltk::tk_select.list(fsl_fcs_dairy, title = "FCS Dairy/Milk column")
      if(fsl_fcs_dairy == ""){
        fsl_fcs_dairy <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_dairy","fsl_fcs_dairy")$res
      }
    } else if (length(fsl_fcs_dairy) == 0) {
      fsl_fcs_dairy <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_dairy","fsl_fcs_dairy")$res
    }

    fsl_fcs_sugar <- names(data.list$main)[grepl("sugar|sucre",names(data.list$main))]
    if(length(fsl_fcs_sugar) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_sugar, "' the correct fsl_fcs_sugar column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_sugar <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_sugar","fsl_fcs_sugar")$res
      }
    } else if (length(fsl_fcs_sugar) > 1){
      fsl_fcs_sugar <- tcltk::tk_select.list(fsl_fcs_sugar, title = "FCS Sugar column")
      if(fsl_fcs_sugar == ""){
        fsl_fcs_sugar <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_sugar","fsl_fcs_sugar")$res
      }
    } else if (length(fsl_fcs_sugar) == 0) {
      fsl_fcs_sugar <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_sugar","fsl_fcs_sugar")$res
    }

    fsl_fcs_oil <- names(data.list$main)[grepl("oil|huile",names(data.list$main))]
    if(length(fsl_fcs_oil) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_oil, "' the correct fsl_fcs_oil column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_oil <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_oil","fsl_fcs_oil")$res
      }
    } else if (length(fsl_fcs_oil) > 1){
      fsl_fcs_oil <- tcltk::tk_select.list(fsl_fcs_oil, title = "FCS Oil column")
      if(fsl_fcs_oil == ""){
        fsl_fcs_oil <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_oil","fsl_fcs_oil")$res
      }
    } else if (length(fsl_fcs_oil) == 0) {
      fsl_fcs_oil <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_oil","fsl_fcs_oil")$res
    }

    fcs_check_columns <- c(fsl_fcs_cereal,
                           fsl_fcs_legumes,
                           fsl_fcs_veg,
                           fsl_fcs_fruit,
                           fsl_fcs_meat,
                           fsl_fcs_dairy,
                           fsl_fcs_sugar,
                           fsl_fcs_oil)

    if(!all(fcs_check_columns %in% names(data.list$main))) {
      svDialogs::dlg_message("Please check if the FCS columns selected are correct and available in the dataset")
      stop("Please check if the FCS columns selected are correct and available in the dataset")
    } else {
      data.list$main <- data.list$main %>%
        impactR4PHU::add_fcs(cutoffs = "normal",
                             fsl_fcs_cereal = fsl_fcs_cereal,
                             fsl_fcs_legumes = fsl_fcs_legumes,
                             fsl_fcs_veg = fsl_fcs_veg,
                             fsl_fcs_fruit = fsl_fcs_fruit,
                             fsl_fcs_meat = fsl_fcs_meat,
                             fsl_fcs_dairy = fsl_fcs_dairy,
                             fsl_fcs_sugar = fsl_fcs_sugar,
                             fsl_fcs_oil = fsl_fcs_oil)
    }
  } else {
    fsl_fcs_cereal <- "fsl_fcs_cereal"
    fsl_fcs_legumes <- "fsl_fcs_legumes"
    fsl_fcs_veg <- "fsl_fcs_veg"
    fsl_fcs_fruit <- "fsl_fcs_fruit"
    fsl_fcs_meat <- "fsl_fcs_meat"
    fsl_fcs_dairy <- "fsl_fcs_dairy"
    fsl_fcs_sugar <- "fsl_fcs_sugar"
    fsl_fcs_oil <- "fsl_fcs_oil"
    fcs_check_columns <- c(fsl_fcs_cereal,
                           fsl_fcs_legumes,
                           fsl_fcs_veg,
                           fsl_fcs_fruit,
                           fsl_fcs_meat,
                           fsl_fcs_dairy,
                           fsl_fcs_sugar,
                           fsl_fcs_oil)
  }
} else {
  if("FCS" %in% FSL_indicators){
    data.list$main <- data.list$main %>%
      impactR4PHU::add_fcs(cutoffs = "normal",
                           fsl_fcs_cereal = fsl_fcs_cereal,
                           fsl_fcs_legumes = fsl_fcs_legumes,
                           fsl_fcs_veg = fsl_fcs_veg,
                           fsl_fcs_fruit = fsl_fcs_fruit,
                           fsl_fcs_meat = fsl_fcs_meat,
                           fsl_fcs_dairy = fsl_fcs_dairy,
                           fsl_fcs_sugar = fsl_fcs_sugar,
                           fsl_fcs_oil = fsl_fcs_oil)
  }
}

if(!file.exists("inputs/environment.Rdata")) {
  if ("rCSI" %in% FSL_indicators){
    fsl_rcsi_lessquality <- names(data.list$main)[grepl("less|quality|lessquality|moins",names(data.list$main))]
    if(length(fsl_rcsi_lessquality) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_lessquality, "' the correct fsl_rcsi_lessquality column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_lessquality <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_lessquality","fsl_rcsi_lessquality")$res
      }
    } else if (length(fsl_rcsi_lessquality) > 1){
      fsl_rcsi_lessquality <- tcltk::tk_select.list(fsl_rcsi_lessquality, title = "rCSI Less Quality column")
      if(fsl_rcsi_lessquality == ""){
        fsl_rcsi_lessquality <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_lessquality","fsl_rcsi_lessquality")$res
      }
    } else if (length(fsl_rcsi_lessquality) == 0) {
      fsl_rcsi_lessquality <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_lessquality","fsl_rcsi_lessquality")$res
    }

    fsl_rcsi_borrow <- names(data.list$main)[grepl("borrow|emprunt",names(data.list$main))]
    if(length(fsl_rcsi_borrow) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_borrow, "' the correct fsl_rcsi_borrow column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_borrow <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_borrow","fsl_rcsi_borrow")$res
      }
    } else if (length(fsl_rcsi_borrow) > 1){
      fsl_rcsi_borrow <- tcltk::tk_select.list(fsl_rcsi_borrow, title = "rCSI Borrow column")
      if(fsl_rcsi_borrow == ""){
        fsl_rcsi_borrow <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_borrow","fsl_rcsi_borrow")$res
      }
    } else if (length(fsl_rcsi_borrow) == 0) {
      fsl_rcsi_borrow <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_borrow","fsl_rcsi_borrow")$res
    }

    fsl_rcsi_mealsize <- names(data.list$main)[grepl("mealsize|limit|portion|diminu",names(data.list$main))]
    if(length(fsl_rcsi_mealsize) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_mealsize, "' the correct fsl_rcsi_mealsize column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_mealsize <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealsize","fsl_rcsi_mealsize")$res
      }
    } else if (length(fsl_rcsi_mealsize) > 1){
      fsl_rcsi_mealsize <- tcltk::tk_select.list(fsl_rcsi_mealsize, title = "rCSI Meal Size column")
      if(fsl_rcsi_mealsize == ""){
        fsl_rcsi_mealsize <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealsize","fsl_rcsi_mealsize")$res
      }
    } else if (length(fsl_rcsi_mealsize) == 0) {
      fsl_rcsi_mealsize <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealsize","fsl_rcsi_mealsize")$res
    }

    fsl_rcsi_mealadult <- names(data.list$main)[grepl("mealadult|restrict|consommation",names(data.list$main))]
    if(length(fsl_rcsi_mealadult) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_mealadult, "' the correct fsl_rcsi_mealadult column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_mealadult <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealadult","fsl_rcsi_mealadult")$res
      }
    } else if (length(fsl_rcsi_mealadult) > 1){
      fsl_rcsi_mealadult <- tcltk::tk_select.list(fsl_rcsi_mealadult, title = "rCSI Meal Adult column")
      if(fsl_rcsi_mealadult == ""){
        fsl_rcsi_mealadult <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealadult","fsl_rcsi_mealadult")$res
      }
    } else if (length(fsl_rcsi_mealadult) == 0) {
      fsl_rcsi_mealadult <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealadult","fsl_rcsi_mealadult")$res
    }

    fsl_rcsi_mealnb <- names(data.list$main)[grepl("mealnb|reduce|meals|nb|repas",names(data.list$main))]
    if(length(fsl_rcsi_mealnb) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_mealnb, "' the correct fsl_rcsi_mealnb column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_mealnb <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealnb","fsl_rcsi_mealnb")$res
      }
    } else if (length(fsl_rcsi_mealnb) > 1){
      fsl_rcsi_mealnb <- tcltk::tk_select.list(fsl_rcsi_mealnb, title = "rCSI Meal Number column")
      if(fsl_rcsi_mealnb == ""){
        fsl_rcsi_mealnb <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealnb","fsl_rcsi_mealnb")$res
      }
    } else if (length(fsl_rcsi_mealnb) == 0) {
      fsl_rcsi_mealnb <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealnb","fsl_rcsi_mealnb")$res
    }

    rcsi_check_columns <- c(fsl_rcsi_lessquality,
                            fsl_rcsi_borrow,
                            fsl_rcsi_mealsize,
                            fsl_rcsi_mealadult,
                            fsl_rcsi_mealnb)

    if(!all(rcsi_check_columns %in% names(data.list$main))) {
      svDialogs::dlg_message("Please check if the rCSI columns selected are correct and available in the dataset")
      stop("Please check if the rCSI columns selected are correct and available in the dataset")
    } else {
      data.list$main <- data.list$main %>%
        impactR4PHU::add_rcsi(fsl_rcsi_lessquality = fsl_rcsi_lessquality,
                              fsl_rcsi_borrow = fsl_rcsi_borrow,
                              fsl_rcsi_mealsize = fsl_rcsi_mealsize,
                              fsl_rcsi_mealadult = fsl_rcsi_mealadult,
                              fsl_rcsi_mealnb = fsl_rcsi_mealnb)
    }
  } else {
    fsl_rcsi_lessquality <- "fsl_rcsi_lessquality"
    fsl_rcsi_borrow <- "fsl_rcsi_borrow"
    fsl_rcsi_mealsize <- "fsl_rcsi_mealsize"
    fsl_rcsi_mealadult <- "fsl_rcsi_mealadult"
    fsl_rcsi_mealnb <- "fsl_rcsi_mealnb"
    rcsi_check_columns <- c(fsl_rcsi_lessquality,
                            fsl_rcsi_borrow,
                            fsl_rcsi_mealsize,
                            fsl_rcsi_mealadult,
                            fsl_rcsi_mealnb)
  }
} else {
  if("rCSI" %in% FSL_indicators){
    data.list$main <- data.list$main %>%
      impactR4PHU::add_rcsi(fsl_rcsi_lessquality = fsl_rcsi_lessquality,
                            fsl_rcsi_borrow = fsl_rcsi_borrow,
                            fsl_rcsi_mealsize = fsl_rcsi_mealsize,
                            fsl_rcsi_mealadult = fsl_rcsi_mealadult,
                            fsl_rcsi_mealnb = fsl_rcsi_mealnb)
  }
}

if(!file.exists("inputs/environment.Rdata")) {
  if ("HHS" %in% FSL_indicators){
    fsl_hhs_nofoodhh <- names(data.list$main)[grepl("nofood|lack|aucun",names(data.list$main))]
    if(length(fsl_hhs_nofoodhh) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_nofoodhh, "' the correct fsl_hhs_nofoodhh column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_nofoodhh <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh","fsl_hhs_nofoodhh")$res
      }
    } else if (length(fsl_hhs_nofoodhh) > 1){
      fsl_hhs_nofoodhh <- tcltk::tk_select.list(fsl_hhs_nofoodhh, title = "HHS No Food HH column")
      if(fsl_hhs_nofoodhh == ""){
        fsl_hhs_nofoodhh <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh","fsl_hhs_nofoodhh")$res
      }
    } else if (length(fsl_hhs_nofoodhh) == 0) {
      fsl_hhs_nofoodhh <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh","fsl_hhs_nofoodhh")$res
    }

    fsl_hhs_nofoodhh_freq <- names(data.list$main)[grepl("nofood|lack|aucun",names(data.list$main))]
    if(length(fsl_hhs_nofoodhh_freq) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_nofoodhh_freq, "' the correct fsl_hhs_nofoodhh_freq column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_nofoodhh_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh_freq","fsl_hhs_nofoodhh_freq")$res
      }
    } else if (length(fsl_hhs_nofoodhh_freq) > 1){
      fsl_hhs_nofoodhh_freq <- tcltk::tk_select.list(fsl_hhs_nofoodhh_freq, title = "HHS No Food HH Freq column")
      if(fsl_hhs_nofoodhh_freq == ""){
        fsl_hhs_nofoodhh_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh_freq","fsl_hhs_nofoodhh_freq")$res
      }
    } else if (length(fsl_hhs_nofoodhh_freq) == 0) {
      fsl_hhs_nofoodhh_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh_freq","fsl_hhs_nofoodhh_freq")$res
    }

    fsl_hhs_sleephungry <- names(data.list$main)[grepl("sleephungry|sleep|dormir",names(data.list$main))]
    if(length(fsl_hhs_sleephungry) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_sleephungry, "' the correct fsl_hhs_sleephungry column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_sleephungry <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry","fsl_hhs_sleephungry")$res
      }
    } else if (length(fsl_hhs_sleephungry) > 1){
      fsl_hhs_sleephungry <- tcltk::tk_select.list(fsl_hhs_sleephungry, title = "HHS Sleep Hungry column")
      if(fsl_hhs_sleephungry == ""){
        fsl_hhs_sleephungry <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry","fsl_hhs_sleephungry")$res
      }
    } else if (length(fsl_hhs_sleephungry) == 0) {
      fsl_hhs_sleephungry <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry","fsl_hhs_sleephungry")$res
    }

    fsl_hhs_sleephungry_freq <- names(data.list$main)[grepl("sleephungry|sleep|dormir",names(data.list$main))]
    if(length(fsl_hhs_sleephungry_freq) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_sleephungry_freq, "' the correct fsl_hhs_sleephungry_freq column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_sleephungry_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry_freq","fsl_hhs_sleephungry_freq")$res
      }
    } else if (length(fsl_hhs_sleephungry_freq) > 1){
      fsl_hhs_sleephungry_freq <- tcltk::tk_select.list(fsl_hhs_sleephungry_freq, title = "HHS Sleep Hungry Freq column")
      if(fsl_hhs_sleephungry_freq == ""){
        fsl_hhs_sleephungry_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry_freq","fsl_hhs_sleephungry_freq")$res
      }
    } else if (length(fsl_hhs_sleephungry_freq) == 0) {
      fsl_hhs_sleephungry_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry_freq","fsl_hhs_sleephungry_freq")$res
    }

    fsl_hhs_alldaynight <- names(data.list$main)[grepl("alldaynight|daynoteating|wholeday|assez",names(data.list$main))]
    if(length(fsl_hhs_alldaynight) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_alldaynight, "' the correct fsl_hhs_alldaynight column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_alldaynight <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight","fsl_hhs_alldaynight")$res
      }
    } else if (length(fsl_hhs_alldaynight) > 1){
      fsl_hhs_alldaynight <- tcltk::tk_select.list(fsl_hhs_alldaynight, title = "HHS All Day Night column")
      if(fsl_hhs_alldaynight == ""){
        fsl_hhs_alldaynight <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight","fsl_hhs_alldaynight")$res
      }
    } else if (length(fsl_hhs_alldaynight) == 0) {
      fsl_hhs_alldaynight <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight","fsl_hhs_alldaynight")$res
    }

    fsl_hhs_alldaynight_freq <- names(data.list$main)[grepl("alldaynight|daynoteating|wholeday|assez",names(data.list$main))]
    if(length(fsl_hhs_alldaynight_freq) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_alldaynight_freq, "' the correct fsl_hhs_alldaynight_freq column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_alldaynight_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight_freq","fsl_hhs_alldaynight_freq")$res
      }
    } else if (length(fsl_hhs_alldaynight_freq) > 1){
      fsl_hhs_alldaynight_freq <- tcltk::tk_select.list(fsl_hhs_alldaynight_freq, title = "HHS all Day Night Freq column")
      if(fsl_hhs_alldaynight_freq == ""){
        fsl_hhs_alldaynight_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight_freq","fsl_hhs_alldaynight_freq")$res
      }
    } else if (length(fsl_hhs_alldaynight_freq) == 0) {
      fsl_hhs_alldaynight_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight_freq","fsl_hhs_alldaynight_freq")$res
    }

    hhs_check_columns <- c(fsl_hhs_nofoodhh,
                           fsl_hhs_sleephungry,
                           fsl_hhs_alldaynight)

    hhs_check_columns_freq <- c(fsl_hhs_nofoodhh_freq,
                                fsl_hhs_sleephungry_freq,
                                fsl_hhs_alldaynight_freq)

    if(!all(c(hhs_check_columns,hhs_check_columns_freq) %in% names(data.list$main))) {
      svDialogs::dlg_message("Please check if the HHS columns selected are correct and available in the dataset")
      stop("Please check if the HHS columns selected are correct and available in the dataset")
    } else{
      yes_answer <- tcltk::tk_select.list(unique(unlist(data.list$main[,hhs_check_columns])), title = "Yes Value")
      no_answer <- tcltk::tk_select.list(unique(unlist(data.list$main[,hhs_check_columns])), title = "No Value")
      rarely_answer <- tcltk::tk_select.list(unique(unlist(data.list$main[,hhs_check_columns_freq])), title = "Rarely Value")
      sometimes_answer <- tcltk::tk_select.list(unique(unlist(data.list$main[,hhs_check_columns_freq])), title = "Sometimes Value")
      often_answer <- tcltk::tk_select.list(unique(unlist(data.list$main[,hhs_check_columns_freq])), title = "Often Value")
      data.list$main <- data.list$main %>%
        dplyr::select(-contains("_recoded"))
      data.list$main <- data.list$main %>%
        dplyr::mutate_at(vars(hhs_check_columns),~case_when(. == yes_answer ~ yes_answer,
                                                            . == no_answer ~ no_answer,
                                                            TRUE ~ NA)) %>%
        dplyr::mutate_at(vars(hhs_check_columns_freq),~case_when(. == rarely_answer ~ rarely_answer,
                                                                 . == sometimes_answer ~ sometimes_answer,
                                                                 . == often_answer ~ often_answer,
                                                                 TRUE ~ NA)) %>%
        impactR4PHU::add_hhs(fsl_hhs_nofoodhh = fsl_hhs_nofoodhh,
                             fsl_hhs_nofoodhh_freq = fsl_hhs_nofoodhh_freq,
                             fsl_hhs_sleephungry = fsl_hhs_sleephungry,
                             fsl_hhs_sleephungry_freq = fsl_hhs_sleephungry_freq,
                             fsl_hhs_alldaynight = fsl_hhs_alldaynight,
                             fsl_hhs_alldaynight_freq = fsl_hhs_alldaynight_freq,
                             yes_answer = yes_answer,
                             no_answer = no_answer,
                             rarely_answer = rarely_answer,
                             sometimes_answer = sometimes_answer,
                             often_answer = often_answer)
    }
  } else {
    fsl_hhs_nofoodhh <- "fsl_hhs_nofoodhh"
    fsl_hhs_nofoodhh_freq <- "fsl_hhs_nofoodhh_freq"
    fsl_hhs_sleephungry <- "fsl_hhs_sleephungry"
    fsl_hhs_sleephungry_freq <- "fsl_hhs_sleephungry_freq"
    fsl_hhs_alldaynight <- "fsl_hhs_alldaynight"
    fsl_hhs_alldaynight_freq <- "fsl_hhs_alldaynight_freq"
    hhs_check_columns <- c(fsl_hhs_nofoodhh,
                           fsl_hhs_sleephungry,
                           fsl_hhs_alldaynight)

    hhs_check_columns_freq <- c(fsl_hhs_nofoodhh_freq,
                                fsl_hhs_sleephungry_freq,
                                fsl_hhs_alldaynight_freq)
  }
} else {
  if("HHS" %in% FSL_indicators){
    data.list$main <- data.list$main %>%
      dplyr::select(-contains("_recoded"))
    data.list$main <- data.list$main %>%
      dplyr::mutate_at(vars(hhs_check_columns),~case_when(. == yes_answer ~ yes_answer,
                                                          . == no_answer ~ no_answer,
                                                          TRUE ~ NA)) %>%
      dplyr::mutate_at(vars(hhs_check_columns_freq),~case_when(. == rarely_answer ~ rarely_answer,
                                                               . == sometimes_answer ~ sometimes_answer,
                                                               . == often_answer ~ often_answer,
                                                               TRUE ~ NA)) %>%
      impactR4PHU::add_hhs(fsl_hhs_nofoodhh = fsl_hhs_nofoodhh,
                           fsl_hhs_nofoodhh_freq = fsl_hhs_nofoodhh_freq,
                           fsl_hhs_sleephungry = fsl_hhs_sleephungry,
                           fsl_hhs_sleephungry_freq = fsl_hhs_sleephungry_freq,
                           fsl_hhs_alldaynight = fsl_hhs_alldaynight,
                           fsl_hhs_alldaynight_freq = fsl_hhs_alldaynight_freq,
                           yes_answer = yes_answer,
                           no_answer = no_answer,
                           rarely_answer = rarely_answer,
                           sometimes_answer = sometimes_answer,
                           often_answer = often_answer)
  }
}

if(!file.exists("inputs/environment.Rdata")) {
  if ("LCSI" %in% FSL_indicators){
    fsl_lcsi_stress1 <- names(data.list$main)[grepl("stress|stress1",names(data.list$main))]
    if(length(fsl_lcsi_stress1) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_stress1, "' the correct fsl_lcsi_stress1 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_stress1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress1","fsl_lcsi_stress1")$res
      }
    } else if (length(fsl_lcsi_stress1) > 1){
      fsl_lcsi_stress1 <- tcltk::tk_select.list(fsl_lcsi_stress1, title = "LCSI Stress 1 column")
      if(fsl_lcsi_stress1 == ""){
        fsl_lcsi_stress1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress1","fsl_lcsi_stress1")$res
      }
    } else if (length(fsl_lcsi_stress1) == 0) {
      fsl_lcsi_stress1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress1","fsl_lcsi_stress1")$res
    }
    fsl_lcsi_stress2 <- names(data.list$main)[grepl("stress|stress2",names(data.list$main))]
    if(length(fsl_lcsi_stress2) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_stress2, "' the correct fsl_lcsi_stress2 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_stress2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress2","fsl_lcsi_stress2")$res
      }
    } else if (length(fsl_lcsi_stress2) > 1){
      fsl_lcsi_stress2 <- tcltk::tk_select.list(fsl_lcsi_stress2, title = "LCSI Stress 2 column")
      if(fsl_lcsi_stress2 == ""){
        fsl_lcsi_stress2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress2","fsl_lcsi_stress2")$res
      }
    } else if (length(fsl_lcsi_stress2) == 0) {
      fsl_lcsi_stress2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress2","fsl_lcsi_stress2")$res
    }

    fsl_lcsi_stress3 <- names(data.list$main)[grepl("stress|stress3",names(data.list$main))]
    if(length(fsl_lcsi_stress3) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_stress3, "' the correct fsl_lcsi_stress3 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_stress3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress3","fsl_lcsi_stress3")$res
      }
    } else if (length(fsl_lcsi_stress3) > 1){
      fsl_lcsi_stress3 <- tcltk::tk_select.list(fsl_lcsi_stress3, title = "LCSI Stress 3 column")
      if(fsl_lcsi_stress3 == ""){
        fsl_lcsi_stress3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress3","fsl_lcsi_stress3")$res
      }
    } else if (length(fsl_lcsi_stress3) == 0) {
      fsl_lcsi_stress3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress3","fsl_lcsi_stress3")$res
    }

    fsl_lcsi_stress4 <- names(data.list$main)[grepl("stress|stress4",names(data.list$main))]
    if(length(fsl_lcsi_stress4) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_stress4, "' the correct fsl_lcsi_stress4 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_stress4 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress4","fsl_lcsi_stress4")$res
      }
    } else if (length(fsl_lcsi_stress4) > 1){
      fsl_lcsi_stress4 <- tcltk::tk_select.list(fsl_lcsi_stress4, title = "LCSI Stress 4 column")
      if(fsl_lcsi_stress4 == ""){
        fsl_lcsi_stress4 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress4","fsl_lcsi_stress4")$res
      }
    } else if (length(fsl_lcsi_stress4) == 0) {
      fsl_lcsi_stress4 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress4","fsl_lcsi_stress4")$res
    }

    fsl_lcsi_crisis1 <- names(data.list$main)[grepl("crisis|crisis1",names(data.list$main))]
    if(length(fsl_lcsi_crisis1) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_crisis1, "' the correct fsl_lcsi_crisis1 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_crisis1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis1","fsl_lcsi_crisis1")$res
      }
    } else if (length(fsl_lcsi_crisis1) > 1){
      fsl_lcsi_crisis1 <- tcltk::tk_select.list(fsl_lcsi_crisis1, title = "LCSI Crisis 1 column")
      if(fsl_lcsi_crisis1 == ""){
        fsl_lcsi_crisis1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis1","fsl_lcsi_crisis1")$res
      }
    } else if (length(fsl_lcsi_crisis1) == 0) {
      fsl_lcsi_crisis1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis1","fsl_lcsi_crisis1")$res
    }

    fsl_lcsi_crisis2 <- names(data.list$main)[grepl("crisis|crisis2",names(data.list$main))]
    if(length(fsl_lcsi_crisis2) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_crisis2, "' the correct fsl_lcsi_crisis2 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_crisis2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis2","fsl_lcsi_crisis2")$res
      }
    } else if (length(fsl_lcsi_crisis2) > 1){
      fsl_lcsi_crisis2 <- tcltk::tk_select.list(fsl_lcsi_crisis2, title = "LCSI Crisis 2 column")
      if(fsl_lcsi_crisis2 == ""){
        fsl_lcsi_crisis2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis2","fsl_lcsi_crisis2")$res
      }
    } else if (length(fsl_lcsi_crisis2) == 0) {
      fsl_lcsi_crisis2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis2","fsl_lcsi_crisis2")$res
    }

    fsl_lcsi_crisis3 <- names(data.list$main)[grepl("crisis|crisis3",names(data.list$main))]
    if(length(fsl_lcsi_crisis3) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_crisis3, "' the correct fsl_lcsi_crisis3 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_crisis3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis3","fsl_lcsi_crisis3")$res
      }
    } else if (length(fsl_lcsi_crisis3) > 1){
      fsl_lcsi_crisis3 <- tcltk::tk_select.list(fsl_lcsi_crisis3, title = "LCSI Crisis 3 column")
      if(fsl_lcsi_crisis3 == ""){
        fsl_lcsi_crisis3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis3","fsl_lcsi_crisis3")$res
      }
    } else if (length(fsl_lcsi_crisis3) == 0) {
      fsl_lcsi_crisis3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis3","fsl_lcsi_crisis3")$res
    }

    fsl_lcsi_emergency1 <- names(data.list$main)[grepl("emergency|emergency1",names(data.list$main))]
    if(length(fsl_lcsi_emergency1) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_emergency1, "' the correct fsl_lcsi_emergency1 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_emergency1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency1","fsl_lcsi_emergency1")$res
      }
    } else if (length(fsl_lcsi_emergency1) > 1){
      fsl_lcsi_emergency1 <- tcltk::tk_select.list(fsl_lcsi_emergency1, title = "LCSI Emergency 1 column")
      if(fsl_lcsi_emergency1 == ""){
        fsl_lcsi_emergency1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency1","fsl_lcsi_emergency1")$res
      }
    } else if (length(fsl_lcsi_emergency1) == 0) {
      fsl_lcsi_emergency1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency1","fsl_lcsi_emergency1")$res
    }

    fsl_lcsi_emergency2 <- names(data.list$main)[grepl("emergency|emergency2",names(data.list$main))]
    if(length(fsl_lcsi_emergency2) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_emergency2, "' the correct fsl_lcsi_emergency2 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_emergency2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency2","fsl_lcsi_emergency2")$res
      }
    } else if (length(fsl_lcsi_emergency2) > 1){
      fsl_lcsi_emergency2 <- tcltk::tk_select.list(fsl_lcsi_emergency2, title = "LCSI Emergency 2 column")
      if(fsl_lcsi_emergency2 == ""){
        fsl_lcsi_emergency2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency2","fsl_lcsi_emergency2")$res
      }
    } else if (length(fsl_lcsi_emergency2) == 0) {
      fsl_lcsi_emergency2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency2","fsl_lcsi_emergency2")$res
    }

    fsl_lcsi_emergency3 <- names(data.list$main)[grepl("emergency|emergency3",names(data.list$main))]
    if(length(fsl_lcsi_emergency3) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_emergency3, "' the correct fsl_lcsi_emergency3 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_emergency3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency3","fsl_lcsi_emergency3")$res
      }
    } else if (length(fsl_lcsi_emergency3) > 1){
      fsl_lcsi_emergency3 <- tcltk::tk_select.list(fsl_lcsi_emergency3, title = "LCSI Emergency 3 column")
      if(fsl_lcsi_emergency3 == ""){
        fsl_lcsi_emergency3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency3","fsl_lcsi_emergency3")$res
      }
    } else if (length(fsl_lcsi_emergency3) == 0) {
      fsl_lcsi_emergency3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency3","fsl_lcsi_emergency3")$res
    }

    lcsi_check_columns <- c(fsl_lcsi_stress1,fsl_lcsi_stress2,fsl_lcsi_stress3,fsl_lcsi_stress4,
                            fsl_lcsi_crisis1,fsl_lcsi_crisis2,fsl_lcsi_crisis3,
                            fsl_lcsi_emergency1,fsl_lcsi_emergency2,fsl_lcsi_emergency3)


    if(!all(lcsi_check_columns %in% names(data.list$main))) {
      svDialogs::dlg_message("Please check if the LCSI columns selected are correct and available in the dataset")
      stop("Please check if the LCSI columns selected are correct and available in the dataset")
    } else{
      yes_val <- tcltk::tk_select.list(unique(unlist(data.list$main[,lcsi_check_columns])), title = "Yes Value")
      no_val <- tcltk::tk_select.list(unique(unlist(data.list$main[,lcsi_check_columns])), title = "No Value")
      exhausted_val <- tcltk::tk_select.list(unique(unlist(data.list$main[,lcsi_check_columns])), title = "Exhausted Value")
      not_applicable_val <- tcltk::tk_select.list(unique(unlist(data.list$main[,lcsi_check_columns])), title = "Not Applicable Value")
      data.list$main <- data.list$main %>%
        dplyr::mutate_at(vars(lcsi_check_columns),~case_when(. == yes_val ~ yes_val,
                                                             . == no_val ~ no_val,
                                                             . == exhausted_val ~ exhausted_val,
                                                             . == not_applicable_val ~ not_applicable_val,
                                                             TRUE ~ NA)) %>%
        impactR4PHU::add_lcsi(fsl_lcsi_stress1 = fsl_lcsi_stress1,
                              fsl_lcsi_stress2 = fsl_lcsi_stress2,
                              fsl_lcsi_stress3 = fsl_lcsi_stress3,
                              fsl_lcsi_stress4 = fsl_lcsi_stress4,
                              fsl_lcsi_crisis1 = fsl_lcsi_crisis1,
                              fsl_lcsi_crisis2 = fsl_lcsi_crisis2,
                              fsl_lcsi_crisis3 = fsl_lcsi_crisis3,
                              fsl_lcsi_emergency1 = fsl_lcsi_emergency1,
                              fsl_lcsi_emergency2 = fsl_lcsi_emergency2,
                              fsl_lcsi_emergency3 = fsl_lcsi_emergency3,
                              yes_val = yes_val,
                              no_val = no_val,
                              exhausted_val = exhausted_val,
                              not_applicable_val = not_applicable_val)
    }
  } else {
    fsl_lcsi_stress1 <- "fsl_lcsi_stress1"
    fsl_lcsi_stress2 <- "fsl_lcsi_stress2"
    fsl_lcsi_stress3 <- "fsl_lcsi_stress3"
    fsl_lcsi_stress4 <- "fsl_lcsi_stress4"
    fsl_lcsi_crisis1 <- "fsl_lcsi_crisis1"
    fsl_lcsi_crisis2 <- "fsl_lcsi_crisis2"
    fsl_lcsi_crisis3 <- "fsl_lcsi_crisis3"
    fsl_lcsi_emergency1 <- "fsl_lcsi_emergency1"
    fsl_lcsi_emergency2 <- "fsl_lcsi_emergency2"
    fsl_lcsi_emergency3 <- "fsl_lcsi_emergency3"
    lcsi_check_columns <- c(fsl_lcsi_stress1,fsl_lcsi_stress2,fsl_lcsi_stress3,fsl_lcsi_stress4,
                            fsl_lcsi_crisis1,fsl_lcsi_crisis2,fsl_lcsi_crisis3,
                            fsl_lcsi_emergency1,fsl_lcsi_emergency2,fsl_lcsi_emergency3)
  }
} else {
  if("LCSI" %in% FSL_indicators){
    data.list$main <- data.list$main %>%
      dplyr::mutate_at(vars(lcsi_check_columns),~case_when(. == yes_val ~ yes_val,
                                                           . == no_val ~ no_val,
                                                           . == exhausted_val ~ exhausted_val,
                                                           . == not_applicable_val ~ not_applicable_val,
                                                           TRUE ~ NA)) %>%
      impactR4PHU::add_lcsi(fsl_lcsi_stress1 = fsl_lcsi_stress1,
                            fsl_lcsi_stress2 = fsl_lcsi_stress2,
                            fsl_lcsi_stress3 = fsl_lcsi_stress3,
                            fsl_lcsi_stress4 = fsl_lcsi_stress4,
                            fsl_lcsi_crisis1 = fsl_lcsi_crisis1,
                            fsl_lcsi_crisis2 = fsl_lcsi_crisis2,
                            fsl_lcsi_crisis3 = fsl_lcsi_crisis3,
                            fsl_lcsi_emergency1 = fsl_lcsi_emergency1,
                            fsl_lcsi_emergency2 = fsl_lcsi_emergency2,
                            fsl_lcsi_emergency3 = fsl_lcsi_emergency3,
                            yes_val = yes_val,
                            no_val = no_val,
                            exhausted_val = exhausted_val,
                            not_applicable_val = not_applicable_val)
  }
}


fcm_check_1_columns <- c("fsl_fcs_cat",
                         "fsl_rcsi_cat")


fcm_check_3_columns <- c("fsl_fcs_cat",
                         "fsl_hhs_cat")


fcm_check_6_columns <- c("fsl_fcs_cat",
                         "fsl_rcsi_cat",
                         "fsl_hhs_cat")

if(all(fcm_check_1_columns %in% names(data.list$main)) |
   all(fcm_check_3_columns %in% names(data.list$main)) |
   all(fcm_check_6_columns %in% names(data.list$main))) {
  data.list$main <- data.list$main %>%
    impactR4PHU::add_fcm_phase()
}


admin1_df <- data.list$main %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(num_observartion = n())

ph_int_table <- admin1_df %>%
  dplyr::mutate(children_sick = NA,
                unmet_healthcare = NA,
                amn_phase = NA,
                afi_phase = NA,
                fcs_phase = NA,
                fcs = NA,
                rcsi = NA,
                hhs = NA,
                lcsi = NA,
                impro_water = NA,
                drinking_water = NA,
                sanitation = NA,
                handwash = NA,
                distance_healthcare = NA)

if(mort_collected == "yes"){
  ph_int_table <- ph_int_table %>%
    dplyr::mutate(mort = mort_data$mort[match(!!rlang::sym(admin1),mort_data$admin1)]) %>%
    dplyr::relocate(mort, .before = 3)
}

if("FCS" %in% FSL_indicators) {
  fcs_df <- data.list$main %>%
    dplyr::filter(fsl_fcs_cat == "Poor") %>%
    dplyr::group_by(!!rlang::sym(admin1)) %>%
    dplyr::summarise(fcs = n())
}

if("rCSI" %in% FSL_indicators) {
  rcsi_df <- data.list$main %>%
    dplyr::filter(fsl_rcsi_cat == "High") %>%
    dplyr::group_by(!!rlang::sym(admin1)) %>%
    dplyr::summarise(rcsi = n())
}

if("HHS" %in% FSL_indicators) {
  hhs_df <- data.list$main %>%
    dplyr::filter(fsl_hhs_cat_ipc %in% c("Severe","Very Severe")) %>%
    dplyr::group_by(!!rlang::sym(admin1)) %>%
    dplyr::summarise(hhs = n())
}

if("LCSI" %in% FSL_indicators) {
  lcsi_df <- data.list$main %>%
    dplyr::filter(fsl_lcsi_cat == "Emergency") %>%
    dplyr::group_by(!!rlang::sym(admin1)) %>%
    dplyr::summarise(lcsi = n())
}

fsl_phase_df <- data.list$main %>%
  dplyr::filter(fsl_fc_phase %in% c("Phase 3 FC",
                                    "Phase 4 FC",
                                    "Phase 5 FC")) %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(fcs_phase = n())


## WASH

if(!file.exists("inputs/environment.Rdata")) {
  ##Survey Modality
  survey_modality <- names(data.list$main)[grepl("survey_modality",names(data.list$main))]
  if(length(survey_modality) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", survey_modality, "' the correct column for survey modality?"), type = "yesno")$res
    if (yes_no == "no") {
      survey_modality <- svDialogs::dlg_input(message= "Enter the name of the survey modality","survey_modality")$res
    }
  } else if (length(survey_modality) > 1){
    survey_modality <- tcltk::tk_select.list(survey_modality, title = "Survey modality column")
    if(survey_modality == ""){
      survey_modality <- svDialogs::dlg_input(message= "Enter the name of the survey modality","survey_modality")$res
    }
  } else if (length(survey_modality) == 0) {
    survey_modality <- svDialogs::dlg_input(message= "Enter the name of the survey modality","survey_modality")$res
  }
  survey_modality_in_person <- tcltk::tk_select.list(unique(unlist(data.list$main[,survey_modality])), title = "In Person Values", multiple = T)
  survey_modality_remote <- tcltk::tk_select.list(unique(unlist(data.list$main[,survey_modality])), title = "Remote Values", multiple = T)

  #Handwashing Facility
  facility <- names(data.list$main)[grepl("handwashing|facility",names(data.list$main))]
  if(length(facility) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", facility, "' the correct column for handwashing facility?"), type = "yesno")$res
    if (yes_no == "no") {
      facility <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility","wash_handwashing_facility")$res
    }
  } else if (length(facility) > 1){
    facility <- tcltk::tk_select.list(facility, title = "Handwashing facility column")
    if(facility == ""){
      facility <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility","wash_handwashing_facility")$res
    }
  } else if (length(facility) == 0) {
    facility <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility","wash_handwashing_facility")$res
  }
  facility_yes <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility])), title = "Yes Value")
  facility_no <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility])), title = "None Value")
  facility_no_permission <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility])), title = "No permission Value")
  facility_undefined <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility])), title = "Other Value")

  # Observed water of handwashing facility
  facility_observed_water <- names(data.list$main)[grepl("facility_observed_water",names(data.list$main))]
  if(length(facility_observed_water) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", facility_observed_water, "' the correct column for handwashing facility observed water?"), type = "yesno")$res
    if (yes_no == "no") {
      facility_observed_water <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility observed water","wash_handwashing_facility_observed_water")$res
    }
  } else if (length(facility_observed_water) > 1){
    facility_observed_water <- tcltk::tk_select.list(facility_observed_water, title = "Handwashing facility observed water column")
    if(facility_observed_water == ""){
      facility_observed_water <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility observed water","wash_handwashing_facility_observed_water")$res
    }
  } else if (length(facility_observed_water) == 0) {
    facility_observed_water <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility observed water","wash_handwashing_facility_observed_water")$res
  }
  facility_observed_water_yes <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_observed_water])), title = "Yes Value")
  facility_observed_water_no <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_observed_water])), title = "No Value")

  ## Observed Soap Handwashing Facility
  facility_observed_soap <- names(data.list$main)[grepl("facility_observed_soap",names(data.list$main))]
  if(length(facility_observed_soap) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", facility_observed_soap, "' the correct column for handwashing facility observed soap?"), type = "yesno")$res
    if (yes_no == "no") {
      facility_observed_soap <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility observed soap","wash_handwashing_facility_observed_soap")$res
    }
  } else if (length(facility_observed_soap) > 1){
    facility_observed_soap <- tcltk::tk_select.list(facility_observed_soap, title = "Handwashing facility observed soap column")
    if(facility_observed_soap == ""){
      facility_observed_soap <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility observed soap","wash_handwashing_facility_observed_soap")$res
    }
  } else if (length(facility_observed_soap) == 0) {
    facility_observed_soap <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility observed soap","wash_handwashing_facility_observed_soap")$res
  }
  facility_observed_soap_yes <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_observed_soap])), title = "Yes Value")
  facility_observed_soap_no <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_observed_soap])), title = "No Value")
  facility_observed_soap_alternative <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_observed_soap])), title = "Alternative Value")

  ## Reported Handwashing Facility
  facility_reported <- names(data.list$main)[grepl("facility_reported",names(data.list$main))]
  if(length(facility_reported) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", facility_reported, "' the correct column for handwashing facility reported?"), type = "yesno")$res
    if (yes_no == "no") {
      facility_reported <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility reported","wash_handwashing_facility_reported")$res
    }
  } else if (length(facility_reported) > 1){
    facility_reported <- tcltk::tk_select.list(facility_reported, title = "Handwashing facility reported column")
    if(facility_reported == ""){
      facility_reported <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility reported","wash_handwashing_facility_reported")$res
    }
  } else if (length(facility_reported) == 0) {
    facility_reported <- svDialogs::dlg_input(message= "Enter the name of the handwashing facility reported","wash_handwashing_facility_reported")$res
  }
  facility_reported_yes <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported])), title = "Yes Value", multiple = TRUE)
  facility_reported_no <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported])), title = "No Value", multiple = TRUE)
  facility_reported_undefined <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported])), title = "Undefined Value", multiple = TRUE)

  ## Reported no permission soap
  facility_reported_no_permission_soap <- names(data.list$main)[grepl("soap_observed",names(data.list$main))]
  if(length(facility_reported_no_permission_soap) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", facility_reported_no_permission_soap, "' the correct column for reported no permission soap?"), type = "yesno")$res
    if (yes_no == "no") {
      facility_reported_no_permission_soap <- svDialogs::dlg_input(message= "Enter the name of the reported no permission soap","wash_soap_observed")$res
    }
  } else if (length(facility_reported_no_permission_soap) > 1){
    facility_reported_no_permission_soap <- tcltk::tk_select.list(facility_reported_no_permission_soap, title = "Reported no permission soap column")
    if(facility_reported_no_permission_soap == ""){
      facility_reported_no_permission_soap <- svDialogs::dlg_input(message= "Enter the name of the reported no permission soap","wash_soap_observed")$res
    }
  } else if (length(facility_reported_no_permission_soap) == 0) {
    facility_reported_no_permission_soap <- svDialogs::dlg_input(message= "Enter the name of the reported no permission soap","wash_soap_observed")$res
  }
  facility_reported_no_permission_soap_yes <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_no_permission_soap])), title = "Yes Value", multiple = TRUE)
  facility_reported_no_permission_soap_no <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_no_permission_soap])), title = "No Value")
  facility_reported_no_permission_soap_undefined <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_no_permission_soap])), title = "Undefined Value", multiple = TRUE)

  ## Reported no permission soap type
  facility_reported_no_permission_soap_type <- names(data.list$main)[grepl("soap_observed_type",names(data.list$main))]
  if(length(facility_reported_no_permission_soap_type) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", facility_reported_no_permission_soap_type, "' the correct column for reported no permission soap type?"), type = "yesno")$res
    if (yes_no == "no") {
      facility_reported_no_permission_soap_type <- svDialogs::dlg_input(message= "Enter the name of the reported no permission soap type","wash_soap_observed_type")$res
    }
  } else if (length(facility_reported_no_permission_soap_type) > 1){
    facility_reported_no_permission_soap_type <- tcltk::tk_select.list(facility_reported_no_permission_soap_type, title = "Reported no permission soap type column")
    if(facility_reported_no_permission_soap_type == ""){
      facility_reported_no_permission_soap_type <- svDialogs::dlg_input(message= "Enter the name of the reported no permission soap type","wash_soap_observed_type")$res
    }
  } else if (length(facility_reported_no_permission_soap_type) == 0) {
    facility_reported_no_permission_soap_type <- svDialogs::dlg_input(message= "Enter the name of the reported no permission soap type","wash_soap_observed_type")$res
  }
  facility_reported_no_permission_soap_type_yes <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_no_permission_soap_type])), title = "Yes Value", multiple = TRUE)
  facility_reported_no_permission_soap_type_no <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_no_permission_soap_type])), title = "No Value")
  facility_reported_no_permission_soap_type_undefined <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_no_permission_soap_type])), title = "Undefined Value", multiple = TRUE)

  ## Reported remote soap
  facility_reported_remote_soap <- names(data.list$main)[grepl("soap_reported",names(data.list$main))]
  if(length(facility_reported_remote_soap) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", facility_reported_remote_soap, "' the correct column for reported remote soap?"), type = "yesno")$res
    if (yes_no == "no") {
      facility_reported_remote_soap <- svDialogs::dlg_input(message= "Enter the name of the reported remote soap","wash_soap_reported")$res
    }
  } else if (length(facility_reported_remote_soap) > 1){
    facility_reported_remote_soap <- tcltk::tk_select.list(facility_reported_remote_soap, title = "Reported remote soap column")
    if(facility_reported_remote_soap == ""){
      facility_reported_remote_soap <- svDialogs::dlg_input(message= "Enter the name of the reported remote soap","wash_soap_reported")$res
    }
  } else if (length(facility_reported_remote_soap) == 0) {
    facility_reported_remote_soap <- svDialogs::dlg_input(message= "Enter the name of the reported remote soap","wash_soap_reported")$res
  }
  facility_reported_remote_soap_yes <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_remote_soap])), title = "Yes Value")
  facility_reported_remote_soap_no <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_remote_soap])), title = "No Value")
  facility_reported_remote_soap_undefined <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_remote_soap])), title = "Undefined Value", multiple = TRUE)

  ## Reported remote soap type
  facility_reported_remote_soap_type <- names(data.list$main)[grepl("soap_reported_type",names(data.list$main))]
  if(length(facility_reported_remote_soap_type) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", facility_reported_remote_soap_type, "' the correct column for reported remote soap type?"), type = "yesno")$res
    if (yes_no == "no") {
      facility_reported_remote_soap_type <- svDialogs::dlg_input(message= "Enter the name of the reported remote soap type","wash_soap_reported_type")$res
    }
  } else if (length(facility_reported_remote_soap_type) > 1){
    facility_reported_remote_soap_type <- tcltk::tk_select.list(facility_reported_remote_soap_type, title = "Reported remote soap type column")
    if(facility_reported_remote_soap_type == ""){
      facility_reported_remote_soap_type <- svDialogs::dlg_input(message= "Enter the name of the reported remote soap type","wash_soap_reported_type")$res
    }
  } else if (length(facility_reported_remote_soap_type) == 0) {
    facility_reported_remote_soap_type <- svDialogs::dlg_input(message= "Enter the name of the reported remote soap type","wash_soap_reported_type")$res
  }
  facility_reported_remote_soap_type_yes <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_remote_soap_type])), title = "Yes Value", multiple = TRUE)
  facility_reported_remote_soap_type_no <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_remote_soap_type])), title = "No Value", multiple = TRUE)
  facility_reported_remote_soap_type_undefined <- tcltk::tk_select.list(unique(unlist(data.list$main[,facility_reported_remote_soap_type])), title = "Undefined Value", multiple = TRUE)
}

handwash_df <- data.list$main %>%
  humind::add_handwashing_facility_cat(survey_modality = survey_modality,
                                       survey_modality_in_person = survey_modality_in_person,
                                       survey_modality_remote = survey_modality_remote,
                                       facility = facility,
                                       facility_yes = facility_yes,
                                       facility_no = facility_no,
                                       facility_no_permission = facility_no_permission,
                                       facility_undefined = facility_undefined,
                                       facility_observed_water = facility_observed_water,
                                       facility_observed_water_yes = facility_observed_water_yes,
                                       facility_observed_water_no = facility_observed_water_no,
                                       facility_observed_soap = facility_observed_soap,
                                       facility_observed_soap_yes = facility_observed_soap_yes,
                                       facility_observed_soap_no = facility_observed_soap_no,
                                       facility_observed_soap_alternative = facility_observed_soap_alternative,
                                       facility_reported = facility_reported,
                                       facility_reported_yes = facility_reported_yes,
                                       facility_reported_no = facility_reported_no,
                                       facility_reported_undefined = facility_reported_undefined,
                                       facility_reported_no_permission_soap = facility_reported_no_permission_soap,
                                       facility_reported_no_permission_soap_yes = facility_reported_no_permission_soap_yes,
                                       facility_reported_no_permission_soap_no = facility_reported_no_permission_soap_no,
                                       facility_reported_no_permission_soap_undefined = facility_reported_no_permission_soap_undefined,
                                       facility_reported_no_permission_soap_type = facility_reported_no_permission_soap_type,
                                       facility_reported_no_permission_soap_type_yes = facility_reported_no_permission_soap_type_yes,
                                       facility_reported_no_permission_soap_type_no = facility_reported_no_permission_soap_type_no,
                                       facility_reported_no_permission_soap_type_undefined = facility_reported_no_permission_soap_type_undefined,
                                       facility_reported_remote_soap = facility_reported_remote_soap,
                                       facility_reported_remote_soap_yes = facility_reported_remote_soap_yes,
                                       facility_reported_remote_soap_no = facility_reported_remote_soap_no,
                                       facility_reported_remote_soap_undefined = facility_reported_remote_soap_undefined,
                                       facility_reported_remote_soap_type = facility_reported_remote_soap_type,
                                       facility_reported_remote_soap_type_yes = facility_reported_remote_soap_type_yes,
                                       facility_reported_remote_soap_type_no = facility_reported_remote_soap_type_no,
                                       facility_reported_remote_soap_type_undefined = facility_reported_remote_soap_type_undefined) %>%
  dplyr::select(admin1, wash_handwashing_facility_jmp_cat) %>%
  dplyr::rename("handwash"=wash_handwashing_facility_jmp_cat) %>%
  dplyr::filter(handwash %in% c("basic","limited")) %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(handwash = n())

if(!file.exists("inputs/environment.Rdata")) {
  drinking_water_source <- names(data.list$main)[grepl("drinking",names(data.list$main))]
  if(length(drinking_water_source) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", drinking_water_source, "' the correct column for main source of drinking water?"), type = "yesno")$res
    if (yes_no == "no") {
      drinking_water_source <- svDialogs::dlg_input(message= "Enter the name of the main source of drinking water","wash_drinking_water_source")$res
    }
  } else if (length(drinking_water_source) > 1){
    drinking_water_source <- tcltk::tk_select.list(drinking_water_source, title = "Main source of drinking water column")
    if(drinking_water_source == ""){
      drinking_water_source <- svDialogs::dlg_input(message= "Enter the name of the main source of drinking water","wash_drinking_water_source")$res
    }
  } else if (length(drinking_water_source) == 0) {
    drinking_water_source <- svDialogs::dlg_input(message= "Enter the name of the main source of drinking water","wash_drinking_water_source")$res
  }
  improved_drinking_water <- tcltk::tk_select.list(unique(unlist(data.list$main[,drinking_water_source])), title = "Improved Values", multiple = T)
  unimproved_drinking_water <- tcltk::tk_select.list(unique(unlist(data.list$main[,drinking_water_source])), title = "Unimproved Values", multiple = T)
  surface_water <- tcltk::tk_select.list(unique(unlist(data.list$main[,drinking_water_source])), title = "Surface Water Values", multiple = T)
  undefined_drinking_water <- tcltk::tk_select.list(unique(unlist(data.list$main[,drinking_water_source])), title = "Undefined Values", multiple = T)
}

impro_water_df <- data.list$main %>%
  humind::add_drinking_water_source_cat(drinking_water_source = drinking_water_source,
                                        improved = improved_drinking_water,
                                        unimproved = unimproved_drinking_water,
                                        surface_water = surface_water,
                                        undefined = undefined_drinking_water) %>%
  dplyr::select(admin1, wash_drinking_water_source_cat) %>%
  dplyr::rename("impro_water" = wash_drinking_water_source_cat)%>%
  dplyr::filter(impro_water %in% c("improved")) %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(impro_water = n())

if(!file.exists("inputs/environment.Rdata")) {
  sanitation_facility <- names(data.list$main)[grepl("sanitation",names(data.list$main))]
  if(length(sanitation_facility) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", sanitation_facility, "' the correct column for toilet facility?"), type = "yesno")$res
    if (yes_no == "no") {
      sanitation_facility <- svDialogs::dlg_input(message= "Enter the name of the toilet facility","wash_sanitation_facility")$res
    }
  } else if (length(sanitation_facility) > 1){
    sanitation_facility <- tcltk::tk_select.list(sanitation_facility, title = "Toilet facility column")
    if(sanitation_facility == ""){
      sanitation_facility <- svDialogs::dlg_input(message= "Enter the name of the toilet facility","wash_sanitation_facility")$res
    }
  } else if (length(sanitation_facility) == 0) {a
    sanitation_facility <- svDialogs::dlg_input(message= "Enter the name of the toilet facility","wash_sanitation_facility")$res
  }
  improved_sanitation_facility <- tcltk::tk_select.list(unique(unlist(data.list$main[,sanitation_facility])), title = "Improved Values", multiple = T)
  unimproved_sanitation_facility <- tcltk::tk_select.list(unique(unlist(data.list$main[,sanitation_facility])), title = "Unimproved Values", multiple = T)
  none_sanitation_facility <- tcltk::tk_select.list(unique(unlist(data.list$main[,sanitation_facility])), title = "None Values", multiple = T)
  undefined_sanitation_facility <- tcltk::tk_select.list(unique(unlist(data.list$main[,sanitation_facility])), title = "Undefined Values", multiple = T)
}

sanitation_df <- data.list$main %>%
  humind::add_sanitation_facility_cat(sanitation_facility = sanitation_facility,
                                      improved = improved_sanitation_facility,
                                      unimproved = unimproved_sanitation_facility,
                                      none = none_sanitation_facility,
                                      undefined = undefined_sanitation_facility) %>%
  dplyr::select(admin1, wash_sanitation_facility_cat) %>%
  dplyr::rename("sanitation" = wash_sanitation_facility_cat)%>%
  dplyr::filter(sanitation %in% c("improved")) %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(sanitation = n())

if(!file.exists("inputs/environment.Rdata")) {
  wash_water_quantity <- names(data.list$main)[grepl("wash_water_quantity|quantity",names(data.list$main))]
  if(length(wash_water_quantity) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", wash_water_quantity, "' the correct column for drinking water quantity?"), type = "yesno")$res
    if (yes_no == "no") {
      wash_water_quantity <- svDialogs::dlg_input(message= "Enter the name of the wash_water_quantity","wash_water_quantity")$res
    }
  } else if (length(wash_water_quantity) > 1){
    wash_water_quantity <- tcltk::tk_select.list(wash_water_quantity, title = "Wash Water Quantity column")
    if(wash_water_quantity == ""){
      wash_water_quantity <- svDialogs::dlg_input(message= "Enter the name of the wash_water_quantity","wash_water_quantity")$res
    }
  } else if (length(wash_water_quantity) == 0) {
    wash_water_quantity <- svDialogs::dlg_input(message= "Enter the name of the wash_water_quantity","wash_water_quantity")$res
  }
}


drinking_water_df <- data.list$main %>%
  dplyr::select(admin1, wash_water_quantity) %>%
  dplyr::rename("drinking_water" = wash_water_quantity)%>%
  dplyr::filter(drinking_water %in% c("always","often")) %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(drinking_water = n())

## Health
if(!file.exists("inputs/environment.Rdata")) {
  distance_healthcare <- names(data.list$main)[grepl("health_facility_time|time|distance",names(data.list$main))]
  if(length(distance_healthcare) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", distance_healthcare, "' the correct column for travel time to health facility?"), type = "yesno")$res
    if (yes_no == "no") {
      distance_healthcare <- svDialogs::dlg_input(message= "Enter the name of the health_facility_time","distance_healthcare")$res
    }
  } else if (length(distance_healthcare) > 1){
    distance_healthcare <- tcltk::tk_select.list(distance_healthcare, title = "Healthcare facility time column")
    if(distance_healthcare == ""){
      distance_healthcare <- svDialogs::dlg_input(message= "Enter the name of the health_facility_time","distance_healthcare")$res
    }
  } else if (length(distance_healthcare) == 0) {
    distance_healthcare <- svDialogs::dlg_input(message= "Enter the name of the health_facility_time","distance_healthcare")$res
  }
}

distance_healthcare_df <- data.list$main %>%
  dplyr::select(admin1, distance_healthcare) %>%
  dplyr::filter(as.numeric(!!rlang::sym(distance_healthcare)) >= 60) %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(distance_healthcare = n())


## Health
if(!file.exists("inputs/environment.Rdata")) {
  healthcare_sheet <- tcltk::tk_select.list(sheet_names, title = "Health Individual Sheet")

  ## healthcare is needed
  ind_healthcare_needed <- names(data.list[[healthcare_sheet]])[grepl("healthcare_needed",names(data.list[[healthcare_sheet]]))]
  if(length(ind_healthcare_needed) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", ind_healthcare_needed, "' the correct column for healthcare is needed?"), type = "yesno")$res
    if (yes_no == "no") {
      ind_healthcare_needed <- svDialogs::dlg_input(message= "Enter the name of the healthcare is needed","health_ind_healthcare_needed")$res
    }
  } else if (length(ind_healthcare_needed ) > 1){
    ind_healthcare_needed <- tcltk::tk_select.list(ind_healthcare_needed, title = "Healthcare is needed column")
    if(ind_healthcare_needed  == ""){
      ind_healthcare_needed  <- svDialogs::dlg_input(message= "Enter the name of the healthcare is needed","health_ind_healthcare_needed")$res
    }
  } else if (length(ind_healthcare_needed) == 0) {
    ind_healthcare_needed <- svDialogs::dlg_input(message= "Enter the name of the healthcare is needed","health_ind_healthcare_needed")$res
  }
  ind_healthcare_needed_levels <- unique(unlist(data.list[[healthcare_sheet]][,ind_healthcare_needed]))
  ind_healthcare_needed_levels <- ind_healthcare_needed_levels[!is.na(ind_healthcare_needed_levels)]

  ## healthcare is needed
  ind_healthcare_received <- names(data.list[[healthcare_sheet]])[grepl("healthcare_received",names(data.list[[healthcare_sheet]]))]
  if(length(ind_healthcare_received) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", ind_healthcare_received, "' the correct column for healthcare is received?"), type = "yesno")$res
    if (yes_no == "no") {
      ind_healthcare_received <- svDialogs::dlg_input(message= "Enter the name of the healthcare is received","health_ind_healthcare_received")$res
    }
  } else if (length(ind_healthcare_received ) > 1){
    ind_healthcare_received <- tcltk::tk_select.list(ind_healthcare_received, title = "Healthcare is received column")
    if(ind_healthcare_received  == ""){
      ind_healthcare_received  <- svDialogs::dlg_input(message= "Enter the name of the healthcare is received","health_ind_healthcare_received")$res
    }
  } else if (length(ind_healthcare_received) == 0) {
    ind_healthcare_received <- svDialogs::dlg_input(message= "Enter the name of the healthcare is received","health_ind_healthcare_received")$res
  }
  ind_healthcare_received_levels <- unique(unlist(data.list[[healthcare_sheet]][,ind_healthcare_received]))
  ind_healthcare_received_levels <- ind_healthcare_received_levels[!is.na(ind_healthcare_received_levels)]
  ## ind age
  ind_age <- names(data.list[[healthcare_sheet]])[grepl("ind_age",names(data.list[[healthcare_sheet]]))]
  if(length(ind_age) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", ind_age, "' the correct column for age of individual?"), type = "yesno")$res
    if (yes_no == "no") {
      ind_age <- svDialogs::dlg_input(message= "Enter the name of the age of individual","ind_age")$res
    }
  } else if (length(ind_age ) > 1){
    ind_age <- tcltk::tk_select.list(ind_age, title = "Age of individual column")
    if(ind_age  == ""){
      ind_age  <- svDialogs::dlg_input(message= "Enter the name of the age of individual","ind_age")$res
    }
  } else if (length(ind_age) == 0) {
    ind_age <- svDialogs::dlg_input(message= "Enter the name of the age of individual","ind_age")$res
  }

  ## UUID Health Loop
  uuid_health_loop <- names(data.list[[healthcare_sheet]])[grepl("uuid",names(data.list[[healthcare_sheet]]))]
  if(length(uuid_health_loop) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_health_loop, "' the correct column for uuid of HH?"), type = "yesno")$res
    if (yes_no == "no") {
      uuid_health_loop <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
    }
  } else if (length(uuid_health_loop ) > 1){
    uuid_health_loop <- tcltk::tk_select.list(uuid_health_loop, title = "Uuid of HH in Health loop column")
    if(uuid_health_loop  == ""){
      uuid_health_loop  <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
    }
  } else if (length(uuid_health_loop) == 0){
    uuid_health_loop <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
  }

  ## UUID Main
  uuid_main <- names(data.list$main)[grepl("uuid",names(data.list$main))]
  if(length(uuid_main) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_main, "' the correct column for uuid of HH?"), type = "yesno")$res
    if (yes_no == "no") {
      uuid_main <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
    }
  } else if (length(uuid_main ) > 1){
    uuid_main <- tcltk::tk_select.list(uuid_main, title = "Uuid of HH column")
    if(uuid_main  == ""){
      uuid_main  <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
    }
  } else if (length(uuid_main) == 0){
    uuid_main <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
  }
}

unmet_loop_df <- data.list[[healthcare_sheet]] %>%
  humind::add_loop_healthcare_needed_cat(ind_healthcare_needed = ind_healthcare_needed,
                                         ind_healthcare_needed_levels = ind_healthcare_needed_levels,
                                         ind_healthcare_received = ind_healthcare_received,
                                         ind_healthcare_received_levels = ind_healthcare_received_levels,
                                         ind_age = ind_age)%>%
  dplyr::rename(!!rlang::sym(uuid_main) := uuid_health_loop)

unmet_health_df <- humind::add_loop_healthcare_needed_cat_to_main(data.list$main,
                                                                  unmet_loop_df ,
                                                                  id_col_main = uuid_main,
                                                                  id_col_loop = uuid_main) %>%
  dplyr::select(admin1, health_ind_healthcare_needed_yes_unmet_n) %>%
  dplyr::filter(as.numeric(health_ind_healthcare_needed_yes_unmet_n) > 0) %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(unmet_healthcare = n())


## Nutrition
if(!file.exists("inputs/environment.Rdata")) {
  nut_sheet <- tcltk::tk_select.list(sheet_names, title = "Nutrtion Individual Sheet")

  ## under5 sick
  under5_sick <- names(data.list[[nut_sheet]])[grepl("under5",names(data.list[[nut_sheet]]))]
  if(length(under5_sick) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", under5_sick, "' the correct column for under5 sick?"), type = "yesno")$res
    if (yes_no == "no") {
      under5_sick <- svDialogs::dlg_input(message= "Enter the name of the under5 sick","nut_ind_under5_sick_yn")$res
    }
  } else if (length(under5_sick ) > 1){
    under5_sick <- tcltk::tk_select.list(under5_sick, title = "Under5 sick column")
    if(under5_sick  == ""){
      under5_sick  <- svDialogs::dlg_input(message= "Enter the name of the under5 sick","nut_ind_under5_sick_yn")$res
    }
  } else if (length(under5_sick) == 0) {
    under5_sick <- svDialogs::dlg_input(message= "Enter the name of the under5 sick","nut_ind_under5_sick_yn")$res
  }
  under5_sick_yes <- tcltk::tk_select.list(unique(unlist(data.list[[nut_sheet]][,under5_sick])), title = "Yes value")
  ## UUID Main
  uuid_nut <- names(data.list[[nut_sheet]])[grepl("uuid",names(data.list[[nut_sheet]]))]
  if(length(uuid_nut) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_nut, "' the correct column for uuid of HH?"), type = "yesno")$res
    if (yes_no == "no") {
      uuid_nut <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
    }
  } else if (length(uuid_nut ) > 1){
    uuid_nut <- tcltk::tk_select.list(uuid_nut, title = "Uuid of HH in Nutrition column")
    if(uuid_nut  == ""){
      uuid_nut  <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
    }
  } else if (length(uuid_nut) == 0){
    uuid_nut <- svDialogs::dlg_input(message= "Enter the name of the uuid of HH","uuid")$res
  }
}

nut_under5_sick_loop <- data.list[[nut_sheet]] %>%
  dplyr::mutate(under5_sick_n = ifelse(!!rlang::sym(under5_sick) == under5_sick_yes,1,0)) %>%
  dplyr::rename(!!rlang::sym(uuid_main) := uuid_nut) %>%
  dplyr::group_by(!!rlang::sym(uuid_main)) %>%
  dplyr::summarise(under5_sick_n = sum(under5_sick_n, na.rm = T))

nut_under5_sick_df <- data.list$main %>%
  dplyr::left_join(nut_under5_sick_loop) %>%
  dplyr::select(admin1, under5_sick_n) %>%
  dplyr::filter(as.numeric(under5_sick_n) > 0) %>%
  dplyr::group_by(!!rlang::sym(admin1)) %>%
  dplyr::summarise(children_sick = n())

ph_int_table <- ph_int_table %>%
  dplyr::mutate(fcs = fcs_df$fcs[match(!!rlang::sym(admin1),fcs_df[[admin1]])] / num_observartion,
                rcsi = rcsi_df$rcsi[match(!!rlang::sym(admin1),rcsi_df[[admin1]])] / num_observartion,
                fcs_phase = fsl_phase_df$fcs_phase[match(!!rlang::sym(admin1),fsl_phase_df[[admin1]])] / num_observartion,
                lcsi = lcsi_df$lcsi[match(!!rlang::sym(admin1),lcsi_df[[admin1]])] / num_observartion,
                handwash = handwash_df$handwash[match(!!rlang::sym(admin1),handwash_df[[admin1]])] / num_observartion,
                impro_water = impro_water_df$impro_water[match(!!rlang::sym(admin1),impro_water_df[[admin1]])] / num_observartion,
                sanitation = sanitation_df$sanitation[match(!!rlang::sym(admin1),sanitation_df[[admin1]])] / num_observartion,
                drinking_water = drinking_water_df$drinking_water[match(!!rlang::sym(admin1),drinking_water_df[[admin1]])] / num_observartion,
                distance_healthcare = distance_healthcare_df$distance_healthcare[match(!!rlang::sym(admin1),distance_healthcare_df[[admin1]])] / num_observartion,
                unmet_healthcare = unmet_health_df$unmet_healthcare[match(!!rlang::sym(admin1),unmet_health_df[[admin1]])] / num_observartion,
                children_sick = nut_under5_sick_df$children_sick[match(!!rlang::sym(admin1),nut_under5_sick_df[[admin1]])] / num_observartion,
                hhs = hhs_df$hhs[match(!!rlang::sym(admin1),hhs_df[[admin1]])] / num_observartion)

ph_int_cat <- ph_int_table %>%
  dplyr::mutate(children_sick = case_when(children_sick <= 0.1 ~ "Low",
                                          children_sick <= 0.2 ~ "Moderate",
                                          children_sick <= 0.3 ~ "High",
                                          children_sick <= 0.4 ~ "Very high",
                                          children_sick > 0.4 ~ "Extremely high",
                                          TRUE ~ NA),
                unmet_healthcare = case_when(unmet_healthcare <= 0.1 ~ "Low",
                                             unmet_healthcare <= 0.2 ~ "Moderate",
                                             unmet_healthcare <= 0.3 ~ "High",
                                             unmet_healthcare <= 0.4 ~ "Very high",
                                             unmet_healthcare > 0.4 ~ "Extremely high",
                                             TRUE ~ NA),
                fcs_phase = case_when(fcs_phase <= 0.1 ~ "Low",
                                      fcs_phase <= 0.2 ~ "Moderate",
                                      fcs_phase <= 0.3 ~ "High",
                                      fcs_phase <= 0.4 ~ "Very high",
                                      fcs_phase > 0.4 ~ "Extremely high",
                                      TRUE ~ NA),
                fcs = case_when(fcs <= 0.1 ~ "Low",
                                fcs <= 0.2 ~ "Moderate",
                                fcs <= 0.3 ~ "High",
                                fcs <= 0.4 ~ "Very high",
                                fcs > 0.4 ~ "Extremely high",
                                TRUE ~ NA),
                rcsi = case_when(rcsi <= 0.1 ~ "Low",
                                 rcsi <= 0.2 ~ "Moderate",
                                 rcsi <= 0.3 ~ "High",
                                 rcsi <= 0.4 ~ "Very high",
                                 rcsi > 0.4 ~ "Extremely high",
                                 TRUE ~ NA),
                hhs = case_when(hhs <= 0.1 ~ "Low",
                                hhs <= 0.2 ~ "Moderate",
                                hhs <= 0.3 ~ "High",
                                hhs <= 0.4 ~ "Very high",
                                hhs > 0.4 ~ "Extremely high",
                                TRUE ~ NA),
                lcsi = case_when(lcsi <= 0.1 ~ "Low",
                                 lcsi <= 0.2 ~ "Moderate",
                                 lcsi <= 0.3 ~ "High",
                                 lcsi <= 0.4 ~ "Very high",
                                 lcsi > 0.4 ~ "Extremely high",
                                 TRUE ~ NA),
                impro_water = case_when(impro_water >= 0.8 ~ "Low",
                                        impro_water >= 0.6 ~ "Moderate",
                                        impro_water >= 0.4 ~ "High",
                                        impro_water >= 0.2 ~ "Very high",
                                        impro_water < 0.2 ~ "Extremely high",
                                        TRUE ~ NA),
                sanitation = case_when(sanitation >= 0.8 ~ "Low",
                                       sanitation >= 0.6 ~ "Moderate",
                                       sanitation >= 0.4 ~ "High",
                                       sanitation >= 0.2 ~ "Very high",
                                       sanitation < 0.2 ~ "Extremely high",
                                       TRUE ~ NA),
                handwash = case_when(handwash >= 0.8 ~ "Low",
                                     handwash >= 0.6 ~ "Moderate",
                                     handwash >= 0.4 ~ "High",
                                     handwash >= 0.2 ~ "Very high",
                                     handwash < 0.2 ~ "Extremely high",
                                     TRUE ~ NA),
                drinking_water = case_when(drinking_water <= 0.2 ~ "Low",
                                           drinking_water <= 0.4 ~ "Moderate",
                                           drinking_water <= 0.6 ~ "High",
                                           drinking_water <= 0.8 ~ "Very high",
                                           drinking_water > 0.8 ~ "Extremely high",
                                           TRUE ~ NA),
                distance_healthcare = case_when(distance_healthcare <= 0.1 ~ "Low",
                                                distance_healthcare <= 0.2 ~ "Moderate",
                                                distance_healthcare <= 0.3 ~ "High",
                                                distance_healthcare <= 0.4 ~ "Very high",
                                                distance_healthcare > 0.4 ~ "Extremely high",
                                                TRUE ~ NA))

if(mort_collected == "yes") {
  ph_int_cat <- ph_int_cat %>%
    dplyr::mutate(mort = case_when(mort <= 0.5 ~ "Low",
                                   mort <= 0.75 ~ "Moderate",
                                   mort <= 1 ~ "High",
                                   mort <= 1.4 ~ "Very high",
                                   mort > 1.4 ~ "Extremely high",
                                   TRUE ~ NA))
  ph_int_table <- ph_int_table %>%
    dplyr::arrange(desc(mort)) %>%
    dplyr::mutate(mort_lci = mort_data$mort_lci[match(!!rlang::sym(admin1),mort_data$admin1)],
                  mort_uci = mort_data$mort_uci[match(!!rlang::sym(admin1),mort_data$admin1)],
                  mort = paste0(mort," [",mort_lci," - ",mort_uci,"]")) %>%
    dplyr::select(-c(mort_lci,mort_uci))
}

ph_int_table <- ph_int_table %>%
  dplyr::mutate_at(vars(children_sick,unmet_healthcare,amn_phase,afi_phase,fcs_phase,fcs,rcsi,hhs,
                     lcsi,impro_water,drinking_water,sanitation,handwash,distance_healthcare),
                   ~ ifelse(is.na(.),NA,paste0(round(.*100,2),"%")))



source("src/output_table.R")

save.ph.integrated.tables(ph_int_table,ph_int_cat,"PH_Integrated_Table",mort = T,use_template = T)

## save environment
list_of_var <- c("admin1","fsl_fcs_cereal","fsl_fcs_legumes","fsl_fcs_veg","FSL_indicators",
                 "fsl_fcs_fruit","fsl_fcs_meat","fsl_fcs_dairy","fsl_fcs_sugar",
                 "fsl_fcs_oil","fsl_rcsi_lessquality","fsl_rcsi_borrow",
                 "fsl_rcsi_mealsize","fsl_rcsi_mealadult","fsl_rcsi_mealnb",
                 "fsl_hhs_nofoodhh","fsl_hhs_nofoodhh_freq","fsl_hhs_sleephungry",
                 "fsl_hhs_sleephungry_freq","fsl_hhs_alldaynight","fsl_hhs_alldaynight_freq",
                 "yes_answer","no_answer","rarely_answer","sometimes_answer", "wash_water_quantity",
                 "often_answer","fsl_lcsi_stress1","fsl_lcsi_stress2","fsl_lcsi_stress3",
                 "fsl_lcsi_stress4","fsl_lcsi_crisis1","fsl_lcsi_crisis2","healthcare_sheet",
                 "sanitation_facility","improved_sanitation_facility","unimproved_sanitation_facility",
                 "none_sanitation_facility","undefined_sanitation_facility","undefined_drinking_water",
                 "unimproved_sanitation_facility","none_sanitation_facility","undefined_sanitation_facility",
                 "drinking_water_source","improved_drinking_water","unimproved_drinking_water","surface_water",
                 "fsl_lcsi_crisis3","fsl_lcsi_emergency1","fsl_lcsi_emergency2",
                 "hhs_check_columns","hhs_check_columns_freq",
                 "survey_modality","survey_modality_in_person","survey_modality_remote",
                 "facility","facility_yes","facility_no","facility_no_permission",
                 "facility_undefined","facility_observed_water","facility_observed_water_yes",
                 "facility_observed_water_no","facility_observed_soap","lcsi_check_columns",
                 "facility_observed_soap_yes","facility_observed_soap_no","facility_observed_soap_alternative","facility_reported",
                 "facility_reported_yes","facility_reported_no","facility_reported_undefined","facility_reported_no_permission_soap",
                 "facility_reported_no_permission_soap_yes","facility_reported_no_permission_soap_no",
                 "facility_reported_no_permission_soap_undefined","facility_reported_no_permission_soap_type",
                 "facility_reported_no_permission_soap_type_yes","facility_reported_no_permission_soap_type_no",
                 "facility_reported_no_permission_soap_type_undefined","facility_reported_remote_soap",
                 "facility_reported_remote_soap_yes","facility_reported_remote_soap_no",
                 "ind_healthcare_needed","ind_healthcare_needed_levels","ind_healthcare_received",
                 "ind_healthcare_received_levels","ind_age","uuid_health_loop","uuid_main","healthcare_sheet",
                 "nut_sheet","under5_sick","uuid_nut","under5_sick_yes",
                 "facility_reported_remote_soap_undefined","facility_reported_remote_soap_type",
                 "facility_reported_remote_soap_type_yes","facility_reported_remote_soap_type_no","facility_reported_remote_soap_type_undefined",
                 "fsl_lcsi_emergency3","yes_val","no_val","exhausted_val","not_applicable_val","distance_healthcare")

if(!file.exists("inputs/environment.Rdata")){
  save(list = list_of_var, file = "inputs/environment.Rdata")
}

cat("\n> Creation of table completed! You can check your output folder.")
