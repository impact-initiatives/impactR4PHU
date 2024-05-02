################################################################################
### FORMAT DATASET
################################################################################

# create shorthands to make working with the data easier:
main <- data.list$main
if(file.exists("inputs/environment.Rdata")) {
  load("inputs/environment.Rdata")
}
if(!file.exists("inputs/environment.Rdata")) {
  FSL_indicators <- tcltk::tk_select.list(c("FCS","rCSI","HHS","LCSI","HDDS"), title = "FSL indicators", multiple = T)
}
################################################################################
# FSL
if(!file.exists("inputs/environment.Rdata")) {
  if ("FCS" %in% FSL_indicators){
    fsl_fcs_cereal <- names(main)[grepl("cereal",names(main))]
    if(length(fsl_fcs_cereal) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_cereal, "' the correct fcs_cereal column?"), type = "yesno")$res
      if(yes_no == "no"){
        fsl_fcs_cereal <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_cereal","fsl_fcs_cereal")$res
      }
    } else if (length(fsl_fcs_cereal) > 1){
      fsl_fcs_cereal <- tcltk::tk_select.list(fsl_fcs_cereal, title = "FCS Cereal column")
    } else if (length(fsl_fcs_cereal) == 0) {
      fsl_fcs_cereal <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_cereal","fsl_fcs_cereal")$res
    }

    fsl_fcs_legumes <- names(main)[grepl("legume|pulse|bean|nuts",names(main))]
    if(length(fsl_fcs_legumes) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_legumes, "' the correct fsl_fcs_legumes column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_legumes <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_legumes","fsl_fcs_legumes")$res
      }
    } else if (length(fsl_fcs_legumes) > 1){
      fsl_fcs_legumes <- tcltk::tk_select.list(fsl_fcs_legumes, title = "FCS Legumes/Pulses column")
    } else if (length(fsl_fcs_legumes) == 0) {
      fsl_fcs_legumes <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_legumes","fsl_fcs_legumes")$res
    }

    fsl_fcs_veg <- names(main)[grepl("veg",names(main))]
    if(length(fsl_fcs_veg) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_veg, "' the correct fsl_fcs_veg column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_veg <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_veg","fsl_fcs_veg")$res
      }
    } else if (length(fsl_fcs_veg) > 1){
      fsl_fcs_veg <- tcltk::tk_select.list(fsl_fcs_veg, title = "FCS Vegetables column")
    } else if (length(fsl_fcs_veg) == 0) {
      fsl_fcs_veg <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_veg","fsl_fcs_veg")$res
    }

    fsl_fcs_fruit <- names(main)[grepl("fruit",names(main))]
    if(length(fsl_fcs_fruit) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_fruit, "' the correct fsl_fcs_fruit column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_fruit <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_fruit","fsl_fcs_fruit")$res
      }
    } else if (length(fsl_fcs_fruit) > 1){
      fsl_fcs_fruit <- tcltk::tk_select.list(fsl_fcs_fruit, title = "FCS Fruits column")
    } else if (length(fsl_fcs_fruit) == 0) {
      fsl_fcs_fruit <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_fruit","fsl_fcs_fruit")$res
    }

    fsl_fcs_meat <- names(main)[grepl("meat",names(main))]
    if(length(fsl_fcs_meat) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_meat, "' the correct fsl_fcs_meat column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_meat <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_meat","fsl_fcs_meat")$res
      }
    } else if (length(fsl_fcs_meat) > 1){
      fsl_fcs_meat <- tcltk::tk_select.list(fsl_fcs_meat, title = "FCS Meat column")
    } else if (length(fsl_fcs_meat) == 0) {
      fsl_fcs_meat <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_meat","fsl_fcs_meat")$res
    }

    fsl_fcs_dairy <- names(main)[grepl("dairy|milk",names(main))]
    if(length(fsl_fcs_dairy) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_dairy, "' the correct fsl_fcs_dairy column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_dairy <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_dairy","fsl_fcs_dairy")$res
      }
    } else if (length(fsl_fcs_dairy) > 1){
      fsl_fcs_dairy <- tcltk::tk_select.list(fsl_fcs_dairy, title = "FCS Dairy/Milk column")
    } else if (length(fsl_fcs_dairy) == 0) {
      fsl_fcs_dairy <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_dairy","fsl_fcs_dairy")$res
    }

    fsl_fcs_sugar <- names(main)[grepl("sugar",names(main))]
    if(length(fsl_fcs_sugar) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_sugar, "' the correct fsl_fcs_sugar column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_sugar <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_sugar","fsl_fcs_sugar")$res
      }
    } else if (length(fsl_fcs_sugar) > 1){
      fsl_fcs_sugar <- tcltk::tk_select.list(fsl_fcs_sugar, title = "FCS Sugar column")
    } else if (length(fsl_fcs_sugar) == 0) {
      fsl_fcs_sugar <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_sugar","fsl_fcs_sugar")$res
    }

    fsl_fcs_oil <- names(main)[grepl("oil",names(main))]
    if(length(fsl_fcs_oil) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_fcs_oil, "' the correct fsl_fcs_oil column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_fcs_oil <- svDialogs::dlg_input(message= "Enter the name of the fsl_fcs_oil","fsl_fcs_oil")$res
      }
    } else if (length(fsl_fcs_oil) > 1){
      fsl_fcs_oil <- tcltk::tk_select.list(fsl_fcs_oil, title = "FCS Oil column")
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

    if(!all(fcs_check_columns %in% names(main))) {
      svDialogs::dlg_message("Please check if the FCS columns selected are correct and available in the dataset")
      stop("Please check if the FCS columns selected are correct and available in the dataset")
    } else {
      main <- main %>%
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
    main <- main %>%
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
    fsl_rcsi_lessquality <- names(main)[grepl("less|quality|lessquality",names(main))]
    if(length(fsl_rcsi_lessquality) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_lessquality, "' the correct fsl_rcsi_lessquality column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_lessquality <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_lessquality","fsl_rcsi_lessquality")$res
      }
    } else if (length(fsl_rcsi_lessquality) > 1){
      fsl_rcsi_lessquality <- tcltk::tk_select.list(fsl_rcsi_lessquality, title = "rCSI Less Quality column")
    } else if (length(fsl_rcsi_lessquality) == 0) {
      fsl_rcsi_lessquality <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_lessquality","fsl_rcsi_lessquality")$res
    }

    fsl_rcsi_borrow <- names(main)[grepl("borrow",names(main))]
    if(length(fsl_rcsi_borrow) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_borrow, "' the correct fsl_rcsi_borrow column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_borrow <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_borrow","fsl_rcsi_borrow")$res
      }
    } else if (length(fsl_rcsi_borrow) > 1){
      fsl_rcsi_borrow <- tcltk::tk_select.list(fsl_rcsi_borrow, title = "rCSI Borrow column")
    } else if (length(fsl_rcsi_borrow) == 0) {
      fsl_rcsi_borrow <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_borrow","fsl_rcsi_borrow")$res
    }

    fsl_rcsi_mealsize <- names(main)[grepl("mealsize|limit|portion",names(main))]
    if(length(fsl_rcsi_mealsize) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_mealsize, "' the correct fsl_rcsi_mealsize column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_mealsize <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealsize","fsl_rcsi_mealsize")$res
      }
    } else if (length(fsl_rcsi_mealsize) > 1){
      fsl_rcsi_mealsize <- tcltk::tk_select.list(fsl_rcsi_mealsize, title = "rCSI Meal Size column")
    } else if (length(fsl_rcsi_mealsize) == 0) {
      fsl_rcsi_mealsize <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealsize","fsl_rcsi_mealsize")$res
    }

    fsl_rcsi_mealadult <- names(main)[grepl("mealadult|restrict",names(main))]
    if(length(fsl_rcsi_mealadult) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_mealadult, "' the correct fsl_rcsi_mealadult column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_mealadult <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealadult","fsl_rcsi_mealadult")$res
      }
    } else if (length(fsl_rcsi_mealadult) > 1){
      fsl_rcsi_mealadult <- tcltk::tk_select.list(fsl_rcsi_mealadult, title = "rCSI Meal Adult column")
    } else if (length(fsl_rcsi_mealadult) == 0) {
      fsl_rcsi_mealadult <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealadult","fsl_rcsi_mealadult")$res
    }

    fsl_rcsi_mealnb <- names(main)[grepl("mealnb|reduce|meals",names(main))]
    if(length(fsl_rcsi_mealnb) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_rcsi_mealnb, "' the correct fsl_rcsi_mealnb column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_rcsi_mealnb <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealnb","fsl_rcsi_mealnb")$res
      }
    } else if (length(fsl_rcsi_mealnb) > 1){
      fsl_rcsi_mealnb <- tcltk::tk_select.list(fsl_rcsi_mealnb, title = "rCSI Meal Number column")
    } else if (length(fsl_rcsi_mealnb) == 0) {
      fsl_rcsi_mealnb <- svDialogs::dlg_input(message= "Enter the name of the fsl_rcsi_mealnb","fsl_rcsi_mealnb")$res
    }

    rcsi_check_columns <- c(fsl_rcsi_lessquality,
                            fsl_rcsi_borrow,
                            fsl_rcsi_mealsize,
                            fsl_rcsi_mealadult,
                            fsl_rcsi_mealnb)

    if(!all(rcsi_check_columns %in% names(main))) {
      svDialogs::dlg_message("Please check if the rCSI columns selected are correct and available in the dataset")
      stop("Please check if the rCSI columns selected are correct and available in the dataset")
    } else {
      main <- main %>%
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
    main <- main %>%
      impactR4PHU::add_rcsi(fsl_rcsi_lessquality = fsl_rcsi_lessquality,
                            fsl_rcsi_borrow = fsl_rcsi_borrow,
                            fsl_rcsi_mealsize = fsl_rcsi_mealsize,
                            fsl_rcsi_mealadult = fsl_rcsi_mealadult,
                            fsl_rcsi_mealnb = fsl_rcsi_mealnb)
  }
}

if(!file.exists("inputs/environment.Rdata")) {
  if ("HHS" %in% FSL_indicators){
    fsl_hhs_nofoodhh <- names(main)[grepl("nofood",names(main))]
    if(length(fsl_hhs_nofoodhh) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_nofoodhh, "' the correct fsl_hhs_nofoodhh column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_nofoodhh <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh","fsl_hhs_nofoodhh")$res
      }
    } else if (length(fsl_hhs_nofoodhh) > 1){
      fsl_hhs_nofoodhh <- tcltk::tk_select.list(fsl_hhs_nofoodhh, title = "HHS No Food HH column")
    } else if (length(fsl_hhs_nofoodhh) == 0) {
      fsl_hhs_nofoodhh <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh","fsl_hhs_nofoodhh")$res
    }

    fsl_hhs_nofoodhh_freq <- names(main)[grepl("nofood",names(main))]
    if(length(fsl_hhs_nofoodhh_freq) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_nofoodhh_freq, "' the correct fsl_hhs_nofoodhh_freq column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_nofoodhh_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh_freq","fsl_hhs_nofoodhh_freq")$res
      }
    } else if (length(fsl_hhs_nofoodhh_freq) > 1){
      fsl_hhs_nofoodhh_freq <- tcltk::tk_select.list(fsl_hhs_nofoodhh_freq, title = "HHS No Food HH Freq column")
    } else if (length(fsl_hhs_nofoodhh_freq) == 0) {
      fsl_hhs_nofoodhh_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_nofoodhh_freq","fsl_hhs_nofoodhh_freq")$res
    }

    fsl_hhs_sleephungry <- names(main)[grepl("sleephungry",names(main))]
    if(length(fsl_hhs_sleephungry) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_sleephungry, "' the correct fsl_hhs_sleephungry column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_sleephungry <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry","fsl_hhs_sleephungry")$res
      }
    } else if (length(fsl_hhs_sleephungry) > 1){
      fsl_hhs_sleephungry <- tcltk::tk_select.list(fsl_hhs_sleephungry, title = "HHS Sleep Hungry column")
    } else if (length(fsl_hhs_sleephungry) == 0) {
      fsl_hhs_sleephungry <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry","fsl_hhs_sleephungry")$res
    }

    fsl_hhs_sleephungry_freq <- names(main)[grepl("sleephungry",names(main))]
    if(length(fsl_hhs_sleephungry_freq) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_sleephungry_freq, "' the correct fsl_hhs_sleephungry_freq column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_sleephungry_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry_freq","fsl_hhs_sleephungry_freq")$res
      }
    } else if (length(fsl_hhs_sleephungry_freq) > 1){
      fsl_hhs_sleephungry_freq <- tcltk::tk_select.list(fsl_hhs_sleephungry_freq, title = "HHS Sleep Hungry Freq column")
    } else if (length(fsl_hhs_sleephungry_freq) == 0) {
      fsl_hhs_sleephungry_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_sleephungry_freq","fsl_hhs_sleephungry_freq")$res
    }

    fsl_hhs_alldaynight <- names(main)[grepl("alldaynight|daynoteating",names(main))]
    if(length(fsl_hhs_alldaynight) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_alldaynight, "' the correct fsl_hhs_alldaynight column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_alldaynight <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight","fsl_hhs_alldaynight")$res
      }
    } else if (length(fsl_hhs_alldaynight) > 1){
      fsl_hhs_alldaynight <- tcltk::tk_select.list(fsl_hhs_alldaynight, title = "HHS All Day Night column")
    } else if (length(fsl_hhs_alldaynight) == 0) {
      fsl_hhs_alldaynight <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight","fsl_hhs_alldaynight")$res
    }

    fsl_hhs_alldaynight_freq <- names(main)[grepl("alldaynight|daynoteating",names(main))]
    if(length(fsl_hhs_alldaynight_freq) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hhs_alldaynight_freq, "' the correct fsl_hhs_alldaynight_freq column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hhs_alldaynight_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight_freq","fsl_hhs_alldaynight_freq")$res
      }
    } else if (length(fsl_hhs_alldaynight_freq) > 1){
      fsl_hhs_alldaynight_freq <- tcltk::tk_select.list(fsl_hhs_alldaynight_freq, title = "HHS all Day Night Freq column")
    } else if (length(fsl_hhs_alldaynight_freq) == 0) {
      fsl_hhs_alldaynight_freq <- svDialogs::dlg_input(message= "Enter the name of the fsl_hhs_alldaynight_freq","fsl_hhs_alldaynight_freq")$res
    }

    hhs_check_columns <- c(fsl_hhs_nofoodhh,
                           fsl_hhs_sleephungry,
                           fsl_hhs_alldaynight)

    hhs_check_columns_freq <- c(fsl_hhs_nofoodhh_freq,
                                fsl_hhs_sleephungry_freq,
                                fsl_hhs_alldaynight_freq)

    if(!all(c(hhs_check_columns,hhs_check_columns_freq) %in% names(main))) {
      svDialogs::dlg_message("Please check if the HHS columns selected are correct and available in the dataset")
      stop("Please check if the HHS columns selected are correct and available in the dataset")
    } else{
      yes_answer <- tcltk::tk_select.list(dplyr::pull(main[,hhs_check_columns]) %>% unique, title = "Yes Value")
      no_answer <- tcltk::tk_select.list(dplyr::pull(main[,hhs_check_columns]) %>% unique, title = "No Value")
      rarely_answer <- tcltk::tk_select.list(dplyr::pull(main[,hhs_check_columns_freq]) %>% unique, title = "Rarely Value")
      sometimes_answer <- tcltk::tk_select.list(dplyr::pull(main[,hhs_check_columns_freq]) %>% unique, title = "Sometimes Value")
      often_answer <- tcltk::tk_select.list(dplyr::pull(main[,hhs_check_columns_freq]) %>% unique, title = "Often Value")
      main <- main %>%
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
    main <- main %>%
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
    fsl_lcsi_stress1 <- names(main)[grepl("stress|stress1",names(main))]
    if(length(fsl_lcsi_stress1) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_stress1, "' the correct fsl_lcsi_stress1 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_stress1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress1","fsl_lcsi_stress1")$res
      }
    } else if (length(fsl_lcsi_stress1) > 1){
      fsl_lcsi_stress1 <- tcltk::tk_select.list(fsl_lcsi_stress1, title = "LCSI Stress 1 column")
    } else if (length(fsl_lcsi_stress1) == 0) {
      fsl_lcsi_stress1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress1","fsl_lcsi_stress1")$res
    }
    fsl_lcsi_stress2 <- names(main)[grepl("stress|stress2",names(main))]
    if(length(fsl_lcsi_stress2) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_stress2, "' the correct fsl_lcsi_stress2 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_stress2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress2","fsl_lcsi_stress2")$res
      }
    } else if (length(fsl_lcsi_stress2) > 1){
      fsl_lcsi_stress2 <- tcltk::tk_select.list(fsl_lcsi_stress2, title = "LCSI Stress 2 column")
    } else if (length(fsl_lcsi_stress2) == 0) {
      fsl_lcsi_stress2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress2","fsl_lcsi_stress2")$res
    }

    fsl_lcsi_stress3 <- names(main)[grepl("stress|stress3",names(main))]
    if(length(fsl_lcsi_stress3) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_stress3, "' the correct fsl_lcsi_stress3 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_stress3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress3","fsl_lcsi_stress3")$res
      }
    } else if (length(fsl_lcsi_stress3) > 1){
      fsl_lcsi_stress3 <- tcltk::tk_select.list(fsl_lcsi_stress3, title = "LCSI Stress 3 column")
    } else if (length(fsl_lcsi_stress3) == 0) {
      fsl_lcsi_stress3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress3","fsl_lcsi_stress3")$res
    }

    fsl_lcsi_stress4 <- names(main)[grepl("stress|stress4",names(main))]
    if(length(fsl_lcsi_stress4) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_stress4, "' the correct fsl_lcsi_stress4 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_stress4 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress4","fsl_lcsi_stress4")$res
      }
    } else if (length(fsl_lcsi_stress4) > 1){
      fsl_lcsi_stress4 <- tcltk::tk_select.list(fsl_lcsi_stress4, title = "LCSI Stress 4 column")
    } else if (length(fsl_lcsi_stress4) == 0) {
      fsl_lcsi_stress4 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_stress4","fsl_lcsi_stress4")$res
    }

    fsl_lcsi_crisis1 <- names(main)[grepl("crisis|crisis1",names(main))]
    if(length(fsl_lcsi_crisis1) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_crisis1, "' the correct fsl_lcsi_crisis1 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_crisis1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis1","fsl_lcsi_crisis1")$res
      }
    } else if (length(fsl_lcsi_crisis1) > 1){
      fsl_lcsi_crisis1 <- tcltk::tk_select.list(fsl_lcsi_crisis1, title = "LCSI Crisis 1 column")
    } else if (length(fsl_lcsi_crisis1) == 0) {
      fsl_lcsi_crisis1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis1","fsl_lcsi_crisis1")$res
    }

    fsl_lcsi_crisis2 <- names(main)[grepl("crisis|crisis2",names(main))]
    if(length(fsl_lcsi_crisis2) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_crisis2, "' the correct fsl_lcsi_crisis2 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_crisis2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis2","fsl_lcsi_crisis2")$res
      }
    } else if (length(fsl_lcsi_crisis2) > 1){
      fsl_lcsi_crisis2 <- tcltk::tk_select.list(fsl_lcsi_crisis2, title = "LCSI Crisis 2 column")
    } else if (length(fsl_lcsi_crisis2) == 0) {
      fsl_lcsi_crisis2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis2","fsl_lcsi_crisis2")$res
    }

    fsl_lcsi_crisis3 <- names(main)[grepl("crisis|crisis3",names(main))]
    if(length(fsl_lcsi_crisis3) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_crisis3, "' the correct fsl_lcsi_crisis3 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_crisis3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis3","fsl_lcsi_crisis3")$res
      }
    } else if (length(fsl_lcsi_crisis3) > 1){
      fsl_lcsi_crisis3 <- tcltk::tk_select.list(fsl_lcsi_crisis3, title = "LCSI Crisis 3 column")
    } else if (length(fsl_lcsi_crisis3) == 0) {
      fsl_lcsi_crisis3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_crisis3","fsl_lcsi_crisis3")$res
    }

    fsl_lcsi_emergency1 <- names(main)[grepl("emergency|emergency1",names(main))]
    if(length(fsl_lcsi_emergency1) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_emergency1, "' the correct fsl_lcsi_emergency1 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_emergency1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency1","fsl_lcsi_emergency1")$res
      }
    } else if (length(fsl_lcsi_emergency1) > 1){
      fsl_lcsi_emergency1 <- tcltk::tk_select.list(fsl_lcsi_emergency1, title = "LCSI Emergency 1 column")
    } else if (length(fsl_lcsi_emergency1) == 0) {
      fsl_lcsi_emergency1 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency1","fsl_lcsi_emergency1")$res
    }

    fsl_lcsi_emergency2 <- names(main)[grepl("emergency|emergency2",names(main))]
    if(length(fsl_lcsi_emergency2) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_emergency2, "' the correct fsl_lcsi_emergency2 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_emergency2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency2","fsl_lcsi_emergency2")$res
      }
    } else if (length(fsl_lcsi_emergency2) > 1){
      fsl_lcsi_emergency2 <- tcltk::tk_select.list(fsl_lcsi_emergency2, title = "LCSI Emergency 2 column")
    } else if (length(fsl_lcsi_emergency2) == 0) {
      fsl_lcsi_emergency2 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency2","fsl_lcsi_emergency2")$res
    }

    fsl_lcsi_emergency3 <- names(main)[grepl("emergency|emergency3",names(main))]
    if(length(fsl_lcsi_emergency3) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_lcsi_emergency3, "' the correct fsl_lcsi_emergency3 column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_lcsi_emergency3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency3","fsl_lcsi_emergency3")$res
      }
    } else if (length(fsl_lcsi_emergency3) > 1){
      fsl_lcsi_emergency3 <- tcltk::tk_select.list(fsl_lcsi_emergency3, title = "LCSI Emergency 3 column")
    } else if (length(fsl_lcsi_emergency3) == 0) {
      fsl_lcsi_emergency3 <- svDialogs::dlg_input(message= "Enter the name of the fsl_lcsi_emergency3","fsl_lcsi_emergency3")$res
    }

    lcsi_check_columns <- c(fsl_lcsi_stress1,fsl_lcsi_stress2,fsl_lcsi_stress3,fsl_lcsi_stress4,
                            fsl_lcsi_crisis1,fsl_lcsi_crisis2,fsl_lcsi_crisis3,
                            fsl_lcsi_emergency1,fsl_lcsi_emergency2,fsl_lcsi_emergency3)


    if(!all(lcsi_check_columns %in% names(main))) {
      svDialogs::dlg_message("Please check if the LCSI columns selected are correct and available in the dataset")
      stop("Please check if the LCSI columns selected are correct and available in the dataset")
    } else{
      yes_val <- tcltk::tk_select.list(dplyr::pull(main[,lcsi_check_columns]) %>% unique, title = "Yes Value")
      no_val <- tcltk::tk_select.list(dplyr::pull(main[,lcsi_check_columns]) %>% unique, title = "No Value")
      exhausted_val <- tcltk::tk_select.list(dplyr::pull(main[,lcsi_check_columns]) %>% unique, title = "Exhausted Value")
      not_applicable_val <- tcltk::tk_select.list(dplyr::pull(main[,lcsi_check_columns]) %>% unique, title = "Not Applicable Value")
      main <- main %>%
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
    main <- main %>%
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

if(!file.exists("inputs/environment.Rdata")) {
  if ("HDDS" %in% FSL_indicators){
    fsl_hdds_cereals <- names(main)[grepl("cereal",names(main))]
    if(length(fsl_hdds_cereals) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_cereals, "' the correct fsl_hdds_cereals column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_cereals <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_cereals","fsl_hdds_cereals")$res
      }
    } else if (length(fsl_hdds_cereals) > 1){
      fsl_hdds_cereals <- tcltk::tk_select.list(fsl_hdds_cereals, title = "HDDS Cereals column")
    } else if (length(fsl_hdds_cereals) == 0) {
      fsl_hdds_cereals <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_cereals","fsl_hdds_cereals")$res
    }

    fsl_hdds_tubers <- names(main)[grepl("tubers",names(main))]
    if(length(fsl_hdds_tubers) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_tubers, "' the correct fsl_hdds_tubers column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_tubers <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_tubers","fsl_hdds_tubers")$res
      }
    } else if (length(fsl_hdds_tubers) > 1){
      fsl_hdds_tubers <- tcltk::tk_select.list(fsl_hdds_tubers, title = "HDDS Tubers column")
    } else if (length(fsl_hdds_tubers) == 0) {
      fsl_hdds_tubers <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_tubers","fsl_hdds_tubers")$res
    }

    fsl_hdds_veg <- names(main)[grepl("veg",names(main))]
    if(length(fsl_hdds_veg) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_veg, "' the correct fsl_hdds_veg column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_veg <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_veg","fsl_hdds_veg")$res
      }
    } else if (length(fsl_hdds_veg) > 1){
      fsl_hdds_veg <- tcltk::tk_select.list(fsl_hdds_veg, title = "HDDS Vegetables column")
    } else if (length(fsl_hdds_veg) == 0) {
      fsl_hdds_veg <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_veg","fsl_hdds_veg")$res
    }

    fsl_hdds_fruit <- names(main)[grepl("fruit",names(main))]
    if(length(fsl_hdds_fruit) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_fruit, "' the correct fsl_hdds_fruit column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_fruit <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_fruit","fsl_hdds_fruit")$res
      }
    } else if (length(fsl_hdds_fruit) > 1){
      fsl_hdds_fruit <- tcltk::tk_select.list(fsl_hdds_fruit, title = "HDDS Fruits column")
    } else if (length(fsl_hdds_fruit) == 0) {
      fsl_hdds_fruit <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_fruit","fsl_hdds_fruit")$res
    }

    fsl_hdds_meat <- names(main)[grepl("meat",names(main))]
    if(length(fsl_hdds_meat) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_meat, "' the correct fsl_hdds_meat column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_meat <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_meat","fsl_hdds_meat")$res
      }
    } else if (length(fsl_hdds_meat) > 1){
      fsl_hdds_meat <- tcltk::tk_select.list(fsl_hdds_meat, title = "HDDS Meat column")
    } else if (length(fsl_hdds_meat) == 0) {
      fsl_hdds_meat <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_meat","fsl_hdds_meat")$res
    }

    fsl_hdds_eggs <- names(main)[grepl("egg",names(main))]
    if(length(fsl_hdds_eggs) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_eggs, "' the correct fsl_hdds_eggs column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_eggs <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_eggs","fsl_hdds_eggs")$res
      }
    } else if (length(fsl_hdds_eggs) > 1){
      fsl_hdds_eggs <- tcltk::tk_select.list(fsl_hdds_eggs, title = "HDDS Eggs column")
    } else if (length(fsl_hdds_eggs) == 0) {
      fsl_hdds_eggs <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_eggs","fsl_hdds_eggs")$res
    }

    fsl_hdds_fish <- names(main)[grepl("fish",names(main))]
    if(length(fsl_hdds_fish) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_fish, "' the correct fsl_hdds_fish column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_fish <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_fish","fsl_hdds_fish")$res
      }
    } else if (length(fsl_hdds_fish) > 1){
      fsl_hdds_fish <- tcltk::tk_select.list(fsl_hdds_fish, title = "HDDS Fish column")
    } else if (length(fsl_hdds_fish) == 0) {
      fsl_hdds_fish <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_fish","fsl_hdds_fish")$res
    }

    fsl_hdds_legumes <- names(main)[grepl("legume|pulse",names(main))]
    if(length(fsl_hdds_legumes) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_legumes, "' the correct fsl_hdds_legumes column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_legumes <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_legumes","fsl_hdds_legumes")$res
      }
    } else if (length(fsl_hdds_legumes) > 1){
      fsl_hdds_legumes <- tcltk::tk_select.list(fsl_hdds_legumes, title = "HDDS Legumes/Pulses column")
    } else if (length(fsl_hdds_legumes) == 0) {
      fsl_hdds_legumes <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_legumes","fsl_hdds_legumes")$res
    }

    fsl_hdds_dairy <- names(main)[grepl("milk|dairy",names(main))]
    if(length(fsl_hdds_dairy) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_dairy, "' the correct fsl_hdds_dairy column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_dairy <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_dairy","fsl_hdds_dairy")$res
      }
    } else if (length(fsl_hdds_dairy) > 1){
      fsl_hdds_dairy <- tcltk::tk_select.list(fsl_hdds_dairy, title = "HDDS Dairy/Milk column")
    } else if (length(fsl_hdds_dairy) == 0) {
      fsl_hdds_dairy <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_dairy","fsl_hdds_dairy")$res
    }

    fsl_hdds_oil <- names(main)[grepl("oil",names(main))]
    if(length(fsl_hdds_oil) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_oil, "' the correct fsl_hdds_oil column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_oil <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_oil","fsl_hdds_oil")$res
      }
    } else if (length(fsl_hdds_oil) > 1){
      fsl_hdds_oil <- tcltk::tk_select.list(fsl_hdds_oil, title = "HDDS Oil column")
    } else if (length(fsl_hdds_oil) == 0) {
      fsl_hdds_oil <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_oil","fsl_hdds_oil")$res
    }

    fsl_hdds_sugar <- names(main)[grepl("sugar",names(main))]
    if(length(fsl_hdds_sugar) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_sugar, "' the correct fsl_hdds_sugar column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_sugar <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_sugar","fsl_hdds_sugar")$res
      }
    } else if (length(fsl_hdds_sugar) > 1){
      fsl_hdds_sugar <- tcltk::tk_select.list(fsl_hdds_sugar, title = "HDDS Sugar column")
    } else if (length(fsl_hdds_sugar) == 0) {
      fsl_hdds_sugar <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_sugar","fsl_hdds_sugar")$res
    }

    fsl_hdds_condiments <- names(main)[grepl("condiment",names(main))]
    if(length(fsl_hdds_condiments) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", fsl_hdds_condiments, "' the correct fsl_hdds_condiments column?"), type = "yesno")$res
      if (yes_no == "no") {
        fsl_hdds_condiments <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_condiments","fsl_hdds_condiments")$res
      }
    } else if (length(fsl_hdds_condiments) > 1){
      fsl_hdds_condiments <- tcltk::tk_select.list(fsl_hdds_condiments, title = "HDDS Condiments column")
    } else if (length(fsl_hdds_condiments) == 0) {
      fsl_hdds_condiments <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_condiments","fsl_hdds_condiments")$res
    }

    hdds_check_columns <- c(fsl_hdds_cereals,fsl_hdds_tubers,fsl_hdds_veg,fsl_hdds_fruit,fsl_hdds_meat,
                            fsl_hdds_eggs,fsl_hdds_fish,fsl_hdds_legumes,fsl_hdds_dairy,fsl_hdds_oil,
                            fsl_hdds_sugar,fsl_hdds_condiments)

    if(!all(hdds_check_columns %in% names(main))) {
      svDialogs::dlg_message("Please check if the HDDS columns selected are correct and available in the dataset")
      stop("Please check if the HDDS columns selected are correct and available in the dataset")
    } else {
      yes_val <- tcltk::tk_select.list(dplyr::pull(main[,hdds_check_columns]) %>% unique, title = "Yes Value")
      no_val <- tcltk::tk_select.list(dplyr::pull(main[,hdds_check_columns]) %>% unique, title = "No Value")
      main <- main %>%
        impactR4PHU::add_hdds(fsl_hdds_cereals = fsl_hdds_cereals,
                              fsl_hdds_tubers = fsl_hdds_tubers,
                              fsl_hdds_veg = fsl_hdds_veg,
                              fsl_hdds_fruit = fsl_hdds_fruit,
                              fsl_hdds_meat = fsl_hdds_meat,
                              fsl_hdds_eggs = fsl_hdds_eggs,
                              fsl_hdds_fish = fsl_hdds_fish,
                              fsl_hdds_legumes = fsl_hdds_legumes,
                              fsl_hdds_dairy = fsl_hdds_dairy,
                              fsl_hdds_oil = fsl_hdds_oil,
                              fsl_hdds_sugar = fsl_hdds_sugar,
                              fsl_hdds_condiments = fsl_hdds_condiments,
                              yes_val = yes_val,
                              no_val = no_val)
    }
  } else {
    fsl_hdds_cereals <- "fsl_hdds_cereals"
    fsl_hdds_tubers <- "fsl_hdds_tubers"
    fsl_hdds_veg <- "fsl_hdds_veg"
    fsl_hdds_fruit <- "fsl_hdds_fruit"
    fsl_hdds_meat <- "fsl_hdds_meat"
    fsl_hdds_eggs <- "fsl_hdds_eggs"
    fsl_hdds_fish <- "fsl_hdds_fish"
    fsl_hdds_legumes <- "fsl_hdds_legumes"
    fsl_hdds_dairy <- "fsl_hdds_dairy"
    fsl_hdds_oil <- "fsl_hdds_oil"
    fsl_hdds_sugar <- "fsl_hdds_sugar"
    fsl_hdds_condiments <- "fsl_hdds_condiments"
    hdds_check_columns <- c(fsl_hdds_cereals,fsl_hdds_tubers,fsl_hdds_veg,fsl_hdds_fruit,fsl_hdds_meat,
                            fsl_hdds_eggs,fsl_hdds_fish,fsl_hdds_legumes,fsl_hdds_dairy,fsl_hdds_oil,
                            fsl_hdds_sugar,fsl_hdds_condiments)
  }
} else {
  if("HDDS" %in% FSL_indicators){
    main <- main %>%
      impactR4PHU::add_hdds(fsl_hdds_cereals = fsl_hdds_cereals,
                            fsl_hdds_tubers = fsl_hdds_tubers,
                            fsl_hdds_veg = fsl_hdds_veg,
                            fsl_hdds_fruit = fsl_hdds_fruit,
                            fsl_hdds_meat = fsl_hdds_meat,
                            fsl_hdds_eggs = fsl_hdds_eggs,
                            fsl_hdds_fish = fsl_hdds_fish,
                            fsl_hdds_legumes = fsl_hdds_legumes,
                            fsl_hdds_dairy = fsl_hdds_dairy,
                            fsl_hdds_oil = fsl_hdds_oil,
                            fsl_hdds_sugar = fsl_hdds_sugar,
                            fsl_hdds_condiments = fsl_hdds_condiments,
                            yes_val = yes_val,
                            no_val = no_val)
  }
}

fcm_check_1_columns <- c("fsl_fcs_cat",
                         "fsl_rcsi_cat")

fcm_check_2_columns <- c("fsl_hdds_cat",
                         "fsl_rcsi_cat")

fcm_check_3_columns <- c("fsl_fcs_cat",
                         "fsl_hhs_cat")

fcm_check_4_columns <- c("fsl_hdds_cat",
                         "fsl_hhs_cat")

fcm_check_5_columns <- c("fsl_hdds_cat",
                         "fsl_rcsi_cat",
                         "fsl_hhs_cat")

fcm_check_6_columns <- c("fsl_fcs_cat",
                         "fsl_rcsi_cat",
                         "fsl_hhs_cat")

if(all(fcm_check_1_columns %in% names(main)) |
   all(fcm_check_2_columns %in% names(main)) |
   all(fcm_check_3_columns %in% names(main)) |
   all(fcm_check_4_columns %in% names(main)) |
   all(fcm_check_5_columns %in% names(main)) |
   all(fcm_check_6_columns %in% names(main))) {
  main <- main %>%
    impactR4PHU::add_fcm_phase()
}

fclcm_check_columns <- c("fsl_fc_phase",
                         "fsl_lcsi_cat")
if(all(fclcm_check_columns %in% names(main))) {
  main <- main %>%
    impactR4PHU::add_fclcm_phase()
}

################################################################################
## FSL
if ("HDDS" %in% FSL_indicators){
  if(all(hdds_check_columns %in% names(main))) {
    # HDDS
    hdds_table <- data.frame()
    hdds_survey <- srvyr::as_survey_design(main)
    for(i in hdds_check_columns){
      # make a long table:
      res.long <- hdds_survey %>%
        group_by(!!rlang::sym(i), .add = T) %>%
        # num_samples here is the actual number of responses for each option in each group
        summarise(num_samples = n(), ## to review later stages survey_total(na.rm = T, vartype = "var")
                  prop = srvyr::survey_prop(na.rm = T, vartype = "var")) %>%
        mutate(prop = paste0(round(prop,2) *100,"%")) %>%
        select(-num_samples)
      # widen the table:
      res.wide <- res.long %>% tidyr::pivot_wider(names_from = !!rlang::sym(i), values_from = c(prop),
                                                  values_fill = "0%") %>%
        select(-prop_var) %>%
        rename(No = "no",
               Yes = "yes") %>%
        mutate(HDDS = i) %>%
        relocate(HDDS, .before=1)
      hdds_table <- rbind(hdds_table,res.wide)
    }

    # HDDS Cat
    hdds_cat_table <- hdds_survey %>%
      group_by(fsl_hdds_cat, .add = T) %>%
      summarise(num_samples = n(),
                prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>%
      mutate(Percentage = paste0(round(prop,2) *100,"%")) %>%
      select(-c(prop,prop_var))

    # HDDS Score
    hdds_score_table <- hdds_survey %>%
      group_by() %>%
      summarise(Mean = srvyr::survey_mean(fsl_hdds_score, na.rm=T, vartype ="ci")) %>%
      mutate_at(vars(starts_with("Mean")),~round(.,2)) %>%
      mutate(Variable = "fsl_hdds_score") %>%
      relocate(Variable, .before = 1)
  }
}

if ("HHS" %in% FSL_indicators){
  if(all(c(hhs_check_columns,hhs_check_columns_freq) %in% names(main))) {
    # HHS
    hhs_table <- data.frame()
    hhs_survey <- srvyr::as_survey_design(main)
    for(i in hhs_check_columns){
      # make a long table:
      res.long <- hhs_survey %>%
        group_by(!!rlang::sym(i), .add = T) %>%
        # num_samples here is the actual number of responses for each option in each group
        summarise(num_samples = n(), ## to review later stages survey_total(na.rm = T, vartype = "var")
                  prop = srvyr::survey_prop(na.rm = T, vartype = "var")) %>%
        mutate(prop = paste0(round(prop,2) *100,"%")) %>%
        select(-num_samples)
      # widen the table:
      res.wide <- res.long %>% tidyr::pivot_wider(names_from = !!rlang::sym(i), values_from = c(prop),
                                                  values_fill = "0%") %>%
        select(-prop_var) %>%
        rename(No = no_answer,
               Yes = yes_answer) %>%
        mutate(HHS = i) %>%
        relocate(HHS, .before=1)
      hhs_table <- rbind(hhs_table,res.wide)
    }
    hhs_tabl_Freq <- data.frame()
    for(i in hhs_check_columns_freq){
      # make a long table:
      res.long <- hhs_survey %>%
        filter(!is.na(!!rlang::sym(i))) %>%
        group_by(!!rlang::sym(i), .add = T) %>%
        # num_samples here is the actual number of responses for each option in each group
        summarise(num_samples = n(), ## to review later stages survey_total(na.rm = T, vartype = "var")
                  prop = srvyr::survey_prop(na.rm = T, vartype = "var")) %>%
        mutate(prop = paste0(round(prop,2) *100,"%")) %>%
        select(-c(num_samples,prop_var))
      # widen the table:
      res.wide <- res.long %>% tidyr::pivot_wider(names_from = !!rlang::sym(i), values_from = c(prop),
                                                  values_fill = "0%") %>%
        rename(Often = often_answer,
               Rarely = rarely_answer,
               Sometimes = sometimes_answer)
      hhs_tabl_Freq <- rbind(hhs_tabl_Freq,res.wide)
    }

    hhs_table <- cbind(hhs_table,hhs_tabl_Freq)

    # HHS Cat
    hhs_cat_table <- hhs_survey %>%
      group_by(fsl_hhs_cat_ipc, .add = T) %>%
      summarise(num_samples = n(),
                prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>%
      mutate(Percentage = paste0(round(prop,2) *100,"%")) %>%
      select(-c(prop,prop_var))
  }
}

if ("FCS" %in% FSL_indicators){
  # FCS
  if(all(fcs_check_columns %in% names(main))) {
    fcs_table <- data.frame()
    fcs_survey <- srvyr::as_survey_design(main)
    for(i in fcs_check_columns){
      # make a long table:
      res.long <- fcs_survey %>%
        select(!!rlang::sym(i)) %>%
        group_by() %>%
        summarise(Mean = srvyr::survey_mean(!!rlang::sym(i),na.rm = T, vartype = "ci"),
                  Median = srvyr::survey_median(!!rlang::sym(i),na.rm = T)) %>%
        mutate_at(vars(starts_with("Mean")),~round(.,2)) %>%
        select(-Median_se) %>%
        mutate(Name = i) %>%
        relocate(Name, .before = 1)

      fcs_table <- rbind(fcs_table,res.long)
    }

    # HDDS Cat
    fcs_cat_table <- fcs_survey %>%
      group_by(fsl_fcs_cat, .add = T) %>%
      summarise(num_samples = n(),
                prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>%
      mutate(Percentage = paste0(round(prop,2) *100,"%")) %>%
      select(-c(prop,prop_var))

    # HDDS Score
    fcs_score_table <- fcs_survey %>%
      group_by() %>%
      summarise(Mean = srvyr::survey_mean(fsl_fcs_score, na.rm=T, vartype ="ci")) %>%
      mutate_at(vars(starts_with("Mean")),~round(.,2)) %>%
      mutate(Variable = "fsl_fcs_score") %>%
      relocate(Variable, .before = 1)
  }
}
if ("rCSI" %in% FSL_indicators){
  if(all(rcsi_check_columns %in% names(main))) {
    # RCSI
    rcsi_table <- data.frame()
    rcsi_survey <- srvyr::as_survey_design(main)
    for(i in rcsi_check_columns){
      # make a long table:
      res.long <- rcsi_survey %>%
        select(!!rlang::sym(i)) %>%
        group_by() %>%
        summarise(Mean = srvyr::survey_mean(!!rlang::sym(i),na.rm = T, vartype = "ci"),
                  Median = srvyr::survey_median(!!rlang::sym(i),na.rm = T)) %>%
        mutate_at(vars(starts_with("Mean")),~round(.,2)) %>%
        select(-Median_se) %>%
        mutate(Name = i) %>%
        relocate(Name, .before = 1)

      rcsi_table <- rbind(rcsi_table,res.long)
    }

    # HDDS Cat
    rcsi_cat_table <- rcsi_survey %>%
      filter(!is.na(fsl_rcsi_cat)) %>%
      group_by(fsl_rcsi_cat) %>%
      summarise(num_samples = n(),
                prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>%
      mutate(Percentage = paste0(round(prop,2) *100,"%")) %>%
      select(-c(prop,prop_var))

    # HDDS Score
    rcsi_score_table <- rcsi_survey %>%
      group_by() %>%
      summarise(Mean = srvyr::survey_mean(fsl_rcsi_score, na.rm=T, vartype ="ci")) %>%
      mutate_at(vars(starts_with("Mean")),~round(.,2)) %>%
      mutate(Variable = "fsl_rcsi_score") %>%
      relocate(Variable, .before = 1)
  }
}
if ("LCSI" %in% FSL_indicators){
  if(all(lcsi_check_columns %in% names(main))) {
    # LCSI
    lcsi_table <- data.frame()
    lcsi_survey <- srvyr::as_survey_design(main)
    for(i in lcsi_check_columns){
      # make a long table:
      res.long <- lcsi_survey %>%
        group_by(!!rlang::sym(i), .add = T) %>%
        # num_samples here is the actual number of responses for each option in each group
        summarise(num_samples = n(), ## to review later stages survey_total(na.rm = T, vartype = "var")
                  prop = srvyr::survey_prop(na.rm = T, vartype = "var")) %>%
        mutate(prop = paste0(round(prop,2) *100,"%"))%>%
        select(-c(num_samples,prop_var))
      # widen the table:
      res.wide <- res.long %>% tidyr::pivot_wider(names_from = !!rlang::sym(i), values_from = c(prop),
                                                  values_fill = "0%") %>%
        mutate(LCSI = i) %>%
        relocate(LCSI, .before=1)
      lcsi_table <- dplyr::bind_rows(lcsi_table,res.wide)
    }

    # HDDS Cat
    lcsi_cat_table <- lcsi_survey %>%
      group_by(fsl_lcsi_cat, .add = T) %>%
      summarise(num_samples = n(),
                prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>%
      mutate(Percentage = paste0(round(prop,2) *100,"%")) %>%
      select(-c(prop,prop_var))
  }
}

if(all(fcm_check_1_columns %in% names(main)) |
   all(fcm_check_2_columns %in% names(main)) |
   all(fcm_check_3_columns %in% names(main)) |
   all(fcm_check_4_columns %in% names(main)) |
   all(fcm_check_5_columns %in% names(main)) |
   all(fcm_check_6_columns %in% names(main))) {
  # FC_PHASE
  fc_phase_table <- lcsi_survey %>%
    filter(!is.na(fsl_fc_phase)) %>%
    group_by(fsl_fc_phase, .add = T) %>%
    summarise(num_samples = n(),
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>%
    mutate(Percentage = paste0(round(prop,2) *100,"%")) %>%
    select(-c(prop,prop_var))
}

if(all(fclcm_check_columns %in% names(main))) {
  # FCLCM_PHASE
  fclcm_phase_table <- lcsi_survey %>%
    filter(!is.na(fclcm_phase)) %>%
    group_by(fclcm_phase, .add = T) %>%
    summarise(num_samples = n(),
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>%
    mutate(Percentage = paste0(round(prop,3) * 100,"%")) %>%
    select(-c(prop,prop_var))
}

data.list$main <- main

list_of_var <- c("FSL_indicators","fsl_daf_variable","fsl_fcs_cereal","fsl_fcs_legumes",
                 "fsl_fcs_veg","fsl_fcs_fruit","fsl_fcs_meat","fsl_fcs_dairy","fsl_fcs_sugar",
                 "fsl_fcs_oil","fsl_rcsi_lessquality","fsl_rcsi_borrow","fsl_rcsi_mealsize","fsl_rcsi_mealadult",
                 "fsl_rcsi_mealnb","fsl_hhs_nofoodhh","fsl_hhs_nofoodhh_freq","fsl_hhs_sleephungry","fsl_hhs_sleephungry_freq",
                 "fsl_hhs_alldaynight","fsl_hhs_alldaynight_freq","yes_answer","no_answer","rarely_answer","sometimes_answer","often_answer",
                 "fsl_lcsi_stress1","fsl_lcsi_stress2","fsl_lcsi_stress3","fsl_lcsi_stress4",
                 "fsl_lcsi_crisis1","fsl_lcsi_crisis2","fsl_lcsi_crisis3","fsl_lcsi_emergency1",
                 "fsl_lcsi_emergency2","fsl_lcsi_emergency3","yes_val","no_val","exhausted_val","not_applicable_val",
                 "fsl_hdds_cereals","fsl_hdds_tubers","fsl_hdds_veg","fsl_hdds_fruit","fsl_hdds_meat",
                 "fsl_hdds_eggs","fsl_hdds_fish","fsl_hdds_legumes","fsl_hdds_dairy","fsl_hdds_oil","fsl_hdds_sugar","fsl_hdds_condiments",
                 "fclcm_check_columns","fcm_check_1_columns","fcm_check_2_columns","fcm_check_3_columns",
                 "fcm_check_4_columns","fcm_check_5_columns","fcm_check_6_columns","hhs_check_columns_freq",
                 "hdds_check_columns","lcsi_check_columns","hhs_check_columns","rcsi_check_columns","fcs_check_columns")

if(!file.exists("inputs/environment.Rdata")){
  save(list = list_of_var, file = "inputs/environment.Rdata")
}
