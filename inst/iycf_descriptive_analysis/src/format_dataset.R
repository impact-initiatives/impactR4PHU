################################################################################
### FORMAT DATASET
################################################################################

# create shorthands to make working with the data easier:
path.sheet.with.main <- tcltk::tk_select.list(names(data.list), title = "Main HH sheet", multiple = F)
path.sheet.with.iycf <- tcltk::tk_select.list(names(data.list), title = "IYCF indicators sheet", multiple = F)
main <- data.list[[path.sheet.with.main]]
iycf <- data.list[[path.sheet.with.iycf]]

if(file.exists("inputs/environment.Rdata")) {
  load("inputs/environment.Rdata")
}

if(!file.exists("inputs/environment.Rdata")) {
  ## Detect uuid_main column
  uuid_main <- names(main)[grepl("uuid",names(main))]

  if(length(uuid_main) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_main, "' the correct HH UUID column in main data?"), type = "yesno")$res
    if(yes_no == "no"){
      uuid_main <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in main data","uuid_main")$res
    }
  } else if (length(uuid_main) > 1){
    uuid_main <- tcltk::tk_select.list(uuid_main, title = "HH UUID Column [Main Data]")
    if(uuid_main == ""){
      uuid_main <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in main data","uuid_main")$res
    }
  } else if (length(uuid_main) == 0) {
    uuid_main <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in main data","uuid_main")$res
  }
  ## Detect Team column
  yes_no_team <- svDialogs::dlg_message(paste0("Is there teams of enumerators?"), type = "yesno")$res
  if(yes_no_team == "yes"){
    team <- names(main)[grepl("team|organization|organisation",names(main))]

    if(length(team) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", team, "' the correct team/organization column?"), type = "yesno")$res
      if(yes_no == "no"){
        team <- svDialogs::dlg_input(message= "Enter the name of the team/organization Column","enumerator")$res
      }
    } else if (length(team) > 1){
      team <- tcltk::tk_select.list(team, title = "Team/Organization Columns")
      if(team == ""){
        team <- svDialogs::dlg_input(message= "Enter the name of the team/organization Column","team")$res
      }
    } else if (length(team) == 0) {
      team <- svDialogs::dlg_input(message= "Enter the name of the team/organization Column","team")$res
    }
  } else {
    team <- NULL
  }
  ## Detect Enumerator column
  enumerator <- names(main)[grepl("enum|team",names(main))]

  if(length(enumerator) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", enumerator, "' the correct enumerator column?"), type = "yesno")$res
    if(yes_no == "no"){
      enumerator <- svDialogs::dlg_input(message= "Enter the name of the Enumerator Column","enumerator")$res
    }
  } else if (length(enumerator) > 1){
    enumerator <- tcltk::tk_select.list(enumerator, title = "Enumerator Columns")
    if(enumerator == ""){
      enumerator <- svDialogs::dlg_input(message= "Enter the name of the Enumerator Column","enumerator")$res
    }
  } else if (length(enumerator) == 0) {
    enumerator <- svDialogs::dlg_input(message= "Enter the name of the Enumerator Column","enumerator")$res
  }

  ## Detect uuid_main column
  uuid_iycf <- names(iycf)[grepl("uuid",names(iycf))]

  if(length(uuid_iycf) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_iycf, "' the correct HH UUID column in iycf data?"), type = "yesno")$res
    if(yes_no == "no"){
      uuid_iycf <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in iycf data","uuid_iycf")$res
    }
  } else if (length(uuid_iycf) > 1){
    uuid_iycf <- tcltk::tk_select.list(uuid_iycf, title = "IYCF UUID Column [IYCF Data]")
    if(uuid_iycf == ""){
      uuid_iycf <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in iycf data","uuid_iycf")$res
    }
  } else if (length(uuid_iycf) == 0) {
    uuid_iycf <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in iycf data","uuid_iycf")$res
  }

  ## Detect age month
  age_months <- names(iycf)[grepl("month|age",names(iycf))]

  if(length(age_months) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", age_months, "' the correct age month column?"), type = "yesno")$res
    if(yes_no == "no"){
      age_months <- svDialogs::dlg_input(message= "Enter the name of the age month column","age_months")$res
    }
  } else if (length(age_months) > 1){
    age_months <- tcltk::tk_select.list(age_months, title = "Age Month Column")
    if(age_months == ""){
      age_months <- svDialogs::dlg_input(message= "Enter the name of the age month column","age_months")$res
    }
  } else if (length(age_months) == 0) {
    age_months <- svDialogs::dlg_input(message= "Enter the name of the age month column","age_months")$res
  }

  ## Detect sex
  sex <- names(iycf)[grepl("sex|gender",names(iycf))]

  if(length(sex) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", sex, "' the correct sex/gender column?"), type = "yesno")$res
    if(yes_no == "no"){
      sex <- svDialogs::dlg_input(message= "Enter the name of the sex/gender column","sex")$res
    }
  } else if (length(sex) > 1){
    sex <- tcltk::tk_select.list(sex, title = "Sex/Gender Column")
    if(sex == ""){
      sex <- svDialogs::dlg_input(message= "Enter the name of the sex/gender column","sex")$res
    }
  } else if (length(sex) == 0) {
    sex <- svDialogs::dlg_input(message= "Enter the name of the sex/gender column","sex")$res
  }

  if(sex %in% names(iycf)){
    sex_codes <- unique(iycf[[sex]])
    ideal_codes <- c("1", "2")
    sex_recodes <- c("1", "2", "NA")

    if(length(setdiff(sex_codes, ideal_codes))==0) {
      male <- 1
      female <- 2
    } else {
      male <- tcltk::tk_select.list(sex_codes, title = "Male Option", multiple = T)
      female <- tcltk::tk_select.list(sex_codes, title = "Female Option", multiple = T)
    }
  }
  iycf_caregiver <- names(iycf)[grepl("caregiver",names(iycf))]
  if(length(iycf_caregiver) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_caregiver, "' the correct column that indicates if the caregiver is present?"), type = "yesno")$res
    if(yes_no == "no"){
      iycf_caregiver <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the caregiver is present","iycf_caregiver")$res
    }
  } else if (length(iycf_caregiver) > 1){
    iycf_caregiver <- tcltk::tk_select.list(iycf_caregiver, title = "Caregiver Present column")
    if(iycf_caregiver == ""){
      iycf_caregiver <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the caregiver is present","iycf_caregiver")$res
    }
  } else if (length(iycf_caregiver) == 0) {
    iycf_caregiver <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the caregiver is present","iycf_caregiver")$res
  }

  iycf_1 <- names(iycf)[grepl("iycf_1|ever",names(iycf))]
  if(length(iycf_1) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_1, "' the correct column that indicates if the child ever breastfed?"), type = "yesno")$res
    if(yes_no == "no"){
      iycf_1 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child ever breastfed","iycf_1")$res
    }
  } else if (length(iycf_1) > 1){
    iycf_1 <- tcltk::tk_select.list(iycf_1, title = "Ever Breastfed column")
    if(iycf_1 == ""){
      iycf_1 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child ever breastfed","iycf_1")$res
    }
  } else if (length(iycf_1) == 0) {
    iycf_1 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child ever breastfed","iycf_1")$res
  }

  iycf_2 <- names(iycf)[grepl("iycf_2|bf_ei|breastfed",names(iycf))]
  if(length(iycf_2) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_2, "' the correct column that indicates how long the child started breastfeeding after birth"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_2 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates how long the child started breastfeeding after birth","iycf_2")$res
    }
  } else if (length(iycf_2) > 1){
    iycf_2 <- tcltk::tk_select.list(iycf_2, title = "Child Started Breastfeeding After Birth column")
    if(iycf_2 == ""){
      iycf_2 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates how long the child started breastfeeding after birth","iycf_2")$res
    }
  } else if (length(iycf_2) == 0) {
    iycf_2 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates how long the child started breastfeeding after birth","iycf_2")$res
  }

  iycf_3 <- names(iycf)[grepl("iycf_3|bf_newborn_consumption",names(iycf))]
  if(length(iycf_3) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_3, "' the correct column that indicates Exclusive Breastfeeding First 2 Days?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_3 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates Exclusive Breastfeeding First 2 Days","iycf_3")$res
    }
  } else if (length(iycf_3) > 1){
    iycf_3 <- tcltk::tk_select.list(iycf_3, title = "Exclusive Breastfeeding First 2 Days column")
    if(iycf_3 == ""){
      iycf_3 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates Exclusive Breastfeeding First 2 Days","iycf_3")$res
    }
  } else if (length(iycf_3) == 0) {
    iycf_3 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates Exclusive Breastfeeding First 2 Days","iycf_3")$res
  }

  iycf_4 <- names(iycf)[grepl("iycf_4|yesterday",names(iycf))]
  if(length(iycf_4) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_4, "' the correct column that indicates if the child was breastfed yesterday?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_4 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child was breastfed yesterday","iycf_4")$res
    }
  } else if (length(iycf_4) > 1){
    iycf_4 <- tcltk::tk_select.list(iycf_4, title = "Breastfed Yesterday column")
    if(iycf_4 == ""){
      iycf_4 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child was breastfed yesterday","iycf_4")$res
    }
  } else if (length(iycf_4) == 0) {
    iycf_4 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child was breastfed yesterday","iycf_4")$res
  }

  iycf_5 <- names(iycf)[grepl("iycf_5|bottlefed",names(iycf))]
  if(length(iycf_5) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_5, "' the correct column that indicates if the child drink anything from a bottle yesterday"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_5 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child drink anything from a bottle yesterday","iycf_5")$res
    }
  } else if (length(iycf_5) > 1){
    iycf_5 <- tcltk::tk_select.list(iycf_5, title = "Child Drink From A Bottle Yesterday column")
    if(iycf_5 == ""){
      iycf_5 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child drink anything from a bottle yesterday","iycf_5")$res
    }
  } else if (length(iycf_5) == 0) {
    iycf_5 <- svDialogs::dlg_input(message= "Enter the name of the column that indicates if the child drink anything from a bottle yesterday","iycf_5")$res
  }

  iycf_6a <- names(iycf)[grepl("iycf_6a|water",names(iycf))]
  if(length(iycf_6a) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6a, "' the correct liquid water column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6a <- svDialogs::dlg_input(message= "Enter the name of the liquid water","iycf_6a")$res
    }
  } else if (length(iycf_6a) > 1){
    iycf_6a <- tcltk::tk_select.list(iycf_6a, title = "Liquid Water column")
    if(iycf_6a == ""){
      iycf_6a <- svDialogs::dlg_input(message= "Enter the name of the liquid water","iycf_6a")$res
    }
  } else if (length(iycf_6a) == 0) {
    iycf_6a <- svDialogs::dlg_input(message= "Enter the name of the liquid water","iycf_6a")$res
  }

  iycf_6b <- names(iycf)[grepl("iycf_6b|formula",names(iycf))]
  if(length(iycf_6b) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6b, "' the correct liquid infant formula column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6b <- svDialogs::dlg_input(message= "Enter the name of the liquid infant formula","iycf_6b")$res
    }
  } else if (length(iycf_6b) > 1){
    iycf_6b <- tcltk::tk_select.list(iycf_6b, title = "Liquid Infant Formula column")
    if(iycf_6b == ""){
      iycf_6b <- svDialogs::dlg_input(message= "Enter the name of the liquid infant formula","iycf_6b")$res
    }
  } else if (length(iycf_6b) == 0) {
    iycf_6b <- svDialogs::dlg_input(message= "Enter the name of the liquid infant formula","iycf_6b")$res
  }

  iycf_6c <- names(iycf)[grepl("iycf_6c|milk",names(iycf))]
  if(length(iycf_6c) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6c, "' the correct liquid milk from animal column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6c <- svDialogs::dlg_input(message= "Enter the name of the liquid milk from animal","iycf_6c")$res
    }
  } else if (length(iycf_6c) > 1){
    iycf_6c <- tcltk::tk_select.list(iycf_6c, title = "Liquid Milk From Animal column")
    if(iycf_6c == ""){
      iycf_6c <- svDialogs::dlg_input(message= "Enter the name of the liquid milk from animal","iycf_6c")$res
    }
  } else if (length(iycf_6c) == 0) {
    iycf_6c <- svDialogs::dlg_input(message= "Enter the name of the liquid milk from animal","iycf_6c")$res
  }

  iycf_6d <- names(iycf)[grepl("iycf_6d|yoghurt",names(iycf))]
  if(length(iycf_6d) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6d, "' the correct liquid yogurt column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6d <- svDialogs::dlg_input(message= "Enter the name of the liquid yogurt","iycf_6d")$res
    }
  } else if (length(iycf_6d) > 1){
    iycf_6d <- tcltk::tk_select.list(iycf_6d, title = "Liquid Yogurt column")
    if(iycf_6d == ""){
      iycf_6d <- svDialogs::dlg_input(message= "Enter the name of the liquid yogurt","iycf_6d")$res
    }
  } else if (length(iycf_6d) == 0) {
    iycf_6d <- svDialogs::dlg_input(message= "Enter the name of the liquid yogurt","iycf_6d")$res
  }

  iycf_6e <- names(iycf)[grepl("iycf_6e|chocolate",names(iycf))]
  if(length(iycf_6e) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6e, "' the correct liquid chocolate flavoured drink column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6e <- svDialogs::dlg_input(message= "Enter the name of the liquid chocolate flavoured drink","iycf_6e")$res
    }
  } else if (length(iycf_6e) > 1){
    iycf_6e <- tcltk::tk_select.list(iycf_6e, title = "Liquid Chocolate Flavoured Drink column")
    if(iycf_6e == ""){
      iycf_6e <- svDialogs::dlg_input(message= "Enter the name of the liquid chocolate flavoured drink","iycf_6e")$res
    }
  } else if (length(iycf_6e) == 0) {
    iycf_6e <- svDialogs::dlg_input(message= "Enter the name of the liquid chocolate flavoured drink","iycf_6e")$res
  }

  iycf_6f <- names(iycf)[grepl("iycf_6f|juice",names(iycf))]
  if(length(iycf_6f) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6f, "' the correct liquid fruit juice or fruit flavored drink column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6f <- svDialogs::dlg_input(message= "Enter the name of the liquid fruit juice or fruit flavored drink","iycf_6f")$res
    }
  } else if (length(iycf_6f) > 1){
    iycf_6f <- tcltk::tk_select.list(iycf_6f, title = "Liquid Fruit Juice or Fruit Flavoured Drink column")
    if(iycf_6f == ""){
      iycf_6f <- svDialogs::dlg_input(message= "Enter the name of the liquid fruit juice or fruit flavored drink","iycf_6f")$res
    }
  } else if (length(iycf_6f) == 0) {
    iycf_6f <- svDialogs::dlg_input(message= "Enter the name of the liquid fruit juice or fruit flavored drink","iycf_6f")$res
  }

  iycf_6g <- names(iycf)[grepl("iycf_6g|soda",names(iycf))]
  if(length(iycf_6g) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6g, "' the correct liquid sodas/malt/sports/energy drink column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6g <- svDialogs::dlg_input(message= "Enter the name of the liquid sodas/malt/sports/energy drink","iycf_6g")$res
    }
  } else if (length(iycf_6g) > 1){
    iycf_6g <- tcltk::tk_select.list(iycf_6g, title = "Liquid Sodas/Malt/Sports/Energy Drink column")
    if(iycf_6g == ""){
      iycf_6g <- svDialogs::dlg_input(message= "Enter the name of the liquid sodas/malt/sports/energy drink","iycf_6g")$res
    }
  } else if (length(iycf_6g) == 0) {
    iycf_6g <- svDialogs::dlg_input(message= "Enter the name of the liquid sodas/malt/sports/energy drink","iycf_6g")$res
  }


  iycf_6h <- names(iycf)[grepl("iycf_6h|tea",names(iycf))]
  if(length(iycf_6h) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6h, "' the correct liquid tea/coffee/herbal drink column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6h <- svDialogs::dlg_input(message= "Enter the name of the liquid tea/coffee/herbal drink","iycf_6h")$res
    }
  } else if (length(iycf_6h) > 1){
    iycf_6h <- tcltk::tk_select.list(iycf_6h, title = "Liquid Tea/Coffee/Herbal Drink column")
    if(iycf_6h == ""){
      iycf_6h <- svDialogs::dlg_input(message= "Enter the name of the liquid tea/coffee/herbal drink","iycf_6h")$res
    }
  } else if (length(iycf_6h) == 0) {
    iycf_6h <- svDialogs::dlg_input(message= "Enter the name of the liquid tea/coffee/herbal drink","iycf_6h")$res
  }


  iycf_6i <- names(iycf)[grepl("iycf_6i|broth",names(iycf))]
  if(length(iycf_6i) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6i, "' the correct liquid clear broth/soup column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6i <- svDialogs::dlg_input(message= "Enter the name of the liquid clear broth/soup","iycf_6i")$res
    }
  } else if (length(iycf_6i) > 1){
    iycf_6i <- tcltk::tk_select.list(iycf_6i, title = "Liquid Clear Broth/Soup column")
    if(iycf_6i == ""){
      iycf_6i <- svDialogs::dlg_input(message= "Enter the name of the liquid clear broth/soup","iycf_6i")$res
    }
  } else if (length(iycf_6i) == 0) {
    iycf_6i <- svDialogs::dlg_input(message= "Enter the name of the liquid clear broth/soup","iycf_6i")$res
  }


  iycf_6j <- names(iycf)[grepl("iycf_6j|other",names(iycf))]
  if(length(iycf_6j) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6j, "' the correct other liquids column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6j <- svDialogs::dlg_input(message= "Enter the name of the other liquids","iycf_6j")$res
    }
  } else if (length(iycf_6j) > 1){
    iycf_6j <- tcltk::tk_select.list(iycf_6j, title = "Other Liquids column")
    if(iycf_6j == ""){
      iycf_6j <- svDialogs::dlg_input(message= "Enter the name of the other liquids","iycf_6j")$res
    }
  } else if (length(iycf_6j) == 0) {
    iycf_6j <- svDialogs::dlg_input(message= "Enter the name of the other liquids","iycf_6j")$res
  }

  iycf_7a <- names(iycf)[grepl("iycf_7a|yoghurt",names(iycf))]
  if(length(iycf_7a) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7a, "' the correct yogurt as food column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7a <- svDialogs::dlg_input(message= "Enter the name of the yogurt as food column","iycf_7a")$res
    }
  } else if (length(iycf_7a) > 1){
    iycf_7a <- tcltk::tk_select.list(iycf_7a, title = "Yogurt as Food column")
    if(iycf_7a == ""){
      iycf_7a <- svDialogs::dlg_input(message= "Enter the name of the yogurt as food column","iycf_7a")$res
    }
  } else if (length(iycf_7a) == 0) {
    iycf_7a <- svDialogs::dlg_input(message= "Enter the name of the yogurt as food column","iycf_7a")$res
  }

  iycf_7b <- names(iycf)[grepl("iycf_7b|cereals",names(iycf))]
  if(length(iycf_7b) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7b, "' the correct porridge/bread/rice/noodles/pasta column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7b <- svDialogs::dlg_input(message= "Enter the name of the porridge/bread/rice/noodles/pasta column","iycf_7b")$res
    }
  } else if (length(iycf_7b) > 1){
    iycf_7b <- tcltk::tk_select.list(iycf_7b, title = "Porridge/Bread/Rice/Noodles/Pasta column")
    if(iycf_7b == ""){
      iycf_7b <- svDialogs::dlg_input(message= "Enter the name of the porridge/bread/rice/noodles/pasta column","iycf_7b")$res
    }
  } else if (length(iycf_7b) == 0) {
    iycf_7b <- svDialogs::dlg_input(message= "Enter the name of the porridge/bread/rice/noodles/pasta column","iycf_7b")$res
  }

  iycf_7c <- names(iycf)[grepl("iycf_7c|orange|pumkin",names(iycf))]
  if(length(iycf_7c) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7c, "' the correct pumpkin/carrots/sweet red peppers/squash/sweet potato column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7c <- svDialogs::dlg_input(message= "Enter the name of the pumpkin/carrots/sweet red peppers/squash/sweet potato column","iycf_7c")$res
    }
  } else if (length(iycf_7c) > 1){
    iycf_7c <- tcltk::tk_select.list(iycf_7c, title = "Pumpkin/Carrots/Sweet red peppers/Squash/Sweet potato column")
    if(iycf_7c == ""){
      iycf_7c <- svDialogs::dlg_input(message= "Enter the name of the pumpkin/carrots/sweet red peppers/squash/sweet potato column","iycf_7c")$res
    }
  } else if (length(iycf_7c) == 0) {
    iycf_7c <- svDialogs::dlg_input(message= "Enter the name of the pumpkin/carrots/sweet red peppers/squash/sweet potato column","iycf_7c")$res
  }

  iycf_7d <- names(iycf)[grepl("iycf_7d|roots",names(iycf))]
  if(length(iycf_7d) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7d, "' the correct plantains/white potato/yams/manioc/cassava column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7d <- svDialogs::dlg_input(message= "Enter the name of the plantains/white potato/yams/manioc/cassava column","iycf_7d")$res
    }
  } else if (length(iycf_7d) > 1){
    iycf_7d <- tcltk::tk_select.list(iycf_7d, title = "Plantains/White potato/Yams/Manioc/Cassava column")
    if(iycf_7d == ""){
      iycf_7d <- svDialogs::dlg_input(message= "Enter the name of the plantains/white potato/yams/manioc/cassava column","iycf_7d")$res
    }
  } else if (length(iycf_7d) == 0) {
    iycf_7d <- svDialogs::dlg_input(message= "Enter the name of the plantains/white potato/yams/manioc/cassava column","iycf_7d")$res
  }


  iycf_7e <- names(iycf)[grepl("iycf_7e|greens|leaf",names(iycf))]
  if(length(iycf_7e) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7e, "' the correct dark green leafy vegetables column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7e <- svDialogs::dlg_input(message= "Enter the name of the dark green leafy vegetables column","iycf_7e")$res
    }
  } else if (length(iycf_7e) > 1){
    iycf_7e <- tcltk::tk_select.list(iycf_7e, title = "Dark Green Leafy Vegetables column")
    if(iycf_7e == ""){
      iycf_7e <- svDialogs::dlg_input(message= "Enter the name of the dark green leafy vegetables column","iycf_7e")$res
    }
  } else if (length(iycf_7e) == 0) {
    iycf_7e <- svDialogs::dlg_input(message= "Enter the name of the dark green leafy vegetables column","iycf_7e")$res
  }

  iycf_7f <- names(iycf)[grepl("iycf_7f|other_veg",names(iycf))]
  if(length(iycf_7f) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7f, "' the correct other vegetables column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7f <- svDialogs::dlg_input(message= "Enter the name of the other vegetables column","iycf_7f")$res
    }
  } else if (length(iycf_7f) > 1){
    iycf_7f <- tcltk::tk_select.list(iycf_7f, title = "Other Vegetables column")
    if(iycf_7f == ""){
      iycf_7f <- svDialogs::dlg_input(message= "Enter the name of the other vegetables column","iycf_7f")$res
    }
  } else if (length(iycf_7f) == 0) {
    iycf_7f <- svDialogs::dlg_input(message= "Enter the name of the other vegetables column","iycf_7f")$res
  }


  iycf_7g <- names(iycf)[grepl("iycf_7g|fruit",names(iycf))]
  if(length(iycf_7g) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7g, "' the correct ripe mangoes/ripe papayas column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7g <- svDialogs::dlg_input(message= "Enter the name of the ripe mangoes/ripe papayas column","iycf_7g")$res
    }
  } else if (length(iycf_7g) > 1){
    iycf_7g <- tcltk::tk_select.list(iycf_7g, title = "Ripe mangoes/Ripe papayas column")
    if(iycf_7g == ""){
      iycf_7g <- svDialogs::dlg_input(message= "Enter the name of the ripe mangoes/ripe papayas column","iycf_7g")$res
    }
  } else if (length(iycf_7g) == 0) {
    iycf_7g <- svDialogs::dlg_input(message= "Enter the name of the ripe mangoes/ripe papayas column","iycf_7g")$res
  }

  iycf_7h <- names(iycf)[grepl("iycf_7h|other_fruit",names(iycf))]
  if(length(iycf_7h) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7h, "' the correct other fruits column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7h <- svDialogs::dlg_input(message= "Enter the name of the other fruits column","iycf_7h")$res
    }
  } else if (length(iycf_7h) > 1){
    iycf_7h <- tcltk::tk_select.list(iycf_7h, title = "Other Fruits column")
    if(iycf_7h == ""){
      iycf_7h <- svDialogs::dlg_input(message= "Enter the name of the other fruits column","iycf_7h")$res
    }
  } else if (length(iycf_7h) == 0) {
    iycf_7h <- svDialogs::dlg_input(message= "Enter the name of the other fruits column","iycf_7h")$res
  }


  iycf_7i <- names(iycf)[grepl("iycf_7i|organs",names(iycf))]
  if(length(iycf_7i) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7i, "' the correct liver/kidney/heart column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7i <- svDialogs::dlg_input(message= "Enter the name of the liver/kidney/heart column","iycf_7i")$res
    }
  } else if (length(iycf_7i) > 1){
    iycf_7i <- tcltk::tk_select.list(iycf_7i, title = "Liver/Kidney/Heart column")
    if(iycf_7i == ""){
      iycf_7i <- svDialogs::dlg_input(message= "Enter the name of the liver/kidney/heart column","iycf_7i")$res
    }
  } else if (length(iycf_7i) == 0) {
    iycf_7i <- svDialogs::dlg_input(message= "Enter the name of the liver/kidney/heart column","iycf_7i")$res
  }

  iycf_7j <- names(iycf)[grepl("iycf_7j|meat",names(iycf))]
  if(length(iycf_7j) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7j, "' the correct sausage/hot dogs/ham/bacon/salami/canned meat column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7j <- svDialogs::dlg_input(message= "Enter the name of the sausage/hot dogs/ham/bacon/salami/canned meat column","iycf_7j")$res
    }
  } else if (length(iycf_7j) > 1){
    iycf_7j <- tcltk::tk_select.list(iycf_7j, title = "Sausage/Hot dogs/Ham/Bacon/Salami/Canned meat column")
    if(iycf_7j == ""){
      iycf_7j <- svDialogs::dlg_input(message= "Enter the name of the sausage/hot dogs/ham/bacon/salami/canned meat column","iycf_7j")$res
    }
  } else if (length(iycf_7j) == 0) {
    iycf_7j <- svDialogs::dlg_input(message= "Enter the name of the sausage/hot dogs/ham/bacon/salami/canned meat column","iycf_7j")$res
  }

  iycf_7k <- names(iycf)[grepl("iycf_7k|other_meat",names(iycf))]
  if(length(iycf_7k) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7k, "' the correct meat/beef/pork/lamb/goat/chicken/duck column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7k <- svDialogs::dlg_input(message= "Enter the name of the meat/beef/pork/lamb/goat/chicken/duck column","iycf_7k")$res
    }
  } else if (length(iycf_7k) > 1){
    iycf_7k <- tcltk::tk_select.list(iycf_7k, title = "Meat/Beef/Pork/Lamb/Goat/Chicken/Duck column")
    if(iycf_7k == ""){
      iycf_7k <- svDialogs::dlg_input(message= "Enter the name of the meat/beef/pork/lamb/goat/chicken/duck column","iycf_7k")$res
    }
  } else if (length(iycf_7k) == 0) {
    iycf_7k <- svDialogs::dlg_input(message= "Enter the name of the meat/beef/pork/lamb/goat/chicken/duck column","iycf_7k")$res
  }


  iycf_7l <- names(iycf)[grepl("iycf_7l|egg",names(iycf))]
  if(length(iycf_7l) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7l, "' the correct eggs column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7l <- svDialogs::dlg_input(message= "Enter the name of the eggs column","iycf_7l")$res
    }
  } else if (length(iycf_7l) > 1){
    iycf_7l <- tcltk::tk_select.list(iycf_7l, title = "Eggs column")
    if(iycf_7l == ""){
      iycf_7l <- svDialogs::dlg_input(message= "Enter the name of the eggs column","iycf_7l")$res
    }
  } else if (length(iycf_7l) == 0) {
    iycf_7l <- svDialogs::dlg_input(message= "Enter the name of the eggs column","iycf_7l")$res
  }

  iycf_7m <- names(iycf)[grepl("iycf_7m|fish",names(iycf))]
  if(length(iycf_7m) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7m, "' the correct fresh/dried/shell fish column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7m <- svDialogs::dlg_input(message= "Enter the name of the fresh/dried/shell fish column","iycf_7m")$res
    }
  } else if (length(iycf_7m) > 1){
    iycf_7m <- tcltk::tk_select.list(iycf_7m, title = "Fresh/Dried/Shell Fish column")
    if(iycf_7m == ""){
      iycf_7m <- svDialogs::dlg_input(message= "Enter the name of the fresh/dried/shell fish column","iycf_7m")$res
    }
  } else if (length(iycf_7m) == 0) {
    iycf_7m <- svDialogs::dlg_input(message= "Enter the name of the fresh/dried/shell fish column","iycf_7m")$res
  }

  iycf_7n <- names(iycf)[grepl("iycf_7n|legumes",names(iycf))]
  if(length(iycf_7n) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7n, "' the correct beans/peas/lentils/nuts/seeds column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7n <- svDialogs::dlg_input(message= "Enter the name of the beans/peas/lentils/nuts/seeds column","iycf_7n")$res
    }
  } else if (length(iycf_7n) > 1){
    iycf_7n <- tcltk::tk_select.list(iycf_7n, title = "Beans/Peas/Lentils/Nuts/Seeds column")
    if(iycf_7n == ""){
      iycf_7n <- svDialogs::dlg_input(message= "Enter the name of the beans/peas/lentils/nuts/seeds column","iycf_7n")$res
    }
  } else if (length(iycf_7n) == 0) {
    iycf_7n <- svDialogs::dlg_input(message= "Enter the name of the beans/peas/lentils/nuts/seeds column","iycf_7n")$res
  }


  iycf_7o <- names(iycf)[grepl("iycf_7o|cheese",names(iycf))]
  if(length(iycf_7o) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7o, "' the correct hard/soft cheese column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7o <- svDialogs::dlg_input(message= "Enter the name of the hard/soft cheese column","iycf_7o")$res
    }
  } else if (length(iycf_7o) > 1){
    iycf_7o <- tcltk::tk_select.list(iycf_7o, title = "Hard/Soft Cheese column")
    if(iycf_7o == ""){
      iycf_7o <- svDialogs::dlg_input(message= "Enter the name of the hard/soft cheese column","iycf_7o")$res
    }
  } else if (length(iycf_7o) == 0) {
    iycf_7o <- svDialogs::dlg_input(message= "Enter the name of the hard/soft cheese column","iycf_7o")$res
  }

  iycf_7p <- names(iycf)[grepl("iycf_7p|sweet",names(iycf))]
  if(length(iycf_7p) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7p, "' the correct sweet foods column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7p <- svDialogs::dlg_input(message= "Enter the name of the sweet foods column","iycf_7p")$res
    }
  } else if (length(iycf_7p) > 1){
    iycf_7p <- tcltk::tk_select.list(iycf_7p, title = "Sweet Foods column")
    if(iycf_7p == ""){
      iycf_7p <- svDialogs::dlg_input(message= "Enter the name of the sweet foods column","iycf_7p")$res
    }
  } else if (length(iycf_7p) == 0) {
    iycf_7p <- svDialogs::dlg_input(message= "Enter the name of the sweet foods column","iycf_7p")$res
  }

  iycf_7q <- names(iycf)[grepl("iycf_7q|crisps|fries",names(iycf))]
  if(length(iycf_7q) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7q, "' the correct chips/crisps/puffs/french fries column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7q <- svDialogs::dlg_input(message= "Enter the name of the chips/crisps/puffs/french fries column","iycf_7q")$res
    }
  } else if (length(iycf_7q) > 1){
    iycf_7q <- tcltk::tk_select.list(iycf_7q, title = "Chips/Crisps/Puffs/French Fries column")
    if(iycf_7q == ""){
      iycf_7q <- svDialogs::dlg_input(message= "Enter the name of the chips/crisps/puffs/french fries column","iycf_7q")$res
    }
  } else if (length(iycf_7q) == 0) {
    iycf_7q <- svDialogs::dlg_input(message= "Enter the name of the chips/crisps/puffs/french fries column","iycf_7q")$res
  }

  iycf_7r <- names(iycf)[grepl("iycf_7r|other",names(iycf))]
  if(length(iycf_7r) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_7r, "' the correct other solid food column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_7r <- svDialogs::dlg_input(message= "Enter the name of the other solid food column","iycf_7r")$res
    }
  } else if (length(iycf_7r) > 1){
    iycf_7r <- tcltk::tk_select.list(iycf_7r, title = "Other Solid Food column")
    if(iycf_7r == ""){
      iycf_7r <- svDialogs::dlg_input(message= "Enter the name of the other solid food column","iycf_7r")$res
    }
  } else if (length(iycf_7r) == 0) {
    iycf_7r <- svDialogs::dlg_input(message= "Enter the name of the other solid food column","iycf_7r")$res
  }

  iycf_8 <- names(iycf)[grepl("iycf_8|mealfreq|solid",names(iycf))]
  if(length(iycf_8) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_8, "' the correct meal frequency column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_8 <- svDialogs::dlg_input(message= "Enter the name of the meal frequency column","iycf_8")$res
    }
  } else if (length(iycf_8) > 1){
    iycf_8 <- tcltk::tk_select.list(iycf_8, title = "Meal Frequency column")
    if(iycf_8 == ""){
      iycf_8 <- svDialogs::dlg_input(message= "Enter the name of the meal frequency column","iycf_8")$res
    }
  } else if (length(iycf_8) == 0) {
    iycf_8 <- svDialogs::dlg_input(message= "Enter the name of the meal frequency column","iycf_8")$res
  }

  iycf_6c_swt <- names(iycf)[grepl("iycf_6c_swt|sweet_milk",names(iycf))]
  if(length(iycf_6c_swt) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6c_swt, "' the correct sweet milk column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6c_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet milk column","iycf_6c_swt")$res
    }
  } else if (length(iycf_6c_swt) > 1){
    iycf_6c_swt <- tcltk::tk_select.list(iycf_6c_swt, title = "Sweet Milk column")
    if(iycf_6c_swt == ""){
      iycf_6c_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet milk column","iycf_6c_swt")$res
    }
  } else if (length(iycf_6c_swt) == 0) {
    iycf_6c_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet milk column","iycf_6c_swt")$res
  }

  iycf_6d_swt <- names(iycf)[grepl("iycf_6d_swt|sweet_yoghurt",names(iycf))]
  if(length(iycf_6d_swt) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6d_swt, "' the correct sweet yogurt column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6d_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet yogurt column","iycf_6d_swt")$res
    }
  } else if (length(iycf_6d_swt) > 1){
    iycf_6d_swt <- tcltk::tk_select.list(iycf_6d_swt, title = "Sweet Yogurt column")
    if(iycf_6d_swt == ""){
      iycf_6d_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet yogurt column","iycf_6d_swt")$res
    }
  } else if (length(iycf_6d_swt) == 0) {
    iycf_6d_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet yogurt column","iycf_6d_swt")$res
  }

  iycf_6h_swt <- names(iycf)[grepl("iycf_6h_swt|sweet_tea",names(iycf))]
  if(length(iycf_6h_swt) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6h_swt, "' the correct sweet tea column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6h_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet tea column","iycf_6h_swt")$res
    }
  } else if (length(iycf_6h_swt) > 1){
    iycf_6h_swt <- tcltk::tk_select.list(iycf_6h_swt, title = "Sweet Tea column")
    if(iycf_6h_swt == ""){
      iycf_6h_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet tea column","iycf_6h_swt")$res
    }
  } else if (length(iycf_6h_swt) == 0) {
    iycf_6h_swt <- svDialogs::dlg_input(message= "Enter the name of the sweet tea column","iycf_6h_swt")$res
  }

  iycf_6j_swt <- names(iycf)[grepl("iycf_6j_swt|sweet_lq_other",names(iycf))]
  if(length(iycf_6j_swt) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", iycf_6j_swt, "' the correct other sweet drink column?"), type = "yesno")$res
    if (yes_no == "no") {
      iycf_6j_swt <- svDialogs::dlg_input(message= "Enter the name of the other sweet drink column","iycf_6j_swt")$res
    }
  } else if (length(iycf_6j_swt) > 1){
    iycf_6j_swt <- tcltk::tk_select.list(iycf_6j_swt, title = "Other Sweet Drink column")
    if(iycf_6j_swt == ""){
      iycf_6j_swt <- svDialogs::dlg_input(message= "Enter the name of the other sweet drink column","iycf_6j_swt")$res
    }
  } else if (length(iycf_6j_swt) == 0) {
    iycf_6j_swt <- svDialogs::dlg_input(message= "Enter the name of the other sweet drink column","iycf_6j_swt")$res
  }

  yes_no_columns <- c(iycf_7b,iycf_7c,iycf_7d,iycf_7e,iycf_7f,iycf_7g,iycf_7h,iycf_7i,iycf_7j,
                      iycf_7k,iycf_7l,iycf_7m,iycf_7n,iycf_7o,iycf_7p,iycf_7q)

  yes_answer <- tcltk::tk_select.list(unique(unlist(iycf[,yes_no_columns])), title = "Yes Value")
  no_answer <- tcltk::tk_select.list(unique(unlist(iycf[,yes_no_columns])) %>% unique, title = "No Value")
  dnk_answer <- tcltk::tk_select.list(unique(unlist(iycf[,yes_no_columns])) %>% unique, title = "Don't Know Value")
  pnta_answer <- tcltk::tk_select.list(unique(unlist(iycf[,yes_no_columns])) %>% unique, title = "Prefer Not To Answer Value")

  if(!is.null(iycf_2)){
    if(!is_empty_new("iycf_2")){
      iycf2_immediate_value <- tcltk::tk_select.list(unique(unlist(iycf[,iycf_2])) %>% unique, title = "Immediately Value")
      iycf2_lessday_value <- tcltk::tk_select.list(unique(unlist(iycf[,iycf_2])) %>% unique, title = "Less Than A Day Value")
      iycf2_moreday_value <- tcltk::tk_select.list(unique(unlist(iycf[,iycf_2])) %>% unique, title = "More Than A Day Value")
    } else {
      iycf2_immediate_value  <- NULL
      iycf2_lessday_value  <- NULL
      iycf2_moreday_value  <- NULL
    }

  } else {
    iycf2_immediate_value  <- NULL
    iycf2_lessday_value  <- NULL
    iycf2_moreday_value  <- NULL
  }
  if(!all(c(uuid_iycf,age_months) %in% names(iycf))) {
    svDialogs::dlg_message("Please check if the uuid or Age month column exist in the IYCF dataset")
    stop("Please check if the uuid or Age month column exist in the IYCF dataset")
  } else{
    iycf <- iycf %>%
      impactR4PHU::add_iycf(age_months = age_months,
                            iycf_1 = iycf_1,
                            iycf_2 = iycf_2,
                            iycf_3 = iycf_3,
                            iycf_4 = iycf_4,
                            iycf_5 = iycf_5,
                            iycf_6a = iycf_6a,
                            iycf_6b = iycf_6b,
                            iycf_6c = iycf_6c,
                            iycf_6d = iycf_6d,
                            iycf_6e = iycf_6e,
                            iycf_6f = iycf_6f,
                            iycf_6g = iycf_6g,
                            iycf_6h = iycf_6h,
                            iycf_6i = iycf_6i,
                            iycf_6j = iycf_6j,
                            iycf_7a = iycf_7a,
                            iycf_7b = iycf_7b,
                            iycf_7c = iycf_7c,
                            iycf_7d = iycf_7d,
                            iycf_7e = iycf_7e,
                            iycf_7f = iycf_7f,
                            iycf_7g = iycf_7g,
                            iycf_7h = iycf_7h,
                            iycf_7i = iycf_7i,
                            iycf_7j = iycf_7j,
                            iycf_7k = iycf_7k,
                            iycf_7l = iycf_7l,
                            iycf_7m = iycf_7m,
                            iycf_7n = iycf_7n,
                            iycf_7o = iycf_7o,
                            iycf_7p = iycf_7p,
                            iycf_7q = iycf_7q,
                            iycf_7r = iycf_7r,
                            iycf_8 = iycf_8,
                            iycf_6c_swt = iycf_6c_swt,
                            iycf_6d_swt = iycf_6d_swt,
                            iycf_6h_swt = iycf_6h_swt,
                            iycf_6j_swt = iycf_6j_swt,
                            yes_value = yes_answer,
                            no_value = no_answer,
                            dnk_value = dnk_answer,
                            pna_value = pnta_answer,
                            iycf2_immediate_value = iycf2_immediate_value,
                            iycf2_lessday_value = iycf2_lessday_value,
                            iycf2_moreday_value = iycf2_moreday_value,
                            uuid = uuid_iycf)
  }
} else {
  iycf <- iycf %>%
    impactR4PHU::add_iycf(age_months = age_months,
                          iycf_1 = iycf_1,
                          iycf_2 = iycf_2,
                          iycf_3 = iycf_3,
                          iycf_4 = iycf_4,
                          iycf_5 = iycf_5,
                          iycf_6a = iycf_6a,
                          iycf_6b = iycf_6b,
                          iycf_6c = iycf_6c,
                          iycf_6d = iycf_6d,
                          iycf_6e = iycf_6e,
                          iycf_6f = iycf_6f,
                          iycf_6g = iycf_6g,
                          iycf_6h = iycf_6h,
                          iycf_6i = iycf_6i,
                          iycf_6j = iycf_6j,
                          iycf_7a = iycf_7a,
                          iycf_7b = iycf_7b,
                          iycf_7c = iycf_7c,
                          iycf_7d = iycf_7d,
                          iycf_7e = iycf_7e,
                          iycf_7f = iycf_7f,
                          iycf_7g = iycf_7g,
                          iycf_7h = iycf_7h,
                          iycf_7i = iycf_7i,
                          iycf_7j = iycf_7j,
                          iycf_7k = iycf_7k,
                          iycf_7l = iycf_7l,
                          iycf_7m = iycf_7m,
                          iycf_7n = iycf_7n,
                          iycf_7o = iycf_7o,
                          iycf_7p = iycf_7p,
                          iycf_7q = iycf_7q,
                          iycf_7r = iycf_7r,
                          iycf_8 = iycf_8,
                          iycf_6c_swt = iycf_6c_swt,
                          iycf_6d_swt = iycf_6d_swt,
                          iycf_6h_swt = iycf_6h_swt,
                          iycf_6j_swt = iycf_6j_swt,
                          yes_value = yes_answer,
                          no_value = no_answer,
                          dnk_value = dnk_answer,
                          pna_value = pnta_answer,
                          iycf2_immediate_value = iycf2_immediate_value,
                          iycf2_lessday_value = iycf2_lessday_value,
                          iycf2_moreday_value = iycf2_moreday_value,
                          uuid = uuid_iycf)
}


enum_iycf <- main %>%
  dplyr::select(uuid_main,enumerator) %>%
  dplyr::rename(uuid = uuid_main)

iycf <- iycf %>%
  dplyr::rename(uuid = uuid_iycf) %>%
  dplyr::left_join(enum_iycf)



data.list[[path.sheet.with.main]] <- main
data.list[[path.sheet.with.iycf]] <- iycf


yes_no_weight <- svDialogs::dlg_message("Is your data weighted?", type = "yesno")$res
if(yes_no_weight == "yes"){
  weight <- names(data.list[[path.sheet.with.main]])[grepl("weight",names(data.list[[path.sheet.with.main]]))]
  if(length(weight) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", weight, "' the correct weight column?"), type = "yesno")$res
    if (yes_no == "no") {
      weight <- svDialogs::dlg_input(message= "Enter the name of the weight column","weight")$res
    }
  } else if (length(weight) > 1){
    weight <- tcltk::tk_select.list(weight, title = "Weight column")
    if(weight == "") {
      weight <- svDialogs::dlg_input(message= "Enter the name of the weight column","weight")$res
    }
  } else if (length(weight) == 0) {
    weight <- svDialogs::dlg_input(message= "Enter the name of the weight column","weight")$res
  }
} else {
  weight <- 1
}
if(yes_no_weight == "yes"){
  for(sheet in names(data.list)){
    data.list[[sheet]] <- data.list[[sheet]] %>%
      mutate(overall = "overall",
             weight = !!rlang::sym(weight)) %>%
      mutate_at(vars(everything()),~ifelse(. == "",NA,.))
  }
} else {
  for(sheet in names(data.list)){
    data.list[[sheet]] <- data.list[[sheet]] %>%
      mutate(overall = "overall",
             weight = 1) %>%
      mutate_at(vars(everything()),~ifelse(. == "",NA,.))
  }
}

###### CONTINUE HERE
list_of_var <- c("uuid_main","enumerator","iycf_caregiver",
                 "age_months","iycf_1","iycf_2","iycf_3","iycf_4","iycf_5","iycf_6a",
                 "iycf_6b","iycf_6c","iycf_6d","iycf_6e","iycf_6f","iycf_6g","iycf_6h","iycf_6i",
                 "iycf_6j","iycf_7a","iycf_7b","iycf_7c","iycf_7d","iycf_7e","iycf_7f","sex","team",
                 "iycf_7g","iycf_7h","iycf_7i","iycf_7j","iycf_7k","iycf_7l","iycf_7m","yes_no_team",
                 "iycf_7n","iycf_7o","iycf_7p","iycf_7q","iycf_7r","iycf_8","yes_no_weight","weight",
                 "iycf_6c_swt","iycf_6d_swt","iycf_6h_swt","iycf_6j_swt","yes_answer","no_answer","dnk_answer","male","female",
                 "pnta_answer","iycf2_immediate_value","iycf2_lessday_value","iycf2_moreday_value","uuid_iycf")

if(!file.exists("inputs/environment.Rdata")){
  save(list = list_of_var, file = "inputs/environment.Rdata")
}

## Create IYCF Tables for EACH CALCULATED IYCF

##### 0 to 23 month

zero_to_23 <- iycf %>%
  dplyr::select(uuid_iycf,
                sex,
                iycf_evbf,
                iycf_eibf,
                iycf_ebf2d,
                iycf_bof)

total_zero_to_23 <- nrow(filter(iycf, as.numeric(!!rlang::sym(age_months)) < 24))

overall_zero_to_23 <- zero_to_23 %>%
  dplyr::group_by() %>%
  dplyr::summarise(`Ever Breastfed` = sum(iycf_evbf, na.rm = T),
                   `Early Initiation of Breastfeeding` = sum(iycf_eibf, na.rm = T),
                   `Exclusive Breastfeeding First 2 Days After Birth` = sum(iycf_ebf2d, na.rm = T),
                   `Bottle Feeding` = sum(iycf_bof, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = everything()) %>%
  dplyr::mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))

gender_zero_to_23 <- zero_to_23 %>%
  dplyr::group_by(!!rlang::sym(sex)) %>%
  dplyr::summarise(`Ever Breastfed` = sum(iycf_evbf, na.rm = T),
                   `Early Initiation of Breastfeeding` = sum(iycf_eibf, na.rm = T),
                   `Exclusive Breastfeeding First 2 Days After Birth` = sum(iycf_ebf2d, na.rm = T),
                   `Bottle Feeding` = sum(iycf_bof, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = -sex) %>%
  mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))%>%
  dplyr::mutate(!!rlang::sym(sex) := case_when(!!rlang::sym(sex) == male ~ "Male",
                                               !!rlang::sym(sex) == female ~ "Female")) %>%
  pivot_wider(names_from = sex, values_from = c(value, prop))



zero_to_6 <- iycf %>%
  dplyr::select(uuid_iycf,
                sex,
                iycf_ebf,
                iycf_mixmf)

total_zero_to_6 <-  nrow(filter(iycf, as.numeric(!!rlang::sym(age_months)) < 7))

overall_zero_to_6 <- zero_to_6 %>%
  dplyr::group_by() %>%
  dplyr::summarise(`Exclusive Breastfeeding` = sum(iycf_ebf, na.rm = T),
                   `Mixed Milk Feeding` = sum(iycf_mixmf, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = everything()) %>%
  dplyr::mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))

gender_zero_to_6 <- zero_to_6 %>%
  dplyr::group_by(!!rlang::sym(sex)) %>%
  dplyr::summarise(`Exclusive Breastfeeding` = sum(iycf_ebf, na.rm = T),
                   `Mixed Milk Feeding` = sum(iycf_mixmf, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = -sex) %>%
  mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))%>%
  dplyr::mutate(!!rlang::sym(sex) := case_when(!!rlang::sym(sex) == male ~ "Male",
                                               !!rlang::sym(sex) == female ~ "Female")) %>%
  pivot_wider(names_from = sex, values_from = c(value, prop))


twelve_to_23 <- iycf %>%
  dplyr::select(uuid_iycf,
                sex,
                iycf_cbf)

total_twelve_to_23 <-  nrow(filter(iycf, as.numeric(!!rlang::sym(age_months)) > 11 &
                                     as.numeric(!!rlang::sym(age_months)) < 24))

overall_twelve_to_23 <- twelve_to_23 %>%
  dplyr::group_by() %>%
  dplyr::summarise(`Continued Breastfeeding` = sum(iycf_cbf, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = everything()) %>%
  dplyr::mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))

gender_twelve_to_23 <- twelve_to_23 %>%
  dplyr::group_by(!!rlang::sym(sex)) %>%
  dplyr::summarise(`Continued Breastfeeding` = sum(iycf_cbf, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = -sex) %>%
  mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))%>%
  dplyr::mutate(!!rlang::sym(sex) := case_when(!!rlang::sym(sex) == male ~ "Male",
                                               !!rlang::sym(sex) == female ~ "Female")) %>%
  pivot_wider(names_from = sex, values_from = c(value, prop))


six_to_8 <- iycf %>%
  dplyr::select(uuid_iycf,
                sex,
                iycf_isssf)

total_six_to_8 <-  nrow(filter(iycf, as.numeric(!!rlang::sym(age_months)) > 5 &
                                 as.numeric(!!rlang::sym(age_months)) < 9))

overall_six_to_8 <- six_to_8 %>%
  dplyr::group_by() %>%
  dplyr::summarise(`Introduction of Solid, Semi-Solid, or Soft Foods` = sum(iycf_isssf, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = everything()) %>%
  dplyr::mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))

gender_six_to_8 <- six_to_8 %>%
  dplyr::group_by(!!rlang::sym(sex)) %>%
  dplyr::summarise(`Introduction of Solid, Semi-Solid, or Soft Foods` = sum(iycf_isssf, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = -sex) %>%
  mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))%>%
  dplyr::mutate(!!rlang::sym(sex) := case_when(!!rlang::sym(sex) == male ~ "Male",
                                               !!rlang::sym(sex) == female ~ "Female")) %>%
  pivot_wider(names_from = sex, values_from = c(value, prop))

six_to_23 <- iycf %>%
  dplyr::select(uuid_iycf,
                sex,
                iycf_mdd_cat,
                iycf_mmf,
                iycf_mmff,
                iycf_mad,
                iycf_eff,
                iycf_swb,
                iycf_ufc,
                iycf_zvf)

total_six_to_23 <-  nrow(filter(iycf, as.numeric(!!rlang::sym(age_months)) > 5 &
                                  as.numeric(!!rlang::sym(age_months)) < 24))

overall_six_to_23 <- six_to_23 %>%
  dplyr::group_by() %>%
  dplyr::summarise(`Minimum Dietary Diversity` = sum(iycf_mdd_cat, na.rm = T),
                   `Minimum Meal Frequency` = sum(iycf_mmf, na.rm = T),
                   `Minimum Milk Feeding Frequency For Non-Breastfed Children` = sum(iycf_mmff, na.rm = T),
                   `Minimum Acceptable Diet` = sum(iycf_mad, na.rm = T),
                   `Eggs & Flesh Foods Consumption` = sum(iycf_eff, na.rm = T),
                   `Sweet Beverage Consumption` = sum(iycf_swb, na.rm = T),
                   `Unhealthy Food Consumption` = sum(iycf_ufc, na.rm = T),
                   `Zero Vegetable or Fruit Consumption` = sum(iycf_zvf, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = everything()) %>%
  dplyr::mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))

gender_six_to_23 <- six_to_23 %>%
  dplyr::group_by(!!rlang::sym(sex)) %>%
  dplyr::summarise(`Minimum Dietary Diversity` = sum(iycf_mdd_cat, na.rm = T),
                   `Minimum Meal Frequency` = sum(iycf_mmf, na.rm = T),
                   `Minimum Milk Feeding Frequency For Non-Breastfed Children` = sum(iycf_mmff, na.rm = T),
                   `Minimum Acceptable Diet` = sum(iycf_mad, na.rm = T),
                   `Eggs & Flesh Foods Consumption` = sum(iycf_eff, na.rm = T),
                   `Sweet Beverage Consumption` = sum(iycf_swb, na.rm = T),
                   `Unhealthy Food Consumption` = sum(iycf_ufc, na.rm = T),
                   `Zero Vegetable or Fruit Consumption` = sum(iycf_zvf, na.rm = T)) %>%
  # dplyr::mutate_all(.,as.character) %>%
  pivot_longer(cols = -sex) %>%
  mutate(prop = paste0(round((value/total_zero_to_23)*100,2), " %"))%>%
  dplyr::mutate(!!rlang::sym(sex) := case_when(!!rlang::sym(sex) == male ~ "Male",
                                               !!rlang::sym(sex) == female ~ "Female")) %>%
  pivot_wider(names_from = sex, values_from = c(value, prop))

