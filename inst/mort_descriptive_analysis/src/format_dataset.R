################################################################################
### FORMAT DATASET
################################################################################

# create shorthands to make working with the data easier:
raw.main <- data.list$main
raw.hh_roster <- data.list$hh_roster
raw.left_member <- data.list$left_member
raw.died_member <- data.list$died_member
collected_df_left <- tcltk::tk_select.list(c(TRUE,FALSE), title = "Did you collect info about leavers? (1 for Yes, 0 for No")
if(file.exists("inputs/environment.Rdata")) {
  load("inputs/environment.Rdata")
}

if(!file.exists("inputs/environment.Rdata")) {
  ## Detect date of data collection column
  date_dc <- names(raw.main)[grepl("today",names(raw.main))]
  if(length(date_dc) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", date_dc, "' the correct date of data collection column?"), type = "yesno")$res
    if(yes_no == "no"){
      date_dc <- svDialogs::dlg_input(message= "Enter the name of the date of data collection column","date_dc")$res
    }
  } else if (length(date_dc) > 1){
    date_dc <- tcltk::tk_select.list(date_dc, title = "Date of data collection column")
  } else if (length(date_dc) == 0) {
    date_dc <- svDialogs::dlg_input(message= "Enter the name of the date of data collection column","date_dc")$res
  }

  ## Detect Recall date column
  date_recall_event <- names(raw.main)[grepl("recall",names(raw.main))]
  if(length(date_recall_event) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", date_recall_event, "' the correct recall date column?"), type = "yesno")$res
    if(yes_no == "no"){
      date_recall_event <- svDialogs::dlg_input(message= "Enter the name of the recall date column","date_recall_event")$res
    }
  } else if (length(date_recall_event) > 1){
    date_recall_event <- tcltk::tk_select.list(date_recall_event, title = "Recall date column")
  } else if (length(date_recall_event) == 0) {
    date_recall_event <- svDialogs::dlg_input(message= "Enter the name of the recall date column","date_recall_event")$res
  }


  ## Detect Team column
  yes_no_team <- svDialogs::dlg_message(paste0("Is there teams of enumerators?"), type = "yesno")$res
  if(yes_no_team == "yes"){
    team <- names(raw.main)[grepl("team|organization|organisation",names(raw.main))]

    if(length(team) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", team, "' the correct team/organization column?"), type = "yesno")$res
      if(yes_no == "no"){
        team <- svDialogs::dlg_input(message= "Enter the name of the team/organization Column","enumerator")$res
      }
    } else if (length(team) > 1){
      team <- tcltk::tk_select.list(team, title = "Team/Organization Columns")
    } else if (length(team) == 0) {
      team <- svDialogs::dlg_input(message= "Enter the name of the team/organization Column","team")$res
    }
  } else {
    team <- NULL
  }
  ## Detect Enumerator column
  enumerator <- names(raw.main)[grepl("enum|team",names(raw.main))]

  if(length(enumerator) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", enumerator, "' the correct enumerator column?"), type = "yesno")$res
    if(yes_no == "no"){
      enumerator <- svDialogs::dlg_input(message= "Enter the name of the Enumerator Column","enumerator")$res
    }
  } else if (length(enumerator) > 1){
    enumerator <- tcltk::tk_select.list(enumerator, title = "Enumerator Column")
  } else if (length(enumerator) == 0) {
    enumerator <- svDialogs::dlg_input(message= "Enter the name of the Enumerator Column","enumerator")$res
  }

  ## Detect Admin1 column
  admin1 <- names(raw.main)[grepl("admin",names(raw.main))]

  if(length(admin1) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", admin1, "' the correct admin1 column?"), type = "yesno")$res
    if(yes_no == "no"){
      admin1 <- svDialogs::dlg_input(message= "Enter the name of the Admin 1 Column","admin1")$res
    }
  } else if (length(admin1) > 1){
    admin1 <- tcltk::tk_select.list(admin1, title = "Admin 1 Column")
  } else if (length(admin1) == 0) {
    admin1 <- svDialogs::dlg_input(message= "Enter the name of the Admin 1 Column","admin1")$res
  }

  ## Detect Admin1 column
  admin2 <- names(raw.main)[grepl("admin",names(raw.main))]

  if(length(admin2) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", admin2, "' the correct admin2 column?"), type = "yesno")$res
    if(yes_no == "no"){
      admin2 <- svDialogs::dlg_input(message= "Enter the name of the Admin 2 Column","admin2")$res
    }
  } else if (length(admin2) > 1){
    admin2 <- tcltk::tk_select.list(admin2, title = "Admin 2 Column")
  } else if (length(admin2) == 0) {
    admin2 <- svDialogs::dlg_input(message= "Enter the name of the Admin 2 Column","admin2")$res
  }

  ## Detect Cluster column
  cluster <- names(raw.main)[grepl("cluster",names(raw.main))]

  if(length(cluster) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", cluster, "' the correct cluster column?"), type = "yesno")$res
    if(yes_no == "no"){
      cluster <- svDialogs::dlg_input(message= "Enter the name of the Cluster Column","cluster")$res
    }
  } else if (length(cluster) > 1){
    cluster <- tcltk::tk_select.list(cluster, title = "Cluster Column")
  } else if (length(cluster) == 0) {
    cluster <- svDialogs::dlg_input(message= "Enter the name of the Cluster Column","cluster")$res
  }

  ## Detect uuid_main column
  uuid_main <- names(raw.main)[grepl("uuid",names(raw.main))]

  if(length(uuid_main) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_main, "' the correct HH UUID column in main data?"), type = "yesno")$res
    if(yes_no == "no"){
      uuid_main <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in main data","uuid_main")$res
    }
  } else if (length(uuid_main) > 1){
    uuid_main <- tcltk::tk_select.list(uuid_main, title = "HH UUID Column [Main Data]")
  } else if (length(uuid_main) == 0) {
    uuid_main <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in main data","uuid_main")$res
  }

  ## Detect uuid_roster column
  uuid_roster <- names(raw.hh_roster)[grepl("uuid",names(raw.hh_roster))]

  if(length(uuid_roster) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_roster, "' the correct HH UUID column in roster data?"), type = "yesno")$res
    if(yes_no == "no"){
      uuid_roster <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in roster data","uuid_roster")$res
    }
  } else if (length(uuid_roster) > 1){
    uuid_roster <- tcltk::tk_select.list(uuid_roster, title = "HH UUID Column [Roster Data]")
  } else if (length(uuid_roster) == 0) {
    uuid_roster <- svDialogs::dlg_input(message= "Enter the name of the HH UUID Column in roster data","uuid_roster")$res
  }

  ## Detect sex_roster column
  sex_roster <- names(raw.hh_roster)[grepl("sex",names(raw.hh_roster))]

  if(length(sex_roster) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", sex_roster, "' the correct sex roster column?"), type = "yesno")$res
    if(yes_no == "no"){
      sex_roster <- svDialogs::dlg_input(message= "Enter the name of the sex roster column","sex_roster")$res
    }
  } else if (length(sex_roster) > 1){
    sex_roster <- tcltk::tk_select.list(sex_roster, title = "Sex Column [Roster Data]")
  } else if (length(sex_roster) == 0) {
    sex_roster <- svDialogs::dlg_input(message= "Enter the name of the sex roster column","sex_roster")$res
  }

  ## Detect age_roster column
  age_roster <- names(raw.hh_roster)[grepl("final",names(raw.hh_roster))]

  if(length(age_roster) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", age_roster, "' the correct age roster column?"), type = "yesno")$res
    if(yes_no == "no"){
      age_roster <- svDialogs::dlg_input(message= "Enter the name of the age roster column","age_roster")$res
    }
  } else if (length(age_roster) > 1){
    age_roster <- tcltk::tk_select.list(age_roster, title = "Age Column [Roster Data]")
  } else if (length(age_roster) == 0) {
    age_roster <- svDialogs::dlg_input(message= "Enter the name of the age roster column","age_roster")$res
  }

  ## Detect birth_roster column
  birth_roster <- names(raw.hh_roster)[grepl("born",names(raw.hh_roster))]

  if(length(birth_roster) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", birth_roster, "' the correct birth roster column?"), type = "yesno")$res
    if(yes_no == "no"){
      birth_roster <- svDialogs::dlg_input(message= "Enter the name of the birth roster column","birth_roster")$res
    }
  } else if (length(birth_roster) > 1){
    birth_roster <- tcltk::tk_select.list(birth_roster, title = "Birth Column [Roster Data]")
  } else if (length(birth_roster) == 0) {
    birth_roster <- svDialogs::dlg_input(message= "Enter the name of the birth roster column","birth_roster")$res
  }

  ## Detect birthdate_roster column
  birthdate_roster <- names(raw.hh_roster)[grepl("dob",names(raw.hh_roster))]

  if(length(birthdate_roster) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", birthdate_roster, "' the correct birth date roster column?"), type = "yesno")$res
    if(yes_no == "no"){
      birthdate_roster <- svDialogs::dlg_input(message= "Enter the name of the birth date roster column","birthdate_roster")$res
    }
  } else if (length(birthdate_roster) > 1){
    birthdate_roster <- tcltk::tk_select.list(birthdate_roster, title = "Birth date Column [Roster Data]")
  } else if (length(birthdate_roster) == 0) {
    birthdate_roster <- svDialogs::dlg_input(message= "Enter the name of the birth date roster column","birthdate_roster")$res
  }

  ## Detect joined_roster column
  joined_roster <- names(raw.hh_roster)[grepl("join",names(raw.hh_roster))]

  if(length(joined_roster) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", joined_roster, "' the correct joined roster column?"), type = "yesno")$res
    if(yes_no == "no"){
      joined_roster <- svDialogs::dlg_input(message= "Enter the name of the joined roster column","joined_roster")$res
    }
  } else if (length(joined_roster) > 1){
    joined_roster <- tcltk::tk_select.list(joined_roster, title = "Joined Column [Roster Data]")
  } else if (length(joined_roster) == 0) {
    joined_roster <- svDialogs::dlg_input(message= "Enter the name of the joined roster column","joined_roster")$res
  }

  ## Detect joined_date_roster column
  joined_date_roster <- names(raw.hh_roster)[grepl("final",names(raw.hh_roster))]

  if(length(joined_date_roster) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", joined_date_roster, "' the correct joined date roster column?"), type = "yesno")$res
    if(yes_no == "no"){
      joined_date_roster <- svDialogs::dlg_input(message= "Enter the name of the joined date roster column","joined_date_roster")$res
    }
  } else if (length(joined_date_roster) > 1){
    joined_date_roster <- tcltk::tk_select.list(joined_date_roster, title = "Joined Date Column [Roster Data]")
  } else if (length(joined_date_roster) == 0) {
    joined_date_roster <- svDialogs::dlg_input(message= "Enter the name of the joined date roster column","joined_date_roster")$res
  }
  if(collected_df_left){
    ## Detect uuid_left column
    uuid_left <- names(raw.left_member)[grepl("uuid",names(raw.left_member))]

    if(length(uuid_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_left, "' the correct HH UUID column in left data?"), type = "yesno")$res
      if(yes_no == "no"){
        uuid_left <- svDialogs::dlg_input(message= "Enter the name of the HH UUID column in left data","uuid_left")$res
      }
    } else if (length(uuid_left) > 1){
      uuid_left <- tcltk::tk_select.list(uuid_left, title = "HH UUID Column [Left Data]")
    } else if (length(uuid_left) == 0) {
      uuid_left <- svDialogs::dlg_input(message= "Enter the name of the HH UUID column in left data","uuid_left")$res
    }

    ## Detect sex_left column
    sex_left <- names(raw.left_member)[grepl("sex",names(raw.left_member))]

    if(length(sex_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", sex_left, "' the correct sex left column?"), type = "yesno")$res
      if(yes_no == "no"){
        sex_left <- svDialogs::dlg_input(message= "Enter the name of the sex left column","sex_left")$res
      }
    } else if (length(sex_left) > 1){
      sex_left <- tcltk::tk_select.list(sex_left, title = "Sex date Column [Left Data]")
    } else if (length(sex_left) == 0) {
      sex_left <- svDialogs::dlg_input(message= "Enter the name of the sex left column","sex_left")$res
    }

    ## Detect age_left column
    age_left <- names(raw.left_member)[grepl("final",names(raw.left_member))]

    if(length(age_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", age_left, "' the correct age left column?"), type = "yesno")$res
      if(yes_no == "no"){
        age_left <- svDialogs::dlg_input(message= "Enter the name of the age left column","age_left")$res
      }
    } else if (length(age_left) > 1){
      age_left <- tcltk::tk_select.list(age_left, title = "Age date Column [Left Data]")
    } else if (length(age_left) == 0) {
      age_left <- svDialogs::dlg_input(message= "Enter the name of the age left column","age_left")$res
    }

    ## Detect birth_left column
    birth_left <- names(raw.left_member)[grepl("born",names(raw.left_member))]

    if(length(birth_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", birth_left, "' the correct birth column?"), type = "yesno")$res
      if(yes_no == "no"){
        birth_left <- svDialogs::dlg_input(message= "Enter the name of the birth column","birth_left")$res
      }
    } else if (length(birth_left) > 1){
      birth_left <- tcltk::tk_select.list(birth_left, title = "Birth date Column [Left Data]")
    } else if (length(birth_left) == 0) {
      birth_left <- svDialogs::dlg_input(message= "Enter the name of the birth column","birth_left")$res
    }

    ## Detect birthdate_left column
    birthdate_left <- names(raw.left_member)[grepl("dob",names(raw.left_member))]

    if(length(birthdate_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", birthdate_left, "' the correct birth date column?"), type = "yesno")$res
      if(yes_no == "no"){
        birthdate_left <- svDialogs::dlg_input(message= "Enter the name of the birth date column","birthdate_left")$res
      }
    } else if (length(birthdate_left) > 1){
      birthdate_left <- tcltk::tk_select.list(birthdate_left, title = "Birth date Column [Left Data]")
    } else if (length(birthdate_left) == 0) {
      birthdate_left <- svDialogs::dlg_input(message= "Enter the name of the birth date column","birthdate_left")$res
    }

    ## Detect joined_left column
    joined_left <- names(raw.left_member)[grepl("present",names(raw.left_member))]

    if(length(joined_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", joined_left, "' the correct joined column in left data?"), type = "yesno")$res
      if(yes_no == "no"){
        joined_left <- svDialogs::dlg_input(message= "Enter the name of the joined column in left data","joined_left")$res
      }
    } else if (length(joined_left) > 1){
      joined_left <- tcltk::tk_select.list(joined_left, title = "Joined Column [Left Data]")
    } else if (length(joined_left) == 0) {
      joined_left <- svDialogs::dlg_input(message= "Enter the name of the joined column in left data","joined_left")$res
    }

    ## Detect joined_date_left column
    joined_date_left <- names(raw.left_member)[grepl("final",names(raw.left_member))]

    if(length(joined_date_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", joined_date_left, "' the correct joined date column in left data?"), type = "yesno")$res
      if(yes_no == "no"){
        joined_date_left <- svDialogs::dlg_input(message= "Enter the name of the joined date column in left data","joined_date_left")$res
      }
    } else if (length(joined_date_left) > 1){
      joined_date_left <- tcltk::tk_select.list(joined_date_left, title = "Joined date Column [Left Data]")
    } else if (length(joined_date_left) == 0) {
      joined_date_left <- svDialogs::dlg_input(message= "Enter the name of the joined date column in left data","joined_date_left")$res
    }

    ## Detect left_date_left column
    left_date_left <- names(raw.left_member)[grepl("final",names(raw.left_member))]

    if(length(left_date_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", left_date_left, "' the correct left date column?"), type = "yesno")$res
      if(yes_no == "no"){
        left_date_left <- svDialogs::dlg_input(message= "Enter the name of the left date column","left_date_left")$res
      }
    } else if (length(left_date_left) > 1){
      left_date_left <- tcltk::tk_select.list(left_date_left, title = "Left date Column [Left Data]")
    } else if (length(left_date_left) == 0) {
      left_date_left <- svDialogs::dlg_input(message= "Enter the name of the left date column","left_date_left")$res
    }
  }
  ## Detect uuid_died column
  uuid_died <- names(raw.died_member)[grepl("uuid",names(raw.died_member))]

  if(length(uuid_died) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", uuid_died, "' the correct HH UUID column in died data?"), type = "yesno")$res
    if(yes_no == "no"){
      uuid_died <- svDialogs::dlg_input(message= "Enter the name of the HH UUID column in died data","uuid_died")$res
    }
  } else if (length(uuid_died) > 1){
    uuid_died <- tcltk::tk_select.list(uuid_died, title = "HH UUID Column [Died Data]")
  } else if (length(uuid_died) == 0) {
    uuid_died <- svDialogs::dlg_input(message= "Enter the name of the HH UUID column in died data","uuid_died")$res
  }

  ## Detect sex_died column
  sex_died <- names(raw.died_member)[grepl("sex",names(raw.died_member))]

  if(length(sex_died) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", sex_died, "' the correct sex died column?"), type = "yesno")$res
    if(yes_no == "no"){
      sex_died <- svDialogs::dlg_input(message= "Enter the name of the sex died column","sex_died")$res
    }
  } else if (length(sex_died) > 1){
    sex_died <- tcltk::tk_select.list(sex_died, title = "Sex date Column [Died Data]")
  } else if (length(sex_died) == 0) {
    sex_died <- svDialogs::dlg_input(message= "Enter the name of the sex died column","sex_died")$res
  }

  ## Detect age_died column
  age_died <- names(raw.died_member)[grepl("final",names(raw.died_member))]

  if(length(age_died) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", age_died, "' the correct age died column?"), type = "yesno")$res
    if(yes_no == "no"){
      age_died <- svDialogs::dlg_input(message= "Enter the name of the age died column","age_died")$res
    }
  } else if (length(age_died) > 1){
    age_died <- tcltk::tk_select.list(age_died, title = "Age date Column [Died Data]")
  } else if (length(age_died) == 0) {
    age_died <- svDialogs::dlg_input(message= "Enter the name of the age died column","age_died")$res
  }

  ## Detect birth_died column
  birth_died <- names(raw.died_member)[grepl("born",names(raw.died_member))]

  if(length(birth_died) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", birth_died, "' the correct birth column?"), type = "yesno")$res
    if(yes_no == "no"){
      birth_died <- svDialogs::dlg_input(message= "Enter the name of the birth column","birth_died")$res
    }
  } else if (length(birth_died) > 1){
    birth_died <- tcltk::tk_select.list(birth_died, title = "Birth Column [Died Data]")
  } else if (length(birth_died) == 0) {
    birth_died <- svDialogs::dlg_input(message= "Enter the name of the birth column","birth_died")$res
  }

  ## Detect birthdate_died column
  birthdate_died <- names(raw.died_member)[grepl("dob",names(raw.died_member))]

  if(length(birthdate_died) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", birthdate_died, "' the correct birth date column?"), type = "yesno")$res
    if(yes_no == "no"){
      birthdate_died <- svDialogs::dlg_input(message= "Enter the name of the birth date column","birthdate_died")$res
    }
  } else if (length(birthdate_died) > 1){
    birthdate_died <- tcltk::tk_select.list(birthdate_died, title = "Birth date Column [Died Data]")
  } else if (length(birthdate_died) == 0) {
    birthdate_died <- svDialogs::dlg_input(message= "Enter the name of the birth date column","birthdate_died")$res
  }

  ## Detect joined_died column
  joined_died <- names(raw.died_member)[grepl("present",names(raw.died_member))]

  if(length(joined_died) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", joined_died, "' the correct joined column in died data?"), type = "yesno")$res
    if(yes_no == "no"){
      joined_died <- svDialogs::dlg_input(message= "Enter the name of the joined column in died data","joined_died")$res
    }
  } else if (length(joined_died) > 1){
    joined_died <- tcltk::tk_select.list(joined_died, title = "Joined Column [Died Data]")
  } else if (length(joined_died) == 0) {
    joined_died <- svDialogs::dlg_input(message= "Enter the name of the joined column in died data","joined_died")$res
  }

  ## Detect joined_date_died column
  joined_date_died <- names(raw.died_member)[grepl("final",names(raw.died_member))]

  if(length(joined_date_died) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", joined_date_died, "' the correct joined date column in died data?"), type = "yesno")$res
    if(yes_no == "no"){
      joined_date_died <- svDialogs::dlg_input(message= "Enter the name of the joined date column in died data","joined_date_died")$res
    }
  } else if (length(joined_date_died) > 1){
    joined_date_died <- tcltk::tk_select.list(joined_date_died, title = "Joined date Column [Died Data]")
  } else if (length(joined_date_died) == 0) {
    joined_date_died <- svDialogs::dlg_input(message= "Enter the name of the joined date column in died data","joined_date_died")$res
  }
  ## Detect date_death column
  date_death <- names(raw.died_member)[grepl("final",names(raw.died_member))]

  if(length(date_death) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", date_death, "' the correct death date column in died data?"), type = "yesno")$res
    if(yes_no == "no"){
      date_death <- svDialogs::dlg_input(message= "Enter the name of the death date column in died data","date_death")$res
    }
  } else if (length(date_death) > 1){
    date_death <- tcltk::tk_select.list(date_death, title = "Death date Column [Died Data]")
  } else if (length(date_death) == 0) {
    date_death <- svDialogs::dlg_input(message= "Enter the name of the death date column in died data","date_death")$res
  }

  ## Detect death_cause column
  death_cause <- names(raw.died_member)[grepl("cause",names(raw.died_member))]

  if(length(death_cause) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", death_cause, "' the correct cause of death column?"), type = "yesno")$res
    if(yes_no == "no"){
      death_cause <- svDialogs::dlg_input(message= "Enter the name of the cause of death column","death_cause")$res
    }
  } else if (length(death_cause) > 1){
    death_cause <- tcltk::tk_select.list(death_cause, title = "Cause of Death Column [Died Data]")
  } else if (length(death_cause) == 0) {
    death_cause <- svDialogs::dlg_input(message= "Enter the name of the cause of death column","death_cause")$res
  }

  ## Detect death_location column
  death_location <- names(raw.died_member)[grepl("location",names(raw.died_member))]

  if(length(death_location) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", death_location, "' the correct location of death column?"), type = "yesno")$res
    if(yes_no == "no"){
      death_location <- svDialogs::dlg_input(message= "Enter the name of the location of death column","death_location")$res
    }
  } else if (length(death_location) > 1){
    death_location <- tcltk::tk_select.list(death_location, title = "Location of Death Column [Died Data]")
  } else if (length(death_location) == 0) {
    death_location <- svDialogs::dlg_input(message= "Enter the name of the location of death column","death_location")$res
  }
}

dates_collected <- tcltk::tk_select.list(c("All","Some","None"), title = "Do you have all the Dates Collected?[Left/Join/Birth/Death]")

if(dates_collected == "All") {
  smart <- T
  collect_num_join_left <- "no"
  num_join <- NULL
  num_left <- NULL
} else if (dates_collected == "None") {
  smart <- F
  collect_num_join_left <- "no"
  num_join <- NULL
  num_left <- NULL
} else if (dates_collected == "Some") {
  smart <- T
  collect_num_join_left <- svDialogs::dlg_message(paste0("Do you still collect num_join and num_left on HH level?"), type = "yesno")$res
  if(collect_num_join_left == "yes") {
    ## Detect num_left column
    num_left <- names(raw.main)[grepl("leaver|num_left|left",names(raw.main))]

    if(length(num_left) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", num_left, "' the correct number of Leavers column in main data?"), type = "yesno")$res
      if(yes_no == "no"){
        num_left <- svDialogs::dlg_input(message= "Enter the number of Leavers column in main data","num_left")$res
      }
    } else if (length(num_left) > 1){
      num_left <- tcltk::tk_select.list(num_left, title = "Number of Leavers Column [Main Data]")
    } else if (length(num_left) == 0) {
      num_left <- svDialogs::dlg_input(message= "Enter the number of Leavers column in main data","num_left")$res
    }
    ## Detect num_join column
    num_join <- names(raw.main)[grepl("joiner|num_join|join",names(raw.main))]
    if(length(num_join) == 1){
      yes_no <- svDialogs::dlg_message(paste0("Is '", num_join, "' the correct number of Joiners column in main data?"), type = "yesno")$res
      if(yes_no == "no"){
        num_join <- svDialogs::dlg_input(message= "Enter the number of Joiners column in main data","num_join")$res
      }
    } else if (length(num_join) > 1){
      num_join <- tcltk::tk_select.list(num_join, title = "Number of Joiners Column [Main Data]")
    } else if (length(num_join) == 0) {
      num_join <- svDialogs::dlg_input(message= "Enter the number of Joiners column in main data","num_join")$res
    }
  } else {
    collect_num_join_left <- "no"
    num_join <- NULL
    num_left <- NULL

  }
} else {
  collect_num_join_left <- "no"
  num_join <- NULL
  num_left <- NULL
}

all_vars <- ls()
is_empty <- function(x) {
  obj <- get(x)
  length(obj) == 0 || is.null(obj) ||  (is.character(obj) && all(obj == ""))
}
empty_vars <- all_vars[sapply(all_vars, is_empty)]

for (i in empty_vars) {
  assign(i, NULL)
}
if(!is.null(date_dc)){
  if(!purrr::is_empty(date_dc)){
    if(date_dc %in% names(raw.main)){
      raw.main <- raw.main %>%
        dplyr::mutate(!!rlang::sym(date_dc) := ifelse(is.na(!!rlang::sym(date_dc)), NA,
                                                      ifelse(nchar(!!rlang::sym(date_dc)) == 5,
                                                             lubridate::as_date(as.numeric(!!rlang::sym(date_dc)), origin = "1899-12-30"),
                                                             stringr::str_sub(string = !!rlang::sym(date_dc), start = 1, end = 10))))
    }
  }
}

if(!is.null(date_recall_event)){
  if(!purrr::is_empty(date_recall_event)){
    if(date_recall_event %in% names(raw.main)){
      raw.main <- raw.main %>%
        dplyr::mutate(!!rlang::sym(date_recall_event) := ifelse(is.na(!!rlang::sym(date_recall_event)), NA,
                                                                ifelse(nchar(!!rlang::sym(date_recall_event)) == 5,
                                                                       lubridate::as_date(as.numeric(!!rlang::sym(date_recall_event)), origin = "1899-12-30"),
                                                                       stringr::str_sub(string = !!rlang::sym(date_recall_event), start = 1, end = 10))))
    }
  }
}
if(!is.null(birthdate_roster)){
  if(!purrr::is_empty(birthdate_roster)){
    if(birthdate_roster %in% names(raw.hh_roster)){
      raw.hh_roster <- raw.hh_roster %>%
        dplyr::mutate(age_years = NULL,
                      !!rlang::sym(birthdate_roster) := ifelse(is.na(!!rlang::sym(birthdate_roster)), NA,
                                                               ifelse(nchar(!!rlang::sym(birthdate_roster)) == 5,
                                                                      lubridate::as_date(as.numeric(!!rlang::sym(birthdate_roster)), origin = "1899-12-30"),
                                                                      stringr::str_sub(string = !!rlang::sym(birthdate_roster), start = 1, end = 10))))
    }
  }
}
if(!is.null(joined_date_roster)){
  if(!purrr::is_empty(joined_date_roster)){
    if(joined_date_roster %in% names(raw.hh_roster)){
      raw.hh_roster <- raw.hh_roster %>%
        dplyr::mutate(age_years = NULL,
                      !!rlang::sym(joined_date_roster) := ifelse(is.na(!!rlang::sym(joined_date_roster)), NA,
                                                                 ifelse(nchar(!!rlang::sym(joined_date_roster)) == 5,
                                                                        lubridate::as_date(as.numeric(!!rlang::sym(joined_date_roster)), origin = "1899-12-30"),
                                                                        stringr::str_sub(string = !!rlang::sym(joined_date_roster), start = 1, end = 10))))
    }
  }
}
if(collected_df_left){
  if(!is.null(birthdate_left)){
    if(!purrr::is_empty(birthdate_left)){
      if(birthdate_left %in% names(raw.left_member)){
        raw.left_member <- raw.left_member %>%
          dplyr::mutate(!!rlang::sym(birthdate_left) := ifelse(is.na(!!rlang::sym(birthdate_left)), NA,
                                                               ifelse(nchar(!!rlang::sym(birthdate_left)) == 5,
                                                                      lubridate::as_date(as.numeric(!!rlang::sym(birthdate_left)), origin = "1899-12-30"),
                                                                      stringr::str_sub(string = !!rlang::sym(birthdate_left), start = 1, end = 10))))
      }
    }
  }

  if(!is.null(joined_date_left)){
    if(!purrr::is_empty(joined_date_left)){
      if(joined_date_left %in% names(raw.left_member)){
        raw.left_member <- raw.left_member %>%
          dplyr::mutate(!!rlang::sym(joined_date_left) := ifelse(is.na(!!rlang::sym(joined_date_left)), NA,
                                                                 ifelse(nchar(!!rlang::sym(joined_date_left)) == 5,
                                                                        lubridate::as_date(as.numeric(!!rlang::sym(joined_date_left)), origin = "1899-12-30"),
                                                                        stringr::str_sub(string = !!rlang::sym(joined_date_left), start = 1, end = 10))))
      }
    }
  }

  if(!is.null(left_date_left)){
    if(!purrr::is_empty(left_date_left)){
      if(left_date_left %in% names(raw.left_member)){
        raw.left_member <- raw.left_member %>%
          dplyr::mutate(!!rlang::sym(left_date_left) := ifelse(is.na(!!rlang::sym(left_date_left)), NA,
                                                               ifelse(nchar(!!rlang::sym(left_date_left)) == 5,
                                                                      lubridate::as_date(as.numeric(!!rlang::sym(left_date_left)), origin = "1899-12-30"),
                                                                      stringr::str_sub(string = !!rlang::sym(left_date_left), start = 1, end = 10))))
      }
    }
  }
}
if(!is.null(joined_date_died)){
  if(!purrr::is_empty(joined_date_died)){
    if(joined_date_died %in% names(raw.died_member)){
      raw.died_member <- raw.died_member %>%
        dplyr::mutate(!!rlang::sym(joined_date_died) := ifelse(is.na(!!rlang::sym(joined_date_died)), NA,
                                                               ifelse(nchar(!!rlang::sym(joined_date_died)) == 5,
                                                                      lubridate::as_date(as.numeric(!!rlang::sym(joined_date_died)), origin = "1899-12-30"),
                                                                      stringr::str_sub(string = !!rlang::sym(joined_date_died), start = 1, end = 10))))
    }
  }
}


if(!is.null(birthdate_died)){
  if(!purrr::is_empty(birthdate_died)){
    if(birthdate_died %in% names(raw.died_member)){
      raw.died_member <- raw.died_member %>%
        dplyr::mutate(!!rlang::sym(birthdate_died) := ifelse(is.na(!!rlang::sym(birthdate_died)), NA,
                                                             ifelse(nchar(!!rlang::sym(birthdate_died)) == 5,
                                                                    lubridate::as_date(as.numeric(!!rlang::sym(birthdate_died)), origin = "1899-12-30"),
                                                                    stringr::str_sub(string = !!rlang::sym(birthdate_died), start = 1, end = 10))))
    }
  }
}

if(!is.null(date_death)){
  if(!purrr::is_empty(date_death)){
    if(date_death %in% names(raw.died_member)){
      raw.died_member <- raw.died_member %>%
        dplyr::mutate(!!rlang::sym(date_death) := ifelse(is.na(!!rlang::sym(date_death)), NA,
                                                         ifelse(nchar(!!rlang::sym(date_death)) == 5,
                                                                lubridate::as_date(as.numeric(!!rlang::sym(date_death)), origin = "1899-12-30"),
                                                                stringr::str_sub(string = !!rlang::sym(date_death), start = 1, end = 10))))
    }
  }
}


if(collected_df_left){
  raw.left_member <- raw.left_member
} else {
  raw.left_member <- NULL
}

df_mortality_long <- impactR4PHU::create_mortality_long_df(df_main = raw.main,
                                                           date_dc = date_dc,
                                                           date_recall_event = date_recall_event,
                                                           enumerator = enumerator,
                                                           admin1 = admin1,
                                                           admin2 = admin2,
                                                           cluster = cluster,
                                                           uuid_main = uuid_main,
                                                           df_roster = raw.hh_roster,
                                                           uuid_roster = uuid_roster,
                                                           sex_roster = sex_roster,
                                                           age_roster = age_roster,
                                                           joined_roster = joined_roster,
                                                           joined_date_roster = joined_date_roster,
                                                           birth_roster = birth_roster,
                                                           birthdate_roster = birthdate_roster,
                                                           collected_df_left = collected_df_left,
                                                           df_left = raw.left_member,
                                                           uuid_left = uuid_left,
                                                           sex_left = sex_left,
                                                           age_left = age_left,
                                                           birth_left = birth_left,
                                                           birthdate_left = birthdate_left,
                                                           joined_left = joined_left,
                                                           joined_date_left = joined_date_left,
                                                           left_date_left = left_date_left,
                                                           df_died = raw.died_member,
                                                           uuid_died = uuid_died,
                                                           sex_died = sex_died,
                                                           age_died = age_died,
                                                           birth_died = birth_died,
                                                           birthdate_died = birthdate_died,
                                                           joined_died = joined_died,
                                                           joined_date_died = joined_date_died,
                                                           date_death = date_death,
                                                           death_cause = death_cause,
                                                           death_location = death_location)



if(!file.exists("inputs/environment.Rdata")) {
  if("sex" %in% names(df_mortality_long)){
    sex_codes <- unique(df_mortality_long$sex)
    ideal_codes <- c("1", "2")
    sex_recodes <- c("1", "2", "NA")

    if(length(setdiff(sex_codes, ideal_codes))==0) {
      male <- 1
      female <- 2
      print("Good - Sex coded as 1/2 for male/female")
    } else {
      male <- tcltk::tk_select.list(sex_codes, title = "Male Option", multiple = T)
      female <- tcltk::tk_select.list(sex_codes, title = "Female Option", multiple = T)
    }
  }

  if(c("date_dc") %in% names(df_mortality_long)) {

    date_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df_mortality_long %>% dplyr::filter(!is.na(date_dc)) %>% dplyr::select(date_dc) %>% t %>% c %>% unique
    date_dc_reformat <- svDialogs::dlg_input(message= paste0("Example of Date of Data collection values: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                             "\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n What is the date format for the DATE OF DATA COLLECTION column? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    while(length(setdiff(date_dc_reformat, date_recodes))==1) {
      date_dc_reformat <- svDialogs::dlg_input(message= paste0("Example of Date of Data collection values: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                               "Invalid input. \n ", "\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n How is DATE OF DATA COLLECTION formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    }
  }


  if(c("date_recall") %in% names(df_mortality_long)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df_mortality_long %>% dplyr::filter(!is.na(date_recall)) %>% dplyr::select(date_recall) %>% t %>% c %>% unique



    date_recall_reformat <- svDialogs::dlg_input(message= paste0("Example of RECALL DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                                 "\n RE-FORMATTING VARIABLE : RECALL DATE \n What is the date format for the RECALL DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    while(length(setdiff(date_recall_reformat, dob_recodes))==1) {
      date_recall_reformat <- svDialogs::dlg_input(message= paste0("Example of RECALL DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                                   "Invalid input. \n", "\n RE-FORMATTING VARIABLE : RECALL DATE \n How is RECALL DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    }
  }

  # Checking Date Join

  if(c("date_join") %in% names(df_mortality_long)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df_mortality_long %>% dplyr::filter(!is.na(date_join)) %>% dplyr::select(date_join) %>% t %>% c %>% unique

    date_join_reformat <- svDialogs::dlg_input(message= paste0("Example of JOIN DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                               "\n RE-FORMATTING VARIABLE : JOIN DATE \n What is the date format for the JOIN DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA." ))$res
    while(length(setdiff(date_join_reformat, dob_recodes))==1) {
      date_join_reformat <- svDialogs::dlg_input(message= paste0("Example of JOIN DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                                 "Invalid input. ", "\n RE-FORMATTING VARIABLE : JOIN DATE \n How is JOIN DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    }
  }

  # Checking Date Left

  if(c("date_left") %in% names(df_mortality_long)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df_mortality_long %>% dplyr::filter(!is.na(date_left)) %>% dplyr::select(date_left) %>% t %>% c %>% unique

    date_left_reformat <- svDialogs::dlg_input(message= paste0("Example of LEFT DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                               "\n RE-FORMATTING VARIABLE : LEFT DATE \n What is the date format for the LEFT DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    while(length(setdiff(date_left_reformat, dob_recodes))==1) {
      date_left_reformat <- svDialogs::dlg_input(message= paste0("Example of LEFT DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                                 "Invalid input. ", "\n RE-FORMATTING VARIABLE : LEFT DATE \n How is LEFT DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    }
  }


  # Checking Date Birth

  if(c("date_birth") %in% names(df_mortality_long)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df_mortality_long %>% dplyr::filter(!is.na(date_birth)) %>% dplyr::select(date_birth) %>% t %>% c %>% unique

    date_birth_reformat <- svDialogs::dlg_input(message= paste0("Example of BIRTH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                                "\n RE-FORMATTING VARIABLE : BIRTH DATE \n What is the date format for the BIRTH DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    while(length(setdiff(date_birth_reformat, dob_recodes))==1) {
      date_birth_reformat <- svDialogs::dlg_input(message= paste0("Example of BIRTH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                                  "Invalid input. ", "\n RE-FORMATTING VARIABLE : BIRTH DATE \n How is BIRTH DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    }
  }

  # Checking Date Death

  if(c("date_death") %in% names(df_mortality_long)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df_mortality_long %>% dplyr::filter(!is.na(date_death)) %>% dplyr::select(date_death) %>% t %>% c %>% unique

    date_death_reformat <- svDialogs::dlg_input(message= paste0("Example of DEATH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                                "\n RE-FORMATTING VARIABLE : DEATH DATE \n What is the date format for the DEATH DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    while(length(setdiff(date_death_reformat, dob_recodes))==1) {
      date_death_reformat <- svDialogs::dlg_input(message= paste0("Example of DEATH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                                  "Invalid input. ", "\n RE-FORMATTING VARIABLE : DEATH DATE \n How is DEATH DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month \n 'NA' for NA."))$res
    }
  }

  # Checking Cause of Death Coding

  cause_codes <- unique(df_mortality_long$death_cause)
  ideal_codes <- c("1", "2", "3")

  if(length(setdiff(cause_codes, ideal_codes))==0) {
    print("Good - Cause of Death coded as 1/2/3 for unknown/injury/illness")
  } else {
    unknown <- tcltk::tk_select.list(cause_codes, title = "Unknown Options [Cause of Death]", multiple = T)
    injury_trauma <- tcltk::tk_select.list(cause_codes, title = "Injury/trauma Options [Cause of Death]", multiple = T)
    illness <- tcltk::tk_select.list(cause_codes, title = "Illness Options [Cause of Death]", multiple = T)
  }

  # Checking Location of Death Coding
  location_codes <- unique(df_mortality_long$death_location)
  ideal_codes <- c("1", "2", "3", "4")

  if(length(setdiff(location_codes, ideal_codes))==0) {
    print("Good - Cause of Death coded as 1/2/3/4 for unknown/injury/illness")
  } else {
    current_location <- tcltk::tk_select.list(location_codes, title = "Current Location Options [LOCATION OF DEATH]", multiple = T)
    during_migration <- tcltk::tk_select.list(location_codes, title = "During Migration Options  [LOCATION OF DEATH]", multiple = T)
    last_place <- tcltk::tk_select.list(location_codes, title = "Last Place Residence Options [LOCATION OF DEATH]", multiple = T)
    other_place <- tcltk::tk_select.list(location_codes, title = "Other Place Options [LOCATION OF DEATH]", multiple = T)
  }

  demographic_vars <- c("join", "left", "birth", "death")

  list_to_check <- intersect(demographic_vars, names(df_mortality_long))
  df_mortality_long[list_to_check] <- lapply(df_mortality_long[list_to_check], as.character)
  list_to_check_codes <- df_mortality_long %>% dplyr::select(list_to_check) %>% t %>% c %>% unique

  ideal_codes <- c("1")

  if(length(setdiff(list_to_check_codes, ideal_codes))==0) {
    print("Good - The demographic variables are already coded as 1/0 for yes/no")
  } else {
    yes_demo_values <- tcltk::tk_select.list(list_to_check_codes, title = "Yes values [Demographic Column]", multiple = T)
    no_demo_values <- tcltk::tk_select.list(list_to_check_codes, title = "No/NA values [Demographic Column]", multiple = T)
  }

}

if(is.character(df_mortality_long$date_dc)) {
  df_mortality_long <- df_mortality_long %>%
    dplyr::mutate(date_dc = ifelse(date_dc == "", NA, date_dc))
}

if(is.character(df_mortality_long$date_recall)) {
  df_mortality_long <- df_mortality_long %>%
    dplyr::mutate(date_recall = ifelse(date_recall == "", NA, date_recall))
}

if(is.character(df_mortality_long$date_join)) {
  df_mortality_long <- df_mortality_long %>%
    dplyr::mutate(date_join = ifelse(date_join == "", NA, date_join))
}


if(is.character(df_mortality_long$date_left)) {
  df_mortality_long <- df_mortality_long %>%
    dplyr::mutate(date_left = ifelse(date_left == "", NA, date_left))
}


if(is.character(df_mortality_long$date_birth)) {
  df_mortality_long <- df_mortality_long %>%
    dplyr::mutate(date_birth = ifelse(date_birth == "", NA, date_birth))
}


if(is.character(df_mortality_long$date_death)) {
  df_mortality_long <- df_mortality_long %>%
    dplyr::mutate(date_death = ifelse(date_death == "", NA, date_death))
}

df_mortality_long <- df_mortality_long  %>%
  dplyr::mutate(death_cause_smart = "",
                death_location_smart = "",
                sex = ifelse(is.na(sex),NA,
                             ifelse(sex == male, 1,
                                    ifelse(sex == female,2,sex))),
                date_dc_date = lubridate::parse_date_time(date_dc, orders = date_dc_reformat),
                date_dc_month = lubridate::month(date_dc_date),
                date_dc_day = lubridate::day(date_dc_date),
                date_dc_year = lubridate::year(date_dc_date),
                date_dc_char = paste(date_dc_month, date_dc_day, date_dc_year, sep = "/"),
                date_dc_char = ifelse(is.na(date_dc_char), NA, ifelse(date_dc_char == "NA/NA/NA", NA, date_dc_char)),
                date_recall_date = lubridate::parse_date_time(date_recall, orders = date_recall_reformat),
                date_recall_month = lubridate::month(date_recall_date),
                date_recall_day = lubridate::day(date_recall_date),
                date_recall_year = lubridate::year(date_recall_date),
                date_recall = paste(date_recall_month, date_recall_day, date_recall_year, sep = "/"),
                date_recall = ifelse(is.na(date_recall), NA, ifelse(date_recall == "NA/NA/NA", NA, date_recall)),
                date_join_date = lubridate::parse_date_time(date_join, orders = date_join_reformat),
                date_join_month = lubridate::month(date_join_date),
                date_join_day = lubridate::day(date_join_date),
                date_join_year = lubridate::year(date_join_date),
                date_join = paste(date_join_month, date_join_day, date_join_year, sep = "/"),
                date_join = ifelse(is.na(date_join), NA, ifelse(date_join == "NA/NA/NA", NA, date_join)),
                date_left_date = lubridate::parse_date_time(date_left, orders = date_left_reformat),
                date_left_month = lubridate::month(date_left_date),
                date_left_day = lubridate::day(date_left_date),
                date_left_year = lubridate::year(date_left_date),
                date_left = paste(date_left_month, date_left_day, date_left_year, sep = "/"),
                date_left = ifelse(is.na(date_left), NA, ifelse(date_left == "NA/NA/NA", NA, date_left)),
                date_birth_date = lubridate::parse_date_time(date_birth, orders = date_birth_reformat),
                date_birth_month = lubridate::month(date_birth_date),
                date_birth_day = lubridate::day(date_birth_date),
                date_birth_year = lubridate::year(date_birth_date),
                date_birth = paste(date_birth_month, date_birth_day, date_birth_year, sep = "/"),
                date_birth = ifelse(is.na(date_birth), NA, ifelse(date_birth == "NA/NA/NA", NA, date_birth)),
                date_death_date = lubridate::parse_date_time(date_death, orders = date_death_reformat),
                date_death_month = lubridate::month(date_death_date),
                date_death_day = lubridate::day(date_death_date),
                date_death_year = lubridate::year(date_death_date),
                date_death = paste(date_death_month, date_death_day, date_death_year, sep = "/"),
                date_death = ifelse(is.na(date_death), NA, ifelse(date_death == "NA/NA/NA", NA, date_death)),
                death_cause_smart = ifelse(is.na(death_cause),NA,
                                           ifelse(death_cause %in% unknown, 1,
                                                  ifelse(death_cause %in% injury_trauma, 2,
                                                         ifelse(death_cause %in% illness, 3, death_cause_smart)))),
                death_location_smart = ifelse(is.na(death_location), NA,
                                              ifelse(death_location %in% current_location, 1,
                                                     ifelse(death_location %in% during_migration, 2,
                                                            ifelse(death_location %in% last_place, 3,
                                                                   ifelse(death_location %in% other_place,4,death_location_smart)))))) %>%
  dplyr::mutate_at(vars(c("join", "left", "birth", "death")), ~ifelse(is.na(.),NA,
                                                                      ifelse(. %in% no_demo_values, NA,
                                                                             ifelse(. %in% yes_demo_values, 1, .))))


if(!file.exists("inputs/environment.Rdata")) {
  cause_death_f <- tcltk::tk_select.list(dplyr::pull(df_mortality_long[,"death_cause"]) %>% unique, title = "Cause Death related to Women",multiple = T)
}



if(!file.exists("inputs/environment.Rdata")) {
  # exp_sex_ratio
  yes_no <- svDialogs::dlg_message(paste0("Apply a 1:1 male to female ratio. No to provide your ratios."), type = "yesno")$res
  if(yes_no == "no"){
    right <- as.numeric(svDialogs::dlg_input(message= "Enter the male ratio",1)$res)
    left <- as.numeric(svDialogs::dlg_input(message= "Enter the female ratio",1)$res)
    exp_sex_ratio <- c(right,left)
  } else {
    exp_sex_ratio <- NULL
  }
  # exp_ratio_0_4
  yes_no <- svDialogs::dlg_message(paste0("Apply a 1:4 <5 years to >5 years ratio ratio. No to provide your ratios."), type = "yesno")$res
  if(yes_no == "no"){
    right <- as.numeric(svDialogs::dlg_input(message= "Enter the <5 years ratio",1)$res)
    left <- as.numeric(svDialogs::dlg_input(message= "Enter the >5 years ratio",4)$res)
    exp_ratio_0_4 <- c(right,left)
  } else {
    exp_ratio_0_4 <- NULL
  }
  # exp_ratio_2_5
  yes_no <- svDialogs::dlg_message(paste0("Apply a ~41% of individuals are <2 years of age of all children <5 years. No to provide your ratios."), type = "yesno")$res
  if(yes_no == "no"){
    right <- as.numeric(svDialogs::dlg_input(message= "Enter the <2 years ratio",7)$res)
    left <- as.numeric(svDialogs::dlg_input(message= "Enter the <5 years ratio",10)$res)
    exp_ratio_2_5 <- c(right,left)
  } else {
    exp_ratio_2_5 <- NULL
  }
  # exp_ratio_5_10
  yes_no <- svDialogs::dlg_message(paste0("Apply a ~52% of individuals are <5 years of age out of under-10 children. No to provide your ratios."), type = "yesno")$res
  if(yes_no == "no"){
    right <- as.numeric(svDialogs::dlg_input(message= "Enter the <2 years ratio",11)$res)
    left <- as.numeric(svDialogs::dlg_input(message= "Enter the <5 years ratio",10)$res)
    exp_ratio_5_10 <- c(right,left)
  } else {
    exp_ratio_5_10 <- NULL
  }
  # exp_hh_size
  yes_no <- svDialogs::dlg_message(paste0("Apply a 5 for HH average size. No to provide your average size."), type = "yesno")$res
  if(yes_no == "no"){
    exp_hh_size <- as.numeric(svDialogs::dlg_input(message= "Enter the HH size",5)$res)
  } else {
    exp_hh_size <- NULL
  }

}

df_mortality_long <- impactR4PHU::add_persontime(df_mortality_long,
                                                 smart = smart)

if(collect_num_join_left == "yes") {
  results_mort <- impactR4PHU::create_mortality_plaus(df_mortality = df_mortality_long,
                                                      df_main = raw.main,
                                                      uuid_main = uuid_main,
                                                      date_dc = date_dc,
                                                      date_recall_event = date_recall_event,
                                                      num_join = num_join,
                                                      num_left = num_left,
                                                      enumerator = enumerator,
                                                      exp_sex_ratio = exp_sex_ratio,
                                                      exp_ratio_0_4 = exp_ratio_0_4,
                                                      exp_ratio_2_5 = exp_ratio_2_5,
                                                      exp_ratio_5_10 = exp_ratio_5_10,
                                                      exp_hh_size = exp_hh_size)

  results_mort_enum <- impactR4PHU::create_mortality_plaus(df_mortality = df_mortality_long,
                                                           df_main = raw.main,
                                                           uuid_main = uuid_main,
                                                           date_dc = date_dc,
                                                           date_recall_event = date_recall_event,
                                                           num_join = num_join,
                                                           num_left = num_left,
                                                           enumerator = enumerator,
                                                           exp_sex_ratio = exp_sex_ratio,
                                                           exp_ratio_0_4 = exp_ratio_0_4,
                                                           exp_ratio_2_5 = exp_ratio_2_5,
                                                           exp_ratio_5_10 = exp_ratio_5_10,
                                                           exp_hh_size = exp_hh_size,
                                                           grouping = "enumerator")
} else {
  results_mort <- impactR4PHU::create_mortality_plaus(df_mortality = df_mortality_long,
                                                      exp_sex_ratio = exp_sex_ratio,
                                                      exp_ratio_0_4 = exp_ratio_0_4,
                                                      exp_ratio_2_5 = exp_ratio_2_5,
                                                      exp_ratio_5_10 = exp_ratio_5_10,
                                                      exp_hh_size = exp_hh_size)

  results_mort_enum <- impactR4PHU::create_mortality_plaus(df_mortality = df_mortality_long,
                                                           exp_sex_ratio = exp_sex_ratio,
                                                           exp_ratio_0_4 = exp_ratio_0_4,
                                                           exp_ratio_2_5 = exp_ratio_2_5,
                                                           exp_ratio_5_10 = exp_ratio_5_10,
                                                           exp_hh_size = exp_hh_size,
                                                           grouping = "enumerator")
}

result_mort_pivot <- results_mort %>%
  dplyr::mutate_all(., as.character) %>%
  tidyr::pivot_longer(cols = everything(),names_to = "Criteria",
                      values_to = "Score") %>%
  filter(stringr::str_starts(Criteria,"plaus_"))


yes_no_weight <- svDialogs::dlg_message("Is your data weighted?", type = "yesno")$res
if(yes_no_weight == "yes"){
  weight <- names(data.list$main)[grepl("weight",names(data.list$main))]
  if(length(weight) == 1){
    yes_no <- svDialogs::dlg_message(paste0("Is '", weight, "' the correct weight column?"), type = "yesno")$res
    if (yes_no == "no") {
      weight <- svDialogs::dlg_input(message= "Enter the name of the fsl_hdds_condiments","weight")$res
    }
  } else if (length(weight) > 1){
    weight <- tcltk::tk_select.list(weight, title = "Weight column")
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

if(collected_df_left){
  list_of_var <- c("date_dc","date_recall_event","enumerator","admin1","admin2","cluster",
                   "uuid_main","uuid_roster","sex_roster","age_roster","joined_roster","joined_date_roster",
                   "birth_roster","birthdate_roster","uuid_left","sex_left","age_left","birth_left",
                   "birthdate_left","joined_left","joined_date_left","left_date_left","uuid_died",
                   "sex_died","age_died","birth_died","birthdate_died","joined_died","weight","yes_no_weight",
                   "joined_date_died","date_death","death_cause","death_location","cause_death_f",
                   "exp_sex_ratio","exp_ratio_0_4","exp_ratio_2_5","exp_ratio_5_10","exp_hh_size",
                   "label_colname","collect_num_join_left","num_join","num_left", "dates_collected",
                   "male","female","date_dc_reformat","date_recall_reformat","date_join_reformat",
                   "date_birth_reformat","date_left_reformat","date_death_reformat","yes_no_team","team",
                   "unknown", "injury_trauma", "illness", "no_demo_values", "yes_demo_values",
                   "current_location","during_migration","last_place","other_place")
} else {
  list_of_var <- c("date_dc","date_recall_event","enumerator","admin1","admin2","cluster",
                   "uuid_main","uuid_roster","sex_roster","age_roster","joined_roster","joined_date_roster",
                   "birth_roster","birthdate_roster","uuid_died","weight","yes_no_weight",
                   "sex_died","age_died","birth_died","birthdate_died","joined_died",
                   "joined_date_died","date_death","death_cause","death_location","cause_death_f",
                   "exp_sex_ratio","exp_ratio_0_4","exp_ratio_2_5","exp_ratio_5_10","exp_hh_size",
                   "label_colname","collect_num_join_left","num_join","num_left", "dates_collected",
                   "male","female","date_dc_reformat","date_recall_reformat","date_join_reformat",
                   "date_birth_reformat","date_left_reformat","date_death_reformat","yes_no_team","team",
                   "unknown", "injury_trauma", "illness", "no_demo_values", "yes_demo_values",
                   "current_location","during_migration","last_place","other_place")
}
if(!file.exists("inputs/environment.Rdata")){
  save(list = list_of_var, file = "inputs/environment.Rdata")
}
