
# ------------------------------------------------------------------------------------------
# CLEANING LOG FUNCTIONS
# ------------------------------------------------------------------------------------------

add.to.cleaning.log.other.recode.LOOP <- function(data, x){
  if(!"existing.other" %in% colnames(x)){
    x <- rename_with(x, ~gsub(".v",".other", .), ends_with(".v"))
  } # a bit of a dirty fix :)
  if (x$ref.type[1]=="select_one") res <- add.to.cleaning.log.other.recode.one.LOOP(x)
  if (x$ref.type[1]=="select_multiple") res <- add.to.cleaning.log.other.recode.multiple.LOOP(data, x)
  if (res == "err") cat("Errors encountered while recoding other. Check the warnings!")
}


add.to.cleaning.log.other.remove.LOOP <- function(data, x){
  issue <- "Invalid other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$name, issue=issue,
                   old.value=x$response.fr, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # remove relative entries
  if (x$ref.type[1]=="select_one"){
    old.value <- "other"
    df <- data.frame(uuid=x$uuid,loop_index=x$loop_index, variable=x$ref.name, issue=issue, old.value=old.value, new.value=NA)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
  if (x$ref.type[1]=="select_multiple"){
    if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_hh_roster")){
      old.value <- as.character(data[data$loop_index==x$loop_index[1], x$ref.name])
    } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_ind_health")) {
      old.value <- as.character(data[data$loop_index==x$loop_index[1], x$ref.name])
    } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_child_nutrition")) {
      old.value <- as.character(data[data$loop_index==x$loop_index[1], x$ref.name])
    } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_died_member")) {
      old.value <- as.character(data[data$loop_index==x$loop_index[1], x$ref.name])
    } else {
      old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name])
    }
    l <- str_split(old.value, " ")[[1]]
    new.value <- paste(l[l!="other"], collapse=" ")
    new.value <- ifelse(new.value=="", NA, new.value)
    df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$ref.name, issue=issue, old.value=old.value, new.value=new.value)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    if (is.na(new.value)){
      # set all choices columns to NA
      cols <- colnames(data)[str_starts(colnames(data), paste0(x$ref.name, "/"))]
      df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=cols, issue=issue, old.value="0 or 1", new.value=NA)
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    } else{
      df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=paste0(x$ref.name, "/other"), issue=issue,
                       old.value="1", new.value="0")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
  }
}

add.to.cleaning.log.other.recode.multiple.LOOP <- function(data, x){
  issue <- "Recoding other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$name, issue=issue,
                   old.value=x$response.fr, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices from other response
  if (str_detect(x$existing.other, ";")) {
    choices <- str_trim(str_split(x$existing.other, ";")[[1]])
  } else {
    choices <- str_trim(str_split(x$existing.other, "\r\n")[[1]])
  }
  choices <- choices[choices!=""]
  # set variable/other to "0"
  df <- data.frame(uuid=x$uuid, loop_index=x$loop_index,  variable=paste0(x$ref.name, "/other"), issue=issue,
                   old.value="1", new.value="0")
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices already selected

  if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_hh_roster")){
    old.value <- as.character(data[data$loop_index==x$loop_index[1], x$ref.name])
  } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_ind_health")) {
    old.value <- as.character(data[data$loop_index==x$loop_index[1], x$ref.name])
  } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_child_nutrition")) {
    old.value <- as.character(data[data$loop_index==x$loop_index[1], x$ref.name])
  } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_died_member")) {
    old.value <- as.character(data[data$loop_index==x$loop_index[1], x$ref.name])
  } else {
    old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name])
  }
  l <- str_split(old.value, " ")[[1]]
  l.cumulative <- l[l!="other"]
  # add to the cleaning log each choice in the other response
  for (choice in choices){
    # set corresponding variable to "1" if not already "1"
    list.name <- filter(tool.survey, name==x$ref.name[1])$list_name
    new.code <- filter(tool.choices, list_name==list.name & !!sym(label_colname)==choice)
    if (nrow(new.code)!=1){
      warning(paste0("Choice is not in the list. UUID: ", x$uuid,"; recode.into: ", choice))
      return("err")
    }
    variable.name <- paste0(x$ref.name, "/", new.code$name)
    if (variable.name %in% colnames(data)){
      if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_hh_roster")){
        old.boolean <- data[[variable.name]][data$loop_index==x$loop_index[1]]
      } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_ind_health")){
        old.boolean <- data[[variable.name]][data$loop_index==x$loop_index[1]]
      } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_child_nutrition")){
        old.boolean <- data[[variable.name]][data$loop_index==x$loop_index[1]]
      } else if (!is.na(x$loop_index) & str_starts(x$loop_index[1],"loop_died_member")){
        old.boolean <- data[[variable.name]][data$loop_index==x$loop_index[1]]
      } else {
        old.boolean <- data[[variable.name]][data$uuid==x$uuid[1]]
      }
    } else stop("Column not found")
    if (!is.na(old.boolean) && old.boolean=="0"){
      df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=variable.name, issue=issue,
                       old.value=old.boolean, new.value="1")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
    l.cumulative <- unique(c(l.cumulative, new.code$name))
  }
  # update cumulative variable
  new.value <- paste(sort(l.cumulative), collapse=" ")
  df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$ref.name, issue=issue,
                   old.value=old.value, new.value=new.value)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  return("succ")
}

add.to.cleaning.log.other.recode.one.LOOP <- function(x){
  issue <- "Recoding other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid,loop_index=x$loop_index, variable=x$name, issue=issue,
                   old.value=x$response.fr, new.value=NA)

  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # # get list of choices from other response
  # if (str_detect(x$existing.other, ";")) {
  #   choices <- str_trim(str_split(x$existing.other, ";")[[1]])
  # } else {
  #   choices <- str_trim(str_split(x$existing.other, "\r\n")[[1]])
  # }
  choices <- x$existing.other
  choices <- choices[choices!=""]
  if (length(choices)>1) {
    print(select(x, uuid, name))
    stop("More than one existing.option for a select_one question")
  }
  # recode choice
  choice <- choices[1]
  list.name <- filter(tool.survey, name==x$ref.name[1])$list_name
  new.code <- filter(tool.choices, list_name==list.name & !!sym(label_colname)==choice)
  if (nrow(new.code)!=1) {
    warning(paste0("Choice is not in the list. UUID: ", x$uuid,"; recode.into: ", choice))
    return("err")
  }
  else{
    df <- data.frame(uuid=x$uuid,loop_index=x$loop_index, variable=x$ref.name, issue=issue,
                     old.value="other", new.value=new.code$name)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    return("succ")
  }
}

add.to.cleaning.log.LOOP <- function(checks, check.id, question.names=c(), issue="", is.Loop = F){
  for(q.n in question.names){
    if(!is.Loop){
      new.entries <- checks %>% filter(flag) %>%
        mutate(uuid=uuid,
               loop_index = NA,
               variable = q.n,
               issue=issue,
               old.value =!!sym(q.n),
               new.value=NA,
               invalid =NA,
               explanation =NA)
    } else {
      new.entries <- checks %>% filter(flag) %>%
        mutate(uuid=uuid,
               loop_index=loop_index,
               variable = q.n,
               issue=issue,
               old.value =!!sym(q.n),
               new.value=NA,
               invalid =NA,
               explanation =NA)
    }
    new.entries[["check.id"]] <- check.id
    new.entries <- new.entries %>% select(uuid,loop_index, check.id,
                                          variable,issue, old.value, new.value,invalid, explanation)
    cleaning.log.checks <<- arrange(rbind(cleaning.log.checks, new.entries), uuid)
  }
}

# ------------------------------------------------------------------------------------------

add.to.cleaning.log.trans.remove.LOOP <- function(data, x){
  issue <- "Invalid text responses"
  # remove text of the response
  df <- data.frame(uuid=x$uuid,loop_index=x$loop_index, variable=x$name, issue=issue,
                   old.value=x$response.fr, new.value=NA)
  cleaning.log.trans <<- rbind(cleaning.log.trans, df)
}

# ------------------------------------------------------------------------------------------
save.follow.up.requests.LOOP <- function(cleaning.log, data){
  use.color <- function(check.id){
    return(str_starts(check.id, "0"))
    # |  str_starts(check.id, "3") | str_starts(check.id, "4"))
  }
  # define styles
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  # arrange cleaning.log so that colors are properly assigned later
  cleaning.log <- cleaning.log %>%
    group_modify(~ rbind(
      filter(.x, !use.color(check.id)) %>% arrange(check.id, uuid),
      filter(.x, use.color(check.id)) %>% arrange(check.id)))
  # add missing columns
  cl <- cleaning.log %>%
    left_join(select(data, uuid, `_submission_time`), by="uuid") %>%
    rename(submission_time="_submission_time") %>%
    select(uuid, loop_index, submission_time, check.id,
           variable, issue, old.value, new.value) %>%
    mutate(explanation=NA)
  c <- cl %>% arrange(match(check.id, str_sort(unique(cl$check.id), numeric=T)))
  # save follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)

  addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=9)
  addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=10)

  setColWidths(wb, "Follow-up", cols=1:ncol(cl), widths="auto")
  setColWidths(wb, "Follow-up", cols=7, widths=50)
  setColWidths(wb, "Follow-up", cols=8, widths=50)

  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=7)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=8)

  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl))

  col.id <- which(colnames(cl)=="old.value")
  if(nrow(cl) > 0){
    random.color <- ""
    for (r in 2:nrow(cl)){
      if((!use.color(as.character(cl[r, "check.id"])) &
          as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
          as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"])) |
         (use.color(as.character(cl[r, "check.id"])) &
          as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"]))){
        if (random.color == "") random.color <- randomColor(1, luminosity = "light")
        addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T),
                 rows = r:(r+1), cols=col.id)
      } else random.color=""
    }
  }
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=9)
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=10)
  filename <- paste0("output/checking/requests/follow_up_requests.xlsx")
  saveWorkbook(wb, filename, overwrite = TRUE)
  rm(cl)
}


recode.multiple.add.choice.LOOP <- function(data, variable, choice, issue, isLoop = F){
    #' [obsolete]
  choice_column <- paste0(variable,"/",choice)
  if(!choice_column %in% colnames(data)) stop(paste("Column",choice_column,"not present in data!"))
  # filter out cases that already have choice selected
  if(!isLoop){
    data <- data %>% filter(str_detect(!!sym(variable), choice, negate = T)) %>%
      mutate(loop_index = NA)
  }
  if(nrow(data) > 0){
    cl_cummulative <- select(data, uuid, loop_index, variable) %>%
      rename(old.value = !!sym(variable)) %>%
      mutate(variable = variable, new.value = paste(old.value, choice), issue = issue)

    cl_choice <- select(data, uuid, loop_index) %>%
      mutate(variable = choice_column, old.value = "0", new.value = "1", issue = issue)
    return(rbind(cl_cummulative, cl_choice))
  }
  return(data.frame())
}

recode.multiple.remove.choice.LOOP <- function(data, variable, choice, issue, isLoop = F){

  #' [obsolete]
  choice_column <- paste0(variable,"/",choice)
  if(!choice_column %in% colnames(data)) stop(paste("Column",choice_column,"not present in data!"))
  # filter out cases that dont have the choice selected
  if(!isLoop){
    data <- data %>% filter(str_detect(!!sym(variable), choice)) %>%
      mutate(loop_index = NA)
  }
  if(nrow(data) > 0){
    cl_cummulative <- select(data, uuid, loop_index, variable) %>%
      rename(old.value = !!sym(variable)) %>%
      mutate(variable = variable, new.value = str_squish(str_remove(old.value, choice)), issue = issue)

    cl_choice <- select(data, uuid, loop_index) %>%
      mutate(variable = choice_column, old.value = "1", new.value = "0", issue = issue)
    return(rbind(cl_cummulative, cl_choice))
  }
  return(data.frame())
}


apply.changes.LOOP <- function(data, clog, isLoop = F){
  #' Apply changes to main data basing on a cleaning log.
  #'
  #' Outputs warnings if uuids or variables from `clog` are not found in `data`
  #' @param data Data (raw.main)
  #' @param clog Cleaning log - dataframe containing columns uuid, variable, new.value, old.value
  #'
  #' @returns Dataframe containing data with applied changes
  #'

  # TODO: fix this function to not produce so many unnecessary warnings

  if(nrow(clog) == 0){
    warning("No changes to be applied (cleaning log empty).")
    return(data)
  }
  else{
    if(!isLoop && ("loop_index" %in% colnames(clog))){
      clog <- filter(clog, is.na(loop_index))
    }else(
      clog <- filter(clog, !is.na(loop_index))
    )
    missinguuids <- c()
    missingloop_indexs <- c()
    missingvars <- c()
    for (r in 1:nrow(clog)){
      uuid <- as.character(clog$uuid[r])
      loop_index <- as.character(clog$loop_index[r])
      if(!uuid %in% data$uuid) {
        missinguuids <- append(missinguuids, uuid)
        next
      }
      variable <- as.character(clog$variable[r])
      if(!variable %in% colnames(data)) {
        missingvars <- append(missingvars, variable)
        next
      }
      if(!is.na(loop_index) & !loop_index %in% data$loop_index){
        missingloop_indexs <- append(missingloop_indexs,loop_index)
      }
      if(!isLoop){
        if(data[data$uuid == uuid, variable] %!=na% clog$old.value[r]){
          warning(paste0("Value in data is different than old.value in Cleaning log!\nUUID: ", uuid,
                         "\tExpected: ", clog$old.value[r], "\t found: ", data[data$uuid == uuid, variable],
                         "\tReplacing with: ", clog$new.value[r]))
        }
        data[data$uuid == uuid, variable] <- as.character(clog$new.value[r])
      } else {
        if(data[data$loop_index == loop_index, variable] %!=na% clog$old.value[r]){
          warning(paste0("Value in data is different than old.value in Cleaning log!\nUUID: ", loop_index,
                         "\tExpected: ", clog$old.value[r], "\t found: ", data[data$loop_index == loop_index, variable],
                         "\tReplacing with: ", clog$new.value[r]))
        }
        data[data$loop_index == loop_index, variable] <- as.character(clog$new.value[r])
      }
    }
    if(length(missinguuids > 0) && !isLoop) warning(paste0("uuids from cleaning log not found in data:\n", paste0(missinguuids, collapse = "\n")))
    if(length(missingloop_indexs) > 0 && isLoop)  warning(paste0("loop_indexs from cleaning log not found in data:\n", paste0(missingloop_indexs, collapse = "\n")))
    if(length(missingvars > 0))  warning(paste0("variables from cleaning log not found in data:\n", paste0(missingvars, collapse = "\n")))
    return(data)
  }
}
