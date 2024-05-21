
# ------------------------------------------------------------------------------------------
# SAVING RESPONSES/REQUESTS
# ------------------------------------------------------------------------------------------

# styles
style.col.blue <- openxlsx::createStyle(fgFill="#CCE5FF", valign="top",
                              border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000",
                              valign="top", fontSize = 10, fontName = "Arial Narrow", wrapText=T)
style.col.green.bold <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                              border="TopBottomLeftRight", borderColour="#000000",
                              fontSize = 10, fontName = "Arial Narrow", wrapText=T)

# ------------------------------------------------------------------------------------------
save.responses <- function(df, wb_name, or.submission=""){
  # TODO: upgrade this function to work on changing df sizes
  # this function is most likely superceded by save.other.requests and save.trans.requests

  style.col.green.first <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  style.col.green.first2 <- openxlsx::createStyle(textDecoration="bold", fgFill="#CCE5FF", valign="top",
                                        border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb = wb, x = df, sheet = "Sheet1", startRow = 1)
  openxlsx::addStyle(wb, "Sheet1", style = style.col.green, rows = 1:(nrow(df)+1), cols=10)
  openxlsx::addStyle(wb, "Sheet1", style = style.col.green, rows = 1:(nrow(df)+1), cols=11)
  openxlsx::addStyle(wb, "Sheet1", style = style.col.green, rows = 1:(nrow(df)+1), cols=12)
  openxlsx::setColWidths(wb, "Sheet1", cols=1, widths=35)
  openxlsx::setColWidths(wb, "Sheet1", cols=c(5, 7), widths=50)
  openxlsx::setColWidths(wb, "Sheet1", cols=c(8:9), widths=30)
  openxlsx::setColWidths(wb, "Sheet1", cols=c(2:4, 6), widths=20)
  openxlsx::setColWidths(wb, "Sheet1", cols=c(10:12), widths=40)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=1)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=2)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=3)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=4)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=5)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=6)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=7)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=8)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=9)
  openxlsx::addStyle(wb, "Sheet1", style = openxlsx::createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df))
  openxlsx::addStyle(wb, "Sheet1", style = style.col.green.first, rows = 1, cols=10:12)
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Calibri")
  filename <- paste0("output/checking/requests/", wb_name, ".xlsx")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
}

save.other.requests <- function(df, wb_name, use_template = F){

  if(use_template) wb <- openxlsx::loadWorkbook("resources/other_requests_template.xlsm")
  else wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet2", zoom = 90)
  openxlsx::writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1,
            headerStyle = openxlsx::createStyle(textDecoration="bold", border = "Bottom", fontName = "Arial"))

    response_cols_ind <- which(stringr::str_starts(colnames(df), "response"))
  for(i in response_cols_ind){
    openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
             rows = 1:nrow(df)+1, cols=i)
    openxlsx::setColWidths(wb, "Sheet2", cols = i, widths = 30)
  }
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
           rows = 1:nrow(df)+1, cols=which(colnames(df) == "choices.label"))
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(fontSize = 11, wrapText = T),
           rows = 1:nrow(df)+1, cols=which(colnames(df) == "full.label"))

  openxlsx::setColWidths(wb, "Sheet2", cols = 1, widths = 5)
  openxlsx::setColWidths(wb, "Sheet2", cols = 2:which(colnames(df) == "choices.label")-1, widths = "auto")
  openxlsx::setColWidths(wb, "Sheet2", cols = which(colnames(df) == "choices.label"), widths = 50)
  openxlsx::setColWidths(wb, "Sheet2", cols = which(colnames(df) == "full.label"), widths = 30)
  openxlsx::setColWidths(wb, "Sheet2", cols = (ncol(df)-4):(ncol(df)), widths = 35)

  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df), stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-2, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-1, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df), stack = T)
  openxlsx::addWorksheet(wb, "Sheet3", visible = F)
  openxlsx::writeData(wb, "Sheet3", x = tool.choices)
  ##Adding data validation
  for (i in 1:nrow(df)){
    list_name <- get.choice.list.from.name(df$ref.name[i])
    range_min <- min(which(tool.choices$list_name %in% list_name)) + 1
    range_max <- max(which(tool.choices$list_name %in% list_name)) + 1
    validate <- paste0("'Sheet3'!$C",range_min,":$C",range_max)
    openxlsx::dataValidation(wb, "Sheet2", cols = ncol(df)-1, rows = 1 + i, type = "list", value = validate)
  }
  filename <- paste0(dir.requests, wb_name, ".xlsm")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)

}

save.deletion.requests <- function(df, wb_name, use_template = F){
  
  if(use_template) wb <- openxlsx::loadWorkbook("resources/deletion_requests_template.xlsm")
  else wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet2", zoom = 90)
  openxlsx::writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1,
            headerStyle = openxlsx::createStyle(textDecoration="bold", border = "Bottom", fontName = "Arial"))
  
  openxlsx::setColWidths(wb, "Sheet2", cols = 1, widths = 5)
  openxlsx::setColWidths(wb, "Sheet2", cols = 2:ncol(df), widths = "auto")
  
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-2, stack = T)
  openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-1, stack = T)
  # addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df), stack = T)
  openxlsx::addWorksheet(wb, "Sheet3", visible = F)
  openxlsx::writeData(wb, "Sheet3", x = c("Remove","Keep","Change"))
  ## add roster loop_indexes
  dl_uuids_roster <- df %>% 
    filter(variable == "num_hh" & stringr::str_starts(reason, "hh_roster")) %>% 
    pull(uuid)
  
  uuid_roster_loops <- raw.hh_roster %>% 
    filter(uuid %in% dl_uuids_roster) %>% 
    dplyr::select(uuid, loop_index)
  
  openxlsx::addWorksheet(wb, "Sheet4", visible = F)
  openxlsx::writeData(wb, "Sheet4",x = uuid_roster_loops)
  ## add health loop_indexes
  dl_uuids_health <- df %>% 
    filter(variable == "num_hh" & stringr::str_starts(reason, "ind_health")) %>% 
    pull(uuid)
  uuid_health_loops <- raw.ind_health %>% 
    filter(uuid %in% dl_uuids_health) %>% 
    dplyr::select(uuid, loop_index)
  openxlsx::addWorksheet(wb, "Sheet5", visible = F)
  openxlsx::writeData(wb, "Sheet5",x = uuid_health_loops)
  
  if(!is.null(raw.water_count_loop)){
    ## add water loop_indexes
    dl_uuids_water <- df %>% 
      filter(variable == "num_containers") %>% 
      pull(uuid)
    uuid_water_loops <- raw.water_count_loop %>% 
      filter(uuid %in% dl_uuids_water) %>% 
      dplyr::select(uuid, loop_index)
    openxlsx::addWorksheet(wb, "Sheet6", visible = F)
    openxlsx::writeData(wb, "Sheet6",x = uuid_water_loops)
  }
  
  ## add child loop_indexes
  dl_uuids_child <- df %>% 
    filter(variable == "num_hh" & stringr::str_starts(reason, "child")) %>% 
    pull(uuid)
  uuid_child_loops <- raw.child_nutrition %>% 
    filter(uuid %in% dl_uuids_child) %>% 
    dplyr::select(uuid, loop_index)
  openxlsx::addWorksheet(wb, "Sheet7", visible = F)
  openxlsx::writeData(wb, "Sheet7",x = uuid_child_loops)
  
  if(!is.null(raw.women)){
    ## add water loop_indexes
    dl_uuids_women <- df %>% 
      filter(variable == "num_hh" & stringr::str_starts(reason, "women")) %>% 
      pull(uuid)
    uuid_women_loops <- raw.women %>% 
      filter(uuid %in% dl_uuids_women) %>% 
      dplyr::select(uuid, loop_index)
    openxlsx::addWorksheet(wb, "Sheet8", visible = F)
    openxlsx::writeData(wb, "Sheet8",x = uuid_women_loops)
  }
  
  if(!is.null(raw.died_member)){
    ## add died loop_indexes
    dl_uuids_died <- df %>% 
      filter(variable == "num_died") %>% 
      pull(uuid)
    uuid_died_loops <- raw.died_member %>% 
      filter(uuid %in% dl_uuids_died) %>% 
      dplyr::select(uuid, loop_index)
    openxlsx::addWorksheet(wb, "Sheet9", visible = F)
    openxlsx::writeData(wb, "Sheet9",x = uuid_died_loops)
  }
  
  for (i in 1:nrow(df)){
    if(!is.na(df$variable[i]) & df$variable[i]  == "num_died"){
      uuid <- df$uuid[i]
      range_min <- min(which(uuid_died_loops$uuid %in% uuid)) + 1
      range_max <- max(which(uuid_died_loops$uuid %in% uuid)) + 1
      validate <- paste0("'Sheet9'!$B",range_min,":$B",range_max)
      openxlsx::dataValidation(wb, "Sheet2", cols = ncol(df)-1, rows = 1 + i, type = "list", value = validate)
    }
    if(!is.na(df$variable[i]) & df$variable[i] == "num_containers"){
      uuid <- df$uuid[i]
      range_min <- min(which(uuid_water_loops$uuid %in% uuid)) + 1
      range_max <- max(which(uuid_water_loops$uuid %in% uuid)) + 1
      validate <- paste0("'Sheet6'!$B",range_min,":$B",range_max)
      openxlsx::dataValidation(wb, "Sheet2", cols = ncol(df)-1, rows = 1 + i, type = "list", value = validate)
    }
    if(!is.na(df$variable[i]) & df$variable[i] == "num_hh" & stringr::str_starts(df$reason[i], "child")){
      uuid <- df$uuid[i]
      range_min <- min(which(uuid_child_loops$uuid %in% uuid)) + 1
      range_max <- max(which(uuid_child_loops$uuid %in% uuid)) + 1
      validate <- paste0("'Sheet7'!$B",range_min,":$B",range_max)
      openxlsx::dataValidation(wb, "Sheet2", cols = ncol(df)-1, rows = 1 + i, type = "list", value = validate)
    }
    if(!is.na(df$variable[i]) & df$variable[i] == "num_hh" & stringr::str_starts(df$reason[i], "women")){
      uuid <- df$uuid[i]
      range_min <- min(which(uuid_women_loops$uuid %in% uuid)) + 1
      range_max <- max(which(uuid_women_loops$uuid %in% uuid)) + 1
      validate <- paste0("'Sheet8'!$B",range_min,":$B",range_max)
      openxlsx::dataValidation(wb, "Sheet2", cols = ncol(df)-1, rows = 1 + i, type = "list", value = validate)
    }
    if(!is.na(df$variable[i]) & df$variable[i] == "num_hh" & stringr::str_starts(df$reason[i], "ind_health")){
      uuid <- df$uuid[i]
      range_min <- min(which(uuid_health_loops$uuid %in% uuid)) + 1
      range_max <- max(which(uuid_health_loops$uuid %in% uuid)) + 1
      validate <- paste0("'Sheet5'!$B",range_min,":$B",range_max)
      openxlsx::dataValidation(wb, "Sheet2", cols = ncol(df)-1, rows = 1 + i, type = "list", value = validate)
    }
    if(!is.na(df$variable[i]) & df$variable[i] == "num_hh" & stringr::str_starts(df$reason[i], "hh_roster")){
      uuid <- df$uuid[i]
      range_min <- min(which(uuid_roster_loops$uuid %in% uuid)) + 1
      range_max <- max(which(uuid_roster_loops$uuid %in% uuid)) + 1
      validate <- paste0("'Sheet4'!$B",range_min,":$B",range_max)
      openxlsx::dataValidation(wb, "Sheet2", cols = ncol(df)-1, rows = 1 + i, type = "list", value = validate)
    }
  }
  
  ##Adding data validation
  for (i in 1:nrow(df)){
    validate <- paste0("'Sheet3'!$A1:$A3")
    suppressWarnings(openxlsx::dataValidation(wb, "Sheet2", cols = ncol(df)-2, rows = 1 + i, type = "list", value = validate))
  }
  filename <- paste0(dir.requests, wb_name, ".xlsm")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
  
}


save.trans.requests <- function(df, wb_name, blue_cols = NULL, use_template = F){

    if(use_template) wb <- openxlsx::loadWorkbook("resources/trans_requests_template.xlsx")
    else wb <- openxlsx::createWorkbook()
    
    # remove the useless "EXISTING" column, and the word 'other':
    if(length(df %>% select(starts_with("EXISTING")) %>% names) > 0){
      df <- df %>% select(-starts_with("EXISTING"))
      colnames(df) <- gsub("other ", "", colnames(df))
    }
    df <- df %>% select(-starts_with("ref")) %>% select(-starts_with("choices"))
    
    openxlsx::addWorksheet(wb, "Sheet2")
    openxlsx::writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1)

    openxlsx::setColWidths(wb, "Sheet2", cols = 1, widths = 5)
    openxlsx::setColWidths(wb, "Sheet2", cols = 2:ncol(df), widths = "auto")

    response_cols_ind <- which(stringr::str_starts(colnames(df), "response"))
    for(i in append(response_cols_ind, 1)){
        openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(fontSize = 10, fontName = "Arial Narrow", wrapText = T),
                 rows = 1:nrow(df)+1, cols=i)
        openxlsx::setColWidths(wb, "Sheet2", cols = i, widths = 30)
    }
    for (col in blue_cols) {
        i <- grep(paste0('^',col,'$'), colnames(df))
        if(length(i) == 0) stop(paste(col,"not found in df!"))
        openxlsx::addStyle(wb, "Sheet2", style = style.col.blue, rows = 1:(nrow(df)+1), cols = i, stack = T)
        openxlsx::setColWidths(wb, "Sheet2", cols = which(colnames(df) == col), widths = 20)
    }

    openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(textDecoration="bold", valign = "bottom"), rows = 1, cols=1:ncol(df), stack = T)

    openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = which(stringr::str_starts(colnames(df), "TRUE")), stack = T)
    openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = which(stringr::str_starts(colnames(df), "TRUE")), stack = T)
    openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = which(stringr::str_starts(colnames(df), "INVALID")), stack = T)
    openxlsx::addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = which(stringr::str_starts(colnames(df), "INVALID")), stack = T)

    filename <- paste0(dir.requests, wb_name, ".xlsx")
    openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
}

# ------------------------------------------------------------------------------------------
# OUTLIER SECTION
# ------------------------------------------------------------------------------------------

save.outlier.responses <- function(df,wb_name, use_template = F){
  if(use_template) wb <- openxlsx::loadWorkbook("resources/outliers_requests_template.xlsx")
  else wb <- openxlsx::createWorkbook()
  
  style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000",
                                 valign="top", wrapText=T)
  style.col.green.first <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  style.col.green.first2 <- openxlsx::createStyle(textDecoration="bold", fgFill="#CCE5FF", valign="top",
                                        border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  openxlsx::addWorksheet(wb, "Sheet2")
  openxlsx::writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols=6)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols=7)
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols=8)
  openxlsx::setColWidths(wb, "Sheet2", cols=c(1:5), widths=35)
  openxlsx::setColWidths(wb, "Sheet2", cols=c(6:8), widths=40)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=1)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=2)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=3)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=4)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(valign="top"), rows = 1:(nrow(df)+1), cols=5)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=6)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=7)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(wrapText=T, valign="top"), rows = 1:(nrow(df)+1), cols=8)
  openxlsx::addStyle(wb, "Sheet2", style = openxlsx::createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df))
  openxlsx::addStyle(wb, "Sheet2", style = style.col.green.first, rows = 1, cols=6:8)
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Calibri")
  filename <- paste0("output/checking/requests/", wb_name)
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
}

save.follow.up.requests <- function(cleaning.log, data){
    #' [obsolete] - replaced by `create.follow.up.requests`
  use.color <- function(check.id){
    return(stringr::str_starts(check.id, "0"))
    # |  stringr::str_starts(check.id, "3") | stringr::str_starts(check.id, "4"))
  }
  # define styles
  style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.green.first <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  col.style <- openxlsx::createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  # dplyr::arrange cleaning.log so that colors are properly assigned later
  cleaning.log <- cleaning.log %>%
    dplyr::arrange(country) %>%
    dplyr::group_by(country) %>%
    dplyr::group_modify(~ rbind(
      filter(.x, !use.color(check.id)) %>% dplyr::arrange(check.id, uuid),
      filter(.x, use.color(check.id)) %>% dplyr::arrange(check.id)))
  # add missing columns
  cl <- cleaning.log %>%
    dplyr::left_join(select(data, uuid, `_submission_time`), by="uuid") %>%
    rename(submission_time="_submission_time") %>%
    dplyr::select(uuid, submission_time, country, Reporting_organization, enumerator.code, check.id,
           variable, issue, old.value, new.value) %>%
    dplyr::mutate(explanation=NA)
  cl <- cl %>% dplyr::arrange(match(check.id, stringr::str_sort(unique(cl$check.id), numeric=T)))
  for(i in country_list){
    cl1 <- cl %>%
      filter(country == i)
    # save follow-up requests
    wb <- createWorkbook()
    openxlsx::addWorksheet(wb, "Follow-up")
    openxlsx::writeData(wb = wb, x = cl1, sheet = "Follow-up", startRow = 1)

    openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl1)+1), cols=10)
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl1)+1), cols=11)

    openxlsx::setColWidths(wb, "Follow-up", cols=1:ncol(cl1), widths="auto")
    openxlsx::setColWidths(wb, "Follow-up", cols=7, widths=50)
    openxlsx::setColWidths(wb, "Follow-up", cols=8, widths=50)

    openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(wrapText=T), rows = 1:(nrow(cl1)+1), cols=7)
    openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(wrapText=T), rows = 1:(nrow(cl1)+1), cols=8)

    openxlsx::addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl1))

    col.id <- which(colnames(cl1)=="old.value")
    if(nrow(cl1) > 0){
      random.color <- ""
      for (r in 2:nrow(cl1)){
        if((!use.color(as.character(cl1[r, "check.id"])) &
            as.character(cl1[r, "uuid"])==as.character(cl1[r-1, "uuid"]) &
            as.character(cl1[r, "check.id"])==as.character(cl1[r-1, "check.id"])) |
           (use.color(as.character(cl1[r, "check.id"])) &
            as.character(cl1[r, "country"])==as.character(cl1[r-1, "country"]) &
            as.character(cl1[r, "check.id"])==as.character(cl1[r-1, "check.id"]))){
          if (random.color == "") random.color <- randomcoloR::randomColor(1, luminosity = "light")
          openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(fgFill=random.color, wrapText=T),
                   rows = r:(r+1), cols=col.id)
        } else random.color=""
      }
    }
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=10)
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=11)
    filename <- paste0("output/checking/requests/", stringr::str_to_lower(i) , "_follow_up_requests.xlsx")
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    rm(cl1)
  }
}

create.follow.up.requests <- function(checks.df,loop_data = NULL, wb_name, use_template = F){
    use.color <- function(check.id){
        return(stringr::str_starts(check.id, "0"))
    }
    # define styles
    style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
    style.col.green.first <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                         border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
    col.style <- openxlsx::createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                             border="TopBottomLeftRight", borderColour="#000000")
    # dplyr::arrange cleaning.log so that colors are properly assigned later
    cl <- checks.df %>%
        dplyr::arrange(variable) %>%
        dplyr::group_modify(~ rbind(
            filter(.x, !use.color(check.id)) %>% dplyr::arrange(check.id, uuid),
            filter(.x, use.color(check.id)) %>% dplyr::arrange(check.id)))
    cl <- cl %>% dplyr::arrange(match(check.id, stringr::str_sort(unique(cl$check.id), numeric=T)))
    # save follow-up requests
    if(use_template) wb <- openxlsx::loadWorkbook("resources/logical_requests_template.xlsm")
    else wb <- openxlsx::createWorkbook()
    
    openxlsx::addWorksheet(wb, "Follow-up", zoom = 90)
    openxlsx::writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)

    openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="explanation"))
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="loops_to_remove"))
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="invalid"))
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="new.value"))
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="explanation"))
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="loops_to_remove"))
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="new.value"))
    openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="invalid"))


    openxlsx::setColWidths(wb, "Follow-up", cols=1:ncol(cl), widths="auto")
    # openxlsx::setColWidths(wb, "Follow-up", cols=ncol(cl)-1, widths=50)

    openxlsx::setColWidths(wb, "Follow-up", cols=which(colnames(cl)=="issue"), widths=50)
    openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="issue"))

    openxlsx::addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl))

    col.id <- which(colnames(cl)=="old.value")
    if(nrow(cl) > 0){
      random.color <- ""
      for (r in 2:nrow(cl)){
        if((!use.color(as.character(cl[r, "check.id"])) &
            as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
            as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"])) |
           (use.color(as.character(cl[r, "check.id"])) &
            as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"]))){
          if (random.color == "") random.color <- randomcoloR::randomColor(1, luminosity = "light")
          openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(fgFill=random.color, wrapText=T),
                   rows = r:(r+1), cols=col.id)
        } else random.color=""
      }
    }
    new_tool <- tool.choices %>% 
      group_by(list_name) %>% 
      summarise(name = list(c(name, NA)), .groups = 'keep') %>% 
      tidyr::unnest(name) %>% 
      ungroup()
    openxlsx::addWorksheet(wb, "Sheet3", visible = F)
    openxlsx::writeData(wb, "Sheet3",x = new_tool)
    
    if(!is.null(loop_data)){
      cl_uuids <- cl %>% 
        filter(variable == "num_died") %>% 
        pull(uuid)
      uuid_died_loops <- loop_data %>% 
        filter(uuid %in% cl_uuids) %>% 
        dplyr::select(uuid, loop_index)
      openxlsx::addWorksheet(wb, "Sheet4", visible = F)
      openxlsx::writeData(wb, "Sheet4",x = uuid_died_loops)
      for (i in 1:nrow(cl)){
        if(cl$variable[i] == "num_died"){
          uuid <- cl$uuid[i]
          range_min <- min(which(uuid_died_loops$uuid %in% uuid)) + 1
          range_max <- max(which(uuid_died_loops$uuid %in% uuid)) + 1
          validate <- paste0("'Sheet4'!$B",range_min,":$B",range_max)
          openxlsx::dataValidation(wb, "Follow-up", cols = ncol(cl)-1, rows = 1 + i, type = "list", value = validate)
        }
      }
    }
    
    ##Adding data validation
    for (i in 1:nrow(cl)){
      if(stringr::str_detect(cl$variable[i],"/")) next
      type <- get.type(cl$variable[i])
      if(!is.na(type) & stringr::str_detect(type, "select")){
        list_name <- get.choice.list.from.name(cl$variable[i])
        range_min <- min(which(new_tool$list_name %in% list_name)) + 1
        range_max <- max(which(new_tool$list_name %in% list_name)) + 1
        validate <- paste0("'Sheet3'!$B",range_min,":$B",range_max)
        openxlsx::dataValidation(wb, "Follow-up", cols = ncol(cl)-3, rows = 1 + i, type = "list", value = validate)
      }
    }
    filename <- paste0("output/checking/requests/", wb_name)
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

create.follow.up.requests.data.quality <- function(checks.df,loop_data = NULL, wb_name, use_template = F){
  use.color <- function(check.id){
    return(stringr::str_starts(check.id, "0"))
  }
  # define styles
  style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.green.first <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  col.style <- openxlsx::createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  # dplyr::arrange cleaning.log so that colors are properly assigned later
  cl <- checks.df %>%
    dplyr::arrange(variable) %>%
    group_modify(~ rbind(
      filter(.x, !use.color(check.id)) %>% dplyr::arrange(check.id, uuid),
      filter(.x, use.color(check.id)) %>% dplyr::arrange(check.id)))
  cl <- cl %>% dplyr::arrange(match(check.id, stringr::str_sort(unique(cl$check.id), numeric=T)))
  # save follow-up requests
  if(use_template) wb <- openxlsx::loadWorkbook("./../resources/daily_requests.xlsx")
  else wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "Follow-up", zoom = 90)
  openxlsx::writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="explanation"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="loops_to_remove"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="invalid"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="new.value"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="explanation"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="loops_to_remove"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="new.value"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="invalid"))
  
  
  openxlsx::setColWidths(wb, "Follow-up", cols=1:ncol(cl), widths="auto")
  # openxlsx::setColWidths(wb, "Follow-up", cols=ncol(cl)-1, widths=50)
  
  openxlsx::setColWidths(wb, "Follow-up", cols=which(colnames(cl)=="issue"), widths=50)
  openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="issue"))
  
  openxlsx::addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl))
  
  col.id <- which(colnames(cl)=="old.value")
  if(nrow(cl) > 0){
    random.color <- ""
    for (r in 2:nrow(cl)){
      if((!use.color(as.character(cl[r, "check.id"])) &
          as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
          as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"])) |
         (use.color(as.character(cl[r, "check.id"])) &
          as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"]))){
        if (random.color == "") random.color <- randomcoloR::randomColor(1, luminosity = "light")
        openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(fgFill=random.color, wrapText=T),
                 rows = r:(r+1), cols=col.id)
      } else random.color=""
    }
  }
  new_tool <- tool.choices %>% 
    dplyr::group_by(list_name) %>% 
    dplyr::summarise(name = list(c(name, NA)), .groups = 'keep') %>% 
    tidyr::unnest(name) %>% 
    dplyr::ungroup()
  openxlsx::addWorksheet(wb, "Sheet3", visible = F)
  openxlsx::writeData(wb, "Sheet3",x = new_tool)
  
  if(!is.null(loop_data)){
    cl_uuids <- cl %>% 
      filter(variable == "num_died") %>% 
      pull(uuid)
    uuid_died_loops <- loop_data %>% 
      filter(uuid %in% cl_uuids) %>% 
      dplyr::select(uuid, loop_index)
    openxlsx::addWorksheet(wb, "Sheet4", visible = F)
    openxlsx::writeData(wb, "Sheet4",x = uuid_died_loops)
    for (i in 1:nrow(cl)){
      if(cl$variable[i] == "num_died"){
        uuid <- cl$uuid[i]
        range_min <- min(which(uuid_died_loops$uuid %in% uuid)) + 1
        range_max <- max(which(uuid_died_loops$uuid %in% uuid)) + 1
        validate <- paste0("'Sheet4'!$B",range_min,":$B",range_max)
        openxlsx::dataValidation(wb, "Follow-up", cols = ncol(cl)-1, rows = 1 + i, type = "list", value = validate)
      }
    }
  }
  
  ##Adding data validation
  for (i in 1:nrow(cl)){
    type <- get.type(cl$variable[i])
    if(!is.na(type) & stringr::str_detect(type, "select")){
      list_name <- get.choice.list.from.name(cl$variable[i])
      range_min <- min(which(new_tool$list_name %in% list_name)) + 1
      range_max <- max(which(new_tool$list_name %in% list_name)) + 1
      validate <- paste0("'Sheet3'!$B",range_min,":$B",range_max)
      openxlsx::dataValidation(wb, "Follow-up", cols = ncol(cl)-3, rows = 1 + i, type = "list", value = validate)
    }
  }
  openxlsx::saveWorkbook(wb, wb_name, overwrite = TRUE)
}

create.follow.up.requests.daily <- function(checks.df,loop_data = NULL, wb_name, use_template = F){
  use.color <- function(check.id){
    return(stringr::str_starts(check.id, "0"))
  }
  # define styles
  style.col.green <- openxlsx::createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.green.first <- openxlsx::createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  col.style <- openxlsx::createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  # dplyr::arrange cleaning.log so that colors are properly assigned later
  cl <- checks.df %>%
    dplyr::arrange(variable) %>%
    group_modify(~ rbind(
      filter(.x, !use.color(check.id)) %>% dplyr::arrange(check.id, uuid),
      filter(.x, use.color(check.id)) %>% dplyr::arrange(check.id)))
  cl <- cl %>% dplyr::arrange(match(check.id, stringr::str_sort(unique(cl$check.id), numeric=T)))
  # save follow-up requests
  if(use_template) wb <- openxlsx::loadWorkbook("resources/logical_requests_template.xlsm")
  else wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "Follow-up", zoom = 90)
  openxlsx::writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="explanation"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="loops_to_remove"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="invalid"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green, rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="new.value"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="explanation"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="loops_to_remove"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="new.value"))
  openxlsx::addStyle(wb, "Follow-up", style = style.col.green.first, rows = 1, cols=which(colnames(cl)=="invalid"))
  
  
  openxlsx::setColWidths(wb, "Follow-up", cols=1:ncol(cl), widths="auto")
  # openxlsx::setColWidths(wb, "Follow-up", cols=ncol(cl)-1, widths=50)
  
  openxlsx::setColWidths(wb, "Follow-up", cols=which(colnames(cl)=="issue"), widths=50)
  openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(wrapText=T), rows = 1:(nrow(cl)+1), cols=which(colnames(cl)=="issue"))
  
  openxlsx::addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:ncol(cl))
  
  col.id <- which(colnames(cl)=="old.value")
  if(nrow(cl) > 0){
    random.color <- ""
    for (r in 2:nrow(cl)){
      if((!use.color(as.character(cl[r, "check.id"])) &
          as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
          as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"])) |
         (use.color(as.character(cl[r, "check.id"])) &
          as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"]))){
        if (random.color == "") random.color <- randomcoloR::randomColor(1, luminosity = "light")
        openxlsx::addStyle(wb, "Follow-up", style = openxlsx::createStyle(fgFill=random.color, wrapText=T),
                 rows = r:(r+1), cols=col.id)
      } else random.color=""
    }
  }
  new_tool <- tool.choices %>% 
    dplyr::group_by(list_name) %>% 
    dplyr::summarise(name = list(c(name, NA)), .groups = 'keep') %>% 
    tidyr::unnest(name) %>% 
    dplyr::ungroup()
  openxlsx::addWorksheet(wb, "Sheet3", visible = F)
  openxlsx::writeData(wb, "Sheet3",x = new_tool)
  
  if(!is.null(loop_data)){
    cl_uuids <- cl %>% 
      filter(variable == "num_died") %>% 
      pull(uuid)
    uuid_died_loops <- loop_data %>% 
      filter(uuid %in% cl_uuids) %>% 
      dplyr::select(uuid, loop_index)
    openxlsx::addWorksheet(wb, "Sheet4", visible = F)
    openxlsx::writeData(wb, "Sheet4",x = uuid_died_loops)
    for (i in 1:nrow(cl)){
      if(cl$variable[i] == "num_died"){
        uuid <- cl$uuid[i]
        range_min <- min(which(uuid_died_loops$uuid %in% uuid)) + 1
        range_max <- max(which(uuid_died_loops$uuid %in% uuid)) + 1
        validate <- paste0("'Sheet4'!$B",range_min,":$B",range_max)
        openxlsx::dataValidation(wb, "Follow-up", cols = ncol(cl)-1, rows = 1 + i, type = "list", value = validate)
      }
    }
  }
  
  ##Adding data validation
  for (i in 1:nrow(cl)){
    type <- get.type(cl$variable[i])
    if(!is.na(type) & stringr::str_detect(type, "select")){
      list_name <- get.choice.list.from.name(cl$variable[i])
      range_min <- min(which(new_tool$list_name %in% list_name)) + 1
      range_max <- max(which(new_tool$list_name %in% list_name)) + 1
      validate <- paste0("'Sheet3'!$B",range_min,":$B",range_max)
      openxlsx::dataValidation(wb, "Follow-up", cols = ncol(cl)-3, rows = 1 + i, type = "list", value = validate)
    }
  }
  openxlsx::saveWorkbook(wb, wb_name, overwrite = TRUE)
}

# ------------------------------------------------------------------------------------------
# LOADING REQUESTS/RESPONSES FILES
# ------------------------------------------------------------------------------------------

load.requests <- function(dir, filename.pattern, sheet=NULL, validate=FALSE){
  #' Load 'request' logs from specified directory.
  #'
  #' Searches `dir` to find XLSX files with names that start with a match for `filename.pattern`.
  #' NB: This pattern-matching is case-insensitive. If files contain the classic "TRUE", "EXISTING" or "INVALID" (TEI) columns,
  #' these will be renamed to "true.v", "existing.v", and "invalid.v" respectively and optionally validated for errors.
  #'
  #' @param dir Directory which should be searched for files.
  #' @param filename.pattern String with a regex pattern which will be passed to `list.files` to match files,
  #' however: '^' (string start) is added at the start of the pattern, and ".*\\.xlsx" is added at the end,
  #' so effectively files that will be loaded must be XLSX and have names that start with the provided pattern.
  #'
  #' @param sheet Optional parameter passed to `readxl::read_xlsx`, defaults to NULL (first sheet of an Excel workbook)
  #' @param validate Should the file be validated (make sure that only one of TEI columns is filled.)
  #' 
  file.type = stringr::str_squish(stringr::str_replace_all(filename.pattern, "[^a-zA-Z]+"," "))
  filenames <- list.files(dir, recursive=FALSE, full.names=TRUE, ignore.case = TRUE,
                          pattern=paste0(dataset.name.short,"_",filename.pattern))
  if (length(filenames) == 0){
    warning(paste("Files with",file.type,"requests not found!"))
  } else {
    cat(paste("Loading",length(filenames),file.type,"requests files:\n"),paste(filenames, collapse = "\n "),"\n")
    res <- data.frame()
    for (filename in filenames){
      # load file
      other <- readxl::read_xlsx(filename, col_types = "text", trim_ws = T, sheet = sheet)
      if (filename==filenames[1]) res <- other
      else{
        if(ncol(res)!=ncol(other)) warning("Number of columns differs between files! Check them to make sure everything is correct, please!")
        res <- bind_rows(res, other)
      }

    }
    # rename: TRUE -> true.v, EXISTING -> existing.v, INVALID -> invalid.v
    c_tei_cols <- c("true", "existing", "invalid")
    for(c in c_tei_cols) colnames(res)[stringr::str_starts(colnames(res), stringr::str_to_upper(c))] <- paste0(c,'.v')

    if(validate){
      c_tei_cols <- paste0(c_tei_cols, ".v")
      if(all(c_tei_cols %in% colnames(res))){
        res <- res %>% mutate(check = rowSums(is.na(select(res, all_of(c_tei_cols)))))
        check.res <- res %>% select(c(
          any_of(c("uuid","ref.type","check"))))
        check.missing <- check.res %>% filter(check == 3)
        if(nrow(check.missing)){
          warning(paste0("Missing entries:\n", paste0(" ", unlist(check.missing[,1]) , collapse = "\n")))
        }
        if("ref.type" %in% colnames(check.res)){
          check.res <- check.res %>%
            filter(check == 0 | (check == 1 & ref.type != "select_multiple"))  # select_multiple can have 2 columns selected
        }else{
          check.res <- filter(check.res, check != 2)
        }
        if(nrow(check.res)>0) {
          warning(paste0("Multiple columns selected:\n", paste0(" ", unlist(check.res[,1]) , collapse = "\n")))
        }
      }else{
        stop("One or more of 'true', 'existing', 'invalid' columns not found in requests files.")
      }
    }
    return(res)
  }
}

load.edited <- function(dir.edited, file.type){
  #' Load logs from specified directory.
  #'
  #' [obsolete] This function is superceded by load.requests


  # file.type should be one of the following:
  valid_types = c("other","translate","follow_up","outliers")
  if(!(file.type %in% valid_types))
    warning("Unexpected file.type for load.edited")

  filenames <- list.files(dir.edited, recursive=FALSE, full.names=TRUE, ignore.case = TRUE,
                          pattern=paste0(".*",file.type,"((responses)|(requests))?(_edited)?.*\\.xlsx$"))
  if (length(filenames) == 0){
    warning(paste("Files with",file.type,"responses not found!"))
  } else {
    cat(paste("Loading",length(filenames),file.type,"logs:\n"),paste(filenames, collapse = "\n "),"\n")
    res <- data.frame()
    for (filename in filenames){
      # load file
      other <- readxl::read_xlsx(filename, col_types = "text") %>% mutate(uuid=uuid, .before=1)
      if (filename==filenames[1]) res <- other
      else res <- rbind(res, other)
    }
    return(res)

  }
}

load.logic.request <- function(dir.requests){
  logic.filenames <- list.files(dir.requests, pattern="follow_up_requests",
                                recursive=FALSE, full.names=TRUE)
  cat(paste("\nLoading",length(logic.filenames),"logic requests logs:\n"),paste(logic.filenames, collapse = "\n "),"\n")
  for (filename in logic.filenames){
    # load file
    trans <- readxl::read_xlsx(filename) %>%
      mutate(uuid=uuid, .before=1)
    if (filename==logic.filenames[1]) res <- trans
    else res <- rbind(res, trans)
  }
  return(res)
}

load.outlier.edited <- function(dir.outlier.edited){
  logic.filenames <- list.files(dir.outlier.edited, pattern="outliers_responses.xlsx",
                                recursive=TRUE, full.names=TRUE)
  cat(paste("Loading",length(logic.filenames),"outlier logs:\n"),paste(logic.filenames, collapse = "\n "),"\n")
  res <- data.frame()
  for (filename in logic.filenames){
    # load file
    trans <- readxl::read_xlsx(filename) %>%
      mutate(uuid=uuid, .before=1)
    if (filename==logic.filenames[1]) res <- trans
    else res <- rbind(res, trans)
  }
  return(res)
}

# ------------------------------------------------------------------------------------------
# CLEANING LOG FUNCTIONS
# ------------------------------------------------------------------------------------------

# recoding select_multiples:
# ------------------------------------------------------------------------------
CL_COLS <- c("uuid", "loop_index", "variable", "old.value", "new.value", "issue")

recode.set.NA.if <- function(data, variables, code, issue, ignore_case = T){
    #' Recode a question by setting variables to NA if they are equal to a given value (code).
    #'
    #' @param data Dataframe containing records which will be affected.
    #' @param variables Vector of strings (or a single string) containing the names of the variables.
    #' @param code Vector of strings (or a single string) which will be changed to NA.
    #' @param issue String with explanation used for the cleaning log entry.
    #' @param ignore_case Whether `code` should be matched case-insensitively. Defaults to True.
    #'
    #' @returns Dataframe containing cleaning log entries constructed from `data`.
    #'
    #' @usage `recode.set.NA.if(data = filter(raw.main, condition),
    #'  variables = c("question1", "question2"),
    #'   code = "999", issue = "explanation")`
    #'
    
  # TODO filter variables to only include those in data (and produce warnings)
  
    clog <- tibble()
    for(variable in variables){
        if(ignore_case) data1 <- data %>% filter(stringr::str_to_lower(!!sym(variable)) %in% stringr::str_to_lower(code))
        else data1 <- data %>% filter(!!sym(variable) %in% code)
        cl <- data1 %>% mutate(variable = variable, old.value = !!sym(variable), new.value = NA,
                              issue = issue) %>% select(any_of(CL_COLS))
        clog <- rbind(clog, cl)
    }
    return(clog)
}

recode.set.NA.regex <- function(data, variables, pattern, issue){
  #' Recode a question by setting variables to NA if they are equal to a given value (code).
  #'
  #' @param data Dataframe containing records which will be affected.
  #' @param variables Vector of strings (or a single string) containing the names of the variables.
  #' @param code Vector of strings (or a single string) which will be changed to NA.
  #' @param issue String with explanation used for the cleaning log entry.
  #'
  #' @returns Dataframe containing cleaning log entries constructed from `data`.
  #'
  #' @usage `recode.set.NA.if(data = filter(raw.main, condition),
  #'  variables = c("question1", "question2"),
  #'   code = "999", issue = "explanation")`
  #'
  clog <- tibble()
  for(variable in variables){
    data1 <- data %>% filter(stringr::str_detect(!!sym(variable), pattern = pattern))
    cl <- data1 %>% mutate(variable = variable, old.value = !!sym(variable), new.value = NA,
                           issue = issue) %>% select(any_of(CL_COLS))
    clog <- rbind(clog, cl)
  }
  return(clog)
}

recode.set.value.regex <- function(data, variables, pattern, new.value, issue){
  #' Recode a question by setting variables to NA if they are equal to a given value (code).
  #'
  #' @param data Dataframe containing records which will be affected.
  #' @param variables Vector of strings (or a single string) containing the names of the variables.
  #' @param code Vector of strings (or a single string) which will be changed to NA.
  #' @param issue String with explanation used for the cleaning log entry.
  #'
  #' @returns Dataframe containing cleaning log entries constructed from `data`.
  #'
  #' @usage `recode.set.NA.if(data = filter(raw.main, condition),
  #'  variables = c("question1", "question2"),
  #'   code = "999", issue = "explanation")`
  #'
  clog <- tibble()
  for(variable in variables){
    data1 <- data %>% filter(stringr::str_detect(!!sym(variable), pattern = pattern))
    cl <- data1 %>% mutate(variable = variable, old.value = !!sym(variable), new.value = new.value,
                           issue = issue) %>% select(any_of(CL_COLS))
    clog <- rbind(clog, cl)
  }
  return(clog)
}

recode.multiple.set.NA <- function(data, variable, issue){
    #' Recode select_multiple responses: set to NA.
    #'
    #' Changes all 1s and 0s to NA in choice columns, sets cumulative variable and _other text answers to NA.
    #'
    #' @param data Dataframe containing records which will be affected.
    #' @param variable String containing the name of the select_multiple variable.
    #' @param issue String with explanation used for the cleaning log entry.
    #'
    #' @returns Dataframe containing cleaning log entries constructed from `data`.
    #'
    #' @usage `recode.multiple.set.NA(data = filter(raw.main, condition), variable = "question_name", issue = "explanation")`

    ccols <- colnames(data)[stringr::str_starts(colnames(data), paste0(variable, "/"))]

    # filter out cases that already are NA
    data <- data %>% filter(!if_all(all_of(ccols), ~is.na(.)))
    if(nrow(data)>0){
        cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
            mutate(variable = variable, old.value = !!sym(variable), new.value = NA, issue = issue) %>%
            select(any_of(CL_COLS))

        cl_choices <- data.frame()
        for(col in ccols){
            df <- data %>% filter(!is.na(!!sym(col)))
            if(nrow(df)>0){
                cl <- df %>%
                    mutate(variable = col, old.value = !!sym(col), new.value = NA, issue = issue) %>%
                    select(any_of(CL_COLS))

                cl_choices <- rbind(cl_choices, cl)
                # remove text from text other response
                if(stringr::str_ends(col, "/other")){
                    cl_other_text <- df %>% filter(!is.na(!!sym(paste0(variable, "_other")))) %>% 
                      mutate(variable = paste0(variable, "_other"), old.value = !!sym(paste0(variable, "_other")),
                      new.value = NA, issue = issue) %>%
                      select(any_of(CL_COLS))
                    
                    cl_choices <- rbind(cl_choices, cl_other_text) 
                }
            }
        }
        return(rbind(cl_cummulative, cl_choices))

    }
    return(data.frame())

}

recode.multiple.set.choice <- function(data, variable, choice, issue){
    #' Recode select_multiple responses: set answer to one particular choice.
    #'
    #' Changes all 1s to 0 in choice columns (except for `choice`) and sets cumulative variable to be equal to `choice`.
    #' Additionally, all NAs will be changed too.
    #'
    #' @param data Dataframe containing records which will be affected.
    #' @param variable String containing the name of the select_multiple variable.
    #' @param choice String containing the choice that must be a valid option for this variable.
    #' @param issue String with explanation used for the cleaning log entry.
    #'
    #' @returns Dataframe containing cleaning log entries constructed from `data`.
    #'
    #' @usage `recode.multiple.set.choice(data = filter(raw.main, condition), variable = "question_name", choice = "option", issue = "explanation")`
    #'
    choice_column <- paste0(variable,"/",choice)
    if(!choice_column %in% colnames(data)) stop(paste("Column",choice_column,"not present in data!"))
    # filter out cases that already have only this choice selected
    data <- data %>% filter(!!sym(variable) %!=na% choice)
    if(nrow(data) > 0){
        cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
            rename(old.value = !!sym(variable)) %>%
            mutate(variable = variable, new.value = choice, issue = issue)

        cl_choices <- data %>%
            mutate(variable = choice_column, old.value = !!sym(choice_column), new.value = "1", issue = issue) %>%
            select(any_of(CL_COLS))

        # set all other choices columns to 0
        cols <- colnames(data)[stringr::str_starts(colnames(data), paste0(variable, "/")) &
                                   !(stringr::str_ends(colnames(data), choice))]
        for(col in cols){
            df <- data %>% filter(!!sym(col) %!=na% "0")
            if(nrow(df>0)){
                cl <- df %>%
                    mutate(variable = col, old.value = !!sym(col), new.value = "0", issue = issue) %>%
                    select(any_of(CL_COLS))

                cl_choices <- rbind(cl_choices, cl)
            }
        }
        return(rbind(cl_cummulative, cl_choices))

    }
    return(data.frame())
}

recode.multiple.add.choices <- function(data, variable, choices, issue){
    #' TODO add documentation
    #'
    choice_columns <- paste0(variable,"/",choices)
    if(any(!choice_columns %in% colnames(data))){
      stop(paste("\nColumn",choice_columns[!choice_columns %in% colnames(data)],"not present in data!"))
    }
    choices_pattern <- paste0("(",paste0(choices, collapse = ")|("), ")")
    choices_len <- stringr::str_length(paste0(choices, collapse = "")) + length(choices)
    # filter out cases that already have all choices selected
    data <- data %>%
      select(any_of(c("uuid", "loop_index", variable)), all_of(choice_columns)) %>% filter(!is.na(!!sym(variable))) %>%
        mutate(variable2 = stringr::str_squish(stringr::str_remove_all(!!sym(variable), choices_pattern))) %>%
        mutate(len_diff = stringr::str_length(!!sym(variable)) - stringr::str_length(variable2)) %>%
        filter(stringr::str_length(!!sym(variable)) - stringr::str_length(variable2) != choices_len)
    if(nrow(data) > 0){
      cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable, "variable2"))) %>%
          rename(old.value = !!sym(variable)) %>%
          mutate(variable = variable, new.value = stringr::str_squish(paste(variable2, paste0(choices, collapse = " "))), issue = issue) %>%
          select(-variable2)
      if(all(cl_cummulative$new.value %==na% cl_cummulative$old.value)) cl_cummulative <- data.frame()
      cl_choices <- data.frame()
      for(choice in choices){
          choice_column <- paste0(variable,"/",choice)
          data1 <- data %>% filter(stringr::str_detect(!!sym(variable), choice, negate = T))
          if(nrow(data1) > 0){
            cl_choice <- data1 %>% select(any_of(c("uuid","loop_index"))) %>%
                mutate(variable = choice_column, old.value = "0", new.value = "1", issue = issue)
            cl_choices <- rbind(cl_choices, cl_choice)
          }
      }
      return(rbind(cl_cummulative, cl_choices))
    }
    return(data.frame())
}

recode.multiple.add.choice <- function(data, variable, choice, issue){
  #' [obsolete] use `recode.multiple.add.choices` instead
  
  choice_column <- paste0(variable,"/",choice)
  if(!choice_column %in% colnames(data)) stop(paste("Column",choice_column,"not present in data!"))
  # filter out cases that already have choice selected
  data <- data %>% filter(stringr::str_detect(!!sym(variable), choice, negate = T))
  if(nrow(data) > 0){
    cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
      rename(old.value = !!sym(variable)) %>%
      mutate(variable = variable, new.value = stringr::str_squish(paste(old.value, choice)), issue = issue)

    cl_choice <- data %>% select(any_of(c("uuid", "loop_index"))) %>%
      mutate(variable = choice_column, old.value = "0", new.value = "1", issue = issue)
    return(rbind(cl_cummulative, cl_choice))
  }
  return(data.frame())
}

recode.multiple.remove.choice <- function(data, variable, choice, issue){
  #' TODO add documentation
  choice_column <- paste0(variable,"/",choice)
  if(!choice_column %in% colnames(data)) stop(paste("Column",choice_column,"not present in data!"))
  # filter out cases that dont have the choice selected
  data <- data %>% filter(stringr::str_detect(!!sym(variable), choice))
  if(nrow(data) > 0){
    cl_cummulative <- data %>% select(any_of(c("uuid", "loop_index", variable))) %>%
      rename(old.value = !!sym(variable)) %>%
      mutate(variable = variable, new.value = stringr::str_squish(stringr::str_remove(old.value, choice)), issue = issue)

    cl_choice <- data %>% select(any_of(c("uuid", "loop_index"))) %>%
      mutate(variable = choice_column, old.value = "1", new.value = "0", issue = issue)
    return(rbind(cl_cummulative, cl_choice))
  }
  return(data.frame())
}


apply.changes <- function(data, clog, is.loop = F, suppress.diff.warnings = F){
  #' Apply changes to main data basing on a cleaning log.
  #'
  #' Outputs warnings if uuids, loop indexes or variables from `clog` are not found in `data`.
  #' Be aware: all values will be written to data as character.
  #' @param data Data (raw.main or raw.loop#)
  #' @param clog Cleaning log - dataframe containing columns uuid, variable, new.value, old.value
  #' @param is.loop Set to True if the provided dataframe is a loop.
  #' @param suppress.diff.warnings Set to True if you want to omit warnings related to value mismatches. Default False.
  #'
  #' @returns Dataframe containing data with applied changes

    if(!is.loop && ("loop_index" %in% colnames(clog))){
      clog <- filter(clog, is.na(loop_index))
    }else if(is.loop)
      clog <- filter(clog, !is.na(loop_index))
    if(nrow(clog) == 0){
        warning("No changes to be applied (cleaning log empty).")
    }
    else{
        missinguuids <- c()
        missingloop_indexs <- c()
        missingvars <- c()
        for (r in 1:nrow(clog)){
          variable <- as.character(clog$variable[r])
            if(!variable %in% colnames(data)) {
              missingvars <- append(missingvars, variable)
              next
            }
          if(is.loop){
            loop_index <- as.character(clog$loop_index[r])
            if(!loop_index %in% data$loop_index){
              missingloop_indexs <- append(missingloop_indexs, loop_index)
              next
            }
            if(!suppress.diff.warnings && data[data$loop_index == loop_index, variable] %!=na% clog$old.value[r]){
              warning(paste0("Value in data is different than old.value in Cleaning log!\nloop_index: ", loop_index,
                             "\tExpected: ", clog$old.value[r], "\t found: ", data[data$loop_index == loop_index, variable],
                             "\tReplacing with: ", clog$new.value[r]))
            }
            data[data$loop_index == loop_index, variable] <- as.character(clog$new.value[r])
          }else {
            uuid <- as.character(clog$uuid[r])
            if(!uuid %in% data$uuid) {
              missinguuids <- append(missinguuids, uuid)
              next
            }
            if(!suppress.diff.warnings && data[data$uuid == uuid, variable] %!=na% clog$old.value[r]){
              warning(paste0("Value in data is different than old.value in Cleaning log!\nUUID: ", uuid,
                            "\tExpected: ", clog$old.value[r], "\t found: ", data[data$uuid == uuid, variable],
                            "\tReplacing with: ", clog$new.value[r]))
            }
            data[data$uuid == uuid, variable] <- as.character(clog$new.value[r])
          }
        }
        if(length(missinguuids > 0)) warning(paste0("uuids from cleaning log not found in data:\n", paste0(missinguuids, collapse = "\n")))
        if(length(missingloop_indexs) > 0) warning(paste0("loop_indexes from cleaning log not found in data:\n", paste0(missingloop_indexs, collapse = "\n")))
        if(length(missingvars > 0))  warning(paste0("variables from cleaning log not found in data:\n", paste0(missingvars, collapse = "\n")))
    }
    return(data)
}


make.logical.check.entry <- function(check, id, question.names,cols_to_keep = c(), issue, is.loop = F){
  #' Create a logical check DF
  #'
  #' this function replaces `add.to.cleaning.log`. The functionality is changed:
  #' no longer modifies a global environment variable, instead returns a dataframe.
  #'
  #' @param check Dataframe with data, filtered according to some flag. Must contain columns `uuid` and all columns in `question.names`
  #' @param id The identifier of this logical check.
  #' @param question.names List of relevant queston names for this logical check.
  #' @param cols_to_keep List of columns from raw.main to be included in result.
  #'
  #' @returns Dataframe containing at the least columns: `uuid`, `check.id`, `variable`, `issue`, `old.value`, `new.value`, `explanation`.
  #' This object can be later added to cleaning log.

  res <- data.frame()
  if(is.loop) {
      cols_to_keep <- append(cols_to_keep, "loop_index")
  }
  for(q.n in question.names){
    new.entries <- check %>%
      mutate(variable = q.n, issue=issue,
             old.value =!!sym(q.n), new.value = NA, invalid = NA, explanation = NA)
    new.entries[["check.id"]] <- id
    new.entries <- new.entries %>%
      select(any_of(c(cols_to_keep, "uuid", "check.id", "variable", "issue",
                      "old.value", "new.value", "invalid", "explanation"))) %>%
      relocate(uuid) %>%
      mutate_all(as.character)
    res <- rbind(res, new.entries)
  }
  if(is.loop & !("loop_index" %in% colnames(res))){
      # res$loop_index <- NA
      res <- res %>% mutate(loop_index = NA, .after = uuid)
    }
  return(res %>% dplyr::arrange(uuid))
}

add.to.cleaning.log.other.recode <- function(data, x){
  if(!"existing.other" %in% colnames(x)){
    x <- rename_with(x, ~gsub(".v",".other", .), ends_with(".v"))
  } # a bit of a dirty fix :)
  if (x$ref.type[1]=="select_one") res <- add.to.cleaning.log.other.recode.one(data, x)
  if (x$ref.type[1]=="select_multiple") res <- add.to.cleaning.log.other.recode.multiple(data, x)
  if (res == "err") cat("Errors while recoding other. Check the warnings!\t")
}

add.to.cleaning.log <- function(checks, check.id, question.names=c(), issue="", enumerator.code.col="Staff_Name"){
  #' [obsolete]
  for(q.n in question.names){
    new.entries <- checks %>% filter(flag) %>%
      mutate(variable = q.n,
             issue=issue,
             old.value =!!sym(q.n),
             new.value=NA,
             explanation =NA)
    new.entries[["check.id"]] <- check.id
    new.entries <- new.entries %>% select(any_of(c("today", "uuid", "country", "Reporting_organization",
                                                   enumerator.code.col, "check.id",
                                                   "variable", "issue", "old.value", "new.value", "explanation"))) %>%
      dplyr::rename(enumerator.code=enumerator.code.col)
    cleaning.log.checks <<- dplyr::arrange(rbind(cleaning.log.checks, new.entries),country, uuid)
  }
}

add.to.cleaning.log.other.remove <- function(data, x){
  issue <- "Invalid other response"
  old.response <- data %>% filter(uuid == x$uuid) %>% pull(!!sym(x$name))
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=old.response, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # remove relative entries
  if (x$ref.type[1]=="select_one"){
    old.value <- "other"
    df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue, old.value=old.value, new.value=NA)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
  if (x$ref.type[1]=="select_multiple"){
    old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name])
    l <- stringr::str_split(old.value, " ")[[1]]
    new.value <- paste(l[l!="other"], collapse=" ")
    new.value <- ifelse(new.value=="", NA, new.value)
    df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue, old.value=old.value, new.value=new.value)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    if (is.na(new.value)){
      # set all choices columns to NA
      cols <- colnames(data)[stringr::str_starts(colnames(data), paste0(x$ref.name, "/"))]
      oldvalues <- data %>% filter(uuid == x$uuid) %>%
        select(all_of(cols)) %>% unlist() %>% unname()
      df <- data.frame(uuid=x$uuid, variable=cols, issue=issue, old.value=oldvalues, new.value=NA)
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    } else{
      df <- data.frame(uuid=x$uuid, variable=paste0(x$ref.name, "/other"), issue=issue,
                       old.value="1", new.value="0")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
  }
}

add.to.cleaning.log.trans.remove <- function(data, x){
  issue <- "Invalid other response"
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=x$response.uk, new.value=NA)
  cleaning.log.trans <<- rbind(cleaning.log.trans, df)
}

add.to.cleaning.log.other.recode.one <- function(data, x){
  issue <- "Recoding other response"
  old.response <- data %>% filter(uuid == x$uuid) %>% pull(!!sym(x$name))
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=old.response, new.value=NA)

  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices from other response
  if (stringr::str_detect(x$existing.other, ";")) {
    choices <- stringr::str_trim(stringr::str_split(x$existing.other, ";")[[1]])
  } else {
    choices <- stringr::str_trim(stringr::str_split(x$existing.other, "\r\n")[[1]])
  }
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
    df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue,
                     old.value="other", new.value=new.code$name)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
    return("succ")
  }
}

add.to.cleaning.log.other.recode.multiple <- function(data, x){
  issue <- "Recoding other response"
  old.response <- data %>% filter(uuid == x$uuid) %>% pull(!!sym(x$name))
  # remove text of the response
  df <- data.frame(uuid=x$uuid, variable=x$name, issue=issue,
                   old.value=old.response, new.value=NA)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices from other response
  if (stringr::str_detect(x$existing.other, ";")) {
    choices <- stringr::str_trim(stringr::str_split(x$existing.other, ";")[[1]])
  } else {
    choices <- stringr::str_trim(stringr::str_split(x$existing.other, "\r\n")[[1]])
  }
  choices <- choices[choices!=""]
  # set variable/other to "0"
  df <- data.frame(uuid=x$uuid,  variable=paste0(x$ref.name, "/other"), issue=issue,
                   old.value="1", new.value="0")
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  # get list of choices already selected
  old.value <- as.character(data[data$uuid==x$uuid[1], x$ref.name[1]])
  l <- stringr::str_split(old.value, " ")[[1]]
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
      old.boolean <- data[[variable.name]][data$uuid==x$uuid[1]]
    } else stop(paste("Column", variable.name,"not found in data"))
    if (old.boolean=="0"){
      df <- data.frame(uuid=x$uuid, variable=variable.name, issue=issue,
                       old.value=old.boolean, new.value="1")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
    }
    l.cumulative <- unique(c(l.cumulative, new.code$name))
  }
  # update cumulative variable
  new.value <- stringr::str_squish(paste(sort(l.cumulative), collapse=" "))
  df <- data.frame(uuid=x$uuid, variable=x$ref.name, issue=issue,
                   old.value=old.value, new.value=new.value)
  cleaning.log.other <<- rbind(cleaning.log.other, df)
  return("succ")
}


# ------------------------------------------------------------------------------------------
# DELETION LOG FUNCTIONS
# ------------------------------------------------------------------------------------------

create.deletion.log <- function(data, col_enum, reason){
  #' Creates a deletion log for the provided ids and reason.
  #'
  #' @param data Dataframe containing columns 'uuid' and `col_enum`. For example, this could be a subset of `raw.main`, filtered.
  #' @param col_enum Name of the column which contains the enumerator's id.
  #' @param reason This is a string describing the reason for removing a survey from data.
  #' @returns A dataframe containing a deletion log with columns `uuid`, `col_enum`, `reason`, OR an empty dataframe if `data` has 0 rows.
  
  if(nrow(data) > 0){
    # if it's a loop, then include the loop_index in the deletion log
    if("loop_index" %in% colnames(data))
      data <- data %>% select(uuid, loop_index, any_of(col_enum))
    else
      data <- data %>% select(uuid, any_of(col_enum))
    
    return(data %>% mutate(reason=reason))
  }else return(data.frame())
}


# ------------------------------------------------------------------------------------------
# FIND & TRANSLATE RESPONSES
# ------------------------------------------------------------------------------------------

find.responses <- function(data, questions.db, values_to="response.uk", is.loop = F){
  #' Look up a raw Kobo dataframe to find all responses to a given set of questions.
  #'
  #' The dataframe `questions.db` needs to contain a column `name` (like a subset of `tool.survey`) which will be used to look up `data`.
  #' The input `data` needs to contain a column "uuid", and all the columns specified in `questions.db`
  #' The vector containing found responses is stored in column specified by parameter `values_to`.
  #'
  #' Be warned: all responses will be converted to character.sou
  #' @param values_to Name of the column in which found responses will be stored.
  #' @returns A dataframe containing columns "uuid", "question.name", and the column specified by `values_to`. Additionally, "loop_index" if `is.loop` is TRUE.
  #' @example
  #' q.db <- data.frame(name = c("age", "occupation"))
  #' raw.data <- data.frame(age = c(21,32), occupation = c("cook", "train conductor"), uuid = c("abc","def"))
  #' find.responses(raw.data, q.db, "responses")

  if(nrow(questions.db) == 0){
      warning("questions.db is empty - returning an empty dataframe.")
      return(data.frame())
  }

  if(nrow(data) == 0){
      warning("data is empty - returning an empty dataframe.")
      return(data.frame())
  }

  if(!is.loop){
    data[["loop_index"]] <- NA
  }
  responses <- data %>%
      select(c("uuid", "loop_index", any_of(questions.db$name))) %>%
      tidyr::pivot_longer(cols = any_of(questions.db$name),
                   names_to="question.name", values_to=values_to,
                   values_transform = as.character) %>%
      filter(!is.na(!!sym(values_to))) %>%
      select(uuid, loop_index, question.name, !!sym(values_to))

  if(is.loop){
    responses.j <- responses %>%
      left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>%
      left_join(select(data, loop_index), by="loop_index")
    } else {
    responses.j <- responses %>%
        left_join(questions.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>%
        left_join(select(data, uuid), by="uuid") 
    # relevant_colnames <- relevant_colnames[!relevant_colnames %in% c("loop_index")]
    }
  return(responses.j)
}

translate.responses <- function(responses, values_from = "response.uk", language_codes = 'uk', target_lang = "en", threshold = 200000){

  #' Translate a vector from a given dataframe.
  #'
  #' The provided dataframe `responses` must contain the column `values_from` which will be used as input vector for the translation.
  #' Also outputs informative logs to file named "translate_info.csv". Specify the target language using `target_lang` parameter
  #'
  #' Warning: If more than one source language code is provided, the entire translation WILL BE REPEATED. You are advised against that,
  #' because we do not want to hit our monthly limits for the API.
  #'
  #' @param respones Dataframe containing a column which shall be translated.
  #' @param values_from Name of the column from `responses` which shall be translated.
  #' @param language_codes Character vector of two-letter language codes. The input vector will be translated from both of these languages.
  #' @param target_lang Input vector will be translated into this language.
  #' @param threshold Input threshold to interrupt the user if the number of characters is exceeding 200,000 by default. 
  #' @returns The same dataframe as `responses`, but with a new column, containing the translation.
  #' The column will be named according to the given source and target languages. By default, the output will be stored in column named 'response.en.from.uk'
  info_df <- data.frame()
  responses_batch <- data.frame()
  temp_resp_whole <- data.frame()
  start_time <- Sys.time()
  # relevant_colnames <- c("uuid","loop_index","name", "ref.name","full.label","ref.type",
                         # "choices.label", values_from)


  # extract unique responses from the source dataframe
  responses <- responses %>% mutate(resp_lower = stringr::str_to_lower(!!sym(values_from)))
   
  input_vec <- responses %>% distinct(resp_lower) %>% pull(resp_lower)
  # cleaning up html leftovers:
  input_vec <- gsub("&#39;", "'", input_vec)
  # counts characters which will be translated
  char_counter <- sum(stringr::str_length(input_vec))
  # TODO: pause here, print the char_counter, and ask the user if the translation should go ahead
  if (char_counter > threshold){
    yes_no <- svDialogs::dlgInput(paste0("The number of characters exceeds ", threshold, ". Please enter [YES] if you would like to proceed or [NO] to kill:"), "YES or NO")$res
  } else{
    yes_no <- "YES"
  }
  batching <- svDialogs::dlgInput(paste0("How many batches would you like to split your translation (",char_counter," characters)? (please only integer)"), 0)$res
  batching <- as.numeric(batching)
  if(yes_no == "YES"){
    if(length(input_vec) > 0){
      for (code in language_codes) {
        col_name <- paste0("response.",target_lang, ".from.",code)
        # relevant_colnames <- append(relevant_colnames, col_name)  # this line may be bugged??
        
        temp_resp <- tibble(input_vec)
        temp_resp[[col_name]] <- NA
        temp_resp <-  temp_resp[sample(1:nrow(temp_resp)),]
        ## create batches
        temp_resp_batches <- split(temp_resp, factor(sort(rank(row.names(temp_resp))%%batching)))
        progress.bar.title <- as.character(Sys.time())
        pb <- tcltk::tkProgressBar(progress.bar.title, "Number of batches executed", 0, batching, 0, width = 600)
        prog <- 1
        for (temp_resp_batch in temp_resp_batches){
          tcltk::setTkProgressBar(pb, prog, progress.bar.title, paste0("Number of batches executed: ", prog, " of ", batching,"\n",length(temp_resp_batch$input_vec)," responses will be translated from ",code," to ",target_lang, "\nThis means ",sum(stringr::str_length(temp_resp_batch$input_vec))," utf-8 characters."))
          prog <- prog + 1
          # cat(length(temp_resp_batch$input_vec),"responses will be translated from",code,"to",target_lang, "\tThis means",sum(stringr::str_length(temp_resp_batch$input_vec)),"utf-8 characters.\n")
          # actual translation:
          result_vec <- NULL
          result_vec <- try(translateR::translate(content.vec = temp_resp_batch$input_vec,
                              microsoft.api.key = source("resources/microsoft.api.key_publichealth.R")$value,
                              microsoft.api.region = "switzerlandnorth",
                              source.lang = code, target.lang = target_lang))
          if(inherits(result_vec,"try-error")) break
          # checking the results
          info_df <- rbind(info_df, data.frame(## DEBUGG IT HERE
            "input_responses_num" = length(temp_resp_batch$input_vec),
            "translated_characters_num" = sum(stringr::str_length(temp_resp_batch$input_vec)),
            "language_from" = code,
            "result_num" = length(result_vec),
            "time_elapsed" = as.numeric(Sys.time() - start_time),
            "date"=Sys.Date(),
            "status"=NA))
          if(is.null(result_vec)){
            warning("Error while translating responses: result_vec is NULL\n")
            info_df$status <- "error"
          }else{
            temp_resp_batch[[col_name]] <- gsub("&#39;", "'", result_vec)
            if(length(result_vec) == length(temp_resp_batch$input_vec)){ 
              info_df$status <- "success"
              # bind the translated and source dfs
              temp_resp_whole <- rbind(temp_resp_whole,temp_resp_batch)
            }else{
              info_df$status <- "partial success"
            }
          }
        }
        close(pb)
        if("partial success" %in% info_df$status){
          svDialogs::msgBox("translate.responses: finished - PARTIAL SUCCESS?")
        } else{
          svDialogs::msgBox("translate.responses: finished - SUCCESS")
        }
        responses <- responses %>% left_join(temp_resp_whole, by = c("resp_lower" = "input_vec")) 
      }
    }else{
      warning("Nothing to be translated")
    }
  }
  # dump info about the results of translation
  log_filename <- "translate_info.csv"
  if(file.exists(log_filename)) write.table(info_df, file = log_filename, append = T, row.names = F, col.names = F, sep = ',')
  else write.table(info_df, file = log_filename, row.names = F, col.names = T, sep = ',')

  responses <- responses %>% select(-resp_lower)
  return(responses)
}


create.translate.requests <- function(questions.db, responses.j, is.loop = F){

    relevant_colnames <- c("uuid", "loop_index", "name", "ref.name","full.label","ref.type", "choices.label", "today")

      response_cols <- colnames(responses.j)[stringr::str_starts(colnames(responses.j), "response")]
      relevant_colnames <- append(relevant_colnames, response_cols)
      responses.j <- responses.j %>%
          select(any_of(relevant_colnames)) %>%
          relocate(all_of(response_cols), .after = last_col()) %>%
          mutate("TRUE other (provide a better translation if necessary)"=NA,
                 "EXISTING other (copy the exact wording from the options in column choices.label)"=NA,
                 "INVALID other (insert yes or leave blank)"=NA) %>%
          dplyr::arrange(name)
      # if(!is.loop) {
      #     responses.j <- responses.j %>% select(-loop_index)
      #     }

    return(responses.j)
}

#-------------------------------------------------------------------------------
# misc functions for pulling data
#-------------------------------------------------------------------------------

what.country <- function(id){
  #' [useless] Looks up raw.main to find to which country an id belongs to.
  #'
  #' @param id uuid to look up in `raw.main`
  #' @returns a string found in the `country` column of `raw.main`.
  return(raw.main %>% filter(uuid == id) %>% pull(country))
}

pull.raw <- function(uuids = NA, loop_indexes = NA){
    #' Pull records from `raw.main` with the given uuids or loop_index.
    #' 
    #' Either `uuids` or `loop_indexes` must be provided. If pulling by uuid, the dataframe used is `raw.main`.
    #' Otherwise, all of the loop_indexes must belong to the same loop (so all must start with the same string "loop#"),
    #' and the data is pulled from dataframe `raw.loop1`, or `raw.loop2`, etc...
    #' 
    #' @note If both uuids and loop_indexes are provided, only loop indexes will be used! (data is not filtered by the provided uuids)
    #' 
    #' @param uuids Character vector of 
    #' @param loop_indexes Character vector of 
    #' @returns Dataframe: raw.main, or raw.loop1 or raw.loop2, depending on the provided arguments.

    if(all(is.na(loop_indexes))) {
        if(all(is.na(uuids))) stop("Need to provide either uuids, or loop_indexes!")
        uuids <- stringr::str_squish(uuids)
        return(raw.main %>% filter(uuid %in% uuids))
    }
    else{
        loop_indexes <- stringr::str_squish(loop_indexes)
        if (all( stringr::str_starts(loop_indexes,  "loop1")))  return(raw.loop1 %>% filter(loop_index %in% loop_indexes))
        else if(all(stringr::str_starts(loop_indexes,"loop2"))) return(raw.loop2 %>% filter(loop_index %in% loop_indexes))
        # TODO: add additional loops if necessary
        else stop("Referenced loop indexes belong to different loops!")
    }
}


#------------------------------------------------------------------------------------------------------------
# utility operators & other legacy functions
#------------------------------------------------------------------------------------------------------------

# IMPORTANT: THESE OPERATORS HAVE BEEN MOVED TO misc_utils.R
# EVENTUALLY THEY WILL BE REMOVED FROM HERE
# if you will continue using them in the future, add a `source("src/utils/misc_utils.R")` to your init script

"%==%" <- function(a, b) ifelse(!is.na(a), a==b, F)
"%!=%" <- function(a, b) ifelse(!is.na(a), a!=b, F)
"%_<_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)<b, F)
"%_<=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)<=b, F)
"%_>_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)>b, F)
"%_>=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a)>=b, F)
"%_+_%" <- function(a,b) as.numeric(a) + as.numeric(b)
"%==na%" <- function(e1, e2) (e1 == e2 | (is.na(e1) & is.na(e2)))
"%!=na%" <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))


# ------------------------------------------------------------------------------------------
is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

# ------------------------------------------------------------------------------------------
add.to.fu.requests <- function(checks, check.id){
  new.entries <- filter(checks, flag) %>% mutate(check.id=check.id) %>% select(uuid, check.id)
  fu.requests <<- dplyr::arrange(rbind(fu.requests, new.entries), uuid)
}

# ------------------------------------------------------------------------------------------
write_excel_pwd <- function(df, file, password){
  xlsx::write.xlsx2(df, file, row.names=F, password=password)
}

# ------------------------------------------------------------------------------------------
name2label_question <- function(col){
  if (stringr::str_detect(col, "/")) {
    q.name <- stringr::str_split(col, "/")[[1]][1]
    c.name <- paste0(tail(stringr::str_split(col, "/")[[1]], -1), collapse="/")
  } else {
    q.name <- col
    c.name <- NA
  }
  if (q.name %in% tool.survey$name){
    q <- tool.survey[tool.survey$name==q.name,]
    q.label <- q[label_colname]
    if (is.na(q.label) | q$q.type %in% c("note")) q.label <- q.name
    if (!is.na(c.name)){
      q.list_name=ifelse(q$list_name=="NA", NA, q$list_name)
      c.label <- tool.choices[tool.choices$list_name==q.list_name & tool.choices$name==c.name, label_colname]
    } else c.label <- NA
    label <- ifelse(is.na(c.label), q.label, paste0(q.label, "/", c.label))
  } else label <- q.name
  return(label)
}
