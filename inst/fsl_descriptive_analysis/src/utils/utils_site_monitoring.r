# RECODING:
#-------------------------------------------------------------------------------

recode.new.site <- function(raw.data, x, f){
  #' Recode a new site.
  #'
  #' Creates cleaning log entries and assigns new pCodes based on newsite request entry.
  #' Function `f` is used to generate a new pCode and is assumed to not require any arguments.
  #'
  #' @param raw.data Raw dataset containing columns: uuid, site_pCode, site_Name
  #' @param x Entry from a newsite requests file (containing columns uuid and true.v)
  #' @param f Function that returns a character string containing new pCode
  #' @returns Cleaning log entries - dataframe with columns uuid, variable, issue, old and new values.
  
  # recode pcode
  cl <- data.frame(
    uuid=x$uuid, variable="site_pCode", issue="Recoding an identified new site.",
    old.value=pull(raw.data %>% filter(uuid==x$uuid), site_pCode), new.value=f())
  
  # recode name
  old_name <- pull(raw.data %>% filter(uuid==x$uuid), site_Name)
  if(x$true.v %!=na% old_name){
    cl <- rbind(cl, data.frame(
        uuid=x$uuid, variable="site_Name", issue="Recoding an identified new site.",
        old.value=old_name, new.value=x$true.v))
  }
  return(cl)
}

# REQUESTS:
#-------------------------------------------------------------------------------
create.gis.checks <- function(data, cols_to_keep){
  
  gis.check.df <- data %>% filter(site_pCode != "new") %>%
    select(all_of(cols_to_keep)) %>%
    arrange(site_pCode) %>%
    relocate(all_of(colnames(data)[str_starts(colnames(data), "(site_Add?ress)|(admin)")]), .after = last_col()) %>%
    relocate(comments_text, .after = last_col()) %>%
    relocate(site_pCode, uuid) %>%
    mutate("TRUE NEW site (provide a name)"=NA,
           "EXISTING (paste the correct pCode for the site)"=NA,
           "INVALID (provide an explanation)"=NA)
  return(gis.check.df)
}

save.gis.checks <- function(df, wb_name, blue_cols = NULL){
  style.col.blue <- createStyle(fgFill="#CCE5FF", valign="top",
                                border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  style.col.green <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000",
                                 valign="top", fontSize = 10, fontName = "Arial Narrow", wrapText=T)
  style.col.green.bold <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
                                      border="TopBottomLeftRight", borderColour="#000000",
                                      fontSize = 10, fontName = "Arial Narrow", wrapText=T)

  wb <- loadWorkbook("resources/gis_checks_template.xlsx")
  helper_col_present <- "what.to.clean" %in% colnames(df)
  
  if(helper_col_present){
    sheet_names <- levels(factor(df %>% pull(what.to.clean)))
  }else{
    sheet_names <- c("Sheet2")
    df$what.to.clean <- "Sheet2"
  }
  
  for(sheet_name in sheet_names){
    addWorksheet(wb, sheet_name)
    
    writeData(wb = wb, x = df %>% filter(what.to.clean == sheet_name),
              sheet = sheet_name, startRow = 1)
    setColWidths(wb, sheet_name, cols = 1:(ncol(df)-4), widths = "auto")
    setColWidths(wb, sheet_name, cols = (ncol(df)-4):(ncol(df)), widths = 35)
    i <- grep("comments", colnames(df))
    setColWidths(wb, sheet_name, cols = i, widths = 60)
    addStyle(wb, sheet_name, style = createStyle(wrapText=T, valign="top", fontSize = 10, fontName = "Arial Narrow"),
             rows = 1:(nrow(df)+1), cols=i, stack = T)
    for (col in blue_cols) {
      i <- grep(paste0('^',col,'$'), colnames(df))
      if(length(i) == 0) stop(paste(col,"not found in df!"))
      addStyle(wb, sheet_name, style = style.col.blue, rows = 1:(nrow(df)+1), cols = i, stack = T)
    }
    addStyle(wb, sheet_name, style = createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df), stack = T)
    i <- ifelse(helper_col_present, -1, 0)
    addStyle(wb, sheet_name, style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2+i, stack = T)
    addStyle(wb, sheet_name, style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1+i, stack = T)
    addStyle(wb, sheet_name, style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)  +i, stack = T)
    addStyle(wb, sheet_name, style.col.green.bold, rows = 1, cols = ncol(df)-2+i, stack = T)
    addStyle(wb, sheet_name, style.col.green.bold, rows = 1, cols = ncol(df)-1+i, stack = T)
    addStyle(wb, sheet_name, style.col.green.bold, rows = 1, cols = ncol(df)  +i, stack = T)
  }
  filename <- paste0(dir.requests, wb_name, ".xlsx")
  saveWorkbook(wb, filename, overwrite=TRUE)

}

#-------------------------------------------------------------------------------
create.newsite.requests <- function(data, cols_to_keep){

    tryCatch({
        new.sites.df <- data %>% filter(site_pCode == "new") %>%
           arrange(`_index`) %>%
           select(all_of(cols_to_keep)) %>%
           relocate(all_of(colnames(data)[str_starts(colnames(data), "(site_Add?ress)|(admin)")]), .after = last_col()) %>%
           relocate(comments_text, .after = last_col()) %>%
           mutate("TRUE NEW site (provide a better name if necessary)"=NA,
                    "EXISTING (paste the correct pCode for the site)"=NA,
                    "INVALID (provide an explanation)"=NA) %>%
          relocate(uuid)

    }, error = function(err){
        warning("Error while saving newsite requests\n::",err)
    })

    return(new.sites.df)
}

save.newsite.requests <- function(df, wb_name, blue_cols = NULL){

  # style.col.green <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000",
  #                                valign="top", wrapText=T)
  # style.col.green.bold <- createStyle(textDecoration="bold", fgFill="#E5FFCC", valign="top",
  #                                      border="TopBottomLeftRight", borderColour="#000000", wrapText=T)
  # style.col.blue <- createStyle(fgFill="#CCE5FF", valign="top",
  #                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=T)

  wb <- loadWorkbook("resources/newsite_requests_template.xlsx")
  addWorksheet(wb, "Sheet2")
  writeData(wb = wb, x = df, sheet = "Sheet2", startRow = 1)
  setColWidths(wb, "Sheet2", cols = 1:(ncol(df)-4), widths = "auto")
  setColWidths(wb, "Sheet2", cols = (ncol(df)-4):(ncol(df)), widths = 35)
  i <- grep("comments", colnames(df))
  setColWidths(wb, "Sheet2", cols = i, widths = 60)
  addStyle(wb, "Sheet2", style = createStyle(wrapText=T, valign="top", fontSize = 10, fontName = "Arial Narrow"),
            rows = 1:(nrow(df)+1), cols=i, stack = T)
  for (col in blue_cols) {
     i <- grep(paste0('^',col,'$'), colnames(df))
     if(length(i) == 0) {
       warning(paste(col,"not found in df!"))
       next
     }
     addStyle(wb, "Sheet2", style = style.col.blue, rows = 1:(nrow(df)+1), cols = i, stack = T)
  }
  addStyle(wb, "Sheet2", style = createStyle(textDecoration="bold"), rows = 1, cols=1:ncol(df), stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style = style.col.green, rows = 1:(nrow(df)+1), cols = ncol(df), stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-2, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df)-1, stack = T)
  addStyle(wb, "Sheet2", style.col.green.bold, rows = 1, cols = ncol(df), stack = T)

  filename <- paste0("output/checking/requests/", wb_name, ".xlsx")
  saveWorkbook(wb, filename, overwrite=TRUE)
}

#-------------------------------------------------------------------------------
save.pduplicates.requests <- function(df, wb_name, pcode_col = "site_pCode"){
  
  use_color <- function(vec, i){
    return(vec[i]==vec[i-1])
  }
  wb <- createWorkbook(creator = "reach")
  addWorksheet(wb, "Sheet1", zoom = 90)
  writeDataTable(wb, "Sheet1", df)
  
  setColWidths(wb, "Sheet1", cols=1:ncol(df), widths="auto")
  for(r in 2:nrow(df)){
    random_color <- ""
    if(use_color(df[[pcode_col]], r)){
      if (random_color == "") {
        random_color <- randomColor(1, luminosity = "light")
        for(i in 1:ncol(df)){
          addStyle(wb, "Sheet1", style = createStyle(fgFill=random_color, wrapText=T),
                 rows = r:(r+1), cols = i)}
        
      }
  } else random_color=""
  }
  filename <- paste0("output/checking/requests/MSL_pduplicates/", wb_name)
  saveWorkbook(wb, filename, overwrite = TRUE)
  
}