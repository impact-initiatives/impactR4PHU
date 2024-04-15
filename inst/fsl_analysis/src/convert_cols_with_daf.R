# this is the good old convert_cols_with_daf function, but not as a function anymore :)

# read DAF by each datasheet
for(sheet in names(data.list)){
  converted <- c()
  # filter the daf using the datasheet
  dafp <- daf %>% filter(variable %in% colnames(data.list[[sheet]]))
  if(nrow(dafp) == 0){
    cat("\nNo variables from sheet '",sheet,"' are in your DAF.", sep = "")
    next
  }
  for(r in 1:nrow(dafp)){
    entry <- load_entry(dafp[r,])
    col <- entry$variable
    
    if(!all(is.na(entry$disaggregate.variables))){
      for(disagg.var in entry$disaggregate.variables){
        if(!disagg.var %in% colnames(data.list[[sheet]])){
          # disagg.var was not found, but maybe it's located in main? let's try to fix!
          if(sheet == "main") stop("Disaggregation variable ", disagg.var, " was not found in main!!\n")
          if(disagg.var %in% colnames(data.list$main)){
            # cat("... Disaggregation variable", disagg.var,"was not found in sheet",sheet,"but it exists in main. Will attempt to simply left_join by uuid... ")
            join_attempt <- data.list()$main %>% dplyr::select(uuid, !!sym(disagg.var))
            data.list[[sheet]] <- data.list[[sheet]] %>% left_join(join_attempt, by = "uuid")
            # cat(" success!")
          }else stop(paste("Disaggregation variable", disagg.var, "not found in sheet",sheet,"nor in main!!\n"))
        }
        data.list[[sheet]][[disagg.var]] <- as.character(data.list[[sheet]][[disagg.var]])  # as character, not factor!
      }
    }
    
    if(col %in% converted) next
    cat("\nConverting column:",col," ...")
    
    if(entry$func == "select_multiple"){
      # not converting to label here. instead just replace "/" with "___" and convert to numeric
      choice_cols <- colnames(data.list[[sheet]])[colnames(data.list[[sheet]]) %>% stringr::str_starts(paste0(col, "/"))]
      data.list[[sheet]] <- data.list[[sheet]] %>%
        mutate(across(all_of(choice_cols), as.numeric)) %>%
        rename_with(~stringr::str_replace(., "/", "___"), choice_cols)
      if(!entry$omit_na){
        # change NAs from all other choice columns to 0
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(across(starts_with(paste0(col,"___")), ~replace_na(., 0)))
        # create a new NA column
        na_colname <- paste0(col,"___NA")
        data.list[[sheet]][[na_colname]] <- is.na(data.list[[sheet]][[col]]) %>% as.numeric
        data.list[[sheet]] <- data.list[[sheet]] %>% relocate(all_of(na_colname), .after = !!sym(col))
      }
    }else {
      if(entry$func == "select_one") {
        # try to convert to label:
        choice_names <- tool.choices %>% filter(list_name == entry$list_name) %>% pull(name)
        not_in_choices <- data.list[[sheet]] %>% filter(!(!!sym(col) %in% choice_names) & !isna(!!sym(col))) %>%
          pull(!!sym(col)) %>% unique
        if(length(not_in_choices) > 0){
          conv_vec <- data.list[[sheet]][[col]]
        }else{
          if(!entry$list_name %in% tool.choices$list_name) stop(paste("list",entry$list_name, "not found in tool.choices!"))
          
          res <- data.frame(name = unlist(data.list[[sheet]][[col]])) %>%
            left_join(dplyr::select(tool.choices, name, list_name, label_colname) %>% filter(list_name == entry$list_name),
                      by = "name", na_matches = "never")
          if(nrow(res) == 0) stop("All choices not in the list!")
          
          conv_vec <- pull(res, label_colname)
        }
        if(entry$omit_na) {
          data.list[[sheet]][[col]] <- factor(conv_vec, exclude = NA)
        }else{
          data.list[[sheet]][[col]] <- factor(conv_vec, exclude = NULL)
        }
        rm(conv_vec, choice_names, not_in_choices)
      }
      else if(entry$func %in% c("mean", "median", "integer", "numeric","decimal")) data.list[[sheet]][[col]] <- as.numeric(data.list[[sheet]][[col]])
    }
    ## Deal with disaggregated change to Label
    if(is.na(entry$disaggregate.variables)){
    } else {
     disag.col <- entry$disaggregate.variables
      type.disag.col <- tool.survey %>% 
        filter(name == disag.col) %>% pull(q.type)
      list_name_disag <- tool.survey %>%
        filter(name == disag.col) %>% pull(list_name)
      if(type.disag.col == "select_multiple"){
        data.list[[sheet]][[disag.col]] <- xml2label_choices_multiple(tool.survey,tool.choices,label_colname,data.list[[sheet]],disag.col)
      }else {
        if(type.disag.col == "select_one") {
          # try to convert to label:
          choice_names <- tool.choices %>% filter(list_name == list_name_disag) %>% pull(name)
          not_in_choices <- data.list[[sheet]] %>% filter(!(!!sym(disag.col) %in% choice_names)) %>%
            pull(!!sym(disag.col)) %>% unique %>% na.omit()
          if(length(not_in_choices) > 0){
            conv_vec <- data.list[[sheet]][[disag.col]]
          }else{
            if(!list_name_disag %in% tool.choices$list_name) stop(paste("list",list_name_disag, "not found in tool.choices!"))
            
            res <- data.frame(name = unlist(data.list[[sheet]][[disag.col]])) %>%
              left_join(dplyr::select(tool.choices, name, list_name, label_colname) %>% filter(list_name == list_name_disag),
                        by = "name", na_matches = "never")
            if(nrow(res) == 0) stop("All choices not in the list!")
            
            conv_vec <- pull(res, label_colname)
          }
          if(entry$omit_na) {
            data.list[[sheet]][[disag.col]] <- factor(conv_vec, exclude = NA)
          }else{
            data.list[[sheet]][[disag.col]] <- factor(conv_vec, exclude = NULL)
          }
          rm(conv_vec, choice_names, not_in_choices)
        }
        else if(type.disag.col %in% c("mean", "median", "integer", "numeric","decimal")) data.list[[sheet]][[disag.col]] <- as.numeric(data.list[[sheet]][[disag.col]])
        }
      }
    converted <- append(converted, col)
    }
  cat("... done.\n")
  rm(dafp)
}

cat("\nAll conversions done!\n")
