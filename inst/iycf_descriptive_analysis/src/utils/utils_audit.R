load.audit.files <- function(dir.audits, uuids=NULL, track.changes=F){
  #' Returns a dataframe with contents of all `audit.csv` files from `dir.audits` or its subdirectories.

  #' @param dir.audits The directory in which to look for audit files (path resembling .../data/audits/...)
  #' @param uuids The uuids of surveys that are to be loaded. If NULL is provided here (and by default) all uuids from dir.audits will be loaded.
  #' @param track.changes Whether the survey has the parameter track-changes set to `true`

  audit.filenames <- list.files(dir.audits, pattern="audit.csv", recursive=TRUE, full.names=TRUE)
  cat("Loading audit logs from",dir.audits,"...\n")

  counter <- 0
  res <- data.frame()
  for (filename in audit.filenames){
    # get uuid from filename
    sp <- strsplit(filename, "\\/")[[1]]  # could throw an error on Unix?
    uuid <- sp[length(sp)-1]
    if(is.null(uuids) | uuid %in% uuids){
      # load file
      audit <- read_csv(filename, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
        mutate(uuid=uuid, .before=1)
      # TODO: make sure that the below is correctly done (probably not)
      if(track.changes & "old-value" %in% colnames(audit)) {
        audit <- audit %>% rename("old.value" = `old-value`, "new.value" = `new-value`)
      } else {
        audit <- audit %>% mutate(old.value = NA, new.value = NA)
      }
      counter <- counter + 1
      res <- bind_rows(res, audit)
      cat("...")
    }
  }
  if(nrow(res) > 0){
    res <- res  %>%
      mutate(duration=(end-start)/1000,
             group=sapply(stringr::str_split(node, '\\/'), function(x){
               id.group <- ifelse("G_survey" %in% x, 4, 3)
               return(x[id.group])}),
             question=sapply(stringr::str_split(node, '\\/'), function(x){return(x[length(x)])})) %>%
      mutate(event=stringr::str_replace_all(event, " ", "."))
    cat("\n...Done\n")
    cat(paste("Loaded", counter, "audit logs.\n"))
  }else{
    warning("No relevant audit logs found!")
  }
  return(res)
}


process.uuid <- function(df){
  # df <- audits
  max.num.iterations <- 15
  t <- list() # Time of each iteration
  rt <- list() # response time of each iteration 
  j <- list() # number of jumps
  q <- list() # number of questions
  w <- list() # waiting time
  e <- list() # number of edits
  t1 <- df$start[1]
  if (df$event[1]!="form.start") stop("First event is not form.start?!")
  status <- "filling"
  for (r in 2:nrow(df)){
    if (status=="filling" & df$event[r]=="form.exit"){
      t2 <- df$start[r]
      t <- append(t, (t2-t1)/1000/60)
      sub.df <- filter(df, start>=t1 & start<=t2)
      questions <- filter(sub.df, event %in% c("question", "group.questions"))
      rt <- append(rt, sum(questions$duration)/60)
      q <- append(q, length(unique(questions$node)))
      j <- append(j, sum(sub.df$event=="jump"))
      e <- append(e, nrow(filter(questions, !is.na(`old.value`) & `old.value`!="")))
      status <- "waiting"
    } else if (status=="waiting" & df$event[r]=="form.resume"){
      t1 <- df$start[r]
      w <- append(w, (t1-t2)/1000/60)
      status <- "filling"
    } else if (status=="waiting" & df$event[r]=="form.exit"){
      if("uuid2" %in% colnames(df)) warning(paste("status=waiting while form.exit! uuid:",df$uuid2[r]))
      else warning("status=waiting while form.exit!")
    } 
  }
  res <- data.frame()
  res[1, "n.iteration"] <- length(t)
  res[1, "tot.t"] <- round((df[nrow(df), "start"] - df[1, "start"])/60000, 1)
  res[1, "tot.rt"] <- round(sum(filter(df, event %in% c("question", "group.questions"))$duration)/60, 1)
  for (i in 1:max.num.iterations){
    res[1, c(paste0("t", i), paste0("rt", i), 
             paste0("q", i), paste0("j", i),
             paste0("e", i),
             paste0("w", i))] <- NA
  }
  if (length(t)==0) stop()
  else{
    for (i in 1:min(length(t), max.num.iterations)){
      res[1, paste0("t", i)] <- round(t[[i]], 1)
      res[1, paste0("rt", i)] <- round(rt[[i]], 1)
      res[1, paste0("q", i)] <- q[[i]]
      res[1, paste0("j", i)] <- j[[i]]
      res[1, paste0("e", i)] <- e[[i]]
    }
  }
  if (length(w)>0){
    for (i in 1:min(length(w), max.num.iterations)) res[1, paste0("w", i)] <- round(w[[i]], 1)
  }
  if("uuid2" %in% colnames(res)) res <- res %>% select(-uuid2)
  
  # new functionality :)
  # res <- res %>%  discard(~any_of(is.na(.)))  # dropping empty columns (all NA)
  return(res)
}


find.similar.surveys <- function(data.main, tool.survey, uuid="_uuid", staff_name_col="Staff_Name", idnk_value="idnk"){
#' for each survey, it finds the closest matching survey with the minimum number of different columns
#'
#' @param uuid Name of the column in which uuids are stored.
#' @param staff_name_col Name of the column in which enumerator's name (or identifier etc.) is stored
#' @param idnk_value Value (from tool.choices) that represents the answer "I don't know"

  data <- data.main

  # 1) store UUIDs
  uuids <- data[[uuid]]

  # 2) convert all columns to character and tolower
  data <- mutate_all(data, as.character)
  data <- mutate_all(data, tolower)

  # 3) remove columns that are naturally different in each survey:
  # - columns of type = "start", "end", etc.
  # - columns starting with "_"
  # - option columns for the select multiple -> keeping only the concatenation column
  types_to_remove <- c("start", "end", "today", "deviceid", "date", "geopoint", "audit",
                       "note", "calculate")
  cols_to_keep <- data.frame(column=colnames(data)) %>%
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>%
    filter(column==staff_name_col | (!(type %in% types_to_remove) &
             !stringr::str_starts(column, "_") & !stringr::str_detect(column, "/") & !stringr::str_ends(column, "_other")))
  data <- data[, all_of(cols_to_keep$column)]

  # 4) remove columns with all NA; convert remaining NA to "NA"; convert all columns to factor
  data <- data[, colSums(is.na(data))<nrow(data)]
  data[is.na(data)] <- "NA"
  data <- data %>% mutate_if(is.character, factor)
  error.message <- "NAs detected, remove them before proceeding (it can happen when converting to factor)"
  if (sum(is.na(data))>0) stop(error.message)

  # 5) calculate gower distance
  gower_dist <- cluster::daisy(data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
  gower_mat <- as.matrix(gower_dist)

  # 6) convert distance to number of differences and determine closest matching survey
  r <- unlist(lapply(1:nrow(data), function(i){
    srv1 <- sort(gower_mat[i,]*ncol(data))[1]
    srv2 <- sort(gower_mat[i,]*ncol(data))[2]
    if (names(srv1)==as.character(i)) return(srv2)
    else return(srv1)
  }))

  # 7) add relevant columns
  outdata <- data.main[, all_of(cols_to_keep$column)]
  outdata[["num_cols_not_NA"]] <- rowSums(data!="NA")
  outdata[[paste0("num_cols_", idnk_value)]] <- rowSums(data==idnk_value)
  outdata[[uuid]] <- uuids
  outdata[["_id_most_similar_survey"]] <- uuids[as.numeric(names(r))]
  outdata[["number_different_columns"]] <- as.numeric(r)
  outdata <- outdata %>% arrange(number_different_columns, !!sym(uuid))

  return(outdata)
}

check.soft.duplicates <- function(data.main, ids, uuid="_uuid", only_differences=F){
  data <- data.main
  check <- data %>% filter(!!sym(uuid) %in% ids) %>% t() %>% as.data.frame()
  check$num.unique <- unlist(lapply(1:nrow(check), function(r)
    length(unique(as.character(check[r, all_of(colnames(check))])))))
  check <- check[!(rownames(check) %in% "_index"),]
  if (only_differences){
    check <- check %>% filter(num.unique!=1) %>% select(-num.unique)
  } else{
    check <- check %>% arrange(-num.unique)
  }
  return(check)
}

# silhouette analysis based on gower distance between surveys
# METHOD: check for anomalies using the silhouette function. We assume the dataset is clustered using the
# enumerator IDs as the cluster IDs and we calculate the silhouette for this clustering scenario. A
# silhouette value close to 1 indicates that the entries of the cluster are very similar to each other and
# very dissimilar from entries of other clusters. Thus, we need to raise a flag if the silhouette value gets
# close to 1 for any of the clusters/enumerators.
# https://en.wikipedia.org/wiki/Silhouette_(clustering)
# https://dpmartin42.github.io/posts/r/cluster-mixed-types
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9
calculate.enumerator.similarity <- function(data, tool.survey, col_enum, col_admin="adm"){
  # convert columns using the tool
  data <- convertColTypes(data, tool.survey)
  # keep only relevant columns
  cols <- data.frame(column=colnames(data)) %>%
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>%
    filter(!(type %in% c("date", "start", "end", "today",
                         "audit", "note", "calculate", "deviceid", "geopoint")) &
             !stringr::str_starts(column, "_"))
  # convert character columns to factor and add enum.id
  data <- data[, all_of(cols$column)] %>%
    mutate_if(is.character, factor) %>%
    arrange(!!sym(col_enum)) %>%
    mutate(enum.id=as.numeric(!!sym(col_enum)), .after=!!sym(col_enum))
  # add "SY" column in case col_admin is not specified
  if (col_admin=="adm") data <- mutate(data, adm="SY", .before=cols$column[1])
  # calculate similarity (for enumerators who completed at least 5 surveys)
  res <- data %>% split(data[[col_admin]]) %>%
    lapply(function(gov){
      df <- gov %>%
        group_by(enum.id) %>% mutate(n=n()) %>% filter(n>=5) %>% ungroup() %>%
        select_if(function(x) any(!is.na(x)))
      if (length(unique(df$enum.id)) > 1){
        # calculate gower distance
        gower_dist <- daisy(select(df, -c(!!sym(col_enum), enum.id)),
                            metric = "gower", warnBin = F, warnAsym = F, warnConst = F)
        # gower_mat <- as.matrix(gower_dist)
        # calculate silhouette
        si <- silhouette(df$enum.id, gower_dist)
        res.si <- summary(si)
        # create output
        r <- data.frame(enum.id=as.numeric(names(res.si$clus.avg.widths)), si=res.si$clus.avg.widths) %>%
          left_join(distinct(select(df, !!sym(col_admin), !!sym(col_enum), enum.id)), by="enum.id") %>%
          left_join(group_by(df, enum.id) %>% summarise(num.surveys=n(), .groups="drop_last"), by="enum.id") %>%
          select(!!sym(col_admin), !!sym(col_enum), num.surveys, si) %>% arrange(-si)
        return(r)}})
  return(do.call(rbind, res))
}

#-------------------------------------------------------------------------------
# FUNCTIONS FOR OUTPUTTING ENUM PERFORMANCE ANALYSES
#-------------------------------------------------------------------------------

create.count_deleted_enu <- function(deletion.log, col_enum)  {
    #' was previously named create.count_enu

  count_del <- deletion.log %>%
    group_by(!!sym(col_enum)) %>%
    summarize(count = n()) %>%
      arrange(col_enum)

  count_del_reas <- deletion.log %>%
    group_by(!!sym(col_enum),reason) %>%
    summarize(count = n(), .groups = "keep") %>%
      arrange(col_enum)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "dlog entries")
  openxlsx::addWorksheet(wb, "dlog reasons")
  openxlsx::writeData(wb = wb, x = count_del, sheet = "dlog entries", startRow = 1)
  openxlsx::writeData(wb = wb, x = count_del_reas, sheet = "dlog reasons", startRow = 1)
  filename <- paste0("output/enum_performance/", "count_deleted_enu", ".xlsx")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)


}

create.count_collected_enu <- function(kobo.raw, col_enum)  {
    #' Create an analysis of the number of surveys conducted per enumerator.
    #'
    #' The output spreadsheet is saved to a file named 'count_collected_enu.xlsx in directory 'output/enum_performance'
    #'
    #' @param kobo.raw This dataframe should contain the raw data, as it was exported from Kobo.
    #' @param col_enum The name of the column which contains the enumerator's ID.

  count_del <- kobo.raw %>%
    group_by(!!sym(col_enum)) %>%
    summarize(count = n()) %>%
      arrange(col_enum)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb = wb, x = count_del, sheet = "Sheet1", startRow = 1)
  filename <- paste0("output/enum_performance/", "count_collected_enu", ".xlsx")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)

}

create.count_enu_cleaning <- function(cleaning.log, col_enum)  {
    #' Create an analysis of the numbers of cleaning log entries per enumerator.
    #'
    #' The output spreadsheet is saved to a file named 'count_enu_cleaning.xlsx in directory 'output/enum_performance'
    #' The first sheet named "clog entries" has the numbers of entires per enumerator
    #' The second sheet named "clog reasons" includes grouping by reason too.
    #'
    #' @param cleaning.log The entire cleaning.log, containing the column `col_enum`
    #' @param col_enum The name of the column which contains the enumerator's ID.
    #'
  count_cl_entries <- cleaning.log %>%
    group_by(!!sym(col_enum)) %>%
    summarize(count = n()) %>%
      arrange(col_enum)

  count_cl_entries_reas <- cleaning.log %>%
    group_by(!!sym(col_enum), issue) %>%
    summarize(count = n(), .groups = "keep") %>%
      arrange(col_enum)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "clog entries")
  openxlsx::addWorksheet(wb, "clog reasons")
  openxlsx::writeData(wb = wb, x = count_cl_entries, sheet = "clog entries", startRow = 1)
  openxlsx::writeData(wb = wb, x = count_cl_entries_reas, sheet = "clog reasons", startRow = 1)
  filename <- paste0("output/enum_performance/", "count_enu_cleaning", ".xlsx")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)

}
