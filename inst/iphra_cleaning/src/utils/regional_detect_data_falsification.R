convertColTypes <- function(data, tool.survey){
  #' converts columns based on 'type' column from tool.survey
  #' possibly deprecated - utils_analysis already contains a function for that (convert.col.type)
  
  
  # select_multiple: numeric or factor?
  col.types <- data.frame(column=colnames(data)) %>% 
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>% 
    mutate(type.edited = case_when(
      type %in% c("integer", "decimal", "calculate") ~ "numeric",
      str_starts(type, "select_") ~ "factor",
      str_detect(column, "/") ~ "factor",
      TRUE ~ "text"))
  
  cols <- col.types[col.types$type.edited=="numeric", "column"]
  data[,cols] <- lapply(data[,cols], as.numeric)
  cols <- col.types[col.types$type.edited=="text", "column"]
  data[,cols] <- lapply(data[,cols], as.character)
  cols <- col.types[col.types$type.edited=="factor", "column"]
  data[,cols] <- lapply(data[,cols], as.factor)
  
  return(data)
}
