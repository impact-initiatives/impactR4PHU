###--------------------------------------------------------------------------------------------------------------
### Styling of the tabular analysis
###--------------------------------------------------------------------------------------------------------------

# add table to HTML
# add table to HTML
subch <- function(g, fig_height=7, fig_width=5) {
  g_deparsed <- paste0(deparse(function() {g}), collapse = '')
  sub_chunk <- paste0("\n\n<center>\n", "```{r sub_chunk_", as.numeric(Sys.time())*1000, 
                      ", fig.height=", fig_height, ", fig.width=", fig_width, ", echo=FALSE}\n(", 
                      g_deparsed, ")()\n```", "\n</center>")
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}

# add section to HTML
add_to_html.section <- function(name) cat(paste0("\n\n## ", name," {.tabset}"))

# add title to HTML
add_to_html.title <- function(title){
  cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",4), collapse=""), " ", "<strong>",title,"</strong>"))
}

# add subtitle to HTML
add_to_html.sub_title <- function(disaggregate.variable){
  if (is.na(disaggregate.variable)) {
    cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",5), collapse=""), " No disaggregation"))
  } else{
    cat(paste0(paste0(rep("\n",2), collapse=""), paste0(rep("#",5), collapse=""), " Disaggregated by ",
               disaggregate.variable))
  }
}
#--------------------------------------------------------------------------------------------------------------
# list to add to datatable to style the table
tableFormat <-list(
  dom = 'T<"clear">lfrtip',
  scrollX = TRUE)

# jsFunc <- "(function(value){
#   // find a number preceeded by an open parenthesis with an optional minus sign
#   var matches = value.replace('%','');
#   // ignore values which do not match our pattern, returning white as the background color
#   if(!matches || matches.length < 2) { 
#     return 'white'; 
#   }
#   // attempt to convert the match we found into a number
#   var int = parseInt(matches[1]); 
#   // if we can't ignore it and return a white color
#   if(isNaN(int)) { 
#     return 'white';
#   } 
#   // if the value is negative, return red
#   if(int < 50) { 
#     return 'red' 
#   }
#   // otherwise, by default, return green
#   return 'green';
# })(value)"
#TO DEBUGGG
# function to produce HTML table

sm_ccols_to_choices <- function(ccols){
  #' small utility to split names of select_multiple choice columns into choice names
  stringr::str_split(ccols %>% stringr::str_remove_all("_prop$"), "___", simplify = T)[,2]
}

make_table.select_one <- function(srvyr.design.grouped, entry, add_total = FALSE) {
  
  disagg_vars <- c(entry$admin, entry$disaggregate.variables)
  disagg_vars <- disagg_vars[disagg_vars %in% (srvyr.design.grouped %>% variable.names)]
  
  # make a long table:
  res.long <- srvyr.design.grouped %>% 
    group_by(!!sym(entry$variable), .add = T) %>% 
    # num_samples here is the actual number of responses for each option in each group
    summarise(num_samples = n(),
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))
  
  # widen the table:
  res.wide <- res.long %>% tidyr::pivot_wider(id_cols = any_of(disagg_vars),
                                       names_from = !!sym(entry$variable), values_from = c(num_samples, prop),
                                       values_fill = 0) %>% 
    # calculate total num_samples per group, instead of number of responses for each option
    mutate(num_samples = rowSums(across(starts_with("num_samples_")), na.rm = T), .before = starts_with("prop_")) %>% 
    # remove and rename columns
    select(-starts_with("num_samples_")) %>% rename_with(~stringr::str_remove(., "prop_"), starts_with("prop_"))
  
  if(add_total){
    # calculate total and bind it to res
    res.total <- make_table.select_one(srvyr.design.grouped %>% ungroup %>% 
                                         select(!!sym(entry$admin), !!sym(entry$variable)) %>% 
                                         group_by(!!sym(entry$admin)), entry)
    res.wide <- res.wide %>% bind_rows(res.total)
  }
  
  return(res.wide)
}

make_table.select_multiple <- function(srvyr.design.grouped, entry, add_total = FALSE){
  
  disagg_vars <- c(entry$admin, entry$disaggregate.variables)
  if(any(isna(disagg_vars))) disagg_vars <- NULL
  disagg_vars <- disagg_vars[disagg_vars %in% (srvyr.design.grouped %>% variable.names)]
  
  # calculate the totals and percentages, then join them together
  s_props <- srvyr.design.grouped %>% 
    summarise(across(.fns = list(prop = ~ srvyr::survey_mean(., na.rm = T, vartype = "var"))))
  
  s_samples <- srvyr.design.grouped %>% 
    summarise(num_samples = n())
  
  res <- s_samples %>% left_join(s_props, by = disagg_vars)
  
  if(add_total){
    # calculate total and bind it to res
    total <- srvyr.design.grouped %>% ungroup %>% group_by(!!sym(entry$admin)) %>% select(contains("___")) %>%
      summarise(across(.fns = list(prop = ~ srvyr::survey_mean(., na.rm = T, vartype = "var"))), .groups = "drop") 
    
    total_samples <- s_samples %>% group_by(!!sym(entry$admin)) %>% summarise(num_samples = sum(num_samples))
    
    res <- res %>% bind_rows(total_samples %>% left_join(total, by = entry$admin))
  }
  # convert choice names to labels (all except the NA column)
  res <- res %>% rename_with(~get.choice.label(sm_ccols_to_choices(.), entry$list_name, simplify = T),
                             ends_with("_prop") & !contains("___NA"))
  # also convert the NA column:
  if(!entry$omit_na) res <- res %>% rename("NA" = !!paste0(entry$variable, "___NA_prop"))
  
  return(res)
}

make_table <- function(srvyr.design, entry, disagg.var){
  # grouping - if no disaggregations, then simply by admin
  # (the design is already grouped by admin, so another group_by will not do anything)
  if(isna(disagg.var)) disagg.var <- entry$admin   
  srvyr.design.grouped <- srvyr.design %>% group_by(!!sym(disagg.var), .add = T)
  
  # select the relevant columns / variables
  srvyr.design.grouped <- switch (entry$func,
                                  select_multiple = { srvyr.design.grouped %>% select(starts_with(paste0(entry$variable,"___"))) },
                                  # default - just select the relevant variable:
                                  { srvyr.design.grouped %>% select(!!sym(entry$variable)) }
  )
  
  # calculate metrics
  res <- switch (entry$func,
                 numeric =    { srvyr.design.grouped %>% 
                     summarise(
                       num_samples = n(),
                       mean  =  srvyr::survey_mean(  !!sym(entry$variable), na.rm = T, vartype = "var"),
                       median = srvyr::survey_median(!!sym(entry$variable), na.rm = T, vartype = "var"),
                       min = min(!!sym(entry$variable), na.rm = T),
                       max = max(!!sym(entry$variable), na.rm = T)) },
                 select_one = { srvyr.design.grouped  %>%
                     make_table.select_one(entry, add_total = entry$add_total & disagg.var != entry$admin) },
                 select_multiple = { srvyr.design.grouped %>%
                     make_table.select_multiple(entry, add_total = entry$add_total & disagg.var != entry$admin) }
  )
  
  ##### cleaning up the res #####
  
  # remove the variance columns, filter out choices with n = 0
  res <- res %>% select(-ends_with("_var")) 
  if("num_samples" %in% names(res)) res <- res %>% filter(num_samples > 0)
  
  # round & convert to percentages:
  res <- switch (entry$func,
                 numeric = res %>% mutate(across(where(is.numeric), ~round(., 2))), 
                 # default (select_one or multiple): convert everything except num_samples to percent
                 { res %>% mutate(across(where(is.numeric) & !matches("^num_samples$"), as_perc)) %>% 
                     # add label for the TOTAL row
                     mutate(!!disagg.var := tidyr::replace_na(!!sym(disagg.var) %>% as.character, "<TOTAL>"))  }
  )
  return(res)
}
