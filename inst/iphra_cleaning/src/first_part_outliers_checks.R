source("src/init.R")
options(warn=-1)
#############################################################################################################
# 5) Outliers
#############################################################################################################
n.sd <- as.numeric(strings['n_sd'])
res.outliers_all <- data.frame()
cleaning.log.outliers <- data.frame()
# define columns to check for outliers

cols.integer_main <- filter(tool.survey, type %in% c("integer","decimal"))
cols.integer_main <- cols.integer_main %>% 
  filter(!name %in% c("num_died", "num_hh", "wash_num_containers"))
cols.integer_raw.main <- cols.integer_main[cols.integer_main$name %in% colnames(raw.main),] %>% pull(name)
cols.integer_raw.main <- cols.integer_raw.main[!stringr::str_detect(cols.integer_raw.main,"fsl_fcs_|fsl_rcsi_")]
cols.integer_raw.water_count_loop <- cols.integer_main[cols.integer_main$name %in% colnames(raw.water_count_loop),] %>% pull(name)
cols.integer_raw.child_nutrition <- cols.integer_main[cols.integer_main$name %in% colnames(raw.child_nutrition),] %>% pull(name)
if(!is.null(raw.women)){
  cols.integer_raw.women <- cols.integer_main[cols.integer_main$name %in% colnames(raw.women),] %>% pull(name)
  res.outliers_raw.women <- data.frame()
  #------------------------------------------------------------------------------------------------------------
  # [women SHEET] -> detect outliers 
  df.all <- data.frame()
  raw.women.outliers <- raw.women %>%
    select("uuid",loop_index, cols.integer_raw.women) %>%
    mutate_at(cols.integer_raw.women, as.numeric)
  
  # Outliers per country
  if(length(cols.integer_raw.women)>0){
    
    for (col in cols.integer_raw.women) {
      values <- raw.women.outliers %>% 
        filter(!!sym(col) %_>_% 0) %>% 
        rename(value=col) %>%  select(uuid,loop_index, value) %>% 
        mutate(value.log=log10(value)) %>%  mutate(variable=col) %>% 
        mutate(is.outlier.lin = (value > mean(value) + n.sd*sd(value)) |
                 (value < mean(value) - n.sd*sd(value)),
               is.outlier.log = (value.log > mean(value.log) + n.sd*sd(value.log)) |
                 (value.log < mean(value.log) - n.sd*sd(value.log)))
      values <- filter(values, is.outlier.log) %>%  select(uuid,loop_index, variable, value)
      if (nrow(values)>0) print(paste0(col, ": ", nrow(values), " outliers detected"))
      res.outliers_raw.women <- rbind(res.outliers_raw.women, values)
    }
    
    f.alpha <- function(x) return(ifelse(x, 1, 0))
    
    # Outliers Boxplot generator per country
    
    df <- raw.women.outliers %>% 
      select(uuid, all_of(cols.integer_raw.women)) %>% 
      tidyr::pivot_longer(-uuid, names_to = "variable", values_to = "value") %>% 
      mutate(value.log = log10(value)) %>% 
      left_join(select(res.outliers_raw.women, -value) %>% mutate(is.outlier=T), by = c("uuid","variable")) %>% 
      mutate(is.outlier = ifelse(is.na(is.outlier), F, is.outlier)) %>% 
      filter(!is.na(value) & value>0)
    df <- tidyr::gather(df, key = "measure", value = "value", variable)
    df.all <- rbind(df.all, df)
    
    
    openxlsx::write.xlsx(df.all, paste0("output/checking/outliers/women_outlier_prices_analysis_", n.sd, "sd.xlsx"), overwrite=T)
    
    # generating prices boxplots for same locations
    g.outliers_women <- ggplot2::ggplot(df.all) +
      ggplot2::geom_boxplot(ggplot2::aes(x= measure, y=value.log), width = 0.2) + ggplot2::ylab("Values (log10)") +
      ggplot2::geom_point(ggplot2::aes(x=measure, y=value.log, group = measure), alpha=f.alpha(df.all$is.outlier), colour="red") +
      ggplot2::facet_wrap(~value, ncol = 4, scales = "free_y")+
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank())
    
    
    # Save
    ggplot2::ggsave(paste0("output/checking/outliers/women_outlier_prices_analysis_", n.sd, "sd.pdf"), g.outliers_women, 
           width = 40, height = 80, units = "cm", device="pdf")
    
    if(nrow(res.outliers_raw.women)>0){
      # Output requests to check
      res.outliers_raw.women <- res.outliers_raw.women %>% 
        mutate(issue = "Outliers",
               loop_index = loop_index,
               new.value = NA,
               explanation=NA) %>% 
        rename("old.value"=value) %>% 
        select(uuid,loop_index,variable,issue,old.value,new.value,explanation)
      
      res.outliers_all <- rbind(res.outliers_all,res.outliers_raw.women)
      
      cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_raw.women)
    }
  }
}

if(!is.null(raw.died_member)){
  cols.integer_raw.died_member <- cols.integer_main[cols.integer_main$name %in% colnames(raw.died_member),] %>% pull(name)
  res.outliers_died_member <- data.frame()
  #------------------------------------------------------------------------------------------------------------
  # [died_member SHEET] -> detect outliers 
  df.all <- data.frame()
  raw.died_member.outliers <- raw.died_member %>%
    select("uuid",loop_index, cols.integer_raw.died_member) %>%
    mutate_at(cols.integer_raw.died_member, as.numeric)
  
  # Outliers per country
  if(length(cols.integer_raw.died_member)>0){
    
    for (col in cols.integer_raw.died_member) {
      values <- raw.died_member.outliers %>% 
        filter(!!sym(col) %_>_% 0) %>% 
        rename(value=col) %>%  select(uuid,loop_index, value) %>% 
        mutate(value.log=log10(value)) %>%  mutate(variable=col) %>% 
        mutate(is.outlier.lin = (value > mean(value) + n.sd*sd(value)) |
                 (value < mean(value) - n.sd*sd(value)),
               is.outlier.log = (value.log > mean(value.log) + n.sd*sd(value.log)) |
                 (value.log < mean(value.log) - n.sd*sd(value.log)))
      values <- filter(values, is.outlier.log) %>%  select(uuid,loop_index, variable, value)
      if (nrow(values)>0) print(paste0(col, ": ", nrow(values), " outliers detected"))
      res.outliers_died_member <- rbind(res.outliers_died_member, values)
    }
    
    f.alpha <- function(x) return(ifelse(x, 1, 0))
    
    # Outliers Boxplot generator per country
    
    df <- raw.died_member.outliers %>% 
      select(uuid, all_of(cols.integer_raw.died_member)) %>% 
      tidyr::pivot_longer(-uuid, names_to = "variable", values_to = "value") %>% 
      mutate(value.log = log10(value)) %>% 
      left_join(select(res.outliers_died_member, -value) %>% mutate(is.outlier=T), by = c("uuid","variable")) %>% 
      mutate(is.outlier = ifelse(is.na(is.outlier), F, is.outlier)) %>% 
      filter(!is.na(value) & value>0)
    df <- tidyr::gather(df, key = "measure", value = "value", variable)
    df.all <- rbind(df.all, df)
    
    
    openxlsx::write.xlsx(df.all, paste0("output/checking/outliers/died_member_outlier_prices_analysis_", n.sd, "sd.xlsx"), overwrite=T)
    
    # generating prices boxplots for same locations
    g.outliers_died_member <- ggplot2::ggplot(df.all) +
      ggplot2::geom_boxplot(ggplot2::aes(x= measure, y=value.log), width = 0.2) + ggplot2::ylab("Values (log10)") +
      ggplot2::geom_point(ggplot2::aes(x=measure, y=value.log, group = measure), alpha=f.alpha(df.all$is.outlier), colour="red") +
      ggplot2::facet_wrap(~value, ncol = 4, scales = "free_y")+
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank())
    
    
    # Save
    ggplot2::ggsave(paste0("output/checking/outliers/died_member_outlier_prices_analysis_", n.sd, "sd.pdf"), g.outliers_died_member, 
           width = 40, height = 80, units = "cm", device="pdf")
    
    if(nrow(res.outliers_died_member)>0){
      # Output requests to check
      res.outliers_died_member <- res.outliers_died_member %>% 
        mutate(issue = "Outliers",
               loop_index = loop_index,
               new.value = NA,
               explanation=NA) %>% 
        rename("old.value"=value) %>% 
        select(uuid,loop_index,variable,issue,old.value,new.value,explanation)
      
      res.outliers_all <- rbind(res.outliers_all,res.outliers_died_member)
      
      cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_died_member)
    }
  }
}

# cols <- filter(tool.survey, stringr::str_starts(name, "G_3")) %>% pull(name)



res.outliers_main <- data.frame()

res.outliers_child_nutrition <- data.frame()
df.all <- data.frame()
#------------------------------------------------------------------------------------------------------------
# [MAIN SHEET] -> detect outliers 

raw.main.outliers <- raw.main %>%
  select("uuid", cols.integer_raw.main) %>%
  mutate_at(cols.integer_raw.main, as.numeric)
if(length(cols.integer_raw.main)>0){
  
  # Outliers per country
  for (col in cols.integer_raw.main) {
    values <- raw.main.outliers %>% 
      filter(!!sym(col) %_>_% 0) %>% 
      rename(value=col) %>%  select(uuid, value) %>% 
      mutate(value.log=log10(value)) %>%  mutate(variable=col) %>% 
      mutate(is.outlier.lin = (value > mean(value) + n.sd*sd(value)) |
               (value < mean(value) - n.sd*sd(value)),
             is.outlier.log = (value.log > mean(value.log) + n.sd*sd(value.log)) |
               (value.log < mean(value.log) - n.sd*sd(value.log)))
    values <- filter(values, is.outlier.log) %>%  select(uuid, variable, value)
    if (nrow(values)>0) print(paste0(col, ": ", nrow(values), " outliers detected"))
    res.outliers_main <- rbind(res.outliers_main, values)
  }
  
  f.alpha <- function(x) return(ifelse(x, 1, 0))
  
  # Outliers Boxplot generator per country
  
  df <- raw.main.outliers %>% 
    select(uuid, all_of(cols.integer_raw.main)) %>% 
    tidyr::pivot_longer(-uuid, names_to = "variable", values_to = "value") %>% 
    mutate(value.log = log10(value)) %>% 
    left_join(select(res.outliers_main, -value) %>% mutate(is.outlier=T), by = c("uuid","variable")) %>% 
    mutate(is.outlier = ifelse(is.na(is.outlier), F, is.outlier)) %>% 
    filter(!is.na(value) & value>0)
  df <- tidyr::gather(df, key = "measure", value = "value", variable)
  df.all <- rbind(df.all, df)
  
  
  openxlsx::write.xlsx(df.all, paste0("output/checking/outliers/main_outlier_prices_analysis_", n.sd, "sd.xlsx"), overwrite=T)
  
  # generating prices boxplots for same locations
  g.outliers_main <- ggplot2::ggplot(df.all) +
    ggplot2::geom_boxplot(ggplot2::aes(x= measure, y=value.log), width = 0.2) + ggplot2::ylab("Values (log10)") +
    ggplot2::geom_point(ggplot2::aes(x=measure, y=value.log, group = measure), alpha=f.alpha(df.all$is.outlier), colour="red") +
    ggplot2::facet_wrap(~value, ncol = 4, scales = "free_y")+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank())
  
  
  # Save
  ggplot2::ggsave(paste0("output/checking/outliers/main_outlier_prices_analysis_", n.sd, "sd.pdf"), g.outliers_main, 
         width = 40, height = 80, units = "cm", device="pdf")
  
  if(nrow(res.outliers_main)>0){
    # Output requests to check
    res.outliers_main <- res.outliers_main %>% 
      mutate(issue = "Outliers",
             loop_index = NA,
             new.value = NA,
             explanation=NA) %>% 
      rename("old.value"=value) %>% 
      select(uuid,loop_index,variable,issue,old.value,new.value,explanation)
    res.outliers_all <- rbind(res.outliers_all,res.outliers_main)
    cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_main)
  }
}
#------------------------------------------------------------------------------------------------------------
if(!is.null(raw.water_count_loop)){
  # [water_count_loop SHEET] -> detect outliers 
  df.all <- data.frame()
  res.outliers_water_count_loop <- data.frame()
  raw.water_count_loop.outliers <- raw.water_count_loop %>%
    select("uuid",loop_index, cols.integer_raw.water_count_loop) %>%
    mutate_at(cols.integer_raw.water_count_loop, as.numeric)
  
  # Outliers per country
  if(length(cols.integer_raw.water_count_loop) > 0){
    for (col in cols.integer_raw.water_count_loop) {
      values <- raw.water_count_loop.outliers %>% 
        filter(!!sym(col) %_>_% 0) %>% 
        rename(value=col) %>%  select(uuid,loop_index, value) %>% 
        mutate(value.log=log10(value)) %>%  mutate(variable=col) %>% 
        mutate(is.outlier.lin = (value > mean(value) + 3*sd(value)) |
                 (value < mean(value) - 3*sd(value)),
               is.outlier.log = (value.log > mean(value.log) + 3*sd(value.log)) |
                 (value.log < mean(value.log) - 3*sd(value.log)))
      values <- filter(values, is.outlier.log) %>%  select(uuid,loop_index, variable, value)
      if (nrow(values)>0) print(paste0(col, ": ", nrow(values), " outliers detected"))
      res.outliers_water_count_loop <- rbind(res.outliers_water_count_loop, values)
    }
    
    f.alpha <- function(x) return(ifelse(x, 1, 0))
    
    # Outliers Boxplot generator per country
    
    df <- raw.water_count_loop.outliers %>% 
      select(uuid, all_of(cols.integer_raw.water_count_loop)) %>% 
      tidyr::pivot_longer(-uuid, names_to = "variable", values_to = "value") %>% 
      mutate(value.log = log10(value)) %>% 
      left_join(select(res.outliers_water_count_loop, -value) %>% mutate(is.outlier=T), by = c("uuid","variable")) %>% 
      mutate(is.outlier = ifelse(is.na(is.outlier), F, is.outlier)) %>% 
      filter(!is.na(value) & value>0)
    df <- tidyr::gather(df, key = "measure", value = "value", variable)
    df.all <- rbind(df.all, df)
    
    
    openxlsx::write.xlsx(df.all, paste0("output/checking/outliers/water_count_loop_outlier_prices_analysis_", n.sd, "sd.xlsx"), overwrite=T)
    
    # generating prices boxplots for same locations
    g.outliers_water_count_loop <- ggplot2::ggplot(df.all) +
      ggplot2::geom_boxplot(ggplot2::aes(x= measure, y=value.log), width = 0.2) + ggplot2::ylab("Values (log10)") +
      ggplot2::geom_point(ggplot2::aes(x=measure, y=value.log, group = measure), alpha=f.alpha(df.all$is.outlier), colour="red") +
      ggplot2::facet_wrap(~value, ncol = 4, scales = "free_y")+
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank())
    
    
    # Save
    ggplot2::ggsave(paste0("output/checking/outliers/water_count_loop_outlier_prices_analysis_", n.sd, "sd.pdf"), g.outliers_water_count_loop, 
           width = 40, height = 80, units = "cm", device="pdf")
    
    
    if(nrow(res.outliers_water_count_loop)>0){
      # Output requests to check
      res.outliers_water_count_loop <- res.outliers_water_count_loop %>% 
        mutate(issue = "Outliers",
               loop_index = loop_index,
               new.value = NA,
               explanation=NA) %>% 
        rename("old.value"=value) %>% 
        select(uuid,loop_index,variable,issue,old.value,new.value,explanation)
      
      res.outliers_all <- rbind(res.outliers_all,res.outliers_water_count_loop)
      
      cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_water_count_loop)
    }
  }
}
#------------------------------------------------------------------------------------------------------------
# [child_nutrition SHEET] -> detect outliers 
if(length(cols.integer_raw.child_nutrition) > 0){
  df.all <- data.frame()
  raw.child_nutrition.outliers <- raw.child_nutrition %>%
    select("uuid",loop_index, cols.integer_raw.child_nutrition) %>%
    mutate_at(cols.integer_raw.child_nutrition, as.numeric)
  
  # Outliers per country
  
  for (col in cols.integer_raw.child_nutrition) {
    values <- raw.child_nutrition.outliers %>% 
      filter(!!sym(col) %_>_% 0) %>% 
      rename(value=col) %>%  select(uuid,loop_index, value) %>% 
      mutate(value.log=log10(value)) %>%  mutate(variable=col) %>% 
      mutate(is.outlier.lin = (value > mean(value) + 3*sd(value)) |
               (value < mean(value) - 3*sd(value)),
             is.outlier.log = (value.log > mean(value.log) + 3*sd(value.log)) |
               (value.log < mean(value.log) - 3*sd(value.log)))
    values <- filter(values, is.outlier.log) %>%  select(uuid,loop_index, variable, value)
    if (nrow(values)>0) print(paste0(col, ": ", nrow(values), " outliers detected"))
    res.outliers_child_nutrition <- rbind(res.outliers_child_nutrition, values)
  }
  
  f.alpha <- function(x) return(ifelse(x, 1, 0))
  
  # Outliers Boxplot generator per country
  
  df <- raw.child_nutrition.outliers %>% 
    select(uuid, all_of(cols.integer_raw.child_nutrition)) %>% 
    tidyr::pivot_longer(-uuid, names_to = "variable", values_to = "value") %>% 
    mutate(value.log = log10(value)) %>% 
    left_join(select(res.outliers_child_nutrition, -value) %>% mutate(is.outlier=T), by = c("uuid","variable")) %>% 
    mutate(is.outlier = ifelse(is.na(is.outlier), F, is.outlier)) %>% 
    filter(!is.na(value) & value>0)
  df <- tidyr::gather(df, key = "measure", value = "value", variable)
  df.all <- rbind(df.all, df)
  
  
  openxlsx::write.xlsx(df.all, paste0("output/checking/outliers/child_nutrition_outlier_prices_analysis_", n.sd, "sd.xlsx"), overwrite=T)
  
  # generating prices boxplots for same locations
  g.outliers_child_nutrition <- ggplot2::ggplot(df.all) +
    ggplot2::geom_boxplot(ggplot2::aes(x= measure, y=value.log), width = 0.2) + ggplot2::ylab("Values (log10)") +
    ggplot2::geom_point(ggplot2::aes(x=measure, y=value.log, group = measure), alpha=f.alpha(df.all$is.outlier), colour="red") +
    ggplot2::facet_wrap(~value, ncol = 4, scales = "free_y")+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank())
  
  
  # Save
  ggplot2::ggsave(paste0("output/checking/outliers/child_nutrition_outlier_prices_analysis_", n.sd, "sd.pdf"), g.outliers_child_nutrition, 
         width = 40, height = 80, units = "cm", device="pdf")
  
  if(nrow(res.outliers_child_nutrition)>0){
    # Output requests to check
    res.outliers_child_nutrition <- res.outliers_child_nutrition %>% 
      mutate(issue = "Outliers",
             loop_index = loop_index,
             new.value = NA,
             explanation=NA) %>% 
      rename("old.value"=value) %>% 
      select(uuid,loop_index,variable,issue,old.value,new.value,explanation)
    
    res.outliers_all <- rbind(res.outliers_all,res.outliers_child_nutrition)
    
    cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_child_nutrition)
  }
}

res <- cleaning.log.outliers %>% 
  mutate(invalid = NA) %>% 
  relocate(invalid, .before = ncol(cleaning.log.outliers))
if(nrow(cleaning.log.outliers)>0){
  save.outlier.responses(res,paste0(dataset.name.short, "_outliers_requests_",strings["out_date"],".xlsx"), use_template = T)  
  if(language_assessment == "English"){
    cat("\n\n#############################################################################################\n")
    cat("The outliers check are done. Please go to output/checking/requests/ and check the file with \nthe name outliers_requests and follow the instructions in the read me tab.\n")
    cat("#############################################################################################\n")
  } else {
    cat("\n\n#############################################################################################\n")
    cat("La vérification des valeurs aberrantes est terminée. Allez dans output/checking/requests/ \net vérifiez le fichier avec le nom outliers_requests et suivez les instructions dans l'onglet read me.\n")
    cat("#############################################################################################\n")
  }
} else {
  if(language_assessment == "English"){
    cat("\n\n#############################################################################################\n")
    cat("There were no outliers detected. You can rerun the same file with a \nlower number of SD to check if any outliers will be detected.\n")
    cat("#############################################################################################\n")
  }else{
    cat("\n\n#############################################################################################\n")
    cat("Aucune valeur aberrante n'a été détectée. Vous pouvez réexécuter le même fichier \navec un nombre inférieur de SD pour vérifier si des valeurs aberrantes sont détectées..\n")
    cat("#############################################################################################\n")
  }
}
options(warn=0)
