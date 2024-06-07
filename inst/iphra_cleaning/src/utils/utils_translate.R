translate.responses_iphra <- function(responses, api_key = NULL, api =NULL, values_from = "response.uk", language_codes = 'uk', target_lang = "en", threshold = 200000){
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
  responses <- responses %>% mutate(resp_lower = str_to_lower(!!sym(values_from)))
  
  input_vec <- responses %>% distinct(resp_lower) %>% pull(resp_lower)
  # cleaning up html leftovers:
  input_vec <- gsub("&#39;", "'", input_vec)
  # counts characters which will be translated
  char_counter <- sum(str_length(input_vec))
  # TODO: pause here, print the char_counter, and ask the user if the translation should go ahead
  if (char_counter > threshold){
    yes_no <- svDialogs::dlgInput(paste0("The number of characters exceeds ", threshold, ". Please enter [YES] if you would like to proceed or [NO] to kill:"), "YES or NO")$res
  } else{
    yes_no <- "YES"
  }
  batching <- 10
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
          tcltk::setTkProgressBar(pb, prog, progress.bar.title, paste0("Number of batches executed: ", prog, " of ", batching,"\n",length(temp_resp_batch$input_vec)," responses will be translated from ",code," to ",target_lang, "\nThis means ",sum(str_length(temp_resp_batch$input_vec))," utf-8 characters."))
          prog <- prog + 1
          # cat(length(temp_resp_batch$input_vec),"responses will be translated from",code,"to",target_lang, "\tThis means",sum(str_length(temp_resp_batch$input_vec)),"utf-8 characters.\n")
          # actual translation:
          result_vec <- NULL
          if(api == "Microsoft"){
            result_vec <- try(translateR::translate(content.vec = temp_resp_batch$input_vec,
                                                    microsoft.api.key = as.character(strings['api_key']),
                                                    microsoft.api.region = "switzerlandnorth",
                                                    source.lang = code, target.lang = target_lang))
          } else {
            result_vec <- try(deeplr::translate2(text = temp_resp_batch$input_vec,
                                                 source_lang = toupper(code),
                                                 target_lang = toupper(target_lang),
                                                 auth_key = strings['api_key']))
          }
          if(inherits(result_vec,"try-error")) break
          # checking the results
          info_df <- rbind(info_df, data.frame(## DEBUGG IT HERE
            "input_responses_num" = length(temp_resp_batch$input_vec),
            "translated_characters_num" = sum(str_length(temp_resp_batch$input_vec)),
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
  log_filename <- "output/info/translate_info.csv"
  if(file.exists(log_filename)) write.table(info_df, file = log_filename, append = T, row.names = F, col.names = F, sep = ',')
  else write.table(info_df, file = log_filename, row.names = F, col.names = T, sep = ',')
  
  responses <- responses %>% select(-resp_lower)
  return(responses)
}
