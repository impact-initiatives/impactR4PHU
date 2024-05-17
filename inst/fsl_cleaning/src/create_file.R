if(nrow(checks_followups) > 0){
  create.follow.up.requests(checks_followups,loop_data = NULL, paste0("Mort_followup_requests.xlsm"), use_template = T)
  options(warn=0)
  cat("\n\n#############################################################################################\n")
  cat("Logical checks are flagged and a file is created for follow up in \noutput/checking/requests/ with follow_up_requests in the title. \nPlease check the READ_ME file for information on filling the file.\n")
  cat("#############################################################################################\n")
} else {
  cat("\n\n#############################################################################################\n")
  cat(" No logical checks were flagged!!. All good with cleaning for Mortality")
  cat("#############################################################################################\n")
}
