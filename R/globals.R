# remotes::install_github("thinkr-open/checkhelper")
# checkhelper::print_globals()

globalVariables(unique(c(
  # add_fcs:
  "fcs_weight_cereal1", "fcs_weight_dairy3",
  "fcs_weight_fruit6", "fcs_weight_legume2",
  "fcs_weight_meat4", "fcs_weight_oil7",
  "fcs_weight_sugar8", "fcs_weight_veg5",
  # add_hdds:
  "fsl_hdds_score", "fsl_hdds_cat",
  # add_hhs:
  ".", "fsl_hhs_comp1", "fsl_hhs_comp2",
  "fsl_hhs_comp3", "fsl_hhs_score",
  "fsl_hhs_cat", "fsl_hhs_cat_ipc",
  # add_lcsi:
  "lcsi_stress2",
  # add_rcsi:
  "rcsi_lessquality_weighted", "rcsi_borrow_weighted",
  "rcsi_mealsize_weighted", "rcsi_mealadult_weighted",
  "rcsi_mealnb_weighted", "fsl_rcsi_score",
  # check_fsl_flags:
  "group", "flag_meat_cereal_ratio",
  "flag_low_cereal", "flag_low_fcs", "flag_low_oil",
  "flag_low_sugar_cond_hdds", "flag_meat_cereal_ratio",
  "flag_protein_rcsi", "flag_rcsi_children",
  "flag_sd_foodgroup", "flag_sd_rcsicoping", "flag_severe_hhs",
  "all_of", "flag_fc_cell", "flag_high_fcs", "flag_fcs_rcsi",
  "flag_high_rcsi", "flag_fcsrcsi_box",
  # check_wash_flags:
  "group","flag_no_container",
  "flag_not_immediate", "flag_sd_litre",
  "litre" ,"litre_per_day", "litre_per_day_per_hh",
  "litre_per_day_per_person", "litre_z_score",
  "flag_low_litre", "flag_high_container", "flag_high_litre",
  "flag_no_container", "container_type_litre",
  # get.label:
  "name"
)))
