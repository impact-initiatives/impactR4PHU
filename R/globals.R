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
  # add_mfaz:
  "mfaz","age_months","severe_mfaz","moderate_mfaz","global_mfaz",
  # add_muac
  "age_months","sam_muac","mam_muac","gam_muac",
  # add_iycf
  "mdd1", "mdd2","mdd3", "mdd4","mdd5", "mdd6",
  "mdd7", "mdd8","iycf_6b_num", "iycf_6c_num","iycf_6d_num", "iycf_7a_num",
  "iycf_evbf", "iycf_eibf","iycf_ebf2d", "iycf_ebf","iycf_mixmf", "iycf_cbf",
  "iycf_isssf", "iycf_mdd_score","iycf_mdd_cat", "iycf_mmf","iycf_mmff", "iycf_mad",
  "iycf_eff", "iycf_swb", "iycf_ufc", "iycf_zvf","iycf_bof",
  # check_anthro_flags:
  "sex","age_months","age_days","mfaz",
  "flag_sd_mfaz","mfaz_noflag","global_mfaz","moderate_mfaz",
  "severe_mfaz","mfaz_who_flag","mean_mfaz_noflag","sd_mfaz_noflag",
  "global_mfaz_noflag","moderate_mfaz_noflag","severe_mfaz_noflag","flag_extreme_muac",
  "gam_muac","mam_muac","sam_muac","nut_muac_mm",
  "gam_muac_noflag","mam_muac_noflag","sam_muac_noflag","muac_noflag","flag_edema_pitting",
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
  # check_health_flags:
  "month_exp","health_exp","prop_health_exp","flag_severe_health_exp",
  "flag_catastrophic_health_exp","month_exp_1","month_exp_2",
  # check_iycf_flags:
  "foods_all_no","liquids_all_no","flag_yes_foods",
  # calculate_plausibility
  "age_ratio.pvalue","sex_ratio.pvalue","dps_muac","flag_perc_mfaz_children","n_children_muac",
  "sd_muac_mm","plaus_sd_muac_mm","plaus_n_children_muac","plaus_ageratio","plaus_sexratio",
  "plaus_perc_mfaz_children","plaus_dps_muac","plaus_anthro_score","cdr","prop_hh_flag_deaths",
  "age_ratio_0_5.pvalue","age_ratio_2_5.pvalue","age_ratio_5_10.pvalue","mean_hh_size.pvalue","prop_join_people",
  "prop_left_people","poisson_pvalues.deaths","plaus_cdr","plaus_hh_multiple_death","plaus_sex_ratio",
  "plaus_age0to4_5plus_ratio","plaus_age0to1_2to4_ratio","plaus_age0to4_5to10_ratio","plaus_mean_hh_size.pvalue",
  "plaus_prop_joiners","plaus_prop_leavers","plaus_poisson_pvalues.deaths","plaus_mort_score","plaus_flag_lcsi_liv_agriculture",
  "plaus_flag_lcsi_liv_livestock","corr.fcs_rcsi","corr.fcs_rcsi.pvalue","corr.fcs_hhs","corr.fcs_hhs.pvalue",
  "corr.hhs_rcsi","corr.hhs_rcsi.pvalue","prop_fc_flags","mad_ratio.pvalue","prop_flag_high_mdd_low_mmf",
  "age_ratio_under6m_6to23m.pvalue","sd_mdd","prop_iycf_caregiver","plaus_prop_iycf_caregiver","plaus_sdd_mdd",
  "plaus_age_ratio_under6m_6to23m.pvalue","plaus_prop_flag_high_mdd_low_mmf","plaus_mad_ratio.pvalue","iycf_plaus_score",
  # create_fsl_plaus
  "fsl_fcs_score","n","fsl_fc_phase","flag_fsl_fc_cell","fsl_fc_cell",
  "p1","p2","p3","p4","p5",
  # get.label:
  "name",
  # create_mortality_long_df
  "uuid","admin1_roster","admin2_roster","age_years",
  "date_dc_date","date_recall_date","date_join_date","person_time",
  "date_death_date","death","join","date_left_date",
  "left","date_birth_date","birth","under_5",
  "hh_id","individual_id","id",
  #reformat_mortality
  "date_dc","date_dc_date","date_dc_month","date_dc_day","date_dc_year",
  "date_dc_char","date_recall","date_recall_date","date_recall_month","date_recall_day",
  "date_recall_year","date_join","date_join_date","date_join_month","date_join_day",
  "date_join_year","date_left","date_left_date","date_left_month",
  "date_left_day","date_left_year","date_birth","date_birth_date","date_birth_month",
  "date_birth_day","date_birth_year","date_death","date_death_date",
  "date_death_month","date_death_day","date_death_year",
  "death_cause","death_cause_smart","death_location","death_location_smart",
  #check_mortality_flags
  "cause_death","under_5_pt","join_under5","left_under5",
  "birth_under5","death_under5","age_0to5","age_5plus","age_0to2",
  "age_2to5","age_5to10","deaths","total_persontime","cdr_se",
  "deaths_under5","total_under5_persontime","u5dr","u5dr_se","cdr_lower_ci",
  "cdr_upper_ci","u5dr_lower_ci","u5dr_upper_ci","joins","total_people",
  "lefts","cdr_ci","u5dr_ci","flag_multiple_death","flag_cause_death",
  "total_under5","total_flag_deaths","total_flag_cause_deaths","hh_size","num_deaths",
  "is_hh","is_hh_under5","is_hh_flag_deaths","n_hh_under_5","n_hh",
  "n_hh_flag_deaths","mean_deaths_per_hh","mean_hh_size","mean_num_under5","sex_ratio",
  "age_ratio_0_5","age_ratio_2_5","age_ratio_5_10","births","plaus_mort_cat",
  "enumerator","flag_negative_pt",
  #create_mortality_plaus
  "birth_rate","birth_rate_se","birth_rate_lower_ci","birth_rate_upper_ci",
  "birth_rate_ci","person_time_out","person_time_in","total_persontime_in",
  "total_persontime_out","num_days","pt_join","pt_left"

)))
