library(dplyr)
raw.main <- readxl::read_excel("data/inputs/kobo_export/dummy_raw_data.xlsx", sheet = "main")
raw.hh_roster <- readxl::read_excel("data/inputs/kobo_export/dummy_raw_data.xlsx", sheet = "hh_roster")
raw.ind_health <- readxl::read_excel("data/inputs/kobo_export/dummy_raw_data.xlsx", sheet = "ind_health")
raw.water_count_loop <- readxl::read_excel("data/inputs/kobo_export/dummy_raw_data.xlsx", sheet = "water_count_loop")
raw.child_nutrition <- readxl::read_excel("data/inputs/kobo_export/dummy_raw_data.xlsx", sheet = "child_nutrition")
raw.women <- readxl::read_excel("data/inputs/kobo_export/dummy_raw_data.xlsx", sheet = "women")
raw.died_member <- readxl::read_excel("data/inputs/kobo_export/dummy_raw_data.xlsx", sheet = "died_member")
raw.died_member <- raw.died_member %>% 
  filter(!uuid %in%uuidtoremove)

datasheets <- list("main" = raw.main,
                   "hh_roster" = raw.hh_roster,
                   "ind_health" = raw.ind_health,
                   "water_count_loop" = raw.water_count_loop,
                   "child_nutrition" = raw.child_nutrition,
                   "women" = raw.women,
                   "died_member" = raw.died_member
)
openxlsx::write.xlsx(datasheets, "data/inputs/kobo_export/dummy_raw_data.xlsx", overwrite = T,
           zoom = 90, firstRow = T)
