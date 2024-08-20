save.ph.integrated.tables <- function(df, df_cat, wb_name, mort = F, use_template = F) {
  if(use_template && mort) {
    wb <- openxlsx::loadWorkbook("resources/ph_integrated_template.xlsx")
    } else if(use_template && !mort) {
      wb <- openxlsx::loadWorkbook("resources/ph_integrated_template_wo_mort.xlsx")
    } else {
      wb <- openxlsx::createWorkbook()
    }

  openxlsx::writeData(wb = wb, x = df, sheet = "Data", startRow = 3, colNames = F)
  openxlsx::writeData(wb = wb, x = df_cat, sheet = "Cat", startRow = 3, colNames = F)

  if(mort){
    end_column <- 18
  } else {
    end_column <- 17
  }

  openxlsx::conditionalFormatting(wb,
                                  sheet = "Cat",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="Extremely high"',
                                  style = openxlsx::createStyle(bgFill = "#960000",
                                                                fontColour = "white"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Cat",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="Very high"',
                                  style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                fontColour = "white"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Cat",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="High"',
                                  style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                fontColour = "black"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Cat",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="Moderate"',
                                  style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                fontColour = "#833C0C"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Cat",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="Low"',
                                  style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                fontColour = "black"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Cat",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '=""',
                                  style = openxlsx::createStyle(bgFill = "black"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Data",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="Extremely high"',
                                  style = openxlsx::createStyle(bgFill = "#960000",
                                                                fontColour = "white"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Data",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="Very high"',
                                  style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                fontColour = "white"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Data",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="High"',
                                  style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                fontColour = "black"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Data",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="Moderate"',
                                  style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                fontColour = "#833C0C"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Data",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '="Low"',
                                  style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                fontColour = "black"))
  openxlsx::conditionalFormatting(wb,
                                  sheet = "Data",
                                  cols = 3:end_column,
                                  rows = 3:(nrow(df_cat)+2),
                                  rule = '=""',
                                  style = openxlsx::createStyle(bgFill = "black"))


  filename <- paste0("output/", wb_name, ".xlsx")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
}
