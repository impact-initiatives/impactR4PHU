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
  ## 25/20/15/10
  if(mort){
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Cat",
                                    cols = c(3,5:end_column),
                                    rows = 3:(nrow(df_cat)+2),
                                    rule = '="Extremely high"',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Cat",
                                    cols = c(3,5:end_column),
                                    rows = 3:(nrow(df_cat)+2),
                                    rule = '="Very high"',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Cat",
                                    cols = c(3,5:end_column),
                                    rows = 3:(nrow(df_cat)+2),
                                    rule = '="High"',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Cat",
                                    cols = c(3,5:end_column),
                                    rows = 3:(nrow(df_cat)+2),
                                    rule = '="Moderate"',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Cat",
                                    cols = c(3,5:end_column),
                                    rows = 3:(nrow(df_cat)+2),
                                    rule = '="Low"',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Cat",
                                    cols = c(3,5:end_column),
                                    rows = 3:(nrow(df_cat)+2),
                                    rule = '=""',
                                    style = openxlsx::createStyle(bgFill = "black"))
    #25/20/15/10
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(5),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<=0.10',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(5),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.10',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(5),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.15',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(5),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.20',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(5),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.25',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))

    ## 20/15/10/5

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(12),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<=0.05',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(12),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.05',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(12),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.10',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(12),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.15',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(12),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.20',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))
    ## 10/8/6/4

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<=0.04',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.04',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.06',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.08',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.10',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))
    ## 20/15/10/5

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(17),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>=0.2',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(17),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<0.2',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(17),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<0.15',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(17),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<0.1',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(17),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<0.05',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))

    ## 20/40/60/80
    cols <- c(14,16)
    for(col in cols){
      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>=0.8',
                                      style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                    fontColour = "black"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<0.8',
                                      style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                    fontColour = "#833C0C"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<0.6',
                                      style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                    fontColour = "black"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<0.4',
                                      style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                    fontColour = "white"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<0.20',
                                      style = openxlsx::createStyle(bgFill = "#960000",
                                                                    fontColour = "white"))
    }


    ### 40/30/20/10 %

    cols <- c(6,9:11,13,end_column)

    for(col in cols){
      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<=0.10',
                                      style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                    fontColour = "black"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>0.10',
                                      style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                    fontColour = "#833C0C"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>0.20',
                                      style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                    fontColour = "black"))
      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>0.3',
                                      style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                    fontColour = "white"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>0.4',
                                      style = openxlsx::createStyle(bgFill = "#960000",
                                                                    fontColour = "white"))
    }
    # MORT
    for(i in 1:nrow(df)) {
      # Extract the lower bound of the confidence interval
      lower_bound <- as.numeric(stringr::str_extract(df$mort[i],"^[^ ]*"))
      # Determine the style based on the value
      if (lower_bound >= 2) {
        style <- openxlsx::createStyle(fgFill = "#960000",
                                       fontColour = "white")  # Red
      } else if (lower_bound > 1) {
        style <- openxlsx::createStyle(fgFill = "#FF0000",
                                       fontColour = "white")  # Orange
      } else if (lower_bound > 0.75) {
        style <- openxlsx::createStyle(fgFill = "#ED7D31",
                                       fontColour = "black")  # Yellow
      } else if (lower_bound > 0.5) {
        style <- openxlsx::createStyle(fgFill = "#FFE699",
                                       fontColour = "#833C0C") # Light Green
      } else {
        style <- openxlsx::createStyle(fgFill = "#C6E0B4",
                                       fontColour = "black")  # Green
      }

      # Apply the style to the cell
      openxlsx::addStyle(wb, "Data", style = style, rows = i + 2, cols = 3, gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(3,5:end_column),
                                    rows = 3:(nrow(df)+2),
                                    rule = '=""',
                                    style = openxlsx::createStyle(bgFill = "black"))
  } else {
    #25/20/15/10
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(3),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<=0.10',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(3),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.10',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(3),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.15',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(3),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.20',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(3),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.25',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))

    ## 20/15/10/5

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(10),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<=0.05',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(10),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.05',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(10),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.10',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(10),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.15',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(10),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.20',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))
    ## 10/8/6/4

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(13),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<=0.04',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(13),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.04',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(13),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.06',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(13),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.08',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(13),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>0.10',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))
    ## 20/15/10/5

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '>=0.2',
                                    style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<0.2',
                                    style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                  fontColour = "#833C0C"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<0.15',
                                    style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                  fontColour = "black"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<0.1',
                                    style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                  fontColour = "white"))

    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(15),
                                    rows = 3:(nrow(df)+2),
                                    rule = '<0.05',
                                    style = openxlsx::createStyle(bgFill = "#960000",
                                                                  fontColour = "white"))
    ## 20/40/60/80
    cols <- c(12,14)
    for(col in cols){
      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>=0.8',
                                      style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                    fontColour = "black"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<0.8',
                                      style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                    fontColour = "#833C0C"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<0.6',
                                      style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                    fontColour = "black"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<0.4',
                                      style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                    fontColour = "white"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<0.20',
                                      style = openxlsx::createStyle(bgFill = "#960000",
                                                                    fontColour = "white"))
    }
    ### 40/30/20/10 %

    cols <- c(4,7:9,11,end_column)

    for(col in cols){
      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '<=0.10',
                                      style = openxlsx::createStyle(bgFill = "#C6E0B4",
                                                                    fontColour = "black"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>0.10',
                                      style = openxlsx::createStyle(bgFill = "#FFE699",
                                                                    fontColour = "#833C0C"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>0.20',
                                      style = openxlsx::createStyle(bgFill = "#ED7D31",
                                                                    fontColour = "black"))
      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>0.3',
                                      style = openxlsx::createStyle(bgFill = "#FF0000",
                                                                    fontColour = "white"))

      openxlsx::conditionalFormatting(wb,
                                      sheet = "Data",
                                      cols = col,
                                      rows = 3:(nrow(df)+2),
                                      rule = '>0.4',
                                      style = openxlsx::createStyle(bgFill = "#960000",
                                                                    fontColour = "white"))
    }
    openxlsx::conditionalFormatting(wb,
                                    sheet = "Data",
                                    cols = c(3:end_column),
                                    rows = 3:(nrow(df)+2),
                                    rule = '=""',
                                    style = openxlsx::createStyle(bgFill = "black"))
  }

  filename <- paste0("output/", wb_name, ".xlsx")
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
}
