# Libraries ------------------------------------------------------------------
library(tidyverse)
library(DBI)
library(odbc)
library(openxlsx)

# Directories -------------------------------------------------------------

output_dir <- paste0("/SharedDrive/deans/Presidents/SixSigma/",
                     "MSHS Productivity/Productivity/Labor - Data/",
                     "Multi-site/Quality_Check/")

# oao_production db connection
oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")

# User Input---------------------------------------------------------------
# date range
start_date <- "2024-10-20"
end_date <- "2024-11-30"

# Data References ---------------------------------------------------------

rep_def <- tbl(oao_con, "LPM_MAPPING_REPDEF") %>%
  collect()

payperiod_ct <- tbl(oao_con, "LPM_MAPPING_PAYCYCLE") %>%
  select(PP_END_DATE) %>%
  filter(PP_END_DATE >= as.Date(start_date),
         PP_END_DATE <= as.Date(end_date)) %>%
  distinct() %>%
  collect() %>%
  nrow()

# Data Pre-processing -----------------------------------------------------

data_wrk_hr <- tbl(oao_con, "LPM_MAPPED_MSHS_ORACLE") %>%
  filter(PP_END_DATE >= as.Date(start_date),
         PP_END_DATE <= as.Date(end_date),
         PROVIDER == 0,
         WORKED_PAYCODE == 1,
         INCLUDE_HOURS == 1) %>%
  select(WORKED_FACILITY, WD_HOURS, PP_END_DATE,
         DEFINITION_CODE, DEFINITION_NAME, CORPORATE_SERVICE_LINE) %>%
  collect() %>%
  left_join(rep_def %>%
              select(DEFINITION_CODE, SITE, DEPARTMENT_BREAKDOWN)) %>%
  filter(DEPARTMENT_BREAKDOWN == 1) %>%
  mutate(DEPARTMENT_BREAKDOWN = NULL)

summary_wrk_hr <- data_wrk_hr %>%
  group_by(WORKED_FACILITY, SITE,
           DEFINITION_CODE, DEFINITION_NAME, CORPORATE_SERVICE_LINE) %>%
  summarize(WRK_FTE = round(sum(WD_HOURS / (75 * payperiod_ct), na.rm = TRUE),
                            1)) %>%
  ungroup()

facility_ct_wrk_hr <- summary_wrk_hr %>%
  select(WORKED_FACILITY, DEFINITION_CODE) %>%
  distinct() %>%
  group_by(DEFINITION_CODE) %>%
  summarize(facility_ct = n()) %>%
  ungroup()

rpt_wrk_hr <- data_wrk_hr %>%
  group_by(DEFINITION_CODE) %>%
  summarize(tot_WRK_FTE = round(sum(WD_HOURS / (75 * payperiod_ct),
                                    na.rm = TRUE), 1)) %>%
  ungroup()

rpt_live_wrk_hr <- summary_wrk_hr %>%
  group_by(DEFINITION_CODE) %>%
  summarize(max_WRK_FTE = max(WRK_FTE)) %>%
  ungroup()

rpt_pct_miss <- rpt_wrk_hr %>%
  left_join(rpt_live_wrk_hr) %>%
  mutate(potential_pct_missing =
           round(100 * (tot_WRK_FTE - max_WRK_FTE) / tot_WRK_FTE, 1),
         tot_WRK_FTE = NULL,
         max_WRK_FTE = NULL)

summary_wrk_hr_qc_view <- summary_wrk_hr %>%
  arrange(SITE, DEFINITION_CODE) %>%
  pivot_wider(id_cols = c(SITE, DEFINITION_CODE, DEFINITION_NAME,
                          CORPORATE_SERVICE_LINE),
              names_from = WORKED_FACILITY,
              values_from = WRK_FTE) %>%
  left_join(facility_ct_wrk_hr) %>%
  left_join(rpt_pct_miss) %>%
  filter(facility_ct > 1)


# Data Formatting ---------------------------------------------------------

date_range <- paste0(start_date, "_to_", end_date)

wb <- createWorkbook()

sheet <- addWorksheet(wb, "x-site_wrk_FTEs")

# set freeze pane and col widths
freezePane(wb, sheet, firstActiveRow = 2)
setColWidths(wb, sheet, cols = seq_len(ncol(summary_wrk_hr_qc_view)),
             widths = "auto")

# write data to sheet
writeDataTable(wb,
               sheet = sheet,
               x = summary_wrk_hr_qc_view,
               keepNA = FALSE,
               withFilter = TRUE)

#create cell styles
header_style <- createStyle(fgFill = "grey", halign = "left", valign = "top",
                            textDecoration = "bold", fontColour = "#010101")
body_style <- createStyle(border = "TopBottomLeftRight")

# apply cell styles
addStyle(wb, sheet, header_style, rows = 1,
         cols = seq_len(ncol(summary_wrk_hr_qc_view)), gridExpand = TRUE,
         stack = TRUE)
setRowHeights(wb, sheet, 1, heights = 43.5)

addStyle(wb, sheet, body_style, rows = 1:(nrow(summary_wrk_hr_qc_view) + 1),
         cols = seq_len(ncol(summary_wrk_hr_qc_view)), gridExpand = TRUE,
         stack = TRUE)

# File Saving -------------------------------------------------------------

saveWorkbook(wb, file = paste0(output_dir, "cross-site_check_",
                               date_range, "___",
                               Sys.time(), ".xlsx"))

# Script End --------------------------------------------------------------
