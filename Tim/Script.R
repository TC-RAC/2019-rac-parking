library(knitr)
library(dplyr)
library(kableExtra)


# print summary table
wal.summary.formatted<-readRDS("WSF.Rds")
yearz=2022
current.year=2017
dp.tables=2

wal.summary.formatted %>%
  kable( "latex",
         booktabs = T,
         digits = dp.tables,
         longtable = TRUE,
         escape = F,
         align = c("r"),
         caption = "Summary of parking accounts for Wales (£ millions)",
         col.names = c("", "", "yearz", paste0("Change ", current.year, "-",
                                             current.year-1999, " on ",
                                             current.year-1, "-",
                                             current.year-1999-1))) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  kable_styling(full_width = TRUE,
                latex_options =c("HOLD_position", "striped"),
                font_size = 10) %>%
  column_spec(1:2, width = "3cm")




