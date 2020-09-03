library(SWATfarmR)
library(tidyverse)

# Weight functions

## Logit function
logit_wgt <- function(x, lwr, upr) {
  k   <- 9.19024 / (upr - lwr)
  x_0 <- upr/2 + lwr/2
  1 / (1 + exp(-k*(x - x_0)))
}

## Linear function
lin_wgt <- function(x, lw_bd, up_bd) {
  (x >= up_bd) + (x < up_bd & x > lw_bd) * (x - lw_bd) / (up_bd - lw_bd)
}

txtio_pth <- "D:/Projects_R/SWATfarmR/data/raab"
mgt_file <- "D:/Projects_R/SWATfarmR/data/mgt_input/mgt_raab.csv"

new_farmr(project_name = "raab", project_path = txtio_pth)

raab$read_management(file = mgt_file)
raab$schedule_management_operations()

# To check whether any operations were skipped:
# This is useful to check if the dynamic rules were set wrong, that all weights
# are zero. Or to check if operations that are intended to be skipped in some rare
# cases actually were skipped.
# In this example the dates for applying fertilizer for SWHT were set wrong and
# were too close to the last possible date of the previous operation.
raab$.data$scheduled_operations$skipped_operations


# To check e.g. individual management tables:
raab$.data$scheduled_operations$scheduled_operations$`000010010`

raab$write_mgt_files(start_year = 2000, end_year = 2005)


# path = raab$.data$meta$project_path
# mgt_raw = raab$.data$meta$mgt_raw
# schedule = raab$.data$scheduled_operations$scheduled_operations
# variable = raab$.data$variables[[1]]
# write_all = TRUE
# start_year = 2005
# end_year = 2010


# load_farmr(file = raab_farmr)

# mgt_tbl <- read_csv("/home/christoph/Documents/projects/SWATfarmR/data/mgt_input/mgt_conv_neu.csv")
