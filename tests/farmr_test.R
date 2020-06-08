library(SWATfarmR)

raab_farmr <- "/home/christoph/Documents/projects/SWATfarmR/data/TxtInOut/raab.farm"
pth <- "/home/christoph/Documents/projects/SWATfarmR/data/TxtInOut"
mgt_file <- "/home/christoph/Documents/projects/SWATfarmR/data/mgt_input/mgt_conv_neu.csv"
new_farmr("raab", pth)

raab$read_management(file = mgt_file)
raab$schedule_management_operations()
raab$write_mgt_files()
# raab$reset_mgt_files()

# path = raab$.data$meta$project_path
# mgt_raw = raab$.data$meta$mgt_raw
# schedule = raab$.data$scheduled_operations$scheduled_operations
# variable = raab$.data$variables[[1]]
# write_all = TRUE
# start_year = 2005
# end_year = 2010


# load_farmr(file = raab_farmr)
