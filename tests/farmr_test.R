library(SWATfarmR)

pth <- "/home/christoph/Documents/projects/SWATfarmR/data/TxtInOut"
mgt_file <- "/home/christoph/Documents/projects/SWATfarmR/data/mgt_input/mgt_conv_1.0.csv"
new_farmr("raab", pth)

raab$read_management(file = mgt_file)
