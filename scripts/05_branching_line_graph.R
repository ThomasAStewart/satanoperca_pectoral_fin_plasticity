# PURPOSE ####
#
# To produce a graph showing frequencies of fin ray branching in Satanoperca daemon in three experimental treatments
# This is intended to be used for Figure 3 A
#
# Date: 2024-06-10
# Author: TAS

# initialize libraries
library(ggplot2)

# load data
data = read.csv(file = "data_cleaned/fin_ray_branching_frequency.csv")  # what file?

# tidy data for graphing
data_left <- data[which(data$treatment!='Wild'),]
data_left <- data_left[which(data_left$side=='left'),]
data_left$treatment <- as.factor(data_left$treatment)

percent_unbranched <- data_left$absent/(data_left$absent+data_left$present) # calculate percentage unbranched

data_left <- cbind(data_left, percent_unbranched) # add this new variable to the data frame

# PLOT DATA ####
branch_plot <- ggplot (data=data_left, aes(x=ray, y=1-percent_unbranched, group=interaction(side, treatment), color=treatment, linetype=side)) + 
  geom_line() +
  geom_point() +
  theme_minimal()

ggsave("output_files/fig_3a_branch_plot.pdf", plot=branch_plot,  width=10, height=10, units = "cm")

