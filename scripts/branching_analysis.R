# PURPOSE ####
#
# To produce a graph showing frequencies of fin ray branching in Satanoperca daemon in three experimental treatments
# This is intended to be used for Figure 2 A
#
# Date: 2023-09-08
# Author: TAS


# INITIALIZE PACKAGES ####
#install.packages("ggplot2") 
library(ggplot2)


# LOAD DATA FILES #### 
branch_data <- read.delim2("data/branching_frequency.txt") # load raw data


# PERFORM CALCULATIONS ####
percent_unbranched <- branch_data$absent/(branch_data$absent+branch_data$present) # calculate percentage unbranched
branch_data <- cbind(branch_data, percent_unbranched) # add this new variable to the data frame


# PLOT DATA ####
branch_plot <- ggplot (data=branch_data, aes(x=ray, y=percent_unbranched, group=interaction(side, treatment), color=treatment, linetype=side)) + 
  geom_line() +
  geom_point() +
  theme_minimal()

ggsave("output_figures/branch_plot.pdf", plot=branch_plot,  width=10, height=10, units = "cm")


