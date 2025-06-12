# PURPOSE ####
#
# To produce a graph showing frequencies of fin ray branching in Satanoperca daemon in three experimental treatments
# This is intended to be used for Figure 2 A
#
# Date: 2023-09-08
# Author: TAS

# initialize libraries
library(ggplot2)
library(ggtree)

# load data
data = read.csv(file = "data_cleaned/fin_ray_branching.csv")  # what file?

data = data[which(data$side=='left'),]

Pel <- data[which(data$Treatment=='Pelagic'),]
Rock <- data[which(data$Treatment=='Rock'),]
Sand <- data[which(data$Treatment=='Sand'),]
Wild <- data[which(data$Treatment=='Wild'),]

# plot data
plot_plagic <- ggplot(Pel, aes(x = SL, fill=Ray3BrachP, color=Ray3BrachP))+
  xlim(4, 10)+
  ylim(0,3)+
  geom_histogram(alpha=0.5, binwidth = .25)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

plot_sand <- ggplot(Sand, aes(x = SL, fill=Ray3BrachP, color=Ray3BrachP ))+
  xlim(3, 10)+
  ylim(0,3)+
  geom_histogram( alpha=0.5, binwidth = 0.25)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

plot_rock <- ggplot(Rock, aes(x = SL, fill=Ray3BrachP, color=Ray3BrachP))+
  xlim(3, 10)+
  ylim(0,3)+
  geom_histogram(alpha=0.5, binwidth = 0.25)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


plot_wild <- ggplot(Wild, aes(x = SL, fill=Ray3BrachP, color=Ray3BrachP))+
  xlim(3, 25)+
  ylim(0,3)+
  geom_histogram(alpha=0.5, binwidth = 0.25)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

multiplot(plot_plagic, plot_sand, plot_rock, plot_wild, ncol=1)
# presently doesn't match the figure in the MS - need to update the wild to include full specimen list

pdf("output_files/fig_3bc_branching_histogram.pdf")
multiplot(plot_plagic, plot_sand, plot_rock, plot_wild, ncol=1)
dev.off()
