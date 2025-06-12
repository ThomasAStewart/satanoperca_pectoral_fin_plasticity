# Purpose: Make the box and whisker plots for segment length (Figure S2)
#
# Author: TAS
# Date: June 11, 2025

# initialize libraries
library(ggplot2)
library(gridExtra)
library(ggtree)

# load data ####
data = read.csv(file = "data_cleaned/") # todo: add data file to folder and link here
data$Treatment <- factor(data$Treatment, levels = c("Pelagic", "Sand", "Rock"))

# plot data
p1 <- ggplot(data, aes(x=Treatment, y=Radial_1_Length)) + 
  geom_boxplot(outlier.shape=NA, fill =  c("#d5efd6","#14d129","#f870f2")) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=0, height=0)) +
  xlab("")+ 
  ylab("Ray 3 Length")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

p2 <- ggplot(data, aes(x=Treatment, y=Radial_2_Length)) + 
  geom_boxplot(outlier.shape=NA, fill =  c("#d5efd6","#14d129","#f870f2")) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=0, height=0)) +
  xlab("")+ 
  ylab("Ray 4 Length")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

p3 <- ggplot(data, aes(x=Treatment, y=Radial_3_Length)) + 
  geom_boxplot(outlier.shape=NA, fill =  c("#d5efd6","#14d129","#f870f2")) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=0, height=0)) +
  xlab("")+ 
  ylab("Ray 5 Length")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

pdf("output_files/fig2A_radial_box_and_whisker.pdf")
multiplot(p1, p2, p3, ncol=4)
dev.off()
