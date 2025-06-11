library(StereoMorph)
#library(beepr)
library(geomorph)

#set working directory to the folder in which  data are held

Data = read.csv("Master_CSV.csv")
shapes <- readShapes("Shapes") #Import all our txt files for data analyses

shapesGM <- readland.shapes(shapes,
                            nCurvePts = c(7))

GPA <- gpagen(shapesGM, ProcD = FALSE)
GPA
plot(GPA)
#GPA2<-rotate.coords(GPA, "rotateCC")

summary(Data)
Data$ID <- factor(Data$ID)
Data$Treatment <- factor(Data$Treatment)
summary(Data)

#Save GPA names to GE for matching
spec.names <- dimnames(GPA$coords)[[3]]
#Same with the master file
data.names <- Data$ID
#Now match the specimens
name.match <- match(data.names,spec.names)
Data2 <- Data[name.match, ]
summary(Data2) # MAKE SURE THERE ARE NO NA's

GDF <- geomorph.data.frame(shape = GPA$coords, CS = GPA$Csize, Tr = Data2$Treatment)
summary(GDF)

par(mfrow=c(2,1))

fit1 <- procD.lm(shape ~ log(CS) * Tr, iter = 9999, RRPP = TRUE, data = GDF)
summary(fit1)
plotAllometry(fit1, size = GDF$CS, logsz = T, method = "PredLine", pch = 21,
              bg= c("#d5efd6","#f870f2", "#14d129")[as.numeric(Data2$Treatment)],cex = 1.5)
legend('bottomleft', legend = c('Pelagic', 'Rock', 'Sand'), col = c(c("#d5efd6","#f870f2", "#14d129")), pch = 19,cex = 1)

fit.null <- procD.lm(shape ~ log(CS), data = GDF,
                     iter = 9999, print.progress = FALSE) 
fit.full <- procD.lm(shape ~ log(CS) + Tr, data = GDF,
                     iter = 9999, print.progress = FALSE)

PW <- pairwise(fit.full, groups = GDF$Tr, covariate = NULL)
T1 <- summary(PW, stat.table = FALSE)
T1
write.csv(T1$pairwise.tables, "Post Stats.csv")

means = PW$LS.means[[1]] #coefficients are group means

meansA = arrayspecs(means, 9, 2)
consensus = mshape(GPA$coords)
consensus = matrix(consensus, 9, 2)

levels(Data2$Treatment)

par(mfrow=c(3,1)) #rows by columns
plotRefToTarget(consensus, meansA[,,1], mag = 3) #Pelagic
plotRefToTarget(consensus, meansA[,,2], mag = 3) #Rock
plotRefToTarget(consensus, meansA[,,3], mag = 3) #Sand


MD <- morphol.disparity(shape ~ Tr, groups = GDF$Tr, iter = 9999, data = GDF)
MD
write.csv(MD$PV.dist, "MD_Abs.csv")
write.csv(MD$PV.dist.Pval, "MD_P.csv")

par(mfrow=c(2,1))
PCA = gm.prcomp(GPA$coords)
summary(PCA)
plot(PCA$x[,2:3])
points(PCA$x[,2:3], asp=1, pch=21, 
       bg= c("#d5efd6","#f870f2", "#14d129")[as.numeric(Data2$Treatment)], 
       cex=1)
xlab <- paste("Principal Component 1", "(", round(0.674716964 *100, 1), "%)", sep="")
xlab
ylab <- paste("Principal Component 2", "(", round(0.135168185 *100, 1), "%)", sep="")
ylab


polygon.plot(PCA$x[,2:3], group = Data2$Treatment, xlab = xlab, ylab = ylab,
             cols = c("#d5efd6","#f870f2", "#14d129"),
             borders = c("#d5efd6","#f870f2", "#14d129"),
             pch = 19,
             lwd=1.5,lty=2, asp=1)
points(PCA$x, asp=1, pch=21, 
       bg= c("#d5efd6","#f870f2", "#14d129")[as.numeric(Data2$Treatment)], 
       cex=1.5)
legend('bottomleft', legend = c('Pelagic', 'Rock', 'Sand'), col = c("#d5efd6","#f870f2", "#14d129"), pch = 19, cex = 1)
pc.means2 <- aggregate(PCA$x ~ Data2$Treatment, FUN=mean)
rownames(pc.means2) <- pc.means2[,1]
pc.means2 <- pc.means2[,-1]

points(pc.means2[,1:2], asp=1, pch=24, bg=c("#d5efd6","#f870f2", "#14d129"), cex=3) 

pdf("GM Boot data.pdf")
par(mfrow =c(2,1))
plotAllometry(fit1, size = GDF$CS, logsz = T, method = "PredLine", pch = 21,
              bg= c("#d5efd6","#f870f2", "#14d129")[as.numeric(Data2$Treatment)],cex = 1.5)

polygon.plot(PCA$x, group = Data2$Treatment, xlab = xlab, ylab = ylab,
             cols = c("#55efd4","#f870f2", "#14d129"),
             borders = c("#55efd6","#f870f2", "#14d129"),
             pch = 19,
             lwd=1.5,lty=2, asp=1)
points(PCA$x, asp=1, pch=21, 
       bg= c("#55efd6","#f870f2", "#14d129")[as.numeric(Data2$Treatment)], 
       cex=1.5)
legend('bottomleft', legend = c('Pelagic', 'Rock', 'Sand'), col = c("#55efd6","#f870f2", "#14d129"), pch = 19, cex = 1)
points(pc.means2[,1:2], asp=1, pch=24, bg=c("#55efd6","#f870f2", "#14d129"), cex=3) 
dev.off()

pdf("GM Boot grids.pdf")
par(mfrow=c(3,1)) #rows by columns
plotRefToTarget(consensus, meansA[,,1], mag = 3) #Pelagic
plotRefToTarget(consensus, meansA[,,2], mag = 3) #Rock
plotRefToTarget(consensus, meansA[,,3], mag = 3) #Sand
dev.off()

library(ACBC)
par(mfrow = c(1,1))
P <- gm.prcomp(GPA$coords)$x[,1:2]
grid.preview(P, pts = 100, pt.scale = 0.1)
chc <- achc(P, std = FALSE, Data2$Treatment, iter = 999, grid.points = 100, grid.space = 0.1)
chc
summary(chc, confidence = 0.95)
plot(chc, lwd = 2)

library(rgl)
plot3d(chc$grid)

png("GM Boot grids.png")
par(mfrow=c(1,1))
plot(chc, lwd = 2)
dev.off()




par(mfrow = c(3,1))
plot(PCA$x[,1:2])
points(PCA$x[,1:2], asp=1, pch=21, 
       bg= c("#55efd4","#f870f2", "#14d129")[as.numeric(Data2$Treatment)], 
       cex=1)
xlab <- paste("Principal Component 1", "(", round(0.674716964 *100, 1), "%)", sep="")
xlab
ylab <- paste("Principal Component 2", "(", round(0.135168185 *100, 1), "%)", sep="")
ylab
plot(PCA$x[,2:3])
points(PCA$x[,2:3], asp=1, pch=21, 
       bg= c("#55efd4","#f870f2", "#14d129")[as.numeric(Data2$Treatment)], 
       cex=1)
xlab <- paste("Principal Component 1", "(", round(0.674716964 *100, 1), "%)", sep="")
xlab
ylab <- paste("Principal Component 2", "(", round(0.135168185 *100, 1), "%)", sep="")
ylab
plot(PCA$x[,3:4])
points(PCA$x[,3:4], asp=1, pch=21, 
       bg= c("#55efd4","#f870f2", "#14d129")[as.numeric(Data2$Treatment)], 
       cex=1)
xlab <- paste("Principal Component 1", "(", round(0.674716964 *100, 1), "%)", sep="")
xlab
ylab <- paste("Principal Component 2", "(", round(0.135168185 *100, 1), "%)", sep="")
ylab
