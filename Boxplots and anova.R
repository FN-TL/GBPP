library(readxl)
library(reshape)
library(ggplot2)
data_0_3 <- read_xlsx("class_0_3bintypes.xlsx")
data_0_3 <- as.data.frame(data_0_3)


meltdata_0_3 <- melt(data_0_3)
anova03 <- aov(value~factor(variable), data = meltdata_0_3)
summary(anova03)
png("C0_3B.png")
boxplot(data = meltdata_0_3, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 0, 3 bin types")
dev.off()
png("C0_3B_zoom.png")
boxplot(data = meltdata_0_3, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 0, 3 bin types", ylim = c(35, 75))
dev.off()


data_0_5 <- read_xlsx("class_0_5bintypes.xlsx")
data_0_5 <- as.data.frame(data_0_5)

meltdata_0_5 <- melt(data_0_5)
anova05 <- aov(value~factor(variable), data = meltdata_0_5)
summary(anova05)
png("C0_5B.png")
boxplot(data = meltdata_0_5, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 0, 5 bin types")
dev.off()
png("C0_5B_zoom.png")
boxplot(data = meltdata_0_5, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 0, 5 bin types", ylim = c(40, 95))
dev.off()


data_1_3 <- read_xlsx("class_1_3bintypes.xlsx")
data_1_3 <- as.data.frame(data_1_3)

meltdata_1_3 <- melt(data_1_3)
anova13 <- aov(value~factor(variable), data = meltdata_1_3)
summary(anova13)
png("C1_3B.png")
boxplot(data = meltdata_1_3, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 1, 3 bin types")
dev.off()
png("C1_3B_zoom.png")
boxplot(data = meltdata_1_3, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 1, 3 bin types", ylim = c(35,70))
dev.off()


data_1_5 <- read_xlsx("class_1_5bintypes.xlsx")
data_1_5 <- as.data.frame(data_1_5)

meltdata_1_5 <- melt(data_1_5)
anova15 <- aov(value~factor(variable), data = meltdata_1_5)
summary(anova15)
png("C1_5B.png")
boxplot(data = meltdata_1_5, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 1, 5 bin types")
dev.off()
png("C1_5B_zoom.png")
boxplot(data = meltdata_1_5, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 1, 5 bin types", ylim = c(35, 95))
dev.off()


data_2_3 <- read_xlsx("class_2_3bintypes.xlsx")
data_2_3 <- as.data.frame(data_2_3)

meltdata_2_3<- melt(data_2_3)
anova23 <- aov(value~factor(variable), data = meltdata_2_3)
summary(anova23)
png("C2_3B.png")
boxplot(data = meltdata_2_3, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 2, 3 bin types")
dev.off()
png("C2_3B_zoom.png")
boxplot(data = meltdata_2_3, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 2, 3 bin types", ylim = c(35, 70))
dev.off()

data_2_5 <- read_xlsx("class_2_5bintypes.xlsx")
data_2_5 <- as.data.frame(data_2_5)

meltdata_2_5<- melt(data_2_5)
anova25 <- aov(value~factor(variable), data = meltdata_2_5)
summary(anova25)
png("C2_5B.png")
boxplot(data = meltdata_2_5, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 2, 5 bin types")
dev.off()
png("C2_5B_zoom.png")
boxplot(data = meltdata_2_5, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 2, 5 bin types", ylim = c(35, 95))
dev.off()

data_3_3 <- read_xlsx("class_3_3bintypes.xlsx")
data_3_3 <- as.data.frame(data_3_3)

meltdata_3_3<- melt(data_3_3)
anova33 <- aov(value~factor(variable), data = meltdata_3_3)
summary(anova33)
png("C3_3B.png")
boxplot(data = meltdata_3_3, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 3, 3 bin types")
dev.off()
png("C3_3B_zoom.png")
boxplot(data = meltdata_3_3, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 3, 3 bin types", ylim = c(200, 330))
dev.off()

data_3_5 <- read_xlsx("class_3_5bintypes.xlsx")
data_3_5 <- as.data.frame(data_3_5)

meltdata_3_5<- melt(data_3_5)
anova35 <- aov(value~factor(variable), data = meltdata_3_5)
summary(anova35)

#friedman.test(y = meltdata_3_5$value, groups = meltdata_3_5$variable, blocks = )
png("C3_5B.png")
boxplot(data = meltdata_3_5, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 3, 5 bin types")
dev.off()
png("C3_5B_zoom.png")
boxplot(data = meltdata_3_5, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Class 3, 5 bin types", ylim = c(170, 370))
dev.off()

# 
# p <- ggplot(meltdata_0_3, aes(factor(variable), value))
# p+ geom_boxplot() + facet_wrap(~variable, scale = "free")
data_all <- rbind(data_0_3, data_0_5, data_1_3, data_1_5, data_2_3, data_2_5, data_3_3, data_3_5)
meltdata_all <- melt(data_all)
png("overall.png")
boxplot(data = meltdata_all, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Overall")

dev.off()
png("overall_zoom.png")
boxplot(data = meltdata_all, value~variable, xlab = "heuristics", ylab = "no. of bins", main = "Overall", ylim = c(35, 90))
dev.off()

#friedman test


data_0_3_f <- as.matrix(data_0_3)
data_0_5_f <- as.matrix(data_0_5)
data_0_f <- rbind(data_0_3_f, data_0_5_f)
friedman.test(data_0_f)

data_1_3_f <- as.matrix(data_1_3)
data_1_5_f <- as.matrix(data_1_5)
data_1_f <- rbind(data_1_3_f, data_1_5_f)
friedman.test(data_1_f)

data_2_3_f <- as.matrix(data_2_3)
data_2_5_f <- as.matrix(data_2_5)
data_2_f <- rbind(data_2_3_f, data_2_5_f)
friedman.test(data_2_f)

data_3_3_f <- as.matrix(data_3_3)
data_3_5_f <- as.matrix(data_3_5)
data_3_f <- rbind(data_3_3_f, data_3_5_f)
friedman.test(data_3_f)

data_all_f <- as.matrix(data_all)
friedman.test(data_all_f)

#write_xlsx(data_all, "~/Downloads/Stats honours/Project/r codes\\data_overall.xlsx") 


# data_all_f12 <- as.data.frame(cbind(data_all[,1], data_all[,2]))
# 
# friedman.test(data_all_f12)
# 
# data_all_f13 <- as.matrix(cbind(data_all[,1], data_all[,3]))
# 
# friedman.test(data_all_f13)
# 
# data_all_f14 <- as.matrix(cbind(data_all[,1], data_all[,4]))
# 
# friedman.test(data_all_f14)
# 
# data_all_f15 <- as.matrix(cbind(data_all[,1], data_all[,5]))
# 
# friedman.test(data_all_f15)
# 
# data_all_f16 <- as.matrix(cbind(data_all[,1], data_all[,6]))
# 
# friedman.test(data_all_f16)
# 
# data_all_f17 <- as.matrix(cbind(data_all[,1], data_all[,7]))
# 
# friedman.test(data_all_f17)
# 
# 
# data_all_f23 <- as.matrix(cbind(data_all[,2], data_all[,3]))
# 
# friedman.test(data_all_f23)

#anova


melt12 <- melt(cbind(data_all$FFD, data_all$BFD))

a1 <- aov(value~factor(X2), data = melt12)
summary(a1)

melt13 <- melt(cbind(data_all$FFD, data_all$BP))

a2 <- aov(value~factor(X2), data = melt13)
summary(a2)

melt14 <- melt(cbind(data_all$FFD, data_all$BF))

a3 <- aov(value~factor(X2), data = melt14)
summary(a3)

melt15 <- melt(cbind(data_all$FFD, data_all$FFD2))

a4 <- aov(value~factor(X2), data = melt15)
summary(a4)

melt16 <- melt(cbind(data_all$FFD, data_all$BFD2))

a5<- aov(value~factor(X2), data = melt16)
summary(a5)

melt17 <- melt(cbind(data_all$FFD, data_all$FFD3))

a6<- aov(value~factor(X2), data = melt17)
summary(a6)

# new_data_0_3 <- data_0_3[,1:2]
# melt_data_0_3 <- melt(new_data_0_3)
# melt_data_0_3 <- as.matrix(melt_data_0_3)
# friedman.test(melt_data_0_3)

 



