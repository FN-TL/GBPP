
library(reshape)
library(ggplot2)

d <- read.csv("")
d1 <- as.data.frame(d)


d1

data_all = d1[, c(3,14,15,16,17,18,19,20,21)]
colnames(data_all) = c("n", "U-FFD", "U-BFD", "HS1m", "nGreedy", "GRASPm", "LNSm", "GRASP_LNS", "LNS_Local")



data_c0 = data_all[data_all$n==25, 2:9]
data_c0

data_c1 = data_all[data_all$n==50, 2:9]
data_c1

data_c2 = data_all[data_all$n==100, 2:9]
data_c2

data_c3 = data_all[data_all$n==200, 2:9]
data_c3
data_overall = data_all[, 2:9]




data_C0 <- as.data.frame(data_c0)
meltdata_0 <- melt(data_C0)

#levels(meltdata_0$variable) <- c('FFD', 'BFD', 'HS1m', 'nGreedy', 'GRASPm', 'LNSm', 'Grasp_LNS', 'LNS_local')

#create box plot with specific x-axis labels
png("C25.png")
ggplot(meltdata_0, aes(variable, value)) + 
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("25 items")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="heuristics",
       y = "percentage gap")+
  NULL
dev.off()

data_C1 <- as.data.frame(data_c1)
meltdata_1 <- melt(data_C1)

#levels(meltdata_1$variable) <- c('FFD', 'BFD', 'HS1m', 'nGreedy', 'GRASPm', 'LNSm', 'Grasp_LNS', 'LNS_local')

#create box plot with specific x-axis labels
png("C50.png")
ggplot(meltdata_1, aes(variable, value)) + 
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("50 items")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="heuristics",
       y = "percentage gap")+
  NULL
dev.off()

data_C2 <- as.data.frame(data_c2)
meltdata_2 <- melt(data_C2)
#create box plot with specific x-axis labels
png("C100.png")
ggplot(meltdata_2, aes(variable, value)) + 
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("100 items")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="heuristics",
       y = "percentage gap")+
  NULL
dev.off()

data_C3 <- as.data.frame(data_c3)
meltdata_3 <- melt(data_C3)
#create box plot with specific x-axis labels
png("C200.png")
ggplot(meltdata_3, aes(variable, value)) + 
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("200 items")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="heuristics",
       y = "percentage gap")+
  NULL
dev.off()

data_all_C <- as.data.frame(data_overall)
meltdata_all <- melt(data_all_C)
#create box plot with specific x-axis labels
png("C_all_items.png")
ggplot(meltdata_all, aes(variable, value)) + 
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ggtitle("Overall")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="heuristics",
       y = "percentage gap")+
  NULL
dev.off()



#friedman test


data_C0f <- as.matrix(data_C0)
friedman.test(data_C0f)

data_C1f <- as.matrix(data_C1)
friedman.test(data_C1f)

data_C2f <- as.matrix(data_C2)
friedman.test(data_C2f)

data_C3f <- as.matrix(data_C3)
friedman.test(data_C3f)

data_all_f <- as.matrix(data_all_C)
friedman.test(data_all_f)

#

#anova


melt12 <- melt(cbind(data_all_C$FFD, data_all_C$BFD))

a1 <- aov(value~factor(X2), data = melt12)
summary(a1)

melt13 <- melt(cbind(data_all_C$FFD, data_all_C$HS1))

a2 <- aov(value~factor(X2), data = melt13)
summary(a2)

melt14 <- melt(cbind(data_all_C$FFD, data_all_C$greedy))

a3 <- aov(value~factor(X2), data = melt14)
summary(a3)

melt15 <- melt(cbind(data_all_C$FFD, data_all_C$GRASP))

a4 <- aov(value~factor(X2), data = melt15)
summary(a4)

melt16 <- melt(cbind(data_all_C$FFD, data_all_C$LNS))

a5 <- aov(value~factor(X2), data = melt16)
summary(a5)

melt17 <- melt(cbind(data_all_C$FFD, data_all_C$GRASP_LNS))

a6 <- aov(value~factor(X2), data = melt17)
summary(a6)

melt18 <- melt(cbind(data_all_C$FFD, data_all_C$LNS_Local))

a7 <- aov(value~factor(X2), data = melt18)
summary(a7)

melt19 <- melt(cbind(data_all_C$FFD, data_all_C$LOWER_BOUND))

a8 <- aov(value~factor(X2), data = melt19)
summary(a8)



