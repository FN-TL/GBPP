#library(readxl)

d <- read.csv("")
d1 <- as.data.frame(d)

library(DescTools)

library(reshape)

d1

data_all = d1[, c(3,4,5,6,7,8,9,10,11,12)]



data_c0 = data_all[data_all$feature_n==25, 2:10]
data_c0

data_c1 = data_all[data_all$feature_n==50, 2:10]
data_c1

data_c2 = data_all[data_all$feature_n==100, 2:10]
data_c2

data_c3 = data_all[data_all$feature_n==200, 2:10]
data_c3

data_C0 <- as.data.frame(data_c0)
meltdata_0 <- melt(data_C0)
NemenyiTest(value~ factor(variable), meltdata_0, dist = "chisq")

data_C1 <- as.data.frame(data_c1)
meltdata_1 <- melt(data_C1)
NemenyiTest(value~ factor(variable), meltdata_1)

data_C2 <- as.data.frame(data_c2)
meltdata_2 <- melt(data_C2)
NemenyiTest(value~ factor(variable), meltdata_2)

data_C3 <- as.data.frame(data_c3)
meltdata_3 <- melt(data_C3)
NemenyiTest(value~ factor(variable), meltdata_3)

data_overall = data_all[, 2:10]

data_overall_f <- as.data.frame(data_overall)
meltdata_total <- melt(data_overall_f)
NemenyiTest(value~ factor(variable), meltdata_total)



#subset
#cbind two then compare

#--------------

data_ffd_bfd <- as.data.frame(cbind(data_overall[,1], data_overall[,2]))
melt_ffd_bfd <- melt(data_ffd_bfd)
NemenyiTest(value~ factor(variable), melt_ffd_bfd)

data_ffd_bfd <- as.data.frame(cbind(data_c0[,1], data_c0[,2]))
melt_ffd_bfd <- melt(data_ffd_bfd)
NemenyiTest(value~ factor(variable), melt_ffd_bfd)

data_ffd_bfd <- as.data.frame(cbind(data_c1[,1], data_c1[,2]))
melt_ffd_bfd <- melt(data_ffd_bfd)
NemenyiTest(value~ factor(variable), melt_ffd_bfd)

data_ffd_bfd <- as.data.frame(cbind(data_c2[,1], data_c2[,2]))
melt_ffd_bfd <- melt(data_ffd_bfd)
NemenyiTest(value~ factor(variable), melt_ffd_bfd)

data_ffd_hs1m <- as.data.frame(cbind(data_overall[,1], data_overall[,3]))
melt_ffd_hs1m <- melt(data_ffd_hs1m)
NemenyiTest(value~ factor(variable), melt_ffd_hs1m)

data_ffd_hs1m <- as.data.frame(cbind(data_c0[,1], data_c0[,3]))
melt_ffd_hs1m <- melt(data_ffd_hs1m)
NemenyiTest(value~ factor(variable), melt_ffd_hs1m)

data_ffd_hs1m <- as.data.frame(cbind(data_c1[,1], data_c1[,3]))
melt_ffd_hs1m <- melt(data_ffd_hs1m)
NemenyiTest(value~ factor(variable), melt_ffd_hs1m)

data_ffd_hs1m <- as.data.frame(cbind(data_c2[,1], data_c2[,3]))
melt_ffd_hs1m <- melt(data_ffd_hs1m)
NemenyiTest(value~ factor(variable), melt_ffd_hs1m)

data_ffd_nGreedy <- as.data.frame(cbind(data_overall[,1], data_overall[,4]))
melt_ffd_nGreedy <- melt(data_ffd_nGreedy)
NemenyiTest(value~ factor(variable), melt_ffd_nGreedy)

data_ffd_nGreedy <- as.data.frame(cbind(data_c0[,1], data_c0[,4]))
melt_ffd_nGreedy <- melt(data_ffd_nGreedy)
NemenyiTest(value~ factor(variable), melt_ffd_nGreedy)

data_ffd_nGreedy <- as.data.frame(cbind(data_c1[,1], data_c1[,4]))
melt_ffd_nGreedy <- melt(data_ffd_nGreedy)
NemenyiTest(value~ factor(variable), melt_ffd_nGreedy)

data_ffd_nGreedy <- as.data.frame(cbind(data_c2[,1], data_c2[,4]))
melt_ffd_nGreedy <- melt(data_ffd_nGreedy)
NemenyiTest(value~ factor(variable), melt_ffd_nGreedy)


data_ffd_GRASP <- as.data.frame(cbind(data_overall[,1], data_overall[,5]))
melt_ffd_GRASP <- melt(data_ffd_GRASP)
NemenyiTest(value~ factor(variable), melt_ffd_GRASP)

data_ffd_GRASP <- as.data.frame(cbind(data_c0[,1], data_c0[,5]))
melt_ffd_GRASP <- melt(data_ffd_GRASP)
NemenyiTest(value~ factor(variable), melt_ffd_GRASP)

data_ffd_GRASP <- as.data.frame(cbind(data_c1[,1], data_c1[,5]))
melt_ffd_GRASP <- melt(data_ffd_GRASP)
NemenyiTest(value~ factor(variable), melt_ffd_GRASP)

data_ffd_GRASP <- as.data.frame(cbind(data_c2[,1], data_c2[,5]))
melt_ffd_GRASP <- melt(data_ffd_GRASP)
NemenyiTest(value~ factor(variable), melt_ffd_GRASP)


data_ffd_LNS <- as.data.frame(cbind(data_overall[,1], data_overall[,6]))
melt_ffd_LNS <- melt(data_ffd_LNS)
NemenyiTest(value~ factor(variable), melt_ffd_LNS)

data_ffd_LNS <- as.data.frame(cbind(data_c0[,1], data_c0[,6]))
melt_ffd_LNS <- melt(data_ffd_LNS)
NemenyiTest(value~ factor(variable), melt_ffd_LNS)

data_ffd_LNS <- as.data.frame(cbind(data_c1[,1], data_c1[,6]))
melt_ffd_LNS <- melt(data_ffd_LNS)
NemenyiTest(value~ factor(variable), melt_ffd_LNS)

data_ffd_LNS <- as.data.frame(cbind(data_c2[,1], data_c2[,6]))
melt_ffd_LNS <- melt(data_ffd_LNS)
NemenyiTest(value~ factor(variable), melt_ffd_LNS)

data_ffd_grasp_lns <- as.data.frame(cbind(data_overall[,1], data_overall[,7]))
melt_ffd_grasp_lns <- melt(data_ffd_grasp_lns)
NemenyiTest(value~ factor(variable), melt_ffd_grasp_lns)

data_ffd_grasp_lns <- as.data.frame(cbind(data_c0[,1], data_c0[,7]))
melt_ffd_grasp_lns <- melt(data_ffd_grasp_lns)
NemenyiTest(value~ factor(variable), melt_ffd_grasp_lns)

data_ffd_grasp_lns <- as.data.frame(cbind(data_c1[,1], data_c1[,7]))
melt_ffd_grasp_lns <- melt(data_ffd_grasp_lns)
NemenyiTest(value~ factor(variable), melt_ffd_grasp_lns)

data_ffd_grasp_lns <- as.data.frame(cbind(data_c2[,1], data_c2[,7]))
melt_ffd_grasp_lns <- melt(data_ffd_grasp_lns)
NemenyiTest(value~ factor(variable), melt_ffd_grasp_lns)


data_ffd_LNS_local <- as.data.frame(cbind(data_overall[,1], data_overall[,8]))
melt_ffd_LNS_local <- melt(data_ffd_LNS_local)
NemenyiTest(value~ factor(variable), melt_ffd_LNS_local)

data_ffd_LNS_local <- as.data.frame(cbind(data_c0[,1], data_c0[,8]))
melt_ffd_LNS_local <- melt(data_ffd_LNS_local)
NemenyiTest(value~ factor(variable), melt_ffd_LNS_local)

data_ffd_LNS_local <- as.data.frame(cbind(data_c1[,1], data_c1[,8]))
melt_ffd_LNS_local <- melt(data_ffd_LNS_local)
NemenyiTest(value~ factor(variable), melt_ffd_LNS_local)

data_ffd_LNS_local <- as.data.frame(cbind(data_c2[,1], data_c2[,8]))
melt_ffd_LNS_local <- melt(data_ffd_LNS_local)
NemenyiTest(value~ factor(variable), melt_ffd_LNS_local)




#---------------


data_ffd_bfd <- as.data.frame(cbind(data_overall[,1], data_overall[,2]))
melt_ffd_bfd <- melt(data_ffd_bfd)
NemenyiTest(value~ factor(variable), melt_ffd_bfd)

data_ffd_bfd <- as.data.frame(cbind(data_c0[,1], data_c0[,2]))
melt_ffd_bfd <- melt(data_ffd_bfd)
NemenyiTest(value~ factor(variable), melt_ffd_bfd)

data_ffd_bfd <- as.data.frame(cbind(data_c1[,1], data_c1[,2]))
melt_ffd_bfd <- melt(data_ffd_bfd)
NemenyiTest(value~ factor(variable), melt_ffd_bfd)

data_ffd_bfd <- as.data.frame(cbind(data_c2[,1], data_c2[,2]))
melt_ffd_bfd <- melt(data_ffd_bfd)
NemenyiTest(value~ factor(variable), melt_ffd_bfd)

#start here

data_ffd_hs1m <- as.data.frame(cbind(data_overall[,2], data_overall[,3]))
melt_ffd_hs1m <- melt(data_ffd_hs1m)
NemenyiTest(value~ factor(variable), melt_ffd_hs1m)

data_ffd_hs1m <- as.data.frame(cbind(data_c0[,2], data_c0[,3]))
melt_ffd_hs1m <- melt(data_ffd_hs1m)
NemenyiTest(value~ factor(variable), melt_ffd_hs1m)

data_ffd_hs1m <- as.data.frame(cbind(data_c1[,2], data_c1[,3]))
melt_ffd_hs1m <- melt(data_ffd_hs1m)
NemenyiTest(value~ factor(variable), melt_ffd_hs1m)

data_ffd_hs1m <- as.data.frame(cbind(data_c2[,2], data_c2[,3]))
melt_ffd_hs1m <- melt(data_ffd_hs1m)
NemenyiTest(value~ factor(variable), melt_ffd_hs1m)

data_ffd_nGreedy <- as.data.frame(cbind(data_overall[,2], data_overall[,4]))
melt_ffd_nGreedy <- melt(data_ffd_nGreedy)
NemenyiTest(value~ factor(variable), melt_ffd_nGreedy)

data_ffd_nGreedy <- as.data.frame(cbind(data_c0[,2], data_c0[,4]))
melt_ffd_nGreedy <- melt(data_ffd_nGreedy)
NemenyiTest(value~ factor(variable), melt_ffd_nGreedy)

data_ffd_nGreedy <- as.data.frame(cbind(data_c1[,2], data_c1[,4]))
melt_ffd_nGreedy <- melt(data_ffd_nGreedy)
NemenyiTest(value~ factor(variable), melt_ffd_nGreedy)

data_ffd_nGreedy <- as.data.frame(cbind(data_c2[,2], data_c2[,4]))
melt_ffd_nGreedy <- melt(data_ffd_nGreedy)
NemenyiTest(value~ factor(variable), melt_ffd_nGreedy)


data_ffd_GRASPm <- as.data.frame(cbind(data_overall[,2], data_overall[,5]))
melt_ffd_GRASPm <- melt(data_ffd_GRASPm)
NemenyiTest(value~ factor(variable), melt_ffd_GRASPm)

data_ffd_GRASPm <- as.data.frame(cbind(data_c0[,2], data_c0[,5]))
melt_ffd_GRASPm <- melt(data_ffd_GRASPm)
NemenyiTest(value~ factor(variable), melt_ffd_GRASPm)

data_ffd_GRASPm <- as.data.frame(cbind(data_c1[,2], data_c1[,5]))
melt_ffd_GRASPm <- melt(data_ffd_GRASPm)
NemenyiTest(value~ factor(variable), melt_ffd_GRASPm)

data_ffd_GRASPm <- as.data.frame(cbind(data_c2[,2], data_c2[,5]))
melt_ffd_GRASPm <- melt(data_ffd_GRASPm)
NemenyiTest(value~ factor(variable), melt_ffd_GRASPm)





data_ffd_LNSm <- as.data.frame(cbind(data_overall[,2], data_overall[,6]))
melt_ffd_LNSm <- melt(data_ffd_LNSm)
NemenyiTest(value~ factor(variable), melt_ffd_LNSm)

data_ffd_LNSm <- as.data.frame(cbind(data_c0[,2], data_c0[,6]))
melt_ffd_LNSm <- melt(data_ffd_LNSm)
NemenyiTest(value~ factor(variable), melt_ffd_LNSm)

data_ffd_LNSm <- as.data.frame(cbind(data_c1[,2], data_c1[,6]))
melt_ffd_LNSm <- melt(data_ffd_LNSm)
NemenyiTest(value~ factor(variable), melt_ffd_LNSm)

data_ffd_LNSm <- as.data.frame(cbind(data_c2[,2], data_c2[,6]))
melt_ffd_LNSm <- melt(data_ffd_LNSm)
NemenyiTest(value~ factor(variable), melt_ffd_LNSm)

data_ffd_grasp_lns <- as.data.frame(cbind(data_overall[,2], data_overall[,7]))
melt_ffd_grasp_lns <- melt(data_ffd_grasp_lns)
NemenyiTest(value~ factor(variable), melt_ffd_grasp_lns)

data_ffd_grasp_lns <- as.data.frame(cbind(data_c0[,2], data_c0[,7]))
melt_ffd_grasp_lns <- melt(data_ffd_grasp_lns)
NemenyiTest(value~ factor(variable), melt_ffd_grasp_lns)

data_ffd_grasp_lns <- as.data.frame(cbind(data_c1[,2], data_c1[,7]))
melt_ffd_grasp_lns <- melt(data_ffd_grasp_lns)
NemenyiTest(value~ factor(variable), melt_ffd_grasp_lns)

data_ffd_grasp_lns <- as.data.frame(cbind(data_c2[,2], data_c2[,7]))
melt_ffd_grasp_lns <- melt(data_ffd_grasp_lns)
NemenyiTest(value~ factor(variable), melt_ffd_grasp_lns)





data_ffd_LNS_local <- as.data.frame(cbind(data_overall[,2], data_overall[,8]))
melt_ffd_LNS_local <- melt(data_ffd_LNS_local)
NemenyiTest(value~ factor(variable), melt_ffd_LNS_local)

data_ffd_LNS_local <- as.data.frame(cbind(data_c0[,2], data_c0[,8]))
melt_ffd_LNS_local <- melt(data_ffd_LNS_local)
NemenyiTest(value~ factor(variable), melt_ffd_LNS_local)

data_ffd_LNS_local <- as.data.frame(cbind(data_c1[,2], data_c1[,8]))
melt_ffd_LNS_local <- melt(data_ffd_LNS_local)
NemenyiTest(value~ factor(variable), melt_ffd_LNS_local)

data_ffd_LNS_local <- as.data.frame(cbind(data_c2[,2], data_c2[,8]))
melt_ffd_LNS_local <- melt(data_ffd_LNS_local)
NemenyiTest(value~ factor(variable), melt_ffd_LNS_local)




#--------------

data_lb_ffd <- as.data.frame(cbind(data_overall[,9], data_overall[,1]))
melt_lb_ffd <- melt(data_lb_ffd)
NemenyiTest(value~ factor(variable), melt_lb_ffd)

data_lb_ffd <- as.data.frame(cbind(data_c0[,9], data_c0[,1]))
melt_lb_ffd <- melt(data_lb_ffd)
NemenyiTest(value~ factor(variable), melt_lb_ffd)

data_lb_ffd <- as.data.frame(cbind(data_c1[,9], data_c1[,1]))
melt_lb_ffd <- melt(data_lb_ffd)
NemenyiTest(value~ factor(variable), melt_lb_ffd)

data_lb_ffd <- as.data.frame(cbind(data_c2[,9], data_c2[,1]))
melt_lb_ffd <- melt(data_lb_ffd)
NemenyiTest(value~ factor(variable), melt_lb_ffd)

data_lb_bfd <- as.data.frame(cbind(data_overall[,9], data_overall[,2]))
melt_lb_bfd <- melt(data_lb_bfd)
NemenyiTest(value~ factor(variable), melt_lb_bfd)

data_lb_bfd <- as.data.frame(cbind(data_c0[,9], data_c0[,2]))
melt_lb_bfd <- melt(data_lb_bfd)
NemenyiTest(value~ factor(variable), melt_lb_bfd)

data_lb_bfd <- as.data.frame(cbind(data_c1[,9], data_c1[,2]))
melt_lb_bfd <- melt(data_lb_bfd)
NemenyiTest(value~ factor(variable), melt_lb_bfd)

data_lb_bfd <- as.data.frame(cbind(data_c2[,9], data_c2[,2]))
melt_lb_bfd <- melt(data_lb_bfd)
NemenyiTest(value~ factor(variable), melt_lb_bfd)

data_lb_hs1m <- as.data.frame(cbind(data_overall[,9], data_overall[,3]))
melt_lb_hs1m <- melt(data_lb_hs1m)
NemenyiTest(value~ factor(variable), melt_lb_hs1m)

data_lb_hs1m <- as.data.frame(cbind(data_c0[,9], data_c0[,3]))
melt_lb_hs1m <- melt(data_lb_hs1m)
NemenyiTest(value~ factor(variable), melt_lb_hs1m)

data_lb_hs1m <- as.data.frame(cbind(data_c1[,9], data_c1[,3]))
melt_lb_hs1m <- melt(data_lb_hs1m)
NemenyiTest(value~ factor(variable), melt_lb_hs1m)

data_lb_hs1m <- as.data.frame(cbind(data_c2[,9], data_c2[,3]))
melt_lb_hs1m <- melt(data_lb_hs1m)
NemenyiTest(value~ factor(variable), melt_lb_hs1m)

data_lb_nGreedy <- as.data.frame(cbind(data_overall[,9], data_overall[,4]))
melt_lb_nGreedy <- melt(data_lb_nGreedy)
NemenyiTest(value~ factor(variable), melt_lb_nGreedy)

data_lb_nGreedy <- as.data.frame(cbind(data_c0[,9], data_c0[,4]))
melt_lb_nGreedy <- melt(data_lb_nGreedy)
NemenyiTest(value~ factor(variable), melt_lb_nGreedy)

data_lb_nGreedy <- as.data.frame(cbind(data_c1[,9], data_c1[,4]))
melt_lb_nGreedy <- melt(data_lb_nGreedy)
NemenyiTest(value~ factor(variable), melt_lb_nGreedy)

data_lb_nGreedy <- as.data.frame(cbind(data_c2[,9], data_c2[,4]))
melt_lb_nGreedy <- melt(data_lb_nGreedy)
NemenyiTest(value~ factor(variable), melt_lb_nGreedy)


data_lb_GRASPm <- as.data.frame(cbind(data_overall[,9], data_overall[,5]))
melt_lb_GRASPm <- melt(data_lb_GRASPm)
NemenyiTest(value~ factor(variable), melt_lb_GRASPm)

data_lb_GRASPm <- as.data.frame(cbind(data_c0[,9], data_c0[,5]))
melt_lb_GRASPm <- melt(data_lb_GRASPm)
NemenyiTest(value~ factor(variable), melt_lb_GRASPm)

data_lb_GRASPm <- as.data.frame(cbind(data_c1[,9], data_c1[,5]))
melt_lb_GRASPm <- melt(data_lb_GRASPm)
NemenyiTest(value~ factor(variable), melt_lb_GRASPm)

data_lb_GRASPm <- as.data.frame(cbind(data_c2[,9], data_c2[,5]))
melt_lb_GRASPm <- melt(data_lb_GRASPm)
NemenyiTest(value~ factor(variable), melt_lb_GRASPm)





data_lb_LNSm <- as.data.frame(cbind(data_overall[,9], data_overall[,6]))
melt_lb_LNSm <- melt(data_lb_LNSm)
NemenyiTest(value~ factor(variable), melt_lb_LNSm)

data_lb_LNSm <- as.data.frame(cbind(data_c0[,9], data_c0[,6]))
melt_lb_LNSm <- melt(data_lb_LNSm)
NemenyiTest(value~ factor(variable), melt_lb_LNSm)

data_lb_LNSm <- as.data.frame(cbind(data_c1[,9], data_c1[,6]))
melt_lb_LNSm <- melt(data_lb_LNSm)
NemenyiTest(value~ factor(variable), melt_lb_LNSm)

data_lb_LNSm <- as.data.frame(cbind(data_c2[,9], data_c2[,6]))
melt_lb_LNSm <- melt(data_lb_LNSm)
NemenyiTest(value~ factor(variable), melt_lb_LNSm)

data_lb_grasp_lns <- as.data.frame(cbind(data_overall[,9], data_overall[,7]))
melt_lb_grasp_lns <- melt(data_lb_grasp_lns)
NemenyiTest(value~ factor(variable), melt_lb_grasp_lns)

data_lb_grasp_lns <- as.data.frame(cbind(data_c0[,9], data_c0[,7]))
melt_lb_grasp_lns <- melt(data_lb_grasp_lns)
NemenyiTest(value~ factor(variable), melt_lb_grasp_lns)

data_lb_grasp_lns <- as.data.frame(cbind(data_c1[,9], data_c1[,7]))
melt_lb_grasp_lns <- melt(data_lb_grasp_lns)
NemenyiTest(value~ factor(variable), melt_lb_grasp_lns)

data_lb_grasp_lns <- as.data.frame(cbind(data_c2[,9], data_c2[,7]))
melt_lb_grasp_lns <- melt(data_lb_grasp_lns)
NemenyiTest(value~ factor(variable), melt_lb_grasp_lns)





data_lb_LNS_local <- as.data.frame(cbind(data_overall[,9], data_overall[,8]))
melt_lb_LNS_local <- melt(data_lb_LNS_local)
NemenyiTest(value~ factor(variable), melt_lb_LNS_local)

data_lb_LNS_local <- as.data.frame(cbind(data_c0[,9], data_c0[,8]))
melt_lb_LNS_local <- melt(data_lb_LNS_local)
NemenyiTest(value~ factor(variable), melt_lb_LNS_local)

data_lb_LNS_local <- as.data.frame(cbind(data_c1[,9], data_c1[,8]))
melt_lb_LNS_local <- melt(data_lb_LNS_local)
NemenyiTest(value~ factor(variable), melt_lb_LNS_local)

data_lb_LNS_local <- as.data.frame(cbind(data_c2[,9], data_c2[,8]))
melt_lb_LNS_local <- melt(data_lb_LNS_local)
NemenyiTest(value~ factor(variable), melt_lb_LNS_local)



