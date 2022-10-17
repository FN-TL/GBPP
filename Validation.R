library(readxl)

d <- read_xlsx("data_overall.xlsx")
d1 <- as.data.frame(d)

library(DescTools)

library(reshape)



data_0_f_1 <- as.data.frame(data_0_f)
meltdata_0 <- melt(data_0_f_1)
NemenyiTest(value~ factor(variable), meltdata_0, dist = "chisq")
NemenyiTest(value~ factor(variable), meltdata_0_3)
NemenyiTest(value~ factor(variable), meltdata_0_5)



data_1_f_1 <- as.data.frame(data_1_f)
meltdata_1 <- melt(data_1_f_1)
NemenyiTest(value~ factor(variable), meltdata_1)
NemenyiTest(value~ factor(variable), meltdata_1_3)
NemenyiTest(value~ factor(variable), meltdata_1_5)



data_2_f_1 <- as.data.frame(data_2_f)
meltdata_2 <- melt(data_2_f_1)
NemenyiTest(value~ factor(variable), meltdata_2)
NemenyiTest(value~ factor(variable), meltdata_2_3)
NemenyiTest(value~ factor(variable), meltdata_2_5)



data_3_f_1 <- as.data.frame(data_3_f)
meltdata_3 <- melt(data_3_f_1)
NemenyiTest(value~ factor(variable), meltdata_3)
NemenyiTest(value~ factor(variable), meltdata_3_3)
NemenyiTest(value~ factor(variable), meltdata_3_5)



data_overall_f <- as.data.frame(d1)
meltdata_total <- melt(d1)
NemenyiTest(value~ factor(variable), meltdata_total)






#subset
#cbind two then compare



#--------------



data_ffd_bp <- as.data.frame(cbind(d1[,1], d1[,3]))
melt_ffd_bp <- melt(data_ffd_bp)
NemenyiTest(value~ factor(variable), melt_ffd_bp)

data_ffd_bf <- as.data.frame(cbind(d1[,1], d1[,4]))
melt_ffd_bf <- melt(data_ffd_bf)
NemenyiTest(value~ factor(variable), melt_ffd_bf)

data_ffd_ffd1 <- as.data.frame(cbind(d1[,1], d1[,5]))
melt_ffd_ffd1 <- melt(data_ffd_ffd1)
NemenyiTest(value~ factor(variable), melt_ffd_ffd1)

data_ffd_bfd1 <- as.data.frame(cbind(d1[,1], d1[,6]))
melt_ffd_bfd1 <- melt(data_ffd_bfd1)
NemenyiTest(value~ factor(variable), melt_ffd_bfd1)

data_ffd_ffd3 <- as.data.frame(cbind(d1[,1], d1[,7]))
melt_ffd_ffd3 <- melt(data_ffd_ffd3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3)


#Classes 

 
#-----------

#FFD & FFD3

data_ffd_ffd3_3 <- as.data.frame(cbind(data_3_f_1[,1], data_3_f_1[,7]))
melt_ffd_ffd3_3 <- melt(data_ffd_ffd3_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_3)

data_ffd_ffd3_2 <- as.data.frame(cbind(data_2_f_1[,1], data_2_f_1[,7]))
melt_ffd_ffd3_2 <- melt(data_ffd_ffd3_2)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_2)

data_ffd_ffd3_1 <- as.data.frame(cbind(data_1_f_1[,1], data_1_f_1[,7]))
melt_ffd_ffd3_1 <- melt(data_ffd_ffd3_1)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_1)

data_ffd_ffd3_0 <- as.data.frame(cbind(data_0_f_1[,1], data_0_f_1[,7]))
melt_ffd_ffd3_0 <- melt(data_ffd_ffd3_0)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_0)



#FFD & BFD2

data_ffd_bfd2_3 <- as.data.frame(cbind(data_3_f_1[,1], data_3_f_1[,6]))
melt_ffd_bfd2_3 <- melt(data_ffd_bfd2_3)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_3)

data_ffd_bfd2_2 <- as.data.frame(cbind(data_2_f_1[,1], data_2_f_1[,6]))
melt_ffd_bfd2_2 <- melt(data_ffd_bfd2_2)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_2)

data_ffd_bfd2_1 <- as.data.frame(cbind(data_1_f_1[,1], data_1_f_1[,6]))
melt_ffd_bfd2_1 <- melt(data_ffd_bfd2_1)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_1)

data_ffd_bfd2_0 <- as.data.frame(cbind(data_0_f_1[,1], data_0_f_1[,6]))
melt_ffd_bfd2_0 <- melt(data_ffd_bfd2_0)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_0)



#FFD & FFD2

data_ffd_ffd2_3 <- as.data.frame(cbind(data_3_f_1[,1], data_3_f_1[,5]))
melt_ffd_ffd2_3 <- melt(data_ffd_ffd2_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_3)

data_ffd_ffd2_2 <- as.data.frame(cbind(data_2_f_1[,1], data_2_f_1[,5]))
melt_ffd_ffd2_2 <- melt(data_ffd_ffd2_2)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_2)

data_ffd_ffd2_1 <- as.data.frame(cbind(data_1_f_1[,1], data_1_f_1[,5]))
melt_ffd_ffd2_1 <- melt(data_ffd_ffd2_1)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_1)

data_ffd_ffd2_0 <- as.data.frame(cbind(data_0_f_1[,1], data_0_f_1[,5]))
melt_ffd_ffd2_0 <- melt(data_ffd_ffd2_0)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_0)




#FFD & BF

data_ffd_bf_3 <- as.data.frame(cbind(data_3_f_1[,1], data_3_f_1[,4]))
melt_ffd_bf_3 <- melt(data_ffd_bf_3)
NemenyiTest(value~ factor(variable), melt_ffd_bf_3)

data_ffd_bf_2 <- as.data.frame(cbind(data_2_f_1[,1], data_2_f_1[,4]))
melt_ffd_bf_2 <- melt(data_ffd_bf_2)
NemenyiTest(value~ factor(variable), melt_ffd_bf_2)

data_ffd_bf_1 <- as.data.frame(cbind(data_1_f_1[,1], data_1_f_1[,4]))
melt_ffd_bf_1 <- melt(data_ffd_bf_1)
NemenyiTest(value~ factor(variable), melt_ffd_bf_1)

data_ffd_bf_0 <- as.data.frame(cbind(data_0_f_1[,1], data_0_f_1[,4]))
melt_ffd_bf_0 <- melt(data_ffd_bf_0)
NemenyiTest(value~ factor(variable), melt_ffd_bf_0)



#FFD & BP

data_ffd_bp_3 <- as.data.frame(cbind(data_3_f_1[,1], data_3_f_1[,3]))
melt_ffd_bp_3 <- melt(data_ffd_bp_3)
NemenyiTest(value~ factor(variable), melt_ffd_bp_3)

data_ffd_bp_2 <- as.data.frame(cbind(data_2_f_1[,1], data_2_f_1[,3]))
melt_ffd_bp_2 <- melt(data_ffd_bp_2)
NemenyiTest(value~ factor(variable), melt_ffd_bp_2)

data_ffd_bp_1 <- as.data.frame(cbind(data_1_f_1[,1], data_1_f_1[,3]))
melt_ffd_bp_1 <- melt(data_ffd_bp_1)
NemenyiTest(value~ factor(variable), melt_ffd_bp_1)

data_ffd_bp_0 <- as.data.frame(cbind(data_0_f_1[,1], data_0_f_1[,3]))
melt_ffd_bp_0 <- melt(data_ffd_bp_0)
NemenyiTest(value~ factor(variable), melt_ffd_bp_0)



#FFD & BFD

data_ffd_bfd_3 <- as.data.frame(cbind(data_3_f_1[,1], data_3_f_1[,2]))
melt_ffd_bfd_3 <- melt(data_ffd_bfd_3)
NemenyiTest(value~ factor(variable), melt_ffd_bfd_3)

data_ffd_bfd_2 <- as.data.frame(cbind(data_2_f_1[,1], data_2_f_1[,2]))
melt_ffd_bfd_2 <- melt(data_ffd_bfd_2)
NemenyiTest(value~ factor(variable), melt_ffd_bfd_2)

data_ffd_bfd_1 <- as.data.frame(cbind(data_1_f_1[,1], data_1_f_1[,2]))
melt_ffd_bfd_1 <- melt(data_ffd_bfd_1)
NemenyiTest(value~ factor(variable), melt_ffd_bfd_1)

data_ffd_bfd_0 <- as.data.frame(cbind(data_0_f_1[,1], data_0_f_1[,2]))
melt_ffd_bfd_0 <- melt(data_ffd_bfd_0)
NemenyiTest(value~ factor(variable), melt_ffd_bfd_0)


#Bin types


#-----------------

# 3B

data_ffd_ffd3_3_3 <- as.data.frame(cbind(data_3_3_f[,1], data_3_3_f[,7]))
melt_ffd_ffd3_3_3 <- melt(data_ffd_ffd3_3_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_3_3)

data_ffd_ffd3_2_3 <- as.data.frame(cbind(data_2_3_f[,1], data_2_3_f[,7]))
melt_ffd_ffd3_2_3 <- melt(data_ffd_ffd3_2_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_2_3)

data_ffd_ffd3_1_3 <- as.data.frame(cbind(data_1_3_f[,1], data_1_3_f[,7]))
melt_ffd_ffd3_1_3 <- melt(data_ffd_ffd3_1_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_1_3)

data_ffd_ffd3_0_3 <- as.data.frame(cbind(data_0_3_f[,1], data_0_3_f[,7]))
melt_ffd_ffd3_0_3 <- melt(data_ffd_ffd3_0_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_0_3)




#FFD & BFD2

data_ffd_bfd2_3_3 <- as.data.frame(cbind(data_3_3_f[,1], data_3_3_f[,6]))
melt_ffd_bfd2_3_3 <- melt(data_ffd_bfd2_3_3)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_3_3)

data_ffd_bfd2_2_3 <- as.data.frame(cbind(data_2_3_f[,1], data_2_3_f[,6]))
melt_ffd_bfd2_2_3 <- melt(data_ffd_bfd2_2_3)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_2_3)

data_ffd_bfd2_1_3 <- as.data.frame(cbind(data_1_3_f[,1], data_1_3_f[,6]))
melt_ffd_bfd2_1_3 <- melt(data_ffd_bfd2_1_3)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_1_3)

data_ffd_bfd2_0_3 <- as.data.frame(cbind(data_0_3_f[,1], data_0_3_f[,6]))
melt_ffd_bfd2_0_3 <- melt(data_ffd_bfd2_0_3)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_0_3)



#FFD & FFD2

data_ffd_ffd2_3_3 <- as.data.frame(cbind(data_3_3_f[,1], data_3_3_f[,5]))
melt_ffd_ffd2_3_3 <- melt(data_ffd_ffd2_3_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_3_3)

data_ffd_ffd2_2_3 <- as.data.frame(cbind(data_2_3_f[,1], data_2_3_f[,5]))
melt_ffd_ffd2_2_3 <- melt(data_ffd_ffd2_2_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_2_3)

data_ffd_ffd2_1_3 <- as.data.frame(cbind(data_1_3_f[,1], data_1_3_f[,5]))
melt_ffd_ffd2_1_3 <- melt(data_ffd_ffd2_1_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_1_3)

data_ffd_ffd2_0_3 <- as.data.frame(cbind(data_0_3_f[,1], data_0_3_f[,5]))
melt_ffd_ffd2_0_3 <- melt(data_ffd_ffd2_0_3)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_0_3)




#FFD & BF

data_ffd_bf_3_3 <- as.data.frame(cbind(data_3_3_f[,1], data_3_3_f[,4]))
melt_ffd_bf_3_3 <- melt(data_ffd_bf_3_3)
NemenyiTest(value~ factor(variable), melt_ffd_bf_3_3)

data_ffd_bf_2_3 <- as.data.frame(cbind(data_2_3_f[,1], data_2_3_f[,4]))
melt_ffd_bf_2_3 <- melt(data_ffd_bf_2_3)
NemenyiTest(value~ factor(variable), melt_ffd_bf_2_3)

data_ffd_bf_1_3 <- as.data.frame(cbind(data_1_3_f[,1], data_1_3_f[,4]))
melt_ffd_bf_1_3 <- melt(data_ffd_bf_1_3)
NemenyiTest(value~ factor(variable), melt_ffd_bf_1_3)

data_ffd_bf_0_3 <- as.data.frame(cbind(data_0_3_f[,1], data_0_3_f[,4]))
melt_ffd_bf_0_3 <- melt(data_ffd_bf_0_3)
NemenyiTest(value~ factor(variable), melt_ffd_bf_0_3)



#FFD & BP

data_ffd_bp_3_3 <- as.data.frame(cbind(data_3_3_f[,1], data_3_3_f[,3]))
melt_ffd_bp_3_3 <- melt(data_ffd_bp_3_3)
NemenyiTest(value~ factor(variable), melt_ffd_bp_3_3)

data_ffd_bp_2_3 <- as.data.frame(cbind(data_2_3_f[,1], data_2_3_f[,3]))
melt_ffd_bp_2_3 <- melt(data_ffd_bp_2_3)
NemenyiTest(value~ factor(variable), melt_ffd_bp_2_3)

data_ffd_bp_1_3 <- as.data.frame(cbind(data_1_3_f[,1], data_1_3_f[,3]))
melt_ffd_bp_1_3 <- melt(data_ffd_bp_1_3)
NemenyiTest(value~ factor(variable), melt_ffd_bp_1_3)

data_ffd_bp_0_3 <- as.data.frame(cbind(data_0_3_f[,1], data_0_3_f[,3]))
melt_ffd_bp_0_3 <- melt(data_ffd_bp_0_3)
NemenyiTest(value~ factor(variable), melt_ffd_bp_0_3)





# 5B

data_ffd_ffd3_3_5 <- as.data.frame(cbind(data_3_5_f[,1], data_3_5_f[,7]))
melt_ffd_ffd3_3_5 <- melt(data_ffd_ffd3_3_5)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_3_5)

data_ffd_ffd3_2_5 <- as.data.frame(cbind(data_2_5_f[,1], data_2_5_f[,7]))
melt_ffd_ffd3_2_5 <- melt(data_ffd_ffd3_2_5)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_2_5)

data_ffd_ffd3_1_5 <- as.data.frame(cbind(data_1_5_f[,1], data_1_5_f[,7]))
melt_ffd_ffd3_1_5 <- melt(data_ffd_ffd3_1_5)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_1_5)

data_ffd_ffd3_0_5 <- as.data.frame(cbind(data_0_5_f[,1], data_0_5_f[,7]))
melt_ffd_ffd3_0_5<- melt(data_ffd_ffd3_0_5)
NemenyiTest(value~ factor(variable), melt_ffd_ffd3_0_5)


#FFD & BFD2

data_ffd_bfd2_3_5 <- as.data.frame(cbind(data_3_5_f[,1], data_3_5_f[,6]))
melt_ffd_bfd2_3_5 <- melt(data_ffd_bfd2_3_5)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_3_5)

data_ffd_bfd2_2_5 <- as.data.frame(cbind(data_2_5_f[,1], data_2_5_f[,6]))
melt_ffd_bfd2_2_5 <- melt(data_ffd_bfd2_2_5)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_2_5)

data_ffd_bfd2_1_5 <- as.data.frame(cbind(data_1_5_f[,1], data_1_5_f[,6]))
melt_ffd_bfd2_1_5 <- melt(data_ffd_bfd2_1_5)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_1_5)

data_ffd_bfd2_0_5 <- as.data.frame(cbind(data_0_5_f[,1], data_0_5_f[,6]))
melt_ffd_bfd2_0_5 <- melt(data_ffd_bfd2_0_5)
NemenyiTest(value~ factor(variable), melt_ffd_bfd2_0_5)



#FFD & FFD2

data_ffd_ffd2_3_5 <- as.data.frame(cbind(data_3_5_f[,1], data_3_5_f[,5]))
melt_ffd_ffd2_3_5 <- melt(data_ffd_ffd2_3_5)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_3_5)

data_ffd_ffd2_2_5 <- as.data.frame(cbind(data_2_5_f[,1], data_2_5_f[,5]))
melt_ffd_ffd2_2_5 <- melt(data_ffd_ffd2_2_5)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_2_5)

data_ffd_ffd2_1_5 <- as.data.frame(cbind(data_1_5_f[,1], data_1_5_f[,5]))
melt_ffd_ffd2_1_5 <- melt(data_ffd_ffd2_1_5)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_1_5)

data_ffd_ffd2_0_5 <- as.data.frame(cbind(data_0_5_f[,1], data_0_5_f[,5]))
melt_ffd_ffd2_0_5 <- melt(data_ffd_ffd2_0_5)
NemenyiTest(value~ factor(variable), melt_ffd_ffd2_0_5)




#FFD & BF

data_ffd_bf_3_5 <- as.data.frame(cbind(data_3_5_f[,1], data_3_5_f[,4]))
melt_ffd_bf_3_5 <- melt(data_ffd_bf_3_5)
NemenyiTest(value~ factor(variable), melt_ffd_bf_3_5)

data_ffd_bf_2_5 <- as.data.frame(cbind(data_2_5_f[,1], data_2_5_f[,4]))
melt_ffd_bf_2_5 <- melt(data_ffd_bf_2_5)
NemenyiTest(value~ factor(variable), melt_ffd_bf_2_5)

data_ffd_bf_1_5 <- as.data.frame(cbind(data_1_5_f[,1], data_1_5_f[,4]))
melt_ffd_bf_1_5 <- melt(data_ffd_bf_1_5)
NemenyiTest(value~ factor(variable), melt_ffd_bf_1_5)

data_ffd_bf_0_5 <- as.data.frame(cbind(data_0_5_f[,1], data_0_5_f[,4]))
melt_ffd_bf_0_5 <- melt(data_ffd_bf_0_5)
NemenyiTest(value~ factor(variable), melt_ffd_bf_0_5)



#FFD & BP

data_ffd_bp_3_5 <- as.data.frame(cbind(data_3_5_f[,1], data_3_5_f[,3]))
melt_ffd_bp_3_5 <- melt(data_ffd_bp_3_5)
NemenyiTest(value~ factor(variable), melt_ffd_bp_3_5)

data_ffd_bp_2_5 <- as.data.frame(cbind(data_2_5_f[,1], data_2_5_f[,3]))
melt_ffd_bp_2_5 <- melt(data_ffd_bp_2_5)
NemenyiTest(value~ factor(variable), melt_ffd_bp_2_5)

data_ffd_bp_1_5 <- as.data.frame(cbind(data_1_5_f[,1], data_1_5_f[,3]))
melt_ffd_bp_1_5 <- melt(data_ffd_bp_1_5)
NemenyiTest(value~ factor(variable), melt_ffd_bp_1_5)

data_ffd_bp_0_5 <- as.data.frame(cbind(data_0_5_f[,1], data_0_5_f[,3]))
melt_ffd_bp_0_5 <- melt(data_ffd_bp_0_5)
NemenyiTest(value~ factor(variable), melt_ffd_bp_0_5)






