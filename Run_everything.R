
 list_of_instances <-  list.files(path = "") #preferably each class, and bin types separately
 
 instance_counter <- 1
 no_of_instances <- length(list_of_instances)
 while (instance_counter <= no_of_instances){

  
dat<- read.csv(list_of_instances[instance_counter]) #reads in 1 data instances at a time


# FIRST FIT DECREASING...

dat<- dat #data instance




#takeout item related info.
listed = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed<- c(dat[11:nrow(dat),])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed<- c(dat[9:nrow(dat),])
}
new_lis <- c()
new_check_comp <- c()
new_check_profit <- c()
dat_C<- c()

#takeout bin related info.
listed1 = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed1<- c(dat[5:9,])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed1<- c(dat[5:7,])
}

#data frame of bin related info.
new_lis1 <- c()
new_check_cap <- c()
for (i in 1:length(listed1)){
  y1<-strsplit(listed1[i], split=" ")
  lis1<- as.numeric(y1[[1]][5])
  new_lis1 <- append(new_lis1, lis1)
  lis2<- as.numeric(y1[[1]][2])
  new_check_cap <- append(new_check_cap, lis2)
  dat1 <- as.data.frame(cbind(new_lis1, new_check_cap))
  colnames(dat1)[1] <- "numm" 
  colnames(dat1)[2] <- "Cap"
}
num<- dat1[,1]

num_initial <- dat1[,1]

capacities<- dat1[,2]
# 60, 80, 100,120,150
B <- sum(num)
type<- rep(capacities,num)

#data frame of item related info
for(k in 1:length(listed)){
  y<-strsplit(listed[k], split=" ")
  lis<- as.numeric(y[[1]][2])
  new_lis <- append(new_lis, lis)
  check_comp <- as.numeric(y[[1]][4])
  new_check_comp <- append(new_check_comp, check_comp)
  check_profit <- as.numeric(y[[1]][3])
  new_check_profit <- append(new_check_profit, check_profit)
  data_to_use <- as.data.frame(cbind(new_lis, new_check_comp, new_check_profit))
  colnames(data_to_use)[1] <- "Cost" 
  colnames(data_to_use)[2] <- "check_item"
  colnames(data_to_use)[3] <- "profit"
}

dat_C <- data_to_use[data_to_use$check_item ==1, 1]#compulsory items
count_compulsory <- length(dat_C)
dat_nC<- data_to_use[data_to_use$check_item == 0, 1] #non-compulsory items
count_non_compulsory <- length(dat_nC)
dat_nc_profit <- data_to_use[data_to_use$check_item == 0, 3] #profit of non-compulsory items
dat_comp_profit <- data_to_use[data_to_use$check_item == 1, 3]

dat_C<-sort(dat_C,decreasing = TRUE)

bin_used <- c() 

#first pack compulsory items if there are some...

while(length(dat_C)>0){
  #choose a bin which the items can fit in
  chosen_bin <- which(bin_used-dat_C[1]>=0) 
  if (length(chosen_bin)==0){ 
    type_chosen<- (which(capacities-dat_C[1]>=0)) # checks new bins which the item can fit
    sel_num <- which(num[type_chosen]>0) # checks index of the smallest bin the item can fit in the bins still available
    selected<- type_chosen[sel_num[1]] # takes the smallest bin the item can fit in the bins still available
    
    bin_used<-append(bin_used, (capacities[selected]-dat_C[1]))
    num[selected]<- num[selected]-1

    
  }else{ # adding items to already present bins
    
    chosen_b<-bin_used[chosen_bin] # list of bins the item can fit
    fitted_bin<-which(bin_used==chosen_b[1]) # take the first one of those
    bin_used[fitted_bin[1]]<- chosen_b[1]-dat_C[1] # fit the item in
  }
  dat_C<- dat_C[-1] #remove that item from list
}

# non-compulsory items


#start with the updated number of bins for each capacities
non_comp_dat <- cbind(dat_nC, dat_nc_profit)
non_comp_dat <- non_comp_dat[order(dat_nC, decreasing = TRUE),]
dat_nC <- c(non_comp_dat[,1],0) #the 0 is to ensure that the sum of profit from current item to last item runs without crushing when left with 1 tem
dat_nc_profit <- c(non_comp_dat[,2],0)
num_updated <- num
contenders <- c() 
contenders_profit <- c()
non_comp_profit <- c()

if (length(dat_nC) != 1 ) {
  for (i in 1:(length(dat_nC)-1)) {
    
    chosen_bin <- which(bin_used-dat_nC[i]>=0)
    
    if (length(chosen_bin)==0) {
      type_chosen<- (which(capacities-dat_nC[i]>=0))
      sel_num <- (which(num[type_chosen]>0))
      selected<- type_chosen[sel_num[1]]
      if (sum(dat_nc_profit[i], dat_nc_profit[(i+1):length(dat_nC)]) - capacities[selected] >=0){
        bin_used<-append(bin_used, (capacities[selected]-dat_nC[i]))
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        #items_left = items_left - 1
        num[selected]<- num[selected]-1
        
      }
    }
    else {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==chosen_b[1])
      contenders <- append(contenders, dat_nC[i])
      contenders_profit <- append(contenders_profit, dat_nc_profit[i])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (chosen_b[1] - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (chosen_b[1] - contenders[item_to_choose[item_chosen]])
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
    
    
  }
  dat_nC <- dat_nC[- which(dat_nC == 0)]
  dat_nc_profit <- dat_nc_profit[- which(dat_nc_profit == 0)]
}

non_comp_profit <- append(non_comp_profit, 0)
non_comp_profit <- non_comp_profit[- which(non_comp_profit == 0)] #no need really

no_of_bins <- num_initial - num
total_bins <- sum(no_of_bins)

cost_of_bins = sum(no_of_bins * capacities)

profit = sum(dat_comp_profit, non_comp_profit)

total_cost <- cost_of_bins - profit

#count_compulsory
#count_non_compulsory
items <- sum(count_compulsory, count_non_compulsory)
#bin_used


no_of_non_compulsory_items_left <- length(dat_nC)
dat_nC


#print(paste0( 'used ', total_bins , ' bins out of ', B ))

FFD_b <- append(FFD_b, total_cost)  #total_bins when considering number of bins used

FFD <- as.data.frame(FFD_b)




# Best FIT DECREASING...

dat<- dat





listed = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed<- c(dat[11:nrow(dat),])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed<- c(dat[9:nrow(dat),])
}
new_lis <- c()
new_check_comp <- c()
new_check_profit <- c()
dat_C<- c()

listed1 = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed1<- c(dat[5:9,])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed1<- c(dat[5:7,])
}

new_lis1 <- c()
new_check_cap <- c()
for (i in 1:length(listed1)){
  y1<-strsplit(listed1[i], split=" ")
  lis1<- as.numeric(y1[[1]][5])
  new_lis1 <- append(new_lis1, lis1)
  lis2<- as.numeric(y1[[1]][2])
  new_check_cap <- append(new_check_cap, lis2)
  dat1 <- as.data.frame(cbind(new_lis1, new_check_cap))
  colnames(dat1)[1] <- "numm" 
  colnames(dat1)[2] <- "Cap"
}
num<- dat1[,1]

num_initial <- dat1[,1]

capacities<- dat1[,2]
# 60, 80, 100,120,150
B <- sum(num)
type<- rep(capacities,num)

#taking out the data
for(k in 1:length(listed)){
  y<-strsplit(listed[k], split=" ")
  lis<- as.numeric(y[[1]][2])
  new_lis <- append(new_lis, lis)
  check_comp <- as.numeric(y[[1]][4])
  new_check_comp <- append(new_check_comp, check_comp)
  check_profit <- as.numeric(y[[1]][3])
  new_check_profit <- append(new_check_profit, check_profit)
  data_to_use <- as.data.frame(cbind(new_lis, new_check_comp, new_check_profit))
  colnames(data_to_use)[1] <- "Cost" 
  colnames(data_to_use)[2] <- "check_item"
  colnames(data_to_use)[3] <- "profit"
}

dat_C <- data_to_use[data_to_use$check_item ==1, 1]#compulsory items
count_compulsory <- length(dat_C)
dat_nC<- data_to_use[data_to_use$check_item == 0, 1] #non-compulsory items
count_non_compulsory <- length(dat_nC)
dat_nc_profit <- data_to_use[data_to_use$check_item == 0, 3] #profit of non-compulsory items
dat_comp_profit <- data_to_use[data_to_use$check_item == 1, 3]

dat_C<-sort(dat_C,decreasing = TRUE)

bin_used <- c() 

#first pack compulsory items if there are some...

while(length(dat_C)>0){
  #choose a bin which the items can fit in
  chosen_bin <- which(bin_used-dat_C[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_C[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
    
    # if there are no bins which can fit the item take a bin out from the available and subtract this bin from the number availabe
    #if(num[type_chosen]>0){
    bin_used<-append(bin_used, (capacities[selected]-dat_C[1]))
    num[selected]<- num[selected]-1
    # }else{
    # bins<- append(bins,(capacities[type_chosen]-dat_C[1]))
    # num[type_chosen]<- num[type_chosen]-1
    #}
    
  }else{
    
    
    chosen_b<-bin_used[chosen_bin]
    fitted_bin<-which(bin_used==min(chosen_b))
    bin_used[fitted_bin[1]]<- min(chosen_b)-dat_C[1]
  }
  dat_C<- dat_C[-1]
}

# non-compulsory items


#start with the updated number of bins for each capacities
non_comp_dat <- cbind(dat_nC, dat_nc_profit)
non_comp_dat <- non_comp_dat[order(dat_nC, decreasing = TRUE),]
dat_nC <- c(non_comp_dat[,1], 0) #the 0 is to ensure that the sum of profit from current item to last item runs without crushing when left with 1 tem
dat_nc_profit <- c(non_comp_dat[,2], 0)
num_updated <- num
contenders <- c() 
contenders_profit <- c()
non_comp_profit <- c()

if (length(dat_nC) != 1) {
  for (i in 1:(length(dat_nC)-1)) {
    
    chosen_bin <- which(bin_used-dat_nC[i]>=0)
    
    if (length(chosen_bin)==0) {
      type_chosen<- (which(capacities-dat_nC[i]>=0))
      sel_num <- (which(num[type_chosen]>0))
      selected<- type_chosen[sel_num[1]]
      if (sum(dat_nc_profit[i], dat_nc_profit[(i+1):length(dat_nC)]) - capacities[selected] >=0){
        bin_used<-append(bin_used, (capacities[selected]-dat_nC[i]))
      

        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        #items_left = items_left - 1
        num[selected]<- num[selected]-1
        
      }
    }
    else {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==min(chosen_b))
      contenders <- append(contenders, dat_nC[i])
      contenders_profit <- append(contenders_profit, dat_nc_profit[i])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (min(chosen_b) - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (min(chosen_b) - contenders[item_to_choose[item_chosen]])
    
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
    
    
  }
  dat_nC <- dat_nC[- which(dat_nC == 0)]
  dat_nc_profit <- dat_nc_profit[- which(dat_nc_profit == 0)]
}

non_comp_profit <- append(non_comp_profit, 0)
non_comp_profit <- non_comp_profit[- which(non_comp_profit == 0)] #no need really

no_of_bins <- num_initial - num
total_bins <- sum(no_of_bins)

cost_of_bins = sum(no_of_bins * capacities)

profit = sum(dat_comp_profit, non_comp_profit)

total_cost <- cost_of_bins - profit

#count_compulsory
#count_non_compulsory
items <- sum(count_compulsory, count_non_compulsory)
#bin_used

no_of_non_compulsory_items_left <- length(dat_nC)

BFD_b <- append(BFD_b, total_cost)  #total_bins when considering number of bins used

BFD <- as.data.frame(BFD_b)

#print(paste0( 'used ', total_bins , ' bins out of ', B ))




# BEST PROFITABLE...

dat<- dat





listed = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed<- c(dat[11:nrow(dat),])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed<- c(dat[9:nrow(dat),])
}
new_lis <- c()
new_check_comp <- c()
new_check_profit <- c()
dat_C<- c()

listed1 = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed1<- c(dat[5:9,])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed1<- c(dat[5:7,])
}

new_lis1 <- c()
new_check_cap <- c()
for (i in 1:length(listed1)){
  y1<-strsplit(listed1[i], split=" ")
  lis1<- as.numeric(y1[[1]][5])
  new_lis1 <- append(new_lis1, lis1)
  lis2<- as.numeric(y1[[1]][2])
  new_check_cap <- append(new_check_cap, lis2)
  dat1 <- as.data.frame(cbind(new_lis1, new_check_cap))
  colnames(dat1)[1] <- "numm" 
  colnames(dat1)[2] <- "Cap"
}
num<- dat1[,1]

num_initial <- dat1[,1]

capacities<- dat1[,2]
# 60, 80, 100,120,150
B <- sum(num)
type<- rep(capacities,num)
#taking out the data
for(k in 1:length(listed)){
  y<-strsplit(listed[k], split=" ")
  lis<- as.numeric(y[[1]][2])
  new_lis <- append(new_lis, lis)
  check_comp <- as.numeric(y[[1]][4])
  new_check_comp <- append(new_check_comp, check_comp)
  check_profit <- as.numeric(y[[1]][3])
  new_check_profit <- append(new_check_profit, check_profit)
  data_to_use <- as.data.frame(cbind(new_lis, new_check_comp, new_check_profit))
  colnames(data_to_use)[1] <- "Cost" 
  colnames(data_to_use)[2] <- "check_item"
  colnames(data_to_use)[3] <- "profit"
}

dat_C <- data_to_use[data_to_use$check_item ==1, 1]#compulsory items
count_compulsory <- length(dat_C)
dat_nC<- data_to_use[data_to_use$check_item == 0, 1] #non-compulsory items
count_non_compulsory <- length(dat_nC)
dat_nc_profit <- data_to_use[data_to_use$check_item == 0, 3] #profit of non-compulsory items
dat_comp_profit <- data_to_use[data_to_use$check_item == 1, 3]

dat_used1 <- cbind(dat_C, dat_comp_profit)
dat_used1 <- dat_used1[order(dat_comp_profit, decreasing = TRUE),]
dat_C <- dat_used1[,1]
dat_comp_profit <- dat_used1[,2]

dat_used <- cbind(dat_nC, dat_nc_profit)
dat_used <- dat_used[order(dat_nc_profit, decreasing = TRUE),]
dat_nC <- dat_used[,1]
dat_nc_profit <- dat_used[,2]


bin_used <- c() 

while(length(dat_C)>0){
  #choose a bin which the items can fit in
  chosen_bin <- which(bin_used-dat_C[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_C[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
  
    bin_used<-append(bin_used, (capacities[selected]-dat_C[1]))
    num[selected]<- num[selected]-1
 
  }else{
    chosen_b<-bin_used[chosen_bin]
    fitted_bin<-which(bin_used==min(chosen_b))
    bin_used[fitted_bin[1]]<- min(chosen_b)-dat_C[1]
  }
  dat_C<- dat_C[-1]
}

#-------

# Non-compulsory items

#------

num_updated <- num
contenders <- c() 
contenders_profit <- c()
non_comp_profit <- c()

if (length(dat_nC) != 0) {
  
  for (i in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[i]>=0)
    if (length(chosen_bin)==0) {
      type_chosen<- (which(capacities-dat_nC[i]>=0))
      sel_num <- (which(num[type_chosen]>0))
      selected<- type_chosen[sel_num[1]]
      if (dat_nc_profit[i] - capacities[selected] >=0){
        bin_used<-append(bin_used, (capacities[selected]-dat_nC[i]))
      
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        #items_left = items_left - 1
        num[selected]<- num[selected]-1
        
      }
    }
    else {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==min(chosen_b))
      contenders <- append(contenders, dat_nC[i])
      contenders_profit <- append(contenders_profit, dat_nc_profit[i])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (min(chosen_b) - contenders[item_to_choose[item_chosen]] >= 0){
        
        bin_used[fitted_bin[1]] <- (min(chosen_b) - contenders[item_to_choose[item_chosen]])
      
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        
        contenders <- c()
        contenders_profit <- c()
        
      }
      
    }
  }
  
  for (j in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[j]>=0)
    if (length(chosen_bin)!=0) {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==min(chosen_b))
      contenders <- append(contenders, dat_nC[j])
      contenders_profit <- append(contenders_profit, dat_nc_profit[j])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (min(chosen_b) - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (min(chosen_b) - contenders[item_to_choose[item_chosen]])
   
        dat_nC[j] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[j] )
        dat_nc_profit[j] <- 0
        #items_left = items_left - 1
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
    
  }
  dat_nC <- dat_nC[- which(dat_nC == 0)]
  dat_nc_profit <- dat_nc_profit[- which(dat_nc_profit == 0)]
}


new_bin_used <- c()
profit_for_bin <- c()
cost_bin_used <- c()
keep_count <- c()
keep_count1 <- c()
new_keep_count <- c()
dat_nC1 <- dat_nC
dat_nc_profit1 <- dat_nc_profit
while (length(dat_nC1>0)){
  chosen_bin <- which(new_bin_used-dat_nC1[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_nC1[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
    new_bin_used<-append(new_bin_used, (capacities[selected]-dat_nC1[1]))
  
    keep_count1 <- append(keep_count1, 1)
    cost_bin_used <- append(cost_bin_used, capacities[selected])
    profit_for_bin <- append(profit_for_bin, dat_nc_profit1[1])
    
    
    
  }else{
    chosen_b<-new_bin_used[chosen_bin]
    fitted_bin<-which(new_bin_used==(chosen_b[1]))
    new_bin_used[fitted_bin[1]]<- (chosen_b[1])-dat_nC1[1]
  
    profit_for_bin[fitted_bin[1]]<- profit_for_bin[fitted_bin[1]] + dat_nc_profit1[1]
    keep_count1[fitted_bin[1]] <- keep_count1[fitted_bin[1]] + 1
    
  }
  dat_nC1<- dat_nC1[-1]
  dat_nc_profit1 <- dat_nc_profit1[-1]
  
  
}

profitt <- c()
if (length(dat_nC) != 0){
  for (i in 1:length(cost_bin_used)){
    if (profit_for_bin[i] >= cost_bin_used[i]){
      bin_used <- append(bin_used, new_bin_used[i])
     
      profitt <- append(profitt, profit_for_bin[i])
      keep_count <- append(keep_count, keep_count1[i])
      num[selected]<- num[selected]-1
      
    }
    
  }
}


non_comp_profit <- non_comp_profit[- which(non_comp_profit == 0)] #no need really

no_of_bins <- num_initial - num
total_bins <- sum(no_of_bins)

cost_of_bins = sum(no_of_bins * capacities)

profit = sum(dat_comp_profit, non_comp_profit, profitt)

total_cost <- cost_of_bins - profit


#count_compulsory
#count_non_compulsory
#bin_used

no_of_non_compulsory_items_left <- length(dat_nC)- sum(keep_count)


BP_b <- append(BP_b, total_cost)  #total_bins when considering number of bins used

BP <- as.data.frame(BP_b)









# BEST ASSIGNMENT ...

dat<- dat 





listed = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed<- c(dat[11:nrow(dat),])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed<- c(dat[9:nrow(dat),])
}
new_lis <- c()
new_check_comp <- c()
new_check_profit <- c()
dat_C<- c()

listed1 = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed1<- c(dat[5:9,])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed1<- c(dat[5:7,])
}

new_lis1 <- c()
new_check_cap <- c()
for (i in 1:length(listed1)){
  y1<-strsplit(listed1[i], split=" ")
  lis1<- as.numeric(y1[[1]][5])
  new_lis1 <- append(new_lis1, lis1)
  lis2<- as.numeric(y1[[1]][2])
  new_check_cap <- append(new_check_cap, lis2)
  dat1 <- as.data.frame(cbind(new_lis1, new_check_cap))
  colnames(dat1)[1] <- "numm" 
  colnames(dat1)[2] <- "Cap"
}
num<- dat1[,1]

num_initial <- dat1[,1]

capacities<- dat1[,2]
# 60, 80, 100,120,150
B <- sum(num)
type<- rep(capacities,num)

#taking out the data
for(k in 1:length(listed)){
  y<-strsplit(listed[k], split=" ")
  lis<- as.numeric(y[[1]][2])
  new_lis <- append(new_lis, lis)
  check_comp <- as.numeric(y[[1]][4])
  new_check_comp <- append(new_check_comp, check_comp)
  check_profit <- as.numeric(y[[1]][3])
  new_check_profit <- append(new_check_profit, check_profit)
  data_to_use <- as.data.frame(cbind(new_lis, new_check_comp, new_check_profit))
  colnames(data_to_use)[1] <- "Cost" 
  colnames(data_to_use)[2] <- "check_item"
  colnames(data_to_use)[3] <- "profit"
}

dat_C <- data_to_use[data_to_use$check_item ==1, 1]#compulsory items
count_compulsory <- length(dat_C)
dat_nC<- data_to_use[data_to_use$check_item == 0, 1] #non-compulsory items
count_non_compulsory <- length(dat_nC)
dat_nc_profit <- data_to_use[data_to_use$check_item == 0, 3] #profit of non-compulsory items
dat_comp_profit <- data_to_use[data_to_use$check_item == 1, 3]


bin_used <- c() 

while(length(dat_C)>0){
  #choose a bin which the items can fit in
  chosen_bin <- which(bin_used-dat_C[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_C[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
    
  
    bin_used<-append(bin_used, (capacities[selected]-dat_C[1]))
    num[selected]<- num[selected]-1
  
    
  }else{
    chosen_b<-bin_used[chosen_bin]
    fitted_bin<-which(bin_used==min(chosen_b))
    bin_used[fitted_bin[1]]<- min(chosen_b)-dat_C[1]
  }
  dat_C<- dat_C[-1]
}

#-------

# Non-compulsory items

#------

num_updated <- num
contenders <- c() 
contenders_profit <- c()
non_comp_profit <- c()

if (length(dat_nC) != 0) {
  
  for (i in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[i]>=0)
    if (length(chosen_bin)==0) {
      type_chosen<- (which(capacities-dat_nC[i]>=0))
      sel_num <- (which(num[type_chosen]>0))
      selected<- type_chosen[sel_num[1]]
      if (dat_nc_profit[i] - capacities[selected] >=0){
        bin_used<-append(bin_used, (capacities[selected]-dat_nC[i]))
      
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        #items_left = items_left - 1
        num[selected]<- num[selected]-1
        
      }
    }
    else {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==min(chosen_b))
      contenders <- append(contenders, dat_nC[i])
      contenders_profit <- append(contenders_profit, dat_nc_profit[i])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (min(chosen_b) - contenders[item_to_choose[item_chosen]] >= 0){
        
        bin_used[fitted_bin[1]] <- (min(chosen_b) - contenders[item_to_choose[item_chosen]])
      
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        
        contenders <- c()
        contenders_profit <- c()
        
      }
      
    }
  }
  
  for (j in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[j]>=0)
    if (length(chosen_bin)!=0) {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==min(chosen_b))
      contenders <- append(contenders, dat_nC[j])
      contenders_profit <- append(contenders_profit, dat_nc_profit[j])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (min(chosen_b) - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (min(chosen_b) - contenders[item_to_choose[item_chosen]])
     
        dat_nC[j] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[j] )
        dat_nc_profit[j] <- 0
        #items_left = items_left - 1
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
    
  }
  dat_nC <- dat_nC[- which(dat_nC == 0)]
  dat_nc_profit <- dat_nc_profit[- which(dat_nc_profit == 0)]
}

new_bin_used <- c()
profit_for_bin <- c()
cost_bin_used <- c()
keep_count <- c()
keep_count1 <- c()
new_keep_count <- c()
dat_nC1 <- dat_nC
dat_nc_profit1 <- dat_nc_profit
while (length(dat_nC1>0)){
  chosen_bin <- which(new_bin_used-dat_nC1[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_nC1[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
    new_bin_used<-append(new_bin_used, (capacities[selected]-dat_nC1[1]))
    #keep_count <- append(keep_count, dat_nC1[1])
    keep_count1 <- append(keep_count1, 1)
    cost_bin_used <- append(cost_bin_used, capacities[selected])
    profit_for_bin <- append(profit_for_bin, dat_nc_profit1[1])
    
    
    
  }else{
    chosen_b<-new_bin_used[chosen_bin]
    fitted_bin<-which(new_bin_used==(chosen_b[1]))
    new_bin_used[fitted_bin[1]]<- (chosen_b[1])-dat_nC1[1]
    profit_for_bin[fitted_bin[1]]<- profit_for_bin[fitted_bin[1]] + dat_nc_profit1[1]
    keep_count1[fitted_bin[1]] <- keep_count1[fitted_bin[1]] + 1
    
  }
  dat_nC1<- dat_nC1[-1]
  dat_nc_profit1 <- dat_nc_profit1[-1]
  
  
}

profitt <- c()
if (length(dat_nC) != 0){
  for (i in 1:length(cost_bin_used)){
    if (profit_for_bin[i] >= cost_bin_used[i]){
      bin_used <- append(bin_used, new_bin_used[i])

      profitt <- append(profitt, profit_for_bin[i])
      keep_count <- append(keep_count, keep_count1[i])
      num[selected]<- num[selected]-1
      
      #cost_bin_used1 <- append(cost_bin_used1, cost_bin_used[i])
    }
    
  }
}


non_comp_profit <- non_comp_profit[- which(non_comp_profit == 0)] #no need really

no_of_bins <- num_initial - num
total_bins <- sum(no_of_bins)

cost_of_bins = sum(no_of_bins * capacities)

profit = sum(dat_comp_profit, non_comp_profit, profitt)

total_cost <- cost_of_bins - profit


no_of_non_compulsory_items_left <- length(dat_nC)-sum(keep_count)


BF_b <- append(BF_b, total_cost)  #total_bins when considering number of bins used

BF <- as.data.frame(BF_b)

#print(paste0( 'used ', total_bins , ' bins out of ', B ))











# FIRST FIT DECREASING IMPROVEMENT...

dat<- dat 





listed = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed<- c(dat[11:nrow(dat),])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed<- c(dat[9:nrow(dat),])
}
new_lis <- c()
new_check_comp <- c()
new_check_profit <- c()
dat_C<- c()

listed1 = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed1<- c(dat[5:9,])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed1<- c(dat[5:7,])
}

new_lis1 <- c()
new_check_cap <- c()
for (i in 1:length(listed1)){
  y1<-strsplit(listed1[i], split=" ")
  lis1<- as.numeric(y1[[1]][5])
  new_lis1 <- append(new_lis1, lis1)
  lis2<- as.numeric(y1[[1]][2])
  new_check_cap <- append(new_check_cap, lis2)
  dat1 <- as.data.frame(cbind(new_lis1, new_check_cap))
  colnames(dat1)[1] <- "numm" 
  colnames(dat1)[2] <- "Cap"
}
num<- dat1[,1]

num_initial <- dat1[,1]

capacities<- dat1[,2]
# 60, 80, 100,120,150
B <- sum(num)
type<- rep(capacities,num)

#taking out the data
for(k in 1:length(listed)){
  y<-strsplit(listed[k], split=" ")
  lis<- as.numeric(y[[1]][2])
  new_lis <- append(new_lis, lis)
  check_comp <- as.numeric(y[[1]][4])
  new_check_comp <- append(new_check_comp, check_comp)
  check_profit <- as.numeric(y[[1]][3])
  new_check_profit <- append(new_check_profit, check_profit)
  data_to_use <- as.data.frame(cbind(new_lis, new_check_comp, new_check_profit))
  colnames(data_to_use)[1] <- "Cost" 
  colnames(data_to_use)[2] <- "check_item"
  colnames(data_to_use)[3] <- "profit"
}

dat_C <- data_to_use[data_to_use$check_item ==1, 1]#compulsory items
count_compulsory <- length(dat_C)
dat_nC<- data_to_use[data_to_use$check_item == 0, 1] #non-compulsory items
count_non_compulsory <- length(dat_nC)
dat_nc_profit <- data_to_use[data_to_use$check_item == 0, 3] #profit of non-compulsory items
dat_comp_profit <- data_to_use[data_to_use$check_item == 1, 3]
dat_used <- cbind(dat_nC, dat_nc_profit)
dat_used <- dat_used[order(dat_nC, decreasing = TRUE),]
dat_nC <- dat_used[,1]
dat_nc_profit <- dat_used[,2]
dat_C<-sort(dat_C,decreasing = TRUE)

bin_used <- c() 

#first pack compulsory items if there are some...

while(length(dat_C)>0){
  #choose a bin which the items can fit in
  chosen_bin <- which(bin_used-dat_C[1]>=0) 
  if (length(chosen_bin)==0){ 
    type_chosen<- (which(capacities-dat_C[1]>=0)) # checks new bins which the item can fit
    sel_num <- which(num[type_chosen]>0) # checks index of the smallest bin the item can fit in the bins still available
    selected<- type_chosen[sel_num[1]] # takes the smallest bin the item can fit in the bins still available
    
   
    bin_used<-append(bin_used, (capacities[selected]-dat_C[1]))
    num[selected]<- num[selected]-1
   
    
  }else{ 
    
    chosen_b<-bin_used[chosen_bin] # list of bins the item can fit
    fitted_bin<-which(bin_used==chosen_b[1]) # take the first one of those
    bin_used[fitted_bin[1]]<- chosen_b[1]-dat_C[1] # fit the item in
    
  }
  dat_C<- dat_C[-1] #remove that item from list
}

# non-compulsory items


#start with the updated number of bins for each capacities
num_updated <- num
contenders <- c() 
contenders_profit <- c()
non_comp_profit <- c()
if (length(dat_nC) != 0) {
  
  for (i in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[i]>=0)
    if (length(chosen_bin)==0) {
      type_chosen<- (which(capacities-dat_nC[i]>=0))
      sel_num <- (which(num[type_chosen]>0))
      selected<- type_chosen[sel_num[1]]
      if (dat_nc_profit[i] - capacities[selected] >=0){
        bin_used<-append(bin_used, (capacities[selected]-dat_nC[i]))
       
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        #items_left = items_left - 1
        num[selected]<- num[selected]-1
        
      }
    }
    else {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==chosen_b[1])
      contenders <- append(contenders, dat_nC[i])
      contenders_profit <- append(contenders_profit, dat_nc_profit[i])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (chosen_b[1] - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (chosen_b[1] - contenders[item_to_choose[item_chosen]])
     
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
  }
  
  
  
  for (j in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[j]>=0)
    if (length(chosen_bin)!=0) {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==chosen_b[1])
      contenders <- append(contenders, dat_nC[j])
      contenders_profit <- append(contenders_profit, dat_nc_profit[j])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (chosen_b[1] - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (chosen_b[1] - contenders[item_to_choose[item_chosen]])
   
        dat_nC[j] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[j] )
        dat_nc_profit[j] <- 0
        
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
    
  }
  dat_nC <- dat_nC[- which(dat_nC == 0)]
  dat_nc_profit <- dat_nc_profit[- which(dat_nc_profit == 0)]
}

new_bin_used <- c()
profit_for_bin <- c()
cost_bin_used <- c()
keep_count <- c()
keep_count1 <- c()
new_keep_count <- c()
dat_used <- cbind(dat_nC, dat_nc_profit)
dat_used <- dat_used[order(dat_nC, decreasing = TRUE),]
#print(dat_used[,1])
if (length(dat_used ==2)){
  dat_used = matrix(dat_used, ncol = 2)
}
dat_nC1 <- dat_used[,1]
dat_nc_profit1 <- dat_used[,2]
while (length(dat_nC1>0)){
  chosen_bin <- which(new_bin_used-dat_nC1[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_nC1[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
    new_bin_used<-append(new_bin_used, (capacities[selected]-dat_nC1[1]))
    #keep_count <- append(keep_count, dat_nC1[1])
    keep_count1 <- append(keep_count1, 1)
    cost_bin_used <- append(cost_bin_used, capacities[selected])
    profit_for_bin <- append(profit_for_bin, dat_nc_profit1[1])
    
    
    
  }else{
    chosen_b<-new_bin_used[chosen_bin]
    fitted_bin<-which(new_bin_used==(chosen_b[1]))
    new_bin_used[fitted_bin[1]]<- (chosen_b[1])-dat_nC1[1]
    profit_for_bin[fitted_bin[1]]<- profit_for_bin[fitted_bin[1]] + dat_nc_profit1[1]
    keep_count1[fitted_bin[1]] <- keep_count1[fitted_bin[1]] + 1
    
  }
  dat_nC1<- dat_nC1[-1]
  dat_nc_profit1 <- dat_nc_profit1[-1]
  
  
}

profitt <- c()
if (length(dat_nC) != 0){
  for (i in 1:length(cost_bin_used)){
    if (profit_for_bin[i] >= cost_bin_used[i]){
      bin_used <- append(bin_used, new_bin_used[i])
    
      profitt <- append(profitt, profit_for_bin[i])
      keep_count <- append(keep_count, keep_count1[i])
      num[selected]<- num[selected]-1
      
      #cost_bin_used1 <- append(cost_bin_used1, cost_bin_used[i])
    }
    
  }
}


#sum of items.
non_comp_profit <- non_comp_profit[- which(non_comp_profit == 0)] #no need really

no_of_bins <- num_initial - num
total_bins <- sum(no_of_bins)

cost_of_bins = sum(no_of_bins * capacities)

profit = sum(dat_comp_profit, non_comp_profit, profitt)

total_cost <- cost_of_bins - profit

#count_compulsory
#count_non_compulsory
items <- sum(count_compulsory, count_non_compulsory)
#bin_used

no_of_non_compulsory_items_left <- length(dat_nC) - sum(keep_count)

FFD2_b <- append(FFD2_b, total_cost)  #total_bins when considering number of bins used

FFD2 <- as.data.frame(FFD2_b)

#print(paste0( 'used ', total_bins , ' bins out of ', B ))









# BEST FIT DECREASING IMPROVEMENT...

dat<- dat 




listed = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed<- c(dat[11:nrow(dat),])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed<- c(dat[9:nrow(dat),])
}
new_lis <- c()
new_check_comp <- c()
new_check_profit <- c()
dat_C<- c()

listed1 = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed1<- c(dat[5:9,])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed1<- c(dat[5:7,])
}

new_lis1 <- c()
new_check_cap <- c()
for (i in 1:length(listed1)){
  y1<-strsplit(listed1[i], split=" ")
  lis1<- as.numeric(y1[[1]][5])
  new_lis1 <- append(new_lis1, lis1)
  lis2<- as.numeric(y1[[1]][2])
  new_check_cap <- append(new_check_cap, lis2)
  dat1 <- as.data.frame(cbind(new_lis1, new_check_cap))
  colnames(dat1)[1] <- "numm" 
  colnames(dat1)[2] <- "Cap"
}
num<- dat1[,1]

num_initial <- dat1[,1]

capacities<- dat1[,2]
# 60, 80, 100,120,150
B <- sum(num)
type<- rep(capacities,num)

#taking out the data
for(k in 1:length(listed)){
  y<-strsplit(listed[k], split=" ")
  lis<- as.numeric(y[[1]][2])
  new_lis <- append(new_lis, lis)
  check_comp <- as.numeric(y[[1]][4])
  new_check_comp <- append(new_check_comp, check_comp)
  check_profit <- as.numeric(y[[1]][3])
  new_check_profit <- append(new_check_profit, check_profit)
  data_to_use <- as.data.frame(cbind(new_lis, new_check_comp, new_check_profit))
  colnames(data_to_use)[1] <- "Cost" 
  colnames(data_to_use)[2] <- "check_item"
  colnames(data_to_use)[3] <- "profit"
}

dat_C <- data_to_use[data_to_use$check_item ==1, 1]#compulsory items
count_compulsory <- length(dat_C)
dat_nC<- data_to_use[data_to_use$check_item == 0, 1] #non-compulsory items
count_non_compulsory <- length(dat_nC)
dat_nc_profit <- data_to_use[data_to_use$check_item == 0, 3] #profit of non-compulsory items
dat_comp_profit <- data_to_use[data_to_use$check_item == 1, 3]
dat_used <- cbind(dat_nC, dat_nc_profit)
dat_used <- dat_used[order(dat_nC, decreasing = TRUE),]
dat_nC <- dat_used[,1]
dat_nc_profit <- dat_used[,2]
dat_C<-sort(dat_C,decreasing = TRUE)

bin_used <- c() 

while(length(dat_C)>0){
  #choose a bin which the items can fit in
  chosen_bin <- which(bin_used-dat_C[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_C[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
    
    # if there are no bins which can fit the item take a bin out from the available and subtract this bin from the number availabe
    #if(num[type_chosen]>0){
    bin_used<-append(bin_used, (capacities[selected]-dat_C[1]))
    num[selected]<- num[selected]-1
   
    
  }else{
    chosen_b<-bin_used[chosen_bin]
    fitted_bin<-which(bin_used==min(chosen_b))
    bin_used[fitted_bin[1]]<- min(chosen_b)-dat_C[1]
  }
  dat_C<- dat_C[-1]
}

#-------

# Non-compulsory items

#------
# non_comp_dat <- cbind(dat_nC, dat_nc_profit)
# non_comp_dat <- non_comp_dat[order(dat_nc_profit, decreasing = TRUE),]
# dat_nC <- non_comp_dat[,1]
# dat_nc_profit <- non_comp_dat[,2]
num_updated <- num
contenders <- c() 
contenders_profit <- c()
non_comp_profit <- c()

if (length(dat_nC) != 0) {
  
  for (i in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[i]>=0)
    if (length(chosen_bin)==0) {
      type_chosen<- (which(capacities-dat_nC[i]>=0))
      sel_num <- (which(num[type_chosen]>0))
      selected<- type_chosen[sel_num[1]]
      if (dat_nc_profit[i] - capacities[selected] >=0){
        bin_used<-append(bin_used, (capacities[selected]-dat_nC[i]))
       
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        #items_left = items_left - 1
        num[selected]<- num[selected]-1
        
      }
    }
    else {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==min(chosen_b))
      contenders <- append(contenders, dat_nC[i])
      contenders_profit <- append(contenders_profit, dat_nc_profit[i])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (min(chosen_b) - contenders[item_to_choose[item_chosen]] >= 0){
        
        bin_used[fitted_bin[1]] <- (min(chosen_b) - contenders[item_to_choose[item_chosen]])
     
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        
        contenders <- c()
        contenders_profit <- c()
        
      }
      
    }
  }
  
  for (j in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[j]>=0)
    if (length(chosen_bin)!=0) {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==min(chosen_b))
      contenders <- append(contenders, dat_nC[j])
      contenders_profit <- append(contenders_profit, dat_nc_profit[j])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (min(chosen_b) - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (min(chosen_b) - contenders[item_to_choose[item_chosen]])
     
        dat_nC[j] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[j] )
        dat_nc_profit[j] <- 0
        #items_left = items_left - 1
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
    
  }
  dat_nC <- dat_nC[- which(dat_nC == 0)]
  dat_nc_profit <- dat_nc_profit[- which(dat_nc_profit == 0)]
}

#------



new_bin_used <- c()
profit_for_bin <- c()
cost_bin_used <- c()
keep_count <- c()
keep_count1 <- c()
new_keep_count <- c()
dat_used <- cbind(dat_nC, dat_nc_profit)
dat_used <- dat_used[order(dat_nC, decreasing = TRUE),]
if (length(dat_used ==2)){  #otherwise instances with 1 non-compulsory left returns an error
  dat_used = matrix(dat_used, ncol = 2)
}
dat_nC1 <- dat_used[,1]
dat_nc_profit1 <- dat_used[,2]
while (length(dat_nC1>0)){
  chosen_bin <- which(new_bin_used-dat_nC1[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_nC1[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
    new_bin_used<-append(new_bin_used, (capacities[selected]-dat_nC1[1]))
    #keep_count <- append(keep_count, dat_nC1[1])
    keep_count1 <- append(keep_count1, 1)
    cost_bin_used <- append(cost_bin_used, capacities[selected])
    profit_for_bin <- append(profit_for_bin, dat_nc_profit1[1])
    
    
    
  }else{
    chosen_b<-new_bin_used[chosen_bin]
    fitted_bin<-which(new_bin_used==min(chosen_b))
    new_bin_used[fitted_bin[1]]<- min(chosen_b)-dat_nC1[1]
    profit_for_bin[fitted_bin[1]]<- profit_for_bin[fitted_bin[1]] + dat_nc_profit1[1]
    keep_count1[fitted_bin[1]] <- keep_count1[fitted_bin[1]] + 1
    
  }
  dat_nC1<- dat_nC1[-1]
  dat_nc_profit1 <- dat_nc_profit1[-1]
  
  
}

profitt <- c()
if (length(dat_nC) != 0){
  for (i in 1:length(cost_bin_used)){
    if (profit_for_bin[i] >= cost_bin_used[i]){
      bin_used <- append(bin_used, new_bin_used[i])
     
      profitt <- append(profitt, profit_for_bin[i])
      keep_count <- append(keep_count, keep_count1[i])
      num[selected]<- num[selected]-1
      
      #cost_bin_used1 <- append(cost_bin_used1, cost_bin_used[i])
    }
    
  }
}



non_comp_profit <- non_comp_profit[- which(non_comp_profit == 0)] #no need really


no_of_bins <- num_initial - num
total_bins <- sum(no_of_bins)

cost_of_bins = sum(no_of_bins * capacities)

profit = sum(dat_comp_profit, non_comp_profit, profitt)

total_cost <- cost_of_bins - profit


#count_compulsory
#count_non_compulsory
items <- sum(count_compulsory, count_non_compulsory)
#bin_used


no_of_non_compulsory_items_left <- length(dat_nC)- sum(keep_count)

BFD2_b <- append(BFD2_b, total_cost)  #total_bins when considering number of bins used

BFD2 <- as.data.frame(BFD2_b)

#print(paste0( 'used ', total_bins , ' bins out of ', B ))







# large bins first

dat<- dat 





listed = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed<- c(dat[11:nrow(dat),])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed<- c(dat[9:nrow(dat),])
}
new_lis <- c()
new_check_comp <- c()
new_check_profit <- c()
dat_C<- c()

listed1 = c()
if (nrow(dat) == 510 || nrow(dat) == 35 || nrow(dat) == 60 || nrow(dat) == 110 || nrow(dat) == 210){
  listed1<- c(dat[5:9,])
}
if (nrow(dat) == 508 || nrow(dat) == 33 || nrow(dat) == 58 || nrow(dat) == 108 || nrow(dat) == 208){
  listed1<- c(dat[5:7,])
}

new_lis1 <- c()
new_check_cap <- c()
for (i in 1:length(listed1)){
  y1<-strsplit(listed1[i], split=" ")
  lis1<- as.numeric(y1[[1]][5])
  new_lis1 <- append(new_lis1, lis1)
  lis2<- as.numeric(y1[[1]][2])
  new_check_cap <- append(new_check_cap, lis2)
  dat1 <- as.data.frame(cbind(new_lis1, new_check_cap))
  colnames(dat1)[1] <- "numm" 
  colnames(dat1)[2] <- "Cap"
}
num<- dat1[,1]

num_initial <- dat1[,1]

capacities<- sort(dat1[,2], decreasing = TRUE)
# 60, 80, 100,120,150
B <- sum(num)
type<- rep(capacities,num)

#taking out the data
for(k in 1:length(listed)){
  y<-strsplit(listed[k], split=" ")
  lis<- as.numeric(y[[1]][2])
  new_lis <- append(new_lis, lis)
  check_comp <- as.numeric(y[[1]][4])
  new_check_comp <- append(new_check_comp, check_comp)
  check_profit <- as.numeric(y[[1]][3])
  new_check_profit <- append(new_check_profit, check_profit)
  data_to_use <- as.data.frame(cbind(new_lis, new_check_comp, new_check_profit))
  colnames(data_to_use)[1] <- "Cost" 
  colnames(data_to_use)[2] <- "check_item"
  colnames(data_to_use)[3] <- "profit"
}

dat_C <- data_to_use[data_to_use$check_item ==1, 1]#compulsory items
count_compulsory <- length(dat_C)
dat_nC<- data_to_use[data_to_use$check_item == 0, 1] #non-compulsory items
count_non_compulsory <- length(dat_nC)
dat_nc_profit <- data_to_use[data_to_use$check_item == 0, 3] #profit of non-compulsory items
dat_comp_profit <- data_to_use[data_to_use$check_item == 1, 3]
dat_used <- cbind(dat_nC, dat_nc_profit)
dat_used <- dat_used[order(dat_nC, decreasing = TRUE),]
dat_nC <- dat_used[,1]
dat_nc_profit <- dat_used[,2]
dat_C<-sort(dat_C,decreasing = TRUE)

bin_used <- c() 

#first pack compulsory items if there are some...

while(length(dat_C)>0){
  #choose a bin which the items can fit in
  chosen_bin <- which(bin_used-dat_C[1]>=0) 
  if (length(chosen_bin)==0){ 
    type_chosen<- (which(capacities-dat_C[1]>=0)) # checks new bins which the item can fit
    sel_num <- which(num[type_chosen]>0) # checks index of the smallest bin the item can fit in the bins still available
    selected<- type_chosen[sel_num[1]] # takes the smallest bin the item can fit in the bins still available
    
    # if there are no bins which can fit the item take a bin out from the available and subtract this bin from the number availabe
    #if(num[type_chosen]>0){
    bin_used<-append(bin_used, (capacities[selected]-dat_C[1]))
    num[selected]<- num[selected]-1
 
    
  }else{ # adding items to already present bins
   
    
    chosen_b<-bin_used[chosen_bin] # list of bins the item can fit
    fitted_bin<-which(bin_used==chosen_b[1]) # take the first one of those
    bin_used[fitted_bin[1]]<- chosen_b[1]-dat_C[1] # fit the item in
    
  }
  dat_C<- dat_C[-1] #remove that item from list
}

# non-compulsory items


#start with the updated number of bins for each capacities
num_updated <- num
contenders <- c() 
contenders_profit <- c()
non_comp_profit <- c()
if (length(dat_nC) != 0) {
  
  for (i in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[i]>=0)
    if (length(chosen_bin)==0) {
      type_chosen<- (which(capacities-dat_nC[i]>=0))
      sel_num <- (which(num[type_chosen]>0))
      selected<- type_chosen[sel_num[1]]
      if (dat_nc_profit[i] - capacities[selected] >=0){
        bin_used<-append(bin_used, (capacities[selected]-dat_nC[i]))
      
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        #items_left = items_left - 1
        num[selected]<- num[selected]-1
        
      }
    }
    else {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==chosen_b[1])
      contenders <- append(contenders, dat_nC[i])
      contenders_profit <- append(contenders_profit, dat_nc_profit[i])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (chosen_b[1] - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (chosen_b[1] - contenders[item_to_choose[item_chosen]])
      
        dat_nC[i] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[i] )
        dat_nc_profit[i] <- 0
        
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
  }
  
  
  
  for (j in 1:length(dat_nC)) {
    chosen_bin <- which(bin_used-dat_nC[j]>=0)
    if (length(chosen_bin)!=0) {
      chosen_b <- bin_used[chosen_bin]
      fitted_bin<-which(bin_used==chosen_b[1])
      contenders <- append(contenders, dat_nC[j])
      contenders_profit <- append(contenders_profit, dat_nc_profit[j])
      item_to_choose <- which(contenders_profit == max(contenders_profit)) 
      item_chosen <- which(contenders[item_to_choose] == min(contenders[item_to_choose])) #in case there are two items with the same profit, pick the smaller item
      if (chosen_b[1] - contenders[item_to_choose[item_chosen]] >= 0){
        bin_used[fitted_bin[1]] <- (chosen_b[1] - contenders[item_to_choose[item_chosen]])
      
        dat_nC[j] <- 0
        non_comp_profit <- append(non_comp_profit, dat_nc_profit[j] )
        dat_nc_profit[j] <- 0
        
        contenders <- c()
        contenders_profit <- c()
      }
      
    }
    
  }
  dat_nC <- dat_nC[- which(dat_nC == 0)]
  dat_nc_profit <- dat_nc_profit[- which(dat_nc_profit == 0)]
}

new_bin_used <- c()
profit_for_bin <- c()
cost_bin_used <- c()
keep_count <- c()
keep_count1 <- c()
new_keep_count <- c()
dat_used <- cbind(dat_nC, dat_nc_profit)
dat_used <- dat_used[order(dat_nC, decreasing = TRUE),]
#print(dat_used[,1])
if (length(dat_used ==2)){
  dat_used = matrix(dat_used, ncol = 2)
}
dat_nC1 <- dat_used[,1]
dat_nc_profit1 <- dat_used[,2]
while (length(dat_nC1>0)){
  chosen_bin <- which(new_bin_used-dat_nC1[1]>=0)
  if (length(chosen_bin)==0){
    type_chosen<- (which(capacities-dat_nC1[1]>=0))
    sel_num <- min(which(num[type_chosen]>0))
    selected<- type_chosen[sel_num]
    new_bin_used<-append(new_bin_used, (capacities[selected]-dat_nC1[1]))
    #keep_count <- append(keep_count, dat_nC1[1])
    keep_count1 <- append(keep_count1, 1)
    cost_bin_used <- append(cost_bin_used, capacities[selected])
    profit_for_bin <- append(profit_for_bin, dat_nc_profit1[1])
    
    
    
  }else{
    chosen_b<-new_bin_used[chosen_bin]
    fitted_bin<-which(new_bin_used==(chosen_b[1]))
    new_bin_used[fitted_bin[1]]<- (chosen_b[1])-dat_nC1[1]
    profit_for_bin[fitted_bin[1]]<- profit_for_bin[fitted_bin[1]] + dat_nc_profit1[1]
    keep_count1[fitted_bin[1]] <- keep_count1[fitted_bin[1]] + 1
    
  }
  dat_nC1<- dat_nC1[-1]
  dat_nc_profit1 <- dat_nc_profit1[-1]
  
  
}

profitt <- c()
if (length(dat_nC) != 0){
  for (i in 1:length(cost_bin_used)){
    if (profit_for_bin[i] >= cost_bin_used[i]){
      bin_used <- append(bin_used, new_bin_used[i])
     
      profitt <- append(profitt, profit_for_bin[i])
      keep_count <- append(keep_count, keep_count1[i])
      num[selected]<- num[selected]-1
      
      #cost_bin_used1 <- append(cost_bin_used1, cost_bin_used[i])
    }
    
  }
}


#sum of items.
non_comp_profit <- non_comp_profit[- which(non_comp_profit == 0)] #no need really

no_of_bins <- num_initial - num
total_bins <- sum(no_of_bins)

cost_of_bins = sum(no_of_bins * capacities)

profit = sum(dat_comp_profit, non_comp_profit, profitt)

total_cost <- cost_of_bins - profit

#count_compulsory
#count_non_compulsory
items <- sum(count_compulsory, count_non_compulsory)
#bin_used

no_of_non_compulsory_items_left <- length(dat_nC) - sum(keep_count)

FFD3_b <- append(FFD3_b, total_cost) #total_bins when considering number of bins used
FFD3 <- as.data.frame(FFD3_b)

#print(paste0( 'used ', total_bins , ' bins out of ', B ))
instance_counter <- instance_counter+1
}

 #write the data to excel, depending on the class and bin types read

#dat_class_0_3bintypes <- cbind(FFD, BFD, BP, BF, FFD2, BFD2, FFD3)


 #dat_class_0_3bintypes1 <- as.data.frame(dat_class_0_3bintypes)
 #write_xlsx(dat_class_0_3bintypes1, "~/Downloads/Stats honours/Project/r codes\\class_0_3bintypes.xlsx") 
