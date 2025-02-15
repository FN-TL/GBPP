#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 29 00:24:02 2024

@author: funanani
"""




import pandas as pd
import numpy as np

import glob
import csv
import os
import re

from operator import itemgetter

class Bin(object):
    ''' Container for items that keeps a running sum '''
    def __init__(self):
        self.items = []
        self.sum = 0

    def append(self, item):
        self.items.append(item)
        self.sum += item

    def __str__(self):
        ''' Printable representation '''
        return 'Bin(sum=%d, items=%s)' % (self.sum, str(self.items))

def prof(new_itemlist, newbin):
    
    for i in range(len(newbin)):
        if  newbin[i][1] >= new_itemlist[0][0]:
            sum_vol_bin = newbin[i][0] #put the first item in the first bin it fits
            bincost = newbin[i][0]
            global bin_num
            bin_num = i
           
            break
     
    #it = new_itemlist
    #print(it)
    #itemsv = new_itemlist[0][0]
    

    #print(sum_vol_bin)
    #new_item_crit = sum_vol_bin/bincost
    item_crit = []
    item_crit_list = []
    n2_item = []
    n2_p = []
    s = 0
    #crit = 0
    
    for i in range(len(new_itemlist)):
        n_i = new_itemlist[i][0]
        n_p = new_itemlist[i][1]
        n2_p.append(n_p)
        n2_item.append(n_i)

            
        item_crit = (sum_vol_bin - n_i)/bincost
        item_crit_list.append(item_crit)
    
    
    n_itemlist = np.array(n2_item)
    n2_itemlist = np.array(n2_p)
    item_crit_list = np.array(item_crit_list)
    #print(item_crit_list)
    
    
    itemlist_sortcrit = pd.DataFrame(zip(n_itemlist, n2_itemlist, item_crit_list), columns = ['item_volume', 'item_profit', 'cost_contribution']) 
    sorted_df = itemlist_sortcrit.sort_values(by = 'cost_contribution', ascending= False) #sort bin list by order of increasing cost contribution
    
    Sorted_volumes = sorted_df.item_volume #sorted item volumes by cost contribution
    Sorted_profits = sorted_df.item_profit #sorted item profits by cost contribution
    
    sum_vol_bin = sum_vol_bin - Sorted_volumes[0]
    itemsprofit = Sorted_profits[0]
    #print(sum_vol_bin)
        
    for elt in new_itemlist: #item list excluding item i
        if elt[0] <= sum_vol_bin and sum_vol_bin>=0: #if item fits in bin
            sum_vol_bin = sum_vol_bin - elt[0]
            #crit = sum_vol_bin/bincost
            #new_item_crit.append(crit)
            
            itemsprofit = itemsprofit + elt[1]
            #packed_item_profit.append(itemsprofit)
    
    
    
        
            
    if itemsprofit > bincost:
        s = 0 # true
    else:
        s = 1
#    print(sum_vol_bin, itemsprofit, bincost)
    return (s)
    

def HS1(binlist, itemlist):
    used_bin = 0 #index of used_bin
    bin_cost = 0
    total_profit = 0
    z=0
    
    for i in range(len(binlist)):
        if binlist[i][0]>itemlist[0][0]:
            sum_volume = [binlist[i][0]]   #put the first item in the first bin it fits
            cost_bin_1 = sum_volume[0]
            profit_item_1 = itemlist[0][1]
            z =i
            break
    
    #sum_volume = [binlist[0][0]] #sum of volume of each bin after packing
    
    solution = [[0, itemlist[0][0], z, sum_volume[0]]] #item number, volume of item,index of bin, volume of bin
    sum_volume[used_bin] = sum_volume[used_bin] - itemlist[0][0]
    solution[used_bin] = [0, itemlist[0][0], used_bin, sum_volume[used_bin]]
                             
    bin_number = 0 #number of bins used
    rejected_item = []
    for t in range(1, len(itemlist)):
        used_bin = 0   
        min_cost_contr = []
        while used_bin <= bin_number:
            min_cost_contr = [sum_volume[s]/binlist[s][1] for s in range(bin_number+1) if 
                                itemlist[t][0] <= sum_volume[s] ]
            #min_cost_contr = np.array(min_cost_contr)
            
#            print("res_bin", min_residual_bin, bin_number)
            
            #if min_cost_contr!= [] and itemlist[t][0]<= sum_volume[b]: 
            if  sum_volume[used_bin] - itemlist[t][0] >0:    
                #min_bin = max(min_cost_contr)
                bin_index = min_cost_contr.index(max(min_cost_contr))
                #bin_index = sum_volume[bin_index]
               
                if itemlist[t][0]<=sum_volume[bin_index]:
                    sum_volume[bin_index] = sum_volume[bin_index] - itemlist[t][0]
                    total_profit+=itemlist[t][1]
                    solution.append([t, itemlist[t][0], bin_index, sum_volume[bin_index]])
#                print("1", sum_volume, solution)
                    used_bin = used_bin+1
                break
                
            #carry on from here
            else:
                used_bin = used_bin + 1
                if used_bin > bin_number:
                    
                    if itemlist[t][2] == 1 and binlist[used_bin][0] - itemlist[t][0]>=0:
                        solution.append([t, itemlist[t][0],used_bin, binlist[used_bin][0] - itemlist[t][0]])
                        bin_cost+= binlist[used_bin][1]
                        total_profit+=itemlist[t][1]
                        bin_number = bin_number + 1
                        sum_volume.append(binlist[used_bin][0] - itemlist[t][0])
#                    sum_volume.append(binlist[used_bin][0])
#                    print("2", sum_volume, solution)
                        break
                    else:
                        newitemlist = itemlist[t:]
                        newbinlist = binlist[used_bin:len(binlist)] #changed this line
                        if prof(newitemlist, newbinlist) == 0:
                            solution.append([t, itemlist[t][0], bin_num + used_bin , binlist[bin_num+used_bin][0] - itemlist[t][0]])
                            bin_number = bin_number + 1
                            bin_cost+= binlist[bin_num + used_bin ][1]
                            total_profit+=itemlist[t][1]
                            sum_volume.append(binlist[bin_num+used_bin][0] - itemlist[t][0])
                           
                            break
                        else:
                            rejected_item.append(itemlist[t])
                            break
                        newitemlist = []
                
                    
    Total_bin_used = max(solution, key=itemgetter(2))[2] + 1    
    #Total_cost_bin = sum([binlist[h][1] for h in range(1,Total_bin_used)]) #len(sum_volume) 
    Total_cost_bin=cost_bin_1+bin_cost
    
    Loaded_items = [elem for elem in itemlist if elem not in rejected_item]   
    Total_profit_item = sum([Loaded_items[g][1] for g in range(len(Loaded_items))]) 
    Total_profit_item = total_profit+profit_item_1 
    
    obj_func = Total_cost_bin - Total_profit_item
    #obj_func =  -1 * Total_profit_item
            
    return(solution, Total_bin_used, Total_cost_bin, Total_profit_item, rejected_item, obj_func) #, sum_volume
                                
                            
                       
                                
def postopt(mysolution, binlist):
    list_solution = mysolution[0]
    number_bin = mysolution[1]
    sum_items_inbin = [0]*number_bin
    
    #    replace_bin = [0]*number_bin
    unused_bin = binlist[number_bin+1:]
    used_bin = binlist[:number_bin+1]
    
    for x in range(number_bin):
        for sol in list_solution:
            if sol[2] == x:
                sum_items_inbin[x] += sol[1]
                #    print(sum_items_inbin)
            
    for y in range(number_bin): #number_bin
        for abin in unused_bin:
            if sum_items_inbin[y] <= abin[1] and abin[1] < binlist[y][1]: 
                used_bin[y] = abin
                break
            #    print(abin)
            
    New_cost_bin = sum([used_bin[l][1] for l in range(number_bin)])
    Profit_items = mysolution[3]
    
    Net_cost = New_cost_bin-Profit_items
    
    return (used_bin, Net_cost)
                            
                            
                            
                            
                            
                            
                            
                            
