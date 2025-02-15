# -*- coding: utf-8 -*-
"""
Created on Wed Nov  4 11:34:16 2023

@author: Funanani
"""


import numpy as np
import os
import csv
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

#
def prof(new_itemlist, newbin):
    
    for i in range(len(newbin)):
        if  newbin[i][1] >= new_itemlist[0][0]:
            sum_vol_bin = newbin[i][0] #put the first item in the first bin it fits
            bincost = newbin[i][0]
            global bin_num
            bin_num = i
           
            break

        
    itemsprofit = new_itemlist[0][1]
    s = 0
    for elt in new_itemlist:
        if elt[0] <= sum_vol_bin:
            sum_vol_bin = sum_vol_bin - elt[0]
            itemsprofit = itemsprofit + elt[1]
            
    if itemsprofit > bincost:
        s = 0 # true
    else:
        s = 1
#    print(sum_vol_bin, itemsprofit, bincost)
    return (s)



#
def FFD(binlist, itemlist):
    used_bin = 0 #index of used_bin
    bin_cost = 0
    total_profit = 0
    for i in range(len(binlist)):
        if binlist[i][0]>itemlist[0][0]:
            profit_item_1 = itemlist[0][1]
            sum_volume = [binlist[i][0]]   #put the first item in the first bin it fits
            cost_bin_1 = sum_volume[0]
            break
    
    #sum_volume = [binlist[0][0]] #sum of volume of each bin after packing
    
    solution = [[0, itemlist[0][0], used_bin, sum_volume[0]]] #item number, volume of item,index of bin, volume of bin
    sum_volume[used_bin] = sum_volume[used_bin] - itemlist[0][0]
    solution[used_bin] = [0, itemlist[0][0], used_bin, sum_volume[used_bin]]
    
    bin_number = 0
    rejected_item = []
#    print("initial", sum_volume, solution)
       
    for t in range(1, len(itemlist)):
        used_bin = 0
        while used_bin <= bin_number:
            if itemlist[t][0] <= sum_volume[used_bin]: #binlist[used_bin][0]
#                solution.append([t, itemlist[t][0], used_bin, binlist[used_bin][0] - itemlist[t][0]])  #binlist[used_bin][0]
                sum_volume[used_bin] = sum_volume[used_bin] - itemlist[t][0]
                total_profit+=itemlist[t][1]
                solution.append([t, itemlist[t][0], used_bin, sum_volume[used_bin]])
#                print("1", sum_volume, solution)
                break
            else:
                used_bin = used_bin + 1
                if used_bin > bin_number:
                    
                    if itemlist[t][2] == 1 and binlist[used_bin][0] - itemlist[t][0]>=0: 
                        solution.append([t, itemlist[t][0],used_bin, binlist[used_bin][0] - itemlist[t][0]])
                        bin_number = bin_number + 1
                        bin_cost+= binlist[used_bin ][1]
                        total_profit+=itemlist[t][1]
                        sum_volume.append(binlist[used_bin][0] - itemlist[t][0])
#                    sum_volume.append(binlist[used_bin][0])
#                    print("2", sum_volume, solution)
                        break
                    else:
                        newitemlist = itemlist[t:]
                        newbinlist = binlist[used_bin:len(binlist)] #changed this line
                        if prof(newitemlist, newbinlist) == 0:
                            solution.append([t, itemlist[t][0],bin_num + used_bin, binlist[bin_num + used_bin][0] - itemlist[t][0]])
                            bin_number = bin_number + 1
                            bin_cost+= binlist[bin_num + used_bin ][1]
                            total_profit+=itemlist[t][1]
                            sum_volume.append(binlist[bin_num + used_bin][0] - itemlist[t][0])
                            break
                        else:
                            rejected_item.append(itemlist[t])
                            break
                        newitemlist = []
               
                        
    
    Total_bin_used = max(solution, key=itemgetter(2))[2] + 1    
    #Total_cost_bin = sum([binlist[h][1] for h in range(1,Total_bin_used)]) #len(sum_volume) 
    Total_cost_bin=cost_bin_1+bin_cost
    
    #Loaded_items = [elem for elem in itemlist if elem not in rejected_item]   
    #Total_profit_item = sum([Loaded_items[g][1] for g in range(len(Loaded_items))]) 
    Total_profit_item = total_profit+profit_item_1 
    
    obj_func = Total_cost_bin - Total_profit_item
    #obj_func =  -1 * Total_profit_item       
    return(solution, Total_bin_used, Total_cost_bin, rejected_item, obj_func) #, sum_volume


def BFD(binlist, itemlist):
    used_bin = 0 #index of used_bin
    total_profit = 0
    bin_cost = 0
    for i in range(len(binlist)):
        if binlist[i][0]>itemlist[0][0]:
            profit_item_1 = itemlist[0][1]
            sum_volume = [binlist[i][0]]   #put the first item in the first bin it fits
            cost_bin_1 = sum_volume[0]
            break
    
    #sum_volume = [binlist[0][0]] #sum of volume of each bin after packing
    
    solution = [[0, itemlist[0][0], used_bin, sum_volume[0]]] #item number, volume of item,index of bin, volume of bin
    sum_volume[used_bin] = sum_volume[used_bin] - itemlist[0][0]
    solution[used_bin] = [0, itemlist[0][0], used_bin, sum_volume[used_bin]]
 
    bin_number = 0 #number of bins used
    rejected_item = []
#    print("initial", sum_volume, solution)
       
    for t in range(1, len(itemlist)):
        used_bin = 0   
        min_residual_bin = []
        while used_bin <= bin_number:
            min_residual_bin = [sum_volume[s] for s in range(bin_number+1) if 
                                itemlist[t][0] <= sum_volume[s] ]
            
#            print("res_bin", min_residual_bin, bin_number)
            
            if min_residual_bin != []: 
                min_bin = min(min_residual_bin)
                bin_index = sum_volume.index(min_bin)
                total_profit+=itemlist[t][1]
                sum_volume[bin_index] = sum_volume[bin_index] - itemlist[t][0]
                solution.append([t, itemlist[t][0], bin_index, sum_volume[bin_index]])
#                print("1", sum_volume, solution)
                break
            else:
                used_bin = used_bin + 1
                
                if used_bin > bin_number:
                    if itemlist[t][2] == 1 and binlist[used_bin][0] - itemlist[t][0]>=0:
                        solution.append([t, itemlist[t][0],used_bin, binlist[used_bin][0] - itemlist[t][0]])
                        bin_number = bin_number + 1
                        bin_cost+= binlist[used_bin ][1]
                        total_profit+=itemlist[t][1]
                        sum_volume.append(binlist[used_bin][0] - itemlist[t][0])
#                    sum_volume.append(binlist[used_bin][0])
#                    print("2", sum_volume, solution)
                        break
                    else:
                        newitemlist = itemlist[t:]
                        newbinlist = binlist[used_bin:len(binlist)] #changed this line
                        if prof(newitemlist, newbinlist) == 0:
                            solution.append([t, itemlist[t][0],bin_num + used_bin, binlist[bin_num + used_bin][0] - itemlist[t][0]])
                            bin_number = bin_number + 1
                            bin_cost+= binlist[bin_num + used_bin ][1]
                            total_profit+=itemlist[t][1]
                            sum_volume.append(binlist[bin_num + used_bin][0] - itemlist[t][0])
                            break
                        else:
                            rejected_item.append(itemlist[t])
                            break
                        newitemlist = []
               
    Total_bin_used = max(solution, key=itemgetter(2))[2] + 1    
    #Total_cost_bin = sum([binlist[h][1] for h in range(1,Total_bin_used)]) #len(sum_volume) 
    Total_cost_bin=cost_bin_1+bin_cost
    
    #Loaded_items = [elem for elem in itemlist if elem not in rejected_item]   
    #Total_profit_item = sum([Loaded_items[g][1] for g in range(len(Loaded_items))]) 
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

