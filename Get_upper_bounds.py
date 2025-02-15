#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 16 02:12:19 2024

@author: funanani
"""

import os
import time
import numpy as np
import gurobipy as grb
import pandas as pd
import numpy as np
import copy
import glob
import os
import csv
import re

from operator import itemgetter
from sklearn import preprocessing

import characteristics_instances as problem_descr
import algo_proposed as algorithms
import UFFD_UBFD as fb
import HS1m as hs



instances_path = ''



output_file_name = 'results_251_sol.csv'

with open(output_file_name, 'w+', newline='') as csvfile:
    writer = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)
    #writer.writerow(['Instances', 'Source', 'feature_n', 'feature_Mean', 'feature_Median', 'feature_Var', 'feature_Max', 'feature_Min', 'feature_Large', 'feature_Medium', 'feature_Small', 'feature_Tiny', 'feature_mini', 'algo_FFD', 'algo_BFD', 'algo_HS1'])
   # writer.writerow(['Instances', 'Source', 'feature_n', "LB", "nGreedy", "GRASPm", "LNSm", "GRASP_LNS", "LNS_Local"])
    
    # source refers to folders inside instances folder
    for source in os.listdir(os.getcwd() + instances_path):
        if os.path.isdir(os.getcwd() + instances_path + '/' + source):
            # instance_file refer to text files containing instances
            for instance_file in os.listdir(os.getcwd() + instances_path + '/' + source):

                if instance_file[-4:]=='.txt' and instance_file[:2] != '._':
                    # we have a bin packing instance to analyse
                    #print(instance_file)
                    f = open(os.getcwd() + instances_path + '/' + source + '/' + instance_file)
                    fd = f.readlines()
                    
                    
                    line_n_items = str(fd[3])
                    
                    n_items = re.findall(r'\d+', line_n_items)
                    n = int(n_items[0]) # number of items
                    
                    if n<30:
                        print(instance_file)
                        index_items = fd.index('ITEMS_SECTIONS\n')
                        index_bins = fd.index('BINS_SECTIONS\n')
                        
                        items_list = fd[index_items+1:]
                        bins_list = fd[index_bins+1:index_items]
                        
                        def items (my_items_list):
                            my_l = []
                            item_l = []
                            for i in range(len(my_items_list)):
                                my_l.append(my_items_list[i].split())
                        #        item_l.append([int(my_l[-1][1]), int(my_l[-1][2]), int(my_l[-1][3])]) #volume, profit, and compulsory or not of items
                                item_l.append([int(my_l[-1][1]), int(my_l[-1][2]), int(my_l[-1][-1])])    
    
                            return (item_l)
    
                        def bins (my_bins_list):
                            my_ll = []
                            bin_l = []
                            for j in range(len(my_bins_list)):
                                my_ll.append(my_bins_list[j].split())
                        #        bin_l.append([int(my_ll[-1][1]), int(my_ll[-1][2]), int(my_ll[-1][4])]) #volume and cost of bins
                                bin_l.append([int(my_ll[-1][1]), int(my_ll[-1][2]), int(my_ll[-1][-1])])
    
                            return (bin_l)
    
                        IL = items(items_list) #list of items
                        BL = bins(bins_list)
                        
                    
    
                        def my_SBL(my_BL):             #list of bins
                            C = []
                            B = [0]*len(my_BL)
                            out = []
                            for r in range(len(my_BL)):       
                                C = my_BL[r]
                                B[r] = [C.copy() for v in range(my_BL[r][-1])]
                                out += B[r]
                                C = []
                            return (out)
    
                        SIL = IL
                        SBL = my_SBL(BL)
                        sorte_SBL = sorted(SBL, key = itemgetter(1), reverse = False) #sort bin list in increasing costs
    
    
                        item_array = np.array(SIL)
                        items_volumes = item_array[:,0]
                        items_profits = item_array[:,1]
                        items_comp = item_array[:,2]
                        
                        bin_array = np.array(SBL)
                        bin_capacity = bin_array[:,0]
                        n_bins = len(bin_capacity) #total number of bins"""
                        #n_bins = len(np.array(SBL)[:,0])

                        items, bins, max_vol_req = problem_descr.instance_characteristics(SBL, SIL)
                        
                     
                        #-----GUROBI OPTIMIZATION-------
                        
                        
                        #%%
                        
                        model = grb.Model('gbpp')

                        C = [bins[j].C for j in range(n_bins)]
                        V = [bins[j].V for j in range(n_bins)]
                        p = [items[i].p for i in range(n)]
                        v = [items[i].v for i in range(n)]

                        
                        # Y = 1 if bin j is used
                        Y = model.addVars(n_bins, vtype=grb.GRB.BINARY, name='Y')
                        
                        X = model.addVars(n, n_bins, vtype=grb.GRB.BINARY, name='X')
                        
                        # obj
                        expr = sum(
                            C[j] * Y[j] for j in range(n_bins)
                        )
                        expr -= grb.quicksum(p[i] * X[i,j] for i in range(n) for j in range(n_bins))
                        model.setObjective(expr, grb.GRB.MINIMIZE)
                        model.update()
                        
                        # aggregate knapsack capacity constraints
                        model.addConstrs(
                            (grb.quicksum(v[i]*X[i,j] for i in range(n)) <= V[j]*Y[j] \
                                            for j in range(n_bins)),
                            name="capacity_constraint"
                        )
                        
                        
                        for i in range(n):
                            if items_comp[i] == 1:
                            #compulsory items 1
                                model.addConstr(
                                    ( grb.quicksum(X[i,j] for j in range(n_bins)) == 1),
                                    name="compulsory_items"
                                    )
                                
                            else:
                        # non-compulsory items 0 or 1
                                model.addConstr(
                                    ( grb.quicksum(X[i,j] for j in range(n_bins)) <= 1 ),
                                    name="non_compulsory_items"
                                    )
                        
                        
                        model.setParam('TimeLimit', 40)
                        model.optimize()

                        
                        def change_form(items, bins):
                            for j in range(n_bins):
                                for i in range(n):
                                    if X[i,j].X > 0.5:
                                        bins[j].add_item(items[i])
                            return bins

                        sol = problem_descr.Solution(change_form(items, bins), max_vol_req)
                        
                        #%%
                        
                  
                        for bin in bins:
                            bin.reset_bin()
                            
                        
                        
                        #----UFFD, UBFD, HS1m-----
                        
                        #%%
                                            
                        
                        FFD_main_solution = fb.FFD(SBL, SIL)    
                        FFD_main_solution = fb.FFD(sorte_SBL, SIL)
                        #FFD_Bin_used_ratio = FFD_main_solution[1]/number_of_bins
                        FFD_net_cost = FFD_main_solution[4]
                        
                        print("U-FFD:")
                        print(FFD_net_cost)
                        print("")
                        
                        #FFD_final_solution = bpalgs.postopt(FFD_main_solution, SBL)
                        #FFD_final_solution = bpalgs.postopt(FFD_main_solution, sorte_SBL)
                   
                        BFD_main_solution = fb.BFD(SBL, SIL)    
                        BFD_main_solution = fb.BFD(sorte_SBL, SIL)
                       # BFD_Bin_used_ratio = BFD_main_solution[1]/number_of_bins
                        BFD_net_cost = BFD_main_solution[5]
                        print("U-BFD:")
                        print(BFD_net_cost)
                        print("")
                        
                        
                        HS1_main_solution = hs.HS1(SBL, SIL)    
                        HS1_main_solution = hs.HS1(sorte_SBL, SIL)
                        HS1_other_sol = hs.postopt(HS1_main_solution, sorte_SBL)
                        #HS1_Bin_used_ratio = HS1_main_solution[1]/number_of_bins
                        HS1_net_cost = HS1_main_solution[5]
                        
                        print("HS1m:")
                        print(HS1_net_cost)
                        print("")
                        #HS1_net_cost1 = HS1_other_sol[1]
                        
                        #%%
                        
                        #---nGreedy---
                        
                        #%%
                        
                        print("")
                        
                        nGreedy = algorithms.nGreedy(items, bins, max_vol_req)
                        
                        print('nGreedy:')
                        nGreedy.print()
                        print(nGreedy.net_cost(items))
                        print("")
                        
                        #%%
                        
                        
                        #-----GRASPm----
                        
                        
                        #%%
                            
                  
                        GRASPm = algorithms.GRASPm(items, bins, max_vol_req)
                        print('GRASPm:')
                        GRASPm.print()
                        print(GRASPm.net_cost(items))
                        print("")
                        
                        #%%
                        
                        #---LNSm---
                        
                        
                        #%%
                        
                        LNSm = algorithms.LNSm(items, bins, max_vol_req)
                        print('LNSm:')
                        LNSm.print()
                        print(LNSm.net_cost(items))
                        print("")
                        
                        #%%
                        
                        
                        #------GRASP_LNS-----
                        
                        #%%
                        

                        GRASP_LNS = algorithms.GRASP_LNS(items, bins, max_vol_req)
                        print('GRASP_LNS:')
                        GRASP_LNS.print()
                        print(GRASP_LNS.net_cost(items))
                        print("")
                        
                        #%%
                        
                        #----LNS_Local----
                        
                        #%%
                        
                        LNS_Local = algorithms.LNSm(items, bins, max_vol_req)
                        print('LNS_Local:')
                        LNS_Local.print()
                        print(LNS_Local.net_cost(items) )
                        print("")
                        
                        #%%
                        
 
                      #  writer.writerow([instance_file, source, n, model.ObjVal ,nGreedy.net_cost(items), GRASPm.net_cost(items), LNSm.net_cost(items), GRASP_LNS.net_cost(items), LNS_Local.net_cost(items) ])
                        f.close()

