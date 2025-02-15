#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 14 22:33:12 2024

@author: funanani
"""

import characteristics_instances as problem_descr
import UFFD_UBFD
import copy
import numpy as np

#------nGreedy------

#%%

def nGreedy(items, bins, max_vol_req):
    
    SIL_i = copy.deepcopy(items)
    bins1 = copy.deepcopy(bins)
    res_vol_req = max_vol_req

    # Assign each item to the first bin that has enough capacity
    for item in SIL_i:
        for bin in bins1:
            if bin.V_res >= item.v: 
                if bin.items == [] and bin.C <= res_vol_req:  
                    res_vol_req -= bin.C
                    bin.add_item(item)
                    bin.V_res -= item.v
                else:
                    bin.add_item(item)
                    bin.V_res -= item.v
                break

    sol = problem_descr.Solution(bins1, res_vol_req)
    return sol

#%%

#-----variant_of_first_fit-----


#%%

def U_ff(items, bins, max_vol_req, items_order = 'p/v', 
                bins_order = 'C'):
    
    SIL_i = copy.deepcopy(items)
    bins1 = copy.deepcopy(bins)
    res_vol_req = max_vol_req

    SIL_i, bins1 = item_bin_order(SIL_i, bins1, items_order, bins_order)

    # Bins are divided in selected and free ones
    selected_bins = []
    unused_bins = copy.copy(bins1)
    # Assign each item to the first bin that has enough capacity
    for item in SIL_i:
        if item.compulsory: #compulsory items are put in the first 
                            #available bin
            for bin in bins1:
                if bin.V_res >= item.v and bin.C <= res_vol_req:
                    if bin not in selected_bins:    
                        selected_bins.append(bin)
                        unused_bins.remove(bin)
                        res_vol_req -= bin.C
                    bin.add_item(item)
                    bin.V_res -= item.v
                    break
        else: #non compulsory
            flag = 0 
            for bin in selected_bins:
                #nc items are added if some of the selected bins is 
                #available 
                if bin.V_res >= item.v and bin.C <= res_vol_req:
                    bin.add_item(item)
                    bin.V_res -= item.v
                    flag = 1
                    break
            if flag == 1:
                continue
            # If no bin was available, items are added only if 
            # convenient
            for fbin in unused_bins:
                ind = SIL_i.index(item) 
                items2 = SIL_i[ind:] #not yet collected items 
                profit = 0
                for it in items2:
                    if fbin.V_res >= it.v and fbin.C <= res_vol_req:  
                        profit = profit + item.p
                
                if profit > fbin.C:
                    selected_bins.append(fbin)
                    unused_bins.remove(fbin)
                    fbin.add_item(item)
                    fbin.V_res -= item.v
                    res_vol_req -= fbin.C
                    break
    
    # We search among the free bins if there if someone with lower cost
    # that can substitute a selected one.
    for bin in selected_bins:
        for other_bin in unused_bins:
            if bin.V-bin.V_res <= other_bin.V and other_bin.C < bin.C:
                selected_bins.remove(bin)
                unused_bins.append(bin)
                selected_bins.append(other_bin)
                unused_bins.remove(other_bin)
                for item in bin.items:
                    other_bin.add_item(item)
                bin.reset_bin()
                res_vol_req = res_vol_req + bin.C - other_bin.C
                break
            
    sol = problem_descr.Solution(bins1, res_vol_req)
    return sol


#------sorting_items_and_bins-----

    
#%%

def item_bin_order(items, bins, items_order, bins_order):

    
    c_items = [item for item in items if item.compulsory]
    n_c_items = [item for item in items if not item.compulsory]

    #compulsory items can only be sorted by volumes (profit fixed)
    c_items = sorted(c_items, key=lambda x: x.v, reverse=True)

    if items_order == 'v':
        n_c_items = sorted(n_c_items, key=lambda x: x.v, reverse=True)
    if items_order == 'p/v':
        n_c_items = sorted(n_c_items, key=lambda x: x.p/x.v,
                            reverse=True)
    if items_order == 'p':
        n_c_items = sorted(n_c_items, key=lambda x: x.p, reverse=True)

    items = c_items + n_c_items
    
    if bins_order == 'V':
        bins = sorted(bins, key=lambda x: x.V, reverse=True)
    if bins_order == 'V/C':
        bins = sorted(bins, key=lambda x: x.V/x.C, reverse=True)
    if bins_order == 'C':
        bins = sorted(bins, key=lambda x: x.C, reverse=False)
    
    return items, bins

#%%


#------generate_neighbourhood-----

#%%


def generate_neighbourhood(solution, items, bins):

    old_sol_bins = copy.deepcopy(solution.bins) 
    bins_list = copy.deepcopy(solution.bins)
    old_budg_res = solution.max_vol_req_res
    budg_res = solution.max_vol_req_res

    neighbours = []
    
    # neighbours generating strat
    for bin in bins_list:
        bins1 = copy.deepcopy(old_sol_bins)
        for other_bin in bins:
            other_bin1 = copy.deepcopy(other_bin)
            if bin.V-bin.V_res <= other_bin.V and other_bin.C < bin.C:
                for item in bin.items:
                    other_bin1.add_item(item)
                bins1.append(other_bin1)
                for bin1 in bins1:
                    if bin1.name == bin.name:
                        bins1.remove(bin1)
                budg_res = budg_res + bin.C - other_bin.C
                neighbours.append(problem_descr.Solution(bins1, budg_res))
                budg_res = old_budg_res

    return neighbours

#%%


#------GRASPm------

#%%


def GRASPm(items, bins, max_vol_req, max_iter=1000):
    solutions = []
    #starting points
    solutions.append(U_ff(items, bins, max_vol_req, 'p/v', 'V'))
    solutions.append(U_ff(items, bins, max_vol_req, 'p', 'C'))
    solutions.append(U_ff(items, bins, max_vol_req, 'v', 'C'))
    solutions.append(U_ff(items, bins, max_vol_req, 'p', 'V')) 
    solutions.append(U_ff(items, bins, max_vol_req, 'v', 'V'))
    solutions.append(U_ff(items, bins, max_vol_req, 'p/v','C'))
    solutions.append(nGreedy(items, bins, max_vol_req))
    
    j = 0
    for current_best in solutions:
        j+=1  
        current_value = current_best.net_cost(items)

        for i in range(max_iter):
            neighbours = generate_neighbourhood(current_best, items, 
                                            bins)
            
        
            if not neighbours:
                break #if solution has no neighbours, break
            
            
            best_neighbour = min(
                neighbours, key=lambda x: x.net_cost(items)) #else, take the neighbour with least solution

           
            if best_neighbour.net_cost(items) < current_value:
                current_best = best_neighbour
                current_value = best_neighbour.net_cost(items) 
            else:
                break
        
        #update current solution if current best has a better objective value
        if j == 1:
            best_solution = current_best
        elif current_value < best_solution.net_cost(items):
            best_solution = current_best 
    
    return best_solution

#%%


#----------LNSm--------


#%%

def LNSm(items, bins, max_vol_req, max_iter=1000):
    solutions = []

    #starting poinrs
    solutions.append(U_ff(items, bins, max_vol_req, 'p/v', 'V'))
    solutions.append(U_ff(items, bins, max_vol_req, 'p', 'C'))
    solutions.append(U_ff(items, bins, max_vol_req, 'v', 'C'))
    solutions.append(U_ff(items, bins, max_vol_req, 'p', 'V')) 
    solutions.append(U_ff(items, bins, max_vol_req, 'v', 'V'))
    solutions.append(U_ff(items, bins, max_vol_req, 'p/v','C'))
    solutions.append(nGreedy(items, bins, max_vol_req))
    
    # Same process as in GRASPm, different neighbourhood strat.
    j = 0
    for current_best in solutions:
        j+=1  
        current_value = current_best.net_cost(items)

        for i in range(max_iter):
            
            neighbours = generate_large_neighbourhood(current_best, 
                                                items, bins)
            
        
            if not neighbours:
                break
            
            best_neighbour = min(
                neighbours, key=lambda x: x.net_cost(items))
            
            
            if best_neighbour.net_cost(items) < current_value:
                current_best = best_neighbour
                current_value = best_neighbour.net_cost(items)
            else:
                break
        
        if j == 1:
            best_solution = current_best
        elif current_value < best_solution.net_cost(items):
            best_solution = current_best 
    
    return best_solution

#%%


#-----generate_large_neighbourhood----

#%%

def generate_large_neighbourhood(solution, items, bins):
    
    old_sol_bins = copy.deepcopy(solution.bins) 
    bins_list = copy.deepcopy(solution.bins)
    budg_res = solution.max_vol_req_res
    SIL_i = copy.deepcopy(items)

   
    packed_item_name = [it.name for it in solution.items]
    free_items = [it for it in SIL_i if it.name not in packed_item_name]
    free_SIL_i = copy.deepcopy(free_items)
    free_SIL_i = sorted(free_SIL_i, key=lambda x: x.p, reverse=True)

    neighbours = []
    
    for bin in bins_list: 
        bins1 = copy.deepcopy(old_sol_bins)
        for bin1 in bins1:
            if bin.name == bin1.name:
                b = bin1
                break
       
        for item in b.items: #destroy method
            if not item.compulsory:
                b.items.remove(item)
                b.V_res += item.v

        for item in free_SIL_i: #repair method
            if item.v <= b.V_res:
                b.items.append(item)
                b.V_res -= item.v

        neighbours.append(problem_descr.Solution(bins1, budg_res))

    return neighbours

#%%


#----GRASP_LNS-----

#%%

def GRASP_LNS(items, bins, max_vol_req, max_iter=1000):
    current_best = GRASPm(items, bins, max_vol_req)
    # Calculate the objective function value for the current solution
    current_value = current_best.net_cost(items)

    for i in range(max_iter):
        neighbours = generate_large_neighbourhood(current_best, items, 
                                            bins)

        if not neighbours:
            break
        best_neighbour = min(
            neighbours, key=lambda x: x.net_cost(items))

        # If the chosen neighbouring solution has a lower objective 
        # function value than the current solution, update the 
        # current solution
        if best_neighbour.net_cost(items) < current_value:
            current_best = best_neighbour
            current_value = best_neighbour.net_cost(items)
        else:
            break  
    
    return current_best

#%%


#------LNS_Local------


#%%

def LNS_Local(items, bins, max_vol_req, max_iter=1000):
    current_best = LNSm(items, bins, max_vol_req)
    # Calculate the objective function value for the current solution
    current_value = current_best.net_cost(items)

    for i in range(max_iter):
        neighbours = generate_neighbourhood(current_best, items, bins)

        if not neighbours:
            break
        best_neighbour = min(
            neighbours, key=lambda x: x.net_cost(items))

        # If the chosen neighbouring solution has a lower objective 
        # function value than the current solution, update the 
        # current solution
        if best_neighbour.net_cost(items) < current_value:
            current_best = best_neighbour
            current_value = best_neighbour.net_cost(items)
        else:
            break  
    
    return current_best

#%%


