#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 13 00:48:47 2024

@author: funanani
"""

import numpy as np

class Bin:
    def __init__(self, capacity, cost, n):
        self.V = capacity
        self.V_res = capacity
        self.C = cost
        self.name = f'bin{n}'
        self.n = n
        self.items = []

    def reset_bin(self):
        self.V_res = self.V
        self.items = []

    def add_item(self, item):
        self.items.append(item)


class Item:
    def __init__(self, profit, volume, compulsory, n):
        self.p = profit
        self.v = volume
        self.compulsory = compulsory
        self.n = n
        self.name = f'I_{n}'


class Solution:
    def __init__(self, bins, max_vol_req):
        sol = {}  
        bins1 = []  
        for bin in bins:
            if bin.items:   
                bins1.append(bin)
        self.bins = bins1
        for bin in self.bins:
            for item in bin.items:
                sol[item] = bin 
        self.sol = sol
        self.items = list(sol.keys())
        self.max_vol_req_res = max_vol_req

    def print(self):
 
        
        bins = []
        items = []
        for bin, item in self.sol.items():
            bins.append(bin.name)
            items.append(item.name)
        sol = dict(zip(bins, items))
        print(sol)

    def net_cost(self, items):
        
        if not self.is_valid(items):
            return float('inf') #if solution is infeasible

        total_cost = sum(bin.C for bin in self.bins)
        total_profit = sum(
            item.p for item in self.items)

        return total_cost - total_profit
    

    def is_valid(self, items):

        # max_vol_req constraint
        if self.max_vol_req_res < 0:
            return False

        # Capacity constraints
        for bin in self.bins:
            total_volume = sum(
                item.v for item in self.items if self.sol[item] == bin)
            if total_volume > bin.V:
                return False

        item_names = []
        for it in self.items:
            item_names.append(it.name)
        
        # Compulsory items' constraint
        for item in items:
            if item.compulsory and item.name not in item_names:
                return False

        return True


def instance_characteristics(binlist, itemlist):
    item_array = np.array(itemlist)
    items_volumes = item_array[:,0]
    items_profits = item_array[:,1]
    items_comp = item_array[:,2]
    
    bin_array = np.array(binlist)
    bin_capacity = bin_array[:,0]
    n_bins = len(bin_capacity) #total number of bins

    n_items = len(itemlist)
    

    C = np.array(binlist)[:,0] # costs of the bins
    V = np.array(binlist)[:,0]  # capacities of the bins
    p = np.array(itemlist)[:,1]  # profits of the items
    v = np.array(itemlist)[:,0]  # weights of the items
    
    items = []
    for i in range(n_items):
        if items_comp[i] == 1:
            item = Item(p[i], v[i], True, i+1) #profit, volume, comp = 1, item_number
            items.append(item)
        else:
            item = Item(p[i], v[i], False, i+1) #profit, volume, non-comp = 0, item_number
            items.append(item)

    bins = []
    for i in range(n_bins):
        bin = Bin(V[i], C[i], i+1) #volume, cost, bin_number
        bins.append(bin)  

    
    max_vol_req = sum(v)

    return items, bins, max_vol_req
