import pandas as pd
import numpy as np
import copy
import glob
import os
import csv
import re

from operator import itemgetter
from sklearn import preprocessing

import UFFD_UBFD
import HS1m
#import MILP_GBPP
#import optim_sol

instances_path = ''



output_file_name = ''

with open(output_file_name, 'w+', newline='') as csvfile:
    writer = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)
    #writer.writerow(['Instances', 'Source', 'feature_n', 'feature_mean', 'feature_median', 'feature_var', 'feature_max', 'feature_min', 'feature_large', 'feature_medium', 'feature_small', 'feature_tiny', 'feature_mini'])
    #writer.writerow(['Instances', 'Source', 'feature_n', 'min_w', 'max_w', 'min_p', 'max_p'])
    
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
                    
                    if n<530:
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
                        number_of_bins = len(bin_capacity) #total number of bins
                        
                        normalized_weights = preprocessing.normalize([items_volumes])
                        normalized_profits = preprocessing.normalize([items_profits])
                        
                        
                        normalized_weights = normalized_weights[0]
                        normalized_profits = normalized_profits[0]
                       
                        min_weights = np.min(items_volumes)
                        max_weights = np.max(items_volumes)
                        
                        min_profits = np.min(items_profits)
                        max_profits = np.max(items_profits)
                       
                        #mean_weights = np.mean(normalized_weights)
                        #std_weights = np.std(normalized_weights)
                        
                        #mean_profits = np.mean(normalized_profits)
                        #std_profits = np.std(normalized_profits)
                        
                        
    
                       #writer.writerow(['Instances', 'Source', 'feature_n', 'feature_Mean', 'feature_Median', 'feature_Var', 'feature_Max', 'feature_Min', 'feature_Large', 'feature_Medium', 'feature_Small', 'feature_Tiny', 'feature_mini'])
                       
                        #writer.writerow([instance_file, source, n, min_weights, max_weights, min_profits, max_profits, ])
                        #writer.writerow([instance_file, source, n, mean, median, var, maximum, minimum, large, medium, small, tiny, mini ])
                        f.close()