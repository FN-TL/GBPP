#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 15 12:40:36 2024

@author: funanani
"""

import pandas as pd

data_features = pd.read_csv('')


data_features_2 = pd.read_csv('')

#data_features2 = data_features_2.loc[:1700]
#data_features1 = data_features.loc[738:]
data_all = pd.concat([data_features, data_features_2], axis = 0)



data_all = data_all.rename(columns = {'algo_HS1' : 'algo_HS1m', 'algo_GREEDY' : 'algo_nGREEDY', 'algo_GRASP' : 'algo_GRASPm', 'algo_LNS' : 'algo_LNSm' })
data_all = data_all.iloc[:, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]]
#print(data_all)
#data_all = data_all.loc[ : 735]

#data_all = data_all.drop("algo_HS1m", axis = 1)
#data_all = data_all.drop("algo_nGREEDY", axis = 1)
#data_all = data_all.drop("algo_GRASPm", axis = 1)
#data_all = data_all.drop("algo_LNSm", axis = 1)

#data_all = data_all.drop(data_all.columns[[21, 24]], axis = 1)

#data_features2.to_csv('results_feature_space_origen200_more.csv', index = False)


data_all.to_csv('', index = False)