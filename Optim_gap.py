#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 15 12:17:47 2024

@author: funanani
"""

import pandas as pd


data_optim = pd.read_csv('')

data_optim
data_all = pd.DataFrame(data_optim)
#data_all["optim"] = data_optim.obj
data_all



data_all_optim = data_all

data_all_optim['U-FFD'] = data_all_optim.apply(lambda x: (x['obj'] - x['U-FFD'])/x['obj'], axis = 1)
data_all_optim['U-BFD'] = data_all_optim.apply(lambda x: (x['obj'] - x['U-BFD'])/x['obj'], axis = 1)
data_all_optim['HS1m'] = data_all_optim.apply(lambda x: (x['obj'] - x['HS1m'])/x['obj'], axis = 1)

data_all_optim['nGreedy'] = data_all_optim.apply(lambda x: (x['obj'] - x['nGreedy'])/x['obj'], axis = 1)
#data_all_optim['first_fit_optim_gap'] = data_all.apply(lambda x: (x['optim'] - x['first_fit'])/x['optim'], axis = 1)
#data_all_optim['local_search_optim_gap'] = data_all.apply(lambda x: (x['optim'] - x['local_search'])/x['optim'], axis = 1)
data_all_optim['GRASPm'] = data_all_optim.apply(lambda x: (x['obj'] - x['GRASPm'])/x['obj'], axis = 1)
data_all_optim['LNSm'] = data_all_optim.apply(lambda x: (x['obj'] - x['LNSm'])/x['obj'], axis = 1)
data_all_optim['GRASP_LNS'] = data_all_optim.apply(lambda x: (x['obj'] - x['GRASP_LNS'])/x['obj'], axis = 1)
data_all_optim['LNS_Local'] = data_all_optim.apply(lambda x: (x['obj'] - x['LNS_Local'])/x['obj'], axis = 1)

#data_all.to_csv('results_allNet_100.csv', index = False)
data_all_optim = data_all_optim.drop("obj", axis = 1)



data_all_optim.to_csv('', index = False)


