#import pandas as pd
import numpy as np
import copy
import glob
import os
import csv
import re
import pandas as pd


from operator import itemgetter
from sklearn import preprocessing
from ast import literal_eval




data = pd.read_csv('')


data_used = data[data.columns[3:9]]
data_used = pd.DataFrame(data_used)

min_w = np.min(data_used["min_w"] )
max_w = np.max(data_used["max_w"] )

min_p = np.min(data_used["min_p"] )
max_p = np.max(data_used["max_p"] )

bin_caps = list(data_used["bin_capacities"])
bin_ocs = list(data_used["bin_occurance"])

for j in range(len(bin_caps)):
    bin_caps[j] = eval(bin_caps[j])
    bin_ocs[j] = eval(bin_ocs[j])

bin_caps = np.array(bin_caps)
bin_ocs = np.array(bin_ocs)

mean_b = bin_caps.mean(axis = 0)
min_bo = bin_ocs.min(axis = 0)
max_bo = bin_ocs.max(axis = 0)


instances_path = ''

def generate_instances(m, n): #m is the number of bin types, n is the number of items
    np.random.seed(5)
    
    for i in range(5000):
        norm_w = np.random.uniform(min_w, max_w, n) #random weights
        norm_p = np.random.uniform(min_p, max_w, n) #random profits
        
        w = norm_w
        p = norm_p
        
        for x in range(n):
            p[x], w[x] = max(p[x],w[x]), min(p[x],w[x])
        
        c = np.random.choice([0,1], size = n, p = [2./3, 1./3])
        
        '''b1 = np.random.uniform(min_bo[0], max_bo[0], 1)
        b2 = np.random.uniform(min_bo[1], max_bo[1], 1)
        b3 = np.random.uniform(min_bo[2], max_bo[2], 1)
        b4 = np.random.uniform(min_bo[3], max_bo[3], 1)
        b5 = np.random.uniform(min_bo[4], max_bo[4], 1)'''
        
        b = []
        
        for l in range(m):
            b.append(np.random.uniform(min_bo[l], max_bo[l], 1))
        
        
        output_file_name = 'prob_gen_' + str(i)+'.txt'
        
        
        with open(os.getcwd() + instances_path + '/'+ output_file_name, 'w') as file_out:
            file_out.write('NAME GBP_INSTANCES\\' + output_file_name + ' \n')
            file_out.write("COMMENT \n")
            file_out.write("BINTYPES " + str(m) + " \n")
            file_out.write('ITEMS ' + str(n) + " \n")
            file_out.write("BINS_SECTIONS\n")
            for k in range(m):
                file_out.writelines(str(k) + ' ' + str(int(mean_b[k])) + ' ' +str(int(mean_b[k]))+ ' ' + str(0) + ' ' + str(int(b[k])) + ' \n')
            file_out.write("ITEMS_SECTIONS\n")
            for j in range(n):
                file_out.writelines(str(j) + ' ' + str(int(w[j])) + ' ' +str(int(p[j]))+ ' ' + str(c[j]) + ' \n')
            file_out.close()
    return(file_out)

generate_instances(5, 50)

                
                
                    
            
            
    


"""with open(output_file_name, 'w+', newline='') as csvfile:
    writer = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)
    #writer.writerow(['Instances', 'Source', 'feature_n', 'feature_mean', 'feature_median', 'feature_var', 'feature_max', 'feature_min', 'feature_large', 'feature_medium', 'feature_small', 'feature_tiny', 'feature_mini'])
    writer.writerow(['Instances', 'Source', 'feature_n', 'mean_w', 'std_w', 'mean_p', 'std_p'])
    
    # source refers to folders inside instances folder
    
                        
    
                       #writer.writerow(['Instances', 'Source', 'feature_n', 'feature_Mean', 'feature_Median', 'feature_Var', 'feature_Max', 'feature_Min', 'feature_Large', 'feature_Medium', 'feature_Small', 'feature_Tiny', 'feature_mini'])
                       
    writer.writerow([instance_file, source, n, mean_weights, std_weights, mean_profits, std_profits])
    #writer.writerow([instance_file, source, n, mean, median, var, maximum, minimum, large, medium, small, tiny, mini ])
    f.close()"""