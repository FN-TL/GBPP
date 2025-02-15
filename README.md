# GBPP

We first initialize the lists we need to compile (using initialize.R), which will be lists of net costs achieved or list of number of bin used, depending on what we want.
We then run everything at once to compile the net costs to excel, or the number of bins used (using Run_everything.R). Preferably each bin type (3 or 5) of each class separately.
We then run the boxplots and anova (using Boxplotand Anova.R)
lastly, we run the validation to get the results from the Friedman and Nemenyi tests (using validation.R)


#GBPP2

Upper bound and lower bound solutions are compiled (Using UFFD_UBFD.py, HS1m.py, characteristics_instances.py, algo_proposed.py and Get_upper_bounds.py).
The percentage gap between the lower bounds and upper bounds is then calculated (Using Optim_gap.py)
Features are then selected (Using feature_selection.py).
After which the feature space is compiled using the features and the percentage gap (using feature_space.py).
new instances are generated from old instances (using min_volumes_and_profits.py gen_instances.py).
Boxplots and anova results are compiled (using Boxplots_and_anova.R).
Lastly, the results are validated using the Friedman and Nemenyi tests (using data_validation.R)
