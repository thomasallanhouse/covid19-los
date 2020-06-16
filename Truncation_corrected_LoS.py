"""
Truncation_corrected_LoS.py - 
To run this file, first ensure that there is a data file in the directory.

Input data should be for a single length of stay conditioned  on any
predictor variables of interest.

The model is straightforward to loop for different input files based on
different predictor variable. We have not supplied our processing script, 
since this is best constructed to suit the required purpose. 

Data should take the form of two columns, where the first column is the
date of the first event and the second column is the date of the second
event. This should be saved as a .csv. Currently this code assumes the 
delimiter is a space, but this can be changed on line 66 as required.

The code will fit truncated Weibull distributions to the data.

Maximum likelihood estimates are returned for the distribution parameters.

Before running the code, the truncation date (T) on line 55 should be amended
as required.
"""    
#### load required packages start
import os
import numpy as np 
import csv
import math
from scipy.optimize import minimize
from scipy.stats import weibull_min
from scipy.special import gamma
#### load required packages end


##### define functions start 
def loglikelihood(I,a,b,T):
    # calculates the inverse of the loglikelihood function for observing data I
    # when the distribution is truncated at time T, given shape a and scale b 
    event1=I[:,0]; # first events
    event2=I[:,1]; # second events
    L = weibull_min.pdf(event2-event1,b,0,a)/weibull_min.cdf(T-event1,b,0,scale = a);
    logL=(sum(np.log(L))**(-1)); # calculate inverse loglikelihood
    return(logL);

def function(x):
    # turning the loglikelihood into a function of the underlying parameters
    # a and b 
    a=x[0];
    b=x[1];
    return(loglikelihood(I, a, b, T))
#### define functions end


#### input parameters start
T = 43977; # truncation date, currently based on excel date conversion
x0=[1,1]; # initial value for minimisation
#### input parameters end


#### load data files start
pathname = "" # specify input file location
results = {} # preallocated results dictionary
os.chdir(pathname); # move to input file directory
inname = 'inputname'+'.csv'; # specify input file name
with open(inname, newline='') as csvfile:
    data = list(csv.reader(csvfile,delimiter=" ")) # load input data, change delimiter if required
I=np.array(data[1:], dtype=np.float) # turn data into required format
#### load data files end
    
#### find MLE start
output = minimize(function, x0,method='nelder-mead',
                  options={'xatol': 1e-8, 'disp': True}); # minimize inverse loglikelihood

a = output.x[0] # MLE scale parameter
b = output.x[1] # MLE shape parameter

mean=a*gamma(1+1/b); # MLE mean
variance=a**2*(gamma(1+2/b)-(gamma(1+1/b))**2); # MLE variance
SD=math.sqrt(variance); # MLE standard deviation
#### find MLE end
    





