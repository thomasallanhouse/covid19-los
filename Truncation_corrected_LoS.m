%{
Truncation_corrected_LoS.m - 
To run this file, first ensure that there is a data file in the directory.

Input data should be for a single length of stay conditioned  on any
predictor variables of interest.

The model is straightforward to loop for different input files based on
different predictor variable. We have not supplied our processing script, 
since this is best constructed to suit the required purpose. 

Data should take the form of two columns, where the first column is the
date of the first event and the second column is the date of the second
event.

The code will fit truncated Weibull distributions to the data.

Maximum likelihood estimates are returned for the distribution parameters.

Before running the code, the truncation date (T) should be amended as 
required.
%}    



T=43977; %define truncation date, this value is based on excel date conversion
x0=[0.1,0.1]; %initial conditions for fminsearch
f=@(x)loglikelihood(I,x(1),x(2),T).^(-1); %loglikelihood function
out=fminsearch(f,x0); %output,
a=out(1);b=out(2); % a = MLE scale parameter, b = MLE shape parameter
mean=a*gamma(1+1/b); % MLE mean
variance=a^2*(gamma(1+2/b)-(gamma(1+1/b))^2); % MLE variance
SD=sqrt(variance); % MLE standard deviation
%__________________________________________________________________________
function [logL] = loglikelihood(I,a,b,T)

        event1=I(:,1); %first event time
        event2=I(:,2); %second event time
        f1=@(x) wblpdf(event2-x,a,b);
        f2=@(x) wblcdf(T-x,a,b);
        l=f1(event1)./f2(event1);
        logL=sum(log(l)); 

end