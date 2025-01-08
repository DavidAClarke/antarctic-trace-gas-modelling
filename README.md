# Antarctic trace gas modelling [in progress]
Modelling and predicting rates of Antarctic trace gas oxidation across space (i.e., Antarctic ice-free areas).

## Climate data
Temperature data for use in the spatial predictions was obtained from [CHELSA v2.1](https://chelsa-climate.org/downloads/). There are four time periods, where all but the first (current climate) are associated with outputs from five global climate models and three emmissions scenarios. Functions for reading and manipulating the temperature data requires the following folder structure:

|--- chelsa  
&nbsp;&nbsp;&nbsp;&nbsp;|--- 1981-2010  
&nbsp;&nbsp;&nbsp;&nbsp;|--- 2011-2040  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- model 1  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- ssp126  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- ssp370  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- ssp585  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- model 5  
&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;|--- 2071-2100
