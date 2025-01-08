# Antarctic trace gas modelling [in progress]
Modelling and predicting rates of Antarctic trace gas oxidation across space (i.e., across the Antarctic ice-free areas).

## Climate data
Temperature (tas) data for use in the spatial predictions was obtained from [CHELSA v2.1](https://chelsa-climate.org/downloads/). There are four time periods, where all but the first (current climate) are associated with outputs from five global climate models (GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL) and three emmissions scenarios (ssp126, ssp370, ssp585). Specifically, after following the above link and choosing v2.1, proceed as follows: GLOBAL > climatologies > [time period] > [model] > [scenario] > tas. Note that 1981-2010 does not have models or scenarios. Functions for reading and manipulating the temperature data requires the following folder structure (though replace "model n" with the model name, e.g. GFDL-ESM4):

|--- chelsa  
&nbsp;&nbsp;&nbsp;&nbsp;|--- 1981-2010  
&nbsp;&nbsp;&nbsp;&nbsp;|--- 2011-2040  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- model 1  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- ssp126  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- ssp370  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- ssp585  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- model 2  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|--- model 5  
&nbsp;&nbsp;&nbsp;&nbsp;|--- 2041-2070  
&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;.  
&nbsp;&nbsp;&nbsp;&nbsp;|--- 2071-2100
