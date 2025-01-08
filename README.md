# Antarctic trace gas modelling [in progress]
Modelling and predicting rates of Antarctic trace gas oxidation across space (i.e., Antarctic ice-free areas).

## Climate data
Temperature data for use in the spatial predictions was obtained from [CHELSA v2.1](https://chelsa-climate.org/downloads/). There are four time periods, where all but the first (current climate) are associated with outputs from five global climate models and three emmissions scenarios. Functions for reading and manipulating the temperature data requires the following folder structure:

|--- chelsa  
&nbsp;&nbsp;&nbsp;&nbsp;|--- 1981-2010  
  &nbsp;&nbsp;&nbsp;&nbsp;|--- 2011-2040  
    |--- model 1  
      |--- ssp126  
      |--- ssp370  
      |--- ssp585  
    .  
    .  
    .  
    |--- model 5  
  .  
  .  
  .  
  |--- 2071-2100
