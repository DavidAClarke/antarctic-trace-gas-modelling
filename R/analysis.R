################################################################################
## Script name: analysis.R
## Author: David Clarke
## Copyright (c) David Clarke, 2025
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Load required libraries
pkgs <- c("tidyverse", "sf", "terra", "here", "readxl", "glmmTMB", "DHARMa", 
          "summarytools", "tidyterra", "performance")
lapply(pkgs, require, character.only = T)

## Setup file paths
shared_path <- "C:/Users/dcla0021/Documents/postdoc/projects/shared_data"
shared_path <- "C:/Users/david/Documents/postdoc/projects/shared_data"
chelsa <- here(shared_path, "environmental", "chelsa")

## Import user-defined functions
source("R/functions.R")

################################### Analysis ###################################
## Step 1: Prepare climate data----
## Model names, emission scenarios and time periods
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
scenarios <- c("ssp126", "ssp370", "ssp585")
time_periods <- c("1981-2010", "2011-2040", "2041-2070", "2071-2100")

## Create reference extent for cropping
my_ext <- ext(c(xmin = -180, xmax = 180, ymin = -90, ymax = -50))

## Crop and re-project global CHELSA temperature data to equal area polar projection
lapply(time_periods, function(i){
  
  ant_prepare(i, models = models, scenarios = scenarios, 
              my_ext = my_ext, data_path = chelsa)
  
})

## Combine Antarctic CHELSA temperature data from each GCM for each time period
# and emission scenario
lapply(time_periods[2:4], function(i){
  
  clim_combine(i, models = models, scenarios = scenarios, 
               data_path = chelsa)
  
})


## Step 2: Develop trace gas models----
## Load gas rate data
gases <- c("H2", "CO", "CH4")
gas_data <- list()

for(g in gases){
  
  # Read in excel sheet
  mydata <- read_excel(here(dirname(here()),"data", "Antarctic temps gas rates final.xlsx"), 
                       sheet = g, skip = 1) %>%
    
    # Manipulate the data into the proper format  
    pivot_longer(cols = 2:10) %>%
    #drop_na(value) %>%
    dplyr::mutate(value = replace_na(value,0)) %>%
    
    # Creating a new, higher level location variable
    mutate(region = case_when(str_detect(name, "Robinson") ~ "Robinson Ridge",
                              str_detect(name, "Bunger") ~ "Bunger Hills",
                              str_detect(name, "Dronning") ~ "Dronning Maud Land")) %>%
    
    # Convert characters to factors
    mutate_if(is.character,as.factor) %>%
    
    # Easier to work with simple variable names
    rename("temp" = `Temperature (°C)`)
  
  # Place the data frame into the list created above
  gas_data[[g]] <- mydata
  
}

## Create gas rate models
# Hydrogren - zero-inflated Gamma glm
h2m <- glmmTMB(value ~ poly(temp,2), family = ziGamma(link = "log"), 
               ziformula = ~ poly(temp,2), data = gas_data[["H2"]])
summary(h2m)

# Plot model
model_pred(gas_data[["H2"]], "H2", h2m)

# Assess model
simr <- simulateResiduals(h2m)
plot(simr)

# Carbon monoxide - zero-inflated Gamma glm
com <- glmmTMB(value ~ poly(temp,2), family = ziGamma(link = "log"), 
               ziformula = ~ poly(temp,2), data = gas_data[["CO"]])
summary(com)

# Plot model
model_pred(gas_data[["CO"]], "CO", com)

# Assess model
simr <- simulateResiduals(com)
plot(simr)

# Methane - zero-inflated Gamma glm
ch4m <- glmmTMB(value ~ poly(temp,2), family = ziGamma(link = "log"), 
               ziformula = ~ poly(temp,2), data = gas_data[["CH4"]])
summary(ch4m)

# Plot model
model_pred(gas_data[["CH4"]], "CH4", ch4m)

# Assess model
simr <- simulateResiduals(ch4m)
plot(simr)

## Step 3: Create spatial predictions----
# Load Antarctic coastline shapefile
coast <- st_read(here(shared_path, "environmental", "coastline", 
                      "Coastline.shp"), quiet = T) %>%
  
  st_transform("ESRI:102020")

# Read Antarctic temperature data (text example)
r <- rast(here(chelsa, "1981-2010", "CHELSA_tas_1981-2010_ant.tif"))

# Create spatial prediction for each month (i.e., layer)
# ld = T means it will return the raster the layer in addition to writing to disk
myr <- spat_pred(model = h2m, gas = "H2", clim_ras = r, 
                 msk = coast, time_period = "1981-2010", ld = T, 
                 pth = here(dirname(here()), "data"))

spat_pred(model = com, gas = "CO", clim_ras = r, 
          msk = coast, time_period = "1981-2010", ld = F, 
          pth = here(dirname(here()), "data"))
