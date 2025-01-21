################################################################################
## Script name: analysis.R
## Author: David Clarke
## Copyright (c) David Clarke, 2025
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Load required libraries
pkgs <- c("tidyverse", "sf", "terra", "here", "readxl", "glmmTMB", "DHARMa", 
          "summarytools", "tidyterra", "performance", "tidyterra")
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
confint(h2m)

# Plot model
model_pred(gas_data[["H2"]], "H2", h2m)

# Assess model
simr <- simulateResiduals(h2m)
plot(simr)

# Carbon monoxide - zero-inflated Gamma glm
com <- glmmTMB(value ~ poly(temp,2), family = ziGamma(link = "log"), 
               ziformula = ~ poly(temp,2), data = gas_data[["CO"]])
summary(com)
confint(com)

# Plot model
model_pred(gas_data[["CO"]], "CO", com)
# Alternatively, use marginaleffects::plot_predictions()
#plot_predictions(h2m, condition = "temp", points = 1, vcov = T, re.form = NA)

# Assess model
simr <- simulateResiduals(com)
plot(simr)

# Methane - zero-inflated Gamma glm
ch4m <- glmmTMB(value ~ poly(temp,2), family = ziGamma(link = "log"), 
               ziformula = ~ poly(temp,2), data = gas_data[["CH4"]])
summary(ch4m)
confint(ch4)

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

# Load ACBR shapefile
acbr <- st_read(here(shared_path, "biodiversity", "acbr", "ACBRs_v2_2016", 
                     "ACBRs_v2_2016.shp"), quiet = T) %>%
  
  st_transform("ESRI:102020")

## Current climate
# Read Antarctic temperature data (text example)
r <- rast(here(chelsa, "1981-2010", "CHELSA_tas_1981-2010_ant.tif"))

# Create spatial prediction for each month (i.e., layer)
# ld = T means it will return the raster the layer in addition to writing to disk
# Hydrogren
myr <- spat_pred(model = h2m, gas = "H2", clim_ras = r, 
                 msk = coast, time_period = "1981-2010", ld = T, 
                 pth = here(dirname(here()), "data"),
                 fname = "H2_1981-2010_ant.tif")

# Carbon monoxide
spat_pred(model = com, gas = "CO", clim_ras = r, 
          msk = coast, time_period = "1981-2010", ld = F, 
          pth = here(dirname(here()), "data"),
          fname = "CO_1981-2010_ant.tif")

## Future climates
# Hydrogren
for(t in time_periods[-1]){
  
  t_files <- list.files(here(chelsa, t), pattern = "tif", full.names = T)
  t_names <- list.files(here(chelsa, t), pattern = "tif", full.names = F)
  
  for(i in seq_along(t_files)){
    
    r <- rast(t_files[i])
    n <- gsub("CHELSA_mean_tas", "H2", t_names[i])
    
    spat_pred(model = h2m, gas = "H2", clim_ras = r, 
              msk = coast, time_period = t, ld = F, 
              pth = here(dirname(here()), "data"),
              fname = n)
    
  }
}

# Carbon monoxide
for(t in time_periods[-1]){
  
  t_files <- list.files(here(chelsa, t), pattern = "tif", full.names = T)
  t_names <- list.files(here(chelsa, t), pattern = "tif", full.names = F)
  
  for(i in seq_along(t_files)){
    
    r <- rast(t_files[i])
    n <- gsub("CHELSA_mean_tas", "CO", t_names[i])
    
    spat_pred(model = com, gas = "CO", clim_ras = r, 
              msk = coast, time_period = t, ld = F, 
              pth = here(dirname(here()), "data"),
              fname = n)
    
  }
}

# Carbon monoxide - TO TEST
#fut_spat_pred(time_periods[-1], "CO", chelsa, com)

## Mask predictions to ACBR layer
gas_preds <- list.files(here(dirname(here()), "data"), pattern = "tif", full.names = T)
gas_preds_nm <- list.files(here(dirname(here()), "data"), pattern = "tif", full.names = F)
gas_preds_nm <- gsub(".tif", "_acbr.tif", gas_preds_nm)

for(i in seq_along(gas_preds)){
  
  r <- rast(gas_preds[i])
  r <- mask(r, acbr)
  writeRaster(r, here(dirname(here()), "data", gas_preds_nm[i]), overwrite = T)
  
}

## Step 4: Making figures----
## Maps - first make a map of the mean of the 12 months
r <- rast(here(dirname(here()), "data", "H2_1981-2010_ant_acbr.tif"))
r2 <- rast(here(dirname(here()), "data", "CO_1981-2010_ant_acbr.tif"))

pred_maps(r, comb = T, fun = mean, gas = "H2", sve = T, fname = "H2_1981-2010")
pred_maps(r2, comb = T, fun = mean, gas = "CO", sve = T, fname = "CO_1981-2010")

h2_preds <- list.files(here(dirname(here()), "data"), pattern = glob2rx("*H2*acbr*"), full.names = T)
h2_names <- list.files(here(dirname(here()), "data"), pattern = glob2rx("*H2*acbr*"), full.names = F)

co_preds <- list.files(here(dirname(here()), "data"), pattern = glob2rx("*CO*acbr*"), full.names = T)
co_names <- list.files(here(dirname(here()), "data"), pattern = glob2rx("*CO*acbr*"), full.names = F)

for(h in seq_along(h2_preds)){
  
  fname <- gsub("_ant_acbr.tif", "", h2_names[h])
  
  r <- rast(h2_preds[h])
  
  pred_maps(r, comb = T, fun = mean, gas = "H2", sve = T, fname = fname, ret = F)
  
}

for(h in seq_along(co_preds)){
  
  fname <- gsub("_ant_acbr.tif", "", co_names[h])
  
  r <- rast(co_preds[h])
  
  pred_maps(r, comb = T, fun = mean, gas = "CO", sve = T, fname = fname, ret = F)
  
}

## Boxplots - examples

fig_list <- list()

for(t in time_periods[-1]){
  
  ind <- which(time_periods == t)

  co_t <- co_preds[str_detect(co_preds, t)]
  
  vals <- c()
  mnths <- c()
  scen <- c()
  
  for(h in seq_along(co_t)){
    
    r <- rast(co_t[h])
    
    for(i in 1:nlyr(r)){
      
      vals <- c(vals, values(r[[i]], na.rm = T))
      mnths <- c(mnths, rep(month.name[i], length(values(r[[i]], na.rm = T))))
      scen <- c(scen, rep(scenarios[h], length(values(r[[i]], na.rm = T))))
                
      }
    }
  
    df <- data.frame(vals = vals, 
                     mnths = factor(mnths, levels = month.name[1:12]),
                     scenario = factor(scen, levels = scenarios))
  
    
    fig_list[[ind]] <- ggplot(df, aes(x = mnths, y = vals, fill = scenario)) +
                        geom_boxplot() +
                        theme_bw() +
                        theme(panel.grid = element_blank()) +
                        xlab("Month") +
                        ylab(expression(Rate ~ (nmol ~ CO ~ hr^{-1} ~ g^{-1}))) +
                        scale_fill_manual(values = c("#d35199ff", "#52c2e8ff", "#f4e01dff"),
                          name = "Emission\nscenario") +
      ggtitle(t)
}

gg <- ggpubr::ggarrange(fig_list[[2]], fig_list[[3]], fig_list[[4]], 
                        nrow = 3, ncol = 1, common.legend = T)

ggsave(here(dirname(here()), "figures", "CO_boxes.pdf"), gg, device = "pdf", 
       units = "cm", width = 25, height = 20)
