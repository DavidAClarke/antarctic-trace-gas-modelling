#-------------------------------------------------------------------------------
# Analysing trace gas oxidation
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------
## Load required libraries
pkgs <- c("dplyr","ggplot2", "stringr", "sf", "terra", "here", "readxl", 
          "glmmTMB", "DHARMa", "tidyterra", "tidyterra")
lapply(pkgs, require, character.only = T)

## Setup file paths
shared_path <- "C:/Users/dcla0021/Documents/postdoc/projects/shared_data"
shared_path <- "C:/Users/david/Documents/postdoc/projects/shared_data"
chelsa <- here(shared_path, "environmental", "chelsa")

## Import user-defined functions
source("R/functions.R")

#-------------------------------------------------------------------------------
# Prepare climate data
#-------------------------------------------------------------------------------
## Model names, emission scenarios and time periods
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
scenarios <- c("ssp126", "ssp370", "ssp585")
time_periods <- c("1981-2010", "2011-2040", "2041-2070", "2071-2100")

## Create reference extent for cropping
my_ext <- ext(c(xmin = -180, xmax = 180, ymin = -90, ymax = -50))

## Crop & re-project temperature data to equal area polar projection
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

#-------------------------------------------------------------------------------
# Load gas rate data
#-------------------------------------------------------------------------------
gases <- c("H2", "CO", "CH4")
gas_data <- list()

for(g in gases){
  
  # Read in excel sheet
  mydata <- read_excel(here(dirname(here()),
                            "data", 
                            "Antarctic temps gas rates final.xlsx"), 
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

#-------------------------------------------------------------------------------
# Develop trace gas models
#-------------------------------------------------------------------------------
# Hydrogren - zero-inflated Gamma glm
h2m <- glmmTMB(value ~ poly(temp,2), family = ziGamma(link = "log"), 
               ziformula = ~ poly(temp,2), data = gas_data[["H2"]])
summary(h2m)
exp(confint(h2m))
h2_cv <- cv::cv(h2m, reps = 1, k = "loo")
#plot(h2_cv)

# Plot model
h2m_plot <- model_pred(gas_data[["H2"]], "H2", h2m)

## Temperature(s) with greatest changes in oxidation rate
xweight <- seq(min(gas_data[["H2"]]$temp), max(gas_data[["H2"]]$temp), 1) 
delta_ys <- c()

for(i in seq_along(xweight)){
  
  new_data <- data.frame(temp = c(xweight[i], xweight[i+1]))
  y_hat <- predict(h2m, new_data, type = "response")
  delta_y <- exp(diff(y_hat))
  delta_ys <- c(delta_ys, delta_y)
  
}

df <- data.frame(temp = xweight, delta_rate = abs(delta_ys - 1))
plot(df$temp, df$delta_rate, type = "line")
abline(h = as.numeric(summary(df$delta_rate)[5]), lty = 2)

qt3 <- which(df$delta_rate >= summary(df$delta_rate)[5]) # 3rd quart summary(df$delta_rate)
df[qt3,]

# Assess model
simr <- simulateResiduals(h2m, n = 1000)
plot(simr)

# Carbon monoxide - zero-inflated Gamma glm
com <- glmmTMB(value ~ poly(temp,2), family = ziGamma(link = "log"), 
               ziformula = ~ poly(temp,2), data = gas_data[["CO"]])
summary(com)
exp(confint(com))
co_cv <- cv::cv(com, reps = 1, k = "loo")

# Plot model
com_plot <-model_pred(gas_data[["CO"]], "CO", com)
# Alternatively, use marginaleffects::plot_predictions()
#plot_predictions(h2m, condition = "temp", points = 1, vcov = T, re.form = NA)
## Temperature(s) with greatest changes in oxidation rate
xweight <- seq(min(gas_data[["CO"]]$temp), max(gas_data[["CO"]]$temp), 1) 
delta_ys <- c()

for(i in seq_along(xweight)){
  
  new_data <- data.frame(temp = c(xweight[i], xweight[i+1]))
  y_hat <- predict(com, new_data, type = "response")
  delta_y <- exp(diff(y_hat))
  delta_ys <- c(delta_ys, delta_y)
  
}

df <- data.frame(temp = xweight, delta_rate = abs(delta_ys - 1))
plot(df$temp, df$delta_rate, type = "line")
abline(h = as.numeric(summary(df$delta_rate)[5]), lty = 2)

qt3 <- which(df$delta_rate >= summary(df$delta_rate)[5]) # 3rd quart summary(df$delta_rate)
df[qt3,]

# Assess model
simr <- simulateResiduals(com, n = 1000)
plot(simr)

# Methane - zero-inflated Gamma glm
ch4m <- glmmTMB(value ~ poly(temp,2), family = ziGamma(link = "log"), 
               ziformula = ~ poly(temp,2), data = gas_data[["CH4"]])
summary(ch4m)
exp(confint(ch4m))
cv::cv(ch4m, reps = 1, k = "loo")

# Plot model
ch4m_plot <- model_pred(gas_data[["CH4"]], "CH4", ch4m)

## Temperature(s) with greatest changes in oxidation rate
xweight <- seq(min(gas_data[["CH4"]]$temp), max(gas_data[["CH4"]]$temp), 1) 
delta_ys <- c()

for(i in seq_along(xweight)){
  
  new_data <- data.frame(temp = c(xweight[i], xweight[i+1]))
  y_hat <- predict(ch4m, new_data, type = "response")
  delta_y <- exp(diff(y_hat))
  delta_ys <- c(delta_ys, delta_y)
  
}

df <- data.frame(temp = xweight, delta_rate = abs(delta_ys - 1))
plot(df$temp, df$delta_rate, type = "line")
abline(h = as.numeric(summary(df$delta_rate)[5]), lty = 2)

qt3 <- which(df$delta_rate >= summary(df$delta_rate)[5]) # 3rd quart summary(df$delta_rate)
df[qt3,]

# Assess model
simr <- simulateResiduals(ch4m, n = 1000)
plot(simr)

## Panel prediction plot
ggpubr::ggarrange(h2m_plot, com_plot, ch4m_plot, nrow = 1, ncol = 3)

#-------------------------------------------------------------------------------
# Create spatial predictions
#-------------------------------------------------------------------------------
# Load Antarctic coastline shapefile
coast <- st_read(here(shared_path, "environmental", "coastline", 
                      "Coastline.shp"), quiet = T) %>%
  
  st_transform("ESRI:102020")

# Load ACBR shapefile
acbr <- st_read(here(shared_path, "biodiversity", "acbr", "ACBRs_v2_2016", 
                     "ACBRs_v2_2016.shp"), quiet = T) %>%
  
  st_transform("ESRI:102020")

## Current climate
# Read Antarctic temperature data
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

## Mask predictions to ACBR layer
gas_preds <- list.files(here(dirname(here()), "data"), pattern = "tif", full.names = T)
gas_preds_nm <- list.files(here(dirname(here()), "data"), pattern = "tif", full.names = F)
gas_preds_nm <- gsub(".tif", "_acbr.tif", gas_preds_nm)

for(i in seq_along(gas_preds)){
  
  r <- rast(gas_preds[i])
  r <- mask(r, acbr)
  writeRaster(r, here(dirname(here()), "data", gas_preds_nm[i]), overwrite = T)
  
}

#-------------------------------------------------------------------------------
# Making figures
#-------------------------------------------------------------------------------
## Maps - first make a map of the mean of the 12 months
h2_r <- rast(here(dirname(here()), "data", "H2_1981-2010_ant_acbr.tif"))
co_r <- rast(here(dirname(here()), "data", "CO_1981-2010_ant_acbr.tif"))

h2_map <- pred_maps(h2_r, comb = T, fun = median, gas = "H2", 
                    ret = T, sve = F, fname = "H2_1981-2010")
co_map <- pred_maps(co_r, comb = T, fun = median, gas = "CO", 
                    ret = T, sve = T, fname = "CO_1981-2010")

## ACBR boxplots
# Hydrogen
h2_r_mean <- app(h2_r, mean)
h2_r_med <- app(h2_r, median)

df <- data.frame(acbr = c(), rates = c(), long = c())
for(a in unique(acbr$ACBR_Name)){
  
  p <- acbr %>% dplyr::filter(ACBR_Name == a)
  x <- st_coordinates(st_centroid(st_union(p)))[1]
  m <- mask(h2_r_med, p)
  v <- values(m, na.rm = T)
  df2 <- data.frame(acbr = rep(a, length(v)),
                    rates = v,
                    long = rep(x, length(v)))
  df <- rbind(df, df2)
  
}

# Order by median rate
h2_box <- ggplot(df, aes(x = median, y = reorder(acbr, median))) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  xlab(expression(Rate ~ (nmol ~ H[2] ~ hr^{-1} ~ g^{-1}))) +
  ylab("ACBR") 

## Carbon monoxide
co_r_mean <- app(co_r, mean)
co_r_med <- app(co_r, median)

df <- data.frame(acbr = c(), rates = c(), long = c())
for(a in unique(acbr$ACBR_Name)){
  
  p <- acbr %>% dplyr::filter(ACBR_Name == a)
  x <- st_coordinates(st_centroid(st_union(p)))[1]
  m <- mask(co_r_med, p)
  v <- values(m, na.rm = T)
  df2 <- data.frame(acbr = rep(a, length(v)),
                    rates = v,
                    long = rep(x, length(v)))
  df <- rbind(df, df2)
  
}

# Order by median rate
co_box <- ggplot(df, aes(x = median, y = reorder(acbr, median))) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  xlab(expression(Rate ~ (nmol ~ CO ~ hr^{-1} ~ g^{-1}))) +
  ylab("ACBR") 

ggpubr::ggarrange(h2_map, co_map, 
                  h2_box, co_box, 
                  nrow = 2, ncol = 2,
                  common.legend = F)

################################################################################
### Note: making rectangles to represent inset maps
dml <- acbr %>% filter(ACBR_ID == 6)

df <- data.frame(xmin = as.numeric(sf::st_bbox(dml)[1]),
                 ymin = as.numeric(sf::st_bbox(dml)[2]),
                 xmax = as.numeric(sf::st_bbox(dml)[3]),
                 ymax = as.numeric(sf::st_bbox(dml)[4]))

## make inset
inset <- h2_map +
          coord_sf(
            xlim = sf::st_bbox(dml)[c(1,3)],
            ylim = sf::st_bbox(dml)[c(2,4)],
            expand = FALSE
          )

h2_map + 
  geom_rect(data = df, 
            mapping = aes(xmin = xmin ,ymin = ymin, 
                          xmax = xmax, ymax = ymax),
            fill = NA, 
            colour = "black",
            linewidth = 1
)
################################################################################
h2_preds <- list.files(here(dirname(here()), "data"), 
                       pattern = glob2rx("*H2*acbr*"), 
                       full.names = T)
h2_names <- list.files(here(dirname(here()), "data"), 
                       pattern = glob2rx("*H2*acbr*"), 
                       full.names = F)

co_preds <- list.files(here(dirname(here()), "data"), 
                       pattern = glob2rx("*CO*acbr*"), 
                       full.names = T)
co_names <- list.files(here(dirname(here()), "data"), 
                       pattern = glob2rx("*CO*acbr*"), 
                       full.names = F)

# for(h in seq_along(h2_preds)){
#   
#   fname <- gsub("_ant_acbr.tif", "", h2_names[h])
#   
#   r <- rast(h2_preds[h])
#   
#   pred_maps(r, comb = T, fun = mean, gas = "H2", sve = T, fname = fname, ret = F)
#   
# }
# 
# for(h in seq_along(co_preds)){
#   
#   fname <- gsub("_ant_acbr.tif", "", co_names[h])
#   
#   r <- rast(co_preds[h])
#   
#   pred_maps(r, comb = T, fun = mean, gas = "CO", sve = T, fname = fname, ret = F)
#   
# }

## Boxplots - monthly 
h2 <- fut_rate_boxes("H2", 
               time_periods = time_periods[2], #2011-2040
               pred_path = here(dirname(here()), "data"),
               to_file = F,
               out_path = here(dirname(here()), "figures"),
               img_type = "png")

co <- fut_rate_boxes("CO", 
               time_periods = time_periods[2], #2011-2040
               pred_path = here(dirname(here()), "data"),
               to_file = F,
               out_path = here(dirname(here()), "figures"),
               img_type = "png")

## Boxplots - Summer vs Winter
seasons <- c("Summer", "Winter")

h2_season <- fut_rate_season_boxes("H2",
                      time_periods = time_periods[3:4],
                      season = seasons,
                      pred_path = here(dirname(here()), "data"),
                      to_file = F)

co_season <- fut_rate_season_boxes("CO",
                                   time_periods = time_periods[3:4],
                                   season = seasons,
                                   pred_path = here(dirname(here()), "data"),
                                   to_file = F)

