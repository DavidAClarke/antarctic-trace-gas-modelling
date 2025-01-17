################################################################################
## Script name: functions.R
## Author: David Clarke
## Copyright (c) David Clarke, 2025
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Prepare global climate data for Antarctic
ant_prepare <- function(time_period, models=NULL, scenarios, my_ext, data_path){
  
  if(time_period == "1981-2010"){
    
    wc_files <- list.files(here(data_path, time_period), full.names = T)
    wc_names <- list.files(here(data_path, time_period), full.names = F)
    
    r <- rast(wc_files)
    rc <- crop(r, my_ext)
    rp <- project(rc, y = "ESRI:102020", threads = 12, res = 1000)
    
    writeRaster(rp, here(data_path, time_period, 
                         paste0("CHELSA_tas_",time_period,"_ant.tif")), 
                overwrite = T)
    
  }
  
  if(time_period != "1981-2010"){
    
    for(m in models){
      for(s in scenarios){
        
        ## To test...
        # r_name <- here(data_path, time_period, m, s,
        #                paste0("CHELSA_tas_",time_period,"_",m,"_",s,"_ant.tif"))
        # 
        # if(file.exists(r_name)){next}
        
        wc_files <- list.files(here(data_path, time_period, m, s), full.names = T)
        wc_names <- list.files(here(data_path, time_period, m, s), full.names = F)
        
        print(paste("working on",m,"&",s,"from",time_period))
        r <- rast(wc_files)
        rc <- crop(r, my_ext)
        rp <- project(rc, y = "ESRI:102020", threads = 12, res = 1000)
        
        print("Writing raster to disk")
        writeRaster(rp, here(data_path, time_period, m, s,
                             paste0("CHELSA_tas_",time_period,"_",m,"_",s,"_ant.tif")), 
                    overwrite = T)
        
      }
    }
  }
}

## Combine climate models for each time period
clim_combine <- function(time_period, models, scenarios, data_path){
  
  for(t in time_period){
  
    for(s in scenarios){
      
      rt <- list()
      
      for(m in models){
        
        ind <- which(models == m)
      
        r <- rast(here(data_path, time_period, m, s,
                     paste0("CHELSA_tas_",time_period,"_",m,"_",s,"_ant.tif")))
  
        rt[[ind]] <- r
      }
      
      rtm <- mean(rt[[1]], rt[[2]], rt[[3]], rt[[4]], rt[[5]])
      writeRaster(rtm, here(data_path, time_period, 
                            paste0("CHELSA_mean_tas_",time_period,"_",s,"_ant.tif")))
    }
  }
}

## Gas model prediction and figure
model_pred <- function(dat, gas, model){
  
  xweight <- seq(min(dat$temp), max(dat$temp), 1) 
  yweight <- predict(model, list(temp = xweight),type="response")
  
  g <- ggplot(dat, aes(x = temp, y = value, col = region)) +
    geom_point(col = "black") +
    geom_line(data = data.frame(temp = xweight, value = yweight), linewidth = 1, col = "black") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    xlab(expression(Temperature ~ ("°C")))
  
  if(gas == "H2"){
    g <- g + ylab(expression(Rate ~ (nmol ~ H[2] ~ hr^{-1} ~ g^{-1})))
  }
  
  if(gas == "CO"){
    g <- g + ylab(expression(Rate ~ (nmol ~ CO ~ hr^{-1} ~ g^{-1})))
  }
  
  if(gas == "CH4"){
    g <- g + ylab(expression(Rate ~ (nmol ~ CH[4] ~ hr^{-1} ~ g^{-1})))
  }
  
  return(g)
}

## Gas rate spatial prediction
spat_pred <- function(model, gas, clim_ras, msk, time_period, ld = F, 
                      pth, fname, ovr = F){
  
  r <- rast()
  
  for(i in 1:nlyr(clim_ras)){
    
    print(paste("Working on raster layer ", i))
    
    r1 <- clim_ras[[i]]
    names(r1) <- "temp"
    r1 <- mask(r1, msk)
    
    clim_df <- terra::as.data.frame(r1, xy = TRUE)
    clim_df$rates <- predict(model, newdata = clim_df, type = "response")
    r2 <- terra::rast(clim_df[,c('x','y','rates')], type = 'xyz', crs = crs("ESRI:102020"))
    
    names(r2) <- paste0(gas,"_",i)
    r <- c(r,r2)
    
  }
  
  writeRaster(r, here(pth, fname), overwrite = ovr)
  
  if(ld == T){
    
    r <- rast(here(pth, fname))
    return(r) 
    
  }
  
}

## Future gas rate spatial prediction
fut_spat_pred <- function(time_periods, gas, data_path, model, ovr = F){
  
  for(t in time_periods){
    
    t_files <- list.files(here(data_path, t), pattern = "tif", full.names = T)
    t_names <- list.files(here(data_path, t), pattern = "tif", full.names = F)
    
    for(i in seq_along(t_files)){
      
      r <- rast(t_files[i])
      n <- gsub("CHELSA_mean_tas", gas, t_names[i])
      
      spat_pred(model = model, gas = gas, clim_ras = r, 
                msk = coast, time_period = t, ld = F, 
                pth = here(dirname(here()), "data"),
                fname = n)
      
    }
   }
}

## Make maps of gas predictions
pred_maps <- function(pred_ras, comb = T, fun = mean, gas, sve = F, fname = NULL, ret = F){
  
  if(comb == T){
    
  r <- app(pred_ras, fun)
  
  g <- ggplot() +
    geom_sf(data = coast, fill = "#36454F") +
    geom_spatraster(data =  r) +
    theme_bw() +
    theme(plot.margin = unit(c(0, 0, 0, 0), 
                             "inches"))
  
  if(gas == "H2"){
    g <- g + scale_fill_whitebox_c(palette = "muted", 
                                   name = expression(nmol ~ H[2] ~ hr^{-1} ~ g^{-1}),
                                   labels = scales::label_number())
  } else
    
    if(gas == "CO"){
      g <- g + scale_fill_whitebox_c(palette = "muted", 
                                     name = expression(nmol ~ CO ~ hr^{-1} ~ g^{-1}),
                                     labels = scales::label_number())
    }
  
  if(sve == T){
    ggsave(here(dirname(here()), "figures", paste0(fname, ".svg")), g, device = "svg", 
           units = "cm", width = 15, height = 10)
   }
  }
  
  if(comb == F){
    
    for(i in 1:nlyr(r)){
      
      g <- ggplot() +
        geom_sf(data = coast, fill = "#36454F") +
        geom_spatraster(data =  r[[i]]) +
        theme_bw() +
        theme(plot.margin = unit(c(0, 0, 0, 0), 
                                 "inches"))
      
      if(gas == "H2"){
        g <- g + scale_fill_whitebox_c(palette = "muted", 
                                       name = expression(nmol ~ H[2] ~ hr^{-1} ~ g^{-1}),
                                       labels = scales::label_number())
      } else
        
        if(gas == "CO"){
          g <- g + scale_fill_whitebox_c(palette = "muted", 
                                         name = expression(nmol ~ CO ~ hr^{-1} ~ g^{-1}),
                                         labels = scales::label_number())
        }
      
      if(sve == T){
        ggsave(here(dirname(here()), "figures", paste0(fname,"_",i,".svg")), g, device = "svg", 
               units = "cm", width = 15, height = 10)
      }
    }
  }
  
  if(ret == T){
    return(g)
  }
  
  
}
