################################################################################
## Script name: functions.R
## Author: David Clarke
## Copyright (c) David Clarke, 2025
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Prepare global climate data for Antarctic----
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

## Combine climate models for each time period----
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

## Gas model prediction and figure----
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

## Gas rate spatial prediction----
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

## Future gas rate spatial prediction----
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

## Make maps of gas predictions----
pred_maps <- function(pred_ras, comb = T, fun = mean, gas, sve = F, fname = NULL, ret = F){
  
  if(comb == T){
    
  r <- app(pred_ras, fun)
  
  g <- ggplot() +
    geom_sf(data = coast, aes(fill = surface), show.legend = F) +
    scale_fill_manual(values = c("#f4f3ef", "grey", "grey", "grey"),
                      breaks = c("land", "ice shelf", "ice tongue", "rumple")) +
    ggnewscale::new_scale("fill") +
    geom_spatraster(data =  r) +
    theme_bw() +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 
                             "inches"),
          legend.position = "top",
          legend.key.width = unit(2, 'cm'),
          legend.text = element_text(size = 12))
  
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
        geom_sf(data = coast, aes(fill = surface), show.legend = F) +
        scale_fill_manual(values = c("#f4f3ef", "grey", "grey", "grey"),
                          breaks = c("land", "ice shelf", "ice tongue", "rumple")) +
        ggnewscale::new_scale("fill") +
        geom_spatraster(data =  r[[i]]) +
        theme_bw() +
        theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 
                                 "inches"),
              legend.position = "top",
              legend.key.width = unit(2, 'cm'),
              legend.text = element_text(size = 12))
      
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

## Boxplots containing future climate gas predictions----
fut_rate_boxes <- function(gas, time_periods, pred_path, to_file = F, out_path = NULL, img_type = "pdf"){
  
  preds <- list.files(pred_path, pattern = glob2rx(paste0("*",gas,"*acbr*")), full.names = T)
  
  fig_list <- list()
  
  for(t in time_periods){
    
    ind <- which(time_periods == t)
    
    g_t <- preds[str_detect(preds, t)]
    
    vals <- c()
    mnths <- c()
    scen <- c()
    
    for(h in seq_along(g_t)){
      
      r <- rast(g_t[h])
      
      for(i in 1:nlyr(r)){
        
        vals <- c(vals, values(r[[i]], na.rm = T))
        mnths <- c(mnths, rep(month.name[i], length(values(r[[i]], na.rm = T))))
        scen <- c(scen, rep(scenarios[h], length(values(r[[i]], na.rm = T))))
        
      }
    }
    
    df <- data.frame(vals = vals, 
                     mnths = factor(mnths, levels = month.name[1:12]),
                     scenario = factor(scen, levels = scenarios))
    if(gas == "H2"){
      
      gas <- gsub("2", "[2]", gas)
      
    }
    
    g <- str2lang(gas)
    
    fig_list[[ind]] <- ggplot(df, aes(x = mnths, y = vals, fill = scenario)) +
      geom_boxplot() +
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.position = "top") +
      xlab("Month") +
      ylab(bquote(Rate ~ (nmol ~ .(g) ~ hr^{-1} ~ g^{-1}))) +
      scale_fill_manual(values = c("#d35199ff", "#52c2e8ff", "#f4e01dff"),
                        name = "Emission\nscenario") +
      ggtitle(t)
  }
  
  gg <- ggpubr::ggarrange(fig_list[[1]], fig_list[[2]], fig_list[[3]], 
                          nrow = 3, ncol = 1, common.legend = T)
  
  if(to_file == T){
    
    if(gas == "H2"){
      
      gas <- gsub("[2]", "2", gas, fixed = T)
      
    }
    
    ggsave(here(out_path, paste0(gas,"_boxes.",img_type)), gg, device = img_type, 
           units = "cm", width = 25, height = 20)
    
  }
  
  return(gg)
  
}
