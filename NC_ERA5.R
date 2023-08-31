##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/ERA5_meteorology_nc")
getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/ERA5_meteorology_nc"

##packages in need
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting


nc_data <- nc_open('adaptor.mars.internal-1691085791.607091-17634-11-97441f24-485d-4f34-ad8a-f878412189c9.nc')
head(nc_data$var)
nc_data$var


file = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/CSN_CMD_txt_noCsub_noExtreme/Cluster_5/CSN_noCsub_noExtreme_C_5_PMF_CMD.csv")

letters_to_assign <- c("a", "b", "c", "d", "e", "f")
file$series = sample(letters_to_assign, nrow(file), replace = TRUE)



ggplot(file, aes(x = conc_OC, y = conc_EC, color = conc_Zn)) +
  geom_point() + 
  scale_color_gradient(name = "legend.whatever",
                      low = 'white', high = 'red')
  


ggplot( exp_inverse_dist_sum,           
        aes( geometry = geometry,                
             fill = normalize)) +   
  # geom_sf( data = counties.sf, color = 'black', fill = 'white') +   
  geom_sf( color = NA, ) +   
  scale_fill_gradient( low = 'white', high = 'red',                        
                       limits = c( -1, 10),                       
                       breaks = c( -1, 5, 10),                        
                       labels = c( '-1', '5', '10'),                        
                       oob = scales::squish) +   
  theme(plot.title = element_text(size = 20, face = "bold"),         
        rect = element_blank(),         
        axis.text.x = element_blank(),         
        axis.text.y = element_blank(),         
        axis.ticks = element_blank(),         
        legend.position = 'bottom',        
        strip.text = element_text( size = 20))+   
  facet_wrap( . ~ FUEL)


