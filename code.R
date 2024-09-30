## Next Steps
## Author: Carla Canovi (The New School)
## Date: Sep 26, 2024
pacman::p_load(MASS, readxl, tseries, car, tile, simcf, # data refinement
               survival, spaMM, plm, coda, fields, # data analysis
               sf, sp, tmap, raster, spdep, rgdal, rgeos,
               ggplot2, ggpubr, shiny, shinyjs, tmaptools, graph4lg, # spatial 
               tidyverse)
year <- c(rep(2010,3142), rep(2011,3142), rep(2012,3142),
          rep(2013,3142), rep(2014,3142), rep(2015,3142),
          rep(2016,3142), rep(2017,3142), rep(2018,3142),
          rep(2019,3142), rep(2020,3142), rep(2021,3142),
          rep(2022,3142))
data <- read_excel("county_list.xlsx")

year <- c(rep(2010,3142), rep(2011,3142), rep(2012,3142),
          rep(2013,3142), rep(2014,3142), rep(2015,3142),
          rep(2016,3142), rep(2017,3142), rep(2018,3142),
          rep(2019,3142), rep(2020,3142), rep(2021,3142),
          rep(2022,3142), rep(2023,3142), rep(2024, 3142))

fulldata <- rbind(data,data,data,data,data,
                  data,data,data,data,data,
                  data,data,data,data,data)

fulldata$year <- year
