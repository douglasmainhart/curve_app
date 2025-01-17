#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
### Links
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/
# https://shiny.rstudio.com/articles/reactivity-overview.html
# https://www.rdocumentation.org/packages/shiny/versions/1.7.4

rm(list = ls())

# install.packages('shinycssloaders')
# install.packages('rsconnect')
# install.packages("glue")
# install.packages("digest")
# install.packages("rlang")
# install.packages("cli")
# install.packages("stringi")
# install.packages("Rcpp")
# install.packages("curl")
# install.packages("fastmap")
# install.packages("cachem")

###install the moistureProfile package from github
# install_github("douglasmainhart/moistureProfile", force = TRUE)

################### Loading required packages ######################
#### these packages are for 
library(shiny)
library(devtools)
library(shinycssloaders)
library(httr)
library(moistureProfile)
library(readxl)
library(tidyr)
library(dplyr)
library(soilDB)
library(apsimx)
library(stringr)
library(soiltexture)
library(lubridate)
library(ggplot2)
library(dataRetrieval)
library(sf)
library(spData)
library(zoo)
library(EnvStats)
library(solrad)

library(psych)

######set working directory (just needed for pulling in example PRISM)
##working directory for Dougs Laptop
# setwd("C:/Users/dougl/OneDrive - archewild.com/Archewild research/Shared Documents/Projects/Soil Water Light/moisture_profile_code_8-25-2023/")

##working directory for Mark's gaming laptop
# setwd("C:/Users/markw/OneDrive - archewild.com/Documents - ArcheWild Research/Projects/Soil Water Light/moisture_profile_code_8-25-2023")


########################## Obtaining local well data to get idea of water table flux ###############################
###the ability to get the well data is from the dataRetrieval package
###function that converts lat and long given to state so we can query to get the state list of ground water observation sites
## https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html#whatnwisdata
### this function was taken from this page: https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
###updated 5/1/2024: changed finding state name to look for nearest polygon instead of intersect due to issues with points being out of bounds
latlon_to_state <- function(lat, long, states = spData::us_states, name_col = "NAME") {
  
  # ##testing
  # lat <- lat
  # long <- long
  # states <- spData::us_states
  # name_col = "NAME"
  
  ###convert lat,long into a table that can be made a shapefile object
  pointsDF <- data.frame(longitude = long, latitude = lat)
  
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  
  nearest.points <- st_nearest_feature(pts,states)
  
  ####### Find names of state intersected by each point
  ##get the list of statenames to index from
  state_names <- states[[name_col]]
  
  ####figure out which state the points object is closest too with (these return the index)
  # ii <- as.integer(st_intersects(pts, states)) ##gives intersection (if within, however points can be outside entire shapefile for some reason)
  nearest.feature <- st_nearest_feature(pts,states) ##finds nearest state polygon
  
  state_names[nearest.feature]
  
}

##for testing
# state.select <- latlon_to_state(lat = lat, long = long)

###function that takes the state, gets list of sites in the state and then returns the site closest to the gps point given and retrieves the median data
wt_data_retrieve <- function(state, lat, long){
  
  #testing values
  #state = state.select
  
  ###match up the state name with the right state code using the state code table
  state.info <- subset(dataRetrieval::stateCd, STATE_NAME == state )
  
  ###feed the state into the USGS function to get list of sites available
  ###get absolute differences between the coordinates and then use pathagorean theorem to get absolute distance
  #setting site status to active gives us only the currently active wells
  state_well_sites <- whatNWISsites(stateCd = state.info$STUSAB[1], parameterCd = "72019", siteStatus = "active")%>%
    mutate(abs_latdiff = abs(lat - dec_lat_va),
           abs_longdiff = abs(long - dec_long_va),
           distance_deg = sqrt(abs_longdiff^2 + abs_latdiff^2))%>%
    arrange(distance_deg)
  
  ####feed the  site numbers to the whatNWISdata function
  avail.data <- whatNWISdata(siteNumber = state_well_sites$site_no,
                             service = "dv",  parametercd = "72019", statCd = "00003")
  
  ####now join based on the site number to the state_well_sites and subset based on the one with the lowest distance
  close.avail <- left_join(avail.data, state_well_sites, by = "site_no")%>%
    subset(distance_deg == min(distance_deg))
  
  
  return(close.avail)
  
}

##########for testing
# target.site <- wt_data_retrieve(state = state.select, lat = lat, long = long)

##use this to take a peak at a well site to see what data is retrieved
# test.well.data <- readNWISdv(siteNumbers = 393959079555901, parameterCd = "72019", startDate = "2009-10-16", endDate = "2023-11-27")
# 
# plot(test.well.data$X_72019_00003)

#pa_sites <- whatNWISsites(stateCd = "PA", parameterCd = "30210") ##parameter code for Depth to water level, below land surface datum (LSD), meters
# parameters <- parameterCdFile ##gives all the possible parameters that can be quieried for we care about "72019" depth below surface in feet

##testing to se sites in PA that are active
# find.sites <- readNWISdata(stateCd = "pa",
#              siteStatus = "active",
#              service="gwlevels")


###testing to see what parameters are available in our location of interest and making sure start and end dates are within our window we care about
# whatNWISdata(siteNumber = 385849079563901, parametercd = "72019") ###72019 is code for feet below (often don't have meters)


###test download raw data for our time window 01-01-2000 to 01-01-2020 (will need to match the prism data table input)
#well.data <- readNWISdv(siteNumbers = 402643075150501, parameterCd = "72019", startDate = "2000-01-01", endDate = "2020-01-01")

#####function that takes soil.profile and prism and adds in the well data if minimum is present in the soil.profile
##updated 5/23/2024 (properly accounts for modifications both deeper and shallower, also if depth is much deeper than profile wtdepann set to NA)
well.match <- function(latitude, longitude, gen.profile, prism.table){
  
  ###testing
  # latitude <- lat
  # longitude = long
  # gen.profile <- profile.fit
  # prism.table = prism.cleaned
  
  ###testing different water table levels
  # gen.profile$wtdepannmin <- 50
  
  ####if wtdepannmin variable in soil.profile table is NA than forego adding in water table variation
  if (is.na(gen.profile$wtdepannmin[1])| gen.profile$wtdepannmin[1] > max(gen.profile$hzdepb_r) ) {
    
    ###if NA make the watertable column in the prism document NA 
    prism.wt <- prism.table %>%
      mutate(water_tbl_depth_cm = NA)
    
  } else { ####if present than retrieve the well data convert it to centimeter depth and then join to the prism table
    
    #### this is for if well data has data covering entire 20 year period, but often is not the case so we won't be using this method
    #prism.start <- as.character("1990-01-01") ##just to see
    # prism.start <- as.character(prism.table$date[1])
    # prism.end <- as.character(prism.table$date[length(prism.table$date)])
    
    
    
    ###first get the state the location of interest is in
    state.select <- latlon_to_state(lat = latitude, long = longitude)
    
    ###feed state select into wt_data_retrieval function to get the closest active well site to the gps points
    target.site <- wt_data_retrieve(state = state.select, lat = latitude, long = longitude)
    
    ###now that we have the target site, retrieve the data from entire recorded period of the recorded data for the site
    ##use na.approx() on raw values to fill in where needed if na values are present in the data
    well.data <- readNWISdv(siteNumbers = target.site$site_no[1], parameterCd = "72019", startDate = target.site$begin_date[1], endDate = target.site$end_date[1])%>%
      mutate(wt_depth_cm = na.approx(X_72019_00003 * 30.48),
             doy = yday(Date))
    
    ####duplicate, and get the minimums for each year (this is for if we had data for the entire 20 years, however this is not accurate)
    # well.data.mins <- well.data%>%
    #   mutate(year = substr(Date, 0,4))%>%
    #   group_by(year)%>%
    #   summarize(yearly_mincm = min(wt_depth_cm))%>%
    #   mutate(year_num = as.numeric(year))%>%
    #   subset(year_num == min(year_num))
    
    ####average the depths across each DOY values, the average curve is what will be used to simulate the variation in the water table over the year
    annual.avg.depth <- well.data %>%
      mutate(doy_char = as.character(doy))%>%
      group_by(doy_char)%>%
      summarize(avg_wtdep_cm = mean(wt_depth_cm))%>%
      mutate(doy_num = as.numeric(doy_char), mod_abs_difference = abs(gen.profile$wtdepannmin[1] - min(avg_wtdep_cm)), 
             water_tbl_depth_cm = avg_wtdep_cm + (gen.profile$wtdepannmin[1] - min(avg_wtdep_cm)))%>%
      arrange(doy_num)
    
    ###plotting to test how it looks and make sure minimum properly adjusted
    # plot(annual.avg.depth$water_tbl_depth_cm)
    # min(annual.avg.depth$water_tbl_depth_cm)
    
    
    ##testing plot
    #plot(well.data$wt_depth_cm)
    
    ###we now have the beginning minimum which is ideally close to the SSURGO survey time (usually around 1997)
    #on the depth given. If wtdepannmin is given as zero than the min is still zero, where as if its a deeper soil value it could go above
    ##now mutate the depth to scale to the original yearly minimum
    #this will sometimes produce negatives if the depth in some years is shallower than the original minimum, if the original min is at soil surface (0)
    #then this negative is not applicable, but if the minimum is somewhere deeper in the soil layer than the negative means the water table might be reaching
    #shallow depths, this should be allowed to occur up until the soil surface
    # 
    # well.data2 <- well.data%>%
    #   mutate(wt_scaled_cm = wt_depth_cm - abs(gen.profile$wtdepannmin[1]- well.data.mins$yearly_mincm[1]))%>%
    #   mutate(wt_scaled_cm = ifelse(wt_scaled_cm >= 0, wt_scaled_cm, 0))%>%
    #   select(Date, wt_scaled_cm)%>%
    #   mutate(Date = as.character(Date))%>%
    #   rename("date" = "Date", "water_tbl_depth_cm" = "wt_scaled_cm")
    
    ##trim down the fluctuation table
    well.avg.select <- annual.avg.depth%>%
      select(doy_char, water_tbl_depth_cm)
    
    ###now need to match up daily records with each day of the prism data
    prism.wt <- prism.table %>%
      mutate(doy_char = as.character(doy))%>%
      left_join(well.avg.select, by = "doy_char")%>%
      mutate(water_tbl_depth_cm = na.approx(water_tbl_depth_cm)) ###this fills in any na values with linear interpolation
    
    
  }
  
  
  return(prism.wt)
}



###################

########################## loading in early functions needed for making tables #########################

####this function needs to come before reference tables since it is used in making them
#calculate m
m.function <- function(n){
  
  #test
  #n <- n.final
  
  ###simple function
  m <- 1-(1/n)
  
  return(round(m, digits = 2))
  
}



##transpose variables function for fixing the rainfall rate table
#adapted from co2_analysis_8-23.r
#response list is a vector of the column names needing to be transposed into long form
#for this use it will be the various year frequency columns
transpose_vars <- function(df,response_list){
  
  ##make empty dataframe
  df1 <- list()
  
  
  for (i in 1:length(response_list)) {
    
    print(response_list[i])
    #this part performs a selection that takes the columns we want to keep and makes the measurement column
    data.gas <- df[,c("duration_hrs", "duration_mins", response_list[i])]
    #creates column telling what var the measurement corresponds to 
    data.gas$year_freq <- response_list[i]
    names(data.gas)[3] <- "raindepth_in"
    df1 <- rbind(df1, data.gas)
    
    
  }
  return(df1)
}



########################## Data tables needed for functions (5/22/2024) ################################################
###these tables are manually entered so that no excel spreadsheets need to be accessed by the user to minimize difficulty

#################################bedrock VG parameters ##use these to match up bedrock if it is prominent in topsoil
#this will be used in topsoil function and 
bedrock.parameters <- data.frame(type = c("loose", "moderate1", "moderate2", "low1", "low2", "rather_low", 
                                          "moderate3", "low3", "rather_low2","very_low"),
                                 ksat_ms = c(3*10^-5, 1*10^-5, 3*10^-6, 9*10^-7, 3*10^-7, 8*10^-8, 1*10^-6, 3*10^-7, 4*10^-8,
                                             5*10^-9),
                                 theta_s = c(0.32, 0.2, 0.2, 0.12, 0.12,0.08, 0.35, 0.35,0.25, 0.25),
                                 theta_r = c(0.05, 0.03, 0.03, 0.02, 0.02, 0.01, 0.06, 0.06, 0.04, 0.04),
                                 alpha_kpa = c(2.316/2, 1.916/2, 1.559/2, 1.272/2, 1.058/2, 0.85/2, 1.295/2, 1.058/2, 0.759/2,
                                               0.541/2),
                                 n = c((1+4.64)/2, (1+4.096)/2, (1+3.626)/2, (1+3.253)/2, (1+2.975)/2, (1+2.702)/2, (1+3.282)/2, 
                                       (1+2.975)/2, (1+2.581)/2, (1+2.581)/2))%>%
  mutate(ksat_mmhr = ksat_ms*(3.6*10^6), classes_results = "Bedrock", alpha_m = alpha_kpa/9.804)%>%
  mutate(alpha_cm = alpha_m/100)%>%
  select(classes_results, type, ksat_mmhr, ksat_ms, theta_s, theta_r, alpha_kpa, alpha_cm, alpha_m, n)%>%
  mutate(m = m.function(n)) %>%
  subset(type != "low2")

###########################################vg parameters for rocks and organic soils
stone.org.parameters <- data.frame(classes_results = c("Stone","Stone","Stone","Stone","Stone","Org"),
                                   type = c("limestone", "dolostone", "coarse_sandstone", "fine_sandstone", "pumice", "organic_matter"),
                                   theta_s = c(0.043, 0.061, 0.35, 0.036, 0.55, 0.96 ),
                                   theta_r = c(0.0, 0.0, 0.017, 0.0, 0.0, 0.32),
                                   alpha_m = c(0.001, 0.005, 1.003, 0.084, 0.191, 0.08),
                                   n = c(1.682, 1.75, 2.798, 1.219, 1.39, 1.75))%>%
  mutate(alpha_cm = alpha_m/100, alpha_kpa = alpha_m*9.804, m = m.function(n), ksat_mmhr = NA, ksat_ms = NA)%>%
  select(classes_results, type, ksat_mmhr, ksat_ms, theta_s, theta_r, alpha_kpa, alpha_cm, alpha_m, n, m)

#################################NRCS classification VG parameters
nrcs.parameters <- data.frame(classes_results = c("clay", "c_loam", "loam", "l_sand", "sand", "sa_clay", "sa_c_loam", "s_loam", "silt", "si_clay", "si_c_loam", "si_loam"),
                              type = c("clay", "c_loam", "loam", "l_sand", "sand", "sa_clay", "sa_c_loam", "s_loam", "silt", "si_clay", "si_c_loam", "si_loam"),
                              theta_r = c(0.098,0.079,0.061,0.049,0.053,0.117,0.063,0.039,0.050,0.111,0.090,0.065),
                              theta_s = c(0.459,0.442, 0.399, 0.390, 0.375, 0.385, 0.384, 0.387,0.489, 0.481, 0.482, 0.439),
                              alpha_cm = c(0.0150, 0.0158, 0.0111, 0.0348, 0.0352, 0.0334, 0.0211, 0.0267, 0.0066, 0.0162, 0.0084, 0.0051),
                              n = c(1.25, 1.42, 1.47,1.75,3.18,1.21,1.33,1.45,1.68,1.32,1.52,1.66))%>%
  mutate(m = m.function(n), alpha_m = alpha_cm/100, ksat_mmhr = NA, ksat_ms = NA)%>%
  mutate(alpha_kpa = alpha_m*9.804)%>%
  select(classes_results, type, ksat_mmhr, ksat_ms,theta_s, theta_r, alpha_kpa, alpha_cm, alpha_m, n, m)

#####combine them together to make it easier to pull from later
vg.exceptions.table <- rbind(nrcs.parameters, stone.org.parameters, bedrock.parameters)

###create rock components table for binding to the soil.profile object
#so that we can factor in influence of rock components on the soil water content

rock.frag.params <- vg.exceptions.table%>%
  subset(type == "fine_sandstone")%>%
  select(theta_s, theta_r, alpha_kpa, alpha_cm, alpha_m, n, m)
#rename
names(rock.frag.params) <- c("frag_theta_s", "frag_theta_r", "frag_alpha_kpa","frag_alpha_cm","frag_alpha_m","frag_n", "frag_m")

##duplicate to 3 rows for soil profile (treat all rocks as the same)
rock.frag.params <- rbind(rock.frag.params, rock.frag.params[rep(1,2),])

#################################### Rainfall interception tables

##Gash parameters for formation classes (except herbaceous since its a different model)
gash.params <- data.frame(veg_form = c("evergreen forest", "deciduous forest", "mixed forest", "pine_plantation", "shrub", "herbaceous"),
                          canopyS_mm = c(1.97, 1.40, 1.58, 1.70, 0.58, 0.27),
                          pt_mm = c(0.01, 0.01, 0.01, 0.05, 0.01, 0.00),
                          trunkS_mm = c(0.13, 0.08, 0.10, 0.46, 0.55,0.00),
                          Pt_mm = c(9.29, 6.82, 8.20, 9.20, 10.55,0.00),
                          canopy_ht_m = c(15,15,15,15,3,1),
                          base_form = c("forest", "forest", "forest", "forest","shrub", "herbaceous"))

####surface resistance terms ###updated 5/28/2024 displacement height now varies with canopy cover
rs.match.terms <- data.frame(veg_type = c("forest", "forest", "forest", "forest","forest","forest","forest","forest","forest", "forest", "herbaceous", "shrub"),
                             canopy_max = c(100,90,80,70, 60,50,40,30,20,10, NA, NA),
                             canopy_min = c(91,81,71,61,51, 41, 31, 21,11,1, NA,NA),
                             peak_LAI = c(9,8.3,7.6,6.9,6.2,5.5,4.8,4.1,3.4,2.7, 4, 4.5),
                             d_coef = c(0.80,0.80,0.75,0.63,0.55,0.47,0.39,0.31,0.23,0.15,0.75,0.75),
                             rl = c(550,550,550,550,550,550,550,550,550,500,120,230))%>%
  mutate(transition_LAI = peak_LAI/2,
         winter_LAI = 0)



##P'G must be calculated from canopy cover, scaled Evap, and rainfall rate
###rainfall rate table need this to get rainfall rate given precip amount
storm.durations <- data.frame(duration_mins = c(5,10,15,30,60,120,180,360, 720, 1440),
                              depths_1_in = c(0.333, 0.525, 0.65, 0.878, 1.08, 1.31, 1.44, 1.83, 2.27, 2.66),
                              depths_2_in = c(0.397, 0.628,0.780, 1.06, 1.32, 1.58, 1.74, 2.2, 2.73, 3.2),
                              depths_5_in = c(0.471, 0.745, 0.929, 1.3, 1.65, 1.98, 2.17, 2.71, 3.39, 3.98),
                              depths_10_in = c(0.53, 0.833, 1.04, 1.48, 1.91, 2.32, 2.52, 3.16, 3.96, 4.66),
                              depths_25_in = c(0.606,0.948,1.19,1.72,2.26,2.82,3.06,3.84,4.86,5.73),
                              depths_50_in = c(0.668,1.04,1.30,1.91,2.56,3.26,3.54,4.44,5.67,6.70),
                              depths_100_in = c(0.733,1.14,1.42,2.12,2.88,3.76,4.09,5.15,6.61,7.83),
                              depths_200_in = c(0.804,1.24,1.55,2.35,3.24,4.33,4.72,5.97,7.71,9.16),
                              depths_500_in = c(0.906, 1.38,1.74,2.68,3.77,5.23,5.70,7.27,9.47,11.3),
                              depths_1000_in = c(0.994,1.51,1.89,2.95,4.23,6.04,6.59,8.45,11.1,13.3))%>%
  mutate(duration_hrs = duration_mins/60)%>%
  select(duration_hrs, duration_mins, depths_1_in, depths_2_in, depths_5_in, depths_10_in, depths_50_in, depths_100_in, depths_200_in, depths_500_in, depths_1000_in)


##transpose for proper subsetting
#make list of columns interested in
col.list <- names(storm.durations)[3:11]
#run function
storm.transpose1 <- transpose_vars(df = storm.durations, response_list = col.list)

###get numbers for the year frequencies
freq_split <- read.table(text = as.character(storm.transpose1$year_freq), sep = "_") %>%
  select(2)
colnames(freq_split) <- c("freq_num")
###cbind back with the transpose
storm.transpose <- cbind(storm.transpose1, freq_split)%>%
  mutate(raindepth_cm = (raindepth_in * 2.54))

########################## Hargreaves solar radiation and Evap for interception ##################################################

########Hargreaves Methods (solar radiation package was out of date and not functional)
#ideally we want to use penman monteith but lack of available wind and solar data restricts our ability to 
# use the Panman equation (could be possible to use API from solar database)
###out gives ET in mm/day
##inputs required: latitude, date

###funciton that retrieves radiation at top of atmosphere
hargreaves.rad <- function(doy,latitude){
  
  ##testing
  # latitude <- 43.0423
  # date_input <- "2023-01-15"
  # latitude = 43.0423
  # doy = 150
  
  #solar constant
  gsc <- 0.0820
  
  ####covert latitude degrees into radians
  lat_rads <- latitude * pi/180
  #print(lat_rads)
  
  ##covert to joulean day of year
  day_num <- doy
  #print(day_num)
  
  ###get solar declination
  decl <- 0.409*sin((2*pi/365)*day_num - 1.39)
  
  ##inverse relative distance from the sun
  dr <- 1 + 0.033*cos(2*pi/365 * day_num)
  
  ##sunset hour angle 
  sha <- acos((-tan(lat_rads)*tan(decl)))
  
  ###separate out the sides of the bracket for easier computing (also doesn't comput right if you don't)
  sin.sin <- sin(lat_rads)*sin(decl)
  
  cos.cos <- cos(lat_rads)*cos(decl)
  
  ###radiation in regular MJ/m^2 d
  radiation1 <- (24*60/pi)*gsc*dr*(sha*sin.sin+cos.cos*sin(sha))
  
  ###to get equivalence in mm/day multiply by the inverse of the latent heat of vaporization (0.408)
  radiation_mmday <- 0.408*radiation1
  
  return(round(radiation_mmday,digits = 2))
}



###evapotranspiration data for soil surface
#inputs: radiation from above function, tmean, tmin, tmax all in Celsius, canopy cover
#this is is mm/day
ground.harg.et <- function(radiation, t_mean, t_min, t_max, canopy) {
  
  ##testing
  
  ####modifying radiation
  #canopy cover reduction on incoming radiation
  radiation.fact <- (1-canopy)*radiation
  
  
  ###equation
  et <- 0.0023*radiation.fact*(t_mean +17.8)*sqrt(t_max - t_min)
  
  ###issue is that evaporation is predicted as negative in winter times
  if (et < 0) {
    et2 <- 0
  } else{
    et2 <-et
  }
  
  return(round(et2, digits = 5))
  ##result is reference/potential evapo-transpiration (mm/day)
  
}

##just average evap top of canopy (not scaled to canopy or anything)
harg.et <- function(radiation, tmean, tmin, tmax){
  
  ###equation
  et <- 0.0023*radiation*(t_mean +17.8)*sqrt(t_max - t_min)
  
  ###issue is that evaporation is predicted as negative in winter times
  if (et < 0) {
    et2 <- 0
  } else{
    et2 <-et
  }
  
  return(round(et2, digits = 5))
  
}

##canopy et needed for gash model, needs to be in mm/hr
gash.et <- function(radiation, t_mean, t_min, t_max, canopy) {
  
  ##testing
  
  ###equation ##divide by 24 to get hr rate
  et <- 0.0023*radiation*(t_mean +17.8)*sqrt(t_max - t_min)/24
  
  
  
  ##canopy cover is multiplied by the evap rate to get E scaled to the canopy (different from the other et calculations)
  scaled_e <- et*canopy
  
  ###remove negative evaporation
  if (scaled_e <= 0) {
    scaled_e <- 0.001
  } else{
    
    scaled_e <- scaled_e
  }
  
  return(round(scaled_e, digits = 5))
  ##result is reference/potential evapo-transpiration (mm/day)
  
}


############################# Penman-Monteith Evaporation functions ###############################

################ Calculating Direct and Diffuse Solar radiation and including slope and aspect (Solrad package)
###data needed, slope in degrees, aspect in cardinal direction, elevation (from prism file)
##need day of the year as well for the radiation calculations,
##need to tell if cloudy or not (transmissivity needs to multiply by the value if not)

####function for matching vegetation parameters up
##takes the int.param
veg.match <- function(int.params, rs.params = rs.match.terms){
  
  ###testing
  # int.params <- int.param.table
  # rs.params <- rs.match.terms
  
  #####if it is forest then then we have to further subset by the canopy cover to approximate the LAI
  
  if (int.params$base_form == "forest") {
    
    ###get the surface resistance stuff matched in (need to be able to work between herbaceous and forests)
    rs.match <- subset(rs.params, veg_type == int.params$base_form[1])%>%
      subset(canopy_max >= int.params$canopy_cover[1] & canopy_min <= int.params$canopy_cover[1])
    
    ###bind these tables together
    veg_parameters <- cbind(int.params, rs.match)
    
    
    
  } else { ###if not forest just match up to shrubland or herbaceous
    
    ###subset based soley on the veg_form
    rs.match <- subset(rs.params, veg_type == int.params$base_form[1])
    
    veg_parameters <- cbind(int.params, rs.match)
    
  }
  
  return(veg_parameters)
  
}


#######aspect table (with respect to south)
aspects.tab <- data.frame(cardinal = c('S','SW','W','NW','N','NE','E','SE'),
                          asp_deg = c(0,45,90,135,180,225,270,315))

###now accounts for canopy cover (6/3/2024)
net.radiation <- function(doy, elevation, slope_deg, aspect, transmission, lat, long, tmin, tmax, VPD, cc, veg_form){
  
  ####testing values
  # doy <-172
  # elevation <- elevation.m
  # slope_deg <- slope.deg
  # aspect <- "N"
  # transmission = 1
  # lat <- lat
  # long <- long
  # tmin <- -17.2
  # tmax <- -8.7
  # VPD <- 0.235
  # veg_form <- veg_parameters$base_form[1]
  # cc = 0.40
  
  
  ###set up aspect
  aspect.sub <- subset(aspects.tab, cardinal == aspect)
  
  asp.deg <- aspect.sub$asp_deg[1]
  
  ##################### Solar Radiation (shortwave Direct + indirect)
  
  ###need to set intervals for the day in question
  doy.int <- seq(doy, doy+1, 0.05)
  
  #####get the direct radiation 
  #slon is -75 becuase this is the longitude of the eastern timezone
  direct.short <- DirectRadiation(DOY = doy.int, Lat = lat, Lon = long, SLon = -75.00, DS = 0, Slope = slope_deg, Aspect = asp.deg, Elevation = elevation)
  
  ##fix NA values seems to still occur in some cases (I need to come up with my own direct and diffuse radiation equations it seems since this package sucks)
  direct.short[is.na(direct.short)] <- 0
  
  ##make a data table of the radiation and the timestamp within the day in question
  short.dir.table <- data.frame(doy_int = doy.int,Rs = direct.short)
  
  
  
  ###plot for testing purposes
  # plot(doy.int, direct.short)
  
  
  ###get the diffuse radiation
  diffuse.short <-  DiffuseRadiation(DOY = doy.int, Lat = lat, Lon= long, SLon=-75.00, DS=0, Elevation = elevation, Slope = slope_deg)
  
  ###remove the NaN values
  diffuse.short[is.na(diffuse.short)] <- 0
  
  ##make table for it
  short.diff.table <- data.frame(doy_int = doy.int, Rs = diffuse.short)
  
  ### cc influences radiation differently depending on vegetation type 
  if (veg_form == 'herbaceous' | veg_form == "shrub") {
    
    ##if shrub or herbaceous the canopy cover is assumed to be on the edge of the area of interest with peak mid-day values not obstructed
    #this only effects based on the longest day so the range within day of possible light needs to be obtained (intervals .2-.8 is considered full possible window for our sake)
    
    ###get amount of window (0.6) covered by canopy cover and divide by 2 to put on both sides of the day
    window.mod <- (cc*0.6)/2
    
    ###now modify if anything is below this value it is reduced by a flat rate of 70% (canopy on either side )
    #modify both the direct and diffuse tables
    short.dir.table <- short.dir.table%>%
      mutate(Rs_cc_adj = ifelse(doy_int <= (doy+0.2 + window.mod) | doy_int >= ((doy+1)-0.2 - window.mod), Rs*0.70, Rs))
    
    
    ###diffuse
    short.diff.table <- short.diff.table%>%
      mutate(Rs_cc_adj = ifelse(doy_int <= (doy+0.2 + window.mod) | doy_int >= ((doy+1)-0.2 - window.mod), Rs*0.70, Rs))
    
    
    ###sum and convert the direct and diffuse radiation now that it has been adjusted
    ###corrected transformation to give in MJ/m^2 d
    direct.conv <- sum((short.dir.table$Rs_cc_adj*4320)/1000000)
    
    diffuse.conv <-  sum((short.diff.table$Rs_cc_adj*4320)/1000000)
    
    ###total them
    total.conv <- transmission*(diffuse.conv+direct.conv)
    
    
  } else {
    #if the vegetation formation isn't shrub or herbaceous it is a forest of some kind and cc is applied across the totals for whole day
    
    
    ###corrected transformation to give in MJ/m^2 d
    direct.conv <- sum((direct.short*4320)/1000000) *(1-cc)
    
    ###corrected transformation to give in MJ/m^2 d
    diffuse.conv <- sum((diffuse.short*4320)/1000000)*(1-cc)
    
    ###sum the diffuse and direct shortwave and multiply by the transmission factor
    total.conv <- transmission*(diffuse.conv+direct.conv)
    
    
    
  }
  
  
  
  
  ####################### Longwave radiation
  # ##first estimate actual vapor pressure (old way before I realized the calculation is wrong)
  # ea <- 0.611* exp((17.27*tmin)/(tmin+237.3))
  
  ##### new attempt (calculate the actual vapor pressure, first need to calculate sat vap press.)
  sat_vap_min <-0.611* exp((17.27*tmin)/(tmin+237.3))
  sat_vap_max <- 0.611* exp((17.27*tmax)/(tmax+237.3))
  
  ##then average the two to get daily saturation vapor pressure
  es <- (sat_vap_max+sat_vap_min)/2
  
  ###to get ea solve for it since VPD = es - ea    taken from (FAO chapter 3 Humidity section)
  ea <- -(VPD - es)
  
  ####shortwave radiation open sky to actuall measured ratio
  rs.ro <- total.conv / (diffuse.conv +direct.conv)
  
  ###stephen boltzmann
  sigma <- 4.903*10^-9
  
  ###kelvin temps to the 4th
  tmink4 <- (tmin + 273.15)^4
  tmaxk4 <- (tmax + 273.15)^4
  
  ####Net ourgoing longwave radiation equation
  rnl <- ((sigma*tmaxk4 + sigma*tmink4)/2)*(0.34-(0.14*sqrt(ea)))*(1.35*rs.ro - 0.35)
  
  ##now calculate net radiation 
  Rn <- total.conv - rnl
  
  return(Rn)
  
  
}


########wind speed
##for now it needs to be set at 2 m/s
##another possible future optimization would be retrieving proper wind speed data
#NREL database?

# wind.ms <- 2

########aerodynamic resistance 
###updated 5/28/2024 displacement height now varies with canopy cover
ra <- function( veg_height, dcoeff, wind_speed = 2){
  
  ###example tests
  # veg_height <- .5 ###this example looking at a lawn
  # wind_speed <- 2
  
  ##height of wind speed measurements 
  z = veg_height +2 
  z0 = 0.10 *veg_height
  d = dcoeff*veg_height
  k = 0.41 ##von karmans constant
  
  #####plug in equation
  numerator <- log((z-d)/z0)^2
  
  denominator <- (k^2)*wind_speed
  
  
  #####full calculation
  ra <- numerator/denominator
  
  return(ra)
  
  
}


#########Psychrometric constant
##requires input elevation data
psychrom <- function(elev){
  
  ###testing (needs to be in meters)
  # elev <- 90
  
  ####set other variables
  L <- 2.45
  e <- 0.622
  cp <- 1.013*10^-3
  
  ###calculate amtospheric pressure (from elevation input)
  p <- 101.3*((293-0.0065*elev)/293)^5.26
  
  ####calculate psychrom
  psy <- (cp*p)/(e*L)
  
  return(psy)
}

####slope of saturated vapor pressure curve
##need temperature in celsius
delta.slope <- function(temp_c){
  
  ###testing
  # temp_c <- 25
  
  ###separate out numerator and denom
  delta <- (4098*(0.6108*exp((17.27*temp_c)/(temp_c +237.3))))/(temp_c + 237.3)^2
  
  
}


#####Density of the Air
##needs temperature in Kelvin, air pressure kpa
air.dense <- function(tempc, elev){
  
  
  ##testing
  # elev <- 90 ###in meters
  # tempc <- 25
  
  
  
  ###convert temperature to kelvin
  tempk <- tempc +273.15
  
  ###get air pressure from elevation (could be optimized in future with better data)
  ##need to convert from kPa of atmospheric pressure to Pa for this calculation
  p.pa <- (101.3*((293-0.0065*elev)/293)^5.26)*1000
  
  ###calculate air density
  dense.air <- p.pa/(287.058 * tempk)
  
  return(dense.air)
  
}



#####calculating surface resistance 
###requires LAI and rl parameters (match by veg formation and canopy cover)
## this requires some set up as the variables will range throughout the year
rs <- function(LAI, rl){
  
  #####testing
  # LAI <- 4.5
  # rl <- 550
  
  
  ###calculate rs
  rs <- rl/LAI
  
  return(rs)
}



####final penman-monteith funtion
###vpd needs to be in kpa (PRISM outputs it in hPa)
penman.et <- function(wind.ms = 2, Rn, ra, VPD, psychrom, delta, air_dense, rs){
  
  ##testing (day 1 from prism set)
  # wind.ms <- 2
  # Rn <- 1.623015
  # ra <- ra.calc ###big influence on the evaporation (accurately captures differences in forests)
  # VPD <- 7.83/10
  # psychrom <- psy.const
  # delta <- 0.04276243
  # air_dense <- 1.280332
  # rs <- 122 ##testing if in winter set to 0 see what happens, yes set to zero in winter or else will be super wet
  
  ####set constants
  L <- 2.45
  cp <- 1.013*10^-3
  
  
  
  ###numerator
  numer <- delta*(Rn)+ air_dense*cp*(VPD/ra)
  
  #denominator
  denom <- delta + psychrom*(1+ rs/ra)
  
  ###full equation et in mm/day
  et <- (1/L)*(numer/denom)
  
  
  
}



########################## Rainfall interception functions ################################
####### using modified GASH model
#important inputs: rainfall, canopy cover, formation class


########function that takes each days precip value and produces a rainfall duration from storm durations table
##duration goes into calculating the rainfall rate for interception and duration is used in infiltration 
###rainfall duration function
#rainfall depth in cm needed
rainfall_duration_hrs <- function(precip){
  
  #test precip
  #precip <- 0.88
  
  ###make conditional because if 0 precip rainfall rate will equal 0
  if (precip == 0) {
    
    ###make rate = 0
    rainfall.duration <- 0
    
    
    
  } else if (precip > 13.50) { ###if larger than the maximum value on the table storm duration in is 24 hours
    
    ###
    rainfall.duration <- 24
    
    
  }else  {
    
    ####take precip value and find which is closest in the dataframe
    
    ###doing the diff in the transposed table by adding a new column
    storm.transpose2 <- storm.transpose%>%
      mutate(abs_diff = abs(raindepth_cm - precip))
    
    ###group by each frequency year and get minimums
    freq_mins <- storm.transpose2%>%
      group_by(freq_num)%>%
      summarize(min_diff = min(abs_diff))%>%
      subset( min_diff < quantile(min_diff, prob = 0.33, na.rm = TRUE))%>% ##this pulls the bottom 33% of the min_diff values using quantile function
      subset(freq_num == min(freq_num))
    
    ###get the value of the storm duration by subsetting by the freq_mins values
    storm.subset <- storm.transpose2%>%
      subset(freq_num == freq_mins$freq_num & abs_diff == freq_mins$min_diff)
    
    #set the rainfall duration, divide by 60 to get the time in hours
    rainfall.duration <- storm.subset$duration_mins[1]/60
    
  }
  
  return(round(rainfall.duration, digits = 3))
  
  
}

###parameters mostly taken from above GASH table but some needs to be calculated

###Amount of precipt to fill canopy storage (P'G)
##will need to be calculated every day
# S = canopy storage in mm
# rain rate  in mm/hr
#Ec (scaled e) in mm/hr
PPG <- function (rain_rate, scaled_e, Sc, formation){
  
  ##testing
  # rain_rate =
  # scaled_e = 3.1
  # Sc = 128
  # formation = "evergreen forest"
  
  ###if precip is zero than rain rate will be zero and needs to be specified or else PG will be na
  
  ###separate out PPG calculations by the veg formation since canopy cover can be 0 for herbaceous meadows but the plant cover may remain high
  
  if (formation != "herbaceous") {
    
    ##conditional if no rain rainfall rate will equal zero which creates NA values,
    #put placeholder of 1 for ppg just so it doesn't create NA values down the road
    
    if (rain_rate <= 0) {
      ppg <- 0.001
    } else if (rain_rate > 0 ){
      
      # Ec.over.rr <- ifelse(scaled_e/rain_rate <=0, 0.1)
      
      ppg <- -(rain_rate/scaled_e)*Sc*log(1-(scaled_e/rain_rate))
      
    }
  } else if (formation == "herbaceous"){
    #make scaled e equal to .80 (this assumes high cover of herbaceous species) (not always the case)
    scaled_e <- .80
    if (rain_rate <= 0) {
      ppg <- 0.001
    } else if (rain_rate > 0 ){
      
      ppg <- -(rain_rate/scaled_e)*Sc*log(1-(scaled_e/rain_rate))
      
    }
    
    
    
  }
  
  return(round(ppg, digits = 3))
  
}

####break down the interception equations first into there respective parts
#remember herbaceous formation type will be done much differently 

###canopy interception equation
#PPG = P'G ("P Prime G") calculated daily
##precip = PG
##evap is daily predicted evapotranspiration
##rain_rate in mm/hr
##putting all these in function arguments allows for using in mutate() call on the final data frame we have
canopy_int <- function(precip, PPG, cc, rain_rate, scaled_evap, formation){
  
  #######if formation is forest we will use the typical GASH equation, but this isn't structured well for herbaceous/grasslands so 
  ###for those we will use a different approach
  if (formation != "herbaceous"){
    
    if (precip <= PPG) {
      ##if rainfall less than the PG threshold than interception is equal to the canopy times rainfall
      int_c <- cc*precip
      
      
      
    } else if (precip > PPG ){
      
      ###when precip larger than holding capacity than calculations need to be done
      ##need evap and rainfall rate for this one
      int_c <- (cc*scaled_evap/rain_rate)*(precip - PPG) + cc*PPG
      
      
    }
    
    
    
  }else if(formation == "herbaceous"){
    
    ###use straight up equation from Genxu et al. 2012
    #assume plant cover to be high (.80)
    int_perc <- ((15.05*.80 + 16.85)*precip^-0.77)/100 ###this equation gives the proportion of the rainfall that is intercepted per event
    
    ###get actual interception
    int_c <- precip*int_perc
    
    
  }
  
  
  return(int_c)
}

###trunk interception function
#pt = incident precip hitting trunk (mm)
#precip = rainfall depth (mm)
#PPT = P'T (P Prime T) precip to fill trunk storage
#St = trunk storage capacity (mm)
trunk_int <- function(pt, precip, PPT, St){
  
  ###if statement
  if (precip < PPT) {#if precip is less than trunk storage capacity
    
    #trunk int equal to pt times rainfall
    trunk.int <- pt*precip
    
    
    
  } else if (precip >= PPT){
    ###if higher precip than trunk storage is the interception amount
    trunk.int <- St 
    
  }
  
  return(trunk.int)
  
}


########################## Canopy Cover Seasonal Change Functions ###########################################

#logit transform function
logit <- function(ccinput){
  
  logit.trans <- log(ccinput/(1-ccinput))
  
  return(logit.trans)
  
}

##convert logit function
conv.logit <- function(logit.value){
  
  ##convert
  logit.convert <- (exp(logit.value))/(exp(logit.value)+1)
  
  return(logit.convert)
}

###functional that creates canopy cover table from inputs converting between leaf on and off
#ccinput is the canopy cover input user is putting in
#leaf_on is a boolean to ask what time of year this is taken during
#veg_form is the vegetation formation
cc.calculator <- function(ccinput, leaf_on, veg_form){ ###output should be a 3 column dataframe
  
  ####testing
  # ccinput <- cc
  # leaf_on <- TRUE
  # veg_form = "mixed deciduous forest"
  
  ##convert from whole number percentage to decimal
  ccinput.decimal <- ccinput/100
  
  if (veg_form == "deciduous forest" | veg_form == "shrub") { ##if true convert to leaf off period
    
    ##conditional to now calculate the leaf_on/off
    if (leaf_on == TRUE) { ###if leaf_on solve equation for the leaf off
      
      ###leaf on is the ccinput give
      leaf.on <- ccinput.decimal
      
      ##convert to logit
      logit.ccinput <- logit(ccinput.decimal)
      
      ###solve for leaf off
      logit.off <- (logit.ccinput - 1.0630 - 0.01271)/1.5831
      
      ##convert back to proportion
      leaf.off <- conv.logit(logit.off)
      
    } else { ##this is for when CCoff is given run throught the normal model equation
      
      #convert ccinput to logit
      logit.ccoff <- logit(ccinput.decimal)
      
      ##estimate ccon
      logit.ccon <- 1.0630 + 1.5831*(logit.ccoff) + 0.01271
      
      ##convert back to normal values
      leaf.on <- conv.logit(logit.ccon)
      
      leaf.off <- ccinput.decimal
      
    }
    
    
    ######################### for mixed deciduous  
  } else if (veg_form == "mixed forest"){ #assume half of the ccinput is deciduous
    
    ##conditional to now calculate the leaf_on/off
    if (leaf_on == TRUE) { ###if leaf_on is true than calculate leaf_off
      
      ###if leaf on is given assume half of the current canopy cover is deciduous and half is evergreen
      #total leaf on is still the input
      leaf.on <- ccinput.decimal
      
      ##half the changing canopy cover value
      decid.half <- ccinput.decimal/2
      
      #evergreen half
      evg.half <- ccinput.decimal/2
      
      ##convert to logit
      logit.ccinput <- logit(decid.half)
      
      ###solve for leaf off
      logit.off <- (logit.ccinput - 1.0630 - 0.01271)/1.5831
      
      ##convert back to proportion
      leaf.off.decid <- conv.logit(logit.off)
      
      ###add back to the evergreen half to get leaf off  since the evergreen woulnd't change in cover
      leaf.off <- leaf.off.decid+evg.half
      
    } else { ##this is for when CCoff is given
      ##for CCoff we will assume 66% of the CCoff is evergreeen and 34% is sticks of deciduous
      #full leaf off is still leaf off value to keep
      leaf.off <- ccinput.decimal
      
      #get evergreen part of leaf off
      evg.off <- 0.66*ccinput.decimal
      
      #get deciduous part of leaf off
      decid.off <- 0.34*ccinput.decimal
      
      #convert ccinput to logit
      logit.ccoff <- logit(decid.off)
      
      ##estimate ccon
      logit.ccon <- 1.0630 + 1.5831*(logit.ccoff) + 0.01271
      
      ##convert back to normal values
      leaf.on.dec <- conv.logit(logit.ccon)
      
      ##add leaf on dec to the evergreen leaf off to get full canopy cover
      leaf.on <- evg.off + leaf.on.dec
      
    }
    
    ############## For evergreen forest
  }else if (veg_form == "evergreen forest"){ ###no change in canopy cover
    
    ##both are the same
    leaf.on <- ccinput.decimal
    leaf.off <- ccinput.decimal
    
  } else if (veg_form == "herbaceous"){
    ##both are the same
    leaf.on <- ccinput.decimal
    leaf.off <- ccinput.decimal
    
    
  }
  
  
  ##now make the dataframe to keep data
  canopy.seasonality <- data.frame(veg_formation = c(veg_form),
                                   leaf_on_perc = c(as.numeric(round(leaf.on, digits = 2))),
                                   leaf_off_perc = c(as.numeric(round(leaf.off, digits = 2))))%>%
    mutate(leaf_change = (leaf_on_perc - leaf_off_perc)/30)
  
  return(canopy.seasonality)
  
}

##test
# cc.table <- cc.calculator(ccinput = 0.67, leaf_on = TRUE, veg_form = "deciduous forest")

##takes inputs:
#cc - canopy cover coverted to decimal 0-1

##doy = day of year (1-365)
##cc.on = leaf on canopy cover taken from constants table
#veg.form = vegetation formation, needed for understanding changes.
##days of year to start leaf out and senescence (April 1st 97 DOY) and October (274 DOY)
##these values should be pulled from an existing table when used in final program
##output should be the value of cc for that specific day of the year
cc.seasonal <- function(doy, cc.on, cc.off){
  
  ##testing
  #doy <- 130
  # #vegform : not needed because if difference is zero function shouldn't really make a distinction
  # cc.on <- cc.table$leaf_on_perc[1]
  # cc.off <- cc.table$leaf_off_perc[1]
  
  ###get differences between cc.on and cc.off and divide by thirty to get rate of change
  cc.rate <- (cc.on - cc.off)/30
  
  ####Test if the day is within the spring leafout period (97-127)
  if (doy > 97 & doy <= 127) {
    #### get difference between current day and beginning leaf out doy (97)
    day.diff <- doy - 97
    
    ##multiply day by the rate to get how much its change by
    day.change <- day.diff*cc.rate
    
    ##since this is leaf out duration add the change to the leaf off value to get current cc
    current.cc <- day.change + cc.off
    
  } else if (doy > 274 & doy <= 305){##get fall senesence period
    ###get date diff minus start day of 274
    day.diff <- doy-274
    
    #multiply day diff by rate to get the change
    day.change <- day.diff*cc.rate
    
    ##since this is leaf fall subract the day.change from the cc.on value
    current.cc <- cc.on - day.change
    
  } else if (doy > 127 & doy <= 274){ ###leaf on part of year
    
    ##leaf on value assigned
    current.cc <- cc.on
    
  } else if (doy <= 97 | doy > 305 ){
    ##leaf off value
    current.cc <- cc.off
    
  }
  
  
  
  #return the current.cc value
  return(round(current.cc, digits = 2))
  
  
  
  
}



###copy using apply function to take vector and return a vector, if function in mutate needs to return vector to be appended as the new column
cc.season.vec <- function(doy, cc.on, cc.off){
  
  ##testing
  #doy <- 130
  # #vegform : not needed because if difference is zero function shouldn't really make a distinction
  # cc.on <- cc.table$leaf_on_perc[1]
  # cc.off <- cc.table$leaf_off_perc[1]
  
  ###get differences between cc.on and cc.off and divide by thirty to get rate of change
  cc.rate <- (cc.on - cc.off)/30
  
  ####Test if the day is within the spring leafout period (97-127)
  if (doy > 97 & doy <= 127) {
    #### get difference between current day and beginning leaf out doy (97)
    day.diff <- doy - 97
    
    ##multiply day by the rate to get how much its change by
    day.change <- day.diff*cc.rate
    
    ##since this is leaf out duration add the change to the leaf off value to get current cc
    current.cc <- day.change + cc.off
    
  } else if (doy > 274 & doy <= 305){##get fall senesence period
    ###get date diff minus start day of 274
    day.diff <- doy-274
    
    #multiply day diff by rate to get the change
    day.change <- day.diff*cc.rate
    
    ##since this is leaf fall subract the day.change from the cc.on value
    current.cc <- cc.on - day.change
    
  } else if (doy > 127 & doy <= 274){ ###leaf on part of year
    
    ##leaf on value assigned
    current.cc <- cc.on
    
  } else if (doy <= 97 | doy > 305 ){
    ##leaf off value
    current.cc <- cc.off
    
  }
  
  
  
  #return the current.cc value
  return(round(current.cc, digits = 2))
  
  
  
}





########################## Infiltration and runoff functions ####################################
####this series of functions only matters if on a sloped surface where water will readily runoff once time to ponding is reached
### VG parameters are taken for the upper 50 cm soil profile and precipitation value is the one after the interception is factored in
#VG parameters are just for mineral portion since factoring in stones would be a little complicated here.

####### New infil and runoff functions
##calculate ponding time and infil, and runoff all within one function 
## updated 3-22-2024
## updated 3-22-2024
infiltration <- function (ws_infil_i, top_infil_ws, rain_duration, rain_rate, precip_mm, ksat_top_mmhr , slope_mod, soil_df, infil_depth = 5) {
  
  
  # ###### TESTING VALUES
  # ws_infil_i = wsi_infilcm
  # top_infil_ws = ws_topinfil_cm
  # rain_duration = filled.df$rain_dur_hrs[i]
  # rain_rate = filled.df$adj_prate_cmhr[i]
  # precip_mm = filled.df$adj_precip_mm[i]
  # ksat_top_mmhr = ksat_top
  # slope_mod = soil.df$inf_ang_mod[1]
  # soil_df = soil.df.input
  
  ##convert precip to cm and ksat to cm/hr
  precip_cm = precip_mm/10
  ksat_cmhr = ksat_top_mmhr*0.1
  
  ###get the actual infil layer storage max and minimums
  infil.max <- soil_df$adj_theta_s[1]*infil_depth
  infil.min <- soil_df$adj_theta_r[1]*infil_depth
  
  ##make sure values aren't funky
  ws_infil_i <- ifelse(ws_infil_i >infil.max, infil.max, ifelse(ws_infil_i < infil.min, infil.min, ws_infil_i))
  
  #####begin by getting the water deficit in the upper 10 cm
  wd_10 <- top_infil_ws - ws_infil_i
  
  
  
  ###if rainfall is less than this deficit than all rainfall infiltrates into the soil profile
  if (precip_cm > wd_10) {
    ##if precip greater than water deficit than infiltration needs to be limited and runoff will occur
    #calculate time to ponding
    t.pond <- wd_10/rain_rate
    
    ##pre-pond infiltration = just the water deficit
    infil_pp <- wd_10
    
    ###post ponding time
    tpop <- rain_duration - t.pond
    
    ####get the ammount ponded on the surface that may be liable to runoff, reduce the ksat by a fraction in order to account for hysterisis
    # ponded <- (rain_rate - (.75*ksat_cmhr))*tpop
    ##if the Ksat is much larger than the rainfall rate than the ponded amount will go to zero
    ponded <- ifelse(rain_rate - (.75*ksat_cmhr) < 0, 0, (rain_rate - (.75*ksat_cmhr))*tpop)
    
    #because ponded is the difference between Ksat and rain fall rate we need to get the actual amount infiled during postponding time
    infil_pop <- tpop*(.75*ksat_cmhr)
    
    #runoff is based on the slope, when the slope is higher than zero than runoff occurs, if zero (flat) than ponded water just sits on soil
    #eventually infiltrating
    runoff <- slope_mod*ponded 
    
    ##from ponded subtract the runoff and add to the preponded infil to get total infiltration of the rain event
    infil.tot <- (ponded - runoff)+infil_pp+infil_pop
    
    inf.run <- data.frame(infil_cm = c(infil.tot),
                          runoff_cm = c(runoff))
    
  } else if (precip_cm <= wd_10){
    
    ##if not then infiltration will be equal to rainfall with no runoff
    inf.run <- data.frame(infil_cm = c(precip_cm),
                          runoff_cm = c(0))
    
  }
  
  return(inf.run)
  
  
}

##run me daddy
########################## PRISM CLEANING FUNCTION ######################################
##temp conversion to celcius function
temp.convert <- function(temp_f){
  
  temp.c <- (temp_f - 32)* (5/9)
  
  return(round(temp.c, digits = 1))
}

####testing with Nick Anderson property PRISM excel sheet
#read in test data
# prism.raw <- read.csv("./Anderson_prism_2000-2020.csv", skip = 10) ##skip past all the metadeta junk

###function needs to read in the PRISM and output one with better column names, doy column, convert to needed units of temperature and rainfall
#updated 6/6/2024 function is now pipe friendly to reduce load of excess tables
prism.cleaning <- function(prism.table){
  
  ##testing
  # prism.table <- prism.raw
  
  
  ##make sure to just select columns we need some prism sets have the mean dew point temperature (old non-pipable way)
  # prism.table2 <- prism.table%>%
  #   select(Date, ppt..inches. , tmin..degrees.F.   ,tmean..degrees.F. ,tmax..degrees.F.   ,vpdmin..hPa. ,vpdmax..hPa. )
  
  ##making it pipe friendly, use brackets instead of the piped select to get the columns we care about
  prism.table2 <- prism.table[,c('Date','ppt..inches.','tmin..degrees.F.','tmean..degrees.F.','tmax..degrees.F.','vpdmin..hPa.','vpdmax..hPa.')]
  
  ##rename
  names(prism.table2) <- c("date", "precip_in", "tmin_f", "tmean_f", "tmax_f", "vpdmin_hpa", "vpdmax_hpa")
  
  ###get DOY column with yday(function)
  prism.doy <- prism.table2 %>%
    mutate(doy = yday(date))%>%
    mutate(precip_cm = precip_in*2.54)%>%
    mutate(precip_mm = precip_cm*10)%>%
    mutate(tmin_c = temp.convert(tmin_f), tmean_c = temp.convert(tmean_f), tmax_c = temp.convert(tmax_f))
  
  ##return the prepped prism dataset
  return(prism.doy)
  
}

########################## Soil Property functions ########################

###van genucthen soil water balance equation for converting to water potential
##alpha in cm

# converts theta to water potential
vg.pot.conversion <- function(theta, theta_r, theta_s, alpha, n, m){
  
  
  ##s refers to the saturation ratio
  s = (theta_s - theta_r)/(theta - theta_r)
  
  ##raise s to 1/m
  s_to_m <- s^(1/m)
  
  ##subtract 1 and then divide by alpha
  fraction = (s_to_m - 1)/alpha
  
  ##take n root
  vg.water.potential = fraction^(1/n)
  
  
  return(-vg.water.potential)
  
}


####takes wate potential and provides corresponding theta values
wrc.theta <- function(h_cm, theta_r, theta_s, alpha, n, m){
  
  ###testing
  # theta_r = soil.df$theta_r[1]
  # theta_s = soil.df$theta_s[1]
  # alpha = soil.df$alpha_cm[1]
  # n= soil.df$n[1]
  # m = soil.df$m[1]
  
  #h_cm = -16000000
  
  ####equation
  theta.h <- theta_r + ((theta_s - theta_r)/((1+abs(alpha*h_cm)^n)^m))
  
  return(theta.h)
  
}

####ksat PTF functions removed fro now since they don't offer anything really

###equation to determine current K(theta)
##takes adjusted soil theta_r and theta_s and the parameters of the mineral soil
##takes ksat in mm/hr and converts it to cm and then spits it out to mm/hr again
##5/30/2024 added built in bounding feature to prevent NA values from occuring
k.function <- function(theta, adj_theta_r, adj_theta_s, alpha_cm, n, m, ksat){
  
  ##testing values pulled from the profile.fit table that is generated (theta is made up value)
  # theta <- 0.31
  # adj_theta_s <- profile.fit$adj_theta_s[1]
  # adj_theta_r <- profile.fit$adj_theta_r[1]
  # alpha_cm <- profile.fit$wt_alpha_cm[1]
  # n <- profile.fit$wt_n[1]
  # m <- profile.fit$wt_m[1]
  # ksat <- profile.fit$wt_ksat_mmhr[1]
  
  
  #####make sure theta values is correctly in bounds
  theta <- ifelse(theta> adj_theta_s, adj_theta_s, ifelse(theta < adj_theta_r, adj_theta_r, theta))
  
  
  #################### Not including fragment in the hydraulic conductance determination, just adjusting theta_s and theta r
  ##for now we will assume that the rocks in the soil do not conduct water and thus just reduce the area available for water to reside in the soil
  #this is not a perfect solution as it doesn't account for increased drainage from lacunar pore space, but for now its what we will work with
  # refer to adjusted theta r and s values to put into the both the K and water potential equations. 
  #the adjusted values take into account the space taken up by rock fragments
  #its okay if the parameters are not the same as what we calculated because its just to get a fraction of saturation for the vg equations
  
  
  ####convert Ksat from mm/hr to cm/min
  ksat_cmmin = ksat/600
  
  
  ## theta portion of the equation.
  k.theta.calc <- ((theta-adj_theta_r)/(adj_theta_s-adj_theta_r))^0.5
  
  #right side inner theta fractions
  k.right.theta <- ((theta-adj_theta_r)/(adj_theta_s-adj_theta_r))^(n/(n-1))
  
  ##right bracket
  k.brack <- (1-k.right.theta)^m
  
  #right brace value
  k.brace <- (1-k.brack)^2
  
  #now multiply by the left thetas and divide by the Ksat value (from PTF)
  k.final <- (k.theta.calc*k.brace)*ksat_cmmin
  
  ###k.final is in cm/mins we need to convert to mm/hr
  k.final2 <- k.final*600
  
  ###just to test and see output
  #k.final*600
  
  
  
  
  return(k.final2)
}

####### functions for van genuchten to be used on data from soil web

##saturated water content (Wosten et al. 1999)
theta.s <- function (silt, clay, om, bd, topsoil){
  
  ##testing
  #soil.table <- texture.assigned
  
  ##test for code, go off of the soil.test dataframe
  #manual says this is based on percentage whole numbers not decimals
  # bd <- soil.table$dbovendry_r[1]
  # sand <- soil.table$sandtotal_r[1]
  # clay <- soil.table$claytotal_r[1]
  # silt <- soil.table$silttotal_r[1]
  # om <- soil.table$om_r[1]
  # 
  # ##need a topsoil column to be added, for not just a placeholder
  # topsoil <- soil.table$topsoil_bin[1]
  0
  
  #run the equation
  theta_s <- 0.7919 +0.001691*clay-0.29619*bd-0.000001491*(silt^2)+0.0000821*(om^2)+
    (0.02427/clay)+(0.01113/silt)+(0.01472*(log(silt)))-0.0000733*om*clay -
    0.000619*bd*clay-0.001183*bd*om-0.0001664*topsoil*silt
  
  ###
  return(round(theta_s, digits = 3))
  
  
  
}

#testing for high organic
#theta.s(40.1,15.5,85,1.00,1)

####residual water content (Vereecken et al)
theta.r <- function(clay, om){
  
  ##for testing
  # om <- soil.table$om_r[1]
  # clay <- soil.table$claytotal_r[1]
  
  
  ##equation
  theta_r <- 0.015+0.005*clay+0.014*om
  
  return(round(theta_r, digits = 3))
  
}

#####alpha (wosten et al. 1999)
###outputs in cm^-1
alpha <- function(silt, clay, om, bd, topsoil){
  
  ##test for code, go off of the soil.test dataframe
  # #manual says this is based on percentage whole numbers not decimals
  # bd <- soil.table$dbovendry_r[7]
  # sand <- soil.table$sandtotal_r[7]
  # clay <- soil.table$claytotal_r[7]
  # silt <- soil.table$silttotal_r[7]
  # om <- soil.table$om_r[7]
  # 
  # # ##need a topsoil column to be added, for not just a placeholder
  # topsoil <- soil.table$topsoil_bin[7]
  
  ##if any value is zero the log component of the ptfs will not work
  
  #calculate the exponent
  alpha.e <- -14.96+0.03135*clay+0.0351*silt+0.646*om+15.29*bd-0.192*topsoil-
    4.671*(bd^2) -0.000781*(clay^2) -0.00687*(om^2)+(0.0449/om)+0.0663*(log(silt))+
    0.1482*(log(om))-0.04546*bd*silt-0.4852*bd*om+0.00673*topsoil*clay
  
  ##full equation
  alpha.a <- exp(alpha.e)
  
  return(round(alpha.a, digits = 4))
  
}

###n
n.function <- function(silt,clay,om, bd,topsoil){
  
  # ###testing
  # bd <- soil.table$dbovendry_r[1]
  # sand <- soil.table$sandtotal_r[1]
  # clay <- soil.table$claytotal_r[1]
  # silt <- soil.table$silttotal_r[1]
  # om <- soil.table$om_r[1]
  # 
  # ##quantify if topsoil or not
  # topsoil <- soil.table$topsoil_bin[1]
  
  ##if any value is zero the log component of the ptfs will not work
  
  
  ##start with the exponent
  n.exp <- -25.23-0.02195*clay+0.0074*silt-0.1940*om+45.5*bd-7.24*(bd^2)+
    0.0003658*(clay^2)+0.002885*(om^2)-(12.81/bd)-(0.1524/silt)-(0.01958/om)-
    0.2876*(log(silt))-0.0709*(log(om))-44.6*(log(bd))-0.02264*bd*clay+0.0896*bd*om+
    0.00718*topsoil*clay
  
  ##final n
  n.final <- 1 + exp(n.exp)
  
  return(round(n.final, digits = 2))
  
}


### larger function that takes classified horizons and assigns values to them, even exceptions like bedrock and organic matter
vg.fit <- function(classified.table){
  
  ##testing
  #classified.table <- soil.test
  
  ########fitting normal (this doesn't get put into the other if statements, takes if statement outputs).
  ##subset out the "normal" horizons
  vg.norm <- classified.table %>%
    subset(!(classes_results %in% c("Bedrock", "Org"))) ###Or operator doesn't seem to work to well, this way allows us to ignore bedrock and Organic horizons
  
  
  ###Use vg ptf functions to assign van genutchen values for each horizon for the "normal" horizons
  ##looks like it works pretty well. VG parameters fall within the ranges and are believable.
  norm.fit <- vg.norm %>%
    mutate(theta_s = theta.s(silttotal_r, claytotal_r, om_r, dbovendry_r, topsoil_bin),
           theta_r = theta.r(claytotal_r, om_r),
           alpha_cm = alpha(silttotal_r,claytotal_r,om_r, dbovendry_r, topsoil_bin),
           n = n.function(silttotal_r,claytotal_r,om_r, dbovendry_r, topsoil_bin))%>%
    mutate(m = m.function(n),
           alpha_m = alpha_cm*100,
           alpha_kpa = alpha_m*9.804)
  
  
  #######fitting exceptions (has to come after norm so we can get the column names (needed for when neither present))
  ###subset out the exceptions this is the main table to pull from and match stuff to
  vg.exc <- classified.table %>%
    subset(classes_results == "Bedrock" | classes_results == "Org")
  
  ##### if statement section is needed in order to properly determine which exception soil layers need to be quantified
  
  
  if ("Bedrock" %in% vg.exc$classes_results & "Org" %in% vg.exc$classes_results) { ###for when both are present
    ##if both in subset each out and assign parameters separately
    
    ##for organic matching table pull only variables we care about and order them properly
    stone.org.parameters2 <- stone.org.parameters%>%
      select(classes_results, theta_s, theta_r, alpha_cm, n, m, alpha_m, alpha_kpa)
    
    ###organic matter
    vg.exc.org <- subset(vg.exc, classes_results == "Org")%>%
      left_join(stone.org.parameters2, by = "classes_results")
    
    
    
    ##subset out just the bedrock
    vg.exc.bed <- subset(vg.exc, classes_results == "Bedrock")
    
    ##get bedrock parameters using below for loop
    #empty list
    exc.bed.fit <- list()
    
    for (i in 1:length(vg.exc.bed)) {
      
      ##select a row to look into first
      row <- vg.exc.bed[i,]
      
      ##take ksat value in row and get abs difference between it at ksat values in bedrock.parameters
      bed.param.subtract <- bedrock.parameters%>%
        mutate(ksat_diff = abs(ksat_mmhr - row$ksat[1]))%>%
        subset(ksat_diff == min(ksat_diff))
      
      ##select on the parameters we care about
      bed.select <- bed.param.subtract%>%
        select(theta_s, theta_r, alpha_cm, n,m, alpha_m, alpha_kpa)
      
      ##bind the columns to row
      row.fit <- cbind(row,bed.select)
      
      ##rbind to the exc.fit
      exc.bed.fit <- rbind(exc.bed.fit, row.fit)
      
      
    }
    
    ###combine them
    exc.fit <- rbind(exc.bed.fit, vg.exc.org)
    
    
    
  } else if ("Bedrock" %in% vg.exc$classes_results & !("Org" %in% vg.exc$classes_results)){ #when bedrock is present and organic isn't
    
    ##here the returned exc.fit will just be the for loop one, no need to rejoin with anything
    
    #empty list
    exc.fit <- list()
    
    for (i in 1:length(vg.exc)) {
      
      ##select a row to look into first
      row <- vg.exc[i,]
      
      ##take ksat value in row and get abs difference between it at ksat values in bedrock.parameters
      bed.param.subtract <- bedrock.parameters%>%
        mutate(ksat_diff = abs(ksat_mmhr - row$ksat[1]))%>%
        subset(ksat_diff == min(ksat_diff))
      
      ##select on the parameters we care about
      bed.select <- bed.param.subtract%>%
        select(theta_s, theta_r, alpha_cm, n,m, alpha_m, alpha_kpa)
      
      ##bind the columns to row
      row.fit <- cbind(row,bed.select)
      
      ##rbind to the exc.fit
      exc.fit <- rbind(exc.fit, row.fit)
      
      
    }
    
    
    
  } else if ("Org" %in% vg.exc$classes_results & !("Bedrock" %in% vg.exc$classes_results)) {#when Organic layer is present but not bedrock
    ###here exc.fit will be just the organic table
    
    ##for organic matching table pull only variables we care about and order them properly
    stone.org.parameters2 <- stone.org.parameters%>%
      select(classes_results, theta_s, theta_r, alpha_cm, n, m, alpha_m, alpha_kpa)
    
    ###organic matter
    vg.exc.org <- subset(vg.exc, classes_results == "Org")%>%
      left_join(stone.org.parameters2, by = "classes_results")
    
    ###define the final one
    exc.fit <- vg.exc.org
    
  } else if (!("Org" %in% vg.exc$classes_results) & !("Bedrock" %in% vg.exc$classes_results)){ #when neither are present
    
    ###make it a table with all the correct columns but no data is present (like the ksat calculator functions)
    exc.fit <- norm.fit[FALSE,]
    
    
  }
  
  
  ###putting it back together with the exception table and arrange to order properly
  vg.fit <- rbind(norm.fit, exc.fit)%>%
    group_by(compname)%>%
    arrange(hzdept_r, .by_group = T)
  
  ###return vg.fit
  return(vg.fit)
}


#####vg parameters done on the weighted average values since taking weighted averages of the parameters is not the best idea
##takes the generated soil layers with their weighted average values
##this is another effort to get the bimodal Ksat function to work since it doesn't seem to calculate changes in K properly
##updated 4/24/2024 typo with 'org" class assignment which made matching org stuff not possible
vg.profile.fit <- function(soil_profile){
  
  ##for testing
  # soil_profile <- soil.profile
  
  ###basically need to perform the same thing as the classify.horizons function which subsets out bedrock layers and organic layers and matches them up
  profile.clean <- soil_profile %>%
    mutate(classes_results = NA ) ###going identify each layer by soil group either norm, bedrock, or organic
  
  ###assign values of soil_group
  profile.clean[profile.clean$hzname == "bedrock", "classes_results"] <- "bedrock"
  profile.clean[profile.clean$hzname != "bedrock" & profile.clean$wt_om < 30, "classes_results"] <- "norm"
  profile.clean[profile.clean$hzname != "bedrock" & profile.clean$wt_om >= 30, "classes_results"] <- "Org"
  
  #####now subset out bedrock organic and norm tables
  #bedrock
  bedrock.tab <- subset(profile.clean, classes_results == "bedrock")
  norm.tab <- subset(profile.clean, classes_results == "norm")
  org.tab <- subset(profile.clean, classes_results == "Org")
  
  #####on the normal table run the vg pedotransfer functions on the values
  norm.fit <- norm.tab %>%
    mutate(theta_s = theta.s(wt_silt, wt_clay, wt_om, wt_bd_gcm3, top_soil),
           theta_r = theta.r(wt_clay, wt_om),
           alpha_cm = alpha(wt_silt,wt_clay,wt_om, wt_bd_gcm3, top_soil),
           n = n.function(wt_silt,wt_clay,wt_om, wt_bd_gcm3, top_soil))%>%
    mutate(m = m.function(n))%>%
    select(hzname, hzdept_r, hzdepb_r, wt_ksat_mmhr, wt_awc, wt_sand, wt_silt, wt_clay, wt_om, wt_frag, wt_bd_gcm3, wtdepannmin, theta_s,
           theta_r, alpha_cm, n, m, frag_theta_s, frag_theta_r, frag_alpha_cm, frag_n, frag_m, layer_midpoint, soil_vol, top_soil, classes_results)
  
  ###need to rearrange the norm.fit table to match with the other tables since this method adds values on
  
  ######### for organic match up the values
  ##for organic matching table pull only variables we care about and order them properly
  org.parameters2 <- stone.org.parameters%>%
    select(classes_results, theta_s, theta_r, alpha_cm, n, m)
  
  ###organic matter
  vg.org.matched <- org.tab%>%
    left_join(org.parameters2, by = "classes_results")%>%
    select(hzname, hzdept_r, hzdepb_r, wt_ksat_mmhr, wt_awc, wt_sand, wt_silt, wt_clay, wt_om, wt_frag, wt_bd_gcm3, wtdepannmin, theta_s,
           theta_r, alpha_cm, n, m, frag_theta_s, frag_theta_r, frag_alpha_cm, frag_n, frag_m, layer_midpoint, soil_vol, top_soil, classes_results)
  
  ##### need to rename the bedrock parameters and select out the columns we don't need
  names(bedrock.tab)[16:22] <- c('theta_s', 'theta_r', 'alpha_cm', 'alpha_m', 'alpha_kpa', 'n', 'm')
  bedrock.cleaned <- bedrock.tab%>%
    select(hzname, hzdept_r, hzdepb_r, wt_ksat_mmhr, wt_awc, wt_sand, wt_silt, wt_clay, wt_om, wt_frag, wt_bd_gcm3, wtdepannmin, theta_s,
           theta_r, alpha_cm, n, m, frag_theta_s, frag_theta_r, frag_alpha_cm, frag_n, frag_m, layer_midpoint, soil_vol, top_soil, classes_results)
  
  #####bind back together the norm and non-norm stuff and reorder it
  all.bind <- rbind(norm.fit, vg.org.matched, bedrock.cleaned)%>%
    arrange(hzdept_r)%>%
    mutate(frag_frac = wt_frag/100, soil_frac = soil_vol/100)%>%
    mutate(adj_theta_s = round(((frag_frac*frag_theta_s) + (soil_frac*theta_s)), digits = 3),
           adj_theta_r = round(((frag_frac*frag_theta_r) + (soil_frac*theta_r)), digits = 3))
  
  ##change the name of the second row to always be lower_soil
  all.bind$hzname[2] <- "lower_soil"
  
  
  return(all.bind)
  
}


## aws calculator 
aws.calculator <- function(soil.table){
  
  ##test table
  # soil.table <- soil.retrieved
  
  ##test if table supplied is NA (will occur if data is unrecoverable related to certain component types)
  
  if (all(is.na(soil.table))) {
    wt_awc <- NA
    
    
  } else {
    
    #testing table
    #soil.table <- test.point
    
    ##take the soil data from soil.data.retriever
    soil.table.100 <- soil.table%>%
      subset(hzdept_r <= 100)
    
    
    #subset just the aws value out for purpose of removing NA values later (don't want to remove data because NA in another column)
    # want hzname, depths, aws, compname, comppct, pct_component
    soil.table.100b <- soil.table.100 %>% ##make sure it references the one just above with excess soil depths removed
      select(compname, comppct_r, hzname,hzdept_r, hzdepb_r, awc_r, pct_component) #make sure extra columns are not in just the ones we need
    
    ##change any values deeper than 100 to just 100 in the hzdepb.r table (creates bottom stop)
    soil.table.100b[soil.table.100b$hzdepb_r > 100, "hzdepb_r"] <- 100
    
    ###now make column of the absolute value of the difference between the ranges to get the depth of each horizon
    soil.table.100c <- soil.table.100b%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(hz_aws = hz_depth * awc_r)
    
    #need to remove repeat rows if present, will skew the data
    #can use duplicated() functionalities of dplyr 
    soil.table.100d <- soil.table.100c[!duplicated(soil.table.100c),] ###this appears to work
    
    ##group by the component name and the percentage it represents and summ
    awc.sum <- soil.table.100d%>%
      na.omit()%>% ##removes the horizons that did not have any water data which generate NAs
      group_by(compname, comppct_r)%>% ###problem is that the NA omit function is removing rows that have water data because other columns are NA
      summarise(aws_100 = sum(hz_aws)) %>%
      mutate(wt_awc = comppct_r*aws_100) #weighted AWC
    
    ##compute the weighted average
    wt_awc <- sum(awc.sum$wt_awc)/ sum(awc.sum$comppct_r) ####success!!!!! the weighted aws that represents the map unit on soilweb
    #wt_awc <- sum(awc.sum$wt_awc)/ sum(awc.sum$comppct_r)
    
    
  }
  
  
  return(wt_awc)
  
}

########################## Water balance functions #################################

###function for layer drainage based on if saturated or not
#takes current theta, vg params, ksat, previous water storage (ws_prev), total water storage of the layer (ws_tot), and depth of the layer (hz_depth)
#outputs values of drainage in cm/day
####updated 5/15/2024, limited the sat.drain to go to field capacity (-300 cm^-1)
layer.drainage <- function(layer_name, theta_i, ws_prev, soil.df){ ##soil df is soil.df.daily profile that is modified for water table each day
  
  ###testing layer_name = "top50", theta_i = seep.applied$theta_seep_added[1], ws_prev = seep.applied$ws_seep_added[1], soil.df = soil.df.daily
  # theta_i = seep.applied$theta_seep_added[1]
  # layer_name = "top50"
  # ws_prev = seep.applied$ws_seep_added[1]
  # soil.df = soil.df.daily
  
  
  
  ###from layer_name assign values by subsetting the soil.df table for the layer in question (makes it less annoying to use function)
  layer.data <- subset(soil.df, hzname == layer_name)
  
  ###set values (make sure to take adjusted values for theta r and s)
  theta_s = layer.data$adj_theta_s[1]
  theta_r = layer.data$adj_theta_r[1]
  # theta_s = layer.data$theta_s[1] ##testing
  # theta_r = layer.data$theta_r[1] ##testing
  alpha = layer.data$alpha_cm[1]
  n = layer.data$n[1]
  m = layer.data$m[1]
  ksat = layer.data$wt_ksat_mmhr[1]
  wt_tot = layer.data$tot_adj_ws_cm[1]
  dept_r = layer.data$hzdept_r[1]
  depb_r = layer.data$hzdepb_r[1]
  hz_depth = layer.data$hz_depth[1]
  wtdepmin = layer.data$wtdepannmin[1]
  wt_presence = layer.data$wt_presence[1]
  ws_fc = layer.data$ws_fc[1]
  theta_fc = layer.data$theta_fc[1]
  #wtdepmin = 70 ##testing
  
  
  ###plotting the change in K with change in theta just to see
  # k.table <- data.frame(theta = seq(theta_r, theta_s, by = 0.001))%>%
  #   mutate(k_mm_hr = k.function(theta = theta, adj_theta_r = theta_r, adj_theta_s = theta_s, alpha_cm = alpha,
  #                               n = n, m = m, ksat = ksat))
  # plot(x = k.table$theta, y = k.table$k_mm_hr)
  
  
  
  ###see if the layer has the water table within it and if so this prevents drainage, this if it isn't NA
  if (layer.data$wt_presence[1] == 0){ ###if no water table or water table is not present in the specified layer than drainage rate is normal
    
    ###if theta_i >= theta.s than drainage is equation to Ksat for 0.11 hrs and then k(theta) for a single day
    if (theta_i > theta_fc) {
      
      ##get K at the current theta
      k.initial <- k.function(theta = theta_i, adj_theta_r = theta_r, adj_theta_s = theta_s, alpha_cm = alpha,
                              n = n, m = m, ksat = ksat)
      
      ###convert ksat from mm/hr to cm/hr 
      k.initial.cmhr = k.initial*0.1
      
      ###initial drainage drainage multiply by 0.11 hrs
      initial.drain = k.initial.cmhr*0.11
      
      ######## Recalculate the ws for soil with sat.drain factored in
      #richards equation dictates that K needs to be multiplied by the depth of the horizon
      ws.satdrain <- ifelse(ws_prev - (hz_depth*initial.drain) <= ws_fc, ws_fc, ws_prev - (initial.drain))
      
      ##adjust sat drain to be limited to the difference between saturated and ws.satdrain (post sat drain ws)
      sat.dr.adj <- ws_prev - ws.satdrain
      
      ##new theta, divide ws.satdrain value by the layer depth and bound to prevent lower than theta_r
      theta_2 <- ifelse(ws.satdrain/hz_depth < theta_r, theta_r, ws.satdrain/hz_depth)
      
      ##calculate the ksat with the new theta (is in mm/hr)
      k_unsat <- k.function(theta = theta_2, adj_theta_r = theta_r, adj_theta_s = theta_s, alpha_cm = alpha,
                            n = n, m = m, ksat = ksat)
      ##conver to cm/day for daily calculation
      k_unsat_cmday = k_unsat *2.4
      
      ##unsat drainage
      unsat.drain = k_unsat_cmday
      
      ##total drainage, 
      tot.drain = unsat.drain+sat.dr.adj 
      
      
    }else if (theta_i <= theta_fc) {
      
      ###if not saturated that the drainage is just the unsaturated drainage K
      ##check to make sure theta is not less than theta_r
      theta_i <- ifelse(theta_i < theta_r, theta_r, theta_i)
      
      
      #calculate
      k_unsat_mmhr <- k.function(theta = theta_i, adj_theta_r = theta_r, adj_theta_s = theta_s, alpha_cm = alpha,
                                 n = n, m = m, ksat = ksat)
      
      ##convert to cm/day
      k_unsat_cmday = k_unsat_mmhr*2.4
      
      
      
      ##total drainage
      tot.drain = k_unsat_cmday
      
      
    }
  } else if (wt_presence == 1) { ##if water table is present in the layer no drainage will occur from that layer
    
    ###set total drainage to zero
    tot.drain = 0
    
    
    
    
    
  }
  
  
  
  #remember its okay if total drainage is more than the water storage of the horizon,
  #we just need to compare total drainage potential with the input from above layers to get the flux
  #if its still larger than total storage of the layer, we will set it to its minimum
  return(round(tot.drain, digits = 5))
}

###if water table is present then modify the soil.profile tables theta_r values for the layers affected by the water table
water_table_mod <- function(soil.profile) {
  
  ##testing
  #soil.profile <- soil.df.daily
  
  # ###testing if wt present (water table present at 60 cm)
  # soil.profile <- profile.fit%>%
  #   mutate(wtdepannmin = 30)
  
  ####if wtdepannmin is NA then return the original soil.profile undmodified
  if (is.na(soil.profile$wtdepannmin[1])) {
    
    ##no changes to theta_s, but still add the binary for presence of the water table (will be set to zero)
    profile.mod <- soil.profile%>%
      mutate(wt_presence = 0,
             percent_sat = 0,
             pseudo_theta_r = adj_theta_r,
             pseudo_min_ws = min_adj_ws_cm) #set these to 0, because they water values can't be less than zero
    
    
  } else if (!is.na(soil.profile$wtdepannmin[1])){ ###if water table present modify theta_r of affected layers and set binary value to indicate no drainage
    
    ####get difference range
    
    ####set binary value for each layer to see where effect takes place based on if the horizon bottom depth is greater than the wt depth
    ##if water table is present within the horizon determine if the horizon is completely saturated or only partly affected by the water table's presence
    profile.mod <- soil.profile%>%
      mutate(wt_presence = ifelse(hzdepb_r > wtdepannmin, 1,0),
             percent_sat = ifelse(wt_presence == 1 & (wtdepannmin <= hzdept_r),1, (hzdepb_r-wtdepannmin)/(hzdepb_r- hzdept_r) ),
             percent_sat = round(ifelse(percent_sat >=0, percent_sat, 0), digits = 2))%>%
      mutate(pseudo_min_ws = round(ifelse(percent_sat == 0, min_adj_ws_cm, ifelse(percent_sat > 0 & percent_sat <1,(hzdepb_r- wtdepannmin)*adj_theta_s + (wtdepannmin - hzdept_r)*adj_theta_r,
                                                                                  tot_adj_ws_cm)), digits = 3),
             pseudo_theta_r = round(pseudo_min_ws/hz_depth, digits = 3),)
    ##pseudo values are the minimum water content of the horizon due to presence of water table saturated parts of water table.
    ##these pseudo values will be used in the drainage function to determine the lower limits of soil water content
    ## for the pseudo min water storage not only does it need to account for water table saturation It still needs to count the theta_r of the non saturated portion 
    
    
  }
  
  return(profile.mod)
  
}

##flux function for the drainage_update(), takes table with all fluxes calculated and has important values
#4/23/2024
flux.func <- function(flux.table){
  
  ###testing
  # flux.table <- drain.flux
  
  ###adding new columns and calculating ws_deficit
  flux.table <- flux.table%>%
    mutate(ws_deficit = tot_adj_ws_cm - ws_prev ,
           flux_mod = NA)
  
  ###iterate through table starting from the lowest layer to calculate flux modification
  for (i in length(flux.table$hzname):1) {
    
    ##testing
    # i <- 2
    
    
    ##if statement to generate the flux_mod a value that tells us how much to reduce drainage from upper layer if the lower layer will becomes saturated
    if (flux.table[i,"ws_prev"]+flux.table$flux[i] > flux.table$tot_adj_ws_cm[i]) { ###if flux plus the previous ws is greater than maximum ws than flux mod needs to be made
      
      
      
      ###make flux mod
      flux.table$flux_mod[i] <- flux.table$flux[i] - flux.table$ws_deficit[i]
      
      
    } else if (flux.table[i,"ws_prev"]+flux.table$flux[i] < flux.table$min_adj_ws_cm[i]) { ###if flux would put ws below the minimum set to minimum
      
      
      ###change the flux mod to 0
      flux.table$flux_mod[i] <- 0
      
    } else if (flux.table[i,"ws_prev"]+flux.table$flux[i] >= flux.table$min_adj_ws_cm[i] & flux.table[i,"ws_prev"]+flux.table$flux[i] <= flux.table$tot_adj_ws_cm[i]){
      
      
      ###flux mod = 0
      flux.table$flux_mod[i] <- 0
      
    }
    
    
  }
  
  
  ####now lead the flux mod column
  flux.tab.lead <- flux.table%>%
    mutate(flux_lead = lead(flux_mod, default = 0))%>%
    mutate(flux_actual = flux+flux_lead-flux_mod) ###this sets the fluxes to the real value.
  
  ####get the new water storage
  new.ws.table <- flux.tab.lead%>%
    mutate(new_ws = ifelse(ws_prev+flux_actual > tot_adj_ws_cm, tot_adj_ws_cm,
                           ifelse(ws_prev + flux_actual < min_adj_ws_cm, min_adj_ws_cm,
                                  ws_prev + flux_actual)))
  
  
  ########return the table with new water storage
  return(new.ws.table)
  
}

###water storage update function, takes total potential flux from any certain layer. Accounts for theta_r and theta_s and keeps water levels in this range
#this gets used before accounting for rain and other external inputs
#4/23/2024
drain_update <- function(drain_table, soilprofile){
  
  # ######testing drain_table = drain.table, soilprofile = soil.df.daily, drain_table = uphill_drain_st, soilprofile = soil.df.uphill
  # drain_table = uphill_drain_st  ###gives all drainage values for the current day and the previous day water contents
  # soilprofile = soil.df.uphill    ###gives the important layer parameters
  
  
  ###function needs to determine proper drainage amounts based on the limits set by maximal and minimal water storage
  #if upper layer drains into lower layer an amount larger than the lower layer can hold at a certain time difference is 
  #left in the upper layer
  
  ###get flux for each layer by subtracting input drainage from output drainage for each layer
  drain.flux <- drain_table%>%
    mutate(flux = drain_inputs - drain_pot)%>%
    left_join(soilprofile, by = "hzname")
  
  ####now need to determine actual flux and the new water storage values after the drainage (ws)
  new.ws.table <- flux.func(drain.flux)%>%
    mutate(thru_drain = ifelse((drain_inputs + ws_prev) >= drain_pot, drain_pot,
                               (drain_inputs + ws_prev)))# give the amount of water drained through the layer within the timestep
  
  ###determine thru_flux with easier to see if else statements
  
  
  return(new.ws.table)
  
}

###seepage inputs function post_drai_tab is the upland profile that is calculated
seepage_inputs <- function(post_drai_tab, cfa, since_rain){
  
  #####basic function is if the days since rainfall is higher than flow contribution area than the seepage ceases
  ##if the days since rain is lower than this than the seepage input is present for that day
  
  if (since_rain > cfa) {
    ##if days post beyond cfa then no seepage inputs occur
    seep.input <- 0
    
    
  } else{
    
    #if less than cfa make the inputs equal to the potential drainage (ideally it would be actual drainage on the first day, but thats too hard to put in)
    seep.input <- post_drai_tab$drain_pot[3]
    
    
    
  }
  
  
  return(seep.input)
  
}


###New final inputs function (3/22/2024) to get water_flux values showing total balance for each layer
#also calculates final_theta in the function instead of before
final_inputs <- function(seep_inputs, infil_input, runoff_inputs, post.drain.tab, evap, soil.df){
  
  ###testing
  # seep_inputs <- seepage
  # infil_input <- infil.input
  # runoff_inputs <- runoff.inputs
  # post.drain.tab <- post.drainage
  # evap <- evap.adj
  # soil.df <- soil.df.daily
  
  
  #conver evap to cm per day
  evap_cmday <- evap/10 ###convert the evap to cm per day
  
  
  ###to apply seepage evenly across the soil profile divide the value by the depth of the column
  seep.spread <- round(seep_inputs / max(soil.df$hzdepb_r), digits = 5)
  
  ###to get how much in each layer multiply the hz_depth by the seep.spread
  post.drain2 <- post.drain.tab%>%
    mutate(seep_applied = hz_depth*seep.spread)%>%
    mutate(runoff_applied = ifelse(hzname == "top50", runoff_inputs, 0),
           infil_applied = ifelse(hzname == "top50", infil_input, 0),
           evap_applied = ifelse(hzname == "top50", evap_cmday, 0))
  
  #####now add the additions to the new_ws value and conditionally limit them to the max or mins when required
  final_ws_values <- post.drain2%>%
    mutate(ws_additions = new_ws + (seep_applied+runoff_applied+infil_applied - evap_applied),
           water_flux = seep_applied+runoff_applied+infil_applied - evap_applied)%>%
    mutate(ws_i = ifelse(ws_additions > tot_adj_ws_cm, tot_adj_ws_cm, ifelse(ws_additions < min_adj_ws_cm & wt_presence == 0, min_adj_ws_cm, 
                                                                             ifelse(ws_additions < pseudo_min_ws & wt_presence == 1, pseudo_min_ws, ws_additions))))%>%
    mutate(final_theta = ws_i/hz_depth)%>%
    mutate(final_theta = ifelse(final_theta > adj_theta_s, adj_theta_s, final_theta))
  
  return(final_ws_values)
  
}


#####seep applier, applies seepage values prior to drainage in order to reduce over saturation that seepage often causes
### Added 5-15-2024 to restructure how seepage is accounted for
seepage.applier <- function(daily.soil.df = soil.df.daily, seepage.input = seepage, theta_topi = top_theta_p, theta_subi = sub_theta_p, theta_bedi = bed_theta_p){
  
  ###testing
  # daily.soil.df <- soil.df.daily
  # seepage.input <- seepage
  # theta_topi <- top_theta_p
  # theta_subi <- sub_theta_p
  # theta_bedi <- bed_theta_p
  
  
  ###grap previous day theta values and add them to the datatable
  daily.soil.df$theta_p <- c(theta_topi,theta_subi, theta_bedi)
  
  #####do what is normally done for seepage and split divide it evenly across the soil profile
  seep.spread <- round(seepage.input / max(daily.soil.df$hzdepb_r), digits = 5)
  
  ###add the seep.spread to each of the soil layers and bound it correctly
  daily.seep.applied <- daily.soil.df%>%
    mutate(ws_p = round(theta_p*hz_depth, digits = 5))%>%
    mutate(ws_seep_added = ifelse((seep.spread*hz_depth)+ws_p > tot_adj_ws_cm, tot_adj_ws_cm,(seep.spread*hz_depth)+ws_p),
           theta_seep_added = ifelse(ws_seep_added/hz_depth > adj_theta_s, adj_theta_s, ifelse((ws_seep_added/hz_depth) < adj_theta_r, adj_theta_r, (ws_seep_added/hz_depth))))%>%
    select(hzname,theta_p, ws_p, theta_seep_added, ws_seep_added)
  
  return(daily.seep.applied)
  
}


##since the mutate only will work with values already in the dataframe, we will have to iterate through each row for this
###this will output table of soil moisture profile curve
#updated 05-15-2024 revised seepage to not force soil to saturation
water.balance <- function(filled.df, soil.df.input, updateProgress = NULL){ ##takes input of prism dataframe with rainfall interception already calculated and input table of the soil parameters
  
  # # #####for testing
  # filled.df <- prism.cc2
  # soil.df.input <- profile.fit
  
  ###rename just to make it easier below (lazy move so i don't have to change name in every following call)
  soil.df <- soil.df.input
  
  ###create the uphill seepage table
  soil.df.uphill <- soil.df%>%
    mutate(wtdepannmin = NA, wt_presence = 0) ###don't get confused, its the same as the normal one, only modified to remove water table presence
  ##below i set the seepage table values with the original, so don't worry that it isn't taking exactly from this table
  
  ############# Setting values for the target soil profile ##########
  
  
  ###setting some constants for easier calling
  top_hzdepth <- soil.df$hz_depth[1]
  sub_hzdepth <- soil.df$hz_depth[2]
  bed_hzdepth <- soil.df$hz_depth[3]
  
  seep_top_hzdepth <- soil.df$hz_depth[1]
  seep_sub_hzdepth <- soil.df$hz_depth[2]
  seep_bed_hzdepth = soil.df$hz_depth[3]
  
  ###with soil.df set the values of parameters for each layer to make calling them easier
  ##give the adj_theta values for each layer except the bedrock since it doesn't have any data to calculated adjusted values
  top_thetas <- soil.df$adj_theta_s[1]
  sub_thetas <- soil.df$adj_theta_s[2]
  bed_thetas <- soil.df$adj_theta_s[3]
  top_thetar <- soil.df$adj_theta_r[1]
  sub_thetar <- soil.df$adj_theta_r[2]
  bed_thetar <- soil.df$adj_theta_r[3]
  top_alphacm <- soil.df$alpha_cm[1]
  sub_alphacm <- soil.df$alpha_cm[2]
  bed_alphacm <- soil.df$alpha_cm[3]
  top_n <- soil.df$n[1]
  sub_n <- soil.df$n[2]
  bed_n <- soil.df$n[3]
  top_m <- soil.df$m[1]
  sub_m <- soil.df$m[2]
  bed_m = soil.df$m[3]
  
  ######infiltration layer water storage values (upper 5 cm of soil)######
  #set infil layer depth
  infil_depth <- 5
  
  ##get the maximum water storage of the upper infiltration layer
  ws_topinfil_cm <- top_thetas *infil_depth
  
  
  ### yes please  
  ############ setting values for seepage horizon ###########
  seep_top_thetas <- soil.df$adj_theta_s[1]
  seep_sub_thetas <- soil.df$adj_theta_s[2]
  seep_bed_thetas <- soil.df$adj_theta_s[3]
  
  seep_top_thetar <- soil.df$adj_theta_r[1]
  seep_sub_thetar <- soil.df$adj_theta_r[2]
  seep_bed_thetar <- soil.df$adj_theta_r[3]
  
  seep_top_alphacm <- soil.df$alpha_cm[1]
  seep_sub_alphacm <- soil.df$alpha_cm[2]
  seep_bed_alphacm <- soil.df$alpha_cm[3]
  
  seep_top_n <- soil.df$n[1]
  seep_sub_n <- soil.df$n[2]
  seep_bed_n <- soil.df$n[3]
  
  seep_top_m = soil.df$m[1]
  seep_sub_m = soil.df$m[2]
  seep_bed_m = soil.df$m[3]
  
  #Ksat values for each layer
  ksat_top = soil.df$wt_ksat_mmhr[1]
  ksat_sub = soil.df$wt_ksat_mmhr[2]
  ksat_bed = soil.df$wt_ksat_mmhr[3]
  
  ksat_seep_top = soil.df$wt_ksat_mmhr[1]
  ksat_seep_sub = soil.df$wt_ksat_mmhr[2]
  ksat_seep_bed = soil.df$wt_ksat_mmhr[3]
  
  
  ##water storage capacity of each layer
  ws_top = soil.df$tot_adj_ws_cm[1]
  ws_sub = soil.df$tot_adj_ws_cm[2]
  ws_bed = soil.df$tot_adj_ws_cm[3]
  
  ws_seep_top = soil.df$tot_adj_ws_cm[1]
  ws_seep_sub = soil.df$tot_adj_ws_cm[2]
  ws_seep_bed = soil.df$tot_adj_ws_cm[3]
  
  ##minimum water storage values
  ws_min_top <- soil.df$adj_theta_r[1]*soil.df$hz_depth[1]
  ws_min_sub <- soil.df$adj_theta_r[2]*soil.df$hz_depth[2]
  ws_min_bed <- soil.df$adj_theta_r[3]*soil.df$hz_depth[3]
  
  ws_seep_min_top <- soil.df$min_adj_ws_cm[1]
  ws_seep_min_sub <- soil.df$min_adj_ws_cm[2]
  ws_seep_min_bed <- soil.df$min_adj_ws_cm[3]
  
  
  ##################### Creating columns to be filled through the simulation #############################
  
  ###create the empty data columns for the theta values and water storage (WS) values for each layer, and water potential (psi) columns
  filled.df = filled.df %>%
    mutate(runoff_target = NA, runoff_inputs = NA, ws_topinfil_cm = NA, theta_top_cm3cm3 = NA, theta_sub_cm3cm3 = NA, theta_bed_cm3cm3 = NA,
           ws_top_cm = NA, ws_sub_cm = NA, ws_bed_cm = NA,
           psi_top_cm = NA, psi_sub_cm = NA, psi_bed_cm = NA,
           k_top_mmhr = NA, k_sub_mmhr = NA, k_bed_mmhr = NA,
           seep_theta_top = NA, seep_theta_sub = NA, seep_theta_bed = NA,
           seep_ws_top = NA, seep_ws_sub = NA, seep_ws_bed = NA,
           seep_k_top = NA, seep_k_sub = NA, seep_k_bed = NA,
           seep_bank = NA, seep_day_left = NA)
  
  
  ####set starting values to the theta_s for each layer and the rock fragment values, everything is going to start at saturation
  ##testing
  #filled.df$theta_top_cm3cm3[1] = 0.31
  filled.df$theta_top_cm3cm3[1] = top_thetas
  filled.df$theta_sub_cm3cm3[1] = sub_thetas
  filled.df$theta_bed_cm3cm3[1] = bed_thetas
  
  
  
  ###calculate the water storage and the water potential for each layer and its rock values for the first row
  filled.df$ws_top_cm[1] = filled.df$theta_top_cm3cm3[1] *top_hzdepth
  filled.df$ws_sub_cm[1] = filled.df$theta_sub_cm3cm3[1] *sub_hzdepth
  filled.df$ws_bed_cm[1] = filled.df$theta_bed_cm3cm3[1] *bed_hzdepth
  
  
  
  ###get initial water potential values for each layer, appears to work properly
  filled.df$psi_top_cm[1] <- vg.pot.conversion(theta = filled.df$theta_top_cm3cm3[1], theta_r = top_thetar, theta_s = top_thetas,
                                               alpha = top_alphacm, n = top_n, m = top_m)
  filled.df$psi_sub_cm[1] <- vg.pot.conversion(theta = filled.df$theta_sub_cm3cm3[1], theta_r = sub_thetar, theta_s = sub_thetas,
                                               alpha = sub_alphacm, n = sub_n, m = sub_m)
  filled.df$psi_bed_cm[1] <- vg.pot.conversion(theta = filled.df$theta_bed_cm3cm3[1], theta_r = bed_thetar, theta_s = bed_thetas,
                                               alpha = bed_alphacm, n = bed_n, m = bed_m)
  
  ### set initial values for the hydraulic conductivity for each layer (k_"layer") using k.function
  filled.df$k_top_mmhr[1] <- k.function(theta = filled.df$theta_top_cm3cm3[1], adj_theta_r = top_thetar, adj_theta_s = top_thetas, alpha_cm = top_alphacm,
                                        n = top_n, m = top_m, ksat = ksat_top)
  filled.df$k_sub_mmhr[1] <- k.function(theta = filled.df$theta_sub_cm3cm3[1], adj_theta_r = sub_thetar, adj_theta_s = sub_thetas, alpha_cm = sub_alphacm,
                                        n = sub_n, m = sub_m, ksat = ksat_sub)
  filled.df$k_bed_mmhr[1] <- k.function(theta = filled.df$theta_bed_cm3cm3[1], adj_theta_r = bed_thetar, adj_theta_s = bed_thetas, alpha_cm = bed_alphacm,
                                        n = bed_n, m = bed_m, ksat = ksat_bed)
  
  ##set initial runoff as zero (since nothing has been calculated yet)
  filled.df$runoff_target[1] <- 0
  filled.df$runoff_inputs[1] <- 0
  filled.df$ws_topinfil_cm[1] = ws_topinfil_cm
  
  ####### Conditionally setting the seepage values, we don't want to still need to calculate these values if the CFA is 0
  
  if (soil.df$cfa_m2[1] == 0 | is.na(soil.df$cfa_m2[1])) { ###if no flow contribution area than the seepage values are set to NA for the entire column
    
    
    #setting the initial theta value
    filled.df$seep_theta_top[1] = NA
    filled.df$seep_theta_sub[1] = NA
    filled.df$seep_theta_bed[1] = NA
    
    
    ###setting the water storage available
    filled.df$seep_ws_top[1] = NA
    filled.df$seep_ws_sub[1] = NA
    filled.df$seep_ws_bed[1] = NA
    
    
    ##initial K for the seepage
    filled.df$seep_k_top[1] <- NA
    filled.df$seep_k_sub[1] <- NA
    filled.df$seep_k_bed[1] <- NA
    
    filled.df$seep_bank[1] <- 0
    
    
  } else if (soil.df$cfa_m2[1] > 0){
    
    #setting the initial theta value
    filled.df$seep_theta_top[1] = seep_top_thetas
    filled.df$seep_theta_sub[1] = seep_sub_thetas
    filled.df$seep_theta_bed[1] = seep_bed_thetas
    
    ###setting the water storage available
    filled.df$seep_ws_top[1] = filled.df$seep_theta_top[1] *seep_top_hzdepth
    filled.df$seep_ws_sub[1] = filled.df$seep_theta_sub[1] *seep_sub_hzdepth
    filled.df$seep_ws_bed[1] = filled.df$seep_theta_bed[1] *seep_bed_hzdepth
    
    
    ##initial K for the seepage
    filled.df$seep_k_top[1] <- ksat_seep_top
    filled.df$seep_k_sub[1] <- ksat_seep_sub
    filled.df$seep_k_bed[1] <- ksat_seep_bed
    
    ###seepage storage column
    filled.df$seep_bank[1] <- 0
    filled.df$seep_day_left[1] <- 0
    
    
    
    ######### generate the value that will be used for seepage from first day
    
    seep_theta_top_st = filled.df$seep_theta_top[1]
    seep_theta_sub_st = filled.df$seep_theta_sub[1]
    seep_theta_bed_st = filled.df$seep_theta_bed[1]
    
    
    seep_ws_top_st = filled.df$seep_ws_top[1]
    seep_ws_sub_st = filled.df$seep_ws_sub[1]
    seep_ws_bed_st = filled.df$seep_ws_bed[1]
    
    bed.name <- soil.df$hzname[length(soil.df$hzname)]
    
    #get beginning drainage for each layer
    drain_uphill_top_st <- layer.drainage(layer_name = "top50", theta_i = seep_theta_top_st, ws_prev = seep_ws_top_st, soil.df = soil.df.uphill)
    drain_uphill_sub_st <- layer.drainage(layer_name = "lower_soil", theta_i = seep_theta_sub_st, ws_prev = seep_ws_sub_st, soil.df = soil.df.uphill)
    drain_uphill_bed_st <- layer.drainage(layer_name = bed.name, theta_i = seep_theta_bed_st, ws_prev = seep_ws_bed_st, soil.df = soil.df.uphill)
    
    uphill_drain_st <- data.frame(hzname = c(soil.df.uphill$hzname[1], soil.df.uphill$hzname[2], soil.df.uphill$hzname[3]),
                                  drain_pot = c(drain_uphill_top_st, drain_uphill_sub_st, drain_uphill_bed_st),
                                  ws_prev = c(seep_ws_top_st, seep_ws_sub_st, seep_ws_bed_st))%>%
      mutate(drain_inputs = lag(drain_pot, default = 0))
    
    
    
    ###### put through the drainage update function before adding rain and other seepage values
    post.drainage.uphill.st <- drain_update(drain_table = uphill_drain_st, soilprofile = soil.df.uphill)%>%
      mutate(theta_drain = new_ws/hz_depth)
    
    ####rate of seepage that will take away from the seep.bank
    seep.rate <- post.drainage.uphill.st$thru_drain[3]
    
  }
  
  
  
  
  
  
  
  
  ######progress bar so we can tell if it freezes or not
  progress_bar = txtProgressBar(min = 2, max = length(filled.df$date), style = 1, char = "=")
  
  
  
  ######################### Calculating the flux over time #######################
  
  ########with initial values in place we can now begin calculating flux over time
  ##### for loop required since these values are being generated each day independently
  #2:length(filled.df$date) ##this is the original range value in for loop (put back after using testing ranges)
  
  for (i in 2:length(filled.df$date)) {
    
    ######for testing
    # i<- 2
    
    ###previous day index (not a testing value)
    p <- i-1
    
    
    ###for testing print i to see where errors occur
    #print(i)
    
    
    ####set the water table depth for that day and use water_table_mod to tell layer where the water table is present in the soil horizon
    soil.df.daily <- soil.df%>%
      mutate(wtdepannmin = filled.df$water_tbl_depth_cm[i])
    #put through the water table mod to get where it is present in the horizon and how water table is effecting drainage
    soil.df.daily <- water_table_mod(soil.df.daily)
    
    
    ###need to set the daily table for the uphill cfa as well
    soil.df.daily.uphill <- water_table_mod(soil.df.uphill)
    
    ######## set values for the specific day by taking soil values on the previous day #####
    top_theta_p <- filled.df$theta_top_cm3cm3[p]
    sub_theta_p <- filled.df$theta_sub_cm3cm3[p]
    bed_theta_p <- filled.df$theta_bed_cm3cm3[p]
    
    seep_theta_top_p = filled.df$seep_theta_top[p]
    seep_theta_sub_p = filled.df$seep_theta_sub[p]
    seep_theta_bed_p = filled.df$seep_theta_bed[p]
    
    ##get previous water storage values
    top_ws_p <- filled.df$ws_top_cm[p]
    sub_ws_p <- filled.df$ws_sub_cm[p]
    bed_ws_p <- filled.df$ws_bed_cm[p]
    
    seep_ws_top_p = filled.df$seep_ws_top[p]
    seep_ws_sub_p = filled.df$seep_ws_sub[p]
    seep_ws_bed_p = filled.df$seep_ws_bed[p]
    
    
    ####get the evap for later (in mm_day)
    daily_evap <- filled.df$ground_evap_mmday[i]
    
    ##get the name of the bedrock layer since it varies 
    bed.name <- soil.df$hzname[3]
    
    
    ###################### Uphill profile calculations (uses duplicated soil profile modified to make sure drainage present) ##################
    
    ###calculate the values and change for the uphill contributing area
    ###if initial values for seepage are NA than there is no seepage and seepage calculation is zero
    
    if (is.na(filled.df$seep_ws_top[1])) { ###if this is NA then no seepage contributes
      
      ###if no uphill contributing area than seepage does not occur
      seep.deposit = 0
      seepage = 0
      
    } else { #####when the seepage is present
      
      
      ###get seepage for that day based on the seepage days left of previous day
      ###need to get this now before andy drainage is added to the seep profile
      ######seepage value for that day (seepage rate is the saturated thru flow determined by the first day)
      seepage <- ifelse(filled.df$seep_bank[p] == 0, 0, seep.rate)
      
      
      #get beginning drainage for each layer
      drain_uphill_top_cm <- layer.drainage(layer_name = "top50", theta_i = seep_theta_top_p, ws_prev = seep_ws_top_p, soil.df = soil.df.daily.uphill)
      drain_uphill_sub_cm <- layer.drainage(layer_name = "lower_soil", theta_i = seep_theta_sub_p, ws_prev = seep_ws_sub_p, soil.df = soil.df.daily.uphill)
      drain_uphill_bed_cm <- layer.drainage(layer_name = bed.name, theta_i = seep_theta_bed_p, ws_prev = seep_ws_bed_p, soil.df = soil.df.daily.uphill)
      
      uphill_drain_table <- data.frame(hzname = c(soil.df.uphill$hzname[1], soil.df.uphill$hzname[2], soil.df.uphill$hzname[3]),
                                       drain_pot = c(drain_uphill_top_cm, drain_uphill_sub_cm, drain_uphill_bed_cm),
                                       ws_prev = c(seep_ws_top_p, seep_ws_sub_p, seep_ws_bed_p))%>%
        mutate(drain_inputs = lag(drain_pot, default = 0))
      
      ###### put through the drainage update function before adding rain and other seepage values
      post.drainage.uphill <- drain_update(drain_table = uphill_drain_table, soilprofile = soil.df.daily.uphill)%>%
        mutate(theta_drain = new_ws/hz_depth)
      
      ###add water drained out of the soil profile to the seep.bank by making a deposit (this gets added to seep.bank term)
      ##added 0.75 factor to try and reduce amount stored and balance out the equation, assuming that 25% of drainage percolates out
      seep.deposit <- post.drainage.uphill$thru_drain[3] * soil.df$cfa_m2[1]
      
      ####testing to see if just using the amount drained from upper soils would be a better proxy of
      
      
      ###remove water from the bank via multiplying cfa by the evap for the day (convert evap to cm)
      seep.evap <- (daily_evap/10)* soil.df$cfa_m2[1]
      
      ##add to seep.bank
      filled.df$seep_bank[i] = filled.df$seep_bank[p] + seep.deposit - seep.evap
      
      ########### now need to calculate inputs with the post.drainage table (we only need to worry about rain inputs)
      filled.df$uphill_drain_seep[i] <- post.drainage.uphill$thru_drain[3]*soil.df$cfa_m2[1]
      
      ###set the new theta for the top layer from the post.drainage table, used in rain infiltration calculations
      theta.dr.top.uphill <- post.drainage.uphill$theta_drain[1]
      
      #get the current waterstorage for the infil layer for the uphill seepage horizon
      wsi_seepinfil_cm <- theta.dr.top.uphill * infil_depth
      
      ####run the function to get the infil and runoff values for the target location of interest (not the uphill drainage)
      infil_run_uphill <- infiltration(ws_infil_i = wsi_seepinfil_cm, top_infil_ws = ws_topinfil_cm , rain_duration = filled.df$rain_dur_hrs[i],
                                       rain_rate = filled.df$adj_prate_cmhr[i], precip_mm = filled.df$adj_precip_mm[i],
                                       ksat_top_mmhr = ksat_top, slope_mod = soil.df$seep_angle_mod[1], soil_df = soil.df)
      
      ###infil inputs
      infil.input.uphill <- infil_run_uphill$infil_cm[1]
      
      ###runoff inputs for the target area
      runoff.downhill <- infil_run_uphill$runoff_cm[1]*soil.df$cfa_m2[1]
      
      ####give final values to the table and update in the filled.df for the current day, and give the final seepage value
      ####get final inputs
      final.values.uphill <- final_inputs(seep_inputs = 0, infil_input = infil.input.uphill, runoff_inputs = 0, post.drain.tab = post.drainage.uphill, 
                                          evap = daily_evap,
                                          soil.df = soil.df.daily.uphill)
      # mutate(final_theta = ws_i/hz_depth)
      
      ##get seepage drain, this determines how quickly the uphill seep bank depletes
      # (2.4 converts mm/hr to cm/day)
      seep.drain <- (soil.df.daily.uphill$wt_ksat_mmhr[3]*2.4)*final.values.uphill$seep_angle_mod[1]
      
      ##seep days remaining: seep.bank/seep.rate
      filled.df$seep_day_left[i] <- filled.df$seep_bank[p]/seep.drain
      
      ###update runoff inputs
      filled.df$runoff_inputs[i] <- runoff.downhill
      
      
      #now also need to update the seep.bank in the filled.df to reflect amount lost, subtract bank by the seep.drain rate
      seep_bank_i = filled.df$seep_bank[i] - seep.drain
      
      ###floor limit the bank
      filled.df$seep_bank[i] <- ifelse(seep_bank_i <0, 0, seep_bank_i)
      
      #####put final ws and theta values into the table
      filled.df$seep_theta_top[i] <- final.values.uphill$final_theta[1]
      filled.df$seep_theta_sub[i] <- final.values.uphill$final_theta[2]
      filled.df$seep_theta_bed[i] <- final.values.uphill$final_theta[3]
      
      #new ws values
      filled.df$seep_ws_top[i] <- final.values.uphill$ws_i[1]
      filled.df$seep_ws_sub[i] <- final.values.uphill$ws_i[2]
      filled.df$seep_ws_bed[i] <- final.values.uphill$ws_i[3]
      
      ###get new K values and put in the table
      filled.df$seep_k_top[i] <- k.function(theta = final.values.uphill$final_theta[1], adj_theta_r = top_thetar, adj_theta_s = top_thetas, alpha_cm = top_alphacm,
                                            n = top_n, m = top_m, ksat = ksat_top)
      filled.df$seep_k_sub[i] <- k.function(theta = final.values.uphill$final_theta[2], adj_theta_r = sub_thetar, adj_theta_s = sub_thetas, alpha_cm = sub_alphacm,
                                            n = sub_n, m = sub_m, ksat = ksat_sub)
      filled.df$seep_k_bed[i] <- k.function(theta = final.values.uphill$final_theta[3], adj_theta_r = bed_thetar, adj_theta_s = bed_thetas, alpha_cm = bed_alphacm,
                                            n = bed_n, m = bed_m, ksat = ksat_bed)
      
      
      
    }
    
    
    
    
    
    
    ######################### Normal drainage for actual values of target location flux ####################################
    
    
    #####apply seepage to the soil first so that it can drain properly add to soil.df.daily and bound to theta_r and theta_s restrictions
    seep.applied <- seepage.applier(theta_topi = top_theta_p, theta_subi = sub_theta_p, theta_bedi = bed_theta_p, daily.soil.df = soil.df.daily ,
                                    seepage.input = seepage)
    
    
    #run drainage simulations on soil layers based on if they are saturated or not
    ##needs to be done for each soil layer (function created above)
    ### top layer drainage (also an input into the subsoil layer)
    ###pull from seep.applied table that has theta and ws after seepage has been applied to the layer
    
    drain_top_cm <- layer.drainage(layer_name = "top50", theta_i = seep.applied$theta_seep_added[1], ws_prev = seep.applied$ws_seep_added[1], soil.df = soil.df.daily)
    
    ##subsoil drainage (also an input into the bedrock layer layer)
    drain_sub_cm <- layer.drainage(layer_name = "lower_soil", theta_i = seep.applied$theta_seep_added[2], ws_prev = seep.applied$ws_seep_added[2], soil.df = soil.df.daily)
    
    ###bedrock drainage
    drain_bed_cm <- layer.drainage(layer_name = bed.name, theta_i = seep.applied$theta_seep_added[3], ws_prev = seep.applied$ws_seep_added[3], soil.df = soil.df.daily)
    
    ##### make table of the drainage values for each layer
    drain.table <- data.frame(hzname = c(soil.df$hzname[1], soil.df$hzname[2], soil.df$hzname[3]),
                              drain_pot = c(drain_top_cm, drain_sub_cm, drain_bed_cm),
                              ws_prev = c(seep.applied$ws_seep_added[1], seep.applied$ws_seep_added[2], seep.applied$ws_seep_added[3]))%>%
      mutate(drain_inputs = lag(drain_pot, default = 0))
    
    ###### put through the drainage update function before adding rain and other seepage values
    post.drainage <- drain_update(drain_table = drain.table, soilprofile = soil.df.daily)%>%
      mutate(theta_drain = new_ws/hz_depth)
    
    ###set the new theta for the top layer from the post.drainage table, used in rain infiltration calculations
    # theta.dr.top <- post.drainage$theta_drain[1]
    
    
    ######## infiltration and runoff calculations #################
    #calculate the theta_i for the 10 cm upper soil layer from the drained table values
    wsi_infilcm <- post.drainage$theta_drain[1] * infil_depth
    
    ####run the function to get the infil and runoff values for the target location of interest (not the uphill drainage)
    infil_run_target <- infiltration(ws_infil_i = wsi_infilcm, top_infil_ws = ws_topinfil_cm , rain_duration = filled.df$rain_dur_hrs[i],
                                     rain_rate = filled.df$adj_prate_cmhr[i], precip_mm = filled.df$adj_precip_mm[i],
                                     ksat_top_mmhr = ksat_top, slope_mod = soil.df$inf_ang_mod[1],soil_df = soil.df)
    
    ###infil inputs to target soil (runoff removal already factored in for this target spot)
    infil.input <- infil_run_target$infil_cm[1]
    
    
    ##### Runoff inputs from the uphill calculation already done above (runoff.downhill)
    ##if the slope of the target area is greater than 10% than the runoff does not accumulate in the target zone
    #if 10 % or less than the slope modifies the amount that remains on the surface to be infiltrated into it
    runoff.inputs <-  ifelse(slope.deg > 20, 0, (1-infil.angle.mod)*infil_run_target$runoff_cm) 
    
    ################ Calculating final values and adding them to the filled.df table ########
    ###now need to put it all together again
    ##need function for the  taking of all factors in
    ##runoff and infiltration of rain is applied to top soil layer, seepage is applied to the rest of the soil layers
    
    
    ####ned conductivity of top soil post drainage to get accurate soil evaporation for the day
    # k.top.post.drain <- k.function(theta = post.drainage$theta_drain[1], adj_theta_r = top_thetar, adj_theta_s = top_thetas, alpha_cm = top_alphacm,
    #            n = top_n, m = top_m, ksat = ksat_top)
    
    
    ##modify evap if the water content of previous day is less than the adj pwp value from the soil table
    #evap is 10% of PET when water stress ecountered by the plants
    evap.adj <- ifelse(post.drainage$theta_drain[1] > soil.df$theta_pwp[1], daily_evap, daily_evap*.10)
    
    ###get the current post.drainage ksat and multiply by 24 to get daily ksat for evaporation and compare
    # evap.adj <- ifelse(k.top.post.drain*24 >= daily_evap, daily_evap, k.top.post.drain*24 )
    
    ####get final inputs (seepage changed to zero since accounted for earlier)
    final.values <- final_inputs(seep_inputs = 0, infil_input = infil.input, runoff_inputs = runoff.inputs, post.drain.tab = post.drainage, evap = evap.adj,
                                 soil.df = soil.df.daily)
    
    ####now with the final values table update the filled.df table to reflect these changes in this iteration
    
    ####set the runoff value and the upper 10 cm water storage
    filled.df$runoff_target[i] <- infil_run_target$runoff_cm
    filled.df$ws_topinfil_cm[i] <- final.values$final_theta[1] * infil_depth
    
    
    #####with this table update the theta values, water storage values, and K values
    filled.df$theta_top_cm3cm3[i] <- final.values$final_theta[1]
    filled.df$theta_sub_cm3cm3[i] <- final.values$final_theta[2]
    filled.df$theta_bed_cm3cm3[i] <- final.values$final_theta[3]
    
    #new ws values
    filled.df$ws_top_cm[i] <- final.values$ws_i[1]
    filled.df$ws_sub_cm[i] <- final.values$ws_i[2]
    filled.df$ws_bed_cm[i] <- final.values$ws_i[3]
    
    ###get new K values and put in the table
    filled.df$k_top_mmhr[i] <- k.function(theta = final.values$final_theta[1], adj_theta_r = top_thetar, adj_theta_s = top_thetas, alpha_cm = top_alphacm,
                                          n = top_n, m = top_m, ksat = ksat_top)
    filled.df$k_sub_mmhr[i] <- k.function(theta = final.values$final_theta[2], adj_theta_r = sub_thetar, adj_theta_s = sub_thetas, alpha_cm = sub_alphacm,
                                          n = sub_n, m = sub_m, ksat = ksat_sub)
    filled.df$k_bed_mmhr[i] <- k.function(theta = final.values$final_theta[3], adj_theta_r = bed_thetar, adj_theta_s = bed_thetas, alpha_cm = bed_alphacm,
                                          n = bed_n, m = bed_m, ksat = ksat_bed)
    ####################################
    
    ####withprogress call for shiny app progress bar
    if (is.function(updateProgress)) {
      text <- paste0("Day:", i)
      updateProgress(detail = text)
    }
    
    ###set the progress bar
    setTxtProgressBar(progress_bar, value = i)
    
    
    
    
  }
  
  #need to scale the theta_r adjustment based on the theta_r of the top layer
  #minimum set to 0.01% of the total water content
  thetar_mod <- (top_thetas - top_thetar)*0.0001
  
  ###calculate water potentials for each layer
  filled.df.psi <- filled.df%>%
    mutate(theta_top_cm3cm3 = ifelse(theta_top_cm3cm3 < top_thetar + thetar_mod, top_thetar + thetar_mod, theta_top_cm3cm3))%>% ###don't want it going to negative
    mutate(psi_top_cm = vg.pot.conversion(theta = theta_top_cm3cm3, theta_r = top_thetar, theta_s = top_thetas,
                                          alpha = top_alphacm, n = top_n, m = top_m),
           paw_top_cm = ifelse((theta_top_cm3cm3 - soil.df$theta_pwp[1]) <0,0,(theta_top_cm3cm3 - soil.df$theta_pwp[1])*50 ))
  ###testing
  # vg.pot.conversion(theta = 0.08900000, theta_r = 0.089, theta_s = 0.319, alpha = 0.0268000000, n = 1.230000)
  
  close(progress_bar)
  return(filled.df.psi)
  
}

###testing pot.conversion function
# vg.pot.conversion(theta = 0.07201, theta_r = 0.072, theta_s = 0.275, alpha = 0.0278, n = 1.24)

#######
########################## NRCS data import and processing functions #####################################


##assign NRCS classification to our averaged profiles (updated: 3/18/2024)
nrcs <- function(soil.table){
  
  #testing
  # soil.table <- soil.no.o.r
  
  
  #soiltexture package requires a table of just the texture data (sand silt clay and organic matter)
  tri.table <- data.frame(CLAY = soil.table$claytotal_r,
                          SILT = soil.table$silttotal_r,
                          SAND = soil.table$sandtotal_r,
                          OC = soil.table$om_r)
  
  
  ###now put throught the function and specify the system classification to use
  classes.table <- TT.points.in.classes(tri.data = tri.table, class.sys = "USDA.TT") ##issue is that the percentage sums need to be close to 100%
  
  if (any(classes.table == 2 | classes.table == 3)) {
    
    # edge.cases <- as.data.frame(classes.table) %>% filter_all(any_vars(. %in% c(2)))
    edge.index <- data.frame(which(classes.table == 2|classes.table == 3, arr.ind = T))
    
    for (l in 1:length(unique(edge.index$row))) {
      
      ##testing
      # l <- 1
      
      ####subset the edge case indices
      case1 <- subset(edge.index, row == unique(edge.index$row)[l])
      
      ##row num for this first issue case
      row <- unique(edge.index$row)[l]
      
      ##in some cases it can be on edge case but not have another category that it might belong too (extremely sandy soils)
      
      if (length(case1$row) > 1) {
        
        ##change one of the values to zero and the other to 1  
        classes.table[row,case1$col[1]] <- 0
        classes.table[row,case1$col[2]] <- 1
        
      } else{
        
        ##if on edge but no other possible soil, just change it to 1
        classes.table[row,case1$col[1]] <- 1
        
      }
      
      
      
    } 
    
    
  } else {
    
    classes.table <- classes.table
  }
  
  ##plot the points to see how far off they are of each 
  # TT.plot(class.sys = "USDA.TT", tri.data = tri.table)
  
  #return the thing
  return (classes.table)
  
}

#updated 5/7/2024 ignores quarry and other strange soil types
soil.data.retriever <- function(lat,long){
  
  require(soilDB)
  require(apsimx)
  
  ###lat long for testing normal
  # lat <- 40.39970
  # long <- -74.84469
  
  ###lat long for Lewis property
  #lat <- 41.02461
  #long = -74.89723
  
  ###sandy/beach locations (used as Dune Land data as well)
  # lat <- 38.94953
  # long <- -74.85806
  
  ### urban land (no data), udorthents (no data) 
  # lat <- 40.53854 
  # long <- -79.76527
  
  # ##sand/beach with urband land component (want the urband land to be removed)
  # lat <- 38.96419
  # long <- -74.83812
  
  ##example missing some components, function seems to handle this well
  # #38.72708,-76.07525
  # lat <- 38.72708
  # long <- -76.07525
  
  ######### dune lands point
  # 41.659946, -87.068927
  # lat <- 41.659946
  # long <- -87.068927
  
  # "Other soils" component issue
  #38.931272, -78.942090
  # lat <- 38.931272
  # long <- -78.942090
  
  ###gilgo beach issues
  #40.636233, -73.319018
  # lat <- 40.636233
  # long <- -73.319018
  
  ###QUIL problem point without ksat_mm_day output
  # lat <- 41.07868
  # long <- -77.08996
  
  ###rickettsglen
  # lat <- 41.30843
  # long <- -76.30951
  
  # ##testing
  # lat <- lat
  # long <- long
  
  
  #sql query that is submitted to the SSURGO database, here we specify data we need and the codes required to get it
  q.1 <- paste("SELECT legend.areasymbol, mapunit.mukey, mapunit.muname, brockdepmin, wtdepannmin, component.cokey, component.comppct_r, component.compname, chorizon.hzname, chorizon.chkey, chorizon.hzdept_r, chorizon.hzdepb_r, chorizon.ksat_r, chorizon.awc_r, chorizon.sandtotal_r, chorizon.silttotal_r, chorizon.claytotal_r, chorizon.om_r,dbovendry_r, hydricrating, geomdesc,  ph1to1h2o_r
  FROM legend
  INNER JOIN mapunit ON mapunit.lkey = legend.lkey
  LEFT JOIN muaggatt ON muaggatt.mukey = mapunit.mukey
  LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
  LEFT JOIN chorizon ON component.cokey = chorizon.cokey
  WHERE mapunit.mukey IN (
  SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('point(", paste(long, lat, sep = " "),")'))"
  )
  
  
  ###bring in data
  co.1 <- SDA_query(q.1) ###here we can atleast get some of the data for the udothents or something
  
  
  #print the na check, are there any components that are NA values, indicating missing data
  print(paste("Missing component data:", any(is.na(co.1$hzname))))
  
  
  #subset out only the na portion
  #get na comps this is to figure out if there are known components which data we could supplement
  na.comps <- co.1[is.na(co.1$hzname),] ##check based on horizon name, if its NA, no data present in mu
  
  ##check for urban soils and print out warning if they are present
  if ("Urban land" %in% co.1$compname | "Udorthents" %in% co.1$compname) {
    print("WARNING: Urban modification to soil present. Soils data may require in person soil survey. Urban components will be ignored in this analysis")
  } else {
    print("No urban soils")
  }
  
  
  ##check for presence of Dune land, this is not removed but a placeholder soil horizon is used that is based on sandy coastal soils
  if ("Dune land" %in% co.1$compname) {
    print("WARNING: DUNE LAND DATA PRESENT and IS NA. HOOKSAN SOIL SERIES DATA USED INSTEAD")
  } 
  
  
  
  ##remove unusable components now to test if there are some missing we might have use for
  
  ##subset out any Urban Land, Rock outcrop, Rubble land which we don't want
  na.comps2 <- na.comps%>%
    subset(compname != "Rock outcrop")%>% #remove because this now has another way of being described
    subset(compname != "Rock outcrops")%>%
    subset(compname != "Urban land") %>%
    subset(compname != "Rubble land") %>%
    subset(compname != "Other soils")%>%
    #subset(compname != "Dune land") %>% this gets substituted so keep it in.
    subset(compname != "Unnamed soils")%>%
    subset(compname != "Unnamed")%>%
    subset(compname != "Udorthents")%>%
    subset(compname != "Bedrock at 10 to 20 inches")
  
  ##na.comps2 removes any of the zero data stuff, so if length of 0 than no missing components that can be recovered
  
  ##now print out if missing data is recoverable, if na.comps2 has no length than it isn't and all data is NA
  #is it all NA or just a few components
  print(paste("All components missing?:", length(na.omit(co.1$hzname)) == 0))
  
  ##print out the missing components from left overs of na.comps
  print(paste("Missing Components:", paste(c(unique(na.comps2$compname)), collapse = ", "), sep = " "))
  
  ###remove any straggler Urban lands that for some reason do have a horizon name
  #these components may sometimes have a hzname present but it does not contain any soils data we need for the model
  co <- co.1%>%
    subset(compname != "Urban land")
  
  #create empty list for final data
  all.mu.comps.complete <- list()
  
  
  
  ##conditionally fix data if components are missing and the components can have data recovered for them
  if (any(is.na(co$hzname)) & length(na.comps2$compname) > 0) { ###test if there is any na in the hzname column and if the na.comps table has data in it
    
    
    ##test if all component data is missing
    if (length(na.omit(co$hzname)) == 0) { ##this means that either all horizon data lacks data (Urban lands, sand dunes) or main component lacks data (e.g. urban lands), and minor lacks, but may have similar data somewhere
      
      ###if you subset out the other soils and urban, if there isn't anything left then just return "Components Lack data"
      #subset out the dunelands, urban soils, and other soils. These components will never have data associated with them
      no.urb.others <- co %>%
        subset(compname != "Urban land")%>%
        #subset(compname != "Dune land")%>%
        subset(compname != "Other soils")
      
      if (length(no.urb.others) == 0) { #if when urban and "other soils" is subsetted out there are no components left, than no data exists
        
        print("Components present lack data (e.g. Urban or Other Soils). Data = NA")
        
        # make it equal null or something
        all.mu.comps.complete <- NA
        
      } else if (no.urb.others$compname == "Dune land") { ##now if the location is Dune land then return the substitute dataset for the dunes soils ("dunelands")
        
        ###make the table for the Dunelands dataset
        dunelands <- data.frame(areasymbol = c("NJ009", "NJ009"),
                                mukey = c(825249, 825249),
                                muname = c("Hooksan sand, 2 to 15 percent slopes, rarely flooded",'Hooksan sand, 2 to 15 percent slopes, rarely flooded'),
                                brockdepmin = c(NA,NA),
                                wtdepannmin = c(228, 228),
                                cokey = c(22414698,22414698),
                                comppct_r = c(100,100),
                                compname = c("Hooksan","Hooksan"),
                                hzname = c("A", "C"),
                                chkey = c(66253922,66253921),
                                hzdept_r =  c(0,15),
                                hzdepb_r = c(15,228),
                                ksat_r = c(91.74, 91.74),
                                awc_r = c(0.05, 0.02),
                                sandtotal_r = c(95.5, 96.3),
                                silttotal_r = c(1.5, 0.7),
                                claytotal_r = c(3,3),
                                om_r = c(0.75, 0.25),
                                hydricrating = c("No", "No"),
                                geomdesc = c("dunes on barrier islands on coastal plains","dunes on barrier islands on coastal plains"),
                                ph1to1h2o_r = c(5.8,5.8),
                                fragvol_r = c(1,4),
                                ksat = c(330.264, 330.264),
                                ksat_mm_day = c(7926.336, 7926.336),
                                pct_component = c(100,100),
                                perc_hydric = c(0,0))
        
        ##make the output all.mu.comps.complete equal to the dunelands table
        all.mu.comps.complete <- dunelands
        
      } else { ###if all component data is missing but some may be recoverable (based on compname we can look for similar soils data in the survey area)
        
        #make list of the components without data
        ##list of missing components we need to find
        miss.comps.minor <- unique(no.urb.others$compname)
        
        
        ##find the minor component data present within the study area
        q.minor.sa <- paste("SELECT 
        SELECT legend.areasymbol, mapunit.mukey, mapunit.muname, brockdepmin, wtdepannmin, component.cokey, component.comppct_r, component.compname, chorizon.hzname, chorizon.chkey, chorizon.hzdept_r, chorizon.hzdepb_r, chorizon.ksat_r, chorizon.awc_r, chorizon.sandtotal_r, chorizon.silttotal_r, chorizon.claytotal_r, chorizon.om_r,dbovendry_r, hydricrating, geomdesc,  ph1to1h2o_r
        FROM legend
        INNER JOIN mapunit ON mapunit.lkey = legend.lkey
        LEFT JOIN muaggatt ON muaggatt.mukey = mapunit.mukey
        LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
        LEFT JOIN chorizon ON component.cokey = chorizon.cokey
        WHERE legend.areasymbol = ", "'",co$areasymbol[1],"'", sep = "") ###the area symbol must be supplied inside quotes when in the query (other one doesn't), sep = "" so no space between quotes and symbol
        
        
        
        
        ##find frag volume    
        q.minor.sa.frag <- paste("SELECT 
        legend.areasymbol, component.mukey, component.cokey, component.compname, chorizon.hzname, chorizon.chkey, chfrags.fragvol_r
        FROM legend
        INNER JOIN mapunit ON mapunit.lkey = legend.lkey
        LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
        LEFT JOIN chorizon ON component.cokey = chorizon.cokey
        LEFT JOIN chfrags ON chorizon.chkey = chfrags.chkey
        WHERE legend.areasymbol = ", "'",co$areasymbol[1],"'", sep = "")
        
        ###queried data table of the survery area with components and important data
        co.minor.sa <- SDA_query(q.minor.sa) ##retrieves the entire survey area data
        
        co.minor.sa.frag <- SDA_query(q.minor.sa.frag) %>%
          group_by(chkey)%>%
          summarize(fragvol_r = sum(fragvol_r))
        
        ##left join back to co, gets all the data from survye area, 
        co.minor.sa.complete <- left_join(co.minor.sa, co.minor.sa.frag, by = "chkey")
        
        
        ##select only the minor comps of interest
        ##only select the components that we care about that are the ones without data present
        area.minor.comp.select <- co.minor.sa.complete[co.minor.sa.complete$compname %in% miss.comps.minor,]
        
        ###need to run another if statement
        #if the component does not have similar data in the entire survey area (is possible) than there is no data at all. In this case chkey will be empty again and judge on that
        
        if (length(na.omit(area.minor.comp.select$chkey)) == 0 ) { ##base it on horizon key because if this is not present than no data at all
          #its seems in case of Udorthents the sites with 100% of component can have no data, may need to work around this
          #need to subset out data from area.minor subset than is na, but only in the ksat and aws columns
          
          ##state whats wrong and why this isn't working
          print("Major components present lack data (e.g. Urban or Dune Lands) and minor components have no similar data available in this survey area. Data = NA")
          
          # <- make it equal null or something
          all.mu.comps.complete <- NA
          
        } else {
          
          #remove any components that were found that have NA data in the key columns of ksat and aws
          area.minor.comp.select2 <- area.minor.comp.select[!is.na(area.minor.comp.select$ksat_r),]
          area.minor.comp.select3 <- area.minor.comp.select2[!is.na(area.minor.comp.select2$ksat_r),]
          
          ###Now iterate through and pick the ones where each grouping has a majority
          ##empty data
          comp.final.missing <- list() ##put final component horizon data here and combine it back with the other data
          
          for (i in 1:length(miss.comps.minor)) {
            
            ###subset the component name of interest
            comp.miss.sub <- area.minor.comp.select2 %>%
              subset(compname == miss.comps.minor[i])
            
            ##take the max of the percentages
            max <- max(comp.miss.sub$comppct_r)
            
            ##subset out the max data
            comp.major <- comp.miss.sub %>%
              subset(comppct_r == max)
            
            ##in some cases there are two mapunits, just pull from one by taking mapunit of first one (they will be different)
            comp.maj.sub <- comp.major%>%
              subset(mukey == comp.major$mukey[1])
            
            ##append to comp.final.missing
            comp.final.missing <- rbind(comp.final.missing, comp.maj.sub)
            
            
            
          }
          
          ####now we need to put it back together with the main one that may have contained undefined major comp and these target minor comps
          #add on frag_vol column to match this comp.final.missing, this was missing because can't get frag volume when chkey is NA
          co$fragvol_r <- NA
          
          #remove co.completes NA placeholders of our target horizons (do based on the horizon names of the minor components we could find data for)
          #first remove these from the co
          co.ph.rem <- co[!(co$compname %in% miss.comps.minor),]
          
          ##remove the percentages from the comp.final.missing
          comp.final.missing$comppct_r <- NULL
          
          ##trim co.complete to get the component percentages
          pcts <- co %>%
            select(comppct_r, compname)%>%
            unique() ##need to give unique because co.complete is split into the horizons and replicates the percentage so it would duplicate the final table
          
          ##left join to all.mu.comps
          all.mu.comps.final <- left_join(comp.final.missing, pcts, by = "compname")%>%
            arrange(compname, hzdept_r) %>% #arrange in readable order for comparisons
            mutate(ksat = ksat_r *3.6,pct_component = sum(pcts$comppct_r)) ##convert ksat to UCdavis metric, and calculate total component percents
          
          #bind back to the greater one and we get our final output
          all.mu.comps.complete <- rbind(all.mu.comps.complete, all.mu.comps.final)%>%
            mutate(brockdepmin = min(!is.na(brockdepmin)))
          
          
          
        }
      }
      
      
      
    }else { ##this else statement is for when major component data is present but minor components are missing
      
      
      
      ###### do the same addition of fragment info as below
      #this par gets the frag volume for the horizons present, doesn't work if all horizons missing chkey
      q.2.frag <- paste("SELECT legend.areasymbol, component.mukey, component.cokey, component.comppct_r, component.compname, chorizon.hzname, chorizon.chkey, chorizon.hzdept_r, chorizon.hzdepb_r, chfrags.fragvol_r
      FROM legend
      INNER JOIN mapunit ON mapunit.lkey = legend.lkey
      LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
      LEFT JOIN chorizon ON component.cokey = chorizon.cokey
      LEFT JOIN chfrags ON chorizon.chkey = chfrags.chkey
      WHERE mapunit.mukey = ", co$mukey[1] )
      
      #bring in frag data
      co.frag <- SDA_query(q.2.frag) %>%
        group_by(chkey)%>% ###data missing all data will not have a horizon key (chkey), this prevents the function from continuing here
        summarize(fragvol_r = sum(fragvol_r))
      
      #remove extra mukey
      
      ##left join back to co
      co.complete <- left_join(co, co.frag, by = "chkey") ##need to bind to specific horizons give horizon key
      
      ##list of missing components we need to find
      miss.comps <- unique(na.comps2$compname)
      
      
      ######paste together query the areasymbol from the mapunit data table we got from the gps points
      q.sa <- paste("SELECT legend.areasymbol, mapunit.mukey, mapunit.muname, brockdepmin, wtdepannmin, component.cokey, component.comppct_r, component.compname, chorizon.hzname, chorizon.chkey, chorizon.hzdept_r, chorizon.hzdepb_r, chorizon.ksat_r, chorizon.awc_r, chorizon.sandtotal_r, chorizon.silttotal_r, chorizon.claytotal_r, chorizon.om_r,dbovendry_r, hydricrating, geomdesc,  ph1to1h2o_r
      FROM legend
      INNER JOIN mapunit ON mapunit.lkey = legend.lkey
      LEFT JOIN muaggatt ON muaggatt.mukey = mapunit.mukey
      LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
      LEFT JOIN chorizon ON component.cokey = chorizon.cokey
      WHERE legend.areasymbol = ", "'",co.complete$areasymbol[1],"'", sep = "") ###the mapunit symbol must be supplied inside quotes when in the query (other one doesn't), sep = "" so no space between quotes and symbol
      
      #get frag volume
      q.sa.frag <- paste("SELECT legend.areasymbol, component.mukey, muaggatt.brockdepmin, component.cokey, chorizon.hzname, chorizon.chkey, chfrags.fragvol_r
      FROM legend
      INNER JOIN mapunit ON mapunit.lkey = legend.lkey
      LEFT JOIN muaggatt ON muaggatt.mukey = mapunit.mukey
      LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
      LEFT JOIN chorizon ON component.cokey = chorizon.cokey
      LEFT JOIN chfrags ON chorizon.chkey = chfrags.chkey
      WHERE legend.areasymbol = ", "'",co.complete$areasymbol[1],"'", sep = "")
      
      
      ###queried data table of the survey area with components and important data
      co.sa <- SDA_query(q.sa) ##retrieves the entire survey area data
      ###need another conditional if can't find any data
      co.sa.frag <- SDA_query(q.sa.frag) %>%
        group_by(chkey)%>%
        summarize(fragvol_r = sum(fragvol_r))
      
      ##left join back to co
      co.sa.complete <- left_join(co.sa, co.sa.frag, by = "chkey")
      
      
      
      ##only select the components that we care about
      area.comps.select <- co.sa.complete[co.sa.complete$compname %in% miss.comps,]
      
      
      ##subset out based on horizon depth because sometimes this is present
      area.comps.select2 <- area.comps.select[!is.na(area.comps.select$hzdepb_r),]
      
      ###in cases where the same component name is both a major and minor component (in one example Lehew is major and present, while also a minor component of Lehew is missing)
      # because the code now just takes the highest unqiue one it could accidentally duplicate, to prevent this select to remove any coKeys present in co.complete
      area.comps.select3 <- area.comps.select2[!(area.comps.select2$cokey %in% co.complete$cokey),]
      
      
      ###Now iterate through and pick the ones where each grouping has a majority
      ##empty data
      comp.final.missing <- list() ##put final component horizon data here and combine it back with the other data
      
      for (i in 1:length(miss.comps)) {
        
        ###subset the component name of interest
        comp.miss.sub <- area.comps.select3 %>%
          subset(compname == miss.comps[i])
        
        ##take the max of the percentages
        max <- max(comp.miss.sub$comppct_r)
        
        ##subset out the max data
        comp.major <- comp.miss.sub %>%
          subset(comppct_r == max)
        
        ##in some cases there are two mapunits, just pull from one by taking mapunit of first one (they will be different)
        comp.maj.sub <- comp.major%>%
          subset(mukey == comp.major$mukey[1])
        
        ##append to comp.final.missing
        comp.final.missing <- rbind(comp.final.missing, comp.maj.sub)
        
      }
      
      ###now bind in together with co.complete's horizon data 
      
      
      #remove co.completes NA placeholders of our target horizons (do based on the horizon name again)
      #also reformat so columns in right order
      co.comps.narem <- subset(co.complete, !is.na(hzname))%>% #keeps the present horizons that have data
        mutate(similar_data_used = "FALSE")
      
      #add similar_data_used column to NA components data
      ##need to also get there percentages from the original
      comp.final.missing1 <- comp.final.missing %>%
        mutate(similar_data_used = "TRUE")
      
      #remove comppct_r since we will replace with values from our target map unit
      comp.final.missing1$comppct_r <- NULL
      
      ##since in some rare cases the minor components can be the same name as a major component we need to match up the NA comp_pct separately
      pcts.na <- co.complete[is.na(co.complete$hzname),]%>%
        select(comppct_r, compname)
      
      
      ##join back together and correct for the column order
      comp.final.missing2 <- left_join(comp.final.missing1, pcts.na, by = "compname")
      
      ###make it so no matter the length of the table the comppct is placed in the right spot and all other variables are retained
      #get the column number that has the comppct, make sure to make it numeric or else R thinks its a vector
      col.num <- as.numeric(which(colnames(comp.final.missing2) == "comppct_r")[1])
      
      ##### put the columns in the correct order using select
      ###set the water table depth to that of the focal soil series and not the one from borrowed data
      comp.final.missing3 <- comp.final.missing2%>%
        select(1:7, col.num ,8:(col.num -1))%>%
        mutate(wtdepannmin = co.comps.narem$wtdepannmin[1]) 
      
      
      # ##need to remove percentages from the components since they are not correct
      # all.mu.comps <- rbind(co.comps.narem, comp.final.missing)%>% ##issue is that the comp.pcts are not accurate (taking from where minor comps are major components)
      #   mutate(comppct_r = NULL) ##remove percentages for now and then recombine them
      # 
      # 
      # ##trim co.complete to get the component percentages
      # pcts <- co.complete %>%
      #   select(comppct_r, compname)%>%
      #   unique() ##need to give unique because co.complete is split into the horizons and replicates the percentage so it would duplicate the final table
      # 
      ##left join to all.mu.comps
      # all.mu.comps.final <- left_join(all.mu.comps, pcts, by = "compname")%>%
      #   arrange(compname, hzdept_r) %>% #arrange in readable order for comparisons
      #   mutate(ksat = ksat_r *3.6,pct_component = sum(pcts$comppct_r)) ##convert ksat to UCdavis metric, and calculate total component percents
      
      
      
      #bind back to the greater one
      all.mu.comps.complete <- rbind(co.comps.narem, comp.final.missing3)%>%
        arrange(comppct_r,compname, hzdept_r) %>% 
        mutate(ksat = ksat_r *3.6, ksat_mm_day = ksat_r*3.6*24) ##to get total component make up percentage just sum from within 
      #mutate(brockdepmin = min(!is.na(brockdepmin))) ###adjust the bedrock minimum for the entire map unit based on the actual minimum throughout
      
      ##################### get hydric percents #########################
      #need to subset out and sum the hyrdic soils
      hydric.perc <- all.mu.comps.complete %>%
        group_by(compname,comppct_r, hydricrating) %>%
        summarize(comppct_2 = mean(comppct_r)) %>%
        subset(hydricrating == "Yes")
      
      
      
      
      ###calculate the total components percentage (not always 100%)
      percents <- all.mu.comps.complete %>%
        group_by(mukey,compname, cokey)%>%
        summarize(pct_component = mean(comppct_r))
      # group_by()%>%
      # summarize(pct_component_total = sum(pct_component))
      
      ##readjust minimum bedrock depth and total the comppct_r into the pct_component
      ##need an if statement to get this right
      
      if (min(all.mu.comps.complete$brockdepmin[!is.na(all.mu.comps.complete$brockdepmin)]) == Inf) {
        brockdepmin_val <- NA
        
      } else {
        brockdepmin_val <- min(all.mu.comps.complete$brockdepmin[!is.na(all.mu.comps.complete$brockdepmin)])
      }
      
      ###add the value to the final table
      all.mu.comps.complete$brockdepmin <- brockdepmin_val
      
      
      ###calculate the hydric and pct_components
      all.mu.comps.complete$pct_component = sum(percents$pct_component)
      all.mu.comps.complete$perc_hydric = sum(hydric.perc$comppct_2)
      
      
      
      
    }
    ##if statement bracket
    
  } else if (all(is.na(co$hzname)) & length(na.comps2$compname) == 0) { ##if there are NA components, but none that can be recovered not data available
    
    
    #make it equal null or something
    all.mu.comps.complete <- NA
    
    
    
  }else{ ##################################### BRACKET FOR NO ISSUES OF MISSING DATA
    
    ##if no issue bring in fragment volumes for the data as well
    #frag query
    q.2.frag <- paste("SELECT 
    component.cokey, component.compname, chorizon.hzname, chorizon.chkey, chorizon.hzdepb_r, chfrags.fragvol_r
    FROM legend
    INNER JOIN mapunit ON mapunit.lkey = legend.lkey
    LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
    LEFT JOIN chorizon ON component.cokey = chorizon.cokey
    LEFT JOIN chfrags ON chorizon.chkey = chfrags.chkey
    WHERE mapunit.mukey = ", co$mukey[1] )
    
    
    
    #bring in frag data
    co.frag <- SDA_query(q.2.frag) %>%
      group_by(chkey)%>%
      summarize(fragvol_r = sum(fragvol_r))
    
    ##left join back to co
    co.complete <- left_join(co, co.frag, by = "chkey")
    
    ##remove any urban lands and other types that don't have sand silt clay amounts
    co.complete.na.rem <- co.complete %>%
      subset(compname != "Urban land") %>%
      subset(compname != "Rubble land") %>%
      subset(compname != "Other soils")%>%
      subset(compname != "Dune land") %>%
      subset(compname != "Rock outcrop")%>%
      subset(compname != "Unnamed")%>%
      subset(compname != "Unnamed soils")%>%
      subset(compname != "Udorthents")
    
    #put in the final dataset to be kept for the component data
    all.mu.comps.complete <- co.complete.na.rem%>% ##no need to rbind, can just rename co.complete
      arrange(compname, hzdept_r)%>%
      mutate(ksat = ksat_r *3.6)%>%
      mutate(ksat_mm_day = ksat_r*86.4)
    
    
    
    ###calculate the total components percentage (not always 100%)
    percents <- all.mu.comps.complete %>%
      group_by(cokey)%>% 
      summarize(pct_component = mean(comppct_r))
    
    pct_component_sum <- sum(percents$pct_component)
    
    ##calculate the percent hydric rating
    hydric.perc <- all.mu.comps.complete %>%
      group_by(compname, cokey,comppct_r, hydricrating) %>%
      summarize(comppct_2 = mean(comppct_r)) %>%
      subset(hydricrating == "Yes")
    
    #join back in
    all.mu.comps.complete <- left_join(all.mu.comps.complete, percents, by = 'cokey') %>%
      mutate(pct_component = pct_component_sum,perc_hydric = sum(hydric.perc$comppct_2))
    
    
    #else bracket
  }
  
  # ###Mark the bedrock layers if they are present
  # ##need to first subset out the bedrock
  # ##first get the maximum depth for each component type.
  # soil.max.depths <- all.mu.comps.complete%>%
  #   group_by(compname,comppct_r)%>% #group by component and the percentage
  #   summarize(max_depth = max(hzdepb_r)) %>%
  #   select(compname, max_depth)
  # 
  # ##left join back to the original table, now we can determine if horizons contain the bedrock layer
  # soil.table2 <- left_join(all.mu.comps.complete, soil.max.depths, by = "compname")
  # 
  # #check if bedrock present will have deepest layer, and 
  # soil.table2[is.na(soil.table2$om_r) & soil.table2$hzdepb_r == soil.table2$max_depth, "hzname"] <- "R"
  # 
  # ##the organic soils seems to cause issues since it has na values for most other soil components
  # #these layers can be determined by finding if sand silt or clay are na and the OM is present !is.na()
  # #if O horizon turn the NAs of sand, silt, and clay to 0, and put the bulk density to 0.3 grams per centimeter based on info from:
  # # https://www.sciencedirect.com/topics/earth-and-planetary-sciences/bulk-density#:~:text=In%20organic%20soils%2C%20bulk%20density,%E2%80%931.5%20g%20cm%E2%88%92%203.
  # # mineral components estimation based on the description of CATDEN series (41.00231,-75.06274), "less than 5% mineral" these I split evenly.
  # 
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$sandtotal_r), "sandtotal_r"] <- 1.66
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$silttotal_r), "silttotal_r"] <- 1.66
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$claytotal_r), "claytotal_r"] <- 1.66
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$fragvol_r), "fragvol_r"] <- 0
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$dbovendry_r), "dbovendry_r"] <- 0.3
  
  # all.mu.comps.complete <- soil.table2
  
  return(all.mu.comps.complete)
  
  
  
  #function bracket 
}

##separate function for marking bedrock layers, organic horizons and matching the textural classes
## this is going to be used to help obtain the VG parameters

###new classify function (5/17/2024), lindside added to missing silt list
classify.horizons <- function (retrieved.table){
  
  ##testing
  # retrieved.table <- soil.retrieved
  
  
  ####
  
  #################### Below code is old way of finding bedrock #############
  ###Mark the bedrock layers if they are present
  ##need to first subset out the bedrock
  ##first get the maximum depth for each component type.
  # soil.max.depths <- retrieved.table%>%
  #   group_by(compname,comppct_r)%>% #group by component and the percentage
  #   summarize(max_depth = max(hzdepb_r)) %>%
  #   select(compname, max_depth)
  # 
  # ##left join back to the original table, now we can determine if horizons contain the bedrock layer
  # soil.table2 <- left_join(retrieved.table, soil.max.depths, by = "compname")
  
  #check if bedrock present will have deepest layer, and
  #soil.table2[is.na(soil.table2$om_r) & soil.table2$hzdepb_r == soil.table2$max_depth, "hzname"] <- "R"
  
  ###########################New Way of defining bedrock ##################
  ##in some cases Cr horizon is present which is defined as a bedrock layer technically
  #but it is not sitting at the max horizon depth. So instead we go based on if there is missing
  #data in all the categories other than the Ksat, which will often still be listed for bedrock
  
  ##rename table
  soil.table2 <- retrieved.table
  
  #sometimes a Cr layer is present which we will also just treat as bedrock, it does have slightly different properties
  # 2R bedrock layers are sometimes present as well which need to be removed cause they sometimes have awc data even though its zero
  #but no data is present
  soil.table2[is.na(soil.table2$om_r) & (is.na(soil.table2$awc_r)|soil.table2$awc_r == 0) & is.na(soil.table2$sandtotal_r) & is.na(soil.table2$silttotal_r) & is.na(soil.table2$claytotal_r) & is.na(soil.table2$fragvol_r), "hzname"] <- "R"
  soil.table2[soil.table2$hzname == "2R", "hzname"] <- "R"
  
  ###there are also cases where a 2Cg layer is which sometimes will have missing values of sand and silt, need to adjust for this
  ##2C horizons are those which differe from other soils in pedon and are not result of similar weathering processes
  ##in the soils on tohickon creek they have an underlying layer of gravel and sand that composes their 2Cg which is quickly drained
  #these layers seem to have partial data available for some reason
  ##make list of the components that have this problem
  problem.list <- c("Bowmansville", "Knauers", "Rowland",'Gilpin','Rayne')
  ##missing silt loam types
  problem.list2 <- c('Gilpin','Rayne', 'Philo', "Ernest", "Hartleton", "Berks", 'Lindside', "Bedington", 'Blairton')
  
  problem.list3 <- c("Barbour")##missing bedrock horizon with om data for some reason
  problem.list4 <- c('Laidig')
  problem.list5 <- c("Hatboro",'Wurtsboro') ## C horizon can be missing sand and silt data, in description its a Gravelley sand sublayer so make it very sandy
  
  ######## fixing soils missing sand and silt data where soils are very sandy often only clay content is given
  sand.list <- c("Wurtsboro","Conotton", 'Hazleton', 'Leck Kill', "Weikert")
  
  
  ###soils missing ksat values for glacial till bedrock layers (Farmington: 0.036, Galway: 0.774)
  glacial.till.list <- c("Farmington","Galway")
  
  
  ####master problem list 
  # mast.problems <- c(problem.list, problem.list2, problem.list3, problem.list4, problem.list5, sand.list, glacial.till.list)
  
  ############## Other edge cases where bedrock data is missing but rest of the component data is present ##########
  
  ###Modifying for Hartleton
  ###hand modify for certain soil types since they are very difficult to do in mutate due to specific cases of dataloss
  
  soil.table2[soil.table2$compname == "Hartleton" & is.na(soil.table2$ksat) & is.na(soil.table2$ksat) & is.na(soil.table2$sandtotal_r) & is.na(soil.table2$silttotal_r) & is.na(soil.table2$claytotal_r) & is.na(soil.table2$fragvol_r), "hzname"] <- "R"
  soil.table2[soil.table2$compname == "Hartleton" & soil.table2$hzname == "R" & is.na(soil.table2$ksat), "ksat_r"] <- 5
  
  # if (length(soil.table2[soil.table2$compname == "Hartleton" & soil.table2$hzname == "R" & is.na(soil.table2$ksat),]) > 0) {
  #   #get the index of the row with the values in questions, which() gives the index for the row we are looking for
  #   index <- which(soil.table2$compname == "Hartleton" & soil.table2$hzname == "R" & is.na(soil.table2$ksat_r) & is.na(soil.table2$awc_r))
  #   
  #   ##change all the values at once to what we care about
  #   soil.table2$hzname[index] <- "R"
  #   soil.table2$ksat_r[index] <- 18
  #   soil.table2$ksat[index] <- 18*3.6
  #   soil.table2$ksat_mm_day[index] <- 18*3.6*24
  #   
  # } 
  
  if (any(soil.table2$compname == "Cookport"  & is.na(soil.table2$ksat_r) & is.na(soil.table2$awc_r))) {
    #get the index of the row with the values in questions, which() gives the index for the row we are looking for
    index <- which(soil.table2$compname == "Cookport" & is.na(soil.table2$ksat_r) & is.na(soil.table2$awc_r))
    
    ##change all the values at once to what we care about
    soil.table2$hzname[index] <- "R"
    soil.table2$ksat_r[index] <- 1.41
    soil.table2$ksat[index] <- 1.41*3.6
    soil.table2$ksat_mm_day[index] <- 1.41*3.6*24
    
  } 
  
  ###if there are any horizons where two of the texture components are present subtract them to get the last component
  soil.table2$text_nacounts <- rowSums(is.na(soil.table2[,c("sandtotal_r", "silttotal_r", "claytotal_r")]))
  
  
  ###other spot modifications
  soil.table2b <- soil.table2%>%
    mutate(sandtotal_r = ifelse(text_nacounts == 1 & is.na(sandtotal_r), (100-silttotal_r-claytotal_r), sandtotal_r),
           silttotal_r = ifelse(text_nacounts == 1 & is.na(silttotal_r), (100-sandtotal_r-claytotal_r), silttotal_r),
           claytotal_r = ifelse(text_nacounts == 1 & is.na(claytotal_r), (100-sandtotal_r-silttotal_r), claytotal_r))%>%
    mutate(sandtotal_r = ifelse(compname %in% problem.list & hzname == "2Cg" & is.na(sandtotal_r), round(0.69*(100- claytotal_r), digits = 1), sandtotal_r ))%>%
    mutate(silttotal_r = ifelse(compname %in% problem.list & hzname == "2Cg" & is.na(silttotal_r), round(0.31*(100- claytotal_r), digits = 1), silttotal_r ))%>%
    mutate(sandtotal_r = ifelse(compname %in% problem.list & (hzname == "Cg"|hzname == "C") & is.na(sandtotal_r), round(0.69*(100- claytotal_r), digits = 1), sandtotal_r ))%>%
    mutate(silttotal_r = ifelse(compname %in% problem.list & (hzname == "Cg"|hzname == "C") & is.na(silttotal_r), round(0.31*(100- claytotal_r), digits = 1), silttotal_r ))%>%
    mutate(silttotal_r = ifelse(compname %in% problem.list2 & is.na(silttotal_r) & hzname != "R", 60, silttotal_r) ,
           sandtotal_r = ifelse(compname %in% problem.list2 & is.na(sandtotal_r) & hzname != "R", (100 - 60-claytotal_r), sandtotal_r),
           om_r = ifelse(compname %in% problem.list2 & is.na(om_r) & hzname != "R", 0.25, om_r),
           dbovendry_r = ifelse(compname %in% problem.list2 & is.na(dbovendry_r) & hzname != "R", 1.38, dbovendry_r))%>%
    mutate(sandtotal_r = ifelse(compname %in% problem.list5 & hzname == "C" & is.na(sandtotal_r), 74.0, sandtotal_r),
           silttotal_r = ifelse(compname %in% problem.list5 & hzname == "C" & is.na(silttotal_r), 1.0, silttotal_r))%>%
    mutate(sandtotal_r = ifelse(compname %in% sand.list & is.na(sandtotal_r) & is.na(silttotal_r), 67.5, sandtotal_r),
           silttotal_r = ifelse(compname %in% sand.list & is.na(silttotal_r) & is.na(sandtotal_r), (100-sandtotal_r-claytotal_r), silttotal_r),
           dbovendry_r = ifelse(compname %in% sand.list & is.na(dbovendry_r), 1.59, dbovendry_r))%>%
    mutate(hzname = ifelse(compname %in% problem.list3 & !is.na(om_r) & is.na(silttotal_r)& hzname == "H4", "R", hzname),
           om_r = ifelse(compname %in% problem.list3 & !is.na(om_r) & is.na(silttotal_r)& hzname == "R", NA, om_r))%>%
    mutate(silttotal_r = ifelse(compname %in% problem.list4 & is.na(silttotal_r) & hzname == "C", 54.3, silttotal_r),
           sandtotal_r = ifelse(compname %in% problem.list4 & is.na(sandtotal_r) & hzname == "C", 19.2, sandtotal_r),
           claytotal_r = ifelse(compname %in% problem.list4 & is.na(claytotal_r) & hzname == "C", 26.5, claytotal_r),
           dbovendry_r = ifelse(compname %in% problem.list4 & is.na(dbovendry_r) & hzname == "C", 1.55, dbovendry_r),
           ph1to1h2o_r = ifelse(compname %in% problem.list4 & is.na(ph1to1h2o_r) & hzname == "C", 4.6, ph1to1h2o_r),
           fragvol_r = ifelse(compname %in% problem.list4 & is.na(fragvol_r) & hzname == "C", 35, fragvol_r),
           ksat_r = ifelse(compname %in% problem.list4 & is.na(ksat_r) & hzname == "C", 10.152, ksat_r),
           ksat = ifelse(compname %in% problem.list4 & is.na(ksat) & hzname == "C", 10.152*3.6, ksat),
           ksat_mm_day = ifelse(compname %in% problem.list4 & is.na(ksat_mm_day) & hzname == "C", ksat*24, ksat_mm_day),
           ksat_r = ifelse(compname %in% problem.list4 & is.na(ksat_r) & hzname == "R", 10.152, ksat_r),
           ksat = ifelse(compname %in% problem.list4 & is.na(ksat) & hzname == "R", 10.152*3.6, ksat),
           ksat_mm_day = ifelse(compname %in% problem.list4 & is.na(ksat_mm_day) & hzname == "R", ksat*24, ksat_mm_day))%>%
    mutate(ksat_r = ifelse(compname %in% glacial.till.list & is.na(ksat_mm_day) & (hzname == "2R"|hzname == "R") & compname == "Farmington",0.01, 
                           ifelse(compname %in% glacial.till.list & is.na(ksat_mm_day) & (hzname == "2R"|hzname == "R") & compname == "Galway", 0.215, ksat_r)))%>%
    mutate(sandtotal_r = ifelse(is.na(sandtotal_r) & !is.na(silttotal_r) & !is.na(claytotal_r), 100 - silttotal_r - claytotal_r, sandtotal_r),
           silttotal_r = ifelse(is.na(silttotal_r) & !is.na(sandtotal_r) & !is.na(claytotal_r), 100 - sandtotal_r - claytotal_r, silttotal_r),
           claytotal_r = ifelse(is.na(claytotal_r) & !is.na(sandtotal_r) & !is.na(silttotal_r), 100 - sandtotal_r - silttotal_r, claytotal_r))
  
  soil.ksat.recalc <- soil.table2b%>%
    mutate(ksat = ksat_r*3.6, ksat_mm_day = ksat*24)
  
  
  ######### Organic soils components still need to add up to 100% for proper weighted averages
  ##use mutate to tell if horizons should be deemed organic and put in placeholders for other components
  #else if the layer is organic, keep the values the same and don't change them
  soil.table3 <- soil.ksat.recalc %>%
    mutate(sandtotal_r = round(ifelse(!is.na(om_r) & is.na(sandtotal_r), (100-om_r)/3, sandtotal_r), digits = 1),
           silttotal_r = round(ifelse(!is.na(om_r) & is.na(silttotal_r), (100-om_r)/3, silttotal_r), digits = 1),
           claytotal_r = round(ifelse(!is.na(om_r) & is.na(claytotal_r), (100-om_r)/3, claytotal_r), digits = 1),
           dbovendry_r = round(ifelse(!is.na(om_r) & is.na(dbovendry_r), 0.30, dbovendry_r), digits = 2),
           hzname = ifelse(!is.na(om_r) & om_r >=30, "O", hzname))
  
  ######### Old code for sorting out organic layers ##################
  ##the organic soils seems to cause issues since it has na values for most other soil components
  #these layers can be determined by finding if sand silt or clay are na and the OM is present !is.na()
  #if O horizon turn the NAs of sand, silt, and clay to 0, and put the bulk density to 0.3 grams per centimeter based on info from:
  # https://www.sciencedirect.com/topics/earth-and-planetary-sciences/bulk-density#:~:text=In%20organic%20soils%2C%20bulk%20density,%E2%80%931.5%20g%20cm%E2%88%92%203.
  # mineral components estimation based on the description of CATDEN series (41.00231,-75.06274), "less than 5% mineral" these I split evenly.
  # 
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$sandtotal_r), "sandtotal_r"] <- 1.66
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$silttotal_r), "silttotal_r"] <- 1.66
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$claytotal_r), "claytotal_r"] <- 1.66
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$fragvol_r), "fragvol_r"] <- 0
  # soil.table2[!is.na(soil.table2$om_r) & is.na(soil.table2$dbovendry_r), "dbovendry_r"] <- 0.3
  # soil.table2[!is.na(soil.table2$om_r) & soil.table2$om_r >= 30, "hzname"] <- "O"
  
  
  
  #### now subset out the bedrock and O layers since they have NAs or don't add up to 100% ####
  soil.no.o.r <- subset(soil.table3, hzname != "R")%>%
    subset(hzname != "O")
  
  ###mineral classes
  mineral.classes <- data.frame(nrcs(soil.no.o.r))
  
  #get names (just for testing purposes)
  #names(mineral.classes)
  
  #get the classes each belong to from this output
  classes_results <- names(mineral.classes)[apply(mineral.classes, 1, function(i) which(i >= 1))]
  
  ##bind this with the no O and R table
  classes.bind.no.o.r <- cbind(soil.no.o.r, classes_results)
  
  ##now subset out the OM and R horizons and add in there classification results column
  soil.o.r <- subset(soil.table3, hzname == "R" | hzname == "O")%>%
    mutate(classes_results = NA)
  
  soil.o.r[soil.o.r$hzname == "O", "classes_results"] <- "Org"
  soil.o.r[soil.o.r$hzname == "R", "classes_results"] <- "Bedrock"
  
  ##bind back together organic and non-organic results
  binded.results <- rbind(classes.bind.no.o.r, soil.o.r) %>%
    group_by(compname)%>% ##group so that its arranged by the component
    arrange((hzdept_r), .by_group = T) ##.by_group argument makes arrange() group by the group
  
  ###define whether or not layer is topsoil from the depth
  #create topsoil column
  binded.topsoil <- binded.results %>%
    mutate(topsoil_bin = NA)
  
  ##make if depth greater than 50 cm its subsoil
  binded.topsoil[binded.topsoil$hzdepb_r > 50, "topsoil_bin"] <- 0
  binded.topsoil[binded.topsoil$hzdepb_r <= 50, "topsoil_bin"] <- 1
  
  ##for VG equations any zeros cause errors where zeros in texture classes
  #need to replace with small values
  #needs to be done for all textural components
  binded.topsoil[!is.na(binded.topsoil$om_r) & binded.topsoil$om_r == 0, "om_r"] <- 0.10
  binded.topsoil[!is.na(binded.topsoil$sandtotal_r) & binded.topsoil$sandtotal_r == 0, "sandtotal_r"] <- 0.10
  binded.topsoil[!is.na(binded.topsoil$silttotal_r) & binded.topsoil$silttotal_r == 0, "silttotal_r"] <- 0.10
  binded.topsoil[!is.na(binded.topsoil$claytotal_r) & binded.topsoil$claytotal_r == 0, "claytotal_r"] <- 0.10
  
  ##return the binded results
  return(binded.topsoil)
  
}


###ksat calculator calculates the upper 50cm soil layer depth weighted ksat
ksat.calculator.50 <- function(soil.table){
  
  ##soil.table test value
  #soil.table <- bedrock.test
  
  
  ##test to see if table given is NA
  if (all(is.na(soil.table))) {
    wt_ksat_location <- NA
  }else {
    
    ##testing
    #soil.table <- test.point
    
    #select top 50 cm
    soil.table.50 <- soil.table %>%
      subset(hzdept_r <= 51)
    
    ##change any values above 50 to just 50 in the hzdepb.r column
    soil.table.50[soil.table.50$hzdepb_r > 51, "hzdepb_r"] <- 51
    
    #multiply this depth of each horizon by the respective ksat values of each horizon to get sum that used in average
    soil.table.50b <- soil.table.50%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(hz_ksat = hz_depth * ksat)
    
    #need to remove repeat rows if present, will skew the data
    #can use duplicated() functionalities of dplyr 
    soil.table.50c <- soil.table.50b[!duplicated(soil.table.50b),] ###this appears to work and remove duplicates if present
    
    
    ####testing out errors by layer (summ each layer ksat)
    # comp.ksat.layer.sum <- soil.table.50c %>%
    #   group_by(compname, comppct_r, hzname)%>%
    #   summarize(horizon_ksat_sum = sum(hz_ksat), n = n())
    
    
    ##group by the component name and the percentage it represents and summ (subset out all but the ksat columns) ##########
    comp.ksat.sum <- soil.table.50c%>%
      select(compname,comppct_r,ksat, hz_ksat, hz_depth)%>% ##select just the columns we need also allows us to hone in on errors
      na.omit()%>% ##removes the horizons that did not have any ksat data which generate NAs
      group_by(compname, comppct_r)%>% ##group to get the mean for each component in the upper 50cm.
      summarise(ksat_50 = sum(hz_ksat)/51, sum = sum(hz_ksat), hz_sum = sum(hz_depth)) %>% ### sum the ksat for each component and divdie by 50, this gives average ksat for first 50 cm of soil
      # summarise(ksat_50 = sum(hz_ksat)/50, sum = sum(hz_ksat), hz_sum = sum(hz_depth)) %>% ##alternative way to calculate (propably the better)
      mutate(wt_ksat = comppct_r*ksat_50) ##mulitply by the weight  it will then be summed below and divided by the total component percentages
    
    ##compute the weighted average. Numbers are not in decimal percentage (e.g., 98 = 98% = .98) this is okay as long as all in same scale
    wt_ksat_location <- sum(comp.ksat.sum$wt_ksat)/ sum(comp.ksat.sum$comppct_r) 
    #wt_ksat_location <- sum(comp.ksat.sum$wt_ksat)/ 100
    
    
  }
  
  return(wt_ksat_location) ##gives ksat in cm/hr, convert to mm or cm per day later
  
} 

###Ksat calculator for depths greater than 50, helps sus out hardpan or barriers to water movement
##first need to get layers below upper 50, then get weighted average of the depth and then calculate weighted ksat
#need to also separate out the bedrock fraction since it will mess with any values 
ksat.calculator.deep <- function(soil.table){
  
  ##soil.table test value
  #soil.table <- soil.test
  
  
  ##test to see if table given is NA
  if (all(is.na(soil.table))) {
    wt_ksat_location <- NA
  }else {
    
    ##testing
    #soil.table <- test.point
    
    
    ###subset out the bedrock if it is present
    r.removed <- subset(soil.table, hzname != "R")
    
    
    
    #select soil layers below 50 cm
    soil.table.deep <- r.removed %>%
      subset(hzdepb_r > 51)
    
    ##at this point the data has been combed to remove erroneous data entry so we can remove any NAs if present
    #in one example: 40.37859,-75.09962, a C horizon is missing data and needs to be removed
    ##problem is if a soil component is shallow and stops before this cutoff, then the bedrock of that layer will represent the lower soils
    soil.table.deep2 <- na.omit(soil.table.deep)
    
    
    ##change any values above 50 to just 50 in the hzdepb.r column
    soil.table.deep[soil.table.deep$hzdept_r < 51, "hzdept_r"] <- 51
    
    ##need to now substitute in the weighted averaged for the soil depth into each component to make sure the soil profile is the same everywhere
    
    
    #multiply this depth of each horizon by the respective ksat values of each horizon to get sum that used in average
    soil.table.deepb <- soil.table.deep%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(hz_ksat = hz_depth * ksat)
    
    #need to remove repeat rows if present, will skew the data
    #can use duplicated() functionalities of dplyr 
    soil.table.deepc <- soil.table.deepb[!duplicated(soil.table.deepb),] ###this appears to work and remove duplicates if present
    
    
    ####testing out errors by layer (summ each layer ksat)
    # comp.ksat.layer.sum <- soil.table.50c %>%
    #   group_by(compname, comppct_r, hzname)%>%
    #   summarize(horizon_ksat_sum = sum(hz_ksat), n = n())
    
    ###need to calculate the depth of each soil component past 51 cm to use to get the average ksat of the deeper layer 
    comp.max.depths <- soil.table.deepc %>%
      group_by(compname)%>%
      summarise(max_depth = max(hzdepb_r))%>%
      mutate(depth_diffs = max_depth-51)
    
    ##join these values to soil.table.deepc so we can run them through below to get weighted averages
    soil.table.deepd <- left_join(soil.table.deepc, comp.max.depths, by = "compname")%>%
      mutate(depth_times_ksats = ksat*hz_depth) #multiply the depths by the ksat values of each horizon, needed to get average ksat for each component
    
    
    ##prep the table, testing to see why summarise isn''t working right
    comp.prep <- soil.table.deepd%>%
      select(compname,comppct_r,ksat, hz_ksat, hz_depth, depth_diffs, depth_times_ksats)%>%
      mutate(compprop = comppct_r/100)%>%   ##convert percentages to decimal proportions and multiply the values by there depth of horizons
      na.omit()%>% ##removes the horizons that did not have any ksat data which generate NAs
      group_by(compname, compprop, comppct_r, depth_diffs)%>% ##group by these so we can keep them in the table for use later
      summarise(ksat_sum = sum(depth_times_ksats), sum = sum(hz_ksat), hz_sum = sum(hz_depth)) %>% ###summarise must only take the summary functions that call that
      mutate(ksat_deep = ksat_sum/depth_diffs) %>% ###now divide by the depth_diffs
      # summarise(ksat_50 = sum(hz_ksat)/50, sum = sum(hz_ksat), hz_sum = sum(hz_depth)) %>% ##alternative way to calculate (propably the better)
      mutate(wt_ksat = comppct_r*ksat_deep, wt_deci = compprop*ksat_deep) ##mulitply by the weight  it will then be summed below and divided by the total component percentages
    
    ##seems like addding in another column variable into the summarise function calls like so: ksat_deep = sum(depth_times_ksats)/depth_diffs
    ## causes it to error and return more rows than desired even though the calculations are still correct. This is because of the way dplyr recycles vectors
    # if they are of different lenght than the rest of them, when I was putting in the depth_diffs
    
    ##compute the weighted average. Numbers are in decimal but can be either as long as in same scale
    wt_ksat_location <- sum(comp.prep$wt_deci)/ sum(comp.prep$compprop) 
    #wt_ksat_location <- sum(comp.prep$wt_ksat)/ sum(comp.prep$comppct_r) 
    #wt_ksat_location <- sum(comp.ksat.sum$wt_ksat)/ 100
    
    
  }
  
  return(wt_ksat_location)
  
} 


###pH weighted average in upper portion of soil where pH data is available
##updated 3/27/2024
wgt.pH.50 <- function(soil.table){
  
  ###testing
  # soil.table <- soil.retrieved
  
  ##test to see if table given is NA
  if (all(is.na(soil.table))) {
    wt_ph <- NA
  }else {
    
    ##testing
    #soil.table <- test.point
    
    #select top 50 cm
    soil.table.50 <- soil.table %>%
      subset(hzdept_r <= 50)
    
    ##change any values above 50 to just 50 in the hzdepb.r column
    soil.table.50[soil.table.50$hzdepb_r > 50, "hzdepb_r"] <- 50
    
    #multiply this depth of each horizon by the respective ksat values of each horizon to get sum that used in average
    soil.table.50b <- soil.table.50%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(hz_ph = hz_depth * ph1to1h2o_r)
    
    #need to remove repeat rows if present, will skew the data
    #can use duplicated() functionalities of dplyr 
    soil.table.50c <- soil.table.50b[!duplicated(soil.table.50b),] ###this appears to work and remove duplicates if present
    
    
    
    
    ##group by the component name and the percentage it represents and summ (subset out all but the ksat columns) ##########
    comp.ph.sum <- soil.table.50c%>%
      select(compname,comppct_r,ph1to1h2o_r, hz_ph, hz_depth)%>% ##select just the columns we need also allows us to hone in on errors
      na.omit()%>% ##removes the horizons that did not have any ksat data which generate NAs
      group_by(compname, comppct_r)%>% ##group to get the mean for each component in the upper 50cm.
      summarise(ph_50 = sum(hz_ph)/sum(hz_depth), sum = sum(hz_ph), hz_sum = sum(hz_depth)) %>% ### sum the ksat for each component and divdie by depth of avail data, this gives average ksat for first 50 cm of soil
      # summarise(ksat_50 = sum(hz_ksat)/50, sum = sum(hz_ksat), hz_sum = sum(hz_depth)) %>% ##alternative way to calculate (propably the better)
      mutate(wt_ph = comppct_r*ph_50) ##mulitply by the weight  it will then be summed below and divided by the total component percentages
    
    ##compute the weighted average. Numbers are not in decimal percentage (e.g., 98 = 98% = .98) this is okay as long as all in same scale
    wt_ph <- round(sum(comp.ph.sum$wt_ph)/ sum(comp.ph.sum$comppct_r), digits = 1)
    #wt_ksat_location <- sum(comp.ksat.sum$wt_ksat)/ 100
    
    
  }
  
  return(wt_ph) ##gives ksat 
  
}

###weighted total soil depth function. Need this to define how deep our second soil horizon is
#not needed for calculating weighted averages but just for defining the soil layer dimenions for the model
#does not include the bedrock (R) layer since this doesn't contain information needed for VG parameters
##these functions require the soil.horizons table that has been classified and given vg parameters
soil.depth <- function(soil.table){
  
  #soil.table <- bedrock.test
  
  ###code moved to main soil retrieval function
  
  # ##need to first subset out the bedrock
  # ##first get the maximum depth for each component type.
  # soil.depths.table <- soil.table%>%
  #   group_by(compname,comppct_r)%>% #group by component and the percentage
  #   summarize(max_depth = max(hzdepb_r)) %>%
  #   select(compname, max_depth)
  # 
  # ##left join back to the original table, now we can determine if horizons contain the bedrock layer
  # soil.table2 <- left_join(soil.table, soil.depths.table, by = "compname")
  # 
  # #check if bedrock present will have deepest layer, and 
  # soil.table2[is.na(soil.table2$sandtotal_r) & soil.table2$hzdepb_r == soil.table2$max_depth, "hzname"] <- "R"
  
  ##subset out the bedrock horizons
  soil.r.rem <- soil.table %>%
    subset(hzname != "R")
  
  ####group by each component and then take weighted average of the deepest point
  depths.processed <- soil.r.rem%>%
    group_by(compname,comppct_r)%>% #group by component and the percentage, and sandtotal to tell if na hzs
    summarize(max_depth = max(hzdepb_r))%>% 
    mutate(wt_depth_mult = max_depth*comppct_r)
  
  
  ##get weighted average of depth of soil
  weighted_soil_depth <- sum(depths.processed$wt_depth_mult)/sum(depths.processed$comppct_r) ##gives weighted average depth of soil
  #use this for the lower limit of the lower layer ksats to allow for averaging
  
  ##round it
  wt.depth <- round(weighted_soil_depth)
  
  return(wt.depth)
}

##for getting bedrock depth 
bedrock.depth <- function(soil.table){
  
  #soil.table <- bedrock.test
  
  ###code moved to main soil retrieval function
  
  # ##need to first subset out the bedrock
  # ##first get the maximum depth for each component type.
  # soil.depths.table <- soil.table%>%
  #   group_by(compname,comppct_r)%>% #group by component and the percentage
  #   summarize(max_depth = max(hzdepb_r)) %>%
  #   select(compname, max_depth)
  # 
  # ##left join back to the original table, now we can determine if horizons contain the bedrock layer
  # soil.table2 <- left_join(soil.table, soil.depths.table, by = "compname")
  # 
  # #check if bedrock present will have deepest layer, and 
  # soil.table2[is.na(soil.table2$sandtotal_r) & soil.table2$hzdepb_r == soil.table2$max_depth, "hzname"] <- "R"
  
  # ##subset out the bedrock horizons
  # soil.r.rem <- soil.table %>%
  #   subset(hzname != "R")
  
  
  #   ####group by each component and then take weighted average of the deepest point
  depths.processed <- soil.table%>%
    group_by(compname,comppct_r)%>% #group by component and the percentage, and sandtotal to tell if na hzs
    summarize(max_depth = max(hzdepb_r))%>% 
    mutate(wt_depth_mult = max_depth*comppct_r)
  
  
  ##get weighted average of depth of soil
  weighted_soil_depth <- sum(depths.processed$wt_depth_mult)/sum(depths.processed$comppct_r) ##gives weighted average depth of soil
  #use this for the lower limit of the lower layer ksats to allow for averaging
  
  ##round it
  wt.depth <- round(weighted_soil_depth)
  
  return(wt.depth)
}

#test
#soil.depth(soil.test)

###now need soil components analysis for both
###soil components analysis takes a table that has had the horizons classified and vg parameters fit for each layer
###updated 5/30/2024: now automatically detects shallow soil layers below 50cm and changes final hz depth to match
soil_texture50 <- function(soil.table){
  #make a weighted average of the sand, silt, clay, om, and fragment volume
  
  #testing
  # soil.table <- soil.horizons
  
  if (all(is.na(soil.table))) {
    #if NA all aspects of the data need to be represented as NA
    wt_soil_text <- data.frame(wt_sand = NA, wt_silt = NA, wt_clay = NA, wt_om = NA, wt_frag = NA, min_bedrock = NA, perc_hydric = NA, soil_class = NA, wt_bd_g_cm3 = NA)
    
  } else {
    
    #testing table
    #soil.table <- test
    
    ##get the weighted average soil depth to set the layer to later (don't use this for averages of soil components add this at the end to give soil depth)
    topsoil.depth <- ifelse(soil.depth(soil.table ) >= 50, 50, soil.depth(soil.table ))
    
    
    ###remove bedrock layers
    soil.table.r.rem <- soil.table %>%
      subset(hzname != "R")%>% ##subset out bedrock
      subset(hzdept_r <= 50) ###get stuff starting and a depth below 50 cm
    
    ###if hzdepb_r is greater than 50 set it to 50
    soil.table.r.rem[soil.table.r.rem$hzdepb_r > 50, "hzdepb_r"] <- 50
    
    ####shouldn't need to change the depth values to get the average of percentages for 50 cm depth
    
    
    
    #subset just the soil values out for purpose of removing NA values later (don't want to remove data because NA in another column)
    # want hzname, depths, aws, compname, comppct, pct_component
    soil.table.100b <- soil.table.r.rem %>% ##make sure it references the one just above with excess soil depths removed
      select(compname, comppct_r, hzname,hzdept_r, hzdepb_r,ksat, awc_r, sandtotal_r, silttotal_r, claytotal_r, om_r,
             fragvol_r, pct_component, perc_hydric, dbovendry_r, classes_results, theta_s, theta_r, alpha_cm,n,m, alpha_m, alpha_kpa) #make sure extra columns are not in just the ones we need
    ##need to do weighted averages of each vg parameter because we cant get average when bedrock is in upper soil horizon
    
    
    ##the organic soils seems to cause issues since it has na values for most other soil components
    #these layers can be determined by finding if sand silt or clay are na and the OM is present !is.na()
    #if O horizon turn the NAs of sand, silt, and clay to 0, and put the bulk density to 0.3 grams per centimeter based on info from:
    # https://www.sciencedirect.com/topics/earth-and-planetary-sciences/bulk-density#:~:text=In%20organic%20soils%2C%20bulk%20density,%E2%80%931.5%20g%20cm%E2%88%92%203.
    # mineral components estimation based on the description of CATDEN series (41.00231,-75.06274), "less than 5% mineral" these I split evenly.
    
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$sandtotal_r), "sandtotal_r"] <- 1.66
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$silttotal_r), "silttotal_r"] <- 1.66
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$claytotal_r), "claytotal_r"] <- 1.66
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$fragvol_r), "fragvol_r"] <- 0
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$dbovendry_r), "dbovendry_r"] <- 0.3
    
    ###now make column of the absolute value of the difference between the ranges to get the depth of each horizon
    #then multiply the values by their depth so we can get averages
    soil.table.100c <- soil.table.100b%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(hz_sand = hz_depth * sandtotal_r)%>%
      mutate(hz_silt = hz_depth * silttotal_r)%>%
      mutate(hz_clay = hz_depth * claytotal_r)%>%
      mutate(hz_om = hz_depth * om_r)%>%
      mutate(hz_ksat = hz_depth*ksat,
             hz_awc = hz_depth*awc_r)%>% ###gonna do all weighted averages in these 
      mutate(hz_frag = hz_depth * fragvol_r, hz_bd = dbovendry_r*hz_depth, hz_theta_s = theta_s*hz_depth,
             hz_theta_r = theta_r*hz_depth, hz_alpha_cm = alpha_cm*hz_depth, hz_alpha_m = alpha_m*hz_depth,
             hz_alpha_kpa = alpha_kpa*hz_depth, hz_n = n*hz_depth, hz_m = m*hz_depth)
    
    
    
    #need to remove repeat rows if present, will skew the data
    #can use duplicated() functionalities of dplyr 
    soil.table.100d <- soil.table.100c[!duplicated(soil.table.100c),] ###this appears to work
    
    ####i Need to get max depths for each component in order to calculat ethe averages in the first summarize below
    #if a layer has a max depth of less than 50 cm than we can't just divide them all by 50 cm to get expected soil averages
    
    ###can just do a quick summarize
    upper.depths <- soil.table.100d %>%
      group_by(compname)%>%
      summarize(hz_depthc = max(hzdepb_r))%>%
      ungroup()
    
    ###left bind by the compname
    soil.table.depthsfixed <- left_join(soil.table.100d, upper.depths, by = "compname")
    
    ##group by the component name and the percentage it represents and summ
    ssc.sum <- soil.table.depthsfixed%>%
      na.omit()%>% ##removes the horizons that did not have any water data which generate NAs (bedrock layers are removed in this)
      group_by(compname, comppct_r)%>% ###problem is that the NA omit function is removing rows that have water data because other columns are NA
      summarize(ksat_sum = sum(hz_ksat),
                awc_sum = sum(hz_awc),
                sand_sum = sum(hz_sand), 
                silt_sum = sum(hz_silt), 
                clay_sum = sum(hz_clay), 
                om_sum = sum(hz_om), 
                frag_sum = sum(hz_frag),
                bd_sum = sum(hz_bd),
                theta_s_sum = sum(hz_theta_s),
                theta_r_sum = sum(hz_theta_r),
                alpha_cm_sum = sum(hz_alpha_cm),
                alpha_m_sum = sum(hz_alpha_m),
                alpha_kpa_sum = sum(hz_alpha_kpa),
                n_sum = sum(hz_n),
                m_sum = sum(hz_m)) %>%
      left_join( upper.depths, by = "compname")%>%  ###bind in each components respective depth value
      mutate(ksat_comp = ksat_sum/hz_depthc, ### now divide the sums by each comps respective soil depth
             awc_comp = awc_sum/hz_depthc,
             sand_comp = sand_sum/hz_depthc,
             silt_comp = silt_sum/hz_depthc,
             clay_comp = clay_sum/hz_depthc,
             om_comp = om_sum/hz_depthc,
             frag_comp = frag_sum/hz_depthc,
             bd_comp = bd_sum/hz_depthc,
             theta_s_comp = theta_s_sum/hz_depthc,
             theta_r_comp = theta_r_sum/hz_depthc,
             alpha_cm_comp = alpha_cm_sum/hz_depthc,
             alpha_m_comp = alpha_m_sum/hz_depthc,
             alpha_kpa_comp = alpha_kpa_sum/hz_depthc,
             n_comp = n_sum/hz_depthc,
             m_comp = m_sum/hz_depthc)%>%
      mutate(text_perc_check = sum(sand_comp, silt_comp, clay_comp, om_comp))%>% ###check how close the texture stuff adds up to 100 Looks good!
      mutate(wt_ksat_mmhr = comppct_r*ksat_comp,
             wt_awc = comppct_r*awc_comp,
             wt_sand = comppct_r*sand_comp,
             wt_silt = comppct_r*silt_comp,
             wt_clay = comppct_r*clay_comp,
             wt_om = comppct_r*om_comp,
             wt_frag = comppct_r*frag_comp,
             wt_bd = comppct_r*bd_comp,
             wt_theta_s = comppct_r*theta_s_comp,
             wt_theta_r = comppct_r*theta_r_comp,
             wt_alpha_cm = comppct_r*alpha_cm_comp,
             wt_alpha_m = comppct_r*alpha_m_comp,
             wt_alpha_kpa = comppct_r*alpha_kpa_comp,
             wt_n = comppct_r*n_comp,
             wt_m = comppct_r*m_comp) 
    
    ##compute the weighted average for each texture component
    wt_sand <- sum(ssc.sum$wt_sand)/ sum(ssc.sum$comppct_r) #### weighted average of sand across components
    wt_silt <- sum(ssc.sum$wt_silt)/ sum(ssc.sum$comppct_r)
    wt_clay <- sum(ssc.sum$wt_clay)/ sum(ssc.sum$comppct_r)
    wt_om <- sum(ssc.sum$wt_om)/ sum(ssc.sum$comppct_r)
    wt_frag <- sum(ssc.sum$wt_frag)/ sum(ssc.sum$comppct_r)
    wt_bd_gcm3 <- sum(ssc.sum$wt_bd)/ sum(ssc.sum$comppct_r)
    perc_hydric <- unique(soil.table$perc_hydric)
    wt_theta_s <- sum(ssc.sum$wt_theta_s)/sum(ssc.sum$comppct_r)
    wt_theta_r <- sum(ssc.sum$wt_theta_r)/sum(ssc.sum$comppct_r)
    wt_alpha_cm <- sum(ssc.sum$wt_alpha_cm)/sum(ssc.sum$comppct_r)
    wt_alpha_m <- sum(ssc.sum$wt_alpha_m)/sum(ssc.sum$comppct_r)
    wt_alpha_kpa <- sum(ssc.sum$wt_alpha_kpa)/sum(ssc.sum$comppct_r)
    wt_n <- sum(ssc.sum$wt_n)/sum(ssc.sum$comppct_r)
    wt_m <- sum(ssc.sum$wt_m)/sum(ssc.sum$comppct_r)
    wt_ksat = sum(ssc.sum$wt_ksat_mmhr)/sum(ssc.sum$comppct_r)
    wt_awc = sum(ssc.sum$wt_awc)/sum(ssc.sum$comppct_r)
    
    ##setting up sandy/normal/peaty
    #sandy if sand is largest component
    # peaty, if weighted om > 5 percent, and hydric percent higher than 40%
    
    if (wt_sand > wt_silt & wt_sand > wt_clay) {
      ##if sand largest component, than set soil class to s (sand)
      soil_class <- "s"
      
      
    } else if ( perc_hydric > 40 & wt_om > 5){
      ##soil class is p (peaty/hydric)
      soil_class <- "p"
      
    } else {
      ##if not either of these its normal
      soil_class <- "n"
      
    }
    
    ##since sometimes bedrock depth is not available make conditional to make NA if NA 
    if (min(na.omit(soil.table$brockdepmin == Inf))) {
      min_bedrock <- NA
    } else {
      min_bedrock <- min(na.omit(soil.table$brockdepmin))
      
    }
    
    ##since sometimes water table depth is not available make conditional to make NA if NA 
    if (min(na.omit(soil.table$wtdepannmin == Inf))) {
      wtdepannmin <- NA
    } else {
      wtdepannmin <- min(na.omit(soil.table$wtdepannmin))
      
    }
    
    
    ##put the table together
    wt_soil_text <- data.frame(hzname = c("top50"), hzdept_r = c(0), hzdepb_r = c(topsoil.depth),wt_ksat_mmhr = wt_ksat, wt_awc = wt_awc, wt_sand = wt_sand, wt_silt = wt_silt, wt_clay = wt_clay, wt_om = wt_om, wt_frag = wt_frag, 
                               wt_bd_gcm3 = wt_bd_gcm3, min_bedrock = min_bedrock, wtdepannmin = wtdepannmin,  perc_hydric = perc_hydric, soil_class = soil_class,
                               wt_theta_s = wt_theta_s, wt_theta_r = wt_theta_r, wt_alpha_cm = wt_alpha_cm, wt_alpha_m = wt_alpha_m, wt_alpha_kpa = wt_alpha_kpa,
                               wt_n = wt_n, wt_m = wt_m)
    
  }
  
  return(wt_soil_text)
  
  
}

###do the same but for deeper soils excluding the bedrock layer
soil_texturedeep <- function(soil.table){
  #make a weighted average of the sand, silt, clay, om, and fragment volume
  
  #testing
  #soil.table <- soil.horizons
  
  if (all(is.na(soil.table))) {
    #if NA all aspects of the data need to be represented as NA
    wt_soil_text <- data.frame(wt_sand = NA, wt_silt = NA, wt_clay = NA, wt_om = NA, wt_frag = NA, min_bedrock = NA, perc_hydric = NA, soil_class = NA, wt_bd_g_cm3 = NA)
    
  } else {
    
    #testing table
    #soil.table <- test
    
    ##need to determine soil depth for the layer we end up defining for the profile table
    wt.depth <- soil.depth(soil.table)
    
    ### need to subset out each component and modify the max depth so its the same
    #need to subset out the bedrock layers first or else it messes up the actual depth
    soil.tab.nor <- soil.table %>%
      subset(hzname != "R")
    
    
    ##take the soil data from soil.data.retriever
    soil.table.100 <- soil.tab.nor%>%
      subset(hzdepb_r > 50)
    
    
    ####select the columns we care about
    soil.table.100b <- soil.table.100 %>% ##make sure it references the one just above with excess 
      select(compname, comppct_r, hzname,hzdept_r, hzdepb_r,ksat, awc_r, sandtotal_r, silttotal_r, claytotal_r, om_r,
             fragvol_r, pct_component, perc_hydric, dbovendry_r, classes_results, theta_s, theta_r, alpha_cm,n,m, alpha_m, alpha_kpa)
    
    ##change any values deeper than 50 to just 50 in the hzdepb.r table (creates bottom stop)
    soil.table.100b[soil.table.100b$hzdept_r < 50, "hzdept_r"] <- 50
    
    ##the organic layer seems to cause issues since it has na values for most other soil components
    #need to preferentially change if na to zero so the data can still be calculated
    #if O horizon turn the NAs of sand, silt, and clay to 0, and put the bulk density to 0.3 grams per centimeter based on info from:
    # https://www.sciencedirect.com/topics/earth-and-planetary-sciences/bulk-density#:~:text=In%20organic%20soils%2C%20bulk%20density,%E2%80%931.5%20g%20cm%E2%88%92%203.
    
    
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$sandtotal_r), "sandtotal_r"] <- 1.66
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$silttotal_r), "silttotal_r"] <- 1.66
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$claytotal_r), "claytotal_r"] <- 1.66
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$fragvol_r), "fragvol_r"] <- 0
    soil.table.100b[!is.na(soil.table.100b$om_r) & is.na(soil.table.100b$dbovendry_r), "dbovendry_r"] <- 0.3
    
    
    
    ###now make column of the absolute value of the difference between the ranges to get the depth of each horizon
    #then multiply the values by their depth so we can get averages
    soil.table.100c <- soil.table.100b%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(hz_sand = hz_depth * sandtotal_r)%>%
      mutate(hz_silt = hz_depth * silttotal_r)%>%
      mutate(hz_clay = hz_depth * claytotal_r)%>%
      mutate(hz_om = hz_depth * om_r)%>%
      mutate(hz_ksat = hz_depth*ksat,
             hz_awc = hz_depth*awc_r)%>% ###gonna do all weighted averages in these 
      mutate(hz_frag = hz_depth * fragvol_r, hz_bd = dbovendry_r*hz_depth, hz_theta_s = theta_s*hz_depth,
             hz_theta_r = theta_r*hz_depth, hz_alpha_cm = alpha_cm*hz_depth, hz_alpha_m = alpha_m*hz_depth,
             hz_alpha_kpa = alpha_kpa*hz_depth, hz_n = n*hz_depth, hz_m = m*hz_depth)
    
    #need to remove repeat rows if present, will skew the data
    #can use duplicated() functionalities of dplyr 
    soil.table.100d <- soil.table.100c[!duplicated(soil.table.100c),] ###this appears to work
    
    ###for each component get the subsoil depth (needs to be done for correct averaging)
    ###can just do a quick summarize
    subsoil.depths <- soil.table.100d %>%
      group_by(compname)%>%
      summarize(hz_depthc = max(hzdepb_r))%>%
      mutate(comp_depth = abs(hz_depthc-50)) ###for subsoils need to get the depth of soils below 50 cm (our upper layer) this value is what we divide sums by
    
    ###left bind by the compname
    soil.table.depthsfixed <- left_join(soil.table.100d, subsoil.depths, by = "compname")
    
    
    ##group by the component name and the percentage it represents and summ
    ssc.sum <- soil.table.depthsfixed%>%
      na.omit()%>% ##removes the horizons that did not have any water data which generate NAs (bedrock layers are removed in this)
      group_by(compname, comppct_r, comp_depth)%>% ###problem is that the NA omit function is removing rows that have water data because other columns are NA
      summarize(ksat_sum = sum(hz_ksat),
                awc_sum = sum(hz_awc),
                sand_sum = sum(hz_sand), 
                silt_sum = sum(hz_silt), 
                clay_sum = sum(hz_clay), 
                om_sum = sum(hz_om), 
                frag_sum = sum(hz_frag),
                bd_sum = sum(hz_bd),
                theta_s_sum = sum(hz_theta_s),
                theta_r_sum = sum(hz_theta_r),
                alpha_cm_sum = sum(hz_alpha_cm),
                alpha_m_sum = sum(hz_alpha_m),
                alpha_kpa_sum = sum(hz_alpha_kpa),
                n_sum = sum(hz_n),
                m_sum = sum(hz_m)) %>%
      mutate(ksat_comp = ksat_sum/comp_depth, ### now divide the sums by each comps respective soil depth
             awc_comp = awc_sum/comp_depth,
             sand_comp = sand_sum/comp_depth,
             silt_comp = silt_sum/comp_depth,
             clay_comp = clay_sum/comp_depth,
             om_comp = om_sum/comp_depth,
             frag_comp = frag_sum/comp_depth,
             bd_comp = bd_sum/comp_depth,
             theta_s_comp = theta_s_sum/comp_depth,
             theta_r_comp = theta_r_sum/comp_depth,
             alpha_cm_comp = alpha_cm_sum/comp_depth,
             alpha_m_comp = alpha_m_sum/comp_depth,
             alpha_kpa_comp = alpha_kpa_sum/comp_depth,
             n_comp = n_sum/comp_depth,
             m_comp = m_sum/comp_depth)%>%
      mutate(text_perc_check = sum(sand_comp, silt_comp, clay_comp, om_comp))%>% ###check how close the texture stuff adds up to 100 Looks good!
      mutate(wt_ksat_mmhr = comppct_r*ksat_comp,
             wt_awc = comppct_r*awc_comp,
             wt_sand = comppct_r*sand_comp,
             wt_silt = comppct_r*silt_comp,
             wt_clay = comppct_r*clay_comp,
             wt_om = comppct_r*om_comp,
             wt_frag = comppct_r*frag_comp,
             wt_bd = comppct_r*bd_comp,
             wt_theta_s = comppct_r*theta_s_comp,
             wt_theta_r = comppct_r*theta_r_comp,
             wt_alpha_cm = comppct_r*alpha_cm_comp,
             wt_alpha_m = comppct_r*alpha_m_comp,
             wt_alpha_kpa = comppct_r*alpha_kpa_comp,
             wt_n = comppct_r*n_comp,
             wt_m = comppct_r*m_comp) 
    
    
    ##compute the weighted average for each texture component
    wt_sand <- sum(ssc.sum$wt_sand)/ sum(ssc.sum$comppct_r) #### weighted average of sand across components
    wt_silt <- sum(ssc.sum$wt_silt)/ sum(ssc.sum$comppct_r)
    wt_clay <- sum(ssc.sum$wt_clay)/ sum(ssc.sum$comppct_r)
    wt_om <- sum(ssc.sum$wt_om)/ sum(ssc.sum$comppct_r)
    wt_frag <- sum(ssc.sum$wt_frag)/ sum(ssc.sum$comppct_r)
    wt_bd_gcm3 <- sum(ssc.sum$wt_bd)/ sum(ssc.sum$comppct_r)
    perc_hydric <- unique(soil.table$perc_hydric)
    wt_theta_s <- sum(ssc.sum$wt_theta_s)/sum(ssc.sum$comppct_r)
    wt_theta_r <- sum(ssc.sum$wt_theta_r)/sum(ssc.sum$comppct_r)
    wt_alpha_cm <- sum(ssc.sum$wt_alpha_cm)/sum(ssc.sum$comppct_r)
    wt_alpha_m <- sum(ssc.sum$wt_alpha_m)/sum(ssc.sum$comppct_r)
    wt_alpha_kpa <- sum(ssc.sum$wt_alpha_kpa)/sum(ssc.sum$comppct_r)
    wt_n <- sum(ssc.sum$wt_n)/sum(ssc.sum$comppct_r)
    wt_m <- sum(ssc.sum$wt_m)/sum(ssc.sum$comppct_r)
    wt_ksat = sum(ssc.sum$wt_ksat_mmhr)/sum(ssc.sum$comppct_r)
    wt_awc = sum(ssc.sum$wt_awc)/sum(ssc.sum$comppct_r)
    
    
    ##setting up sandy/normal/peaty
    #sandy if sand is largest component
    # peaty, if weighted om > 5 percent, and hydric percent higher than 40%
    
    if (wt_sand > wt_silt & wt_sand > wt_clay) {
      ##if sand largest component, than set soil class to s (sand)
      soil_class <- "s"
      
      
    } else if ( perc_hydric > 40 & wt_om > 5){
      ##soil class is p (peaty/hydric)
      soil_class <- "p"
      
    } else {
      ##if not either of these its normal
      soil_class <- "n"
      
    }
    
    ##since sometimes bedrock depth is not available make conditional to make NA if NA 
    if (min(na.omit(soil.table$brockdepmin == Inf))) {
      min_bedrock <- NA
    } else {
      min_bedrock <- min(na.omit(soil.table$brockdepmin))
      
    }
    
    ##since sometimes water table depth is not available make conditional to make NA if NA 
    if (min(na.omit(soil.table$wtdepannmin == Inf))) {
      wtdepannmin <- NA
    } else {
      wtdepannmin <- min(na.omit(soil.table$wtdepannmin))
      
    }
    
    
    ##put the table together
    wt_soil_text <- data.frame(hzname = c("lower_soil"), hzdept_r = c(50), hzdepb_r = c(wt.depth),wt_ksat_mmhr = wt_ksat, wt_awc = wt_awc, wt_sand = wt_sand, wt_silt = wt_silt, wt_clay = wt_clay, wt_om = wt_om, wt_frag = wt_frag, 
                               wt_bd_gcm3 = wt_bd_gcm3, min_bedrock = min_bedrock, wtdepannmin = wtdepannmin,  perc_hydric = perc_hydric, soil_class = soil_class,
                               wt_theta_s = wt_theta_s, wt_theta_r = wt_theta_r, wt_alpha_cm = wt_alpha_cm, wt_alpha_m = wt_alpha_m, wt_alpha_kpa = wt_alpha_kpa,
                               wt_n = wt_n, wt_m = wt_m)
    
  }
  
  return(wt_soil_text)
  
  
}


####now need to get bedrock layer if present, could possibly assign van genutchen parameters based on rock data
get.bedrock <- function(soil.table){
  
  ##test
  # soil.table <- soil.horizons
  
  #soil depth (top boundary of bedrock)
  wt.depth <- soil.depth(soil.table)
  
  ###subset out the bedrock fraction of the soils
  bedrock <- soil.table %>%
    subset(hzname == "R")
  
  if (all(is.na(bedrock))) {
    bedrock.data <- data.frame(hzname = c("bedrock"), hzdept_r = NA, hzdepb_r = NA ,wt_ksat_mmhr = NA, wt_awc = NA, wt_sand = NA, wt_silt = NA, wt_clay = NA, wt_om = NA, wt_frag = NA, 
                               wt_bd_gcm3 = NA, min_bedrock = NA, wtdepannmin = NA,  perc_hydric = NA, soil_class = NA, wt_theta_s = NA, wt_theta_r = NA,
                               wt_alpha_cm = NA, wt_alpha_m = NA, wt_alpha_kpa = NA,
                               wt_n = NA, wt_m = NA)
    
  } else{
    
    ##get mean soil depth of the bedrock layer
    br.depth <- bedrock.depth(bedrock) ##this will be defined as the hzdepb_r in the end table
    
    #multiply this depth of each horizon by the respective ksat values of each horizon to get sum that used in average
    #AWC is NA for bedrock layers so
    bedrock3 <- bedrock%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(hz_ksat = hz_depth * ksat)%>%
      mutate(hz_theta_s = theta_s*hz_depth,
             hz_theta_r = theta_r*hz_depth, hz_alpha_cm = alpha_cm*hz_depth, hz_alpha_m = alpha_m*hz_depth,
             hz_alpha_kpa = alpha_kpa*hz_depth, hz_n = n*hz_depth, hz_m = m*hz_depth)
    
    #need to remove repeat rows if present, will skew the data
    #can use duplicated() functionalities of dplyr 
    bedrock4 <- bedrock3[!duplicated(bedrock3),] ###this appears to work and remove duplicates if present
    
    
    ##group by the component name and the percentage it represents and summ (subset out all but the ksat columns) ##########
    ### awc_r is NA for bedrock so don't calculate its value
    br.sum <- bedrock4%>% ##select just the columns we need also allows us to hone in on errors
      group_by(compname, comppct_r, hz_depth)%>% ##hz depth functions as the overall target layer depth since there is only one bedrock horizon
      summarise(ksat_sum = sum(hz_ksat),
                theta_s_sum = sum(hz_theta_s),
                theta_r_sum = sum(hz_theta_r),
                alpha_cm_sum = sum(hz_alpha_cm),
                alpha_m_sum = sum(hz_alpha_m),
                alpha_kpa_sum = sum(hz_alpha_kpa),
                n_sum = sum(hz_n),
                m_sum = sum(hz_m)) %>% 
      mutate(ksat_comp = ksat_sum/hz_depth, ####this appears unnecessary since its just one layer for each comp but there are cases where >1 layer of R (i.e. Cr horizons)
             theta_s_comp = theta_s_sum/hz_depth,
             theta_r_comp = theta_r_sum/hz_depth,
             alpha_cm_comp = alpha_cm_sum/hz_depth,
             alpha_m_comp = alpha_m_sum/hz_depth,
             alpha_kpa_comp = alpha_kpa_sum/hz_depth,
             n_comp = n_sum/hz_depth,
             m_comp = m_sum/hz_depth)%>%
      mutate(wt_ksat = comppct_r*ksat_comp,
             wt_theta_s = comppct_r*theta_s_comp,
             wt_theta_r = comppct_r*theta_r_comp,
             wt_alpha_cm = comppct_r*alpha_cm_comp,
             wt_alpha_m = comppct_r*alpha_m_comp,
             wt_alpha_kpa = comppct_r*alpha_kpa_comp,
             wt_n = comppct_r*n_comp,
             wt_m = comppct_r*m_comp) ##mulitply by the weight  it will then be summed below and divided by the total component percentages
    
    ##compute the weighted average. Numbers are not in decimal percentage (e.g., 98 = 98% = .98) this is okay as long as all in same scale
    wt_ksat_mmhr <- sum(br.sum$wt_ksat)/ sum(br.sum$comppct_r) 
    wt_theta_s <- sum(br.sum$wt_theta_s)/sum(br.sum$comppct_r)
    wt_theta_r <- sum(br.sum$wt_theta_r)/sum(br.sum$comppct_r)
    wt_alpha_cm <- sum(br.sum$wt_alpha_cm)/sum(br.sum$comppct_r)
    wt_alpha_m <- sum(br.sum$wt_alpha_m)/sum(br.sum$comppct_r)
    wt_alpha_kpa <- sum(br.sum$wt_alpha_kpa)/sum(br.sum$comppct_r)
    wt_n <- sum(br.sum$wt_n)/sum(br.sum$comppct_r)
    wt_m <- sum(br.sum$wt_m)/sum(br.sum$comppct_r)
    
    ##put it all into a table now
    bedrock.data <- data.frame(hzname = c("bedrock"), hzdept_r = c(wt.depth), hzdepb_r = c(br.depth),  wt_ksat_mmhr = wt_ksat_mmhr,
                               wt_awc = NA, wt_sand = NA, wt_silt = NA, wt_clay = NA, wt_om = NA, wt_frag = NA, 
                               wt_bd_gcm3 = NA, min_bedrock = soil.table$brockdepmin[1], wtdepannmin = soil.table$wtdepannmin[1],  perc_hydric = NA, soil_class = NA,
                               wt_theta_s = wt_theta_s, wt_theta_r = wt_theta_r, wt_alpha_cm = wt_alpha_cm, wt_alpha_m = wt_alpha_m, wt_alpha_kpa = wt_alpha_kpa,
                               wt_n = wt_n, wt_m = wt_m)
    
    
  }
  
  
  
  
  
  return(bedrock.data)
  
}


#####soil profile function that gives layers, generates drainage/bedrock layers as well
###updated 5/30/2024 fix shallow soil capture to account for topsoil depths under 50cm
get.profile <- function (topsoil, subsoil, bedrock){ ##arguments are output tables from tospoil, subsoil and bedrock functions
  
  ####testing purposes
  # topsoil <- text.upper50
  # subsoil <- text.deepsoil
  # bedrock <- br.horizon
  
  ##first we need to test if bedrock layer is present or not, and if NA then make bottom drainage layer a 10 cm layer of the subsoil
  if (is.na(bedrock$wt_ksat_mmhr[1])) {
    
    ###alter the bedrock layer to reflect just a 30 cm drainage of subsoil
    bedrock.drainage <- subsoil%>%
      mutate(hzdept_r = hzdepb_r)%>%
      mutate(hzdepb_r = hzdept_r + 30, hzname = "bedrock_drainage") ##rename it bedrock just for computing purposes, this is just a drainage layer calculation
    
    ###bind the horizons together
    soil.profile <- rbind(topsoil, subsoil, bedrock.drainage)%>%
      cbind(rock.frag.params)%>%
      mutate(layer_midpoint = ((hzdepb_r-hzdept_r)/2)+hzdept_r, soil_vol = 100 - wt_frag)%>%
      mutate(adj_theta_s = round((wt_theta_s*soil_vol+frag_theta_s*wt_frag)/100, digits = 3),
             adj_theta_r = round((wt_theta_r*soil_vol+frag_theta_r*wt_frag)/100, digits = 3))
    
  } else { ##if bedrock is present make the bedrock layer 30 cm drainage layer
    
    bedrock.mod <- bedrock %>%
      mutate(hzdepb_r = hzdept_r + 30,
             wt_frag = 0)
    
    ##bind together
    soil.profile <- rbind(topsoil, subsoil, bedrock.mod)%>% 
      cbind(rock.frag.params)%>%
      mutate(layer_midpoint = ((hzdepb_r-hzdept_r)/2)+hzdept_r, soil_vol = 100 - wt_frag)%>%
      mutate(adj_theta_s = round((wt_theta_s*soil_vol+frag_theta_s*wt_frag)/100, digits = 3),
             adj_theta_r = round((wt_theta_r*soil_vol+frag_theta_r*wt_frag)/100, digits = 3))
    
    
  }
  
  soil.profile2 <- soil.profile %>%
    mutate(hz_depth = hzdepb_r-hzdept_r)%>%
    mutate(total_ws_cm = wt_theta_s*hz_depth, total_frag_ws_cm = frag_theta_s*hz_depth, tot_adj_ws_cm = adj_theta_s*hz_depth)%>%
    mutate(wt_theta_s = round(wt_theta_s, digits = 3), wt_theta_r = round(wt_theta_r, digits = 3), wt_sand = round(wt_sand, 2),
           wt_silt = round(wt_silt, 2), wt_clay = round(wt_clay,2), wt_om = round(wt_om, 2), wt_frag = round(wt_frag, 2),
           wt_bd_gcm3 = round(wt_bd_gcm3, 2), wt_ksat_mmhr = round(wt_ksat_mmhr, 2), soil_vol = round(soil_vol, 2),
           top_soil = ifelse(hzname == "top50",1,0))
  
  ##if a layer has zero adjusted water storage it means that on average the layers of the soil are not more than 50 cm deep
  ## set this laywer to zero
  # soil.profile3 <- soil.profile2%>%
  #   mutate(across(2:length(names(soil.profile2)), ~if_else(hz_depth == 0, NA, .)))
  
  ####if the second layer depth is zero, this means that average soil profile is quite shallow with no mineral soil below this point
  #if so copy the bedrock information to this second row
  if (soil.profile2$hz_depth[2] <= 0) {
    
    ###make another one
    soil.profile3 <- soil.profile2
    
    ##replace second layer with another bedrock layer
    soil.profile3[2,] <- soil.profile3[3,]
    
    ####modify the horizon depths (still 10 cm each but just make sure in correct spots)
    
    soil.profile3$hzdept_r[2] <- soil.profile3$hzdepb_r[1]
    soil.profile3$hzdepb_r[2] <- soil.profile3$hzdepb_r[1]+30
    soil.profile3$hzdept_r[3] <- soil.profile3$hzdepb_r[2]
    soil.profile3$hzdepb_r[3] <- soil.profile3$hzdepb_r[2]+30
    
    
  } else(
    soil.profile3 <- soil.profile2
  )
  
  return(soil.profile3)
  
}



########################## Test GPS points for special cases ##############################
########testing with updated points
#dunes.sub.point <- soil.data.retriever(38.94953,-74.85806)

# #dunelands placeholder (Hooksan)
# dunelands <- subset(test.point, compname == "Hooksan")
# #write it to a csv for saving purposes
# write.csv(dunelands, file = "./dunelands_table.csv")

##translate into dunelands table

####testing changes to soil.data.retriever
# lat <- 38.94953
# long <- -74.85806

##sand/beach with urband land component (want the urband land to be removed)
# lat <- 38.96419
# long <- -74.83812

# part.urb.test <- soil.data.retriever(lat,long) ##works out

#####urban soils mess: just urban land and udorthents (checks out)
# lat <- 40.53854
# long <- -79.76527

# urban.test <- soil.data.retriever(lat, long) #works as intended

########### dune lands point
# #41.659946, -87.068927
#  lat <- 41.659946
#  long <- -87.068927
# 
# dunelands.test <- soil.data.retriever(lat, long) ##appears to work

########## Point for soils with high organic component all the way through
##this case catden component has organic deep all the way through and where organic the soil profile appears to be bedrock because
#the sand silt and clay fractions are NA
#41.00231,-75.06274
# lat <- 41.00231
# long <- -75.06274
# 
# soil.test <- soil.data.retriever(lat, long)

###Arnot component has organic layer with sand silt and clay data present
# lat <- 41.00162 
# long <- -75.06593

# soil.test <- soil.data.retriever(lat, long)


# #####major components present, minor components missing (need similar data)
# # #38.72708,-76.07525
#  lat <- 38.72708
#  long <- -76.07525
# 
#  miss.minor.test <- soil.data.retriever(lat, long) ##works

##bedrock test 40.44030,-75.40695
# lat <- 40.44030
# long <- -75.40695

##where component has shallow bedrock
#40.37858,-75.09966
# 
# lat <- 40.37858
# long <- -75.09966


########################## Putting in our points to get data ##########################

###These are a good test to put through the new code
##bedrock layers do not have any data
#40.35979,-75.08895

#40.36037,-75.08940
# 40.36047,-75.09002

########################## funcitons for cleaning slice data and species analysis ######################################


###function needs inputs of the site sliced, species slices and species ph anc cc data. Also has wieghts of variables available (4/22/2024)
suitability.analyzer <- function(site.sliced, species.slices, species.ph.cc, psi.wt = 55, temp.wt = 15, cc.wt = 10, ph.wt = 10){
  
  ####test, bring in flattened slices
  # site.sliced <- sliced.values
  # species.slices <- species.slice.data
  # species.ph.cc <- spec.ph.cc.all
  # psi.wt = 65
  # temp.wt = 15
  # cc.wt = 5
  # ph.wt = 10
  
  
  ###rename species temp slice column to indicate it is species values
  names(species.slices)[names(species.slices) == 'mean_min_tempc_slice'] <- 'species_mmin_tempc_slice'
  names(species.slices)[names(species.slices) == 'mean_max_tempc_slice'] <- 'species_mmax_tempc_slice'
  
  ####for each species subset out and see if site falls within ranges at each slice and then get percentage of slices that fit within species
  #empty list that will have species and the percentage at which they match
  species.perc <- list()
  
  species.list <- unique(species.slices$species)
  
  for (i in 1:length(species.list)) {
    
    ###testing
    # i <- 8
    
    ##subset a species slice and left join the site data to it
    spp.sub.psi.temp <- species.slices %>%
      subset(species == species.list[i])
    
    ###remove cc and ph data from site slice and left join to species data
    site.spp.slices <- site.sliced%>%
      select(period_num, mean_min_psi_transf_slice, mean_max_psi_transf_slice, mean_tmin_c, mean_tmax_c, site)%>%
      left_join(spp.sub.psi.temp, by = "period_num", suffix = c("_site", "_spec"))
    
    ###slice analysis
    site.spp.temp.psi.test <- site.spp.slices%>%
      mutate(min_psi_match = ifelse(mean_min_psi_transf_slice_site >= mean_min_psi_transf_slice_spec & mean_min_psi_transf_slice_site <= mean_max_psi_transf_slice_spec,1,0),
             max_psi_match = ifelse(mean_max_psi_transf_slice_site >= mean_min_psi_transf_slice_spec & mean_max_psi_transf_slice_site <= mean_max_psi_transf_slice_spec,1,0),
             min_temp_match = ifelse(mean_tmin_c >= species_mmin_tempc_slice & mean_tmin_c <= species_mmax_tempc_slice, 1, 0),
             max_temp_match = ifelse(mean_tmax_c >= species_mmin_tempc_slice & mean_tmax_c <= species_mmax_tempc_slice, 1, 0))
    
    
    
    ##subset out species ph and cc for the species and append the site data to it
    spp.site.ph.cc <- species.ph.cc%>%
      subset(species == species.list[i])%>%
      mutate(site_name = site.sliced$site[1],site_ph50 = site.sliced$pH_50cm[1],
             site_ccoff = site.sliced$leafoff_cc[1],
             site_ccon = site.sliced$leafon_cc[1])%>%
      mutate(ccon_match = ifelse(site_ccon >= min_cc_on & site_ccon <= max_cc_on,1,0),
             ccoff_match = ifelse(site_ccoff >= min_cc_off & site_ccoff <= max_cc_off, 1,0),
             ph_match = ifelse(site_ph50 >= min_ph & site_ph50 <= max_ph, 1, 0))
    
    ##### get the fractional matches for all variables we care about
    psi.match.fract <- sum(site.spp.temp.psi.test$min_psi_match,site.spp.temp.psi.test$max_psi_match )/ (2*length(site.spp.temp.psi.test$period_num))
    
    temp.match.fract <- sum(site.spp.temp.psi.test$min_temp_match, site.spp.temp.psi.test$max_temp_match)/ (2*length(site.spp.temp.psi.test$period_num))
    
    ph.match.fract <- spp.site.ph.cc$ph_match[1]
    
    ccon.match.fract <- spp.site.ph.cc$ccon_match[1]
    ccoff.match.fract <- spp.site.ph.cc$ccoff_match[1]
    
    ####get the overall match percent by weighting the values and dividing but 100
    match.frac <- (psi.match.fract*psi.wt + temp.match.fract*temp.wt + ph.match.fract*ph.wt + ccon.match.fract*cc.wt + ccoff.match.fract*cc.wt)/100
    
    
    ###put into a matching table
    spp.match <- data.frame(perc_fit = paste(round(match.frac*100, digits = 2), "%", sep = ""), species = spp.sub.psi.temp$species[1], frac_fit = match.frac)
    
    ###append to master species.perc table
    species.perc <- rbind(species.perc, spp.match)
    
  }
  
  
  species.perc2 <- species.perc%>%
    arrange(desc(frac_fit))
  
  return(species.perc2)
  
}


########################## load required PRISM and species slice and overall tables ######################################
#### prism locations for all prism files located with "all_prisms" folder,
#needs to be updated whenever new prisms are added and coppied to the app folder
prism.locations <- read.csv("./prism_locations.csv")

###bring in all species cc and ph values
spec.ph.cc.all <- read.csv("spec_ph_cc_data.csv")

##bring in species slices and weekly long form curve values, these values need to be updated and added to the folder
species.slice.data <- read.csv("./all_spp_sliced_data.csv")

species.ov.psi.weekly <- read.csv("./species_ov_psi_weekly.csv")%>%
  select(species, week_num,spec_min_log_geomm_min_psi, spec_max_log_goemm_max_psi)
names(species.ov.psi.weekly)[3:4] <- c('log_geomm_min', 'log_geomm_max')

###################### Shiny App formatting ####################################


# Define UI for dataset viewer app 
ui <- fluidPage(
    
  tabsetPanel(
    tabPanel("Base Inputs",
    # App title
    titlePanel(h1("Species Decision Support Tool"),),
    
    # Sidebar layout with a input and output definitions
        sidebarPanel(
          h1("Data Inputs"),
          
          textInput("zone.name", label = "Zone Name (name your location of interest)", value = ""),
          
          h3("GPS Coordinates"),
          h6("(Decimal Degrees)"),
                    # Input: Numeric entry for number of obs to view 
          numericInput(inputId = "lat",
                       label = "Latitude:",
                       value = NA),
          
          
          numericInput(inputId = "long",
                       label = "Longitude:",
                       value = NA),
          
          numericInput(inputId = 'slope.deg',
                       label = "Slope (degrees)",
                       value = NA),
        
          numericInput(inputId = "cc",
                       label = "Canopy Cover",
                       value = NA),
          
          h6("Leaf out?"),
          checkboxInput("leafon", label = "Leaf on", value = T),
          helpText("Is the canopy cover value for growing season (leaf on)",
                   "period of the year?"),
          
          selectInput(inputId = "aspect",
                      label = "Aspect (Cardinal Direction of the Slope)", 
                      choices = c("N", "S", "E", "W", "NE", "NW", "SE", "SW", "NONE"),
                      selected = "S"),
          
          selectInput(inputId = "veg.form", label = "Vegetation Type",
                      choices = c('evergreen forest', 'deciduous forest','mixed forest','shrub','herbaceous'),
                      selected = "herbaceous"),
          
          ###cfa is automated now
          # numericInput(inputId = "cfa",
          #              label = "Contribution flow distance",
          #              value = 0),
          # helpText("Approximate distance uphill of the location of interest"),
          
          ##Now they must input their PRISM csv file (no longer holding repository for it)
          fileInput("prismfile", "Upload PRISM CSV"),
          
          
          # h1("Adjusted Water table depth"),
          # numericInput(inputId = "wtdepmod",
          #              lable = NA,
          #              value = NA),
          # helpText("Is there a water table present?",
          #          "If so enter depth of water table"),
          
          
          
          submitButton(text = "Analyze")
          
          
        ),
      
    mainPanel(
      
      ####Moisture plot
      h1("Annual Soil Moisture"),
      plotOutput(outputId = "log_geomm_psi_minmax")%>% withSpinner(color = "#0dc5c1"),
      
      ###selector to compare with other species 
      h4("Pick a species to compare to the site"),
      selectInput(inputId = "spp.sel", label = "Species",
                  choices = unique(species.slice.data$species)),
      helpText("Select a species and then hit the 'Analyze' button again"),
      
      #####test output for canopy covers and pH. the character string in the textOutput is the value that the server must provide 
      h2("Other environmental parameters"),
      tableOutput("cc.ph.table")%>% withSpinner(color = "#0dc5c1"),
      
      h2("Species Analysis"),
      helpText("The 'perc_fit' column indicates the percent suitability of the species. Generally, values > 70% indicate a good match"),
      tableOutput("analysis.table") %>% withSpinner(color = "#0dc5c1")
      
      
    )
    
   
    ),
    tabPanel("Advanced Soil Inputs",
             
             
             
             ####main panel to display the soil profile table obtained
             fluidRow(
              column(12,
                     h2("Use this page to modify upper layer soil parameters if those from the database does not match your ground survey"),
                     h3("  Soil Profile (from public database)"),
                     tableOutput(outputId = "soil.profile")%>% withSpinner(color = "#0dc5c1"))
             
             ),
             fluidRow(
               h3("   Input modification to the upper soil layer (top50)")
             ),
             
             ###rows for modification (upper soil)
             fluidRow(
               column(2, numericInput(inputId = "sand.mod",label = "Sand (%)", value = NA)),
               column(2, numericInput(inputId = "silt.mod",label = "Silt (%)", value = NA)),
               column(2, numericInput(inputId = "clay.mod",label = "Clay (%)", value = NA)),
               column(2, numericInput(inputId = "om.mod",label = "OM (%)", value = NA)),
               column(2, numericInput(inputId = "frag.mod",label = "Frag Volume (%)", value = NA)),
               column(2, numericInput(inputId = "bd.mod",label = "Bulk Density (g/cm^3)", value = NA)),
             ),
             
             ##row for water table depth modfication
             fluidRow(
               column(4, numericInput(inputId = "wt.mod",label = "Min annual Water Table Depth (cm)", value = NA)),
               column(4, numericInput(inputId = "depth.mod",label = "Topsoil Depth (cm)", value = NA))
             )
             
             
             ),
    tabPanel("Hydrology Inputs",
             
            ##breif title 
            fluidRow(
              column(12,h2("Use this page to describe hydrology influencing you zone"),
              h3("Choose one type and enter data.")),
            ),
            fluidRow(
              column(12, h3("For Man-made ponds and Drainage Basins")),
              column(2,checkboxInput("manmadePond", "Select Man-made pond")),
              column(3,numericInput(inputId = "surfacewidth", label = "Surface Width (m)", value = NA)),
              column(3,numericInput(inputId = "surfacelen", label = "Surface Length (m)", value = NA)),
              column(3, numericInput(inputId = "pbankslope", label = "Slope of pond bank (degrees)", value = NA)),
              column(3, numericInput(inputId = "maxpdepth", label = "Max pond depth (m)", value = NA), helpText("From bottom to point of overflow")),
              column(4, numericInput(inputId = "posrelfill", label = "Zone Relative Position to Full depth (cm)", value = NA), helpText("Relative distance to max depth of pond.")),
              column(2, checkboxInput(inputId = "inputspres", label = "Drainage Inputs Present?")),
              column(2, numericInput("uphillarea", "Total Uphill Drainage Area (acres)", value = NA))
            ),
            
             
             
             
             
             )
    
    
    
    
    )
  
  

)
# Define server logic to summarize and view selected dataset  
##input is an object that contains all the above inputs that the user put in
server <- function(input, output) {
    
    
  ###for updating ui in the hydrology tab
  observeEvent(input$hydrotype, {
    updateTabsetPanel(inputId = "params", selected = input$hydrotype)
  })
  
  # hydro.table <- reactive({
  #   switch(input$hydrotype,
  #          )
  #   
  # })
  
  
  # ###############################set the values for the input variables 
  # ####enter zone name that we want to look at
  zone.name <- reactive({input$zone.name})


  #####input gps points
  lat <- reactive({input$lat})
  long <- reactive({input$long})



  ####### canopy cover input and ask if this is leaf on canopy cover value or if canopy cover was taken during the winter
  ###canopy cover should be supplied in a whole number percent value (45% not .45)
  cc <- reactive({input$cc})
  leafon <- reactive({input$leafon})

  ####### slope (degrees)
  slope.deg <- reactive({ifelse(input$slope.deg <= 0, 5, input$slope.deg)})

  ###aspect
  aspect <- reactive({ifelse(input$aspect == "NONE", "S", input$aspect)})

  ####### Vegetation formation
  veg.form <- reactive({input$veg.form})

  ####### Flow contribution area (m^2) zero for now since the cfa functionality does not appear to be working properly
  point.cfa <- reactive({input$cfa})

  ####water table modifier
  # wt.dep.mod <- reactive({input$wtdepmod})
  
  
  
  
  
  
  
  ##################################### the rest of the code typically in the moisture model code
  ##need to build the blocks necessary for the final profile fit
  
  #####soil retrieved function
  soil.retrieved.block <- reactive({
    
    ##call the retrieval function on the gps points
    soil.retrieved <- soil.data.retriever(input$lat, input$long)
    
  })
  
  ####ph calculator
  soil.ph <- reactive({
  
    ###ph calculator
    ph <- wgt.pH.50(soil.table = soil.retrieved.block())
    })
  
  
  ###profile fit block takes into acount variable modifications
  soil.layers.block <- reactive({
    
    soil.retrieved <- soil.retrieved.block()
    
    ###now classify the horizons (classifies O and R) layers where present and defines which horizons are topsoil and which are not.
    soil.classified <- classify.horizons(soil.retrieved)
    
    
    ###use vg fit function to fit the parameters to each horizon
    soil.horizons <- vg.fit(soil.classified)
    
    
    #####if soil textural class modification is present, grab the sand, silt, clay values
    if (!is.na(text.mod)) {
      
      ####match the texture mod to the nrcs parameters table
      texture.subset <- nrcs.parameters%>%
        subset(classes_results == text.mod)
      
      ###assign modification values 
      sand.mod <- texture.subset$sand[1]
      silt.mod <- texture.subset$silt[1]
      clay.mod <- texture.subset$clay[1]
      
      ###ksat mod needs to be manually added
      
      
    } else {
      
      ###IF NO texture mod then make them NA
      sand.mod <- NA
      silt.mod <- NA
      clay.mod <- NA
      
    }
    
    
    
    ##create hz data table for the upper 50, should be plugging in the soil.horizons
    text.upper50 <- soil_texture50(soil.horizons)%>%
      mutate(wt_frag = ifelse(!is.na(input$frag.mod), input$frag.mod, wt_frag),
             wt_om = ifelse(!is.na(input$om.mod), input$om.mod, wt_om),
             wt_bd_gcm3 = ifelse(!is.na(input$bd.mod), input$bd.mod, wt_bd_gcm3),
             hzdepb_r = ifelse(!is.na(input$depth.mod), input$depth.mod, hzdepb_r),
             wt_ksat_mmhr = ifelse(!is.na(input$ksat.mod), input$ksat.mod, wt_ksat_mmhr))
    
    ##modify the text.upper50 if all modifications are present and add up to 100
    if (all(!is.na(c(input$sand.mod, input$silt.mod, input$clay.mod))) & (input$sand.mod + input$silt.mod + input$clay.mod == 100)) {
      ##change texture of upper soil horizon
      text.upper50$wt_sand[1] <- input$sand.mod
      text.upper50$wt_silt[1] <- input$silt.mod
      text.upper50$wt_clay[1] <- input$clay.mod
    }
    
    
    #create hz data table for lower soils
    text.deepsoil <- soil_texturedeep(soil.horizons)
    
    ##get the bedrock data only returns the ksat but we can still use this
    br.horizon <- get.bedrock(soil.horizons)
    
    ####get soil.profile with get.profile function that modifies if bedrock is missing to give proper 3rd drainage layers
    soil.profile <- get.profile(text.upper50, text.deepsoil, br.horizon)%>%
      mutate(wtdepannmin = ifelse(!is.na(input$wt.mod), input$wt.mod, wtdepannmin))
    
    
    soil.profile.set <- soil.profile
    
    
  })
  
  #### canopy cover block
  ####use cc calculator and ph retriever
  cc.table.block <-  reactive({
    cc.calculator(ccinput = cc(), leaf_on = leafon(), veg_form = veg.form())
    
  })
  
  ####puts together profile fit and the psri
  profile.full.block <- reactive({
    
    ###try setting values so we don't have to reactive them later
    slope.deg <- input$slope.deg
    aspect <- input$aspect
    point.cfa <- input$cfa
    
    ###not an entered value (calculated to assist with seepage calculations)
    # slope.deg <- atan(slope/100) * (180/pi)
    seep.slope <- ifelse(slope.deg() == 0, 45, slope.deg())##modify the slope of the seepage area if the target area is flat
    seep.angle.mod <- cos((90-seep.slope)* pi/180) ##because the downhill seep is flowing at an angle wee need to modify the flow rate (ksat * cos(angle))
    infil.angle.mod <- ifelse(slope.deg() == 0, 0, cos((90-slope.deg())* pi/180))
    
    
    ###read in all the prism files location information to find closest prism
    all.prisms <- prism.locations%>%
      mutate(distance = sqrt((lat_prism - lat())^2 + (long_prism-long())^2))
    
    ###get the closest prism
    min.dist <- min(all.prisms$distance)
    
    ###subset out closest one
    closest.prism <- subset(all.prisms, distance == min.dist)
    
    ###supply prism link in a characters string so we can rename file outputs automatically
    prism.file <- paste(getwd(), "all_prisms", closest.prism$prism_file[1], sep = "/")
    
    ##extract sit name only needed for file saving purposes
    # site.extract.table <- read.table(text = prism.file, sep = "/", colClasses = "character", col.names = c("null", "dir", "site_name", "prism_file"))
    
    
    ####get the elevation from the prism file
    elev.prism <- read.csv(prism.file)
    
    ###read out the first column to find elevation
    elev.sep <- read.table(text = as.character(elev.prism$PRISM.Time.Series.Data[1]), sep = " ")
    
    ###elevation in numeric format converted to meters
    elevation.m <- as.numeric(str_sub(elev.sep$V12, 0, -3))*0.3048
    
    
    
    ################### assign canopy parameters for PENMAN evaporation and GASH interception equations
    
    ##make table for intereption parameters
    int.param.table <- gash.params%>%
      subset(veg_form == veg.form())%>%
      mutate(canopy_cover = cc())
    
    ###get the parameters for the rs calculations
    veg_parameters <- veg.match(int.params = int.param.table)
    
    ###calculate ra here since it is a static parameter (5/28/2024)
    ra.calc <- ra(veg_height = veg_parameters$canopy_ht_m[1], dcoeff = veg_parameters$d_coef[1])
    
    ###calculate the psychrometric constant
    psy.const <- psychrom(elevation.m)
    
    
    
    
    ################## Soil Retreival and parameter calculations
    soil.layers <- soil.layers.block()
    # 
    # 
    # ###now classify the horizons (classifies O and R) layers where present and defines which horizons are topsoil and which are not.
    # soil.classified <- classify.horizons(soil.retrieved)
    # 
    # 
    # ###use vg fit function to fit the parameters to each horizon
    # soil.horizons <- vg.fit(soil.classified)
    # 
    # ##create hz data table for the upper 50, should be plugging in the soil.horizons
    # text.upper50 <- soil_texture50(soil.horizons)
    # 
    # #create hz data table for lower soils
    # text.deepsoil <- soil_texturedeep(soil.horizons)
    # 
    # ##get the bedrock data only returns the ksat but we can still use this
    # br.horizon <- get.bedrock(soil.horizons)
    # 
    # ####get soil.profile with get.profile function that modifies if bedrock is missing to give proper 3rd drainage layers
    # soil.profile <- get.profile(text.upper50, text.deepsoil, br.horizon)
    
    ##modify the watertable depth
    # soil.profile$wtdepannmin <- wtdepmod
    
    
    ##get vg params based on weighted soil profiles texture values
    ##the paw values are me testing to see if the way i'm calculating it matches the USDA
    ##updated to have field capacity stuff (4/23/2024)
    profile.fit <- vg.profile.fit(soil_profile = soil.layers)%>%
      mutate(cfa_m2 = point.cfa())%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(tot_adj_ws_cm = adj_theta_s*hz_depth,
             min_adj_ws_cm = adj_theta_r*hz_depth)%>%
      mutate(slope_deg = slope.deg(), seep_slope = seep.slope,
             seep_angle_mod = seep.angle.mod, inf_ang_mod = infil.angle.mod)%>% ##need to set slope for seepage calculations
      mutate(theta_pwp_norm = wrc.theta(h_cm = 15300, theta_r = theta_r, theta_s = theta_s, alpha = alpha_cm, n = n, m = m),
             paw_tot_norm = (theta_s - theta_pwp_norm)*hz_depth,
             paw_tot_usda_adj = (theta_s*frag_frac - theta_pwp_norm*frag_frac)*hz_depth,
             theta_pwp = wrc.theta(h_cm = 15300, theta_r = adj_theta_r, theta_s = adj_theta_s, alpha = alpha_cm, n = n, m = m),
             paw_tot_me_adj = (adj_theta_s - theta_pwp)*hz_depth,
             theta_fc = wrc.theta(h_cm = 300, theta_r = adj_theta_r, theta_s = adj_theta_s, alpha = alpha_cm, n = n, m = m),
             ws_fc = theta_fc * hz_depth)
    #if the slope is zero (when target area is flat flood plain) assume the slope of the contributing flow area is 25 degrees
    
    ###testing modification of water table to see what happends to curve
    # profile.fit$wtdepannmin = NA
    
    
    
    ####need to get the 0-100 cm weighted average PAW (take from soil.retrieved table)
    #in this analysis the Urban lands are ignored so if thats in the landscape the Web soil survey values will differ from this
    # awc_100 <- aws.calculator(soil.retrieved)
    
    
    
    ####################### Put together prism table and begin calculating interception
    ###generate the leaf off and leaf on data from cc.calculator
    cc.table <- cc.table.block()
    
    ###clean the prism and match up well data if present
    prism.cleaned <- read.csv(prism.file, skip = 10)%>%
      prism.cleaning()%>%
      mutate(vpdmean_kpa = ((vpdmin_hpa+vpdmax_hpa)/2)/10,
             week_num = week(ymd(date)),
             month_num = substr(date, 6, 7))%>%
      mutate(week_num = ifelse(week_num > 52, 52, week_num))%>% ##if a leap year week number becomes greater than 52, keep to 52 for averaging sake
      well.match(latitude = lat, longitude = long, gen.profile = profile.fit)
    
    
    #### Fetch well data if the minimum water table depth is present
    ###if wtdepannmin is present than fetch well data and add to the prism, else do
    ######match water table data if applicable
    prism.watertable <- well.match(latitude = lat(), longitude = long(), gen.profile = profile.fit, prism.table = prism.cleaned)
    
    
    #####put in other variables 
    ######With apply() functions add in the canopy cover changes,radiation, evaporation (canopy and ground), rain fall duration
    prism.cc <- prism.watertable%>%
      mutate(atm_tran = ifelse(precip_cm > 0, .25, 1))%>%
      mutate(canopy_cover = sapply(doy,cc.seasonal, cc.on = cc.table$leaf_on_perc[1], cc.off = cc.table$leaf_off_perc[1] ),
             rl = sapply(doy,cc.seasonal, cc.on = veg_parameters$rl[1], cc.off = 0),
             rs = rl/veg_parameters$peak_LAI) %>% ##have stomatal resistance increase as leaves come out
      mutate(rad_top_atm = sapply(doy, hargreaves.rad, latitude = lat()))%>%
      ##scaled evap is for the interception function that still uses the Hargreaves ET function
      mutate(scaled_evap_mmhr = mapply( gash.et, rad_top_atm, t_mean = tmean_c, t_min = tmin_c, t_max = tmax_c, canopy = canopy_cover)) %>%##need to use mapply for multiple list arguments
      ####ground evap is the evap taken from the ground, need to calculate PENMAN params for each day
      # mutate(windsp_ms = 2)%>% ##windspeed is default of two since we don't have this data
      mutate(Rn = mapply(net.radiation, doy = doy, elevation = elevation.m, slope_deg = slope.deg, aspect = aspect, transmission = atm_tran, lat = lat(), long = long(), tmin = tmin_c, tmax = tmax_c, VPD = vpdmean_kpa,
                         cc = canopy_cover,veg_form = veg_parameters$base_form[1] ))%>%
      mutate(air_dense = mapply(air.dense, tempc = tmean_c, elev = elevation.m))%>%
      mutate(delta = sapply(tmean_c, delta.slope))%>%
      mutate(ground_evap_mmday = mapply(penman.et, Rn = Rn, ra = ra.calc, VPD = vpdmean_kpa, psychrom =psy.const, delta = delta, air_dense = air_dense, rs = rs ))%>%
      mutate(rain_dur_hrs = sapply(precip_cm, rainfall_duration_hrs))%>%
      mutate(rain_rate_cmhr = round(ifelse(precip_cm > 0,precip_cm/rain_dur_hrs, 0), digits = 2),
             rain_rate_mmhr = round(ifelse(precip_mm > 0,precip_mm/rain_dur_hrs, 0), digits = 2))%>% ##get PPG and then subsequent interception values
      mutate(Sc = canopy_cover*int.param.table$canopyS_mm[1])%>%
      mutate(PPG = mapply(PPG, rain_rate = rain_rate_mmhr, scaled_e = scaled_evap_mmhr, Sc = Sc, formation = veg.form))%>%
      mutate(canopy_int_mm = mapply(canopy_int, precip = precip_mm, PPG = PPG, cc = canopy_cover, rain_rate = rain_rate_mmhr, scaled_evap = scaled_evap_mmhr, formation = veg.form))%>%
      mutate(trunk_int_mm = mapply(trunk_int, pt = int.param.table$pt_mm[1], precip = precip_mm, PPT = int.param.table$Pt_mm[1], St = int.param.table$trunkS_mm[1]))%>%
      mutate(total_int_mm = trunk_int_mm + canopy_int_mm)%>%
      mutate(adj_precip_mm = ifelse(is.na(precip_mm - total_int_mm),0, round(precip_mm - total_int_mm, digits = 4 )),
             adj_prate_cmhr = ifelse(precip_mm > 0, round(((adj_precip_mm/10)/rain_dur_hrs), digits = 2), 0))%>% ##need adjusted rainfall rate for infiltration and runoff
      mutate(ground_evap_mmday = ifelse(ground_evap_mmday < 0, 0, ground_evap_mmday))%>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"),
             daylag = lag(date, default = first(date)-1))%>%
      group_by(grp = cumsum(precip_cm > 0))%>%
      mutate(last_rain = as.numeric(date - daylag[1]))%>%
      ungroup()%>%
      mutate(slope = slope.deg, cfa_m2 = point.cfa)
    
    
    ###################### Create a Progress object and function needed for the progress bar to be set
    progress <- shiny::Progress$new()
    progress$set(message = "Simulating Soil Moisture", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / length(prism.cc2$date)
      }
      progress$set(value = value, detail = detail)
    }
    
    
    
    
    
    ####generate the table with the soil moisture profile over the entire 20 years (or duration of user choosing depending on length of table input)
    profile.full <- water.balance(filled.df = prism.cc2, soil.df.input = profile.fit, updateProgress = updateProgress)%>%
      mutate(doy = as.character(doy), week_char = as.character(week_num),
             geo_trans_psi = abs(psi_top_cm)+1)%>%
      mutate(year = substr(as.character(date), start = 0, stop = 4))
    
    
    
    
    ###plotting entire time series
    # plot(profile.full$psi_top_cm)
    #plot(profile.full$seep_ws_top) 
    # plot(profile.full$ws_top_cm)
    # plot(profile.full$precip_in)
    # plot(profile.full$paw_top_cm)
    
    ###used to split in order to test, now combined above to reduce file size
    # profile.full2 <- profile.full%>%
    #   mutate(doy = as.character(doy), week_char = as.character(week_num),
    #          geo_trans_psi = abs(psi_top_cm)+1)%>%
    #   mutate(year = substr(as.character(date), start = 0, stop = 4))
    
    
  })
  
  
  
  ####data summary block
  data.summ.block <- reactive({
    
    
    ####getting the min and max of each year-week combination for psi and vpd (seeing wierd stuff)
    year.week.mins <- profile.full.block()%>%
      group_by(year,week_num)%>% ## for each year-week, obtain the min and maximum psi values
      summarize(min_psi = min(psi_top_cm), max_psi = max(psi_top_cm),
                # min_log_psi = min(log(geo_trans_psi)), max_log_psi = max(log(geo_trans_psi))) ##testing not making it negative until after subtracting 1
                min_log_psi = min(-log(geo_trans_psi)), max_log_psi = max(-log(geo_trans_psi))) 
    
    ###now take the geometric mean or median for each week over the entire period
    week.max.mins <- year.week.mins%>%
      mutate(min_geot_psi = abs(min_psi)+1, max_geot_psi = abs(max_psi)+1)%>% ##need to transform these raw psi_min and maxes so we can run geometric mean on them
      group_by(week_num)%>%
      summarize(mean_min_psi = mean(min_psi), sd_min_psi = sd(min_psi),
                mean_max_psi = mean(max_psi), sd_max_psi = sd(max_psi),
                median_min_psi = median(min_psi),
                median_max_psi = median(max_psi),
                geommean_min_psi = -(geometric.mean(min_geot_psi)-1),
                geommean_max_psi = -(geometric.mean(max_geot_psi)-1), ##these are also being transformed back into raw values, need to log transform below
                n = n())
    
    ####weekly temperatures and VPD values
    ##5/23/2024: VPD values added
    weekly.tempc <- profile.full2%>%
      select(date, week_num, tmin_c, tmax_c, vpdmin_hpa, vpdmax_hpa)%>%
      mutate(year = substr(date, 1,4))%>%
      group_by(year,week_num)%>%
      summarize(min_wkly_tminc = min(tmin_c), max_wkly_tmaxc = max(tmax_c), min_wkly_vpdmin_hpa = min(vpdmin_hpa), max_wkly_vpdmax_hpa = max(vpdmax_hpa))%>%
      ungroup()%>%
      group_by(week_num)%>%
      summarize(avg_wkly_tminc = mean(min_wkly_tminc), avg_wkly_tmaxc = mean(max_wkly_tmaxc), avg_wkly_vpdmin = mean(min_wkly_vpdmin_hpa), avg_wkly_vpdmax = mean(max_wkly_vpdmax_hpa))
    
    
    # plot(weekly.tempc$avg_wkly_tminc)
    
    #####transforming the mean min and max psi values
    ##using median values from here on out
    post.transf <- week.max.mins%>%
      mutate(post_min_logpsi = -log(abs(mean_min_psi)+1), post_max_logpsi = -log(abs(mean_max_psi)+1),
             post_med_min_transf = -log(abs(median_min_psi)+1), post_med_max_transf = -log(abs(median_max_psi)+1),
             log_geomm_min = -log(abs(geommean_min_psi)+1),
             log_geomm_max = -log(abs(geommean_max_psi)+1))%>%
      mutate(offset_counts = c(1:52), period_num = offset_counts %/% 6 + 1)
    
    
    ###combine weekly ppt with the post.transf to get all key climate and environmental variables
    weekly.psi.tempc <- post.transf%>%
      left_join(weekly.tempc, by = "week_num")
    
    #####transforming the mean min and max psi values
    ##using median values from here on out
    # post.transf <- week.max.mins%>%
    #   mutate(post_min_logpsi = -log(abs(mean_min_psi)+1), post_max_logpsi = -log(abs(mean_max_psi)+1),
    #          post_med_min_transf = -log(abs(median_min_psi)+1), post_med_max_transf = -log(abs(median_max_psi)+1),
    #          log_geomm_min = -log(abs(geommean_min_psi)+1),
    #          log_geomm_max = -log(abs(geommean_max_psi)+1))%>%
    #   mutate(offset_counts = c(1:52), period_num = offset_counts %/% 6 + 1)
    # 
    
  })
  
  ###slicer block, slices and formats for distance matrix comparison with species
  sliced.values <- reactive({
    
    ##make ccblock into a table we can get values from
    cc.table <- cc.table.block()
    
    data.summ <- data.summ.block()
    
    # sliced <- data.summ.block() %>%
    # group_by(period_num)%>%
    # summarize(mean_min_psi_transf_slice = mean(log_geomm_min), 
    #           mean_max_psi_transf_slice = mean(log_geomm_max))%>%
    # mutate(curve = zone.name())%>%
    # mutate(pH_50cm = soil.ph(), leafon_cc = cc.table$leaf_on_perc[1], leafoff_cc = cc.table$leaf_off_perc[1])
    
    
    ####updated 5/23/2024, now includes slices for VPD
    sliced.values <- data.summ.block() %>%
      group_by(period_num)%>%
      summarize(mean_min_psi_transf_slice = mean(log_geomm_min), 
                mean_max_psi_transf_slice = mean(log_geomm_max),
                mean_tmin_c = mean(avg_wkly_tminc),
                mean_tmax_c = mean(avg_wkly_tmaxc),
                mean_vpdmin_hpa = mean(avg_wkly_vpdmin),
                mean_vpdmax_hpa = mean(avg_wkly_vpdmax))%>%
      mutate(site = zone.name())%>%
      mutate(pH_50cm = soil.ph(), leafon_cc = cc.table$leaf_on_perc[1], leafoff_cc = cc.table$leaf_off_perc[1]) 
    
   
  })
  
  #####bind together the overall species water data with the site data that has been generated, need to reformat post.transf and species.ov.psi.weekly
  
  plot.data <- reactive({
    
    validate(
      need(input$lat, "Please input site data")
    )
    
    ###bring in the site data that was calculated
    site.data.formatted <- data.summ.block()%>%
      mutate(species = zone.name())%>%
      select(species,week_num, log_geomm_min, log_geomm_max)
    
    ####subset out the species of interest
    spp.subset <- species.ov.psi.weekly%>%
      subset(species == input$spp.sel)
    
    ####bind them together (this is the data set fed into the plot output)
    data.compare <- rbind(site.data.formatted, spp.subset)
    
    
  })
  
  
  #####plot the graph
  output$log_geomm_psi_minmax <- renderPlot({
    
    
    
    ggplot(data = plot.data(), aes(x = week_num))+
      geom_ribbon(aes(ymin = log_geomm_min, ymax = log_geomm_max, fill = species), alpha = 0.3)+ #
      ggtitle( paste(zone.name(),"soil moisture range (22 years)", sep = " "))+
      xlab("Week (1-52)")+
      ylim(-15,0)+
      ylab("Log(Water Potential (cm of head))")
    
  })
  
  
  ####text output of other important variables
  output$cc.ph.table <- renderTable({
    
    validate(
      need(input$lat, "Please input site data")
    )
    
    
    ####add ph to the table and then make the Column names nicer
    cc.ph <- cc.table.block()%>%
      select(leaf_on_perc, leaf_off_perc)%>%
      mutate(pH = soil.ph())
    
    names(cc.ph) <- c("Peak Canopy Cover", "Winter Canopy Cover", "pH")
    
    cc.ph
  })
  
  ###output for the advanced parameters tab
  output$soil.profile <- renderTable({
    ###validate
    validate(
      need(input$lat, "Please input site in first tab before modifying soil profile")
    )
    
    ####soil.table.display for the second tab
    soil.table.select <- soil.layers.block()%>%
      select(hzname, hzdept_r, hzdepb_r, wt_ksat_mmhr,wt_sand, wt_silt,wt_clay,wt_om,wt_frag,wt_bd_gcm3, wtdepannmin)%>%
      rename(horizon = hzname, top_cm = hzdept_r, bottom_cm = hzdepb_r, ksat_mmhr = wt_ksat_mmhr, sand_perc = wt_sand,
             silt_perc = wt_silt, clay_perc = wt_clay, OM_perc = wt_om, frag_vol = wt_frag, bd_gcm3 = wt_bd_gcm3, min_WT_depth = wtdepannmin)
    
    
    
  })
  
  
  
  ####output table of the matching species
  output$analysis.table <- renderTable({
    
    validate(
      need(input$lat, "Please input site data")
    )
    
    
    ###compare against the other species
    species.analyzed <- suitability.analyzer(site.sliced = sliced.values(), species.slices = species.slice.data, species.ph.cc = spec.ph.cc.all)
    
    
  })
  
  
  
}

# Create Shiny app  
shinyApp(ui = ui, server = server)









