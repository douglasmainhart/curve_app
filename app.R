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



########################## load required PRISM and species slice and overall tables ######################################
#### prism locations for all prism files located with "all_prisms" folder,
#needs to be updated whenever new prisms are added and coppied to the app folder
##no longer needed having the user provide the PRISM data from now on
# prism.locations <- read.csv("./prism_locations.csv")

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
               column(12,
                      h3("Input modification to the upper soil layer (top50)"))
             ),
             
             ###rows for modification (upper soil)
             fluidRow(
               column(2, selectInput(inputId = "upper.soil.mod", label = "Soil Texture",
                                     choices = c("clay", "clay loam", "loam", "loamy sand", "sand", "sandy clay", "sandy clay loam", "sandy loam", "silt",
                                                 "silty clay", "silty clay loam", "silt loam", "organic","none"),
                                     selected = "none")),
               column(2, numericInput(inputId = "upper.om.mod",label = "OM (%)", value = NA)),
               column(2, numericInput(inputId = "upper.frag.mod",label = "Frag Volume (%)", value = NA)),
               column(2, numericInput(inputId = "upper.bd.mod",label = "Bulk Density (g/cm^3)", value = NA)),
               column(2, numericInput(inputId = "upper.ksat.mod", label = "Ksat (mm/hr)",value = NA))
             ),
             
             ##row for water table depth modfication
             fluidRow(
               column(4, numericInput(inputId = "wt.mod",label = "Min annual Water Table Depth (cm)", value = NA)),
               column(4, numericInput(inputId = "upper.depth.mod",label = "Topsoil Depth (cm)", value = NA))
             ),
             
             fluidRow(
               column(12,
                      h2("For Modifiying Entire Soil profile"),
                      helpText("Only modify the lower and bedrock soil profile data if soil observed in the field is utterly different
                               from what is on SSURGO table above. If so data needs to be entered for all values for each layer."),
                      checkboxInput(inputId = "full_soil_mod", label = "Modify entire soil profile"))
             ),
             
             
             fluidRow(
               column(12,
                      h3("Modifications to sub-soil (>50cm depth)"))
             ),
             
             ###rows for modification (lower soil profile)
             fluidRow(
               column(2, selectInput(inputId = "lower.soil.mod", label = "Soil Texture",
                                     choices = c("clay", "clay loam", "loam", "loamy sand", "sand", "sandy clay", "sandy clay loam", "sandy loam", "silt",
                                                 "silty clay", "silty clay loam", "silt loam", "organic","none"),
                                     selected = "none")),
               column(2, numericInput(inputId = "lower.om.mod",label = "OM (%)", value = NA)),
               column(2, numericInput(inputId = "lower.frag.mod",label = "Frag Volume (%)", value = NA)),
               column(2, numericInput(inputId = "lower.bd.mod",label = "Bulk Density (g/cm^3)", value = NA)),
               column(2, numericInput(inputId = "lower.ksat.mod", label = "Ksat (mm/hr)",value = NA))
             ),
             
             
             fluidRow(
               column(12,
                      h3("Modifications to Bedrock Layer"))
             ),
             
             ###rows for modification (lower soil profile)
             fluidRow(
               column(2, numericInput(inputId = "bed.ksat.mod", label = "Ksat (mm/hr)", value = NA))
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
      need(input$lat, "Input cite location to view current SSURGO data")
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









