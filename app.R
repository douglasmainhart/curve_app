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

# rm(list = ls())

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
# install.packages("DT")


###########################reinstalling the package
  # library(devtools)
  # ###remember to makesure your personal access token is up to date
  # install_github("douglasmainhart/moistureProfile", force = TRUE)

################### Loading required packages ######################
#### these packages are for 
library(shiny)

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
library(DT) ## for better data table viewing

library(psych)

######set working directory (just needed for pulling in example PRISM)
##working directory for Dougs Laptop
# setwd("C:/Users/dougl/OneDrive - archewild.com/Archewild research/Shared Documents/Projects/Soil Water Light/moisture_profile_code_8-25-2023/")

##working directory for Mark's gaming laptop
# setwd("C:/Users/markw/OneDrive - archewild.com/Documents - ArcheWild Research/Projects/Soil Water Light/moisture_profile_code_8-25-2023")

###issues
# options(shiny.reactlog=TRUE)
# shiny::runApp(display.mode="showcase")



########################## load required PRISM and species slice and overall tables ######################################
#### prism locations for all prism files located with "all_prisms" folder,
#needs to be updated whenever new prisms are added and coppied to the app folder
##no longer needed having the user provide the PRISM data from now on
# prism.locations <- read.csv("./prism_locations.csv")

###bring in all species cc and ph values (not needed now under the moistureProfile package data imports)
# spec.ph.cc.all <- read.csv("spec_ph_cc_data.csv")
# 
# ##bring in species slices and weekly long form curve values, these values need to be updated and added to the folder
# species.slice.data <- read.csv("./all_spp_sliced_data.csv")
# 
# species.ov.psi.weekly <- read.csv("./species_ov_psi_weekly.csv")%>%
#   select(species, week_num,spec_min_log_geomm_min_psi, spec_max_log_goemm_max_psi)
# names(species.ov.psi.weekly)[3:4] <- c('log_geomm_min', 'log_geomm_max')

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
          
          h4("Leaf out?"),
          checkboxInput("leafon", label = "Leaf on", value = T),
          helpText("Is the canopy cover value for growing season (leaf on)",
                   "period of the year?"),
          
          selectInput(inputId = "aspect",
                      label = "Aspect (Cardinal Direction of the Slope)", 
                      choices = c("N", "S", "E", "W", "NE", "NW", "SE", "SW", "NONE"),
                      selected = "S"),
          
          selectInput(inputId = "veg.form", label = "Vegetation Type",
                      choices = c('evergreen forest', 'deciduous forest','mixed forest','shrub','herbaceous'),
                      selected = "deciduous forest"),
          numericInput(inputId = "veg.ht", label = "Vegetation Height (m)",
                       value = NA),
          
          ###cfa is automated now
          # numericInput(inputId = "cfa",
          #              label = "Contribution flow distance",
          #              value = 0),
          # helpText("Approximate distance uphill of the location of interest"),
          
          ##Now they must input their PRISM csv file (no longer holding repository for it)
          fileInput("prismfile", "Upload PRISM CSV", accept = ".csv"),
          
          
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
      selectInput(inputId = "graph.choice", label = "Select A Graph to View",
                  choices = c("Moisture Categories", "Species Comparison"), selected = "Moisture Categories"),
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
      # tableOutput("analysis.table") %>% withSpinner(color = "#0dc5c1")
      dataTableOutput("analysis.table")%>% withSpinner(color = "#0dc5c1"),
      
      ########## downloads
      h3("Download Results"),
      downloadButton("download.slices", label = "Slices .csv"),
      downloadButton(outputId = "download.species", label = "Species recs .csv"),
      downloadButton(outputId = "download.chart", label = "Moisture chart .png")
      
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
               column(2, selectInput(inputId = "upper.text.mod", label = "Soil Texture",
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
               column(4, numericInput(inputId = "upper.depth.mod",label = "Topsoil Horizon Depth (cm)", value = NA)),
               column(4, numericInput(inputId = "dep.restr",label = "Depth to restrictive layer (cm)", value = NA)),
               column(4, numericInput(inputId = "ph.mod",label = "pH of soil profile", value = NA))
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
               column(2, selectInput(inputId = "subsoil.text.mod", label = "Soil Texture",
                                     choices = c("clay", "clay loam", "loam", "loamy sand", "sand", "sandy clay", "sandy clay loam", "sandy loam", "silt",
                                                 "silty clay", "silty clay loam", "silt loam", "organic","none"),
                                     selected = "none")),
               column(2, numericInput(inputId = "subsoil.om.mod",label = "OM (%)", value = NA)),
               column(2, numericInput(inputId = "subsoil.depth.mod", label = "Susboil Horizon depth (cm)", value = NA)),
               column(2, numericInput(inputId = "subsoil.frag.mod",label = "Frag Volume (%)", value = NA)),
               column(2, numericInput(inputId = "subsoil.bd.mod",label = "Bulk Density (g/cm^3)", value = NA)),
               column(2, numericInput(inputId = "subsoil.ksat.mod", label = "Ksat (mm/hr)",value = NA))
             ),
             
             
             fluidRow(
               column(12,
                      h3("Modifications to Bedrock Layer"))
             ),
             
             ###rows for modification (lower soil profile)
             fluidRow(
               column(2, selectInput(inputId = "restr.drain.type", label = "Drainage type",
                                     choices = c("Well-drained", "Poorly-drained", "none"),selected = "none"))
             )
             
             
             
             ),
    tabPanel("Hydrology Inputs",
             
            ##breif title 
            fluidRow(
              column(12,h2("Use this page to describe hydrology influencing you zone"),
              h3("Choose one type and enter data."),
              selectInput(inputId = "hydro.type", label = "Hydrology Profile",
                                   choices = c("man-made pond", "none"),
                                   selected = "none")),
            ),
          
            fluidRow(
              column(12, h3("For Man-made ponds and Drainage Basins")),
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
  
    
    
  ####get all inputs into separate input reactives so we can make tables and do whatever without issues
  zone_name <- reactive({input$zone.name})
  lat <- reactive({input$lat})
  long <- reactive({input$long})
  
  cc <- reactive({input$cc})
  leafon <- reactive({input$leafon})
  
  ####### slope (degrees)
  slope.deg <- reactive({ifelse(input$slope.deg <= 0, 5, input$slope.deg)})
  
  ###aspect
  aspect <- reactive({ifelse(input$aspect == "NONE", "S", input$aspect)})
  
  ####### Vegetation formation
  veg.form <- reactive({input$veg.form})
  
  veg.ht <- reactive({ifelse(!is.na(input$veg.ht), input$veg.ht, NA)})
  
  
  ####### calculate flow contribution area
  point.cfa <- reactive({
    
    #####cfa. calculations (now automated)
    hydro.tab <- get.cfa(lat(),long())
    
    point.cfa <- hydro.tab$cfa[1]
  }
  )
  
 
  
  
  
  
  #### create a table with all inputs and only ever refer to inputs from this table in all other areas of code
  ##put all of these into a table so they are organized
  target.data <- reactive({
    
   
    
    ####add all inputs to the table
    input.table <- data.frame(
      zone = c(zone_name()),
      lat = c(lat()),
      long = c(long()),
      canopy_cover_perc = c(cc()),
      leafon = c(leafon()),
      slope_degrees = c(slope.deg()),
      aspect = c(aspect()),
      veg_form = c(veg.form()),
      cfa = c(point.cfa()),
      ######### Non-required inputs
      
      vegetation_height_meters = c(veg.ht()),
      # cfa = c(point.cfa()),
      wt_mod = c(ifelse(!is.na(input$wt.mod), input$wt.mod, NA)),
      pH_mod = c(ifelse(!is.na(input$ph.mod),  input$ph.mod, NA)) ,
      
      ########## upper soil profile modifications (need to match columns profile_creator() recognizes)
      depth_mod = c(ifelse(!is.na(input$upper.depth.mod),input$upper.depth.mod, NA )),
      texture_mod = c(ifelse(input$upper.text.mod == "none", NA, input$upper.text.mod)),
      frag_mod = c(ifelse(!is.na(input$upper.frag.mod), input$upper.frag.mod, NA)), 
      om_mod = c(ifelse(!is.na(input$upper.om.mod),input$upper.om.mod,NA )),
      bd_mod = c(ifelse(!is.na(input$upper.bd.mod),input$upper.bd.mod, NA)),
      ksatmod_mmhr = c(ifelse(!is.na(input$upper.ksat.mod), input$upper.ksat.mod,NA )),
      
      ###full soil modification indicator
      full_soil_mod = c(input$full_soil_mod),

      #######subsoil stuff
      hz_depth_subsoil = c(ifelse(!is.na(input$subsoil.depth.mod), input$subsoil.depth.mod, NA)),
      text_subsoil = c(ifelse(input$subsoil.text.mod == "none", NA, input$subsoil.text.mod)),
      frag_subsoil = c(ifelse(!is.na(input$subsoil.frag.mod), input$subsoil.frag.mod, NA)),
      om_subsoil_mod = c(ifelse(!is.na(input$subsoil.om.mod), input$subsoil.om.mod,NA)),
      bd_subsoil = c(ifelse(!is.na(input$subsoil.bd.mod), input$subsoil.bd.mod, NA)),
      subsoil_ksat_mod = c(ifelse(!is.na(input$subsoil.ksat.mod), input$subsoil.ksat.mod,NA)),
      
      ###bedrock modifications
      # bedrock_ksat_mod = c(ifelse(!is.na(input$bed.ksat.mod), input$bed.ksat.mod,NA)),
      restr_drainage_type = c(ifelse(input$restr.drain.type == "none", NA, input$restr.drain.type)), ###restrive layer type
      dep_hardpan_or_bed = c(ifelse(!is.na(input$dep.restr),input$dep.restr, NA)),
      
      
      #### hydrologic stuff
      hydrologic_influence = c(ifelse(input$hydro.type == "none", NA, input$hydro.type)),
      pbanksl_deg = c(ifelse(!is.na(input$pbankslope), input$pbankslope, NA)),
      psurf_width_m = c(ifelse(!is.na(input$surfacewidth),input$surfacewidth,NA )),
      psurf_length_m = c(ifelse(!is.na(input$surfacelen), input$surfacelen,NA)),
      ponmdep_m = c(ifelse(!is.na(input$maxpdepth), input$maxpdepth,NA )),
      pos_rel_fullpond_cm = c(ifelse(!is.na(input$posrelfill),  input$posrelfill,NA ))
      
      
    )
    
    
    
  })
  
  
  
  
  
  
  
  ##################################### the rest of the code typically in the moisture model code
  ##need to build the blocks necessary for the final profile fit
  
  #####soil retrieved function
  soil.retrieved.block <- reactive({
    
   
    
    input.data <- target.data()
    
    
    
    ##call the retrieval function on the gps points
    soil.retrieved <- soil.data.retriever(input.data$lat[1], input.data$long[1])
    
  })
  
  ####ph calculator
  soil.ph <- reactive({
  
   
    
    input.data <- target.data()
    
    
    ###ph calculator
    ph <- ifelse(is.na(input.data$pH_mod[1]),wgt.pH.50(soil.table = soil.retrieved.block()), input.data$pH_mod[1])
    })
  
  
  ###formats the downloaded SSURGO data from above soil.retrieved.block() and formats it for feeding into vg.profile.fit in
  # the profile.full.block () reactive expression
  soil.layers.block <- reactive({
    
    ##previously computed soil block of data straight from SSURGO
    soil.retrieved <- soil.retrieved.block()
    
   
    
    ###streamlined soil. profile creator, need to have target data table available however
    ##target data is dataframe with all the inputs accounted for a present
    soil.profile <- profile_creator(soil.retr = soil.retrieved.block(), input.data = target.data())
    
    
    
    
    
    soil.profile.set <- soil.profile
    
    
  })
  
  
  ###pond data table calculation
  ##calculate needed pond data
  pond_table <- reactive({
    
    ##table with input data
    input.data <- target.data()
    
    pond_table <- pond_setup(bank_slope_deg = input.data$pbanksl_deg[1], avg_full_depth = input.data$ponmdep_m[1], surface_width = input.data$psurf_width_m[1],
               surface_length = input.data$psurf_length_m[1])
    
    
    
  }) 
  
  
  #### canopy cover block
  ####use cc calculator and ph retriever
  cc.table.block <-  reactive({
    
    validate(
      need(input$cc, "Please input site data")
    )
    
    input.data <- target.data()
    
    
    cc.calculator(ccinput = input.data$canopy_cover_perc[1], leaf_on = input.data$leafon[1], veg_form = input.data$veg_form[1])
    
  })
  
  ####takes the fit soil profile and prism data and outputs the 
  profile.full.block <- reactive({
    
    validate(
      need(input$lat, "Please input site data")
    )
    
    ###bring in target data table
    target.dat2 <- target.data()
    
    ###bring in pondtable 
    pond_table <- pond_table()
    
    
    
    ###try setting values so we don't have to reactive them later
    slope.deg <- target.dat2$slope_degrees[1]
    aspect <- target.dat2$aspect[1]
    point.cfa <- point.cfa()
    
    ###not an entered value (calculated to assist with seepage calculations)
    # slope.deg <- atan(slope/100) * (180/pi)
    seep.slope <- ifelse(slope.deg == 0, 45, slope.deg)##modify the slope of the seepage area if the target area is flat
    seep.angle.mod <- cos((90-seep.slope)* pi/180) ##because the downhill seep is flowing at an angle wee need to modify the flow rate (ksat * cos(angle))
    infil.angle.mod <- ifelse(slope.deg == 0, 0, cos((90-slope.deg)* pi/180))
    

    
    ###read in all the prism files location information to find closest prism
    # all.prisms <- prism.locations%>%
    #   mutate(distance = sqrt((lat_prism - lat())^2 + (long_prism-long())^2))
    # 
  
    ###get the closest prism
    # min.dist <- min(all.prisms$distance)
    
    ###subset out closest one
    # closest.prism <- subset(all.prisms, distance == min.dist)
    
    ###supply prism link in a characters string so we can rename file outputs automatically
    # prism.file <- paste(getwd(), "all_prisms", closest.prism$prism_file[1], sep = "/")
    
    ##extract sit name only needed for file saving purposes
    # site.extract.table <- read.table(text = prism.file, sep = "/", colClasses = "character", col.names = c("null", "dir", "site_name", "prism_file"))
    
    
    ###get the prism elevation
    elevation.m <- prism.elev(input$prismfile$datapath)
    
    
    
    ################### assign canopy parameters for PENMAN evaporation and GASH interception equations
    
    ##make table for intereption parameters
    ##need to add canopy cover for adjusting some roughness length parameters
    int.param.table <- gash.params%>%
      subset(veg_form == target.dat2$veg_form[1])%>%
      mutate(canopy_cover = target.dat2$canopy_cover_perc[1])
    
    ###get the parameters for the rs calculations
    veg_parameters <- veg.match(int.params = int.param.table)%>%
      mutate(canopy_ht_m = ifelse(!is.na(target.dat2$vegetation_height_meters[1]),target.dat2$vegetation_height_meters[1], canopy_ht_m ))
    
    ###calculate ra here since it is a static parameter (5/28/2024)
    ra.calc <- ra(veg_height = veg_parameters$canopy_ht_m[1], dcoeff = veg_parameters$d_coef[1])
    
    ###calculate the psychrometric constant
    psy.const <- psychrom(elevation.m)
    
    
    
    
    ################## Pull in the soil table data
    # soil.layers <- soil.layers.block()
    
    ##get the ph
    # ph_50cm <- ifelse(is.na(input$ph.mod),wgt.pH.50(soil.table = soil.layers.block()),target.data()$ph_mod[1]) 

  
    
    
    ##get vg params based on weighted soil profiles texture values
    ##the paw values are me testing to see if the way i'm calculating it matches the USDA
    ##updated to have field capacity stuff (4/23/2024)
    profile.fit <- vg.profile.fit(soil_profile = soil.layers.block())%>%
      mutate(cfa_m2 = point.cfa)%>%
      mutate(hz_depth = hzdepb_r - hzdept_r)%>%
      mutate(tot_adj_ws_cm = adj_theta_s*hz_depth,
             min_adj_ws_cm = adj_theta_r*hz_depth)%>%
      mutate(slope_deg = slope.deg, seep_slope = seep.slope,
             seep_angle_mod = seep.angle.mod, inf_ang_mod = infil.angle.mod)%>% ##need to set slope for seepage calculations
      mutate(theta_pwp_norm = wrc.theta(h_cm = 15300, theta_r = theta_r, theta_s = theta_s, alpha = alpha_cm, n = n, m = m),
             paw_tot_norm = (theta_s - theta_pwp_norm)*hz_depth,
             paw_tot_usda_adj = (theta_s*frag_frac - theta_pwp_norm*frag_frac)*hz_depth,
             theta_pwp = wrc.theta(h_cm = 15300, theta_r = adj_theta_r, theta_s = adj_theta_s, alpha = alpha_cm, n = n, m = m),
             paw_tot_me_adj = (adj_theta_s - theta_pwp)*hz_depth,
             theta_fc = wrc.theta(h_cm = 300, theta_r = adj_theta_r, theta_s = adj_theta_s, alpha = alpha_cm, n = n, m = m),
             ws_fc = theta_fc * hz_depth)%>%
      mutate(hydr_type = target.dat2$hydrologic_influence[1],pond_depth_m = target.dat2$ponmdep_m[1],relposfpond_cm = target.dat2$pos_rel_fullpond_cm[1], pbanksl_deg = target.dat2$pbanksl_deg[1],
             psurf_width_m = target.dat2$psurf_width_m[1], psurf_length_m = target.dat2$psurf_length_m[1],
             poi_lvl_cm = (pond_depth_m*100) + relposfpond_cm, ##the distance from the bottom of the pond to where our POI (point of interest) lies
             delx_full = psurf_width_m - pond_table$bot_length_m[1], pfullvol_m3 = pond_table$pvolFull_m3[1]) ##need max delta_x to calculate delta_x at each time stamp and get a surface area
    #if the slope is zero (when target area is flat flood plain) assume the slope of the contributing flow area is 25 degrees
    
    #if the slope is zero (when target area is flat flood plain) assume the slope of the contributing flow area is 25 degrees
    
    ###testing modification of water table to see what happends to curve
    # profile.fit$wtdepannmin = NA
    
    
    
    ####need to get the 0-100 cm weighted average PAW (take from soil.retrieved table)
    #in this analysis the Urban lands are ignored so if thats in the landscape the Web soil survey values will differ from this
    # awc_100 <- aws.calculator(soil.retrieved)
    
    
    
    ####################### Put together prism table and begin calculating interception
    ###generate the leaf off and leaf on data from cc.calculator
    cc.table <- cc.table.block()
    
    
    
    
    
    ######clean the prism file that the user has provided via input (this is now done with prism.cleaned() function)
    ##prism cleaning function 
    prism.cleaned <- prism.cleaning(prism.path = input$prismfile$datapath , soilprof = profile.fit, lat =target.dat2$lat[1],
                                    long = target.data2$long[1])
    
    ###clean the prism and match up well data if present
    # prism.cleaned <- read.csv(prism.file, skip = 10)%>%
    #   prism.cleaning()%>%
    #   mutate(vpdmean_kpa = ((vpdmin_hpa+vpdmax_hpa)/2)/10,
    #          week_num = week(ymd(date)),
    #          month_num = substr(date, 6, 7))%>%
    #   mutate(week_num = ifelse(week_num > 52, 52, week_num))%>% ##if a leap year week number becomes greater than 52, keep to 52 for averaging sake
    #   well.match(latitude = lat, longitude = long, gen.profile = profile.fit)
    
    
    #### Fetch well data if the minimum water table depth is present
    ###if wtdepannmin is present than fetch well data and add to the prism, else do
    ######match water table data if applicable included under the prism.cleaned now
    # prism.watertable <- well.match(latitude = lat(), longitude = long(), gen.profile = profile.fit, prism.table = prism.cleaned)
    
  
    prism.cc <- prism.cleaned%>%
      mutate(atm_tran = ifelse(precip_cm > 0, .25, 1))%>%
      mutate(canopy_cover = sapply(doy,cc.seasonal, cc.on = cc.table$leaf_on_perc[1], cc.off = cc.table$leaf_off_perc[1] ),
             rl = sapply(doy,cc.seasonal, cc.on = veg_parameters$rl[1], cc.off = 0),
             rs = rl/veg_parameters$peak_LAI) %>% ##have stomatal resistance increase as leaves come out
      mutate(rad_top_atm = sapply(doy, hargreaves.rad, latitude = target.dat2$lat[1]))%>%
      ##scaled evap is for the interception function that still uses the Hargreaves ET function
      mutate(scaled_evap_mmhr = mapply(gash.et, rad_top_atm, t_mean = tmean_c, t_min = tmin_c, t_max = tmax_c, canopy = canopy_cover)) %>%#refers to the changing canopy cover that calculated above
      ####ground evap is the evap taken from the ground, need to calculate PENMAN params for each day
      # mutate(windsp_ms = 2)%>% ##windspeed is default of two since we don't have this data
      mutate(Rn = mapply(net.radiation, doy = doy, elevation = elevation.m, slope_deg = target.dat2$slope_degrees[1], aspect = target.dat2$aspect[1], transmission = atm_tran, lat = target.dat2$lat[1], long = target.dat2$long[1], tmin = tmin_c, tmax = tmax_c, VPD = vpdmean_kpa,
                         cc = canopy_cover))%>%
      mutate(air_dense = mapply(air.dense, tempc = tmean_c, elev = elevation.m))%>%
      mutate(delta = sapply(tmean_c, delta.slope))%>%
      mutate(ground_evap_mmday = mapply(penman.et, Rn = Rn, ra = ra.calc, VPD = vpdmean_kpa, psychrom =psy.const, delta = delta, air_dense = air_dense, rs = rs ),
             pond_evap_mmday = mapply(penman.et, Rn = Rn, ra = ra.calc, VPD = vpdmean_kpa, psychrom =psy.const, delta = delta, air_dense = air_dense, rs = 0 ))%>%
      mutate(rain_dur_hrs = sapply(precip_cm, rainfall_duration_hrs))%>%
      mutate(rain_rate_cmhr = round(ifelse(precip_cm > 0,precip_cm/rain_dur_hrs, 0), digits = 2),
             rain_rate_mmhr = round(ifelse(precip_mm > 0,precip_mm/rain_dur_hrs, 0), digits = 2))%>% ##get PPG and then subsequent interception values
      mutate(Sc = canopy_cover*int.param.table$canopyS_mm[1])%>%
      mutate(PPG = mapply(PPG, rain_rate = rain_rate_mmhr, scaled_e = scaled_evap_mmhr, Sc = Sc, formation = target.dat2$veg_form[1]))%>%
      mutate(canopy_int_mm = mapply(canopy_int, precip = precip_mm, PPG = PPG, cc = canopy_cover, rain_rate = rain_rate_mmhr, scaled_evap = scaled_evap_mmhr, formation = target.dat2$veg_form[1]))%>%
      mutate(trunk_int_mm = mapply(trunk_int, pt = int.param.table$pt_mm[1], precip = precip_mm, PPT = int.param.table$Pt_mm[1], St = int.param.table$trunkS_mm[1]))%>%
      mutate(total_int_mm = trunk_int_mm + canopy_int_mm)%>%
      mutate(adj_precip_mm = ifelse(is.na(precip_mm - total_int_mm),0, round(precip_mm - total_int_mm, digits = 4 )),
             adj_prate_cmhr = ifelse(precip_mm > 0, round(((adj_precip_mm/10)/rain_dur_hrs), digits = 2), 0))%>% ##need adjusted rainfall rate for infiltration and runoff
      mutate(ground_evap_mmday = ifelse(ground_evap_mmday < 0, 0, ground_evap_mmday),
             pond_evap_mmday = ifelse(pond_evap_mmday < 0 , 0, pond_evap_mmday))%>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"),
             daylag = lag(date, default = first(date)-1))%>%
      group_by(grp = cumsum(precip_cm > 0))%>%
      mutate(last_rain = as.numeric(date - daylag[1]))%>%
      ungroup()%>%
      mutate(slope = target.dat2$slope_degrees[1], cfa_m2 = target.dat2$cfa[1])
    
    
    ###################### Create a Progress object and function needed for the progress bar to be set #####
    #####right now we are ignoring this because we would need to add the progress functionality to the water.balance() function itself
    ##this would add another layer of imports required and associated issues, we would need to duplicate the function and have a updateProgress argument
    
    # progress <- shiny::Progress$new()
    # progress$set(message = "Simulating Soil Moisture", value = 0)
    # # Close the progress when this reactive exits (even if there's an error)
    # on.exit(progress$close())
    # 
    # # Create a callback function to update progress.
    # # Each time this is called:
    # # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    # #   distance. If non-NULL, it will set the progress to that value.
    # # - It also accepts optional detail text.
    # updateProgress <- function(value = NULL, detail = NULL) {
    #   if (is.null(value)) {
    #     value <- progress$getValue()
    #     value <- value + (progress$getMax() - value) / length(prism.cc2$date)
    #   }
    #   progress$set(value = value, detail = detail)
    # }
    # 
    # 
    # 
    # ####generate the table with the soil moisture profile over the entire 20 years (or duration of user choosing depending on length of table input)
    # profile.full <- water.balance(filled.df = prism.cc(), soil.df.input = profile.fit(), updateProgress = updateProgress)
    #########################################################
    
    
    profile.full <- water.balance(filled.df = prism.cc, soil.df = profile.fit)
    
    
    
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
    
    
    validate(
      need(input$lat, "Please input site data")
    )
    target.data <- target.data()
    
    ###summarize the data
    psi.summary <- psi.wkly.summ(profile.full.block(), loc_name = target.data$zone[1])
    
    
    
  })
  
  ###slicer block, slices and formats for distance matrix comparison with species
  sliced.values <- reactive({
    
    validate(
      need(input$lat, "Please input site data")
    )
    
    ##make ccblock into a table we can get values from
    cc.table <- cc.table.block()
    
    
    
    ####pull in the data.summ.block
    data.summ <- data.summ.block()
    
    ###get the slices for other variables
    precip.summary <- precip.wkly.summ(profile.full.block()) ##replaces weekly.ppt code
    tempvpd.summary <- temp.vpd.wkly.sum(profile.full.block()) 
    
    ###evaluate the slices
    slices <- slicer(psiWeekSum = data.summ, weekly.ppt = precip.summary, weekly.tmpvpd = tempvpd.summary,ph = soil.ph(), cctable = cc.table.block())
    
   
  })
  
  #####bind together the overall species water data with the site data that has been generated, need to reformat post.transf and species.ov.psi.weekly
  
  plot.data <- reactive({
    
    validate(
      need(input$lat, "Please input site data")
    )
    
    target.data <- target.data()
    
    ###bring in the site data that was calculated
    site.data.formatted <- data.summ.block()%>%
      dplyr::mutate(species = target.data$zone[1])%>%
      dplyr::select(species,week_num, log_geomm_min, log_geomm_max)
    
    ####subset out the species of interest
    spp.subset <- spec.ov.data%>%
      base::subset(species == input$spp.sel)
    
    ####bind them together (this is the data set fed into the plot output)
    data.compare <- rbind(site.data.formatted, spp.subset)
    
    
  })
  
  ####analyze species separately for download
  spec.analysis <- reactive({
    
    species.analyzed <- suitability.analyzer(site.sliced = sliced.values(), species.slices = species.slice.data, species.ph.cc = spec.ph.cc.all)
    
    
  })
  
  
  #####general soil moisture profile graph with relative moisture classes
  gen.moist.graph <- reactive({
    
    
    
    # browser()
    ####bring in target data
    target.data <- target.data()
    
    ###psi.summary reactive
    psi.summary <- data.summ.block()
    
    # browser()
    
    moisture.levels <- data.frame(week_num = rep(1:52,3),
                                  condition = c(rep("wet", 52), rep("moist/mesic",52), rep("dry",52)),
                                  geommean_min_psi = c(rep(-2.5,52), rep(-5,52), rep(ifelse(min(psi.summary$log_geomm_min) >= -15, -15,min(psi.summary$log_geomm_min) ),52)),
                                  geommean_max_psi = c(rep(0,52), rep(-2.5,52), rep(-5,52)))
    
    ####put site in context of general soil moisture classes
    train.graph <- ggplot()+
        geom_ribbon(data = moisture.levels, aes(x = week_num, ymin = geommean_min_psi, ymax = geommean_max_psi, fill = condition), alpha = 0.4)+ #
        geom_ribbon(data = psi.summary , aes(x = week_num, ymin = log_geomm_min, ymax = log_geomm_max, colour = zone), fill = "cadetblue2", alpha = 0.5)+
        scale_color_manual(values = "black")+
        # ggtitle("Raw median min and max transformed")+
        # guides(fill=guide_legend(title="Moisture Level"))+
        ggtitle(paste(target.data$zone[1],"Average Annual Soil Moisture", sep = " "))+
        xlab("Week (1-52)")+
        ylab("Water Potential (cm of head)")+
        theme_bw()
    
    train.graph
    
    
  })
  
  #####plot the graph
  output$log_geomm_psi_minmax <- renderPlot({
    
    validate(
      need(input$lat, "Please input site data")
    )
    
    if (input$graph.choice == "Moisture Categories") {
      
      
      ####if they choose to see the general moisture breakdown show that graph
      gen.moist.graph()
      
      
    } else if (input$graph.choice == "Species Comparison"){ ###if they want to compare species show them the species graph
      
      target.data <- target.data()
      
      ggplot(data = plot.data(), aes(x = week_num))+
        geom_ribbon(aes(ymin = log_geomm_min, ymax = log_geomm_max, fill = species), alpha = 0.3)+ #
        ggtitle( paste(target.data$zone[1],"soil moisture range (22 years)", sep = " "))+
        xlab("Week (1-52)")+
        ylim(-15,0)+
        ylab("Log(Water Potential (cm of head))")
      
      
      
    }
    
    
    
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
  #renderTable
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
  ##renderTable
  output$analysis.table <- renderDataTable({
    
    validate(
      need(input$lat, "Please input site data")
    )
    
    
    ###compare against the other species
    species.analyzed <- spec.analysis()
    
    
  }, options = list(pageLength = 12))
  
  
  
  
  ############################### DOWNLOADS #####################################
  
  ######download for the slices 
  output$download.slices <- downloadHandler(
    
    
    filename = function() {
      
      ###bring in target data
      target.data <- target.data()
      
      paste(paste(target.data$zone[1],"slices", sep = "_"), ".csv", sep="")
    },
    
    
    content = function(file) {
      write.csv(sliced.values(), file)
    }
    ) ##download handler bracket
  
  ############### species recommendations table download
  output$download.species <- downloadHandler(
    
    filename = function() {
      
      ###bring in target data
      target.data <- target.data()
      
      paste(paste(target.data$zone[1],"species_recs",sep = "_"), ".csv", sep="")
    },
    
    
    content = function(file) {
      write.csv(spec.analysis(), file)
    }
    
  )
  
  ##### download export for the chart
  output$download.chart <- downloadHandler(
    
    filename = function() {
      ###bring in target data
      target.data <- target.data()
      
      paste(paste(target.data$zone[1],"zone_moisture_plot", sep = "_"), ".png", sep="")
    },
    
    
    content = function(file) {
      ggsave(file, plot = gen.moist.graph(),width = 6.5, height = 3.5, dpi = 600 )
    }
    
    
  )
  
  
  
  
}

# Create Shiny app  
shinyApp(ui = ui, server = server)









