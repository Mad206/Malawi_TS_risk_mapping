#=============================================================================================================#
#                     PCC risk mapping analysis  - Malawi analysis                                            #
#=============================================================================================================#

# devtools::load_all()

# TO DO add description file

# ====================== #
#   Set working dir      #

#setwd("C:/Users/MattDixon/OneDrive - SCI Foundation/New from Nov 20' - Copy (2)/Malawi TS mapping/PCC risk mapping")
setwd("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Other work/Malawi TS/Malawi TS mapping - R code/PCC risk mapping")

#=================================== #
#  Load Malawi shape file & process  #
# ================================== #

shp2 <- st_read(dsn = "data/shape files/mwi_adm_nso_20181016_shp/mwi_admbnda_adm2_nso_20181016.shp")

shp_subnational <- shp2

# check that we plot Malawi #
ggplot() + 
  geom_sf(data = shp_subnational, aes(), fill = "grey80", colour = "grey90", alpha = 1) +
  coord_sf()

# Malawi subnational data #
admin  <- st_read(dsn = "data/shape files/mwi_adm_nso_20181016_shp/mwi_admbnda_adm2_nso_20181016.shp")

admin$ADM2_EN_label <- as.character(admin$ADM2_EN) # make col into character so can use function below for renaming

# ======================================================================================================= #
#                                    YEAR : 2000                                                          #
# ======================================================================================================= #

# ============== #
#   Load in      #
# ============== #

# 2000 DHS data # 

data_2000 <- read.spss("data/DHS/2000/MW_2000_DHS/MWHR41SV - DHS/MWHR41FL.SAV", 
                       to.data.frame = T,use.value.labels = FALSE)

geo_2000<- st_read(dsn = "data/DHS/2000/MW_2000_DHS/MWGE43FL - shape/MWGE43FL.shp")

# ================================================ #
# View data distribution for variables of interest #

# Wealth quintile (HV270)
# 1 "Poorest", 2 "Poorer", 3 "Middle", 4 "Richer", 5 "Richest"

wealth_2000 <- read.spss("data/DHS/2000/MW_2000_DHS/MWWI42SV - wealth/MWwi42fl.SAV", 
                         to.data.frame = T,use.value.labels = FALSE)

summary(wealth_2000$WLTHIND5)
hist(wealth_2000$WLTHIND5)
levels(wealth_2000$WLTHIND5)
wealth_count <- table(wealth_2000$WLTHIND5)
wealth_count 

# type of toilet 
# 10 "FLUSH TOILET", 11 "Flush to piped sewer system", 12 "Flush to septic tank", 13 "Flush to pit latrine", 
# 14 "Flush to somewhere else", 15 "Flush, don't know where", 20 "PIT TOILET LATRINE", 21 "Ventilated Improved Pit latrine (VIP)", 
# 22 "Pit latrine with slab", 23 "Pit latrine without slab/open pit", 30 "NO FACILITY", 31 "No facility/bush/field", 41 "Composting toilet"
# 42 "Bucket toilet", 43 "Hanging toilet/latrine", 96 "Other")
data_2000$HV205
summary(data_2000$HV205)
hist(data_2000$HV205)
levels(data_2000$HV205)

sanitation_count <- table(data_2000$HV205)
sanitation_count 

# ============================================ #
# recoding & cleaning household survey dataset # 

# sanitation #
data_2000$HV205
data_2000$wc1 <- ifelse(data_2000$HV205 == '31' | data_2000$HV205 == '25' | data_2000$HV205 == '24', 1, 0) # HV205 type of toilet facility; if either of (31 = no facility/bush/field; 25 = uncovered pit latrine with slab; 24 = uncovered pit latrine no slab) = 1
data_2000$wc1
sanitation_count2 <- table(data_2000$wc1)
sanitation_count2 

data_2000$wc2 <- ifelse(data_2000$HV205 == '31', 1, 0) # if no toilet facility/bush/field = 1 
data_2000$wc2
sanitation_count2 <- table(data_2000$wc2)
sanitation_count2 

# wealth #
data_2000$w1 <- ifelse(wealth_2000$WLTHIND5 == '1', 1, 0) # (HV270: wealth quintile); if = 1 (poorest or lowest 20%) = 1
data_2000$w2 <- ifelse(wealth_2000$WLTHIND5 == '2', 1, 0) # (HV270: wealth quintile); if = 2 (poorer or next to lowest 20%) = 1
data_2000$w3 <- ifelse(wealth_2000$WLTHIND5 == '3', 1, 0) # (HV270: wealth quintile); if = 3 (middle 20%) = 1
data_2000$w4 <- ifelse(wealth_2000$WLTHIND5 == '4', 1, 0) # (HV270: wealth quintile); if = 4 (richer or next to highest 20%) = 1
data_2000$w5 <- ifelse(wealth_2000$WLTHIND5 == '5', 1, 0) # (HV270: wealth quintile); if = 5 (richest or highest 20%) = 1

wealth_count <- table(data_2000$w1)
wealth_count 


# ============================================================== #
# recoding & cleaning associated geographic (shape file) dataset # 

myClu = geo_2000 # extract DHS data
myClu <- myClu[which(!myClu$LONGNUM == 0),] # subset (exclude Long & Lat = 0)

myClu = myClu[,c("DHSCC","DHSCLUST","DHSREGCO","DHSREGNA","URBAN_RURA", "ADM1NAME", "LONGNUM","LATNUM")] # subset on these col's
names(myClu) = c("Cc","HV001","REGCODE", "HV024","IsRural", "AdName","x","y", "geometry") # rename & replace col names

# temp <- aggregate(x = data_2000$Pg_num, by = list(data_2000$HV001), 'mean', na.rm = T) # aggregate/ average (mean) of pig number by cluster number (HV001)
# head(temp)
# myPosVec = match(myClu$HV001, temp$Group.1 ) # match clusters (e.g. Group.1) from household survey in temp (with an average pig number) to cluster in total list (geographic cluster i.e. HV001)
# myClu$Pg_num = temp$x[myPosVec] # add average pig number for each cluster to full table (i.e the geographic data) - now GPS and aggregated data together
# # so this calc is average num of pigs for all HHs in each cluster
# head(myClu) # see new col (Pg_num)


# # for Pg_hs & cluster #2 = 11/5 = 2.2 (so number of pigs/number of HHs with pigs in cluster)
# myPosVec = match(myClu$HV001,temp$Group.1 )
# myClu$Pg_hs = temp$x[myPosVec] # average number of pigs per HHs with pigs (for each cluster)
# head(myClu) # see new col (Pg_hs)

temp<- aggregate(x = data_2000$wc1, by = list(data_2000$HV001), 'mean', na.rm = T) # aggregate/avg number reporting no facility/bush field or uncovered latrine w/ or /wo slab per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$wc1 = temp$x[myPosVec] # avg number reporting no facility/bush field or uncovered latrine w/ or /wo slab (per cluster)
head(myClu) # see new col (wc1)

temp <- aggregate(x = data_2000$wc2, by = list(data_2000$HV001), 'mean', na.rm = T) # aggregate/avg number reporting no facility/bush field per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$wc2 = temp$x[myPosVec] # avg number reporting no facility/bush field (per cluster)
head(myClu) # see new col (wc2) - this will be the same as wc1 for 2016 DHS in Malawi as other types of latrines not encoded (uncovered pit etc)

temp <- aggregate(x = data_2000$w1, by = list(data_2000$HV001), 'mean', na.rm = T) # aggregate/avg number classified as poorest (1) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w1 = temp$x[myPosVec] # avg number classified as poorest (per cluster)
head(myClu) # see new col (w1)

temp <- aggregate(x = data_2000$w2, by = list(data_2000$HV001), 'mean', na.rm = T) # aggregate/avg number classified as poorer (2) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w2 = temp$x[myPosVec]  # avg number classified as poorer (per cluster) 
head(myClu) # see new col (w2)

temp <- aggregate(x = data_2000$w3, by = list(data_2000$HV001), 'mean', na.rm = T) # aggregate/avg number classified as middle (3) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w3 = temp$x[myPosVec] # avg number classified as middle (per cluster) 
head(myClu) # see new col (w3)

temp <- aggregate(x = data_2000$w4, by = list(data_2000$HV001), 'mean', na.rm = T) # aggregate/avg number classified as richer (4) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w4 = temp$x[myPosVec] # avg number classified as richer (per cluster) 
head(myClu) # see new col (w4)

temp <- aggregate(x = data_2000$w5, by = list(data_2000$HV001), 'mean', na.rm = T) # aggregate/avg number classified as richest (5) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w5 = temp$x[myPosVec] # avg number classified as richest (per cluster)
head(myClu) # see new col (w5)

# ============================== #
#   Spatial statistics           #
# ============================== #

# create_grid_and_geodata_sf function here

# Assuming shp2, cellsize, x, and y are defined appropriately
Grid_sf_out <- create_grid_and_geodata_sf(shp2, cellsize = 0.0083, x_coord = "x", y_coord = "y", myClu)

# Access the resulting sf objects
grid_sf <- Grid_sf_out$grid_sf
geodata_obj_sf <- Grid_sf_out$geodata_obj_sf

# Optional: Plot to check if everything looks correct
plot(st_geometry(shp2), col = "lightgrey", main = "Grid and Geodata Object")
plot(st_geometry(grid_sf), add = TRUE, col = "red", pch = 16)
plot(st_geometry(geodata_obj_sf), add = TRUE, col = "blue", pch = 1)


# ===================================== #
#     Sanitation variable               #
# ===================================== #

# plot raw variable #
ggplot() + 
  geom_sf(data = admin, aes(), fill = "grey80", colour = "grey90", alpha = 1) +
  geom_point(data = myClu, aes(x = x, y = y, color = wc2) ) + 
  scale_colour_gradientn(colours = heat.colors(12)) + 
  coord_sf()+
  theme_bw()+
  labs(x = "longitude",
       y = "latitude",
       colour = "Proportion of HH
with low sanitation index")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels to 45 degrees

# ================================================== #
# Calculate and fit variograms with multiple models  #

# calculate_variogram_fit function

# call function
# Assuming geodata_obj_sf is your sf object and wc2 is the dependent variable
result <- calculate_variogram_fit(geodata_obj_sf, wc2 ~ 1)

# Extract the variogram and fitted models
vgm <- result$vgm
fit_models <- result$fit_models

p <- plot(vgm, model = fit_models$Sph, main = "Spherical Fit")
p1 <- plot(vgm, model = fit_models$Exp, main = "Exponential Fit")
p2 <- plot(vgm, model = fit_models$Gau, main = "Gaussian Fit")
p3 <- plot(vgm, model = fit_models$Wav, main = "Wave Fit")

all_fit <- ggpubr::ggarrange(p, p1, p2, p3,
                             ncol = 2, nrow = 2,
                             labels = c("A", "B", "C", "D"))
all_fit

p_2000_sanitation <- p

# ======================================== #
#  choose fit - just on visual plots above #

chosen_fit <- fit_models$Sph # spherical model for sanitation

# ==================================== #
# conduct kriging procedure            #

# kriging_and_plot function

# Call the function to perform kriging and generate plots
kriging_out <- kriging_and_plot(geodata_obj_sf, grid_sf, wc2 ~ 1, chosen_fit, admin)

wc2 <- kriging_out$r_kriged # main output needed

# ===================================== #
#     Poverty variable                  #
# ===================================== #

# Example Usage:
# Assuming geodata_obj_sf is your sf object and wc2 is the dependent variable
result <- calculate_variogram_fit(geodata_obj_sf, (w1 = w2) ~ 1)

# Extract the variogram and fitted models
vgm <- result$vgm
fit_models <- result$fit_models

p <- plot(vgm, model = fit_models$Sph, main = "Spherical Fit")
p1 <- plot(vgm, model = fit_models$Exp, main = "Exponential Fit")
p2 <- plot(vgm, model = fit_models$Gau, main = "Gaussian Fit")
p3 <- plot(vgm, model = fit_models$Wav, main = "Wave Fit")

all_fit <- ggpubr::ggarrange(p, p1, p2, p3,
                             ncol = 2, nrow = 2,
                             labels = c("A", "B", "C", "D"))
all_fit

p_2000_poverty <- p # to save

# ======================================== #
#  choose fit - just on visual plots above #

chosen_fit <- fit_models$Sph # spherical model for sanitation

# Call the function to perform kriging and generate plots
kriging_out <- kriging_and_plot(geodata_obj_sf, grid_sf, (w1 = w2) ~ 1, chosen_fit, admin)

w2 <- kriging_out$r_kriged # main output needed

# ====================================== #
#         Pig density variable           #
# ====================================== #

# Extract pig population raster 
# Extract_plot_raster_with_breaks function
# call function
ppop_malawi <- Extract_plot_raster_with_breaks('ppop_malawi.tif', admin_sf = admin)


#===================================================================================================#
# Investigating the distribution of the key variables to define meaningful breaks into high and low #
# ==================================================================================================#

# ============================================================ #
# plot the histograms & 3rd quantile on one plot               #

# plot_histograms_with_quantiles function
plot_histograms_with_quantiles(wc2, w2, ppop_malawi)

# ================================================================= #
# Calculate threshold classification based on the third quantile    #

# process_all_data function 
# Process the data for sanitation, poverty, and population density classification
result <- process_all_data(wc2, w2, ppop_malawi)

# Access the classified rasters
wc2_b_2000 <- result$sanitation
w2_b_2000 <- result$poverty
Pg_den_b2_2000 <- result$population_density

# =================================== #
#    Plot risk factors                #
# =================================== #

# Extract lake malawi shape file
lake_MWI_obj <- lake_MWI_func(file_path = "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Other work/Malawi TS/Malawi TS mapping - R code/PCC risk mapping/ne_50m_lakes.shp")

breakpoints <- c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12)
breakpoints2 <- c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')
colors <- c("gray90", "gold","darkorchid1", "red1", "springgreen", 'orange', 'plum1', "brown")

# prepare and plot all risk factor maps
result <- plot_risk_factors(wc2_b_2000, Pg_den_b2_2000, w2_b_2000, admin, breakpoints, breakpoints2, colors, lake_MWI_obj, Year = 2000)

# To view the plots, you can print the individual plots:
print(result$continous_rf_plot)
print(result$all_rf_plot)
print(result$only_A_plot)
print(result$only_B_plot)
print(result$only_C_plot)

Risk_factor_output_2000 <- result

Risk_factor_output_2000_dataframe <- result$data

# ======================================================================================================= #
#                                    YEAR : 2004                                                          #
# ======================================================================================================= #

# ============== #
#   Load in      #
# ============== #

# 2004 DHS data # 

# data_2000 <- read.spss("data/DHS/2000/MW_2000_DHS/MWHR41SV - DHS/MWHR41FL.SAV", 
#                        to.data.frame = T,use.value.labels = FALSE)
# 
# geo_2000<- st_read(dsn = "data/DHS/2000/MW_2000_DHS/MWGE43FL - shape/MWGE43FL.shp")

data_2004 <- read.spss("data/DHS/2004/MW_2004_DHS/MWHR4ESV - DHS/MWHR4EFL.SAV", 
                       to.data.frame = T,use.value.labels = FALSE)
geo_2004<- st_read(dsn ="data/DHS/2004/MW_2004_DHS/MWGE4BFL - shape/MWGE4BFL.shp")

#============================================================ #
# Data cleaning  - DHS survey & GPS data                      #

# ================================================ #
# View data distribution for variables of interest #

# Wealth quintile (HV270)
# 1 "Poorest", 2 "Poorer", 3 "Middle", 4 "Richer", 5 "Richest"
summary(data_2004$HV270)
hist(data_2004$HV270)
levels(data_2004$HV270)

wealth_count <- table(data_2004$HV270)
wealth_count 

# type of toilet 
# 10 "FLUSH TOILET", 11 "Flush to piped sewer system", 12 "Flush to septic tank", 13 "Flush to pit latrine", 
# 14 "Flush to somewhere else", 15 "Flush, don't know where", 20 "PIT TOILET LATRINE", 21 "Ventilated Improved Pit latrine (VIP)", 
# 22 "Pit latrine with slab", 23 "Pit latrine without slab/open pit", 30 "NO FACILITY", 31 "No facility/bush/field", 41 "Composting toilet"
# 42 "Bucket toilet", 43 "Hanging toilet/latrine", 96 "Other")
data_2004$HV205
summary(data_2004$HV205)
hist(data_2004$HV205)
levels(data_2004$HV205)

sanitation_count <- table(data_2004$HV205)
sanitation_count 

# ============================================ #
# recoding & cleaning household survey dataset # 

# sanitation #
data_2004$HV205
data_2004$wc1 <- ifelse(data_2004$HV205 == '31' | data_2004$HV205 == '25' | data_2004$HV205 == '24', 1, 0) # HV205 type of toilet facility; if either of (31 = no facility/bush/field; 25 = uncovered pit latrine with slab; 24 = uncovered pit latrine no slab) = 1
data_2004$wc1
sanitation_count2 <- table(data_2004$wc1)
sanitation_count2 

data_2004$wc2 <- ifelse(data_2004$HV205 == '31', 1, 0) # if no toilet facility/bush/field = 1 
data_2004$wc2
sanitation_count2 <- table(data_2004$wc2)
sanitation_count2 

# wealth #
data_2004$w1 <- ifelse(data_2004$HV270 == '1', 1, 0) # (HV270: wealth quintile); if = 1 (poorest or lowest 20%) = 1
data_2004$w2 <- ifelse(data_2004$HV270 == '2', 1, 0) # (HV270: wealth quintile); if = 2 (poorer or next to lowest 20%) = 1
data_2004$w3 <- ifelse(data_2004$HV270 == '3', 1, 0) # (HV270: wealth quintile); if = 3 (middle 20%) = 1
data_2004$w4 <- ifelse(data_2004$HV270 == '4', 1, 0) # (HV270: wealth quintile); if = 4 (richer or next to highest 20%) = 1
data_2004$w5 <- ifelse(data_2004$HV270 == '5', 1, 0) # (HV270: wealth quintile); if = 5 (richest or highest 20%) = 1

# ============================================================== #
# recoding & cleaning associated geographic (shape file) dataset # 

myClu = geo_2004 # extract DHS data
myClu <- myClu[which(!myClu$LONGNUM == 0),] # subset (exclude Long & Lat = 0)

myClu = myClu[,c("DHSCC","DHSCLUST","DHSREGCO","DHSREGNA","URBAN_RURA", "ADM1NAME", "LONGNUM","LATNUM")] # subset on these col's
names(myClu) = c("Cc","HV001","REGCODE", "HV024","IsRural", "AdName","x","y", "geometry") # rename & replace col names

myPosVec = match(myClu$HV001, temp$Group.1 ) # match clusters (e.g. Group.1) from household survey in temp (with an average pig number) to cluster in total list (geographic cluster i.e. HV001)

# for Pg_hs & cluster #2 = 11/5 = 2.2 (so number of pigs/number of HHs with pigs in cluster)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$Pg_hs = temp$x[myPosVec] # average number of pigs per HHs with pigs (for each cluster)
head(myClu) # see new col (Pg_hs)

myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$Pg_h = temp$x[myPosVec] # average number of pigs across pig-raising HHs in each cluster
head(myClu) # see new col (Pg_h)

# temp<- aggregate(x = data_2004$Pg_2, by = list(data_2004$HV001), 'mean', na.rm = T) # aggregate/average number of HH (with 1 or 2 pigs) classified as rural by cluster (calc mean)
# myPosVec = match(myClu$HV001,temp$Group.1 )
# myClu$Pg_2 = temp$x[myPosVec] # average number of HH with 1 or 2 pigs in rural area (per cluster) 
# head(myClu) # see new col (Pg_2)

temp<- aggregate(x = data_2004$wc1, by = list(data_2004$HV001), 'mean', na.rm = T) # aggregate/avg number reporting no facility/bush field or uncovered latrine w/ or /wo slab per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$wc1 = temp$x[myPosVec] # avg number reporting no facility/bush field or uncovered latrine w/ or /wo slab (per cluster)
head(myClu) # see new col (wc1)

temp <- aggregate(x = data_2004$wc2, by = list(data_2004$HV001), 'mean', na.rm = T) # aggregate/avg number reporting no facility/bush field per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$wc2 = temp$x[myPosVec] # avg number reporting no facility/bush field (per cluster)
head(myClu) # see new col (wc2) - this will be the same as wc1 for 2016 DHS in Malawi as other types of latrines not encoded (uncovered pit etc)

temp <- aggregate(x = data_2004$w1, by = list(data_2004$HV001), 'mean', na.rm = T) # aggregate/avg number classified as poorest (1) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w1 = temp$x[myPosVec] # avg number classified as poorest (per cluster)
head(myClu) # see new col (w1)

temp <- aggregate(x = data_2004$w2, by = list(data_2004$HV001), 'mean', na.rm = T) # aggregate/avg number classified as poorer (2) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w2 = temp$x[myPosVec]  # avg number classified as poorer (per cluster) 
head(myClu) # see new col (w2)

temp <- aggregate(x = data_2004$w3, by = list(data_2004$HV001), 'mean', na.rm = T) # aggregate/avg number classified as middle (3) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w3 = temp$x[myPosVec] # avg number classified as middle (per cluster) 
head(myClu) # see new col (w3)

temp <- aggregate(x = data_2004$w4, by = list(data_2004$HV001), 'mean', na.rm = T) # aggregate/avg number classified as richer (4) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w4 = temp$x[myPosVec] # avg number classified as richer (per cluster) 
head(myClu) # see new col (w4)

temp <- aggregate(x = data_2004$w5, by = list(data_2004$HV001), 'mean', na.rm = T) # aggregate/avg number classified as richest (5) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w5 = temp$x[myPosVec] # avg number classified as richest (per cluster)
head(myClu) # see new col (w5)

# ============================== #
#   Spatial statistics           #
# ============================== #

# create_grid_and_geodata_sf function here

# Assuming shp2, cellsize, x, and y are defined appropriately
Grid_sf_out <- create_grid_and_geodata_sf(shp2, cellsize = 0.0083, x_coord = "x", y_coord = "y", myClu)

# Access the resulting sf objects
grid_sf <- Grid_sf_out$grid_sf
geodata_obj_sf <- Grid_sf_out$geodata_obj_sf

# Optional: Plot to check if everything looks correct
plot(st_geometry(shp2), col = "lightgrey", main = "Grid and Geodata Object")
plot(st_geometry(grid_sf), add = TRUE, col = "red", pch = 16)
plot(st_geometry(geodata_obj_sf), add = TRUE, col = "blue", pch = 1)


# ===================================== #
#     Sanitation variable               #
# ===================================== #

# plot raw variable #
ggplot() + 
  geom_sf(data = admin, aes(), fill = "grey80", colour = "grey90", alpha = 1) +
  geom_point(data = myClu, aes(x = x, y = y, color = wc2) ) + 
  scale_colour_gradientn(colours = heat.colors(12)) + 
  coord_sf()+
  theme_bw()+
  labs(x = "longitude",
       y = "latitude",
       colour = "Proportion of HH
with low sanitation index")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels to 45 degrees

# ================================================== #
# Calculate and fit variograms with multiple models  #

# calculate_variogram_fit function

# call function
# Assuming geodata_obj_sf is your sf object and wc2 is the dependent variable
result <- calculate_variogram_fit(geodata_obj_sf, wc2 ~ 1)

# Extract the variogram and fitted models
vgm <- result$vgm
fit_models <- result$fit_models

p <- plot(vgm, model = fit_models$Sph, main = "Spherical Fit")
p1 <- plot(vgm, model = fit_models$Exp, main = "Exponential Fit")
p2 <- plot(vgm, model = fit_models$Gau, main = "Gaussian Fit")
p3 <- plot(vgm, model = fit_models$Wav, main = "Wave Fit")

all_fit <- ggpubr::ggarrange(p, p1, p2, p3,
                             ncol = 2, nrow = 2,
                             labels = c("A", "B", "C", "D"))
all_fit

p_2004_sanitation <- p # save

# ======================================== #
#  choose fit - just on visual plots above #

chosen_fit <- fit_models$Sph # spherical model for sanitation

# ==================================== #
# conduct kriging procedure            #

# kriging_and_plot function

# Call the function to perform kriging and generate plots
kriging_out <- kriging_and_plot(geodata_obj_sf, grid_sf, wc2 ~ 1, chosen_fit, admin)

wc2 <- kriging_out$r_kriged # main output needed

# ===================================== #
#     Poverty variable                  #
# ===================================== #

# Example Usage:
# Assuming geodata_obj_sf is your sf object and wc2 is the dependent variable
result <- calculate_variogram_fit(geodata_obj_sf, (w1 = w2) ~ 1)

# Extract the variogram and fitted models
vgm <- result$vgm
fit_models <- result$fit_models

p <- plot(vgm, model = fit_models$Sph, main = "Spherical Fit")
p1 <- plot(vgm, model = fit_models$Exp, main = "Exponential Fit")
p2 <- plot(vgm, model = fit_models$Gau, main = "Gaussian Fit")
p3 <- plot(vgm, model = fit_models$Wav, main = "Wave Fit")

all_fit <- ggpubr::ggarrange(p, p1, p2, p3,
                             ncol = 2, nrow = 2,
                             labels = c("A", "B", "C", "D"))
all_fit

p_2004_poverty <- p # save 

# ======================================== #
#  choose fit - just on visual plots above #

chosen_fit <- fit_models$Sph # spherical model for sanitation

# Call the function to perform kriging and generate plots
kriging_out <- kriging_and_plot(geodata_obj_sf, grid_sf, (w1 = w2) ~ 1, chosen_fit, admin)

w2 <- kriging_out$r_kriged # main output needed

# ====================================== #
#         Pig density variable           #
# ====================================== #

# Extract pig population raster 
# Extract_plot_raster_with_breaks function
# call function
ppop_malawi <- Extract_plot_raster_with_breaks('ppop_malawi.tif', admin_sf = admin)

#===================================================================================================#
# Investigating the distribution of the key variables to define meaningful breaks into high and low #
# ==================================================================================================#

# ============================================================ #
# plot the histograms & 3rd quantile on one plot               #

# plot_histograms_with_quantiles function
plot_histograms_with_quantiles(wc2, w2, ppop_malawi)

# ================================================================= #
# Calculate threshold classification based on the third quantile    #

# process_all_data function 
# Process the data for sanitation, poverty, and population density classification
result <- process_all_data(wc2, w2, ppop_malawi)

# Access the classified rasters
wc2_b_2004 <- result$sanitation
w2_b_2004 <- result$poverty
Pg_den_b2_2004 <- result$population_density

# =================================== #
#    Plot risk factors                #
# =================================== #

# Extract lake malawi shape file
lake_MWI_obj <- lake_MWI_func(file_path = "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Other work/Malawi TS/Malawi TS mapping - R code/PCC risk mapping/ne_50m_lakes.shp")

breakpoints <- c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12)
breakpoints2 <- c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')
colors <- c("gray90", "gold","darkorchid1", "red1", "springgreen", 'orange', 'plum1', "brown")

# prepare and plot all risk factor maps
result <- plot_risk_factors(wc2_b_2004, Pg_den_b2_2004, w2_b_2004, admin, breakpoints, breakpoints2, colors, lake_MWI_obj, Year = 2004)

# To view the plots, you can print the individual plots:
print(result$continous_rf_plot)
print(result$all_rf_plot)
print(result$only_A_plot)
print(result$only_B_plot)
print(result$only_C_plot)

Risk_factor_output_2004 <- result

Risk_factor_output_2004_dataframe <- result$data

# ======================================================================================================= #
#                                    YEAR : 2010                                                          #
# ======================================================================================================= #

# ============== #
#   Load in      #
# ============== #

# 2010 DHS data # 

data_2010 <- read.spss("data/DHS/2010/MW_2010_DHS/MWHR61SV - DHS/MWHR61FL.SAV", 
                       to.data.frame = T,use.value.labels = FALSE)

geo_2010<- st_read(dsn ="data/DHS/2010/MW_2010_DHS/MWGE62FL - shape/MWGE62FL.shp")

#============================================================ #
# Data cleaning  - DHS survey & GPS data                      #

# ================================================ #
# View data distribution for variables of interest #

# Wealth quintile (HV270)
# 1 "Poorest", 2 "Poorer", 3 "Middle", 4 "Richer", 5 "Richest"
summary(data_2010$HV270)
hist(data_2010$HV270)
levels(data_2010$HV270)

wealth_count <- table(data_2010$HV270)
wealth_count 

# type of toilet 
# 10 "FLUSH TOILET", 11 "Flush to piped sewer system", 12 "Flush to septic tank", 13 "Flush to pit latrine", 
# 14 "Flush to somewhere else", 15 "Flush, don't know where", 20 "PIT TOILET LATRINE", 21 "Ventilated Improved Pit latrine (VIP)", 
# 22 "Pit latrine with slab", 23 "Pit latrine without slab/open pit", 30 "NO FACILITY", 31 "No facility/bush/field", 41 "Composting toilet"
# 42 "Bucket toilet", 43 "Hanging toilet/latrine", 96 "Other")
data_2010$HV205
summary(data_2010$HV205)
hist(data_2010$HV205)
levels(data_2010$HV205)

sanitation_count <- table(data_2010$HV205)
sanitation_count 


# ============================================ #
# recoding & cleaning household survey dataset # 

# pig ownership #
data_2010$Pg_num <- ifelse(data_2010$HV246G > 95.5, NA, data_2010$HV246G) # HG246G = Cs owns pigs: replace entries with >95 pigs with NA
data_2010$Pg_h <- ifelse (data_2010$Pg_num > 0, 1, 0) # if pig number > 0, specify 1 for datapoint (household with pigs)
#data_2016$Pg_h 
data_2010$Pg_2 <- ifelse (data_2010$Pg_num == 1 | data_2010$Pg_num == 2 & data_2010$HV025 == 'Rural', 1, 0) # (HV025 = rural v urban) if pig number is 1 or 2 & classified as rural = 1

# sanitation #
data_2010$HV205
data_2010$wc1 <- ifelse(data_2010$HV205 == '31' | data_2010$HV205 == '25' | data_2010$HV205 == '24', 1, 0) # HV205 type of toilet facility; if either of (31 = no facility/bush/field; 25 = uncovered pit latrine with slab; 24 = uncovered pit latrine no slab) = 1
data_2010$wc1
sanitation_count2 <- table(data_2010$wc1)
sanitation_count2 

data_2010$wc2 <- ifelse(data_2010$HV205 == '31', 1, 0) # if no toilet facility/bush/field = 1 
data_2010$wc2
sanitation_count2 <- table(data_2010$wc2)
sanitation_count2 

# wealth #
data_2010$w1 <- ifelse(data_2010$HV270 == '1', 1, 0) # (HV270: wealth quintile); if = 1 (poorest or lowest 20%) = 1
data_2010$w2 <- ifelse(data_2010$HV270 == '2', 1, 0) # (HV270: wealth quintile); if = 2 (poorer or next to lowest 20%) = 1
data_2010$w3 <- ifelse(data_2010$HV270 == '3', 1, 0) # (HV270: wealth quintile); if = 3 (middle 20%) = 1
data_2010$w4 <- ifelse(data_2010$HV270 == '4', 1, 0) # (HV270: wealth quintile); if = 4 (richer or next to highest 20%) = 1
data_2010$w5 <- ifelse(data_2010$HV270 == '5', 1, 0) # (HV270: wealth quintile); if = 5 (richest or highest 20%) = 1

# ============================================================== #
# recoding & cleaning associated geographic (shape file) dataset # 

myClu = geo_2010 # extract DHS data
myClu <- myClu[which(!myClu$LONGNUM == 0),] # subset (exclude Long & Lat = 0)

myClu = myClu[,c("DHSCC","DHSCLUST","DHSREGCO","DHSREGNA","URBAN_RURA", "ADM1NAME", "LONGNUM","LATNUM")] # subset on these col's
names(myClu) = c("Cc","HV001","REGCODE", "HV024","IsRural", "AdName","x","y", "geometry") # rename & replace col names

temp <- aggregate(x = data_2010$Pg_num, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/ average (mean) of pig number by cluster number (HV001)
head(temp)
myPosVec = match(myClu$HV001, temp$Group.1 ) # match clusters (e.g. Group.1) from household survey in temp (with an average pig number) to cluster in total list (geographic cluster i.e. HV001)
myClu$Pg_num = temp$x[myPosVec] # add average pig number for each cluster to full table (i.e the geographic data) - now GPS and aggregated data together
# so this calc is average num of pigs for all HHs in each cluster
head(myClu) # see new col (Pg_num)

temp <- aggregate(x=ifelse(data_2010$Pg_num == 0,NA,data_2010$Pg_num), by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate further (??) & create new vector replacing 0 with NA for pig number (to replace 0s in average pig number by cluster)
# this mean is different from Pg_num because no longer including 0 values for each cluster (replaced by NAs which are removed)
# i.e. for Pg_num & cluster #2 = 11/30 = 0.36667 (number of pigs/ total HHs in cluster); 
# for Pg_hs & cluster #2 = 11/5 = 2.2 (so number of pigs/number of HHs with pigs in cluster)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$Pg_hs = temp$x[myPosVec] # average number of pigs per HHs with pigs (for each cluster)
head(myClu) # see new col (Pg_hs)

temp <- aggregate(x = data_2010$Pg_h, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/average number of HH with pigs by cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$Pg_h = temp$x[myPosVec] # average number of pigs across pig-raising HHs in each cluster
head(myClu) # see new col (Pg_h)

temp<- aggregate(x = data_2010$Pg_2, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/average number of HH (with 1 or 2 pigs) classified as rural by cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$Pg_2 = temp$x[myPosVec] # average number of HH with 1 or 2 pigs in rural area (per cluster) 
head(myClu) # see new col (Pg_2)

temp<- aggregate(x = data_2010$wc1, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/avg number reporting no facility/bush field or uncovered latrine w/ or /wo slab per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$wc1 = temp$x[myPosVec] # avg number reporting no facility/bush field or uncovered latrine w/ or /wo slab (per cluster)
head(myClu) # see new col (wc1)

temp <- aggregate(x = data_2010$wc2, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/avg number reporting no facility/bush field per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$wc2 = temp$x[myPosVec] # avg number reporting no facility/bush field (per cluster)
head(myClu) # see new col (wc2) - this will be the same as wc1 for 2016 DHS in Malawi as other types of latrines not encoded (uncovered pit etc)

temp <- aggregate(x = data_2010$w1, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/avg number classified as poorest (1) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w1 = temp$x[myPosVec] # avg number classified as poorest (per cluster)
head(myClu) # see new col (w1)

temp <- aggregate(x = data_2010$w2, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/avg number classified as poorer (2) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w2 = temp$x[myPosVec]  # avg number classified as poorer (per cluster) 
head(myClu) # see new col (w2)

temp <- aggregate(x = data_2010$w3, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/avg number classified as middle (3) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w3 = temp$x[myPosVec] # avg number classified as middle (per cluster) 
head(myClu) # see new col (w3)

temp <- aggregate(x = data_2010$w4, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/avg number classified as richer (4) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w4 = temp$x[myPosVec] # avg number classified as richer (per cluster) 
head(myClu) # see new col (w4)

temp <- aggregate(x = data_2010$w5, by = list(data_2010$HV001), 'mean', na.rm = T) # aggregate/avg number classified as richest (5) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w5 = temp$x[myPosVec] # avg number classified as richest (per cluster)
head(myClu) # see new col (w5)

# ============================== #
#   Spatial statistics           #
# ============================== #

# create_grid_and_geodata_sf function here

# Assuming shp2, cellsize, x, and y are defined appropriately
Grid_sf_out <- create_grid_and_geodata_sf(shp2, cellsize = 0.0083, x_coord = "x", y_coord = "y", myClu)

# Access the resulting sf objects
grid_sf <- Grid_sf_out$grid_sf
geodata_obj_sf <- Grid_sf_out$geodata_obj_sf

# Optional: Plot to check if everything looks correct
plot(st_geometry(shp2), col = "lightgrey", main = "Grid and Geodata Object")
plot(st_geometry(grid_sf), add = TRUE, col = "red", pch = 16)
plot(st_geometry(geodata_obj_sf), add = TRUE, col = "blue", pch = 1)

# ===================================== #
#     Sanitation variable               #
# ===================================== #

# plot raw variable #
ggplot() + 
  geom_sf(data = admin, aes(), fill = "grey80", colour = "grey90", alpha = 1) +
  geom_point(data = myClu, aes(x = x, y = y, color = wc2) ) + 
  scale_colour_gradientn(colours = heat.colors(12)) + 
  coord_sf()+
  theme_bw()+
  labs(x = "longitude",
       y = "latitude",
       colour = "Proportion of HH
with low sanitation index")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels to 45 degrees

# ================================================== #
# Calculate and fit variograms with multiple models  #

# calculate_variogram_fit function

# call function
# Assuming geodata_obj_sf is your sf object and wc2 is the dependent variable
result <- calculate_variogram_fit(geodata_obj_sf, wc2 ~ 1)

# Extract the variogram and fitted models
vgm <- result$vgm
fit_models <- result$fit_models

p <- plot(vgm, model = fit_models$Sph, main = "Spherical Fit")
p1 <- plot(vgm, model = fit_models$Exp, main = "Exponential Fit")
p2 <- plot(vgm, model = fit_models$Gau, main = "Gaussian Fit")
p3 <- plot(vgm, model = fit_models$Wav, main = "Wave Fit")

all_fit <- ggpubr::ggarrange(p, p1, p2, p3,
                             ncol = 2, nrow = 2,
                             labels = c("A", "B", "C", "D"))
all_fit

p_2010_sanitation <- p

# ======================================== #
#  choose fit - just on visual plots above #

chosen_fit <- fit_models$Sph # spherical model for sanitation

# ==================================== #
# conduct kriging procedure            #

# kriging_and_plot function

# Call the function to perform kriging and generate plots
kriging_out <- kriging_and_plot(geodata_obj_sf, grid_sf, wc2 ~ 1, chosen_fit, admin)

wc2 <- kriging_out$r_kriged # main output needed

# ===================================== #
#     Poverty variable                  #
# ===================================== #

# Example Usage:
# Assuming geodata_obj_sf is your sf object and wc2 is the dependent variable
result <- calculate_variogram_fit(geodata_obj_sf, (w1 = w2) ~ 1)

# Extract the variogram and fitted models
vgm <- result$vgm
fit_models <- result$fit_models

p <- plot(vgm, model = fit_models$Sph, main = "Spherical Fit")
p1 <- plot(vgm, model = fit_models$Exp, main = "Exponential Fit")
p2 <- plot(vgm, model = fit_models$Gau, main = "Gaussian Fit")
p3 <- plot(vgm, model = fit_models$Wav, main = "Wave Fit")

all_fit <- ggpubr::ggarrange(p, p1, p2, p3,
                             ncol = 2, nrow = 2,
                             labels = c("A", "B", "C", "D"))
all_fit

p_2010_poverty <- p

# ======================================== #
#  choose fit - just on visual plots above #

chosen_fit <- fit_models$Sph # spherical model for sanitation

# Call the function to perform kriging and generate plots
kriging_out <- kriging_and_plot(geodata_obj_sf, grid_sf, (w1 = w2) ~ 1, chosen_fit, admin)

w2 <- kriging_out$r_kriged # main output needed

# ====================================== #
#         Pig density variable           #
# ====================================== #

# Extract pig population raster 
# Extract_plot_raster_with_breaks function
# call function
ppop_malawi <- Extract_plot_raster_with_breaks('ppop_malawi.tif', admin_sf = admin)

#===================================================================================================#
# Investigating the distribution of the key variables to define meaningful breaks into high and low #
# ==================================================================================================#

# ============================================================ #
# plot the histograms & 3rd quantile on one plot               #

# plot_histograms_with_quantiles function
plot_histograms_with_quantiles(wc2, w2, ppop_malawi)

# ================================================================= #
# Calculate threshold classification based on the third quantile    #

# process_all_data function 
# Process the data for sanitation, poverty, and population density classification
result <- process_all_data(wc2, w2, ppop_malawi)

# Access the classified rasters
wc2_b_2010 <- result$sanitation
w2_b_2010 <- result$poverty
Pg_den_b2_2010 <- result$population_density

# =================================== #
#    Plot risk factors                #
# =================================== #

# Extract lake malawi shape file
lake_MWI_obj <- lake_MWI_func(file_path = "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Other work/Malawi TS/Malawi TS mapping - R code/PCC risk mapping/ne_50m_lakes.shp")

breakpoints <- c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12)
breakpoints2 <- c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')
colors <- c("gray90", "gold","darkorchid1", "red1", "springgreen", 'orange', 'plum1', "brown")

# prepare and plot all risk factor maps
result <- plot_risk_factors(wc2_b_2010, Pg_den_b2_2010, w2_b_2010, admin, breakpoints, breakpoints2, colors, lake_MWI_obj, 2010)

# To view the plots, you can print the individual plots:
print(result$continous_rf_plot)
print(result$all_rf_plot)
print(result$only_A_plot)
print(result$only_B_plot)
print(result$only_C_plot)

Risk_factor_output_2010 <- result

Risk_factor_output_2010_dataframe <- result$data

# ======================================================================================================= #
#                                    YEAR : 2016                                                          #
# ======================================================================================================= #

# ============== #
#   Load in      #
# ============== #

data_2016 <- read.spss("data/DHS/2016/MW_2015-16_SAV/MWHR7ASV - DHS/MWHR7AFL.SAV", to.data.frame = T,use.value.labels = FALSE)

geo_2016 <- st_read(dsn = "data/DHS/2016_a/MWGE7AFL/MWGE7AFL.shp")

# =============================== #
#    Clean Malawi DHS data        #
# =============================== #

# ================================================ #
# View data distribution for variables of interest #

# Wealth quintile (HV270)
# 1 "Poorest", 2 "Poorer", 3 "Middle", 4 "Richer", 5 "Richest"
summary(data_2016$HV270)
hist(data_2016$HV270)
levels(data_2016$HV270)

wealth_count <- table(data_2016$HV270)
wealth_count 

# type of toilet 
# 10 "FLUSH TOILET", 11 "Flush to piped sewer system", 12 "Flush to septic tank", 13 "Flush to pit latrine", 
# 14 "Flush to somewhere else", 15 "Flush, don't know where", 20 "PIT TOILET LATRINE", 21 "Ventilated Improved Pit latrine (VIP)", 
# 22 "Pit latrine with slab", 23 "Pit latrine without slab/open pit", 30 "NO FACILITY", 31 "No facility/bush/field", 41 "Composting toilet"
# 42 "Bucket toilet", 43 "Hanging toilet/latrine", 96 "Other")
data_2016$HV205
summary(data_2016$HV205)
hist(data_2016$HV205)
levels(data_2016$HV205)

sanitation_count <- table(data_2016$HV205)
sanitation_count 

# pig ownership #
#   0 "None", 95 "95 or more", 98 "Unknown"
summary(data_2016$HV246G)
tail(data_2016$HV246G)
hist(data_2016$HV246G)
summary(data_2016$HV246G)

# ============================================ #
# recoding & cleaning household survey dataset # 

# pig ownership #
data_2016$Pg_num <- ifelse(data_2016$HV246G > 95.5, NA, data_2016$HV246G) # HG246G = Cs owns pigs: replace entries with >95 pigs with NA
data_2016$Pg_h <- ifelse (data_2016$Pg_num > 0, 1, 0) # if pig number > 0, specify 1 for datapoint (household with pigs)
#data_2016$Pg_h 
data_2016$Pg_2 <- ifelse (data_2016$Pg_num == 1 | data_2016$Pg_num == 2 & data_2016$HV025 == 'Rural', 1, 0) # (HV025 = rural v urban) if pig number is 1 or 2 & classified as rural = 1

# sanitation #
data_2016$HV205
data_2016$wc1 <- ifelse(data_2016$HV205 == '31' | data_2016$HV205 == '25' | data_2016$HV205 == '24', 1, 0) # HV205 type of toilet facility; if either of (31 = no facility/bush/field; 25 = uncovered pit latrine with slab; 24 = uncovered pit latrine no slab) = 1
data_2016$wc1
sanitation_count2 <- table(data_2016$wc1)
sanitation_count2 

data_2016$wc2 <- ifelse(data_2016$HV205 == '31', 1, 0) # if no toilet facility/bush/field = 1 
data_2016$wc2
sanitation_count2 <- table(data_2016$wc2)
sanitation_count2 

# wealth #
data_2016$w1 <- ifelse(data_2016$HV270 == '1', 1, 0) # (HV270: wealth quintile); if = 1 (poorest or lowest 20%) = 1
data_2016$w2 <- ifelse(data_2016$HV270 == '2', 1, 0) # (HV270: wealth quintile); if = 2 (poorer or next to lowest 20%) = 1
data_2016$w3 <- ifelse(data_2016$HV270 == '3', 1, 0) # (HV270: wealth quintile); if = 3 (middle 20%) = 1
data_2016$w4 <- ifelse(data_2016$HV270 == '4', 1, 0) # (HV270: wealth quintile); if = 4 (richer or next to highest 20%) = 1
data_2016$w5 <- ifelse(data_2016$HV270 == '5', 1, 0) # (HV270: wealth quintile); if = 5 (richest or highest 20%) = 1

# ============================================================== #
# recoding & cleaning associated geographic (shape file) dataset # 

#myClu = geo_2016@data # extract DHS data
myClu <- geo_2016
myClu <- myClu[which(!myClu$LONGNUM == 0),] # subset (exclude Long & Lat = 0)

myClu = myClu[,c("DHSCC","DHSCLUST","DHSREGCO","DHSREGNA","URBAN_RURA", "ADM1NAME", "LONGNUM","LATNUM")] # subset on these col's
names(myClu) = c("Cc","HV001","REGCODE", "HV024","IsRural", "AdName","x","y", "geometry") # rename & replace col names

temp <- aggregate(x = data_2016$Pg_num, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/ average (mean) of pig number by cluster number (HV001)
head(temp)
myPosVec = match(myClu$HV001, temp$Group.1 ) # match clusters (e.g. Group.1) from household survey in temp (with an average pig number) to cluster in total list (geographic cluster i.e. HV001)
myClu$Pg_num = temp$x[myPosVec] # add average pig number for each cluster to full table (i.e the geographic data) - now GPS and aggregated data together
# so this calc is average num of pigs for all HHs in each cluster
head(myClu) # see new col (Pg_num)

temp <- aggregate(x=ifelse(data_2016$Pg_num == 0,NA,data_2016$Pg_num), by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate further (??) & create new vector replacing 0 with NA for pig number (to replace 0s in average pig number by cluster)
# this mean is different from Pg_num because no longer including 0 values for each cluster (replaced by NAs which are removed)
# i.e. for Pg_num & cluster #2 = 11/30 = 0.36667 (number of pigs/ total HHs in cluster); 
# for Pg_hs & cluster #2 = 11/5 = 2.2 (so number of pigs/number of HHs with pigs in cluster)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$Pg_hs = temp$x[myPosVec] # average number of pigs per HHs with pigs (for each cluster)
head(myClu) # see new col (Pg_hs)

temp <- aggregate(x = data_2016$Pg_h, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/average number of HH with pigs by cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$Pg_h = temp$x[myPosVec] # average number of pigs across pig-raising HHs in each cluster
head(myClu) # see new col (Pg_h)

temp<- aggregate(x = data_2016$Pg_2, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/average number of HH (with 1 or 2 pigs) classified as rural by cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$Pg_2 = temp$x[myPosVec] # average number of HH with 1 or 2 pigs in rural area (per cluster) 
head(myClu) # see new col (Pg_2)

temp<- aggregate(x = data_2016$wc1, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/avg number reporting no facility/bush field or uncovered latrine w/ or /wo slab per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$wc1 = temp$x[myPosVec] # avg number reporting no facility/bush field or uncovered latrine w/ or /wo slab (per cluster)
head(myClu) # see new col (wc1)

temp <- aggregate(x = data_2016$wc2, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/avg number reporting no facility/bush field per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$wc2 = temp$x[myPosVec] # avg number reporting no facility/bush field (per cluster)
head(myClu) # see new col (wc2) - this will be the same as wc1 for 2016 DHS in Malawi as other types of latrines not encoded (uncovered pit etc)

temp <- aggregate(x = data_2016$w1, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/avg number classified as poorest (1) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w1 = temp$x[myPosVec] # avg number classified as poorest (per cluster)
head(myClu) # see new col (w1)

temp <- aggregate(x = data_2016$w2, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/avg number classified as poorer (2) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w2 = temp$x[myPosVec]  # avg number classified as poorer (per cluster) 
head(myClu) # see new col (w2)

temp <- aggregate(x = data_2016$w3, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/avg number classified as middle (3) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w3 = temp$x[myPosVec] # avg number classified as middle (per cluster) 
head(myClu) # see new col (w3)

temp <- aggregate(x = data_2016$w4, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/avg number classified as richer (4) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w4 = temp$x[myPosVec] # avg number classified as richer (per cluster) 
head(myClu) # see new col (w4)

temp <- aggregate(x = data_2016$w5, by = list(data_2016$HV001), 'mean', na.rm = T) # aggregate/avg number classified as richest (5) quintile per cluster (calc mean)
myPosVec = match(myClu$HV001,temp$Group.1 )
myClu$w5 = temp$x[myPosVec] # avg number classified as richest (per cluster)
head(myClu) # see new col (w5)

# ============================== #
#   Spatial statistics           #
# ============================== #

# create_grid_and_geodata_sf function here

# Assuming shp2, cellsize, x, and y are defined appropriately
Grid_sf_out <- create_grid_and_geodata_sf(shp2, cellsize = 0.0083, x_coord = "x", y_coord = "y", myClu)

# Access the resulting sf objects
grid_sf <- Grid_sf_out$grid_sf
geodata_obj_sf <- Grid_sf_out$geodata_obj_sf

# Optional: Plot to check if everything looks correct
plot(st_geometry(shp2), col = "lightgrey", main = "Grid and Geodata Object")
plot(st_geometry(grid_sf), add = TRUE, col = "red", pch = 16)
plot(st_geometry(geodata_obj_sf), add = TRUE, col = "blue", pch = 1)


# ===================================== #
#     Sanitation variable               #
# ===================================== #

# plot raw variable #
ggplot() + 
  geom_sf(data = admin, aes(), fill = "grey80", colour = "grey90", alpha = 1) +
  geom_point(data = myClu, aes(x = x, y = y, color = wc2) ) + 
  scale_colour_gradientn(colours = heat.colors(12)) + 
  coord_sf()+
  theme_bw()+
  labs(x = "longitude",
       y = "latitude",
       colour = "Proportion of HH
with low sanitation index")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels to 45 degrees

# ================================================== #
# Calculate and fit variograms with multiple models  #

 # calculate_variogram_fit function

# call function
# Assuming geodata_obj_sf is your sf object and wc2 is the dependent variable
result <- calculate_variogram_fit(geodata_obj_sf, wc2 ~ 1)

# Extract the variogram and fitted models
vgm <- result$vgm
fit_models <- result$fit_models

p <- plot(vgm, model = fit_models$Sph, main = "Spherical Fit")
p1 <- plot(vgm, model = fit_models$Exp, main = "Exponential Fit")
p2 <- plot(vgm, model = fit_models$Gau, main = "Gaussian Fit")
p3 <- plot(vgm, model = fit_models$Wav, main = "Wave Fit")

all_fit <- ggpubr::ggarrange(p, p1, p2, p3,
                  ncol = 2, nrow = 2,
                  labels = c("A", "B", "C", "D"))
all_fit

p_2016_sanitation <- p

# ======================================== #
#  choose fit - just on visual plots above #

chosen_fit <- fit_models$Sph # spherical model for sanitation

# ==================================== #
# conduct kriging procedure            #

# kriging_and_plot function

# Call the function to perform kriging and generate plots
kriging_out <- kriging_and_plot(geodata_obj_sf, grid_sf, wc2 ~ 1, chosen_fit, admin)

wc2 <- kriging_out$r_kriged # main output needed

# ===================================== #
#     Poverty variable                  #
# ===================================== #

# Example Usage:
# Assuming geodata_obj_sf is your sf object and wc2 is the dependent variable
result <- calculate_variogram_fit(geodata_obj_sf, (w1 = w2) ~ 1)

# Extract the variogram and fitted models
vgm <- result$vgm
fit_models <- result$fit_models

p <- plot(vgm, model = fit_models$Sph, main = "Spherical Fit")
p1 <- plot(vgm, model = fit_models$Exp, main = "Exponential Fit")
p2 <- plot(vgm, model = fit_models$Gau, main = "Gaussian Fit")
p3 <- plot(vgm, model = fit_models$Wav, main = "Wave Fit")

all_fit <- ggpubr::ggarrange(p, p1, p2, p3,
                             ncol = 2, nrow = 2,
                             labels = c("A", "B", "C", "D"))
all_fit

p_2016_poverty <- p

vgms_all_2025 <- list(p_2000_poverty, p_2000_sanitation,
                      p_2004_poverty, p_2004_sanitation,
                      p_2010_poverty, p_2010_sanitation,
                      p_2016_poverty, p_2016_sanitation)

saveRDS(vgms_all_2025, file = "vgms_all_2025.rds")


# ======================================== #
#  choose fit - just on visual plots above #

chosen_fit <- fit_models$Sph # spherical model for sanitation

# Call the function to perform kriging and generate plots
kriging_out <- kriging_and_plot(geodata_obj_sf, grid_sf, (w1 = w2) ~ 1, chosen_fit, admin)

w2 <- kriging_out$r_kriged # main output needed

# ====================================== #
#         Pig density variable           #
# ====================================== #

# Extract pig population raster 
# Extract_plot_raster_with_breaks function
# call function
ppop_malawi <- Extract_plot_raster_with_breaks('ppop_malawi.tif', admin_sf = admin)


#===================================================================================================#
# Investigating the distribution of the key variables to define meaningful breaks into high and low #
# ==================================================================================================#

# ============================================================ #
# plot the histograms & 3rd quantile on one plot               #

# plot_histograms_with_quantiles function
plot_histograms_with_quantiles(wc2, w2, ppop_malawi)

# ================================================================= #
# Calculate threshold classification based on the third quantile    #

# process_all_data function 
# Process the data for sanitation, poverty, and population density classification
result <- process_all_data(wc2, w2, ppop_malawi)

# Access the classified rasters
wc2_b_2016 <- result$sanitation
w2_b_2016 <- result$poverty
Pg_den_b2_2016 <- result$population_density

# =================================== #
#    Plot risk factors                #
# =================================== #

# Extract lake malawi shape file
lake_MWI_obj <- lake_MWI_func(file_path = "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Other work/Malawi TS/Malawi TS mapping - R code/PCC risk mapping/ne_50m_lakes.shp")

breakpoints <- c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12)
breakpoints2 <- c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')
colors <- c("gray90", "gold","darkorchid1", "red1", "springgreen", 'orange', 'plum1', "brown")

# prepare and plot all risk factor maps
result <- plot_risk_factors(wc2_b_2016, Pg_den_b2_2016, w2_b_2016, admin, breakpoints, breakpoints2, colors, lake_MWI_obj, 2016)

# To view the plots, you can print the individual plots:
print(result$continous_rf_plot)
print(result$all_rf_plot)
print(result$only_A_plot)
print(result$only_B_plot)
print(result$only_C_plot)

Risk_factor_output_2016 <- result

Risk_factor_output_2016_dataframe <- result$data
