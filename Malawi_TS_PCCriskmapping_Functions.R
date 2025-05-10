#=============================================================================================================#
#                     PCC risk mapping analysis  - Malawi analysis                                            #
#=============================================================================================================#


# ==================================== #
#        FUNCTIONS                     #
# ==================================== #


# ============================== #
#   Spatial statistics           #
# ============================== #

create_grid_and_geodata_sf <- function(spatial_obj, cellsize, x_coord, y_coord, geo_dat) {
  
  # Step 1: Create grid of points within the spatial object
  grid <- st_make_grid(spatial_obj, cellsize = cellsize, what = "centers")
  
  # Step 2: Convert grid to an sf object
  grid_sf <- st_sf(geometry = grid)
  
  # Step 3: Convert geodata_obj to an sf object
  geodata_obj_sf <- st_as_sf(geo_dat, coords = c(x_coord, y_coord), crs = st_crs(spatial_obj))
  
  # Optional: Plot to check if everything looks correct
  plot(st_geometry(spatial_obj), col = "lightgrey", main = "Grid and Geodata Object")
  plot(st_geometry(grid_sf), add = TRUE, col = "red", pch = 16)
  plot(st_geometry(geodata_obj_sf), add = TRUE, col = "blue", pch = 1)
  
  # Return the grid_sf and geodata_obj_sf
  return(list(grid_sf = grid_sf, geodata_obj_sf = geodata_obj_sf))
}

# ========================================================================= #
# Define the function to calculate and fit variograms with multiple models  #

calculate_variogram_fit <- function(geodata_obj_sf, formula, models = c("Sph", "Exp", "Gau", "Wav")) {
  
  # Convert to SpatialPointsDataFrame for variogram calculation
  geodata_obj_sp <- as_Spatial(geodata_obj_sf)
  
  # Calculate the empirical variogram
  vgm <- variogram(formula, geodata_obj_sp)
  
  # Create an empty list to store the fitted models
  fit_models <- list()
  
  # Fit variogram models
  for (model in models) {
    fit_models[[model]] <- fit.variogram(vgm, model = vgm(model))
  }
  
  # Return the fitted models, the empirical variogram, and the saved plot
  return(list(vgm = vgm, fit_models = fit_models))
}

# ==================================== #
# Define the generic kriging function  #

kriging_and_plot <- function(geodata_obj_sf, grid_sf, formula, model_fit, admin_sf, resolution = 0.01) {
  
  # Convert SF objects to SpatialPointsDataFrame (needed for kriging)
  geodata_obj_sp <- as_Spatial(geodata_obj_sf)
  grid_sp <- as_Spatial(grid_sf)
  
  # Perform Kriging
  kriged <- krige(formula, geodata_obj_sp, grid_sp, model = model_fit)
  
  # Convert kriged output to an SF object
  kriged_sf <- st_as_sf(kriged)
  
  # Convert the SF object to a data frame with coordinates
  kriged_sf <- kriged_sf %>%
    mutate(x = st_coordinates(kriged_sf)[,1],
           y = st_coordinates(kriged_sf)[,2])
  
  # Plot the Kriged surface
  ggplot(kriged_sf) +
    geom_tile(aes(x = x, y = y, fill = var1.pred)) +  # Adjust x, y, and var1.pred as needed
    coord_equal() +
    scale_fill_gradient(low = "yellow", high = "red") +
    geom_sf(data = admin_sf, fill = NA, colour = "black", alpha = 0.7) +
    theme_bw() +
    labs(x = "Longitude", y = "Latitude", fill = "Modelled proportion of HH with low sanitation index") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels
  
  # Convert to SpatVector using terra
  v <- vect(kriged)  # Convert SF to SpatVector (terra's equivalent of SF object)
  print(names(v))  # Print the names of the variables in the SpatVector
  
  # Create raster from the SpatVector
  r <- rast(v, res = resolution)  # Create a raster with the given resolution
  r_kriged <- rasterize(v, r, "var1.pred")  # Rasterize using the 'var1.pred' column from the SpatVector
  plot(r_kriged, main = "Rasterized Kriging Predictions")  # Plot the raster
  
  # Convert the rasterized results to a data frame for ggplot
  r_kriged_df <- as.data.frame(r_kriged, xy = TRUE, na.rm = TRUE)
  
  # Plot the rasterized kriged predictions
  ggplot(r_kriged_df) +
    geom_tile(aes(x = x, y = y, fill = last)) +
    scale_fill_viridis_c() +  # Choose a color scale for continuous data
    coord_equal() +           # Ensures tiles are square
    geom_sf(data = admin_sf, fill = NA, colour = "black", alpha = 0.7) +
    theme_bw() +
    labs(title = "Kriged Prediction Surface", x = "Longitude", y = "Latitude", fill = "Kriged predictions of proportion of HH with low sanitation index") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels
  
  # Return the kriged object, rasterized results, and plots
  return(list(kriged_sf = kriged_sf, r_kriged = r_kriged, r_kriged_df = r_kriged_df))
}

# ====================================== #
# Extract pig population raster function #

Extract_plot_raster_with_breaks <- function(raster_file, breakpoints = c(0, 1, 2, 5, 10, 25, 75, 250, 1000, 3000), 
                                            colors = c("gray90", rainbow_hcl(7)), admin_sf = NULL) {
  
  # Load the raster file
  ppop_raster <- raster(raster_file)  # Replace with any raster file input
  
  # Plot the raster with specified breaks and colors
  plot(ppop_raster, breaks = breakpoints, col = colors, main = "Raster Plot")
  
  # Return the raster object (if needed for further analysis)
  return(ppop_raster)
}

# ============================================================ #
# Function to plot the histograms & 3rd quantile on one plot   #

plot_histograms_with_quantiles <- function(wc2, w2, ppop_malawi) {
  
  # Combine all data into one data frame for ggplot
  wc2_values <- values(wc2)
  w2_values <- values(w2)
  ppop_values <- values(ppop_malawi)
  
  data <- data.frame(
    value = c(wc2_values, w2_values, ppop_values),
    variable = rep(c("Sanitation", "Poverty", "Pig population Density"), 
                   c(length(wc2_values), length(w2_values), length(ppop_values)))
  )
  
  # Calculate the quartiles for each variable
  quartiles <- data %>%
    group_by(variable) %>%
    summarise(
      lower_quartile = quantile(value, 0.25, na.rm = TRUE),
      median = quantile(value, 0.5, na.rm = TRUE),
      upper_quartile = quantile(value, 0.75, na.rm = TRUE)
    )
  
  # Create ggplot for histograms with quantiles
  ggplot(data, aes(x = value)) +
    geom_histogram(binwidth = 0.01, fill = "lightgray", color = "black", alpha = 0.7) +
    facet_wrap(~variable, scales = "free", nrow = 1) +  # Facet by variable
    geom_vline(data = quartiles, aes(xintercept = lower_quartile), color = "green", linetype = "dashed") +
    geom_vline(data = quartiles, aes(xintercept = median), color = "blue", linetype = "dashed") +
    geom_vline(data = quartiles, aes(xintercept = upper_quartile), color = "red", linetype = "dashed") +
    geom_text(data = quartiles, aes(x = upper_quartile, y = 5, label = paste("3rd Quantile:", round(upper_quartile, 4))),
              color = "red", size = 3, hjust = -0.1) +
    theme_minimal() +
    labs(title = "Histograms of Sanitation, Poverty, and Pig population Density", x = "Value", y = "Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
}

# ================================================================= #
# Function for threshold classification based on the third quantile #

threshold_by_quantile <- function(input_raster, plot_title = "Histogram of Values", breaks = 30) {
  
  # Extract the values from the raster
  values_data <- values(input_raster)
  
  # Plot the histogram of the raster values
  hist(values_data, main = plot_title, xlab = "Values", breaks = breaks, col = "lightgray", border = "black")
  
  # Calculate the quartiles and median
  quartiles <- quantile(values_data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  median_val <- quartiles[2]
  lower_quartile <- quartiles[1]
  upper_quartile <- quartiles[3]
  
  # Add vertical lines for the median, lower quartile, and upper quartile
  abline(v = median_val, col = "blue", lwd = 2, lty = 2)
  abline(v = lower_quartile, col = "green", lwd = 2, lty = 2)
  abline(v = upper_quartile, col = "red", lwd = 2, lty = 2)
  
  # Add a shaded region for values above the third quartile (threshold)
  rect(xleft = upper_quartile, ybottom = 0, xright = max(values_data, na.rm = TRUE), ytop = par("usr")[4],
       col = rgb(1, 0, 0, alpha = 0.2), border = NA)
  
  # # Add a text label for the 3rd quartile (upper quartile)
  # text(upper_quartile, max(density(values_data)$y) * 0.8, 
  #      labels = paste("3rd Quantile:", round(upper_quartile, 4)), col = "red", pos = 4)
  # 
  # Return the upper quartile threshold value
  return(upper_quartile)
}

# ============================================================================= #
# Function to process all layers (sanitation, poverty, and population density)  #
process_all_data <- function(wc2, w2, ppop_malawi) {
  
  # 1) Sanitation classification based on the 3rd quantile of wc2
  threshold_wc2 <- threshold_by_quantile(wc2, plot_title = "Histogram of low sanitation Values")
  wc2_b <- wc2
  # wc2_b[wc2 > 0.07165] <- 1  # Classification based on 3rd quantile (high = 1)
  # wc2_b[wc2_b <= 0.07165] <- 0  # Classification (low = 0)
  wc2_b[wc2 > threshold_wc2] <- 1  # Classification based on 3rd quantile (high = 1)
  wc2_b[wc2_b <= threshold_wc2] <- 0  # Classification (low = 0)
  plot(wc2_b)
  
  # 2) Poverty classification based on the 3rd quantile of w2
  threshold_w2 <- threshold_by_quantile(w2, plot_title = "Histogram of high poverty Values")
  w2_b <- w2
  # w2_b[w2 > 0.43826] <- 1
  # w2_b[w2_b <= 0.43826] <- 0
  w2_b[w2 > threshold_w2] <- 1
  w2_b[w2_b <= threshold_w2] <- 0
  plot(w2_b)
  
  # 3) Population density classification based on the 3rd quantile of ppop_malawi
  threshold_ppop <- threshold_by_quantile(ppop_malawi, plot_title = "Histogram of Population Density Values")
  Pg_den_b2 <- ppop_malawi
  # Pg_den_b2[ppop_malawi > threshold_ppop] <- 1
  # Pg_den_b2[Pg_den_b2 <= threshold_ppop] <- 0
  Pg_den_b2[ppop_malawi > 1] <- 1
  Pg_den_b2[Pg_den_b2 < 1] <- 0
  plot(Pg_den_b2)
  
  # Return the results as a list of classified rasters
  return(list(sanitation = wc2_b, poverty = w2_b, population_density = Pg_den_b2))
}

# ====================================================== #
# Define the function to read Lake Malawi data using sf  #
lake_MWI_func <- function(file_path) {
  
  # Use st_read() to read the shapefile from the local path using the dsn parameter
  lakes_sf <- st_read(dsn = file_path)  # Reads shapefile into sf object
  
  # Select relevant columns and filter for Lake Malawi
  lakes_MWI <- lakes_sf %>% dplyr::select(name, name_en)
  lakes_MWI <- lakes_MWI %>% filter(name %in% c("Lake Malawi"))
  
  return(lakes_MWI)
}

# ========================================================================== #
# Define the generic function for preparing & plotting the risk factor maps  #
plot_risk_factors <- function(wc2_b, Pg_den_b2, w2_b, admin_sf, breakpoints, breakpoints2, colors, Lake_malawi_obj, Year) {
  
  # If wc2_b_2016 is a SpatRaster, convert Pg_den_b2_2016 to SpatRaster
  Pg_den_b2 <- rast(Pg_den_b2)
  
  # Resample Pg_den_b2_2016 and w2_b_2016 to match the extent and resolution of wc2_b_2016
  Pg_den_b2_resampled <- resample(Pg_den_b2, wc2_b, method = "bilinear")
  w2_b_resampled <- resample(w2_b, wc2_b, method = "bilinear")
  
  # Perform the arithmetic operation to combine the risk factors
  over <- wc2_b + Pg_den_b2_resampled * 1.01 + w2_b_resampled * 1.1
  
  # Mask the final raster with the administrative boundaries
  over <- raster::mask(over, admin_sf)
  
  # Convert to data frame for plotting
  riskfact_df <- as.data.frame(over, xy = TRUE, long = TRUE)
  
  # Classify into bins based on the provided breakpoints
  riskfact_df <- riskfact_df %>%
    mutate(risk_fact_bins = cut(last,
                                breaks = breakpoints, 
                                right = FALSE,
                                labels = breakpoints2))
  
  # Plot only A
  riskfact_df$only_A <- ifelse(riskfact_df$risk_fact_bins %in% c("A", "AB", "AC", "ABC"), "A", NA)
  
  # Plot only B
  riskfact_df$only_B <- ifelse(riskfact_df$risk_fact_bins %in% c("B", "AB", "BC", "ABC"), "B", NA)
  
  # Plot only C
  riskfact_df$only_C <- ifelse(riskfact_df$risk_fact_bins %in% c("C", "AC", "BC", "ABC"), "C", NA)
  
  riskfact_df$Year <- Year
  
  # Continuous risk factor plot
  continous_rf_plot <- ggplot() +
    geom_tile(data = riskfact_df, aes(x = x, y = y, fill = last)) +
    geom_sf(data = admin_sf, aes(), color = "black", alpha = 0) +
    geom_sf(data = lake_MWI_obj, fill = "lightblue", alpha = 0.8) + 
    scale_fill_gradient("classes", low = 'yellow', high = 'blue', na.value = NA) +
    coord_sf() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()) +
    ggtitle("Continuous Risk Factor Plot")
  
  # Discrete risk factor plot
  all_rf_plot <- ggplot() +
    geom_tile(data = riskfact_df, aes(x = x, y = y, fill = risk_fact_bins)) +
    geom_sf(data = admin_sf, aes(), color = "black", alpha = 0) +
    geom_sf(data = lake_MWI_obj, fill = "lightblue", alpha = 0.8) + 
    scale_fill_manual(name = "Class", values = colors, na.value = NA, na.translate = FALSE) +
    coord_sf() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggtitle("Discrete Risk Factor Plot")
  
  # Only A
  value_rf_A <- c("gold")
  only_A_plot <- ggplot() +
    geom_tile(data = riskfact_df, aes(x = x, y = y, fill = only_A)) +
    geom_sf(data = admin_sf, aes(), color = "black", alpha = 0) +
    geom_sf(data = lake_MWI_obj, fill = "lightblue", alpha = 0.8) + 
    scale_fill_manual(name = "Class", values = value_rf_A, na.value = NA, na.translate = FALSE) +
    coord_sf() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggtitle("Only A Risk Factor")
  
  # Only B
  value_rf_B <- c("darkorchid1")
  only_B_plot <- ggplot() +
    geom_tile(data = riskfact_df, aes(x = x, y = y, fill = only_B)) +
    geom_sf(data = admin_sf, aes(), color = "black", alpha = 0) +
    geom_sf(data = lake_MWI_obj, fill = "lightblue", alpha = 0.8) + 
    scale_fill_manual(name = "Class", values = value_rf_B, na.value = NA, na.translate = FALSE) +
    coord_sf() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggtitle("Only B Risk Factor")
  
  # Only C
  value_rf_C <- c("red1")
  only_C_plot <- ggplot() +
    geom_tile(data = riskfact_df, aes(x = x, y = y, fill = only_C)) +
    geom_sf(data = admin_sf, aes(), color = "black", alpha = 0) +
    geom_sf(data = lake_MWI_obj, fill = "lightblue", alpha = 0.8) + 
    scale_fill_manual(name = "Class", values = value_rf_C, na.value = NA, na.translate = FALSE) +
    coord_sf() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggtitle("Only C Risk Factor")
  
  # Return all plots as a list
  return(list(
    continous_rf_plot = continous_rf_plot,
    all_rf_plot = all_rf_plot,
    only_A_plot = only_A_plot,
    only_B_plot = only_B_plot,
    only_C_plot = only_C_plot,
    data = riskfact_df
  ))
}
