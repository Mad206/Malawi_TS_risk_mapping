#=============================================================================================================#
#                     PCC risk mapping analysis  - Combined plotting across years                             #
#=============================================================================================================#

# =================================== #
#      Combine risk factor dataframe  #
# =================================== #

Risk_factor_allyrs_dataframe <- rbind(Risk_factor_output_2000_dataframe,
                                      Risk_factor_output_2004_dataframe,
                                      Risk_factor_output_2010_dataframe,
                                      Risk_factor_output_2016_dataframe)

# saveRDS(Risk_factor_allyrs_dataframe, file = "Risk_factor_allyrs_dataframe_2025.rds")

# Discrete risk factor plot

all_rf_plot_allyrs <- ggplot() +
  # Add the risk factor tiles
  geom_tile(data = Risk_factor_allyrs_dataframe, aes(x = x, y = y, fill = risk_fact_bins)) +
  scale_fill_manual(name = "Risk Factor", values = colors, labels = c("A" = "A = poor sanitation", 
                                                                      "B" = "B = high pig density", 
                                                                      "C" = "C = high poverty", 
                                                                      "AB" = "AB", 
                                                                      "AC" = "AC", 
                                                                      "BC" = "BC", 
                                                                      "ABC" = "ABC", 
                                                                      "all low" = "all low"), 
                    na.value = NA, na.translate = FALSE) +
  # Add administrative boundaries
  geom_sf(data = admin, aes(), color = "black", alpha = 0) +
  # Use ggnewscale to allow a second scale for the lake layer
  ggnewscale::new_scale_fill() +
  # Add the lake layer with lightblue color
  geom_sf(data = lake_MWI_obj, aes(fill = "Lake Malawi"), colour = NA, alpha = 0.75) +
  # Add the second scale for the lake legend
  scale_fill_manual(name = "", values = c("Lake Malawi" = "lightblue2"), 
                    labels = c("Lake Malawi"), guide = guide_legend(order = 2)) +
  # Set coordinates and theme
  coord_sf() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",  # Position legends at the bottom
        legend.box = "horizontal",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", size = 14),  # Make facet labels bold
        strip.background = element_blank()) +  # Remove background color for facet labels) +  # Make sure the legends are placed horizontally
  cowplot::panel_border(remove = TRUE) +
  # Facet by year
  facet_wrap(~Year, nrow = 1)

# Save the plot as a horizontal A4 PDF
ggsave(filename = "all_rf_plot_allyrs.pdf", 
       plot = all_rf_plot_allyrs, 
       device = "pdf", 
       width = 11.69,  # A4 width in inches
       height = 8.27, # A4 height in inches
       units = "in"  # Unit of measurement (inches)
       )


# ======================================= #
#    Make long-form risk factor dataframe #

Risk_factor_allyrs_dataframe_long <- Risk_factor_allyrs_dataframe %>%
  pivot_longer(cols = c(risk_fact_bins, only_A, only_B, only_C), 
               names_to = "risk_factor_type", 
               values_to = "risk_factor_value")

# View the long format dataframe
head(Risk_factor_allyrs_dataframe_long)

# Year 2000 #
risk_factor_2000_dataframe <- subset(Risk_factor_allyrs_dataframe_long, Year == 2000)

# Year 2004 #
risk_factor_2004_dataframe <- subset(Risk_factor_allyrs_dataframe_long, Year == 2004)

# Year 2010 #
risk_factor_2010_dataframe <- subset(Risk_factor_allyrs_dataframe_long, Year == 2010)

# Year 2016 #
risk_factor_2016_dataframe <- subset(Risk_factor_allyrs_dataframe_long, Year == 2016)


# =====#
# Plot #
#colors <- c("gray90", "gold","darkorchid1", "red1", "springgreen", 'orange', 'plum1', "brown")
# color_mapping <- c("all low" = "gray90", "not present" = "grey45", "A" = "gold",
#                "B" = "darkorchid", "C" = "red1", "AB" = "springgreen", "AC" = "orange",
#                "BC"= "plum1", "ABC" = "brown")


# Discrete risk factor plot
all_rf_plot_specific_yr <- ggplot() +
  geom_tile(data = risk_factor_2016_dataframe, aes(x = x, y = y, fill = factor(risk_factor_value, levels = c("all low", "A", "B", "C", "AB", "AC", "BC", "ABC")))) +
  scale_fill_manual(name = "Risk Factor", values = colors, labels = c("A" = "A = poor sanitation", 
                                                                      "B" = "B = high pig density", 
                                                                      "C" = "C = high poverty", 
                                                                      "AB" = "AB", 
                                                                      "AC" = "AC", 
                                                                      "BC" = "BC", 
                                                                      "ABC" = "ABC", 
                                                                      "all low" = "all low"), 
                    na.value = NA, na.translate = FALSE) +
  # Add administrative boundaries
  geom_sf(data = admin, aes(), color = "black", alpha = 0) +
  # Use ggnewscale to allow a second scale for the lake layer
  ggnewscale::new_scale_fill() +
  # Add the lake layer with lightblue color
  geom_sf(data = lake_MWI_obj, aes(fill = "Lake Malawi"), colour = NA, alpha = 0.75) +
  # Add the second scale for the lake legend
  scale_fill_manual(name = "", values = c("Lake Malawi" = "lightblue2"), 
                    labels = c("Lake Malawi"), guide = guide_legend(order = 2)) +
  # Set coordinates and theme
  coord_sf() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",  # Position legends at the bottom
        legend.box = "horizontal",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 11),
        strip.text = element_blank(),  # Make facet labels bold
        strip.background = element_blank()) +  # Remove background color for facet labels) +  # Make sure the legends are placed horizontally
  cowplot::panel_border(remove = TRUE) +
  facet_wrap(~risk_factor_type, nrow = 1)  # Facet by risk_factor_type
#all_rf_plot_specific_yr

# Save the plot as a horizontal A4 PDF
ggsave(filename = "all_rf_plot_specific_yr_2016.pdf", 
       plot = all_rf_plot_specific_yr, 
       device = "pdf", 
       width = 11.69,  # A4 width in inches
       height = 8.27, # A4 height in inches
       units = "in"  # Unit of measurement (inches)
)

# ===================================================== #
#                  Plot vgms together                   #
# ===================================================== #

p_2000_poverty1 <- p_2000_poverty
p_2000_sanitation1 <- p_2000_sanitation
p_2004_poverty1 <- p_2004_poverty
p_2004_sanitation1 <- p_2004_sanitation
p_2010_poverty1 <- p_2010_poverty
p_2010_sanitation1 <- p_2010_sanitation
p_2016_poverty1 <- p_2016_poverty
p_2016_sanitation1 <- p_2016_sanitation

# Remove the title in the Trellis plot
p_2000_poverty1$main <- NULL
p_2000_sanitation1$main <- NULL
p_2004_poverty1$main <- NULL
p_2004_sanitation1$main <- NULL
p_2010_poverty1$main <- NULL
p_2010_sanitation1$main <- NULL
p_2016_poverty1$main <- NULL
p_2016_sanitation1$main <- NULL

vgms_plot <- ggpubr::ggarrange(p_2000_poverty1, p_2000_sanitation1,
                               p_2004_poverty1, p_2004_sanitation1,
                               p_2010_poverty1, p_2010_sanitation1,
                               p_2016_poverty1, p_2016_sanitation1,
                             ncol = 2, nrow = 4,
                             labels = c("A", "B", "C", "D", "E", "F", "G", "H"))

vgms_plot

# Save the plot as a horizontal A4 PDF
ggsave(filename = "vgms_plot.pdf", 
       plot = vgms_plot,
       width = 8.27,  # A4 width in inches
       height = 11.69, # A4 height in inches
       units = "in"  # Unit of measurement (inches)
)
