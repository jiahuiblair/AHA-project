library(tidyverse)
library(RColorBrewer)
library(maps)
library(ggmap) ### has to cite it when use !!!!!
library(readxl)
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

impute1 <- read.csv("C:/Users/Jiahui/Documents/Loop/AHA/ImputedData/complete_data_NoEXP_1.csv")
impute2 <- read.csv("C:/Users/Jiahui/Documents/Loop/AHA/ImputedData/complete_data_NoEXP_2.csv")
impute3 <- read.csv("C:/Users/Jiahui/Documents/Loop/AHA/ImputedData/complete_data_NoEXP_3.csv")
impute4 <- read.csv("C:/Users/Jiahui/Documents/Loop/AHA/ImputedData/complete_data_NoEXP_4.csv")
impute5 <- read.csv("C:/Users/Jiahui/Documents/Loop/AHA/ImputedData/complete_data_NoEXP_5.csv")


# Load the CBSA shapefile
cbsa_shapefile <- st_read("C:/Users/Jiahui/Documents/Loop/AHA/cb_2023_us_cbsa_500k/cb_2023_us_cbsa_500k.shp")

# Ensure CBSA codes match in format (e.g., numeric or character)
impute1$CBSA_CODE <- as.character(impute1$CBSACODE)
cbsa_shapefile$CBSAFP <- as.character(cbsa_shapefile$CBSAFP) 

# Merge your data with the shapefile
cbsa_merged <- impute1 %>%
  mutate(CBSACODE = as.character(CBSACODE)) %>%
  left_join(cbsa_shapefile, by = c("CBSACODE"="CBSAFP")) %>%
  filter(!is.na(ID))

EXPTOT_summary <- impute1 %>%
  group_by(CBSACODE) %>%
  summarize(EXPTOT = mean(EXPTOT, na.rm = TRUE))

cbsa_EXPTOT <- cbsa_shapefile %>%
  left_join(EXPTOT_summary %>% mutate(CBSACODE = as.character(CBSACODE)),
            by = c("CBSAFP" = "CBSACODE"))

states <- states(cb = TRUE) # Load state border data
cbsa_albers <- st_transform(cbsa_EXPTOT, crs = 5070) # CRS 5070 (NAD83 / Conus Albers) is ideal for U.S. maps with better spacing and aspect ratio
states_albers <- st_transform(states, crs = 5070)


######################################## Multiple features ###########################################
# Summarize multiple features
features_summary <- impute1 %>%
  mutate(TRAUML90_flag = ifelse(TRAUML90 %in% c(3, 4), 1, 0),
         CHC = as.numeric(CHC),
         ONCOLHOS = as.numeric(ONCOLHOS),
         HLTRHOS = as.numeric(HLTRHOS)) %>%
  group_by(CBSACODE) %>%
  summarize(
    EXPTOT = mean(EXPTOT, na.rm = TRUE),
    ADC = mean(ADC, na.rm = TRUE),
    GFEET  = mean(GFEET, na.rm = TRUE),
    VEM  = mean(VEM, na.rm = TRUE),
    PLNTA  = mean(PLNTA, na.rm = TRUE),
    VIDVZ  = mean(VIDVZ, na.rm = TRUE),
    FTMDTF  = mean(FTMDTF, na.rm = TRUE),
    FTRNTF = mean(FTRNTF, na.rm = TRUE),
     = mean(CHC, na.rm = TRUE),
    TRAUML90_flag_mean = mean(TRAUML90_flag, na.rm = TRUE),
    ONCOLHOS_mean = mean(ONCOLHOS, na.rm = TRUE),
    CHC = sum(CHC, na.rm = TRUE),
    TRAUML90_flag = sum(TRAUML90_flag, na.rm = TRUE),
    ONCOLHOS = sum(ONCOLHOS, na.rm = TRUE),
    HLTRHOS_mean = mean(HLTRHOS, na.rm = TRUE)
  )

# Pivot longer for faceting
features_long <- features_summary %>%
  pivot_longer(cols = c(EXPTOT,ADC,GFEET,FTRNTF,PLNTA,), names_to = "Feature", values_to = "Value")

cbsa_features <- features_long %>%
  mutate(CBSACODE = as.character(CBSACODE)) %>%
  left_join(cbsa_shapefile, by = c("CBSACODE"="CBSAFP"))

cbsa_features_albers <- st_transform(cbsa_features, crs = 5070)

# Merge features
cbsa_overlay <- cbsa_shapefile %>%
  left_join(features_summary %>% mutate(CBSACODE = as.character(CBSACODE)),
            by = c("CBSAFP" = "CBSACODE")) %>%
  st_transform(crs = 5070)

cbsa_centroids <- st_centroid(cbsa_overlay) %>%
  st_as_sf()


####################### New plot to add categorical to bivariate plot ####
library(sf)
library(ggplot2)
library(cowplot)
library(dplyr)

# Step 0: (Optional) remove 00000 from map layers but keep in summary
cbsa_overlay_map <- cbsa_overlay %>% filter(CBSAFP != "00000")
cbsa_centroids_map <- cbsa_centroids %>% filter(CBSAFP != "00000")

# Step 1: Create percentile-based categories
change_feature <- "FTRNTF"
cbsa_overlay <- cbsa_overlay %>%
  mutate(
    exptot_cat = cut(EXPTOT,
                     breaks = quantile(EXPTOT, probs = seq(0, 1, 0.25), na.rm = TRUE),
                     include.lowest = TRUE,
                     labels = 1:4),
    var_cat = cut(!!sym(change_feature),
                  breaks = quantile(!!sym(change_feature), probs = seq(0, 1, 0.25), na.rm = TRUE),
                  include.lowest = TRUE,
                  labels = 1:4),
    combo = ifelse(is.na(exptot_cat) | is.na(var_cat),
                   NA,
                   paste(exptot_cat, var_cat, sep = "-"))
  )

# Step 2: Define 4x4 palette
biv_colors <- c(
  "1-1"="#f0f0f0","2-1"="#d9d9d9","3-1"="#bdbdbd","4-1"="#969696",
  "1-2"="#c6dbef","2-2"="#9ecae1","3-2"="#6baed6","4-2"="#3182bd",
  "1-3"="#bae4b3","2-3"="#74c476","3-3"="#31a354","4-3"="#006d2c",
  "1-4"="#fdd0a2","2-4"="#fdae6b","3-4"="#fd8d3c","4-4"="#e6550d"
)

cbsa_overlay <- cbsa_overlay %>%
  mutate(fill_color = ifelse(is.na(combo), "#d9d9d9", biv_colors[combo]))

# Step 3: Compute percentile cutoffs for legend
exptot_quants <- quantile(cbsa_overlay$EXPTOT, probs = seq(0, 1, 0.25), na.rm = TRUE)
feature_quants <- quantile(pull(cbsa_overlay, !!sym(change_feature)),
                           probs = seq(0, 1, 0.25), na.rm = TRUE)

# Step 4: Make two-line labels
# Helper function to format numbers in "k"
format_k <- function(x) {
  paste0(round(x / 1000), "k")
}

# Create EXPTOT labels with k notation and two lines
exptot_labels <- c(
  paste0("Below 25%\n< ", format_k(exptot_quants[2])),
  paste0("26-50%\n", format_k(exptot_quants[2]+1), "-", format_k(exptot_quants[3])),
  paste0("51-75%\n", format_k(exptot_quants[3]+1), "-", format_k(exptot_quants[4])),
  paste0("Above 75%\n> ", format_k(exptot_quants[4]))
)

feature_labels <- c(
  paste0("Below 25%\n< ", format(round(feature_quants[2]), big.mark=",")),
  paste0("26-50%\n", format(round(feature_quants[2]+1), big.mark=","), "-", format(round(feature_quants[3]), big.mark=",")),
  paste0("51-75%\n", format(round(feature_quants[3]+1), big.mark=","), "-", format(round(feature_quants[4]), big.mark=",")),
  paste0("Above 75%\n> ", format(round(feature_quants[4]), big.mark=","))
)

# Step 5: Main map with CHC labels only
map_plot <- ggplot() + 
  geom_sf(data = cbsa_overlay, aes(fill = fill_color), color = "grey80", size = 0.1) + 
  geom_sf(data = states_albers, fill = NA, color = "black", size = 0.3) + 
  # text labels only (CHC counts, rounded to 1 digit)
  geom_text(
    data = cbsa_centroids, 
    aes(
      x = st_coordinates(geometry)[,1], 
      y = st_coordinates(geometry)[,2], 
      label = ifelse(round(ONCOLHOS_mean, 1) > 0, round(ONCOLHOS_mean, 1), NA)  # Add the third feature label!!! round to 1 digit
    ), 
    size = 2, 
    fontface = "bold", 
    color = "black", 
    inherit.aes = FALSE
  ) + 
  scale_fill_identity() + 
  theme_minimal() + 
  theme(panel.grid = element_blank())  # removes graticule lines

# Step 6: Zoomed maps
zoom_main <- map_plot +
  coord_sf(xlim = c(-2400000, 2500000), ylim = c(-300000, 3500000))

zoom_atlantic <- map_plot +
  coord_sf(xlim = c(-5500000, -1400000), ylim = c(3500000, 7000000))

zoom_hawaii <- map_plot +
  coord_sf(xlim = c(-6500000, -6000000), ylim = c(1000000, 2500000))

# Step 7: Legend tile
legend_data <- expand.grid(
  exptot_cat = 1:4,
  var_cat = 1:4
) %>%
  mutate(combo = paste(exptot_cat, var_cat, sep = "-"),
         fill_color = biv_colors[combo])

legend_plot <- ggplot(legend_data, aes(x = exptot_cat, y = var_cat, fill = fill_color)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0), breaks = 1:4, labels = exptot_labels) +
  scale_y_continuous(expand = c(0, 0), breaks = 1:4, labels = feature_labels) +
  labs(x = "EXPTOT percentile", y = paste(change_feature, "percentile")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 5))

# Step 8: Combine map + legend in top right
final_plot <- ggdraw() +
  draw_plot(map_plot, 0, 0, 1, 1) +
  draw_plot(legend_plot, 0.78, 0.72, 0.2, 0.2)

final_plot
zoom_main
zoom_atlantic
zoom_hawaii





