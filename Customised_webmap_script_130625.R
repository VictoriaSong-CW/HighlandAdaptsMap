# R Script for Developing Highland Adapts Interactive Webmap
#Author: Victoria Song
#Developed: 13/06/25
#Tested and works as of 02/07/25

######
#Before you run this script, please ensure the following is done:
  
#Increase your Windows paging file size
#Press Win + R, type sysdm.cpl → Enter.
#Go to Advanced tab → Performance → Settings.
#Click Advanced tab → Virtual memory → Change.
#Uncheck “Automatically manage paging file size”.
#Select your system drive (usually C:).
#Choose “Custom size” and increase the Initial size and Maximum size 
  #(Min: 4096 MB; Max: 8173 MB worked for this exercise).
#Click Set → OK and reboot your PC.

####

library(sf)
library(leaflet)
library(leaflet.extras2)
library(leafpop)
library(RColorBrewer)
library(mapview)
library(terra)
library(htmltools)
library(htmlwidgets)

#load folder of shapefiles
shapefile_folder <- "Final Data"

# List all shapefiles
shapefiles <- list.files(shapefile_folder, pattern = "\\.shp$", full.names = TRUE)

#first read the Palmer Drought Index Layer

#labels
pdsi_classes <- # Define mapping from gridcode to classification
  pdsi_labels <- c(
    "0" = "Extreme Drought",
    "1" = "Severe Drought",
    "2" = "Moderate Drought",
    "4" = "Mild Drought",
    "5" = "Near Normal",
    "7" = "Unusually Moist",
    "8" = "Extremely Moist",
    "9" = "Record Moist"
  )

# Read and transform layer
pdsi_layer <- st_read("Final Data/PalmerDroughtIndex.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid()

# Add classification column using the mapping
pdsi_layer$PDSI_Class <- pdsi_labels[as.character(pdsi_layer$gridcode)]


# Define classification order from dry (red) to wet (blue)
pdsi_levels <- c("Extreme Drought", "Severe Drought", "Moderate Drought",
                 "Mild Drought", "Near Normal", "Unusually Moist",
                 "Extremely Moist", "Record Moist")

pal_pdsi <- colorFactor(
  palette = rev(brewer.pal(length(pdsi_levels), "RdBu")),
  domain = pdsi_levels
)

# Read raster
r <- rast("Final Data/Sentinel2LandCover/Sentinel-2-LandCover.tif")
#r <- rast("Final Data/ceh_landcover/CEH_landcover_Highlands.tif")
#r[r == 0] <- NA


# Define a custom modal function (to handle ties and NAs)
modal_fun <- function(x, ...) {
  ux <- unique(na.omit(x))
  if(length(ux) == 0) return(NA)
  tab <- tabulate(match(x, ux))
  mode_val <- ux[which.max(tab)]
  return(mode_val)
}

#freq_table <- freq(r)
#print(freq_table)

# Aggregate/downsample raster by factor 10 using modal function
r_small <- aggregate(r, fact=10, fun=modal_fun)

# Plot to check downsampling has run for the entire raster
#plot(r_small)

# CEH Land Cover classes
#ceh_classes <- data.frame(
 # VALUE = 1:21,
#  CLASS_NAME = c(
 #   "Broadleaf woodland", "Coniferous woodland", "Arable and Horticulture", "Improved grassland",
  #  "Neutral grassland", "Calcareous grassland", "Acid grassland", "Fen, March and Swamp",
   # "Heather", "Heather grassland", "Bog", "Inland rock",
    #"Saltwater", "Freshwater", "Supralittoral rock", "Supralittoral sediment", "Littoral rock",
    #"Littoral sediment", "Saltmarsh", "Urban", "Suburban"
  #)
#)

# Create color palette
# Get unique non-NA values
landcover_types <- na.omit(unique(as.character(values(r_small))))



# Sentinel-2 land cover mapping (excluding value 3 and 6)
sentinel_classes <- data.frame(
  VALUE = c(0, 1, 2, 4, 5, 7, 8, 9, 10, 11),
  CLASS_NAME = c(
    "No Data",            # 0
    "Water",              # 1
    "Trees",              # 2
    "Flooded Vegetation", # 4
    "Crops",              # 5
    "Built Area",         # 7
    "Bare Ground",        # 8
    "Snow/Ice",           # 9
    "Clouds",             # 10
    "Rangeland"           # 11
  )
)

# Drop No Data from visualization if needed
sentinel_classes_filtered <- sentinel_classes[sentinel_classes$CLASS_NAME != "No Data", ]

#convert class names from numeric to text
landcover_labels <- sentinel_classes_filtered$CLASS_NAME
names(landcover_labels) <- sentinel_classes_filtered$VALUE


# Color palette
pal_sentinel <- colorFactor(
  palette = RColorBrewer::brewer.pal(n = 10, name = "Paired"),
  domain = sentinel_classes_filtered$VALUE
)

#vals <- ceh_classes$VALUE
#pal_colors <- brewer.pal(9, "Set1")
#pal_colors <- colorRampPalette(pal_colors)(length(vals))
#pal <- colorFactor(palette = pal_colors, domain = vals)


#initiate Leaflet map
m <- leaflet() %>% addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron")

#add pdsi layer
m <- m %>%
  addPolygons(
    data = pdsi_layer,
    fillColor = ~pal_pdsi(PDSI_Class),
    fillOpacity = 0.5,
    color = "gray",
    weight = 1,
    group = "Palmer Drought Index",
    popup = popupTable(pdsi_layer, feature.id = FALSE),
    label = ~PDSI_Class
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_pdsi,
    values = pdsi_levels,
    title = "Palmer Drought Index",
    group = "Palmer Drought Index",
    opacity = 1
  )


# Loop through shapefiles
for (shp_path in shapefiles) {
  # Read, transform, clean
  layer <- st_read(shp_path, quiet = TRUE) %>%
    st_transform(4326) %>%
    st_make_valid()
  
  layer <- layer[!st_is_empty(layer), ]
  if (nrow(layer) == 0) next
  
  layer_name <- tools::file_path_sans_ext(basename(shp_path))
  popup_table <- popupTable(layer, feature.id = FALSE)
  
  # Layer logic
  if (layer_name == "SIMD") {
    pal <- colorFactor(brewer.pal(6, "Set2"), domain = layer$Decilev2)
    
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = ~pal(Decilev2),
        fillOpacity = 0.7,
        color = "white",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = layer$Decilev2,
            title = "SIMD Decile",
            group = layer_name,
            opacity = 1
      )
    
  } else if (layer_name == "HighlandCouncilBoundary") {
    m <- m %>%
      addPolygons(
        data = layer,
        fill = TRUE,
        fillColor = "transparent",
        fillOpacity = 0,
        color = "black",
        weight = 3,
        group = "Highland Council Boundary",
        popup = popup_table,
        label = ~as.character(layer_name)
      )
    
  } else if (layer_name == "SSSI") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "darkgreen",
        fillOpacity = 0.7,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
    addLegend("bottomright", colors = "darkgreen", labels = "SSSI",
              title = "SSSI",
              group = layer_name, opacity = 1)
    
    
  } else if (layer_name == "RiverFloodingExtent_H") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = "River Flooding Extent - High",
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
        addLegend("bottomright", colors = "blue", labels = "River Flooding Extent (High Probability)",
                  title = "River Flooding Extent (High Probability)",
                  group = "River Flooding Extent - High", opacity = 1)
    
  } else if (layer_name == "RiverFloodingExtent_M") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "deepskyblue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = "River Flooding Extent - Medium",
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
          addLegend("bottomright", colors = "deepskyblue", labels = "River Flooding Extent (Medium Probability)",
                    title = "River Flooding Extent (Medium Probability)",
                    group = "River Flooding Extent - Medium", opacity = 1)
      
    
  } else if (layer_name == "RiverFloodingExtent_P") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "lightblue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = "River Flooding Extent - Projected",
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
          addLegend("bottomright", colors = "lightblue", labels = "River Flooding Extent (Projected)",
                    title = "River Flooding Extent (Projected)",
                    group = "River Flooding Extent - Projected", opacity = 1)
    
  } else if (layer_name == "CoastalErosion_2050_HE") {
    if (!"ERODETYPE" %in% names(layer)) {
      warning("ERODETYPE not found in CoastalErosion_2050_HE")
    } else {
      vals <- na.omit(unique(layer$ERODETYPE))
      pal <- colorFactor(brewer.pal(min(length(vals), 6), "Set2"), domain = vals)
      
      m <- m %>%
        addPolygons(
          data = layer,
          fillColor = ~pal(ERODETYPE),
          fillOpacity = 0.5,
          color = "gray",
          weight = 1.5,
          group = "Coastal Erosion (Projected 2050)" ,
          popup = popup_table,
          label = ~as.character(ERODETYPE)
        ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = layer$ERODETYPE,
              title = "Coastal Erosion 2050 High Emissions Scenario",
              group = "Coastal Erosion (Projected 2050)"
            )
        
    }
  } else if (layer_name == "CoastalFloodingExtent_H") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = "Coastal Flooding Extent - High",
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
          addLegend("bottomright", colors = "blue", labels = "Coastal Flooding Extent (High Probability)",
                                                    title = "Coastal Flooding Extent (High Probability)", 
                    group = "Coastal Flooding Extent - High", opacity = 1)
      
  } else if (layer_name == "CoastalFloodingExtent_M") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "deepskyblue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = "Coastal Flooding Extent - Medium",
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
          addLegend("bottomright", colors = "deepskyblue", labels = "Coastal Flooding Extent (Medium Probability)",
                    title = "Coastal Flooding Extent (Medium Probability)",
                    group = "Coastal Flooding Extent - Medium", opacity = 1)
      
  } else if (layer_name == "CoastalFloodingExtent_P") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "lightblue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = "Coastal Flooding Extent - Projected",
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
          addLegend("bottomright", colors = "lightblue", labels = "Coastal Flooding Extent (2080 High Emissions)",
                    title = "Coastal Flooding Extent (2080 High Emissions)",
                    group = "Coastal Flooding Extent - Projected", opacity = 1)
    
  } else if (layer_name == "WildfireOccurence") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "red",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = "Wildfire Occurence",
        popup = popup_table,
        label = ~as.character(layer_name)) %>%
      addLegend("bottomright", colors = "red", labels = "Wildfire Occurence",
                title = "Wildfire Occurence", group = "Wildfire Occurence", opacity = 1)
  }
}
  

#add raster to webmap
m <- m %>%
  addRasterImage(r_small, colors = pal_sentinel, opacity = 0.6, group = "Sentinel-2 Landcover",
                 project = F) %>%
  addLegend(
    position = "bottomright",
    pal = pal_sentinel,
    values = as.numeric(names(landcover_labels)),
    title = "Sentinel-2 Land Cover",
    opacity = 1,
    labFormat = labelFormat(
      transform = function(x) landcover_labels[as.character(x)]
  )) %>%
  addLayersControl(
    overlayGroups = c("Sentinel-2 Landcover"),
    options = layersControlOptions(collapsed = FALSE)
  )

#m <- m %>%
 # addRasterImage(r_small, colors = pal, opacity = 0.7, group = "CEH Landcover") %>%
  #addLegend(
   # position = "bottomright",
  #  pal = pal,
   # values = vals,
  #  title = "CEH Land Cover",
  #  labFormat = labelFormat(
   #   transform = function(x) {
    #    ceh_classes_filtered$CLASS_NAME[match(x, ceh_classes_filtered$VALUE)]
     # }
    #)
  #)

#m <- m %>%
 # addRasterImage(r_small, colors = pal, opacity = 0.7, group = "CEH Landcover") %>%
  #addLegend(
   # position = "bottomright",
    #colors = pal,
    #values = ceh_classes_filtered$VALUE,
    #labels = ceh_classes_filtered$CLASS_NAME,
    #opacity = 1,
    #group = "CEH Landcover"
  #)


# Read simplified flooding layers
flood_high <- st_read("Final Data/Surface Water Extent/SurfaceWaterExtent_H.shp", quiet = TRUE) %>% st_transform(4326)
flood_med  <- st_read("Final Data/Surface Water Extent/SurfaceWaterExtent_M.shp", quiet = TRUE) %>% st_transform(4326)
flood_proj  <- st_read("Final Data/Surface Water Extent/SurfaceWaterExtent_P.shp", quiet = TRUE) %>% st_transform(4326)

# Take 0.5% samples to reduce memory load
flood_high_sample <- flood_high %>% dplyr::slice_sample(prop = 0.005)
flood_med_sample  <- flood_med %>% dplyr::slice_sample(prop = 0.005)
flood_proj_sample  <- flood_proj %>% dplyr::slice_sample(prop = 0.005)

# Add each to map
m <- m %>%
  addPolygons(
    data = flood_high_sample,
    fillColor = "blue",
    fillOpacity = 0.5,
    color = "gray",
    weight = 1,
    group = "Surface Flooding High",
    label = ~as.character("High Flood Risk")
  ) %>%
  addLegend("bottomright", colors = "blue", labels = "Surface Flooding High",
            title = "Surface Flooding High", group = "Surface Flooding High", opacity = 1)

m <- m %>%
  addPolygons(
    data = flood_med_sample,
    fillColor = "deepskyblue",
    fillOpacity = 0.5,
    color = "gray",
    weight = 1,
    group = "Surface Flooding Medium",
    label = ~as.character("Medium Flood Risk")
  ) %>%
  addLegend("bottomright", colors = "deepskyblue", labels = "Surface Flooding Medium",
            title = "Surface Flooding Medium", group = "Surface Flooding Medium", opacity = 1)
m <- m %>%
  addPolygons(
    data = flood_proj_sample,
    fillColor = "lightblue",
    fillOpacity = 0.5,
    color = "gray",
    weight = 1,
    group = "Surface Flooding Projected",
    label = ~as.character("Projected (2070) Medium Flood Risk")) %>%
  addLegend("bottomright", colors = "lightblue", labels = "Surface Flooding Projected",
            title = "Surface Flooding Projected", group = "Surface Flooding Projected", opacity = 1)
  

# Add layer controls
# Vector of overlay group names
overlay_names <- c(
  "Palmer Drought Index",
  "SIMD",
  "Highland Council Boundary",
  "SSSI",
  "River Flooding Extent - High",
  "River Flooding Extent - Medium",
  "River Flooding Extent - Projected",
  "Coastal Flooding Extent - High",
  "Coastal Flooding Extent - Medium",
  "Coastal Flooding Extent - Projected",
  "Coastal Erosion (Projected 2050)",
  "Wildfire Occurence",
  "Sentinel-2 Landcover",
  "Surface Flooding High",
  "Surface Flooding Medium",
  "Surface Flooding Projected"
)

# Set all layers except Highland Council Boundary to be hidden on load
groups_to_hide <- setdiff(overlay_names, "Highland Council Boundary")



  # Add the layers control
m <- m %>% addLayersControl(
    baseGroups = c("OSM", "CartoDB.Positron"),
    overlayGroups = overlay_names,
    options = layersControlOptions(collapsed = FALSE)
  )

# Hide all layers except Highland
for (group in groups_to_hide) {
  m <- m %>% hideGroup(group)
}



#m


#note opacity slider settings don't work - have omitted here.


#save map as html file
#exported html will take a while to load - c. 1 minute
saveWidget(m, file="HighlandAdapts_webmap4.html", selfcontained = FALSE)

