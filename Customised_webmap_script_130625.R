# R Script for Developing Highland Adapts Interactive Webmap
#Author: Victoria Song
#Developed: 13/06/25
#Tested and works as of 18/06/25

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

# Read raster
r <- rast("Final Data/ceh_landcover/CEH_landcover_Highlands.tif")
r[r == 0] <- NA


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
ceh_classes <- data.frame(
  VALUE = 1:21,
  CLASS_NAME = c(
    "Broadleaf woodland", "Coniferous woodland", "Arable and Horticulture", "Improved grassland",
    "Neutral grassland", "Calcareous grassland", "Acid grassland", "Fen, March and Swamp",
    "Heather", "Heather grassland", "Bog", "Inland rock",
    "Saltwater", "Freshwater", "Supralittoral rock", "Supralittoral sediment", "Littoral rock",
    "Littoral sediment", "Saltmarsh", "Urban", "Suburban"
  )
)

# Create color palette
#vals <- ceh_classes$VALUE
#pal_colors <- brewer.pal(9, "Set1")
#pal_colors <- colorRampPalette(pal_colors)(length(vals))
#pal <- colorFactor(palette = pal_colors, domain = vals)


#initiate Leaflet map
m <- leaflet() %>% addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron")

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
        label = ~as.character(layer_name)
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
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
    
  } else if (layer_name == "SSSI") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "green",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
    
  } else if (layer_name == "RiverFloodingExtent_H") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
    
  } else if (layer_name == "RiverFloodingExtent_M") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
    
  } else if (layer_name == "RiverFloodingExtent_P") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
    
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
          group = layer_name,
          popup = popup_table,
          label = ~as.character(ERODETYPE)
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
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
  } else if (layer_name == "CoastalFloodingExtent_M") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
  } else if (layer_name == "CoastalFloodingExtent_P") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
  } else if (layer_name == "CoastalFloodingExtent_H") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
  } else if (layer_name == "WildfireOccurence") {
    m <- m %>%
      addPolygons(
        data = layer,
        fillColor = "red",
        fillOpacity = 0.5,
        color = "gray",
        weight = 1.5,
        group = layer_name,
        popup = popup_table,
        label = ~as.character(layer_name)
      )
  }
  
}

#add raster to webmap
m <- m %>%
  addRasterImage(r_small, colors = pal, opacity = 0.7, group = "CEH Landcover") %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = vals,
    title = "CEH Land Cover",
    labFormat = labelFormat(
      transform = function(x) {
        ceh_classes_filtered$CLASS_NAME[match(x, ceh_classes_filtered$VALUE)]
      }
    )
  )

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
    color = "black",
    weight = 1,
    group = "Surface Flooding High",
    label = ~as.character("High Flood Risk")
  ) %>%
  addPolygons(
    data = flood_med_sample,
    fillColor = "deepskyblue",
    fillOpacity = 0.5,
    color = "black",
    weight = 1,
    group = "Surface Flooding Medium",
    label = ~as.character("Medium Flood Risk")
  ) %>%
  addPolygons(
    data = flood_proj_sample,
    fillColor = "lightblue",
    fillOpacity = 0.5,
    color = "black",
    weight = 1,
    group = "Surface Flooding Projected",
    label = ~as.character("Projected (2070) Medium Flood Risk")
  )

# Add layer controls
m <- m %>%
  addLayersControl(
    baseGroups = c("OSM", "CartoDB.Positron"),
    overlayGroups = c(tools::file_path_sans_ext(basename(shapefiles)), "CEH Landcover",
                      "Surface Flooding High", "Surface Flooding Medium", 
                      "Surface Flooding Projected"),
    options = layersControlOptions(collapsed = FALSE)
  )
#note opacity slider settings don't work - have omitted here.

#m

#save map as html file
#exported html will take a while to load - c. 1 minute
saveWidget(m, file="HighlandAdapts_webmap2.html", selfcontained = FALSE)

