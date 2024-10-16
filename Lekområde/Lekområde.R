library(sf)
library(terra)
library(rio)

# Laddar filer
depth <- terra::rast("Lekområde/data/symphony_djup_v2.tif")
depth <- terra::project(depth, "EPSG:4258", method = "near")
areas <- terra::vect("Lekområde/data/Lektidsdatabasens Geografier.shp", crs = terra::crs(depth))
speciesList <- rio::import("Lekområde/data/ArtlistaLekområden.xlsx")

# Slår ihop areapolygonerna med information om vilka arter som leker inom varje polygon
areaSpeciesList <- terra::merge(areas, speciesList, by = "OBJECT_ID")
speciesListNames <- names(speciesList)[-c(1:3)]

# Loopar igenom och skapar ett raster för varje art
allSpecies <- rast(ext(depth), resolution = res(depth), crs = crs(depth))
for(sp in speciesListNames){
  # Gör ett raster av areapolygonerna
  r <- terra::rasterize(areaSpeciesList, depth, field = sp)
  # Skapar matris baserat på artens lekdjup
  m <- matrix(c(-2000, -speciesList[sp][39,], 0,
                -speciesList[sp][39,], -speciesList[sp][38,], 1, 
                -speciesList[sp][38,], 2, 0), 
              ncol = 3, 
              byrow = TRUE)
  # Skapar raster där artens lekdjup får värdet 1 allt annat 0
  depth_filt <- terra::classify(depth, m)
  # Multiplicerar djupfiltret med arearastret
  spawnArea <- as.int(r * depth_filt)
  # Sparar rastret
  writeRaster(spawnArea, paste0("Lekområde/output/artraster/", sp, ".tif"))
  # Lägger till rastret i ett multiraster
  allSpecies <- c(allSpecies, spawnArea)
}

# Summerar alla enskilda raster
allSpeciesSum <- sum(allSpecies)
writeRaster(allSpeciesSum, paste0("Lekområde/output/Lekområde.tif"))
