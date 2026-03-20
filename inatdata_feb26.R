# Meredith Willmott script for Nathaniel Belcher 
# R script for importing and analying iNaturalist
# data on ant-hemipteran interactions

## Libraries 

library(viridis)
library(maps)
library(ggplot2)
library(vegan)

# import your data 
path <- "C:/Users/mered/OneDrive/Desktop/iNaturalistData/" #fill with your own path
inat <- read.csv(paste(path, "inatdata.csv")) 
# The point of having the path variable is to not have to fill it in multiple times
# But actually what you really want is for us all to read from the same cloud 
# version: 
inat <- read.csv("https://raw.githubusercontent.com/leafhopping/iNat-ant-tending/refs/heads/main/inatdata.csv")

# using viridis to assign a different color to each family 

inat$family <- factor(inat$taxon_family_name)
fam_levels <- levels(inat$family)

fam_cols <- setNames(viridis(length(fam_levels)), fam_levels)
inat$col <- fam_cols[as.character(inat$family)]

# filter the data to only desert states

inat$state <- map.where("state", inat$longitude, inat$latitude)

target_states <- c("arizona","new mexico","california","nevada","utah")

inat_sw <- inat[inat$state %in% target_states, , drop = FALSE]
inat_sw$state <- factor(inat_sw$state, levels = target_states)
inat_sw$family <- factor(inat_sw$taxon_family_name)

cat("Records in target states:", nrow(inat_sw), "\n")
cat("Families in target states:", length(levels(inat_sw$family)), "\n")


#plotting the distribution of different families on a map

states_map <- map_data("state")
states_sw <- states_map[states_map$region %in% target_states, ] #gets map polygons

ggplot() +
  geom_polygon(data = states_sw,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               color = "gray70") +
  geom_point(data = inat_sw,
             aes(x = longitude,
                 y = latitude,
                 color = taxon_family_name),
             size = 1.5,
             alpha = 0.7) +
  scale_color_viridis_d(name = "Family", option = "D") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "iNaturalist Observations",
       subtitle = "AZ, NM, CA, NV, UT",
       x = "Longitude",
       y = "Latitude")

title("iNaturalist observations by family")

# plotting distribution of families only in Arizona

## Assign state based on coordinates
inat$state <- maps::map.where("state",
                              inat$longitude,
                              inat$latitude)

## Keep only Arizona
inat_az <- inat[inat$state == "arizona", ]

cat("Arizona records:", nrow(inat_az), "\n")
cat("Arizona families:", length(unique(inat_az$taxon_family_name)), "\n")

az_map <- ggplot2::map_data("state")
az_map <- az_map[az_map$region == "arizona", ]

ggplot() +
  geom_polygon(
    data = az_map,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "grey60",
    linewidth = 0.4
  ) +
  geom_point(
    data = inat_az,
    aes(x = longitude, y = latitude, color = taxon_family_name),
    size = 1.5,
    alpha = 0.7
  ) +
  scale_color_viridis_d(name = "Family", option = "D") +
  coord_fixed(1.3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "iNaturalist Observations in Arizona",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )


# calculating beta diversity: is CA unique because it has unique families
# or does it just have more of everything? 

# state x family abundance matrix
tab_counts <- with(inat_sw, table(state, taxon_family_name))

# Bray-Curtis distance
dist_bc <- vegdist(tab_counts, method = "bray")

pa <- tab_counts > 0
dist_jac <- vegdist(pa, method = "jaccard")

richness <- rowSums(tab_counts > 0)
barplot(richness)


