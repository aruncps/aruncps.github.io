library(ggplot2)
library(maps)
library(tidyverse)
library(RColorBrewer)
library(ggmap)
library(scales)
library(maptools)
library(gridExtra)
library(rgdal)
library(rgeos)
library(mapproj)

worldmap = map_data('world')
knitr::kable(head(worldmap, 20))

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group))


ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group),
               fill = 'gray90', 
               color = 'black') + 
  coord_fixed(xlim = c(77.9,77.99),
              ylim = c(9.5,9.65)) +
  theme_void() +
  geom_point(data = Viru_atms, 
             aes(x = as.numeric(long), 
                 y = as.numeric(lat), size = atm, color = log(atm)), alpha = .7) +
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme(legend.position = 'none') + 
  theme(title = element_text(size = 12))


Viru_atms = read_csv('C:\\Users\\ShanmugA\\Desktop\\Viru.csv')
Viru_atms<-Viru_atms %>% filter(atm>0)

x - c(77.9,77.99)
y - c(9.5,9.65)


states_shape = readShapeSpatial("C:\\Users\\ShanmugA\\Desktop\\Learn_Maps\\IND_adm\\IND_adm1.shp")
class(states_shape)
names(states_shape)
print(states_shape$ID_1)
print(states_shape$NAME_1)
plot(states_shape, main = "Administrative Map of India")
plot(district_shape, main = "Administrative Map of India")
fortify_shape = fortify(states_shape, region = "ID_1")
fortify_shape_TN <- fortify_shape %>% filter(id==31)

class(Ind_boundary_HARV)
Dt_map<-tibble(Ind_boundary_HARV)

TN_map<-Ind_boundary_HARV %>% filter(st_name=='TAMIL NADU')
Viru_map<-Ind_boundary_HARV %>% filter(st_name=='TAMIL NADU' & dist_name=='VIRUDHUNAGAR' & ac_name=='Virudhunagar')
View(Viru_map)

district_shape = readShapeSpatial("C:\\Users\\ShanmugA\\Desktop\\Learn_Maps\\IND_adm\\IND_adm3.shp")
names(district_shape)
print(district_shape$ID_2)
View(cbind(unlist(district_shape$NAME_2),unlist(district_shape$NAME_3)))



fortify_shape_dist = fortify(district_shape, region = "ID_3")
fortify_shape_Viru <- fortify_shape_dist %>% filter(id %in% c(1918,1923))

ggplot() +
  geom_polygon(data = fortify_shape_Viru,
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = "black", size = 0.5) +
  coord_map() +
  theme_void() +
  geom_point(data = Viru_atms, 
             aes(x = as.numeric(long), 
                 y = as.numeric(lat), size = atm, color = log(atm)), alpha = .7)

ggplot(data = Viru_map) +
  geom_sf() +
  geom_sf(data = Viru_map, fill = NA) + 
  geom_label(data = Viru_map, aes(X, Y, label = ac_name), size = 2, fontface = "bold") +
  geom_point(data = Viru_atms, 
             aes(x = as.numeric(long), 
                 y = as.numeric(lat), size = atm, color = atm), alpha = .7)
  
