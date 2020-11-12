#libraries
library(leaflet)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(leaflet.extras)
library(maps)
library(mapdata)
library(mapproj)
library(htmltools)

#world map
w <- map_data('world')
icjr <- map_data('world',
                region = c("India", "China", "Japan", "Russia"))
ggplot(icjr, aes(x = long, y = lat, group = group, fill = region)) + 
  geom_polygon(color = 'black') + 
  coord_map('polyconic')

#USA
s <- map_data('state')
ggplot(s, aes(x = long, y = lat, group = group, fill = region)) + 
  geom_polygon(color = 'black') + 
  coord_map('polyconic') +
  guides(fill = F)

#COVID data - USA
c <- read.csv(file.choose(), header = T)
usa <- c %>% filter(country == 'United States')
usa <- usa %>% group_by(province) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Merging map data with covid data
usa$province <- tolower(usa$province)
data <- merge(s, usa,
              by.x = 'region',
              by.y = 'province')

#Covid map
ggplot(data, aes(x = long, y = lat, 
                 group = group,
                 fill = count)) + 
  geom_polygon(color = 'gray') + 
  coord_map('polyconic') +
scale_fill_gradient2(low = 'white', high = 'green') + 
  theme_void() + 
  ggtitle('COVID Cases in US')

# Leaflet
leaflet() %>% addTiles() #interactive map viewer
names(providers)

#Grand Rapids, MI
leaflet() %>% addProviderTiles('CartoDB') %>%
  setView(lng = -85.670006, lat = 42.963795,
          zoom = 11.5) %>% 
  addCircleMarkers(lng = -85.670006, lat = 42.963795,
                   radius = 40,
                   color = 'red')

#Wuhan, China
leaflet() %>% addProviderTiles('CartoDB') %>%
  setView(lng = 114.3055, lat = 30.5928,
          zoom = 11) %>% 
  addCircleMarkers(lng = 114.3055, lat = 30.5928,
                   radius = 40,
                   color = 'red')

#Covid data - map of US
usa <- c %>% filter(country == 'United States')
usa <- usa %>% group_by(city, province, longitude, latitude) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))


#map for each US county
mycolor <- colorNumeric(palette = 'RdBu',
                        domain = c(1:1000),
                        reverse = T)
usa %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(radius = ~0.01*count,
                   color = ~mycolor(count),
                   popup = ~paste0(city,
                                   "<br/>",
                                   count)) %>%
  addLegend(pal = mycolor,
            values = c(1:1000),
            opacity = 0.75,
            title = 'Covid Count',
            position = 'topleft') %>%
  setView(lng = -85.670006, lat = 42.963795,
          zoom = 4.499)


#Map for NY & MI
usa <- c %>% filter(country == 'United States',
                    province == 'Michigan' | province == 'New York')
usa <- usa %>% group_by(city, province, longitude, latitude) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

map <-usa %>% leaflet() %>%
  addProviderTiles('OpenStreetMap') %>% 
  addCircleMarkers(radius = 2,
                   color = 'red',
                   popup = ~city)

map %>% addMarkers()#Run to add markers on map above
map %>% clearMarkers()#gets rid of them 


#Color Factor
mycolor <- colorFactor(palette = c('red', 'blue'),
                       levels = c('Michigan', 'New York'))
map %>%
  addCircleMarkers(data = usa,
                   radius = 2,
                   color = ~mycolor(province),
                   label = ~paste0(city, " (", province, ")"))


#Search and Reverse search
leaflet() %>% 
  addTiles() %>%
  addSearchOSM() %>%
  addReverseSearchOSM()

#Toggle between states
MI <- filter(usa, province == 'Michigan')
m <- leaflet() %>%
  addProviderTiles('CartoDB') %>%
  addCircleMarkers(data = MI,
                   radius = 5,
                   label = ~htmlEscape(city,),
                   color = 'blue',
                   group = 'Michigan')
m

NY <- filter(usa, province == 'New York')
m <- m %>%
  addCircleMarkers(data = NY,
                   radius = 5,
                   label = ~htmlEscape(city,),
                   color = 'red',
                   group = 'New York') %>%
  addLayersControl(overlayGroups = 
                     c('Michigan', 'New York'))
m #run this line after rest


#Toggle base map & states
m1 <- leaflet() %>%
  addTiles(group = 'OpenRailwayMap') %>%
  addProviderTiles("HERE", group = "HERE") %>%
  addCircleMarkers(data = MI,
                   radius = 5,
                   label = ~htmlEscape(city,),
                   color = 'blue',
                   group = 'Michigan') %>%
  addCircleMarkers(data = NY,
                   radius = 5,
                   label = ~htmlEscape(city,),
                   color = 'red',
                   group = 'New York') %>%
  addLayersControl(baseGroups = c("OpenRailwayMap", "HERE"),
                   overlayGroups = 
                     c('Michigan', 'New York'))
m1

#cluster, zoom in and out of map for effect
usa %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 2,
                   label =~htmlEscape(city),
                   color = 'red',
                   clusterOptions = markerClusterOptions())


#Cluster whole country - not working
mycolor <- colorNumeric(palette = 'RdBu',
                        domain = c(1:1000),
                        reverse = T) %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(radius = ~0.01*count,
                   color = ~mycolor(count),
                   popup = ~paste0(city,
                                   "<br/>",
                                   count)) %>%
  addLegend(pal = mycolor,
            values = c(1:1000),
            opacity = 0.75,
            title = 'Covid Count',
            position = 'topleft') %>%
  setView(lng = -85.670006, lat = 42.963795,
          zoom = 4.499)
