library (tidyverse)
library(ggmap)
library(maps)
library(gganimate)
library(gifski)
library(viridis)

#For this assignment I used Ontario's fish stocking data of the past 10 years, provided by the Ministry of Natural Resources and Forestry
fish_data <- read_csv("https://raw.githubusercontent.com/SZarini/Stat744/main/Fish_Stocking_Data_Ontario.csv")
fish_data$Count <- fish_data$Number_of_Fish_Stocked

#getting the base map
mapzone <- c(left = -98, bottom = 41, right = -73, top = 57)
basemap <- get_stamenmap(mapzone, zoom = 5, maptype = "toner-lite")
ggmap(basemap)

#Plotting the amount of stocked fish in different areas of Ontario for the past 10 years
map1 <- (ggmap(basemap) +
  geom_point(data = fish_data, aes(x = X, y = Y, color=Count,
                                   size=Count, group=Stocking_Year),
             size=3,alpha=0.3))+
  scale_color_gradientn(colours = rainbow(5), trans=scales::log10_trans())+
  transition_time(Stocking_Year)+
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_years <- max(fish_data$Stocking_Year) - min(fish_data$Stocking_Year) + 1
animate(map1, nframes = num_years, height = 800, width =800, fps=0.5)

## JD: Looks like you have warnings here. You could try dropping zeroes.
## You could also pick a better color scale. I like the use of log, though.

#A heatmap which shows most of stocking has been happening near Ottawa and Toronto
heatdata <- select(fish_data, c('X','Y','Number_of_Fish_Stocked' ))

(map2 <- ggmap(basemap)  #I am not sure what the numbers in the legend are representing!
  + stat_density_2d(data=heatdata,
                    aes(x = X, y = Y, fill=..level..), geom = "polygon",
                    alpha = .35, colour = NA) + scale_fill_gradient2("Fish stocking\nheatmap", low = "white", mid = "yellow",
                         high = "red")
)

## JD: Was I supposed to look at map2? It's not printed or animated afaict.
## The numbers are density estimates bases on the units you gave it for x and y. You can read more about stat_density_2d(). Smoothed incidents per squared degree, or something crazy like that. Thanks for point it out instead of sneaking it past.

Trouts <- fish_data %>% filter(fish_data$Species == 'Rainbow Trout'|
                                 fish_data$Species == 'Brook Trout'|
                                 fish_data$Species == 'Brown Trout'|
                                 fish_data$Species == 'Lake Trout')
map3 <- (ggmap(basemap) +
           geom_point(data = Trouts, aes(x = X, y = Y, color=Species,
                                            size=Count, group=Stocking_Year),alpha=0.3))+
  scale_colour_brewer(palette="Set1")+
  scale_size(range = c(.1, 16), name="Count")+
  guides(color = guide_legend(override.aes = list(size=10)))+
  transition_time(Stocking_Year)+
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_years <- max(Trouts$Stocking_Year) - min(Trouts$Stocking_Year) + 1
animate(map3, nframes = num_years, height = 800, width =800, fps=0.5)

## JD: It's a cool animation, but hard for me to use the rstudio viewer. You should figure out how to save to a file maybe (we have code). Remember to scale sizes by area, especially for concrete things like counts. I'm wondering also whether 

## Grade 2.4/3
