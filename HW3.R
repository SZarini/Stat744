
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

#For this assignment I decided to make some graphs from a table (Table 1) in a paper published by Mehdi et al (https://www.sciencedirect.com/science/article/pii/S0165783621001442?casa_token=dosYL2L5RFIAAAAA:bjKmRKD3RCt37QRj6tZbVDFm-CeB8OIghtnaJGDSxQlhcvJaCHw69gTfK7TtDKS2mmuloSvYTz0#tbl0005),
#Which is about the effect of gear type in assessing fish population.
#

#I couldn't use tabulizer because of this error: Error: package or namespace load failed for ‘tabulizer’:
#.onLoad failed in loadNamespace() for 'tabulizerjars', details:
#  call: NULL
#error: .onLoad failed in loadNamespace() for 'rJava', details:
# call: fun(libname, pkgname)
#error: JAVA_HOME cannot be determined from the Registry


#I made an excel file based on the data in the table: 

hw3data <- read.csv ("https://raw.githubusercontent.com/SZarini/Stat744/main/table-hw3.csv")

#For the first graph I decided to show the total abundance of each species in the sampling location.
#I think it wasn't easy to quickly order the most abundant and rare species by just looking at the table which could be a very important piece of data to look at.

p1 <- ggplot(data = hw3data, aes(x = reorder(ï..Species, -Catch), y = Catch)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(name="Count", breaks = seq(0, 700, by = 50))+
  theme_bw()+
  scale_colour_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+ coord_flip()+
  labs(title = "Fish abundance", x = "Species", y = "Count")
print(p1)

#And then splitting the graph by seasons to see the differences between seasons:

p2 <- p1+facet_grid(. ~Season)
print(p2)

#In the table the fish were divided into three categories based on their habitat (Benthic, Benthopelagic, and Pelagic). I decided to show fish from which habitat contributed most to the catch of specific gears.
#It could be another important piece of information which could help scientists to choose the appropriate gear type for sampling their target species based on their habitat.

p3 <- ggplot(data = hw3data, aes(x = Gear.Type, y = Catch, fill = Habitat)) +
               geom_bar(position = "fill",stat = "identity")+
  scale_y_continuous(labels = scales::percent_format())+
  theme_bw()+
  scale_colour_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  labs(title = "Gear and Habitat", x = "Gear type", y = "Catch rate")
 print(p3)
        

  

