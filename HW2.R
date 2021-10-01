#Hello, Sorry for the delay and that my codes are incomplete. I have very little  experience with coding and R in general and have been trying hard during the past few days to make the codes work.
#I will try to update the codes until Sunday.

#Jia You's graph is showing how the introduction and administration of vaccines successfully decreased case number of a lot of diseases throughout the history.

library(tidyverse)
library(cowplot)
library(paletteer)
library(dplyr)
library(colorspace)
library(ggstream)
library(plotly)
library(streamgraph)


#For the first graph I decided to create multiple linegraphs using facet_wrap code to show the trends of each disease:



mydata <- read.csv("https://raw.githubusercontent.com/SZarini/Stat744/main/vaccinedata.csv")


## JD: Script is not running, and for a simple reason (you don't define point.data2 in the script). Please update and get back to us.

## JD: code should run when I download the repo; that means either:
#### code downloads file, or
#### file is in repo

mydata <- read.csv("vaccinedata.csv")


#creating a new data set to mark the vaccine licencing incidents:

## JD: This much typing inside code makes me nervous; try to think of another way.
point.data <- mydata[c(9, 19, 44, 68, 74, 96, 115, 215, 304, 337, 344, 420),]

#Tried to erase the FALSE values from the text box (on the interactive graph). But is is showing as NA anyway.
mydata2 <- mydata %>%
  mutate(vaccine = na_if(vaccine, "FALSE"))


p1 <- ggplot(data = mydata2, aes(x = year, y = cases,
                                text = vaccine, group = disease)) +
  geom_line() +
  facet_wrap(~ disease, scales = "free") + #I decided to choose free x and y axis because the magnitude and timeline of each disease was different and using equal scales could result in being misled.
  scale_x_continuous(breaks = c(1940, 1960, 1980, 2000, 2020),
                     labels= c('1940', '1960','1980','2000', '2020')) +
  theme_bw() +
  scale_y_continuous(name="cases", labels = scales::comma) + #I couldn't find a code to adjust the x and y axis to have equal number of breaks and gridlines for all graphs. I tried the "scales::pretty_breaks()" command but it didn't work.
  geom_point(data=point.data,aes(x = year, y = cases))+
  labs(title = "THE VACCINE WARS",
       x = "Year", y = "Cases")



#For the final step I decided to make the graph interactive by using ggplotly:


ggplotly(p1)

#This way the reader can check the exact case number for each year by moving the cursor.
#Each event will also be displayed by putting the cursor on the dots.


#For the second graph I decided to create a streamgraph to show the overall efficacy of vaccines in controlling diseases:




## JD: don't put install into your code; your code should be something that can be run blindly over and over. Also, people might install in different ways.
## install.packages("ggstream")
## install.packages("paletteer")

## JD: It's usually good to have library statements up front, so script will crash near the beginning and people can download what they need.


#This code takes a little longer to run
gstream1 <- mydata %>%
  ggplot(aes(year, sqrt(cases), fill = disease, label = disease, color = disease)) + #I tried to plot the y axis on a log scale using "scale_y_log10()" but it resulted in some weird numbers
  geom_stream(extra_span = 0.013, true_range = "none", type = "mirror", n_grid = 3000, bw = .78) +
  geom_stream_label(size = 5, type = "mirror", n_grid = 1000) +
  cowplot::theme_minimal_vgrid(font_size = 18) +
  theme(legend.position = "none") +
  scale_colour_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::darken(.8)) +
  scale_fill_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::lighten(.2)) +
  scale_x_continuous(breaks=seq(1940,2020,by=10))+
  labs(title = "THE VACCINE WARS",
       x = "Year",
       y = "Cases") + #I couldn't figure out how to adjust the position of the disease type direct labeling. A few are not well positioned. The Y axis is a little confusing here. I wasn't sure what would the best solution would be. Maybe totally removing it?
  annotate("segment", x = 1955, xend = 1955, y = -500, yend = -850, #I couldn't find a code to automatically add labels for each vaccine event so I decided to add them manually but it is taking a very long time
           colour = "black", size = 1)+
  geom_label(label="First polio vaccine licenced",x=1955,y=-870,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey") +
  annotate("segment", x = 1961, xend = 1961, y = -440, yend = -750,
           colour = "black", size = 1)+
  geom_label(label="Second polio vaccine licenced",x=1962,y=-770,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey") +
  annotate("segment", x = 1947, xend = 1950, y = 590, yend = 770,
           colour = "black", size = 1)+
  geom_label(label="Diphteria vaccine licenced",x=1950,y=790,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 1963, xend = 1960, y = 200, yend = 660,
           colour = "black", size = 1)+
  geom_label(label="Measles vaccine licenced",x=1960,y=680,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 1946, xend = 1946, y = -350, yend = -750,
           colour = "black", size = 1)+
  geom_label(label="Pertussis vaccine licenced",x=1948,y=-770,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 1969, xend = 1967, y = -600, yend = -950,
           colour = "black", size = 1)+
  geom_label(label="Rubella vaccine licenced",x=1967,y=-970,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 1967, xend = 1963, y = -200, yend = -150,
           colour = "black", size = 1)+
  geom_label(label="Mumps vaccine licenced",x=1960,y=-170,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 1995, xend = 1995, y = 500, yend = 750,
           colour = "black", size = 1)+
  geom_label(label="Chickenpox vaccine licenced",x=1995,y=770,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 1981, xend = 1981, y = -260, yend = -970,
           colour = "black", size = 1)+
  geom_label(label="Hepatits B licenced for high risk groups",x=1981,y=-990,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 1991, xend = 1991, y = -275, yend = -860,
           colour = "black", size = 1)+
  geom_label(label="Hepatits B vaccine licenced for all infants",x=1991,y=-880,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 1995, xend = 1998, y = -80, yend = -750,
           colour = "black", size = 1)+
  geom_label(label="Hepatits A licenced for high risk children",x=1999,y=-770,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")+
  annotate("segment", x = 2005, xend = 2007, y = 0, yend = -620,
           colour = "black", size = 1)+
  geom_label(label="Hepatits A vaccine licenced for all children",x=2007,y=-650,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")
  


#I also tried to make an interactive streamgraph but the labeling code for this graph didn't work:

sgg1 <- streamgraph(mydata, key="disease", value="cases", date="year", height="600px", width="1000px") %>%
  sg_legend(show=TRUE, label="Disease: ")
#I tried to label the events but the code didn't work:
sg_annotate(sgg1, label = "test", x = 1961, y = 100000, color = "black", size = 12)

#I believe the streamgraph shows the overall trend in a way that it would be easier and faster for the observer to see the changes in numbers and efficacy of vaccines. 
#However, some details are lost, such as the exact number of cases because of the data transformation. An alternative would be the interactive streamplot without data transformation,
#but because the magnitudes of the diseases are so different then those with low number of cases won't be visible.
#The faceted linegraphs show the trend more accurately but at a cost of not being as collective as the streamgraph.However, their simple design might be very helpful in conveying the information. 
