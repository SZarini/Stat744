#Hello, Sorry for the delay and that my codes are incomplete. I have very little  experience with coding and R in general and have been trying hard during the past few days to make the codes work.
#I will try to update the codes until Sunday.

#Jia You's graph is showing how the introduction and administration of vaccines successfully decreased case number of a lot of diseases throughout the history.

#For the first graph I decided to create multiple linegraphs using facet_wrap code to show the trends of each disease:

library(tidyverse)
mydata <- read.csv("vaccinedata.csv")

#creating a new data set to mark the vaccine licencing incidents:

point.data <- data.frame(year = c(1995, 1947, 1995, 2005, 1981, 1991, 1963,
                                  1967, 1949, 1955, 1961, 1969),
                         cases = c(120624, 12262, 31582, 4488, 21152, 18003, 385156, 0, 69479, 28985, 1312, 57686),
                         disease = c("Chickenpox", "Diphtheria", "Hepatitis A","Hepatitis A", 
                                     "Hepatitis B", "Hepatitis B", "Measles",
                                     "Mumps", "Pertussis", "Polio", "Polio", "Rubella"),
                         vaccine = c("Chickenpox vaccine is licensed and added to the routine childhood vaccine schedule.",
                                     "Diphtheria toxoid is part of DT, the first combination childhood vaccine licensed.",
                                     "Hepatitis A vaccine is licensed. CDC recommends it for children in high-risk communities.",
                                     "CDC extends hepatitis A vaccine recommendation to all children.",
                                     "Hepatitis B vaccine licensed. Initially used in high-risk groups like babies of infected mothers.",
                                     "CDC expands hepatitis B vaccine recommendation to all infants.",
                                     "The first measles vaccines are licensed.",
                                     "Mumps vaccine is licensed.",
                                     "Pertussis vaccine is part of first DTP childhood vaccine licensed.",
                                     "Jonas Salk's injectable polio vaccine is the first licensed. It uses killed virus.",
                                     "Albert Sabin's oral polio vaccine is licensed. It uses live, weakened virus.",
                                     "The first rubella vaccines licensed and recommended for all children."))


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
  geom_point(data=point.data2,aes(x = year, y = cases))+
  labs(title = "THE VACCINE WARS",
       x = "Year", y = "Cases")



#For the final step I decided to make the graph interactive by using ggplotly:

install.packages("plotly")
library(plotly)
ggplotly(p1)

#This way the reader can check the exact case number for each year by moving the cursor.
#Each event will also be displayed by putting the cursor on the dots.


#For the second graph I decided to create a streamgraph to show the overall efficacy of vaccines in controlling diseases:

install.packages("ggstream")
install.packages("paletteer")
library(cowplot)
library(paletteer)
library(dplyr)
library(colorspace)
library(ggstream)

#This code takes a little longer to run
gstream1 <- mydata %>%
  ggplot(aes(year, sqrt(cases), fill = disease, label = disease, color = disease)) + #I tried to plot the y axis on a log scale using "scale_y_log10()" but it resulted in some weird numbers
  geom_stream(extra_span = 0.013, type = "mirror", n_grid = 3000, bw = .78) +
  geom_stream_label(size = 5, type = "mirror", n_grid = 1000) +
  cowplot::theme_minimal_vgrid(font_size = 18) +
  theme(legend.position = "none") +
  scale_colour_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::darken(.8)) +
  scale_fill_manual(values = paletteer::paletteer_d("dutchmasters::milkmaid") %>% colorspace::lighten(.2)) +
  labs(title = "THE VACCINE WARS",
       x = "Year",
       y = "Cases (square root)") + #I couldn't figure out how to adjust the position of the disease type direct labeling. A few are not well positioned.
  annotate("segment", x = 1955, xend = 1955, y = -500, yend = -850, #I couldn't find a code to automatically add labels for each vaccine event so I decided to add them manually but it is taking a very long time
           colour = "black", size = 1)+
  geom_label(label="First polio vaccine licenced",x=1955,y=-870,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey") +
  annotate("segment", x = 1961, xend = 1960, y = -440, yend = -750,
           colour = "black", size = 1)+
  geom_label(label="Second polio vaccine licenced",x=1960,y=-770,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey") +
  annotate("segment", x = 1947, xend = 1950, y = 590, yend = 770,
           colour = "black", size = 1)+
  geom_label(label="Diphteria vaccine licenced",x=1950,y=790,label.padding = unit(0.25,"lines"),
             label.size=0.20,colour="black",fill="grey")


#I also tried to make an interactive streamgraph but the labeling code for this graph didn't work:

devtools::install_github("hrbrmstr/streamgraph")
install.packages("streamgraph")
library(streamgraph)
sgg1 <- streamgraph(mydata, key="disease", value="cases", date="year", height="600px", width="1000px") %>%
  sg_legend(show=TRUE, label="Disease: ")
#I tried to label the events but the code didn't work:
sg_annotate(sgg1, label = "test", x = 1961, y = 100000, color = "black", size = 12)

#I believe the streamgraph shows the overall trend in a way that it would be easier and faster for the observer to see the changes and efficacy of vaccines. 
#However, some details are lost, such as the exact number of cases because of the data transformation. An alternative would be the interactive streamplot without data transformation,
#but because the magnitudes of the diseases are so different then those with low number of cases won't be visible.
#The linegraphs show the trend more accurately but at a cost of not being as collective as the streamgraph.
