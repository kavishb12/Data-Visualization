library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggrepel)

Minard = read.csv("minard-data.csv",header=TRUE,sep=",")

Troops = select(Minard, long = LONP, lat = LATP, survivors = SURV, direction = DIR, division = DIV)
Cities = select(Minard, long = LONC, lat = LATC, city = CITY)
Cities[complete.cases(Cities), ]

Temp = select(Minard, long=LONT, temp=TEMP,MON,DAY)
Temp$date = paste(Temp$temp,'°',Temp$MON, Temp$DAY)
Temp[complete.cases(Temp), ]

p_troops = ggplot(Troops, aes(long, lat)) 
geom_path(mapping = NULL, data = NULL, stat = "identity", position = "identity", lineend = "butt", 
          linejoin = "round", linemitre = 1, arrow = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
p_cities = p_troops + geom_path(aes(size = survivors, color = direction, group = division), lineend = "round", 
                                linejoin = "round")

p_cities_labels = p_cities + geom_text(aes(label = city), size = 4, data = Cities, position = position_nudge(y=0.1,x=-0.1)) + 
  geom_point(data = Cities) + geom_text_repel(aes(label = survivors), size = 3, data = Troops,position = position_nudge(x=-0.1,y=-0.1))

v = c(1, 2, 3) * 10^5

p_march = p_cities_labels + scale_size("Survivors", range = c(1, 22), breaks = v, labels = v) +
  scale_color_manual("Direction", values = c("#8dd3c7", "#fb8072")) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Napoleon's march to Russia")
p_march + guides(size=FALSE)

p_temp = ggplot(Temp, aes(long, temp)) +
  geom_path(color="grey", size=2.5) +
  geom_point(size=2) +
  geom_text(aes(label=date),hjust=1, vjust=2) +
  xlab("Longitude") + ylab("Temperature") +
  coord_cartesian(xlim = c(24, 37.5)) + 
  theme_bw()

grid.arrange(p_march + guides(size=FALSE)+guides(color=FALSE)+theme_bw() + theme(
  plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
) + theme(axis.line = element_line(color = 'black')), p_temp + theme(
  plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
) + theme(axis.line = element_line(color = 'black')), nrow=2, heights=c(4,2) )
