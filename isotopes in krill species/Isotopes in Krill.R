# Matthew Sudmann-Day
# Barcelona GSE Data Science

library(ggplot2)
library(dplyr)

speciesNames <-  c("AT. inermis", "BT. inermis", "CT. raschii", "DT. longi", "EM. norvegica", "FN. megalops")
speciesColors <- c("#DDDDDD"
                   ,"#888888"
                   ,hcl(140,c=90,l=80) # bright green
                   ,hcl(240,c=90,l=70) # blue
                   ,hcl(0,c=95,l=45) # dark red
                   ,hcl(40,c=100,l=75) # orange                   
                   
)
speciesShapes <- c(1, 1, 15, 19, 17, 18)
speciesSizes <- c(4,4,4,4,4,5)

data <- read.csv("IsotopeData.csv")[,2:4]

agg <- summarise(group_by(data, Species), xavg=mean(X.13C), yavg=mean(X.15N))
data$xavg <- agg$xavg[data$Species]
data$yavg <- agg$yavg[data$Species]

data$Species = data$Species+1

data2 <- data[data$Species==2,]
data2$Species=1
data <- rbind(data, data2)

data$xavg[data$Species==2] = NA
data$yavg[data$Species==2] = NA

data$SpeciesName <- speciesNames[data$Species]

data = data[order(data$SpeciesName, decreasing=TRUE),]
hideLineInLegend <- guide_legend(override.aes = list(linetype = c(rep("blank", 6))))

plot <- ggplot (data=data, aes(x=X.13C, y=X.15N))
plot <- plot + geom_segment(data=data, aes(xend=xavg, yend=yavg, color=factor(SpeciesName), order=factor(SpeciesName)), size=0.5)
plot <- plot + geom_point(aes(color=factor(SpeciesName), shape=factor(SpeciesName), size=factor(SpeciesName), order=factor(SpeciesName)))
plot <- plot + scale_colour_manual(values = speciesColors, name="Species", guide=hideLineInLegend)
plot <- plot + scale_shape_manual(values = speciesShapes, name="Species")
plot <- plot + scale_size_manual(values = speciesSizes, name="Species")
plot <- plot + theme(legend.position=c(0.125,0.615)
                     , legend.key=element_rect(fill=NA)
                     , legend.background=element_rect(fill="transparent")
                     , panel.background=element_rect("white")
                     , panel.border=element_rect("darkgray", fill=NA))
plot <- plot + ylab("Isotope 15N")
plot <- plot + scale_x_continuous("Isotope 13C", breaks=c(-23,-22,-21,-20))#, labels, limits)
plot <- plot + ggtitle("Isotope Presence in Krill Species")
plot

ggsave("before manual cleanup.png", width=8, height=6, units="in", dpi=300)
