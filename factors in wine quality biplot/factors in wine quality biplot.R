# Matthew Sudmann-Day
# Barcelona GSE Data Science

library(ggord)
library(RColorBrewer)
library(ggbiplot)

load("C:\\OneDrive\\BGSE\\DV\\HW3\\wine.RData") #wine
wine[,1:11] <- scale(wine[,1:11], center=TRUE, scale=apply(wine[,1:11], 2, sd))

#wine$quality <- ifelse (wine$quality==3,1, ifelse (wine$quality==7,3, ifelse (wine$quality==8,4,2)))
wine$quality <- (wine$quality-2) *2
colnames(wine)[1] <- "fixed acidity"
colnames(wine)[6] <- "free SO2"
colnames(wine)[7] <- "total SO2"
colnames(wine)[8] <- "density, chl."
colnames(wine)[5] <- ""
colnames(wine)
wine.pca <- princomp(wine)

plot <- ggbiplot(wine.pca, obs.scale = 1, var.scale = 2, groups=factor(wine[,12]),
                 ellipse = TRUE, varname.abbrev=FALSE, ellipse.prob = 0.95,
                 varname.size=5, pc.biplot=TRUE, alpha=0.3)
plot <- plot + scale_color_manual(values=c("#B0B0B0","#808080","#6A6060","#7A4040","#8A2020","#9A0000"))
plot <- plot + theme_bw() + xlab("") + ylab("")
plot <- plot + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)
plot1 <- plot
plot1

ggsave("C:\\OneDrive\\BGSE\\DV\\HW3\\WineB1.png", width=8, height=6, units="in", dpi=300)

plot <- ggbiplot(wine.pca, obs.scale = 1, var.scale = 2, groups=factor(wine[,12]),
                 ellipse = TRUE, varname.abbrev=FALSE, ellipse.prob = c(0.95),
                 varname.size=5, pc.biplot=TRUE, alpha=0)
plot <- plot + scale_color_manual(values=c("#B0B0B0","#808080","#6A6060","#7A4040","#8A2020","#9A0000"))
plot <- plot + theme_bw() + xlab("") + ylab("")
plot <- plot + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)
plot2 <- plot
plot2

ggsave("C:\\OneDrive\\BGSE\\DV\\HW3\\WineB2.png", width=8, height=6, units="in", dpi=300)

plot <- ggbiplot(wine.pca, obs.scale = 1, var.scale = 2, groups=factor(wine[,12]),
                 ellipse = TRUE, varname.abbrev=FALSE, ellipse.prob = c(0.95),
                 varname.size=5, pc.biplot=TRUE, alpha=0)
clr <- rgb(1,1,1, alpha=0)
plot <- plot + scale_color_manual(values=c(clr,clr,clr,clr,clr,clr))
plot <- plot + theme_bw() + xlab("") + ylab("")
plot <- plot + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)
plot3 <- plot
plot3

ggsave("C:\\OneDrive\\BGSE\\DV\\HW3\\WineB3.png", width=8, height=6, units="in", dpi=300)
