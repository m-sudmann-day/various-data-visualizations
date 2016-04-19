# Matthew Sudmann-Day
# Barcelona GSE Data Science

if (FALSE)
{
  library(ggplot2)
  library(gridExtra)
  library(dplyr)

  df = read.csv("Bjoernoeya.csv")
}

groupedByMonth = as.data.frame(summarise(group_by(df, Month), avg=median(TAM), sd=sd(TAM)))

for (row in 1:nrow(df))
{
  month = df$Month[row]
  monthData = groupedByMonth[month,]
  numSDs = (df$TAM[row] - monthData$avg)/monthData$sd
  df$NumSDs[row] = numSDs
  df$Rank[row] = ifelse(numSDs >= 2, 5,
                   ifelse(numSDs >= 1, 4,
                          ifelse(numSDs > -1, 3,
                                 ifelse(numSDs > -2, 2,
                                        1))))
}


plot_month = function(month, monthName, skip_lower, skip_upper, show_years=FALSE)
{
  skip_lower=0;skip_upper=0
  
  df_smooth = df[df$Month==month,]
  df_point = df[df$Month==month,]
  
  while(skip_lower > 0)
  {
    i = which.min(df_point$TAM)
    cat(paste(monthName, df_point$Year[i], "lower\n"))
    df_point=df_point[-i,]
    skip_lower = skip_lower-1
  }
  
  while(skip_upper > 0)
  {
    i = which.max(df_point$TAM)
    cat(paste(monthName, df_point$Year[i], "upper\n"))
    df_point=df_point[-i,]
    skip_upper = skip_upper-1
  }
  
  plot = ggplot() + ggtitle(monthName)
  plot = plot + geom_smooth(data=df_smooth, aes(x=Year, y=TAM), method = "loess", level=0.9, size=1, color="green", fill="lightgray")
  plot = plot + geom_point(data=df_point, aes(x=Year, y=TAM, color=factor(Rank)), size=2, shape=19)
  plot = plot + scale_colour_manual(values = c("1"="blue","2"="#336699","3"="black","4"="darkred","5"="red"))
  plot = plot + theme(legend.position="none",
                      axis.title = element_blank(),
                      panel.background=element_rect("white"),
                      panel.border=element_rect("darkgray", fill=NA))
  if (!show_years)
  {
    plot = plot + theme(axis.text.x = element_blank(),
                        axis.ticks.x = element_blank())
  }
  plot = plot + scale_x_continuous(expand=c(0.01,0.01))
  
  if (month == 4) {
    plot = plot + scale_y_continuous(expand=c(0.02,0.02), breaks=seq(-10,0,2))
  } else if (month == 11) {
    plot = plot + scale_y_continuous(expand=c(0.025,0.025), breaks=seq(-8,0,2))
  } else {
    plot = plot + scale_y_continuous(expand=c(0.02,0.02))
  }
  ggsave(paste("months\\with outliers", month, ".png", sep=""), width=4, height=3, units="in", dpi=300)
  return(plot)
}

#pdf("filename.pdf")
grid.arrange(plot_month(1, "January",0,1),
             plot_month(2, "February",4,0),
             plot_month(3, "March",1,1),
             plot_month(4, "April",1,0),
             plot_month(5, "May",1,0),
             plot_month(6, "June",3,2),
             plot_month(7, "July",1,1),
             plot_month(8, "August",0,1),
             plot_month(9, "September",0,3),
             plot_month(10, "October",1,1,TRUE),
             plot_month(11, "November",1,0,TRUE),
             plot_month(12, "December",2,1,TRUE),
             padding=0)
#dev.off()

#ggsave("Matthew - HW1 - Bjørnøya.png", width=12, height=9, units="in", dpi=300)
