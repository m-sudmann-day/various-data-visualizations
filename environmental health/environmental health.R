# Matthew Sudmann-Day
# Barcelona GSE Data Science

library(rworldmap)
library(RColorBrewer)

showOneMap <- function(data, column, title, palette, legend)
{
  map <- joinCountryData2Map(data, joinCode="ISO3", nameJoinColumn = "ISO3v10")
  
  colors <- c("black", rev(brewer.pal(9, palette))[1:8])
  
  mapCountryData(map,
                 nameColumnToPlot=column,
                 numCats=length(colors),
                 catMethod="fixedWidth", 
                 colourPalette = colors, 
                 mapTitle = title, 
                 oceanCol="white", 
                 borderCol="lightgray",
                 addLegend=legend)
}

# I externally converted the Excel file to a CSV file.
data <- read.csv("EPI2014.csv", header=TRUE)

showOneMap(data, "EH...Health.Impacts", "Health Impacts", "Reds", FALSE)
showOneMap(data, "EH..Water.and.Sanitation", "Water Quality and Sanitation", "Blues", FALSE)
showOneMap(data, "EH...Air.Quality", "Air Quality", "Greens", FALSE)


