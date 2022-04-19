#load library

library(jpeg)
library(tidyverse)
library(data.table)
library(geosphere)
library(grid)
library(plyr)
library(stringr)

#choose point and line data
data.point <- read.csv(file.choose(),F)
data.line <- read.csv(file.choose(),F)

#data processing
names(data.point)<- c('city','lon','lat')
names(data.line)<-c('source','destination')

findposition <- function(city) {
  find <- data.point$city==city
  x <- data.point[find,'lon']
  y <- data.point[find,'lat']
  return(data.frame(x,y))
}

from <- lapply(as.character(data.line$source),findposition)
from <- do.call('rbind',from)
from$group <- 1:dim(from)[1]
names(from) <- c('from_lon','from_lat','category')
to <- lapply(as.character(data.line$destination),findposition)
to <- do.call('rbind',to)
to$group <- 1:dim(to)[1]
names(to) <-c('to_lon','to_lat','category')
dat.fromto <- cbind(from,to)
dat.fromto <- dat.fromto[-c(3,6)]

inter <- function(x){
  gcIntermediate(x[,c("from_lon","from_lat")],
                 x[,c("to_lon","to_lat")],
                 n=50,breakAtDateLine = FALSE,
                 addStartEnd = TRUE,sp=TRUE)
}

dat.new <- inter(dat.fromto)
dat.trans <- ldply(dat.new@lines, fortify)
dat.point.new <- dat.trans %>%
  group_by(group) %>%
  filter(row_number() == 1 | row_number() == n())
  
earth <- readJPEG("D:/R programming/World Cruise/BlackMarble_2016_3km.jpg", native = TRUE)
earth <- rasterGrob(earth, interpolate = TRUE)

write.csv(dat.trans,"D:/R programming/World Cruise/dat_trans.csv")
write.csv(dat.point.new,"D:/R programming/World Cruise/dat_pointnew.csv")

library(ggplot2)
ggplot()+
  annotation_custom(earth, xmin = 24, xmax = 180, ymin = -90, ymax = 90) +
  geom_path(data=dat.trans,aes(x = long, y = lat,group=id),
            alpha = 0.8, size = 0.3 , color="#f9ba00")+
  geom_point(data=dat.point.new, aes(long, lat), alpha = 0.8, size=0.1,color = "white") +
  theme(panel.background = element_rect(fill = "#05050f", colour = "#05050f"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.title = element_blank(),axis.text = element_blank(),
        axis.ticks.length = unit(0, "cm"),legend.position = "none")+
  #annotate("text", x = -170, y = -50, hjust = 0, size = 7,
         #label = paste("Title1"), color = "#14ffec") +
  #annotate("text", x = -170, y = -58, hjust = 0, size = 3,
          #label = paste("Title2"), color = "white") +
  #annotate("text", x = -170, y = -61, hjust = 0, size = 2,
           #label = paste("datasource:"), 
           #color = "white", alpha = 0.5) +
  coord_equal()



