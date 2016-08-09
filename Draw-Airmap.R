library(ggmap)
data.port <- read.csv('~/code/airports.dat',F)
data.line <- read.csv('~/code/routes.dat',F)
library(stringr)
portinchina <- str_detect(data.port[,'V4'], "China")
chinaport <- data.port[portinchina,]
chinaport <-chinaport[chinaport$V5!='',
c('V3','V5','V7','V8','V9')]
names(chinaport) <- c('city','code','lan','lon','att')
 
lineinchina <- (data.line[,'V3'] %in% chinaport$code) & (data.line[,'V5'] %in% chinaport$code)
chinaline <- data.line[lineinchina,c('V3','V5','V9')]
names(chinaline) <- c('source','destination','equipment')
 
findposition <- function(code) {
find <- chinaport$code==code
    x <- chinaport[find,'lon']
    y <- chinaport[find,'lan']
return(data.frame(x,y))
}
 
from <- lapply(as.character(chinaline$source),findposition)
from <- do.call('rbind',from)
from$group <- 1:dim(from)[1]
names(from) <- c('lon','lan','group')
 
to <- lapply(as.character(chinaline$destination),findposition)
to <- do.call('rbind',to)
to$group <-1:dim(to)[1]
names(to) <-c('lon','lan','group')
data.line <- rbind(from,to)
temp<- data.line[data.line$group<100,]
ggmap(get_googlemap(center = 'china', zoom=4,
                    maptype='roadmap'),extent='device')+
    geom_point(data=chinaport,aes(x=lon,y=lan),
               colour = 'red4',alpha=0.8)+
    geom_line(data=data.line,aes(x=lon,y=lan,group=group),
              size=0.1,alpha=0.05,color='red4')
