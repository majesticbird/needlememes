library(ggplot2)

##### house gauge

gg.gauge <- function(pos,breaks=c(0,25,50,60,70,80,85,90,95,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  
ggplot() + 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#EBF5FF")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#E3EEF9")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#C7DEF4")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#ABCDEF")+
    geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#8FBDEA")+
    geom_polygon(data=get.poly(breaks[6],breaks[7]),aes(x,y),fill="#73ADE4")+
    geom_polygon(data=get.poly(breaks[7],breaks[8]),aes(x,y),fill="#579CDF")+
    geom_polygon(data=get.poly(breaks[8],breaks[9]),aes(x,y),fill="#3B8CDA")+
    geom_polygon(data=get.poly(breaks[9],breaks[10]),aes(x,y),fill="#207CD5")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) +
    ggtitle("Dem House Probability") + theme(plot.title=element_text(face="bold"))
}

##### senate duplicate

gg.gauge.2 <- function(pos,breaks=c(0,25,50,60,70,80,85,90,95,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  
ggplot() + 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#EBF5FF")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#E3EEF9")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#C7DEF4")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#ABCDEF")+
    geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#8FBDEA")+
    geom_polygon(data=get.poly(breaks[6],breaks[7]),aes(x,y),fill="#73ADE4")+
    geom_polygon(data=get.poly(breaks[7],breaks[8]),aes(x,y),fill="#579CDF")+
    geom_polygon(data=get.poly(breaks[8],breaks[9]),aes(x,y),fill="#3B8CDA")+
    geom_polygon(data=get.poly(breaks[9],breaks[10]),aes(x,y),fill="#207CD5")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) +
    ggtitle("Dem Senate Probability") + theme(plot.title=element_text(face="bold"))
}
