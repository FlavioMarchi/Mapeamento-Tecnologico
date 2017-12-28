grafico_mundo <- function(dados){
  
  library(scales)
  
  WorldData <- map_data(map = 'world')
  WorldData %>% filter(region != "Antarctica") -> WorldData
  WorldData <- fortify(WorldData)
  
  
  p <- ggplot()
  # p <- p +geom_rect(xmin = -Inf, xmax = Inf,   ymin = -Inf, ymax = Inf,   fill = "red")
  p <- p + geom_map(data=WorldData, map=WorldData,
                    aes(x=long, y=lat, group=group, map_id=region),
                    fill="gray90", colour="#7f7f7f", size=0.5)
  p <- p + geom_map(data=dados, map=WorldData,
                    aes(fill=total, map_id=country),
                    colour="#7f7f7f", size=0.5) 
  p <- p + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
  p <- p + scale_fill_continuous(low="#fee0d2", high="#de2d26", 
                                 guide="colorbar", labels = scientific)
  p <- p + scale_y_continuous(breaks=c())
  p <- p + scale_x_continuous(breaks=c())
  p <- p + labs(fill="N Patentes", title="", x="", y="")
  p <- p + theme_bw()
  p <- p + theme(panel.border = element_blank())
  p <- p + theme(panel.background = element_rect(fill = "lightcyan"))

  
  return(p)
  
}