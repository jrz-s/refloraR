#################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

#' Get graph line 

#' database is a dataframe with three columns. 
#' First column is the time vector
#' second and thrid are the variables to plot.

# -------------------------------------------------------------------------
# Graphic

get_time_line <- function(database = db
                          ,name.v1 = 'Species records'
                          ,name.v2 = 'Fire occurrence'
                          ,col.v1 = "#619CFF"
                          ,col.v2 = "tomato"
                          ,x.lab = '\nYears'
                          ,y.lab = 'Count occurrences\n'
                          ,linewidth = 0.10
                          ,y.ls = 30
                          ,y.li = 0
                          ,breaks = 5
                          ,leg.linewidth = 0.5
                          ,ticks.length = 0.25
                          ,legend.position = c(0.7,0.9)
                          ,x.title.size = 8
                          ,y.title.size = 9
                          ,axis.text.size = 7
                          ,legend.text = 7
                          ,x.text.axis = 0
                          ,point.size = 0.8){
  
  colnames(database)[1] <- 'time'
  colnames(database)[-1] <- paste0('v',1:(ncol(database)-1))
  
  #label
  scaleFUN <- function(x) sprintf("%.0f", x)
  
  #graph
  p1 <- ggplot() +
    
    geom_line(mapping = aes(time, v1, linetype = name.v1)
              ,colour = col.v1
              ,alpha = 0.8
              ,linewidth = linewidth
              ,data = database) +
    
    geom_line(mapping = aes(time,v2,linetype = name.v2)
              ,colour = col.v2
              ,alpha = 0.8
              ,linewidth = linewidth
              ,data = database) +
    
    geom_point(mapping = aes(time, v1)
               ,colour = col.v1
               ,alpha = 0.8
               ,size = point.size
               ,data = database) + 
    
    geom_point(mapping = aes(time, v2)
               ,colour = col.v2
               ,alpha = 0.8
               ,size = point.size
               ,data = database) +
    
    guides(linetype = guide_legend(title = NULL
                                   ,override.aes = list(linewidth = c(leg.linewidth,leg.linewidth)
                                                        ,linetype = c("solid","solid")
                                                        ,color = c(col.v1,col.v2)
                                                        ,alpha = c(0.8,0.8)))) +
    
    scale_linetype_manual(values = c("solid","solid")) +
    
    scale_x_continuous(
      limits = c(min(database$time),max(database$time))
      ,breaks = seq(min(database$time),max(database$time),2)) +
    
    # definir automaticamente
    scale_y_continuous(
      expand = c(0, 0)
      ,labels = scaleFUN
      ,limits = c(y.li,y.ls)
      ,breaks = seq(y.li,y.ls,(y.ls-y.li)/breaks)) +
    
    xlab(x.lab) +
    
    ylab(y.lab) +
    
    theme_light() +
    
    theme( axis.line = element_line(colour = 'black',linewidth = linewidth)
           ,axis.ticks = element_line(colour = "black",linewidth = linewidth)
           ,axis.ticks.length = unit(ticks.length,"cm")
           ,axis.title.x = element_text(colour = 'black',size = x.title.size)
           ,axis.title.y = element_text(colour = 'black',size = y.title.size)
           ,axis.text = element_text(colour = 'black',size = axis.text.size)
           ,axis.text.x = element_text(angle = x.text.axis)
           ,legend.text = element_text(size = legend.text)
           ,legend.direction = "horizontal"
           ,legend.position = legend.position
           ,legend.background = element_rect(fill = "transparent", colour = NA)
           ,legend.key = element_rect(fill = "transparent")
           ,plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")
           ,plot.background = element_rect(fill = "#F5F4EF", colour = NA)
           ,panel.background = element_rect(fill = "#F5F4EF",colour = "transparent")
           ,panel.border = element_rect(colour = 'transparent')
           ,panel.grid = element_line(linewidth = linewidth)) 
  
  p1
  
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
