library (ggplot2)

#' GAAPItch
#'
#' @param grass_colour
#' @param line_colour
#' @param background_colour
#' @param goala_colour
#' @param goalb_colour
#' @param tram_colour
#' @param BasicFeatures
#'
#' @return
#' @export
#'
#' @examples
create_GAA_Pitch <- function(grass_colour, line_colour, background_colour, goala_colour,goalb_colour,tram_colour, BasicFeatures){

  theme_blankPitch = function(size=12) {
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      #axis.ticks.y=element_text(size=size),
      #axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      #axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      # panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      panel.background=element_rect(fill= background_colour,colour= background_colour),
      strip.text.x=element_text(size=size*1))}

  ymin <- 0 # minimum width
  ymax <- 98 # maximum width
  xmin <- 0 # minimum length
  xmax <- 144 # maximum length

  # pitch lines
  def13 <- 13
  def20 <- 20
  def45 <- 45
  def65 <- 65
  off65 <- 79
  off45 <- 99
  off20 <- 124
  off13 <- 131


  # small sqaure

  sixYardrd <- 42
  sixYardld <- 56
  sixYardDefxst <- 4.5
  sixYardOffxst <- 139.5

  #13 box
  right14d <- 39.5
  left14d <- 58.5

  #Goals
  glxst <- 0
  glyst <- 45.75
  glxed <- 144
  glyed <- 52.25

  #halfway line
  hwxst <- 72
  hwyst <- 45.75
  hwyed <- 52.25

  #pen spot

  pdxst <- 11
  pdyst <- 48.5
  pdyed <- 49.5
  poxst <- 133

  # tramlines
  srwing <- 24.49
  rhalfsp <- 48.99
  ctre <- 73.49


  circleFundef <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
  {
    tt <- seq(start*pi, end*pi, length.out=npoints)
    data.frame(x = center[1] + diameter / 2 * cos(tt),
               y = center[2] + diameter / 2 * sin(tt))
  }

  circleFunatt <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
  {
    tt <- seq(start*pi, end*pi, length.out=npoints)
    data.frame(x = center[1] - diameter / 2 * cos(tt),
               y = center[2] - diameter / 2 * sin(tt))
  }
  #### create semi circle ####
  def_circle <-  circleFundef(c(20,49), 26, start=1.5, end=2.5)
  att_circle <-  circleFunatt(c(124,49), 26, start=1.5, end=2.5)

  if(BasicFeatures == TRUE){
    ## initiate the plot, set some boundries to the plot
    np <- ggplot() + xlim(c(xmin,xmax)) + ylim(c(ymin,ymax)) +
      # add the theme
      theme_blankPitch() +
      # add lines goals etc
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
      geom_segment(aes(x = def13, y = ymin, xend = def13, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = def20, y = ymin, xend = def20, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = def45, y = ymin, xend = def45, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = def65, y = ymin, xend = def65, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = off65, y = ymin, xend = off65, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = off45, y = ymin, xend = off45, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = off20, y = ymin, xend = off20, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = off13, y = ymin, xend = off13, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmin, y = right14d, xend = def13, yend = right14d),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmin, y = left14d, xend = def13, yend = left14d),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmax, y = right14d, xend = off13, yend = right14d),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmax, y = left14d, xend = off13, yend = left14d),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = hwxst, y = hwyst, xend = hwxst, yend = hwyed),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmin, y = sixYardrd, xend = sixYardDefxst, yend = sixYardrd),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmin, y = sixYardld, xend = sixYardDefxst, yend = sixYardld),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = sixYardDefxst, y = sixYardrd, xend = sixYardDefxst, yend = sixYardld),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmax, y = sixYardrd, xend = sixYardOffxst, yend = sixYardrd),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmax, y = sixYardld, xend = sixYardOffxst, yend = sixYardld),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = sixYardOffxst, y = sixYardrd, xend = sixYardOffxst, yend = sixYardld),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = pdxst, y = pdyst, xend = pdxst, yend = pdyed),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = poxst, y = pdyst, xend = poxst, yend = pdyed),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = glxst, y =  glyst, xend = glxst, yend = glyed),colour = goala_colour, size = 1) +
      geom_segment(aes(x =glxed, y =  glyst, xend =glxed, yend = glyed),colour = goalb_colour, size = 1) +
      geom_path(data=def_circle, aes(x,y), colour = line_colour,size = 0.5) +
      geom_path(data=att_circle, aes(x,y), colour = line_colour,size = 0.5) +
      annotate(geom="text", x= 131, y=2, label="www.witnesstheanalysis.com",color = "white",size = 4, alpha = 0.4) +
      annotate(geom="text", x= 12, y=97, label="www.witnesstheanalysis.com",color = "white",size = 4, alpha = 0.4)
  }

  else{
    ## initiate the plot, set some boundries to the plot
    np <- ggplot() + xlim(c(xmin,xmax)) + ylim(c(ymin,ymax)) +
      # add the theme
      theme_blankPitch() +
      # add lines goals etc
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
      geom_segment(aes(x = def13, y = ymin, xend = def13, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = def20, y = ymin, xend = def20, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = def45, y = ymin, xend = def45, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = def65, y = ymin, xend = def65, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = off65, y = ymin, xend = off65, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = off45, y = ymin, xend = off45, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = off20, y = ymin, xend = off20, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = off13, y = ymin, xend = off13, yend = ymax),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmin, y = right14d, xend = def13, yend = right14d),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmin, y = left14d, xend = def13, yend = left14d),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmax, y = right14d, xend = off13, yend = right14d),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmax, y = left14d, xend = off13, yend = left14d),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = hwxst, y = hwyst, xend = hwxst, yend = hwyed),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmin, y = sixYardrd, xend = sixYardDefxst, yend = sixYardrd),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmin, y = sixYardld, xend = sixYardDefxst, yend = sixYardld),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = sixYardDefxst, y = sixYardrd, xend = sixYardDefxst, yend = sixYardld),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmax, y = sixYardrd, xend = sixYardOffxst, yend = sixYardrd),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = xmax, y = sixYardld, xend = sixYardOffxst, yend = sixYardld),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = sixYardOffxst, y = sixYardrd, xend = sixYardOffxst, yend = sixYardld),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = pdxst, y = pdyst, xend = pdxst, yend = pdyed),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = poxst, y = pdyst, xend = poxst, yend = pdyed),colour = line_colour,linetype="solid",size = 0.5) +
      geom_segment(aes(x = glxst, y =  glyst, xend = glxst, yend = glyed),colour = goala_colour, size = 1) +
      geom_segment(aes(x =glxed, y =  glyst, xend =glxed, yend = glyed),colour = goalb_colour, size = 1) +
      geom_segment(aes(x = xmin, y = srwing, xend = xmax, yend = srwing),colour = tram_colour,linetype="dotted",size = 0.5) +
      geom_segment(aes(x = xmin, y = rhalfsp, xend = xmax, yend = rhalfsp),colour = tram_colour,linetype="dotted",size = 0.5) +
      geom_segment(aes(x = xmin, y = ctre, xend = xmax, yend = ctre),colour = tram_colour,linetype="dotted",size = 0.5) +
      geom_segment(aes(x = 72, y = ymin, xend = 72, yend = ymax),colour = tram_colour,linetype="dotted",size = 0.5) +
      geom_path(data=def_circle, aes(x,y), colour = line_colour,size = 0.5) +
      geom_path(data=att_circle, aes(x,y), colour = line_colour,size = 0.5) +
      annotate(geom="text", x= 131, y=2, label="www.witnesstheanalysis.com",color = "white",size = 4, alpha = 0.4) +
      annotate(geom="text", x= 12, y=97, label="www.witnesstheanalysis.com",color = "white",size = 4, alpha = 0.4)
  }

  return(np)}

create_GAA_Pitch("#A2CD5A", "white", "lightblue", "blue","red","black", BasicFeatures=TRUE)
