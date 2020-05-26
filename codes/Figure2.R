library(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

funcShadedright <- function(x) {
  y <- dnorm(x, mean = 0, sd = 1)
  y[x <= 1.5] <- NA
  return(y)
}

funcShadedleft <- function(x) {
  y <- dnorm(x, mean = 2, sd = 1)
  y[x >=1.65] <- NA
  return(y)
}

p1 <- ggplot(data = data.frame(x = c(-5, 7)), aes(x)) +
  stat_function(fun = dnorm, n = 1000, args = list(mean = 0, sd = 1),linetype=1) + stat_function(fun = dnorm, n = 1000, args = list(mean = 2, sd = 1),linetype=2)+ ylab("") +geom_vline(xintercept = 1.6,linetype=3)+stat_function(fun=funcShadedright, geom="area", fill="royalblue4", alpha=0.8)+stat_function(fun=funcShadedleft, geom="area", fill="gray52", alpha=0.6)+
  scale_y_continuous(breaks = NULL)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.title.x=element_blank(),text = element_text(size=15),
                                          axis.ticks.x=element_blank())+scale_x_continuous(breaks=c(-5,-2.5,0,1.65,2.5,5,7))



funcShadedright <- function(x) {
  y <- dnorm(x, mean = 0, sd = 1)
  y[x <= 0.95] <- NA
  return(y)
}

funcShadedleft <- function(x) {
  y <- dnorm(x, mean = 2, sd = 1)
  y[x >=1.05] <- NA
  return(y)
}

p2 <- ggplot(data = data.frame(x = c(-5, 7)), aes(x)) +
  stat_function(fun = dnorm, n = 1000, args = list(mean = 0, sd = 1)) + stat_function(fun = dnorm, n = 1000, args = list(mean = 2, sd = 1),linetype=2)+ ylab("") +geom_vline(xintercept = 1,linetype=3)+stat_function(fun=funcShadedright, geom="area", fill="royalblue4", alpha=0.8)+stat_function(fun=funcShadedleft, geom="area", fill="gray52", alpha=0.6)+
  scale_y_continuous(breaks = NULL)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.title.x=element_blank(),text = element_text(size=15),
                                          axis.ticks.x=element_blank())+scale_x_continuous(breaks=c(-5,-2.5,0,1,2.50,5,7))

funcShadedright <- function(x) {
  y <- dnorm(x, mean = 0, sd = 1)
  y[x <= 0.6] <- NA
  return(y)
}

funcShadedleft <- function(x) {
  y <- dnorm(x, mean = 2, sd = 1)
  y[x >=0.7] <- NA
  return(y)
}

# This plot is by setting 50% missing data in class one, to calculate the optimal frontier in dimension 1. x=(2+log(0.5))/2
p3 <- ggplot(data = data.frame(x = c(-5, 7)), aes(x)) +
  stat_function(fun = dnorm, n = 1000, args = list(mean = 0, sd = 1)) + stat_function(fun = dnorm, n = 1000, args = list(mean = 2, sd = 1),linetype=2)+ ylab("") +geom_vline(xintercept = 0.65,linetype=3)+stat_function(fun=funcShadedright, geom="area", fill="royalblue4", alpha=0.8)+stat_function(fun=funcShadedleft, geom="area", fill="gray52", alpha=0.6)+
  scale_y_continuous(breaks = NULL)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.title.x=element_blank(),text = element_text(size=15),
                                          axis.ticks.x=element_blank())+scale_x_continuous(breaks=c(-5,-2.5,0,0.65,2.5,5,7))
multiplot(p1, p1, p2, p3, cols=2)