library(ggplot2)
library(ggthemes)
library(grid)
library(directlabels)


data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
        c(mean = mean(x[[col]], na.rm=TRUE),
          sd = sd(x[[col]], na.rm=TRUE))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    
    return(data_sum)
}

base_breaks_x <- function(x){
    b <- x
    d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
    list(geom_segment(data=d, size=1.3, colour="gray35", aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
         scale_x_continuous(breaks=b))
}

# For experiment 1, I used 
# d <- data.frame(y=-Inf, yend=-Inf, x=min(b) + 1, xend=max(b) + 1)

base_breaks_x_discrete<- function(x, my.xend=5, scaled.scale=F){
    b <- x
    d <- data.frame(y=-Inf, yend=-Inf, x=1, xend=my.xend)
    if (scaled.scale) {
        scale = scale_x_discrete(limits=b)
    } else {
        scale = scale_x_discrete(limits=as.factor(b))
        }
        list(geom_segment(data=d, size=1.3, colour="gray35", aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE), scale)
}

base_breaks_y <- function(x){
  b <- x
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  list(geom_segment(data=d, size=1.3, colour="gray35", aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_y_continuous(breaks=b))
}



#' Create a box-plot
plot.x.by.y <- function(df, x, y, xlab, ylab, out.name, report.dir,
                        plot.expected.error=FALSE, start.x.at=0,
                        plot.smooth=FALSE, extreme.outlier.count=NA,
                        count.outliers=F,
                        custom.base.breaks.x=c(0, 1, 2, 3, 4),
                        custom.base.breaks.y=seq(0, 25, 5),
                        my.xend=5,
                        my.width=2.0,
                        scaled.scale=T) {
    cbPalette <- c("#0072B2")
    
    p <- ggplot(df, aes_string(x=x,y=y)) +
        geom_boxplot(width=my.width, aes_string(group=x, fill="approach")) +
        theme_classic() +
         theme(axis.text=element_text(size=20, colour="gray25"),
              axis.title=element_text(size=20, colour="gray25"),
              axis.line = element_blank(),              
              axis.ticks.length=unit(-0.25, "cm"),
              axis.ticks = element_line(colour = 'gray25'),
              panel.spacing.x=unit(.8, "lines"),
              legend.position="none",
              strip.background = element_rect(size = 1.3),
              axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm"),
                                         angle = 0, vjust = 0, hjust=0.5),
              axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))  +
        ylab(ylab) +
        xlab(xlab) + 
        base_breaks_y(custom.base.breaks.y) +
        #base_breaks_x_discrete(custom.base.breaks.x, my.xend, scaled.scale) +
        scale_fill_manual(values=cbPalette)
    out.name <- file.path(report.dir, out.name)
    print(out.name)
    ggsave(out.name, width=7, height=4)    
}


# Create two box-plots per setting side-by-side to compare reality and simulation
plot.x.by.y.side.by.side <- function(df, x, y, xlab, ylab, out.name, report.dir,
                                     plot.expected.error=FALSE, start.x.at=0,
                                     plot.smooth=FALSE, extreme.outlier.count=NA,
                                     count.outliers=F,
                                     custom.base.breaks.x=c(0, 1, 2, 3, 4),
                                     custom.base.breaks.y=seq(0, 25, 5),
                                     x.text = 1,
                                     y.text = 12,
                                     my.width=0.75,
                                     my.xend=5,
                                     my.y.label="simulation",
                                     my.x.shift=0.25,
                                     my.x.shift.2=0.12,
                                     scaled.scale=F
                                     ) {

    cbPalette <- c("#009E73", "#0072B2")
    df[,x] <- as.factor(df[,x])
    df$num.robots <- as.factor(df$num.robots)
    
    p <- ggplot(df, aes_string(x=x,y=y)) +
        geom_boxplot(width=my.width, aes_string(fill="approach")) +
        annotate(geom = "text", hjust=0, vjust=0, x = x.text - my.x.shift.2, y = y.text, size = 6, label = "reality", color = cbPalette[1],
                 angle = 90) +
        annotate(geom = "text", hjust=0, vjust=0, x = x.text + my.x.shift, y = y.text, size = 6, label = my.y.label, color = cbPalette[2],
                 angle = 90) +         
        theme_classic() +
        theme(axis.text=element_text(size=20, colour="gray25"),
              axis.title=element_text(size=20, colour="gray25"),
              axis.line = element_blank(),              
              axis.ticks.length=unit(-0.25, "cm"),
              axis.ticks = element_line(colour = 'gray25'),
              panel.spacing.x=unit(.8, "lines"),
              legend.position="none",
              strip.background = element_rect(size = 1.3),
              axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm"),
                                         angle = 0, vjust = 0, hjust=0.5),
              axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))  +
        ylab(ylab) +
        xlab(xlab) + 
        base_breaks_y(custom.base.breaks.y) +
        base_breaks_x_discrete(custom.base.breaks.x, my.xend, scaled.scale) +
        scale_fill_manual(values=cbPalette)
    
    out.name <- file.path(report.dir, out.name)
    print(out.name)
    ggsave(out.name, width=7, height=4)    
}


# Create two box-plots per setting side-by-side to compare reality and simulation
plot.x.by.y.side.by.side.three <- function(df, x, y, xlab, ylab, out.name, report.dir,
                                     plot.expected.error=FALSE, start.x.at=0,
                                     plot.smooth=FALSE, extreme.outlier.count=NA,
                                     count.outliers=F,
                                     custom.base.breaks.x=c(0, 1, 2, 3, 4),
                                     custom.base.breaks.y=seq(0, 25, 5),
                                     x.text = 1,
                                     y.text = 12,
                                     my.width=0.75,
                                     my.xend=5,
                                     my.y.label="simulation same",
                                     my.x.shift=0.25,
                                     my.x.shift.2=0.12,
                                     my.x.shift.3=0.37,
                                     scaled.scale=F
                                     ) {

    cbPalette <- c("#009E73", "#0072B2", "#CC79A7")

    df$approach <- factor(df$approach,
                          levels = c( "reality", "simulation", "simulation_random"),
                          ordered = TRUE)
    
    df[,x] <- as.factor(df[,x])
    df$num.robots <- as.factor(df$num.robots)
    
    p <- ggplot(df, aes_string(x=x,y=y)) +
        geom_boxplot(width=my.width, aes_string(fill="approach")) +
        annotate(geom = "text", hjust=0, vjust=0, x = x.text - 0.2, y = y.text, size = 6, label = "reality", color = cbPalette[1],
                 angle = 90) +
        annotate(geom = "text", hjust=0, vjust=0, x = x.text + 0.05, y = y.text, size = 6, label = "sim. constant", color = cbPalette[2],
                 angle = 90) +
        annotate(geom = "text", hjust=0, vjust=0, x = x.text + 0.31, y = y.text, size = 6, label = "sim. random", color = cbPalette[3],
                 angle = 90) +        
        theme_classic() +
        theme(axis.text=element_text(size=20, colour="gray25"),
              axis.title=element_text(size=20, colour="gray25"),
              axis.line = element_blank(),              
              axis.ticks.length=unit(-0.25, "cm"),
              axis.ticks = element_line(colour = 'gray25'),
              panel.spacing.x=unit(.8, "lines"),
              legend.position="none",
              strip.background = element_rect(size = 1.3),
              axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm"),
                                         angle = 0, vjust = 0, hjust=0.5),
              axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))  +
        ylab(ylab) +
        xlab(xlab) + 
        base_breaks_y(custom.base.breaks.y) +
        base_breaks_x_discrete(custom.base.breaks.x, my.xend, scaled.scale) +
        scale_fill_manual(values=cbPalette)
    
    out.name <- file.path(report.dir, out.name)
    print(out.name)
    ggsave(out.name, width=7, height=4)    
}


#' Create a box-plot for the scalability experiments (absolute error
#' over time and blockchain size over time)
plot.x.by.y.special <- function(df, x, y, xlab, ylab, out.name, report.dir,
                        plot.expected.error=FALSE, start.x.at=0,
                        plot.smooth=FALSE, extreme.outlier.count=NA,
                        count.outliers=F,
                        custom.base.breaks.x=c(0, 1, 2, 3, 4),
                        custom.base.breaks.y=seq(0, 25, 5),
                        my.xend=10000,
                        my.y.label="5 robots",
                        x.text.1 = 5,
                        x.text.2 = 10,
                        y.text.1=5,
                        y.text.2=10) {

    df[,x] <- as.factor(df[,x])
    df$num.robots <- as.factor(df$num.robots)

    p <- ggplot(df, aes_string(x=x,y=y)) +
        geom_boxplot(aes(fill=num.robots),
                     position="identity"
                     ) +
        annotate(geom = "text", hjust=1, vjust=0, x = x.text.1, y = y.text.1, size = 6, label = "10 robots") +
        annotate(geom = "text", hjust=1, vjust=0, x = x.text.2, y = y.text.2, size = 6, label = my.y.label) +         

        theme_classic() +
         theme(axis.text=element_text(size=20, colour="gray25"),
              axis.title=element_text(size=20, colour="gray25"),
              axis.line = element_blank(),              
              axis.ticks.length=unit(-0.25, "cm"),
              axis.ticks = element_line(colour = 'gray25'),
              panel.spacing.x=unit(.8, "lines"),
              legend.position="none",
              strip.background = element_rect(size = 1.3),
              axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm"),
                                         angle = 90, vjust = 0.4, hjust=0.5),
              axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))  +
             ylab(ylab) +
        xlab(xlab) +
        base_breaks_y(custom.base.breaks.y) +
        base_breaks_x_discrete(custom.base.breaks.x, my.xend)


    out.name <- file.path(report.dir, out.name)
    print(out.name)
    ggsave(out.name, width=7, height=4)    
}


#' Perform a linear regression on the blockchain size per time and
#' create a line plot
plot.bc.size <- function(df, x, y, xlab, ylab, out.name, report.dir,
                         start.x.at=0,
                         stop.x.at=Inf,
                         custom.base.breaks.x=c(0, 1, 2, 3, 4),
                         custom.base.breaks.y=seq(0, 25, 5)) {

    df <- df[df$X1 <= stop.x.at, ]
    df$num.robots <- as.factor(df$num.robots)

    p <- ggplot(df, aes_string(x=x, y=y, group="num.robots")) +
    scale_linetype_manual(values=c("dotdash", "dashed", "dotted", "longdash", "twodash", "solid"))+
    geom_smooth(aes_string(linetype="num.robots"),
                method = lm,
                fullrange=TRUE,
                level=0.99,
                se=FALSE,
                color="black") +
        theme_classic() +
        geom_dl(aes_string(label="num.robots"),method="last.points", cex=6) +
         theme(axis.text=element_text(size=20, colour="gray25"),
               axis.title=element_text(size=20, colour="gray25"),
               axis.line = element_blank(),
               axis.ticks.length=unit(-0.25, "cm"),
               axis.ticks = element_line(colour = 'gray25'),
               panel.spacing.x=unit(.8, "lines"),
              legend.position="none",
              strip.background = element_rect(size = 1.3),
              axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm"),
                                         angle = 0, vjust = 0, hjust=0.5),
              axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))  +
    ylab(ylab) +
    xlab(xlab) + 
    base_breaks_y(custom.base.breaks.y) +
    base_breaks_x(custom.base.breaks.x)

    out.name <- file.path(report.dir, out.name)
    print(out.name)
    ggsave(out.name, width=7, height=4)
}


# Create a side-by-side box-plot
plot.sidebyside.x.by.y <- function(df, x, y, xlab, ylab, out.name, report.dir,
                        plot.expected.error=FALSE, start.x.at=0,
                        plot.smooth=FALSE, extreme.outlier.count=NA,
                        count.outliers=F,
                        custom.base.breaks.x=c(0, 1, 2, 3, 4),
                        custom.base.breaks.y=seq(0, 25, 5)) {

    p <- ggplot(df, aes_string(x=x, y=y)) +
        geom_boxplot() +
        theme_classic() +
        ylab(ylab) +
        xlab(xlab) +
        base_breaks_y(custom.base.breaks.y) +
        base_breaks_x(custom.base.breaks.x)

    out.name <- file.path(report.dir, out.name)
    print(out.name)
    ggsave(out.name, width=7, height=4)
}



# Create two box-plots per setting side-by-side to compare reality and simulation
plot.x.by.y.single <- function(df, x, y, xlab, ylab, out.name, report.dir,
                               plot.expected.error=FALSE, start.x.at=0,
                               plot.smooth=FALSE, extreme.outlier.count=NA,
                               count.outliers=F,
                               custom.base.breaks.x=c(0, 1, 2, 3, 4),
                               custom.base.breaks.y=seq(0, 25, 5),
                               x.text = 1,
                               y.text = 12,
                               my.width=0.75,
                               my.xend=5,
                               my.y.label="simulation",
                               my.x.shift=0.25,
                               my.x.shift.2=0.12,
                               scaled.scale=F
                               ) {

    cbPalette <- c("#0072B2")
    df[,x] <- as.factor(df[,x])
    df$num.robots <- as.factor(df$num.robots)
    
    p <- ggplot(df, aes_string(x=x,y=y)) +
        geom_boxplot(width=my.width, aes_string(fill="approach")) +
#        annotate(geom = "text", hjust=0, vjust=0, x = x.text + my.x.shift, y = y.text, size = 6, label = my.y.label, color = cbPalette[1],
#                 angle = 90) +         
        theme_classic() +
        theme(axis.text=element_text(size=20, colour="gray25"),
              axis.title=element_text(size=20, colour="gray25"),
              axis.line = element_blank(),              
              axis.ticks.length=unit(-0.25, "cm"),
              axis.ticks = element_line(colour = 'gray25'),
              panel.spacing.x=unit(.8, "lines"),
              legend.position="none",
              strip.background = element_rect(size = 1.3),
              axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm"),
                                         angle = 0, vjust = 0, hjust=0.5),
              axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))  +
        ylab(ylab) +
        xlab(xlab) + 
        base_breaks_y(custom.base.breaks.y) +
        base_breaks_x_discrete(custom.base.breaks.x, my.xend, scaled.scale) +
        scale_fill_manual(values=cbPalette)
    
    out.name <- file.path(report.dir, out.name)
    print(out.name)
    ggsave(out.name, width=7, height=4)    
}
