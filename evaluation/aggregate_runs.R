#' Read data for Swarm intelligence 2020 journal article and create
#' plots for the Experiments section of the paper

source("myplothelpers.R")
library(stringr)
library(tidyr)
library(dplyr)
options(scipen=999)
require(reshape2)

## General Settings ------------------------------------------------------
actual.frequency <- 0.32 ## Actual frequency of white tiles
multiplier <- 10000000
tau <- 0.01

base.dir <- file.path("..")
data.dir <- file.path("../data")
plot.dir <- file.path("../plots")
real.runs.folder <- "../data/real_robot_runs/"


#' Conduct Mann-Whitney U tests for the comparison between reality and
#' simulation and print the results (espcially the p-value)
compare.sim.and.real <- function(df, dependent.variable, independent.variable , value.list) {

    for (i in value.list) {
        mask <- df[, independent.variable] == i
        df.tmp = df[mask, ]
        print(wilcox.test(df.tmp[, dependent.variable]~df.tmp$approach))        
        }            
    }


#' This function removes runs where something went wrong (e.g., when
#' another instance of the experiment was still running in the
#' background). However, use this function with care as it might
#' remove valid runs!
clean.df <- function(dirty.df) {

    # Drop empty results from postprocessing
    #clean.df <- na.omit(dirty.df)

    clean.df <- dirty.df
    
    # Drop runs where no consensus has been reached
    clean.df <- clean.df[clean.df["X1"] != 2500, ]
    
    # Check if num.robots and registered robots is the same value
    clean.df <- clean.df[clean.df["num.robots"] == clean.df["X6"], ]    
    
    return(clean.df)
}

#' Take two dataframes, selected the necessary columns and return a
#' long combined dataframe
combine.sim.real <- function(df.sim, df.real) {

    ## Select the necessary columns from both dataframes
    df.sim <- df.sim[,c("num.robots", "byz", "selected.absError", "X1")]
    colnames(df.sim) <- c("num.robots", "byz", "selected.absError", "final.time")
    df.sim$approach <- "simulation"
    
    df.real <- df.real[, c("num.robots", "byz", "selected.absError", "final.time")]
    df.real$approach <- "reality"
    df.combined <- rbind(df.sim, df.real)
    
    return(df.combined)
    }

#' Read the size of one blockchain run from the .RUN1 file
add.size <- function(size.path) {

    if (length(readLines(size.path)) > 1) {
        df <- read.table(size.path, sep="\t",
                         header=F,
                         skip=1)
        final.size  <- df[2,1] / 1000
        initial.size  <- 150 / 1000 # size after the initialization of the bc
        return(c(final.size, initial.size))        
    } else {
        return(c(NA,NA))
    }        
}

#' Read the size per X seconds of one blockchain run from the .RUN1 file
add.sizes <- function(size.path) {

    if (length(readLines(size.path)) > 1) {
        df <- read.table(size.path, sep="\t",
                         header=F,
                         skip=1,
                         fileEncoding="UTF-8-BOM")
        log.every.X.seconds <- 900

        df <- df[seq(2, nrow(df), 2), ]
        
        df$time <- seq(log.every.X.seconds, log.every.X.seconds * nrow(df), by= log.every.X.seconds)
        df <- df[,c(1,3)]
        colnames(df) <- c("size.MB", "clock")
        df$size.MB <-df$size.MB / 1000
        df$num.robots <- str_match(size.path, ".+num(\\d+)_black68_byz(\\d)_size(\\d).0_run(\\d+).RUN1")[2]
        return(df)        
    } else {
        return(c(NA,NA))
    }        
}

#' Read the balance per X seconds of one blockchain run from the .RUN1 file
add.balances <- function(full.path, size.path, max.line=20) {

    if (length(readLines(full.path)) > 1) {
        df <- read.table(full.path, sep="\t",
                         header=F,
                         skip=1,
                         fileEncoding="UTF-8-BOM")
        df <- head(df, max.line)

        df <- df[,c(1,7)]
        colnames(df) <- c("clock", "all.balances")
        print(str_match(size.path, ".+num(\\d+)_black68_byz(\\d)_size(\\d).0_run(\\d+).RUN1")[2])
        df$num.robots <- str_match(size.path, ".+num(\\d+)_black68_byz(\\d)_size(\\d).0_run(\\d+).RUN1")[2]

        return(df)        
    } else {
        return(c(NA,NA))
    }        
}


#' Read the estimate per X seconds of one blockchain run from the -blockchain.RUN1 file
add.estimates <- function(run.path) {

    if (length(readLines(run.path)) > 1) {
        df <- read.table(run.path, sep="\t",
                         header=F,
                         skip=1)

        return(df)        
    } else {
        return(c(NA,NA))
    }        
}


#' Read the results of one blockchain run from the *-blockchain.RUN1 file
postprocess <- function(run.path, tau, what.line=-1) {

    if (length(readLines(run.path)) > 1) {
        df <- read.table(run.path, sep="\t",
                         header=F,
                         skip=1)

        if (what.line == -1) {
            l = nrow(df) # Get the last row
        } else {
            l = what.line
        }

        return(c(df[l,1],
                 df[l,2],
                 df[l,3],
                 df[l,4],
                 df[l,5],
                 df[l,6],
                 df[l,7]))
        
    } else {
        return(c(NA, NA, NA, NA, NA, NA, NA))
    }    
}


#' Create a dataframe of all runs and calculate the absolute error
create.df <- function(experiment.name, tau, return.what="df") {
    experiments <- list.dirs(data.dir, recursive=FALSE,
                             full.names = FALSE)

    experiment.mask <- startsWith(experiments, experiment.name)
    experiments <- experiments[experiment.mask]
    experiments.with.path <- list.dirs(data.dir,
                                       recursive=FALSE)
    experiments.with.path <- experiments.with.path[experiment.mask]
    
    ## Extract information about that run using a regex:
    ## - Number of robots
    ## - Number of Byzantines 
    ## - Repetition number

    all.blockchain.files <- list.files(
        file.path(experiments.with.path,
                  toString(tau * 100000000)),
        full.names = TRUE,
        pattern="*blockchain.RUN1")

    all.size.files <- list.files(
        file.path(experiments.with.path,
                  toString(tau * 100000000)),
        full.names = TRUE,
        pattern="*run(\\d+).RUN1")

    all.experiments.matrix.blockchain <- str_match(all.blockchain.files,
                                        ".+num(\\d+)_black68_byz(\\d)_size(\\d).0_run(\\d+)-blockchain.RUN1")

    ## Create a data frame with all information from all runs
    df <- as.data.frame(all.experiments.matrix.blockchain)
    df <- cbind(df, as.data.frame(all.size.files))

    
    colnames(df) <- c("full.path", "num.robots", "byz", "arena.size", "run", "size.path")
    df$full.path <- as.character(df$full.path)
    df$num.robots <- as.numeric(as.character(df$num.robots))
    df$byz <- as.numeric(as.character(df$byz))
    df$arena.size <- as.numeric(as.character(df$arena.size))
    df$size.path <- as.character(df$size.path)

    df <- na.omit(df) # Remove invalid runs

    ## Needed are:
    ## - the final mean
    ## - consensus time

    
    postprocessed <- lapply(df$full.path, postprocess, what.line=-1)
    postprocessed.df <- data.frame(matrix(unlist(postprocessed),
                                          nrow=length(postprocessed),
                                          byrow=T))
    

    df.complete <- cbind(df, postprocessed.df)
    df.complete$selected.error <- (df.complete$X2 - (actual.frequency * multiplier)) / (multiplier / 100)
    df.complete$selected.absError <- abs(df.complete$selected.error)

    
    
    ## Return size df or normal df?

    if (return.what == "size.df") {

        all.sizes.with.times <- lapply(df$size.path, add.sizes)
        size.df <- Reduce(function(...) merge(..., all=T), all.sizes.with.times)
        colnames(size.df) <- c("size.MB", "clock", "num.robots")
        
        return(size.df)
        
    } else if (return.what == "estimates.df") {
        
        all.estimates.with.times <- lapply(df$full.path, add.estimates)
        estimates.df <- Reduce(function(...) merge(..., all=T), all.estimates.with.times)
        
        colnames(estimates.df) <- c("clock", "estimate", "votes.1", "votes.2", "votes.3", "num.robots", "balance")
        return(estimates.df)
        
    } else if (return.what == "balances.df") {
        all.balances <- lapply(df$full.path, add.balances, df$size.path, max.line=200)
        balances.df <- Reduce(function(...) merge(..., all=T), all.balances)
        colnames(balances.df) <- c("clock", "balance", "num.robots")
        balances.df$balance <- as.character(balances.df$balance)
        
        return(balances.df)
        
    } else if (return.what == "simple.df") {
        return(postprocessed.df)
    } else {
        return(df.complete)
    }
}

## ------------------ Experiments -------------------------

experiment.names <- c("1_Increasing_Byz",
                      "2_Increasing_Swarm_Size_No_Byz",
                      "3_Increasing_Swarm_Size_Byz",
                      "4_Long_Runtime_No_Byz",
                      "5_Increasing_Arena_Size_No_Byz",
                      "6_Increasing_Arena_Size_Constant_Density_No_Byz",
                      "7_Long_Runtime_Byz",
                      "8_Increasing_Arena_Size_Constant_Tiles_Byz",
                      "9_Increasing_Arena_Size_Increasing_Tiles_Byz", # Still to implement cel dim
                      "10_Increasing_Arena_Size_Increasing_Tiles_Constant_Density_Byz",
                      "11_Increasing_Byz_Random_Tiles",
                      "12_Increasing_Byz_50_50",
                      "13_Increasing_Byz_Random_Estimate")


source("myplothelpers.R")
df.1.sim <- create.df(experiment.names[1], tau)
df.1.real <-readRDS(file=file.path(real.runs.folder, "df_1_real.Rda"))
df.1.combined <- combine.sim.real(df.1.sim, df.1.real)
write.csv(df.1.combined, paste0(experiment.names[1], ".csv"), quote=F, row.names=F)


# Plot selected absolute error
plot.x.by.y.side.by.side(df.1.combined,
                         x="byz",
                         y="selected.absError",
                         xlab=expression("Number of Byzantine robots"),
                         ylab=expression("Absolute error (in %)"),
                         out.name=sprintf("exp1_error_combined.pdf"),
                         report.dir=plot.dir,
                         custom.base.breaks.x=c(0,1,2,3,4,5),
                         custom.base.breaks.y=seq(0, 32, by=4),
                         my.xend=6,
                         y.text = 7)

# Plot consensus time
plot.x.by.y.side.by.side(df.1.combined,
                         x="byz",
                         y="final.time",
                         xlab=expression("Number of Byzantine robots"),
                         ylab=expression("Consensus time (s)"),
                         out.name=sprintf("exp1_time_combined.pdf"),
                         report.dir=plot.dir,
                         custom.base.breaks.x=c(0,1,2,3,4,5),
                         custom.base.breaks.y=c(0,250,500,750,1000),
                         y.text=500,
                         my.xend=6)


# ------------------ Experiment 2

source("myplothelpers.R")
df.2.sim <- create.df(experiment.names[2], tau)
df.2.real <-readRDS(file=file.path(real.runs.folder, "df_2_real.Rda"))
df.2.combined <- combine.sim.real(df.2.sim, df.2.real)
compare.sim.and.real(df.2.combined, "selected.absError", "num.robots" , c(5,6,7,8,9,10))
write.csv(df.2.combined, paste0(experiment.names[2], ".csv"), quote=F, row.names=F)

# Plot selected absolute error
plot.x.by.y.side.by.side(df.2.combined,
                         x="num.robots",
                         y="selected.absError",
                         xlab=expression("Number of robots"),
                         ylab=expression("Absolute error (in %)"),
                         out.name=sprintf("exp2_error_combined.pdf"),
                         report.dir=plot.dir,
                         custom.base.breaks.x=c(5,6,7,8,9,10),
                         y.text = 10.5,
                         my.xend=6)

# Plot consensus time
plot.x.by.y.side.by.side(df.2.combined,
                         x="num.robots",
                         y="final.time",
                         xlab=expression("Number of robots"),
                         ylab=expression("Consensus time (s)"),
                         out.name=sprintf("exp2_time_combined.pdf"),
                         report.dir=plot.dir,
                         custom.base.breaks.x=c(5,6,7,8,9,10),
                         custom.base.breaks.y=c(0,250,500,750,1000),
                         y.text=720,
                         my.xend=6,
                         my.y.label="sim.")


## ------------------- Experiment 3
## Plot consensus time

source("myplothelpers.R")
df.3.sim <- create.df(experiment.names[3], tau)
df.3.real <-readRDS(file=file.path(real.runs.folder, "df_3_real.Rda"))
df.3.combined <- combine.sim.real(df.3.sim, df.3.real)
write.csv(df.3.combined, paste0(experiment.names[3], ".csv"), quote=F, row.names=F)

plot.x.by.y.side.by.side(df.3.combined,
                         x="num.robots",
                         y="final.time",
                         xlab=expression("Number of robots"),
                         ylab=expression("Consensus time (s)"),
                         out.name=sprintf("exp3_time_combined.pdf"),
                         report.dir=plot.dir,
                         custom.base.breaks.x=c(5,10),
                         custom.base.breaks.y=c(0,250, 500, 750, 1000),
                         y.text=-50,
                         my.width=0.4,
                         my.xend=2,
                         my.x.shift=0.12,
                         my.x.shift.2=0.08,
                         my.y.label="simulation")


plot.x.by.y.side.by.side(df.3.combined,
                         x="num.robots",
                         y="selected.absError",
                         xlab=expression("Number of robots"),
                         ylab=expression("Absolute error"),
                         out.name=sprintf("exp3_error_combined.pdf"),
                         report.dir=plot.dir,
                         custom.base.breaks.x=c(5,10),
                         custom.base.breaks.y=seq(0, 32, by=4),
                         my.width=0.4,
                         y.text=12.5,
                         my.x.shift.2=0.08,
                         my.xend=2,
                         my.x.shift=0.12)

## ------------------- Experiment 4 (Increasing arena size)


### Create a data frame with the estimates over time
df.4.estimates <- create.df(experiment.names[4], tau, return.what="estimates.df")
df.4.estimates <- df.4.estimates[df.4.estimates$clock <= 10000, ]
df.4.estimates$selected.absError <- 100 * (abs((df.4.estimates$estimate / multiplier) - actual.frequency))
df.4.estimates$approach <- "simulation"
write.csv(df.4.estimates, "4_Increasting_Size_No_Byz_long_estimates.csv", quote=F, row.names=F)


### Create a data frame with the blockchain size over time
df.4.size <- create.df(experiment.names[4], tau, return.what="size.df")
df.4.size <- df.4.size[df.4.size$size.MB != 0, ]
df.4.size <- df.4.size[df.4.size$clock <= 10000, ]
write.csv(df.4.size, "4_Increasting_Size_No_Byz_long_size.csv", quote=F, row.names=F)

# Plot selected absolute error
plot.x.by.y.special(df.4.estimates,
            x="clock",
            y="selected.absError",
            xlab=expression("Time (s)"),
            ylab=expression("Absolute error (in %)"),
            out.name=sprintf("exp4_error_over_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=seq(900, 9900, by=900),
            custom.base.breaks.y=seq(0, 10, by=2),
            my.xend=11,
            x.text.1=11.3,
            x.text.2=11.3,
            y.text.1=2,
            y.text.2=5.7)

# Plot blockchain size
plot.x.by.y.special(df.4.size,
            x="clock",
            y="size.MB",
            xlab=expression("Time (s)"),
            ylab=expression("Blockchain size (MB)"),
            out.name=sprintf("exp4_size_over_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=seq(900, 9900, by=900),
            custom.base.breaks.y=seq(0, 10, by=1.0),
            my.xend=11,
            x.text.1=11.3,
            x.text.2=11.3,
            y.text.1=8.7,
            y.text.2=2)



## ------------------- Experiment 5 (Increasing arena size)
## Plot consensus time


# Remember to use width=2.0 in geom_boxplot
source("myplothelpers.R")
df.5 <- create.df(experiment.names[5], tau)
df.5$area <- df.5$arena.size * df.5$arena.size
df.5$approach <- "simulation"
write.csv(df.5, paste0(experiment.names[5], ".csv"), quote=F, row.names=F)

# Plot selected absolute error
plot.x.by.y(df.5,
            x="area",
            y="selected.absError",
            xlab=bquote(Arena~size~(m^{2})),            
            ylab=expression("Absolute error (in %)"),
            out.name=sprintf("exp5_error.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=c(1,4,9,16,25),
            custom.base.breaks.y=seq(0, 32, 4),
            my.xend=25,
            scaled.scale=T)

# Plot consensus time
plot.x.by.y(df.5,
            x="area",
            y="X1",
            xlab=bquote(Arena~size~(m^{2})),
            ylab=expression("Consensus time (s)"),
            out.name=sprintf("exp5_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=c(1,4,9,16,25),
            custom.base.breaks.y=seq(0, 4000, 400),
            my.xend=25,
            scaled.scale=T)


## ------------------- Experiment 6 (Increasing arena size and swarm
## size proportionally)

source("myplothelpers.R")
df.6 <- create.df(experiment.names[6], tau)
df.6$area <- df.6$arena.size * df.6$arena.size
df.6$approach <- "simulation"
write.csv(df.6, paste0(experiment.names[6], ".csv"), quote=F, row.names=F)


### Create a data frame with the blockchain size over time
df.6.size <- create.df(experiment.6.name, tau, return.what="size.df")
df.6.size <- df.6.size[df.6.size$size.MB != 0, ]
df.6.size <- df.6.size[df.6.size$clock <= 10000, ]
write.csv(df.6.size, "6_Increasing_Dimension_No_Byz_Constant_Density_size.csv", quote=F, row.names=F)

# Plot blockchain size
plot.x.by.y.special(df.6.size,
            x="clock",
            y="size.MB",
            xlab=expression("Time (s)"),
            ylab=expression("Blockchain size (MB)"),
            out.name=sprintf("exp6_size_over_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=seq(0, 1000, by=100),
            custom.base.breaks.y=seq(0, 5, by=0.1),
            my.xend=11,
            x.text.1=11.3,
            x.text.2=11.3,
            y.text.1=8.7,
            y.text.2=2)


# Plot selected absolute error
plot.x.by.y(df.6,
            x="area",
            y="selected.absError",
            xlab=bquote(Arena~size~(m^{2})),
            ylab=expression("Absolute error (in %)"),
            out.name=sprintf("exp6_error.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=c(1,4,9),
            custom.base.breaks.y=seq(0, 32, 4),
            my.width=0.4,
            my.xend=9,
            scaled.scale=T)

# Plot consensus time
plot.x.by.y(df.6,
            x="area",
            y="X1",            
            xlab=bquote(Arena~size~(m^{2})),
            ylab=expression("Consensus time (s)"),
            out.name=sprintf("exp6_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=c(1,4,9),
            custom.base.breaks.y=seq(0, 1000, 250),
            my.width=0.4,
            my.xend=9,
            scaled.scale=T)


## ------------------- Experiment 7 (Increasing size [5 and 10 robots]
## with a run-time of 10,000 seconds)

source("myplothelpers.R")
df.7 <- create.df(experiment.names[7], tau)
df.7$area <- df.7$arena.size * df.7$arena.size
df.7$approach <- "simulation"
write.csv(df.7, paste0(experiment.names[7], ".csv"), quote=F, row.names=F)


### Create a data frame with the balances of the robots over time
df.7.balances <- create.df(experiment.names[7], tau, return.what="balances.df")
df.7.balances$balance <- substr(df.7.balances$balance, 1, nchar(df.7.balances$balance)-1)
## Split balances column into multiple columns
df.7.balances <- separate(df.7.balances, col=balance, into=as.character(seq(10)))
df.7.balances <- df.7.balances %>% mutate_if(is.character,as.numeric)
## REMOVE AGAIN
df.7.balances$num.robots <- NULL
## Group into Byzantine and non-Byzantines
non.ByzantineCols <-c(4,5,6,7,8,9,10,11)
ByzantineCols <-c(2,3)
df.7.balances$non.Byzantines <- rowMeans(df.7.balances[, non.ByzantineCols]) /(10 ** 18)
df.7.balances$Byzantines <- rowMeans(df.7.balances[, ByzantineCols]) / (10 ** 18)
df.7.balances <- na.omit(df.7.balances)
boxplot(df.7.balances$Byzantines~df.7.balances$clock, ylim=c(0, 200))
#boxplot(df.7.balances$non.Byzantines~df.7.balances$clock, ylim=c(0, 200))
#lines(df.7.balances$clock, df.7.balances$non.Byzantines,col="green")

### Create a data frame with the blockchain size over time
df.7.size <- create.df(experiment.names[7], tau, return.what="size.df")
df.7.size <- df.7.size[df.7.size$size.MB != 0, ]
df.7.size <- df.7.size[df.7.size$clock <= 10000, ]
write.csv(df.7.size, "7_Increasing_Size_Byz_10000sec.csv", quote=F, row.names=F)

# Plot blockchain size
plot.x.by.y.special(df.7.size,
            x="clock",
            y="size.MB",
            xlab=expression("Time (s)"),
            ylab=expression("Blockchain size (MB)"),
            out.name=sprintf("exp7_size_over_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=seq(0, 1000, by=100),
            custom.base.breaks.y=seq(0, 5, by=0.1),
            my.xend=11,
            x.text.1=11.3,
            x.text.2=11.3,
            y.text.1=8.7,
            y.text.2=2)


## ------------------- Experiment 8 (Increasing arena size from 1.0 m
## ------------------- to 5.0 m)

source("myplothelpers.R")
df.8 <- create.df(experiment.names[8], tau)
df.8 <- df.8[df.8$num.robots == 10,]

df.8$area <- df.8$arena.size * df.8$arena.size
df.8$approach <- "simulation"
write.csv(df.8, paste0(experiment.names[8], ".csv"), quote=F, row.names=F)

# Plot selected absolute error
plot.x.by.y(df.8,
            x="area",
            y="selected.absError",
            xlab=bquote(Arena~size~(m^{2})),            
            ylab=expression("Absolute error (in %)"),
            out.name=sprintf("exp8_error.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=c(1,4,9,16,25),
            custom.base.breaks.y=seq(0, 32, 4),
            my.xend=25,
            scaled.scale=T)

# Plot consensus time
plot.x.by.y(df.8,
            x="area",
            y="X1",
            xlab=bquote(Arena~size~(m^{2})),
            ylab=expression("Consensus time (s)"),
            out.name=sprintf("exp8_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=c(1,4,9,16,25),
            custom.base.breaks.y=seq(0, 4000, 400),
            my.xend=25,
            scaled.scale=T)

## ------------------- Experiment 10 (Increasing arena size,
## increasing tile size, constant density)

source("myplothelpers.R")
df.10 <- create.df(experiment.names[10], tau)
df.10$area <- df.10$arena.size * df.10$arena.size
df.10$approach <- "simulation"
write.csv(df.10, paste0(experiment.names[10], ".csv"), quote=F, row.names=F)


### Create a data frame with the blockchain size over time
df.10.size <- create.df(experiment.10.name, tau, return.what="size.df")
df.10.size <- df.6.size[df.10.size$size.MB != 0, ]
df.10.size <- df.6.size[df.10.size$clock <= 10000, ]
write.csv(df.6.size, "6_Increasing_Dimension_No_Byz_Constant_Density_size.csv", quote=F, row.names=F)

# Plot blockchain size
plot.x.by.y.special(df.6.size,
            x="clock",
            y="size.MB",
            xlab=expression("Time (s)"),
            ylab=expression("Blockchain size (MB)"),
            out.name=sprintf("exp6_size_over_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=seq(0, 1000, by=100),
            custom.base.breaks.y=seq(0, 5, by=0.1),
            my.xend=11,
            x.text.1=11.3,
            x.text.2=11.3,
            y.text.1=8.7,
            y.text.2=2)


# Plot selected absolute error
plot.x.by.y(df.6,
            x="area",
            y="selected.absError",
            xlab=bquote(Arena~size~(m^{2})),
            ylab=expression("Absolute error (in %)"),
            out.name=sprintf("exp6_error.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=c(1,4,9),
            custom.base.breaks.y=seq(0, 32, 4),
            my.width=0.4,
            my.xend=9,
            scaled.scale=T)

# Plot consensus time
plot.x.by.y(df.6,
            x="area",
            y="X1",            
            xlab=bquote(Arena~size~(m^{2})),
            ylab=expression("Consensus time (s)"),
            out.name=sprintf("exp6_time.pdf"),
            report.dir=plot.dir,
            custom.base.breaks.x=c(1,4,9),
            custom.base.breaks.y=seq(0, 1000, 250),
            my.width=0.4,
            my.xend=9,
            scaled.scale=T)



## ------------------- Experiment 11 (Increasing Byzantines with
## ------------------- random tile distribution)

source("myplothelpers.R")
df.11 <- create.df(experiment.names[11], tau)
df.11 <- clean.df(df.11)

df.11 <- df.11[,c("num.robots", "byz", "selected.absError", "X1")]
colnames(df.11) <- c("num.robots", "byz", "selected.absError", "final.time")

#df.11$area <- df.11$arena.size * df.11$arena.size

df.11$approach <- "simulation_random"

write.csv(df.11, paste0(experiment.names[11], ".csv"), quote=F, row.names=F)



df.1.sim <- create.df(experiment.names[1], tau)
df.1.real <-readRDS(file=file.path(real.runs.folder, "df_1_real.Rda"))

df.1.combined <- combine.sim.real(df.1.sim, df.1.real)





df.11.combined <- rbind(df.1.combined, df.11)
df.11.combined <- df.11.combined[df.11.combined["final.time"] != 2500, ]


source("myplothelpers.R")
# Plot selected absolute error
plot.x.by.y.side.by.side.three(df.11.combined,
                               x="byz",
                               y="selected.absError",
                               xlab=expression("Number of Byzantine robots"),
                               ylab=expression("Absolute error (in %)"),
                               out.name=sprintf("exp11_error_combined.pdf"),
                               report.dir=plot.dir,
                               custom.base.breaks.x=c(0,1,2,3,4,5),
                               custom.base.breaks.y=seq(0, 32, by=4),
                               my.xend=6,
                               y.text = 7)

# Plot consensus time
plot.x.by.y.side.by.side.three(df.11.combined,
                               x="byz",
                               y="final.time",
                               xlab=expression("Number of Byzantine robots"),
                               ylab=expression("Consensus time (s)"),
                               out.name=sprintf("exp11_time_combined.pdf"),
                               report.dir=plot.dir,
                               custom.base.breaks.x=c(0,1,2,3,4,5),
                               custom.base.breaks.y=c(0,250,500,750,1000),
                               y.text=500,
                               my.xend=6)

## ------------------- Experiment 12 (Byzantine failures 50:50)


source("myplothelpers.R")
df.12 <- create.df(experiment.names[12], tau)
df.12$approach <- "simulation"
names(df.12)[names(df.12) == 'X1'] <- 'final.time'
df.12 <- df.12[df.12["final.time"] <= 1000, ]
#df.12 <- df.12[,c("num.robots", "byz", "selected.absError", "X1")]
#colnames(df.12) <- c("num.robots", "byz", "selected.absError", "final.time")
df.12$approach <- "simulation"

source("myplothelpers.R")

# Plot selected absolute error
plot.x.by.y.single(df.12,
                   x="byz",
                   y="selected.absError",
                   xlab=bquote("Number of Byzantine Robots"),            
                   ylab=expression("Absolute error (in %)"),
                   out.name=sprintf("exp12_error.pdf"),
                   report.dir=plot.dir,
                   custom.base.breaks.x=c(0,1,2,3,4,5),
                   custom.base.breaks.y=seq(0, 32, 4),
                   my.xend=6,
                   my.width=0.375,
                   scaled.scale=F)

# Plot consensus time
plot.x.by.y.single(df.12,
                   x="byz",
                   y="final.time",
                   xlab=bquote("Number of Byzantine Robots"),            
                   ylab=expression("Consensus time (s)"),
                   out.name=sprintf("exp12_time.pdf"),
                   report.dir=plot.dir,
                   custom.base.breaks.x=c(0,1,2,3,4,5),
                   custom.base.breaks.y=seq(0, 1000, 250),
                   my.xend=6,
                   my.width=0.375,
                   scaled.scale=F)


## ------------------- Experiment 13 (Byzantine failures - random uniform estimate)


source("myplothelpers.R")
df.13 <- create.df(experiment.names[13], tau)

df.13$approach <- "simulation"
names(df.13)[names(df.13) == 'X1'] <- 'final.time'
df.13 <- df.13[df.13["final.time"] <= 1000, ]
df.13$approach <- "simulation"

source("myplothelpers.R")

# Plot selected absolute error
plot.x.by.y.single(df.13,
                   x="byz",
                   y="selected.absError",
                   xlab=bquote("Number of Byzantine Robots"),            
                   ylab=expression("Absolute error (in %)"),
                   out.name=sprintf("exp13_error.pdf"),
                   report.dir=plot.dir,
                   custom.base.breaks.x=c(0,1,2,3,4,5),
                   custom.base.breaks.y=seq(0, 32, 4),
                   my.xend=6,
                   my.width=0.375,
                   scaled.scale=F)

# Plot consensus time
plot.x.by.y.single(df.13,
                   x="byz",
                   y="final.time",
                   xlab=bquote("Number of Byzantine Robots"),            
                   ylab=expression("Consensus time (s)"),
                   out.name=sprintf("exp13_time.pdf"),
                   report.dir=plot.dir,
                   custom.base.breaks.x=c(0,1,2,3,4,5),
                   custom.base.breaks.y=seq(0, 1000, 250),
                   my.xend=6,
                   my.width=0.375,
                   scaled.scale=F)
