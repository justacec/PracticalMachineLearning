
# Setup the Environment ---------------------------------------------------

library(ggplot2)
library(caret)
library(grid)
library(gtable)

rm(list = ls())

source('rbind_max.R')

# Load the Training and the Test Datasets ---------------------------------

PML_Complete = read.csv('Data/WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv', header = TRUE)
PML_Training = read.csv('Data/pml-training.csv', header = TRUE)
PML_Testing = read.csv('Data/pml-testing.csv', header = TRUE)


# Fix the time variable ---------------------------------------------------

time = PML_Training$raw_timestamp_part_1 + (PML_Training$raw_timestamp_part_2 * 1e-6)
time = as.POSIXct(time, origin='1970-01-01', tz="UTC")
PML_Training$time = time
rm(time)

time = PML_Testing$raw_timestamp_part_1 + (PML_Testing$raw_timestamp_part_2 * 1e-6)
time = as.POSIXct(time, origin='1970-01-01', tz="UTC")
PML_Testing$time = time
rm(time)

time = PML_Complete$raw_timestamp_part_1 + (PML_Complete$raw_timestamp_part_2 * 1e-6)
time = as.POSIXct(time, origin='1970-01-01', tz="UTC")
PML_Complete$time = time
rm(time)

# Split out the window rows and the non-window rows -----------------------

# Each window is 1 second in length and the start of a new window contains all the averaged values 

PML_Training_Window = subset(PML_Training, new_window == 'yes')
PML_Training_NotWindow = subset(PML_Training, new_window == 'no')

# Get a list of the variable names ----------------------------------------

DatasetNames = names(PML_Training)


# Generate a plot of some of the summary statistics -----------------------

plots = list()

# Sensor can be one of the following: arm, belt, dumbbell, forearm
sensor = 'dumbbell'

# User can be one of the following: "adelmo"   "carlitos" "charles"  "eurico"   "jeremy"   "pedro"
user = 'charles'
user = 'eurico'
measures = sprintf('%s_%s', c('roll', 'pitch', 'yaw'), sensor)
measureLabels = c('Roll', 'Pitch', 'Yaw')


ds = subset(PML_Complete, user_name == user)
minT = min(ds$time)
maxT = max(ds$time)
limits = c(minT, maxT)
limits_m = mean(limits)
limits_w = diff(limits)
limits = limits_m + ((c(-1, 1)*0.5)*(limits_w/0.9))

useTrainPlot = ggplot(subset(PML_Training, user_name == user)) + theme_bw() + 
  geom_point(aes(x = time, y = 1, color = classe)) + 
  labs(x = NULL, y = 'Training') +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = limits) +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  theme(panel.margin = unit(c(0,0,0,0), units = 'cm'), plot.margin = unit(c(0,0,0,0), units = 'cm')) +
  theme(panel.border = element_blank())

useTestPlot = ggplot(subset(PML_Testing, user_name == user)) + theme_bw() + 
  geom_point(aes(x = time, y = 1)) + 
  labs(x = NULL, y = 'Testing') +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = limits) +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  theme(panel.margin = unit(c(0,0,0,0), units = 'cm'), plot.margin = unit(c(0,0,0,0), units = 'cm')) +
  theme(panel.border = element_blank())
 
plots[[length(plots) + 1]] = useTrainPlot
plots[[length(plots) + 1]] = useTestPlot

for(i in 1:length(measures)) {
  rp = ggplot(ds) + theme_bw() +
    geom_point(aes_string(x = 'time', y = measures[i], color = 'classe')) +
    coord_cartesian(xlim = limits) +
    theme(plot.margin = unit(c(0,0,0,0), units = 'mm'), panel.margin = unit(c(0,0,0,0), units = 'mm')) +
    scale_color_discrete(name = "Class")
  
  if(i == length(measures)) {  
    rp = rp + labs(x = 'Time (UTC)', y = measureLabels[i]) + theme(legend.position = 'right', legend.key = element_blank())
  } else {
    rp = rp + labs(x = NULL, y = measureLabels[i]) + theme(legend.position = 'none') + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
  }
  
  plots[[i+2]] = rp
}

getLegend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Extract the legend
leg = getLegend(plots[[length(plots)]])

# Remove the legend from the last plot
plots[[length(plots)]] = plots[[length(plots)]] + theme(legend.position = 'none')

gp <- do.call(rbind_max, plots)
gp <- gtable_add_cols(gp, widths = sum(leg$widths))
panels <- gp$layout$t[grep("panel", gp$layout$name)]
# set the relative panel heights 1/3 for the top two
gp$heights[panels] <- lapply(c(1,1,3,3,3), unit, "null")
# set the legend justification to top (it's a gtable embedded in a gtable)
leg[["grobs"]][[1]][["vp"]] <- viewport(just = c(0.5,1))
gp <- gtable_add_grob(gp, leg, t = 1, l = ncol(gp))

grid.draw(gp)


# Generate a series of diagnostic plots to explore the data ---------------

featurePlot(x = PML_Training_Window[,!(names(PML_Training_Window) %in% c('classe'))], y = PML_Training_Window$classe)



PML_Training_Window[,!(names(PML_Training_Window) %in% c('classe'))]
PML_Training_Window[,1:10]


