# Install and Load Packages --------------------------------------------------------

install.packages('tidyverse'); install.packages('vegan')
#install.packages('MASS');install.packages("devtools"); devtools::install_github("gavinsimpson/ggvegan")
install.packages('ggplot2');install.packages('dplyr')


install.packages('scales')
install.packages('viridis'); 
install.packages('lmerTest'); install.packages('gridExtra');install.packages('ggplot2');library(lmerTest); install.packages('ggpubr')
install.packages('colorspace'); install.packages('ggpubr')


library('ggvegan'); library('tidyverse'); library('ggplot2')
library('scales')
library('viridis');  library('dplyr')
library('ggpubr')
library('gridExtra')


setwd("C:\\Users\\Julia Michaels\\Google Drive\\Dissertation Chapter 3")



# Load data ---------------------------------------------------------------
#calibration<-read.csv("2018 Inundation_Stopping 2018-03-19.csv")
data_loggers_2018<-read.csv('2018_Levelloggers.csv')
staff_gauge_2018<-read.csv('2017-2018 Staff Gauges All.csv')
precip_2018<-read.csv('precipitation data_2018.csv')