### renaming data and performing exploratory analysis###
df<-Data_File # chosen data assigned to variable "df"
dim(df) # shows dimensions of data set
summary(df) # displays summary statistics of data set

### Exploring Distribution of variables with histograms###
hist(Population) # shows population distribution
hist(`Pop. Density per sq. mi.`) # shows distribution of population density variable
hist(`GDP dollars  per capita`) # shows distribution of GDP per capita variable
hist(`Infant mortality per 1000 births`) # shows distribution of Infant mortality variable

### Non Parametric Density Curves###
plot(density(Population))
plot(density(`Pop. Density per sq. mi.`))
plot(density(`GDP dollars  per capita`))
plot(density(`Infant mortality per 1000 births`))
### Nonparametric Density Curves created for each variable to demonstrate skewness###

### scatter plot comparing 
pairs(df[,5:6]) # generates scatter plots comparing infant mortality rate and GDP per capita
pairs(df[,3:6])

### loading analyses packages ###
library(ggplot2) 
library(plyr)  
library(tseries) 
library(quantmod)
library(tidyverse)
library(xts)
library(forcats)
library(stringr)
library(rvest)
library(lubridate)
library(plotly)
library(PerformanceAnalytics)
### Useful packages downloaded###

### Plot created with points colored by region###
color<-factor(Region)
colorful_plot<-plot(`Infant mortality per 1000 births`, `GDP dollars  per capita`, col=color)+title('Infant Mortality and GDP per Capita by Region')+theme(plot.title = element_text(hjust = .5))# Plots Infant mortality and GDP per capita while showing the categorical variable by color
colorful_plot_2<-colorful_plot+legend("right", legend=c('Asia','Eastern Europe','Northern Africa','Oceania','Western Europe', 'Sub-Saharan Africa', 'Latin America and Caribbean','C.W. of Ind. States', 'Near East','Asia','Northern America','Baltics'), 
                                  col=color , cex = 0.8)

### plots a smooth line relating Infant mortality and GDP per capita ###
ggplot(df, aes(x = `Infant mortality per 1000 births`, y = `GDP dollars  per capita` ))+
  geom_smooth() # creates a smooth line plot with Infant Mortality rate on the x-axis and GDP per capita on the y-axis

### Creates linear model relating ###
MODEL1<-lm(`Infant mortality per 1000 births` ~ `GDP dollars  per capita` , df) # Builds linear model relating GDP per capita and Infant mortality rate
MODEl2<-lm(`GDP dollars  per capita` ~ `Infant mortality per 1000 births` , df)
summary(MODEL1) # reports summary output of the model object
MODEL1$coefficients # returns Beta estimates of GDP per capita and Infant mortality rate
cor(`Infant mortality per 1000 births`, `GDP dollars  per capita`) # gets correlation between infant mortality and GDP per capita

### Creates charts which display histograms along with nonparametric denscty curves, scatter with trendline, and correlation of variables###
plots<-cbind(df[,3:6]) # Binds variables: Population, Population Density, GDP Per Capita, and Infant mortality; assigned to "plots"
chart.Correlation(plots) # chart created showing correlation, r value, and a trendline between listed variables
plots_2<-cbind(df[,5:6]) # Binds Infant Mortality and GDP per capita varaibles and assigns them to "plots_2" variable
chart.Correlation(plots_2) # Chart created showing relationship between all numeric variables 


