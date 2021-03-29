############################# Loading data ------------------------------------------------------------

## creating path to folder - easy to change later
folder_path <- "/Users/erivero/Documents/MBD\ IE/t1/R\ PROGRAMMING/group_assignment/project(1)/";
## assigning to variable
solar <- readRDS(file.path(folder_path,"solar_dataset.RData"));
stations <- as.data.table(read.csv(file.path(folder_path,"station_info.csv")));
extra_vars <- readRDS(file.path(folder_path,"additional_variables.RData"));
## checking object class
class(solar);
class(stations);
class(extra_vars);

############################# Installing packages -----------------------------------------------------

#here we install packages if not available
install.packages(RColorBrewer);
install.packages(DataExplorer);
install.packages(gtable);
install.packages(grid);
install.packages(egg);
install.packages(gridExtra);
install.packages(kableExtra);
install.packages(papeR);
install.packages(leaflet);
install.packages(scales);
install.packages(dlookr);
install.packages(RColorBrewer);
install.packages(RColorBrewer);
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

############################# Loading libraries -------------------------------------------------------

library(kableExtra)
library(data.table)
library(knitr)
library(papeR)
library(leaflet)
library(corrplot)
library(RColorBrewer)
library(DataExplorer)
library(ggplot2)
library(scales)
library(dlookr)
library(gtable)
library(grid)
library(egg)
library(gridExtra)
library(factoextra)
library(dplyr)
library(caret)
library(Amelia)
library(tseries)
library(forecast)
library(FactoMineR)
library(urca)

############################# Checking structure --------------------------------------------------------

## display tables
View(solar);
View(stations);
View(extra_vars);

# display variable's names
colnames(solar); # date, 98 station names and 357 Principal components (PC)

# dimensions
dim(solar); # dimensions
nrow(solar); # number of rows
ncol(solar); # number of columns

# variable's classes
sapply(solar, class); # data types (according to name's order): char, integer, numeric 

# a few values
head(solar)[,c(1,2,100)] # displaying the first column of each type

############################# Restructuring Data  -----------------------------------------------------------

## creating working dataset
solar_irradiation <- solar[1:5113,1:99]; # real irradiation from 98 stations
solar_stations <- stations; # making a copy to work with
solar_extra <- extra_vars; # making a copy to work with

## casting to date
solar_irradiation$Date <- as.Date.character(solar_irradiation$Date, format = c("%Y%m%d")); #casting variable to date format
solar_extra$Date <- as.Date.character(solar_extra$Date, format = c("%Y%m%d")); #casting variable to date format

## casting to numeric
vec_names_num <- colnames(solar_irradiation)[-1]; # excluding date column
solar_irradiation[,(vec_names_num) := lapply(.SD, as.numeric), .SDcols = vec_names_num] # casting to numeric indexing by name

## checking class and dimensions
class(solar_irradiation); # object type dt
class(solar_stations); # object type dt
class(solar_extra); # object type dt
sapply(solar_irradiation, class); # class of all columns
sapply(solar_stations, class); # class of all columns
sapply(solar_extra, class); # class of all columns
dim(solar_irradiation); # date and real measures
dim(solar_stations); # station, lat, long, elev
dim(solar_extra); #  100 NPW for all days

## display data.table objects
View(solar_irradiation);
View(solar_stations);
View(solar_extra);

# order by column name solar_extra
vec_order_solar_extra <- solar_extra[,order(colnames(solar_extra))] #getting vector of ordered columns

order_solar_extra <- solar_extra %>% select(sort(names(.))) # ordering NWP dt

############################# EDA ---------------------------------------------------------------------

## graphical idea of the dataset
plot_str(solar_irradiation[,1:50]) # not that useful for large datasets; using DataExplorer

## graphical idea of missing values
plot_missing(solar[,1:10]) # showing that for each Mesonet there are 26% values to predict; using DataExplorer
plot_missing(solar_irradiation) # there are no missing values here
plot_missing(solar_extra[,2:25]) # with missing values; using DataExplorer; showing in batches of 25

## using ggplot and grid.arrange function
p1 <- plot_missing(solar_extra[,2:25], group = list("less_5%" = 0.05, "between_5_10%" = 0.1, "above_10%" = 1))
p2 <- plot_missing(solar_extra[,26:50], group = list("less_5%" = 0.05, "between_5_10%" = 0.1, "above_10%" = 1))
p3 <- plot_missing(solar_extra[,51:75], group = list("less_5%" = 0.05, "between_5_10%" = 0.1, "above_10%" = 1))
p4 <- plot_missing(solar_extra[,76:101], group = list("less_5%" = 0.05, "between_5_10%" = 0.1, "above_10%" = 1))
grid.arrange(p1,p2,p3,p4, nrow = 2)

## graphical idea of the numeric variables
plot_histogram(solar_irradiation[,1:50]) # can handle many histograms with ease; Here we do not see marge variation but this is not the best way to observe it.

## summary statistics
summary(solar_irradiation); # attributes summary
summarise(solar_irradiation); # attributes summary using papeR
summarise(solar_extra); # attributes summary solar_extra using papeR

## correlations
solar_corr <- cor(solar_irradiation[,2:99]); # correlations of solar irradiation measures
print(solar_corr); # display correlation matrix

## correlation plot

col_color = brewer.pal(n=8, name="RdYlBu") # defining the color scheme
corrplot(solar_corr, method = "color", 
         type = "upper", 
         tl.col = "black", 
         col = col_color, 
         tl.cex = 0.3) # observations: all are positive; most strong (>= 0.75)

plot_correlation(solar_irradiation, type='continuous', 'Date') # Honestly, this is not better than the one above; using DataExplorer

### correlation plot complete between weather stations and PCs (v1)
a <- round(cor(solar[1:5113,2:99], mypr$x), 3)
corrplot(a, order="hclust", tl.col="black", tl.srt=45, tl.cex = 0.3, col=brewer.pal(n=8, name="RdYlBu"))

### correlation plot upper between weather stations and PCs (v2)
a <- round(cor(solar[1:5113,2:99], mypr$x), 3)
corrplot(a, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = 0.3, col=brewer.pal(n=8, name="RdYlBu"))

### correlation plot complete between weather stations and PCs (v3) [this one we used]
a <- round(cor(solar[1:5113,2:99], mypr$x), 3)
corrplot(a, type="upper", order="FPC", tl.col="black", tl.srt=45, tl.cex = 0.3, col=brewer.pal(n=8, name="RdYlBu"))

## scatter plot matrix
pairs(solar_irradiation[,2:6]) # showing only 5 //todo: more relevant when isolating not highly correlated

# summary solar_irradiation
summary_irradiation <- as.data.table(summarise(solar_irradiation[,-1])) # not including date column; using papeR
class(summary_irradiation) # checking for the class
View(summary_irradiation) # viewing

# summary of all dataset (summary of summary_irradiation)
summarize(summary_irradiation)

# calculating measures of central tendency
sample_mean <- summary_irradiation[,list(sample_mean = mean(summary_irradiation$Mean)),] # mean
sample_min <- summary_irradiation[,list(sample_min = min(summary_irradiation$Min)),] # min
sample_max <- summary_irradiation[,list(sample_max = max(summary_irradiation$Max)),] # max
sample_sd <- summary_irradiation[,list(sample_sd = mean(summary_irradiation$SD)),] # no sense

#calculating the ranges
sort(sapply(solar_irradiation, function(x){(max(x)-min(x))}), decreasing = TRUE) 
# found that IDAB has the highest range

summary(solar_irradiation$IDAB) # summary of IDAB
IQR(solar_irradiation$IDAB) # calculating IQR
var1 <- 22601100 + 1.5*IQR(solar_irradiation$IDAB)
var2 <- 9927900 - 1.5*IQR(solar_irradiation$IDAB)
var1 < var2

############################# Plots -------------------------------------------------------------------

# a first boxplot to get a sense of the distributions of solar irradiation
boxplot(solar_irradiation[,-1]) # boxplot of all 98 stations
abline(h = sample_mean, col = "red")
abline(h = sample_min, col = "green")
abline(h = sample_max, col = "blue")
# abline(h = sample_sd, col = "orange") # no sense

## seasonality v1
time_series_mesonet <- ggplot(solar_irradiation, aes(x = solar_irradiation$Date, y = solar_irradiation$ACME))  + geom_line() + xlab("Date") + ylab("Irradiation") + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "730 days")
time_series_mesonet;

## seasonality v2 (used)
time_series_mesonet <- ggplot(solar_irradiation, aes(x = solar_irradiation$Date, y = solar_irradiation$ACME))  + geom_line() 
time_series_mesonet <- time_series_mesonet + labs(title = "Seasonality - full time series", 
                                                  subtitle = "ACME mesonet", 
                                                  caption = "source: solar_irradiation (dt)",
                                                  x = "Date", 
                                                  y = "Irradiation") 
time_series_mesonet <- time_series_mesonet + theme(plot.title = element_text(color = "blue", size = "10", face = "bold", hjust = 0), 
                                                   plot.subtitle = element_text(color = "black"), 
                                                   plot.caption = element_text(color = "black", face = "italic"))  
time_series_mesonet <- time_series_mesonet + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "730 days")
time_series_mesonet;

## seasonality (2y view) v1
time_series_one_year_mesonet <- ggplot(solar_irradiation, aes(x = solar_irradiation$Date, y = solar_irradiation$ACME))  + geom_line()+ xlab("Date") + ylab("Irradiation") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+ scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month", limits = as.Date(c("2000-01-01", "2001-12-31")))
time_series_one_year_mesonet
  
## seasonality (2y view) v2 (used)
time_series_one_year_mesonet <- ggplot(solar_irradiation, aes(x = solar_irradiation$Date, y = solar_irradiation$ACME))  + geom_line() 
time_series_one_year_mesonet <- time_series_one_year_mesonet + labs(title = "Seasonality - full time series", 
                                                                    subtitle = "ACME mesonet", 
                                                                    caption = "source: solar_irradiation (dt)",
                                                                    x = "Date", 
                                                                    y = "Irradiation") 
time_series_one_year_mesonet <- time_series_one_year_mesonet + theme(plot.title = element_text(color = "blue", size = "10", face = "bold", hjust = 0), 
                                                                     plot.subtitle = element_text(color = "black"), 
                                                                     plot.caption = element_text(color = "black", face = "italic"),
                                                                     axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))  
time_series_one_year_mesonet <- time_series_one_year_mesonet + scale_x_date(date_labels = "%Y-%m", 
                                                                            date_breaks = "3 month", 
                                                                            limits = as.Date(c("2000-01-01", "2001-12-31")))
time_series_one_year_mesonet;

############################# Outliers ----------------------------------------------------------------

(boxplot(solar_irradiation[,-1], range=0)$out) # finding outliers
# there are none
boxplot.stats(solar_irradiation$IDAB)$out

#getting the max of the whole dataset
max(solar_irradiation[,-1])
min(solar_irradiation[,-1])

# getting the name of the weather station with each value ***delete***
solar_irradiation[solar_irradiation == max(solar_irradiation[,-1])]
colnames(solar_irradiation) 

# querying the labels
labels(solar_irradiation) # using papeR

# converting to labeled datatable
solar_irradiation_ldt <- convert.labels(solar_irradiation) # converting to ldf
class(solar_irradiation_ldt)

# ploting with a ldf using papeR
plot(solar_irradiation_ldt) #plots one at a time

############################# Mapping Weather Stations ------------------------------------------------

colnames(solar_stations) <- c("stid", "lat", "lng", "elev") #changing names for automatic recognition by leafled package

# displaying one Mesonet site (ACME)
m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=-98.02325, lat=34.80833, popup="ACME")
m  # Print the map

# displaying all Mesonet sites 
# we changed the names of the coloumns for them to be automatically recognise
# explanation can be found in ?addMarkers
m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=solar_stations$lng, lat=solar_stations$lat, popup=solar_stations$stid)
m  # Print the map

############################# Data Transformations ----------------------------------------------------

## standardization
summary(solar_irradiation)
preprocessParams <- preProcess(solar_irradiation[,-1], method = c("center","scale"))
print(preprocessParams)
transformed_solar_irradiation <- predict(preprocessParams, solar_irradiation)
summary(transformed_solar_irradiation)

############################# Stationary Test ---------------------------------------------------------

adf.test(solar_irradiation$ACME, alternative = "stationary", k = 0); # result = stationary
adf.test(solar_irradiation$IDAB, alternative = "stationary", k = 0); # result = stationary


############################# Short Term Load Forecasting (STLF): Forecasting algorithm --------------------------------------------

library(tseries)
library(forecast)
library(data.table)

tsdata_test <- ts(solar_irradiation[,-1], frequency = 365 ,start = c(1994,1,1))

prediction <- solar[5114:6909,1]
prediction <- as.data.table(prediction)
colnames(prediction) <- "Date"
prediction$Date <- as.Date.character(prediction$Date, format = c("%Y%m%d"));

for (i in 1:98){
  tsdata2 <- window(tsdata_test[,i], start = 1994)
  fcast2 <- stlf(tsdata2, method='naive', h=1796)
  ts_dt_forecast <- as.data.table(summary(fcast2)[,1])
  prediction <- cbind(prediction, ts_dt_forecast)
}

colnames(prediction) <- colnames(solar_irradiation)

write.csv(prediction,"/Users/erivero/Documents/MBD\ IE/t1/R\ PROGRAMMING/group_assignment/project(1)/prediction.csv", row.names = FALSE)
