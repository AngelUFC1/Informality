remove(list=ls())
setwd("C:/Users/angel/Downloads")
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(blackmarbler)
library(raster)
library(exactextractr)
library(lubridate)
library(geodata)
library(scales)
library(mFilter)
library(devtools)
library(stargazer)
library(plyr)
library(lubridate)

########################NIGHTLIGHT DATA FOR PERU################################
devtools::install_github("worldbank/blackmarbler")

bearer <- "CODE_HERE"

#Obtaining region of interest (ROI) as a simple feature (sf) object for Peru
#at the second administrative level

shapefile <- gadm(country = "PER", level = 2, path = tempdir()) %>%
  st_as_sf()
shapefile1 <- gadm(country = "PER", level = 1, path = tempdir()) %>%
  st_as_sf()
#filter(GID_2 == "AUS.5.122_1")

head(shapefile)
head(shapefile1)

# Downloading Black Marble data for the ROI over a specified date range ######## Check
black_marble_data1 <- bm_raster(roi_sf = shapefile1, # this specifies the region of interest
                               product_id = "VNP46A3", # this specifies the monthly luminosity data
                               date = seq.Date(from = ymd("2014-01-01"), to = ymd("2023-12-01"), by = "month"), # this specifies the date range
                               bearer = bearer, # this is the API bearer for NASA LAADS DAAC from Step 3
                               variable = c("AllAngle_Composite_Snow_Free"), # this selects the layer we're interested in -> satellite angle and snow-free observations
                               quality_flag_rm = c(255, 2)) # this drops bad quality observations and coverts them to NA values)

# Storing the layer names from the original raster stack
original_layer_peru <- names(black_marble_data)
original_layer_peru1 <- names(black_marble_data1)

original_layer_peru
original_layer_peru1

# Imputing missing values in the raster data
imputed_data_peru <- approxNA(black_marble_data, method="linear", rule=2, f=0) #addressing the NA values by linear interpolation
imputed_data_peru1 <- approxNA(black_marble_data1, method="linear", rule=2, f=0) #addressing the NA values by linear interpolation

# Reassigning the original layer names to the new raster stack
names(imputed_data_peru) <- original_layer_peru
names(imputed_data_peru1) <- original_layer_peru1

# Transforming the ROI to match the CRS of the raster data
transformed_shapefile_peru <- st_transform(shapefile, crs(imputed_data_peru))
transformed_shapefile_peru1 <- st_transform(shapefile1, crs(imputed_data_peru1))

# Initializing an empty dataframe to store results
results_peru_year <- data.frame(GID_2 = character(), Luminosity = numeric(), Date = character(), length = character())

# Extracting and summarizing data for each raster layer
for (i in 1:nlayers(imputed_data_peru)) {
  layer_values_peru <- exact_extract(imputed_data_peru[[i]], transformed_shapefile_peru, 'sum')
  layer_results_peru <- data.frame(GID_2 = transformed_shapefile_peru$GID_2,
                               Luminosity = layer_values_peru,
                               Date = rep(names(imputed_data_peru)[i], length(layer_values_peru)))
  results_peru_year <- rbind(results_peru_year, layer_results_peru)
}

################################################################################
# Initializing an empty dataframe to store results 2
results_peru_year1 <- data.frame(GID_1 = character(), Luminosity = numeric(), Date = character())

# Extracting and summarizing data for each raster layer
for (i in 1:nlayers(imputed_data_peru1)) {
  layer_values_peru1 <- exact_extract(imputed_data_peru1[[i]], transformed_shapefile_peru1, 'sum')
  layer_results_peru1 <- data.frame(GID_1 = transformed_shapefile_peru1$GID_1,
                                   Luminosity = layer_values_peru1,
                                   Date = rep(names(imputed_data_peru1)[i], length(layer_values_peru1)))
  results_peru_year1 <- rbind(results_peru_year1, layer_results_peru1)
}
# Formatting the Date column
results_peru_year$Date <- as.Date(paste0(substr(results_peru_year$Date, 2, 5), "-", substr(results_peru_year$Date, 7, 8), "-01"))
results_peru_year1$Date <- as.Date(paste0(substr(results_peru_year1$Date, 2, 5), "-", substr(results_peru_year1$Date, 7, 8), "-01"))

write.csv(shapefile, "C:/Users/angel/Downloads/shapefile_peru_year1.csv", row.names=FALSE)
write.csv(results_peru_year, "C:/Users/angel/Downloads/results_peru_year2.csv", row.names=FALSE)

write.csv(shapefile1, "C:/Users/angel/Downloads/shapefile_peru_year1.csv", row.names=FALSE)
write.csv(results_peru_year1, "C:/Users/angel/Downloads/results_peru_year1.csv", row.names=FALSE)

data_results_year <- read.csv("results_peru_year2.csv")
data_results_year1 <- read.csv("results_peru_year1.csv")

# Extract Year and Month
data_results_year1$Year <- format(data_results_year1$Date, "%Y")
data_results_year1$Month <- format(data_results_year1$Date, "%m")

# Aggregate Luminosity by Year and Month
national_results_year1 <- aggregate(Luminosity ~ Year + Month, data = results_peru_year1, sum)

# Order the results by Year and Month
national_results_year1 <- national_results_year1[order(national_results_year1$Year, national_results_year1$Month), ]

national_results_year1[, Date := as.Date(paste(Year, Month, "01", sep = "-"))]

# Print the results
print(national_results_year1)


write.csv(national_results_year1, "C:/Users/angel/Downloads/results_peru_yearly1.csv", row.names=FALSE)

plot_peru <- ggplot(national_results_year1, aes(x = Date, y = (national_lum), color = Date)) +
  geom_line() +
  labs(title = "", subtitle = "", x = "Año", y = "Luminosidad") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal()
plot_peru

results_lima <- subset(results_peru_year1, GID_1 == "PER.16_1")
summary(results_peru_year1)
plot_lima <- ggplot(results_lima, aes(x = Date, y = (Luminosity), group = GID_1, color = GID_1)) +
                    geom_line() +
                    labs(title = "Monthly Luminosity in Lima", subtitle = "Data from NASA Black Marble", x = "Year", y = "Luminosity") +
                    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
                    theme_minimal()
plot_lima
write.csv(results_lima, "C:/Users/angel/Downloads/results_lima.csv", row.names=FALSE)

##############################COMENZAR AQUI#####################################
opar <- par(no.readonly=TRUE)

df <- read.csv('results_peru.csv')
plot(df$national_lum)

#df$Date <- as.Date(df$Date)

ts_data <- ts(df$national_lum, frequency = 12)

filtered_data <- hpfilter(ts_data)

# Extract trend and cycle components
cycle <- as.numeric(filtered_data$cycle)
trend <- as.numeric(filtered_data$trend)

plot(filtered_data)
plot(ts_data)
plot(trend)
plot(cycle)

plot(ts_data, ylim=c(-500000,3000000),
     main="Filtro Hodrick-Prescott de Luminosidad: Tendencia y Ciclo",
     col=1, ylab="Luminosidad")
lines(filtered_data$trend,col=2)
lines(filtered_data$cycle,col=3)

################################################################################
#Using NLD and GDP
data_lum <- read.csv("results_peru_year.csv")
data_lum1 <- read.csv("results_peru_year1.csv")

data_peru$Date <- results_peru_year$Date

data_lum$Date <- as.Date(paste0(substr(data_results_year1$Date, 2, 5), "-", substr(data_results_year1$Date, 7, 8), "-01"))

data_peru[5:9] <- lapply(data_peru[5:9], as.numeric)
national_results_year$Date <- as.Date(national_results_year$Date)

national_results_year <- data_peru %>% group_by(Date) %>%
  summarise(national_lum=sum(Luminosity), national_gdp=sum(GDP))

write.csv(results_peru_year, "C:/Users/angel/Downloads/final_results.csv", row.names=FALSE)

data_lum0 <- read.csv("final_results2.csv")

data_lum0$Year <- as.Date(paste0(data_lum0$Year, "-01-01"))
data_lum0$Date <- as.Date(data_lum0$Year)

# Function factory for secondary axis transforms
train_sec <- function(primary, secondary, na.rm = TRUE) {
  # Thanks Henry Holm for including the na.rm argument!
  from <- range(secondary, na.rm = na.rm)
  to   <- range(primary, na.rm = na.rm)
  # Forward transform for the data
  forward <- function(x) {
    rescale(x, from = from, to = to)
  }
  # Reverse transform for the secondary axis
  reverse <- function(x) {
    rescale(x, from = to, to = from)
  }
  list(fwd = forward, rev = reverse)
}

sec <- with(data_lum0, train_sec(nat_lum, nat_gdp))

nld_gdp <- ggplot(data_lum0, aes(Year)) +
              geom_line(aes(y = nat_lum), colour = "blue") +
              geom_line(aes(y = sec$fwd(nat_gdp)), colour = "red") +
              scale_y_continuous(sec.axis = sec_axis(~sec$rev(.), name = "PBI Nacional")) +
              labs(title = "PBI and Luminosidad", subtitle = "Fuentes: NASA e INEI", x = "Year", y = "Suma de luminosidad") +
              scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
              theme_minimal()
nld_gdp

write.csv(national_results_year, "C:/Users/angel/Downloads/final_results2.csv", row.names=FALSE)

################################################################################
data_peru <- read.csv("results_peru_merged - Copy (2).csv")

data_peru$lnlum <- log(data_peru$Lum)
data_peru$lngdp <- log(data_peru$GDP)
data_peru$Pri <- (data_peru$Primary/(data_peru$Primary+data_peru$Secondary+data_peru$Tertiary))
data_peru$Sec <- (data_peru$Secondary/(data_peru$Primary+data_peru$Secondary+data_peru$Tertiary))
data_peru$Ter <- (data_peru$Tertiary/(data_peru$Primary+data_peru$Secondary+data_peru$Tertiary))
data_peru$Bench1 <- ifelse(data_peru$Region == "Lima", 1, 0)
data_peru$Bench2 <- ifelse(data_peru$Region == "Moquegua", 1, 0)
data_peru$Bench3 <- ifelse(data_peru$Region == "Arequipa", 1, 0)
data_peru$Bench4 <- ifelse(data_peru$Region == "Callao", 1, 0)

data_peru$IntPri <- data_peru$lnlum*data_peru$Pri
data_peru$IntSec <- data_peru$lnlum*data_peru$Sec
data_peru$IntTer <- data_peru$lnlum*data_peru$Ter
data_peru$IntBench1 <- data_peru$lnlum*data_peru$Bench1
data_peru$IntBench2 <- data_peru$lnlum*data_peru$Bench2
data_peru$IntBench3 <- data_peru$lnlum*data_peru$Bench3
data_peru$IntBench4 <- data_peru$lnlum*data_peru$Bench4

#write.csv(data_peru, "C:/Users/angel/Downloads/data_final_2024.csv", row.names=FALSE)

#data_peru <- read.csv("data_peru.csv")

reg_1 <- lm(lngdp ~ data_peru$IntPri + data_peru$IntSec + data_peru$IntTer + data_peru$IntBench1 + Population + PopSquare, data = data_peru)
summary(reg_1) #Review the results
reg_2 <- lm(lngdp ~ data_peru$IntPri + data_peru$IntSec + data_peru$IntTer + data_peru$IntBench2 + Population + PopSquare, data = data_peru)
summary(reg_2)
reg_3 <- lm(lngdp ~ data_peru$IntPri + data_peru$IntSec + data_peru$IntTer + data_peru$IntBench3 + Population + PopSquare, data = data_peru)
summary(reg_3)
reg_4 <- lm(lngdp ~ data_peru$IntPri + data_peru$IntSec + data_peru$IntTer + data_peru$IntBench4 + Population + PopSquare, data = data_peru)
summary(reg_4)
stargazer(reg_1, reg_2, reg_4, type = "latex", title="Results", align=TRUE,  no.space = TRUE)

data_peru$calc_gdp1 <- reg_1$coefficients[1] + (reg_1$coefficients[2]*data_peru$IntPri) + 
  (reg_1$coefficients[3]*data_peru$IntSec) + (reg_1$coefficients[4]*data_peru$IntTer) +
  (reg_1$coefficients[5]*data_peru$IntBench1) + (reg_1$coefficients[6]*data_peru$Population) +
  (reg_1$coefficients[7]*data_peru$PopSquare)

data_peru$calc_gdp2 <- reg_2$coefficients[1] + (reg_2$coefficients[2]*data_peru$IntPri) + 
  (reg_2$coefficients[3]*data_peru$IntSec) + (reg_2$coefficients[4]*data_peru$IntTer) +
  (reg_2$coefficients[5]*data_peru$IntBench2) + (reg_2$coefficients[6]*data_peru$Population) +
  (reg_2$coefficients[7]*data_peru$PopSquare)

data_peru$calc_gdp <- reg_4$coefficients[1] + (reg_4$coefficients[2]*data_peru$IntPri) + 
  (reg_4$coefficients[3]*data_peru$IntSec) + (reg_4$coefficients[4]*data_peru$IntTer) +
  (reg_4$coefficients[5]*data_peru$IntBench4) + (reg_4$coefficients[6]*data_peru$Population) +
  (reg_4$coefficients[7]*data_peru$PopSquare)

data_peru$gdp1 <- exp(data_peru$calc_gdp1)
data_peru$gdp2 <- exp(data_peru$calc_gdp2)
data_peru$gdp_calc <- exp(data_peru$calc_gdp)

write.csv(data_peru, "C:/Users/angel/Downloads/data_peru_final.csv", row.names=FALSE)

data_peru_year <- data_peru %>% group_by(Region) %>%
  summarise(Date=Date, actual_gdp=GDP, c_gdp=gdp_calc)

data_peru_year$informality <- data_peru_year$actual_gdp - data_peru_year$c_gdp
plot(data_peru_year$informality)

write.csv(data_peru_year, "C:/Users/angel/Downloads/data_peru_year_final.csv", row.names=FALSE)

data_2014 <- subset(data_peru_year, data_peru_year$Date=="1/1/2014")
data_2017 <- subset(data_peru_year, data_peru_year$Date=="1/1/2017")
data_2020 <- subset(data_peru_year, data_peru_year$Date=="1/1/2020")
data_2023 <- subset(data_peru_year, data_peru_year$Date=="1/1/2023")

par(mfrow = c(2, 2))
plot(log(data_2014$actual_gdp), log(data_2014$c_gdp))
abline(0,1)
plot(log(data_2017$actual_gdp), log(data_2017$c_gdp))
abline(0,1)
plot(log(data_2020$actual_gdp), log(data_2020$c_gdp))
abline(0,1)
plot(log(data_2023$actual_gdp), log(data_2023$c_gdp))
abline(0,1)

data_peru$informality <- data_peru$GDP - data_peru$gdp_calc

plot(data_peru$informality)
