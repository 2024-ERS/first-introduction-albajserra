# Combine cockle data with elevation data using a relational database approach 
# Schiermonnikoog transect

# clear everything in memory
rm(list=ls())

# restore and load libraries
renv::restore()
library(tidyverse) # including ggplot2, dplyr that we 

# load the elevation data and show the first 10 records of the dataset
names(elevdat)
elevdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") |>
  dplyr::mutate(year=factor(year)) # make year a factor so it is not a continuous variable but a discrete category
elevdat

# plot the change in transect  elevation along the transect, using a separate graph for each for each year 
elevdat |>
  ggplot(aes(x=TransectPoint_ID, y=elevation_m)) + 
  geom_line() + 
  facet_wrap(~year) # each year is separated in a different graph
# plot the change in transect  elevation along the transect, using a separate line color for each year 
elevdat |>
  ggplot(aes(x=TransectPoint_ID, y=elevation_m,col=year)) + 
  geom_line()
# Extract the data for 2017 in a new tibble, keep only variables distance_m and elevation
# omit records where Distance_ID is missing (NA)
elevdat2017 <- elevdat |>
  dplyr::filter(year==2017) |> # select only the year 2017
  dplyr::select(TransectPoint_ID, elevation_m) |> # only choose TransectPoint_ID and elevation_m as variables
  dplyr::filter(!is.na(TransectPoint_ID)) # omit records where Distance_ID is missing

# read the cockle data 
# keep only the data for 2017, 
# omit observations (Obs_ID) 468 and 1531
# calculate the mean number of cockles and mean size for each distance
cdat2017 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSpormjGiM4uWMTIZYm9Upt5j1Ige_Wu9CrhGGdvjXwxmHP2R6S7JDyEmiGTn3fXihl5s5yBn_sdo8h/pub?gid=1538766002&single=true&output=csv") |>
  dplyr::filter(year==2017, # only use 2017
                CockleObs_ID!=468,
                CockleObs_ID!=1531) |> # remove outliers
  dplyr::group_by(TransectPoint_ID) |> # group by distance
  dplyr::summarize(n_obs=n(),
                   avg_l=mean(length_mm,na.rm = T), # calculate mean
                   sd_l=sd(length_mm,na.rm = T), # calculate standard deviation
                   se_l=sd_l/sqrt(n_obs)) # calculate standard error
print(cdat2017)

# plot (with a line and points)  how the number of cockles changes with distance along the transect
##### merge the cockle and elevation data into a single table you call "combidat"
# using TransectPoint_ID as the common variable between the two tables
combidat <- dplyr::left_join(elevdat2017,cdat2017,by="TransectPoint_ID") |>
  dplyr::mutate(n_obs=tidyr::replace_na(n_obs,0))
combidat
# show in a plot how cockle density changes with elevation
combidat |>
  ggplot(aes(x=elevation_m,y=n_obs)) + 
  geom_point() + 
  geom_smooth(method="loess") # fit a linear regression

# predicted at 0.5 m (x)
# y = b0 + b1x   (b0 is intercept and b1 is the slope, x is elevation, y is no cockles)
model_lm<-lm(n_obs~elevation_m, data=combidat)
summary(model_lm)
1.36820 -0.01396*0.5
# show this model as a line in ggplot, with the confidence interval
ggplot2::ggplot(data=combidat,
                mapping=aes(x=elevation_m,y=n_obs)) + 
  geom_point() + 
  geom_smooth(method="lm")
# fit a better model, using a loess smoother
ggplot2::ggplot(data=combidat,
                mapping=aes(x=elevation_m,y=n_obs)) + 
  geom_point() + 
  geom_smooth(method="loess")
# show this model in ggplot

##### plot how the size (as mean length) of cockles changes with elevation along the transect
# omit the observations where length is NA (because no cockles were measures)
# fit a quadratic model (second-order polynomial)
# show for each point also the standard errors
# add appropriate labels for the x and y axis 
combidat_size <- combidat |>
  dplyr::mutate(n_obs=tidyr::replace_na(n_obs,0)) |>
  dplyr::filter(avg_l!=0)

ggplot(data=combidat_size,
                mapping=aes(x=elevation_m,y=avg_l)) + 
  geom_point() + 
  xlab("elevation (mm)") + #lab = labels
  ylab("cockle length (mm)") + 
  geom_smooth(method="loess")

#create a new variable for combidat_size
combidat_size$elevation_m2 <- combidat_size$elevation_m^2

#fit quadratic regression model
quadraticModel <- lm(avg_l ~ elevation_m + elevation_m2, data=combidat_size)
summary(quadraticModel)
