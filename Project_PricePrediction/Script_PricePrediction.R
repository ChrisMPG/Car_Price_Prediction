# Author of the script: Christian Goncalves 
# Work done at 10/01/2023

#---------------------------------------------------------------------------------------------------

# Libraries

library(corrplot)
library(tidyverse)
library(Amelia)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggrepel)
library(formattable)
library(stringr)

#---------------------------------------------------------------------------------------------------

# Dataset 

df <- read.table("C:/Users/cerde/Documents/MTAD/Car Price Prediction/CarPrice_Assignment.csv", sep=",", header = T)


#---------------------------------------------------------------------------------------------------

# Orthographic correction of the Alfa-romero 
df1 <- within (df, {
  CarName[CarName == 'alfa-romero giulia'] <- "alfa giulia"
  CarName[CarName == 'alfa-romero stelvio'] <- "alfa stelvio"
  CarName[CarName == 'alfa-romero Quadrifoglio'] <- "alfa quadrifoglio"
})

# Separation of the CarName into two new columns
df1 <- df1 %>% separate(CarName, c('Brand', 'Model'))


# Orthographic correction in Brand column
df1 <- within(df1,{
  Brand[Brand == 'vw'] <- "volkswagen"
  Brand[Brand == 'maxda'] <- "mazda"
  Brand[Brand == 'porcshce'] <- "porsche"
  Brand[Brand == 'vokswagen'] <- "volkswagen"
  Brand[Brand == 'toyouta'] <- "toyota"
  Brand[Brand == 'Nissan'] <- "nissan"
})

# Addressing the missing Subaru models
df1$Model <- ifelse(df1$Brand == 'subaru' & is.na(df1$Model), 'impreza', df1$Model)

#---------------------------------------------------------------------------------------------------

# Changing the data types of the variables "cylindernumber" and "doornumber"

# Show data types
str(df1)

df1$cylindernumber <- as.numeric(ifelse(df1$cylindernumber == 'two', 2,
                                        ifelse(df1$cylindernumber == 'three', 3,
                                               ifelse(df1$cylindernumber == 'four', 4,
                                                      ifelse(df1$cylindernumber == 'five', 5,
                                                             ifelse(df1$cylindernumber == 'six', 6,
                                                                    ifelse(df1$cylindernumber == 'eight', 8,
                                                                           ifelse(df1$cylindernumber == 'twelve', 12, 4 ))))))))

df1$doornumber <- as.numeric(ifelse(df1$doornumber == 'two', 2, 
                                    ifelse(df1$doornumber == 'four', 4, 4)))
                                    

# Creating a new column meanmpg
df1$meanmpg <- rowMeans(df1[,c('citympg', 'highwaympg')], na.rm=TRUE)

# Discarding the column Car_ID
df1$car_ID = NULL

# Adding a currency to the price column
currency(df1$price, digits = 0L)
df1






#---------------------------------------------------------------------------------------------------

# Check for missing values 
options(repr.plot.width = 12, repr.plot.height = 10)
missmap(df1,margins=c(4,2))

#---------------------------------------------------------------------------------------------------

# Corrplot
num.cols <- sapply(df1, is.numeric)
df2 <- df1[,num.cols]
corrplot(cor(df2), order = "hclust", method = "number")

#---------------------------------------------------------------------------------------------------

#Plotting 


#Car prices taking into account traction

ggplot(df1, aes(drivewheel,price)) + 
  geom_boxplot() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),plot.title = element_text(hjust = .5,lineheight = .20, size = 15)) +
  scale_y_continuous(label = scales::comma_format()) + 
  labs(title = "Car prices taking into account traction")


#Car prices taking into account traction and carbody

ggplot(df1, aes(drivewheel,price)) + 
  geom_boxplot() + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 12), 
        plot.title = element_text(hjust = .5,lineheight = .20, size = 15),
        plot.subtitle = element_text(hjust = .5,lineheight = .20, size = 12)) +
  facet_grid(~carbody) +
  scale_y_continuous(label = scales::comma_format()) + 
  labs(title = "Car prices taking into account traction and carbody")

#Car prices taking into account traction, carbody and fueltype

ggplot(df1, aes(drivewheel,price)) + 
  geom_boxplot() + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 12)) +
  facet_grid(fueltype~carbody) +
  scale_y_continuous(label = scales::comma_format()) +
  labs(title = "Car prices taking into account traction, carbody and fueltype")


#Car prices taking into account traction, carbody, fueltype and doornumber

ggplot(df, aes(drivewheel,price, fill = doornumber)) + 
  geom_boxplot() + facet_grid(fueltype~carbody) + 
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 12)) + 
  scale_y_continuous(label = scales::comma_format())+
  labs(title = "Car prices taking into account traction, carbody, fueltype and doornumber")


#----------------------------------------------------------------------------------------------------

#Car price distribution

ggplot(df1, aes(price)) + 
  geom_histogram(bins = 30, col = "white") + 
  scale_x_continuous(label = scales::comma_format()) + 
  labs(title = "Car price distribution") + 
  theme(plot.title = element_text(hjust = .5,lineheight = .20, size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))


#Concluimos que a maioria dos carros neste dataset custam menos de 20,000

ggplot(df1, aes(price)) + 
  geom_histogram(bins = 30, col = "white") + 
  scale_x_continuous(label = scales::comma_format()) + 
  labs(title = "Car price distribution taking into account the traction") + 
  theme(plot.title = element_text(hjust = .5,lineheight = .20, size = 15),
        plot.subtitle = element_text(hjust = .5,lineheight = .20, size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14)) + 
  facet_wrap(~drivewheel, nrow=3)


#Car price distribution taking into account the traction and cylindernumber

ggplot(df1, aes(price)) + 
  geom_histogram(bins = 30, col = "white") + 
  scale_x_continuous(label = scales::comma_format()) + 
  labs(title = "Car price distribution taking into account the traction and cylindernumber") + 
  theme(plot.title = element_text(hjust = .5,lineheight = .20, size = 15),
        plot.subtitle = element_text(hjust = .5,lineheight = .20, size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14)) + 
  facet_wrap(~cylindernumber, nrow=3)


#---------------------------------------------------------------------------------------------------


ordered_price <- df1 %>% 
  arrange(desc(price))

#Most expensive cars tibble
high_price <- ordered_price %>% filter(price > mean(price)) %>%
  dplyr::select(price, enginesize, drivewheel, Brand, Model)

 
ordered_price %>% 
  group_by(Brand) %>% 
  summarise(mean = round(mean(price),0),sd = round(sd(price),0)) %>% 
  arrange(desc(mean))


# Most expensive cars in the dataset plot

ggplot(high_price, aes(price, reorder(Model, price))) + 
  geom_point() + 
  facet_grid(
    factor(Brand, levels = 
             c("buick", 'bmw', 'porsche','jaguar', 'audi','volvo','nissan','saab','mazda','toyota',
               'peugeot','mercury','alfa','mitsubishi','volkswagen')) ~., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0),strip.text= element_text(size=13),axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = .5,lineheight = .20, size = 15),
        plot.subtitle = element_text(hjust = .5,lineheight = .20, size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10)) +
  scale_x_continuous(label = scales::comma_format()) + 
  labs(title="Carros com um preço acima da média do dataset") + ylab("Model")


#Relationship plot between enginesize and price

ggplot(df1, aes(price, enginesize, color = drivewheel)) + 
  geom_point() + 
  geom_smooth(method = lm, se = F, color = "black", size = 0.2, linetype = "dashed") +
  geom_hline(yintercept = mean(df1$enginesize), linetype = "dashed", size = 1) + 
  geom_vline(xintercept = mean(df1$price), linetype = "dashed", size = 1) +
  annotate("text", x = 14900, y = 250, label = "Mean",size = 5) +
  annotate("text", x = 38000, y = 131, label = "Mean",size = 5) +
  geom_text_repel(data=ordered_price %>% top_n(20,price), aes(price, enginesize, label = Brand),color = "red",
                  segment.color = "black") + scale_x_continuous(label = scales::comma_format()) +
  geom_text_repel(data=ordered_price %>% top_n(-3,price), aes(price, enginesize, label = Brand),color = "red",
                  segment.color = "black") +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = .5,lineheight = .20, size = 15),
        plot.subtitle = element_text(hjust = .5,lineheight = .20, size = 12)) + 
  labs(title = "Relationship plot between enginesize and price")



#Car price distribution taking into account the enginesize, drivewheel and carbody

ggplot(df1, aes(price, enginesize, color = carbody)) + 
  geom_point() +
  facet_wrap(~drivewheel) +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = .5,lineheight = .20, size = 15),
        plot.subtitle = element_text(hjust = .5,lineheight = .20, size = 12),
        strip.text = element_text(size = 12)) +
  scale_x_continuous(label = scales::comma_format()) +
  labs(title = "Car price distribution taking into account the enginesize, drivewheel and carbody")


#----------------------------------------------------------------------------------------------------

#Gradient Boosting Machine

p1 <- read.csv("GBM_SM.csv")

difp1 <- abs(p1$price - p1$predict)
mdp1 <- mean(difp1)

ggplot(p1, aes(price, predict))+
  theme_bw()+
  ggtitle("Model 1 - GBM without Brand and Model: Predict vs Real")+
  geom_abline()+
  labs(y = "Predict", x = "Price")+
  geom_point(shape = 21, size = 1) + 
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_continuous(label = scales::comma_format())


p2 <- read.csv("GBM_M.csv")

difp2 <- abs(p2$price - p2$predict)
mdp2 <- mean(difp2)

ggplot(p2, aes(price, predict))+
  theme_bw()+
  ggtitle("GBM with Brand and Model: Predict vs Real")+
  geom_abline()+
  labs(y = "Predict", x = "Price")+
  geom_point(shape = 21, size = 1) + 
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_continuous(label = scales::comma_format())

#----------------------------------------------------------------------------------------------------

#Distributed Random Forest

p3 <- read.csv("DRF_SM.csv")

difp3 <- abs(p3$price - p3$predict)
mdp3 <- mean(difp3)

ggplot(p3, aes(price, predict))+
  theme_bw()+
  ggtitle("Modelo 3 - DRF without Brand and Model: Predict vs Real")+
  geom_abline()+
  labs(y = "Predict", x = "Price")+
  geom_point(shape = 21, size = 1) + 
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_continuous(label = scales::comma_format())

p4 <- read.csv("DRF_M.csv")

difp4 <- abs(p4$price - p4$predict)
mdp4 <- mean(difp4)

ggplot(p4, aes(price, predict))+
  theme_bw()+
  ggtitle("Modelo 4 - DRF with Brand and Model: Predict vs Real")+
  geom_abline()+
  labs(y = "Predict", x = "Price")+
  geom_point(shape = 21, size = 1) + 
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_continuous(label = scales::comma_format())

#----------------------------------------------------------------------------------------------------


# Dataframe for shiny 
df3 <- df1
df3$car_ID = NULL
df3$symboling = NULL
df3$aspiration = NULL
df3$enginelocation = NULL
df3$carheight = NULL
df3$enginetype = NULL
df3$fuelsystem = NULL
df3$stroke = NULL
df3$compressionratio = NULL
df3$peakrpm = NULL
df3$citympg = NULL
df3$highwaympg = NULL





























