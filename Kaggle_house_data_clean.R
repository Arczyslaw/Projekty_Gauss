library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
data_clean <- kc_house_data[,-1]
summary(data_clean)
#ilość pokoi jako liczba całkowita
data_clean <- as.data.frame(data_clean)
data_clean[,3]<- as.integer(data_clean[,3])
#viev -> ile razy było oglądane.
#condition na liczbe
data_clean[, 10]<- as.integer(data_clean[,10])
data_clean[,8]<- as.factor(data_clean[,8])
data_clean[,1]<- as.Date(data_clean[,1])

data_clean <- data_clean %>%
  select(-c(zipcode, lat, long))

data_clean <- data_clean %>%
  mutate(when_sold=as.double.difftime(difftime(today(), date)))%>%
  select(-date)
summary(data_clean)
data_to_import <- data_clean
fwrite(data_to_import, "data_to_import.csv")


data_clean[,8]<- as.factor(data_clean[,8])
data_clean <- data_clean%>%
  select(-sqft_living)

d=dist(kc_house_data[,18:19])
h=hclust(d, 'average')
plot(h)
data_clean$district <- as.factor(cutree(h, 11))
data_to_import_full <- data_clean
summary(data_to_import_full)
fwrite(data_to_import_full, "data_to_import_full.csv")

data_clean <- data_clean %>%
  select(-c(sqft_lot, sqft_lot15))

data_clean <- data_clean%>%
  mutate(is_basement=as.factor(if_else(sqft_basement>0, 1, 0)))

data_clean <- data_clean%>%
  mutate(is_renovated=as.factor(if_else(yr_renovated>0, 1, 0)))

data_to_import_factor <- data_clean
fwrite(data_to_import_factor, "data_to_import_factor.csv")

summary(data_to_import)

plot(data_clean$price)
ggplot(data = data_clean, aes(x=price, sqft_lot))+
  geom_point()

kc_house_data$district <- cutree(h, 11)
ggplot(kc_house_data, aes(x=lat, long))+
  geom_point(color=as.factor(kc_house_data$district))

#jest outlier
ggplot(kc_house_data, aes(price, sqft_lot))+
  geom_point()

ggplot(data = data_clean, aes(x=price, sqft_living))+
  geom_point()

ggplot(data = data_clean, aes(x=price, y=view))+
  geom_point()

#sfaktoryzować?
ggplot(data = data_clean, aes(x=price, yr_renovated))+
  geom_point()

ggplot(data = data_clean, aes(x=price, grade))+
  geom_point()

ggplot(data=data_clean, aes(x=price, sqft_basement))+
  geom_point()

ggplot(data=data_clean, aes(x=price, is_renovated))+
  geom_point()

ggplot(data=data_clean, aes(x=grade, is_renovated))+
  geom_point()
