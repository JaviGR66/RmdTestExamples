library(nycflights13)


data<-flights
clean_data<-na.omit(data)


head(clean_data)


flights_model_0 <- lm(clean_data$arr_delay~clean_data$distance+clean_data$air_time)
summary(flights_model_0)

linearMod <- lm(clean_data$arr_delay ~ clean_data$dep_delay)
summary(linearMod)


f1 <- summary(flights_model_0)$r.squared
f1

f2 <- summary(linearMod)$r.squared
f2