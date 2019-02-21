
##########################################################
Data.MaxTemperature <- c(67,30,33,45,52,41,32,27,39,48,37)
Data.MinTemperature <- c(44,16,21,37,34,29,24,13,30,36,27)
Data.Condition <- c("cloudy","cloudy", "cloudy", "sunny" ,"sunny", "cloudy", "cloudy","sunny","sunny","cloudy","cloudy")
Data.Precipitation <- c(0,0,0.11,0,0,0,0,0,0,0,0)

### Computing Standard Deviation for Data.Precipitation
Data.Precipitation.Sd <- sd(Data.Precipitation);

### Computing Mean
Data.Temperature.Mean <- mean(c(Data.MaxTemperature,Data.MinTemperature))
Data.Temperature.Sd <- sd(c(Data.MaxTemperature,Data.MinTemperature))

### Frequency Tables
Data.Table<-transform(table(Data.Condition))
print(Data.Table)

### Printing Result 
print(paste0("Max temperature: ",mean(Data.MaxTemperature),"F°"))
print(paste0("Min temperature: ",round(mean(Data.MinTemperature)),"F°"))
print(paste0("Average temperature: ",round(Data.Temperature.Mean),"F°"))

### Tolerance 
tol = 1e-5

### Checking for rainfall/snowfall
if(Data.Temperature.Mean>=32 && Data.Precipitation.Sd >= (0.1-tol)){
  print(paste0("Precipitation: ",round(Data.Precipitation.Sd,3), "Rain"))
}else if(Data.Temperature.Mean<32 && Data.Precipitation.Sd >= (0.1-tol)){
  print(paste0("Precipitation: ",round(Data.Precipitation.Sd,3), "Snow"))
} else{
  print(paste0("Precipitation: ",round(Data.Precipitation.Sd,3), " / No Rain, No Snow "))
}

### Drawing plot - Type: Linear
plot(Data.MaxTemperature, type="l", ylab = "Temperature", main = "Max & Max Temperature",asp = 1/20,col = "red")

### Adding one more line to the grap 
lines(Data.MinTemperature, type="l",asp = 1/20, col = "blue")

### Creating a barchart
counts <- table(Data.Condition)
barplot(counts,  xlab="Condition", ylab = "Frequency", col = c("gold","brown"),asp = 1/4)
