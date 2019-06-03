## Simulation of a Time Series with Gamma Distribution and Graphical Representation

1) Në serinë e parë janë simuluar 500 të dhëna nga një shpërndarje gama 
2) Nga paraqitja grafike themi se seria nuk ka trend dhe nuk vihen re sezonalitete të qarta
3) Vihet re zhurma
4) Seria është stacionare


```R
library(ggplot2)
library(zoo)

data=rgamma ( n =500 ,shape =2 , scale =3)

ts_data=ts(data,frequency=24,start=c(1990,1)

df=data.frame(Value=as.matrix(ts_data), Year=as.Date(as.yearmon(time(ts_data))))
head(df)

p=ggplot(data = df, aes(x =Year, y = Value))
+ geom_line(color = "#00AFBB", size = 1) 
+ ggtitle("Daily data for Gamma Distribution")
+ theme(plot.title = element_text(hjust = 0.5, color="red", size=14, face="bold.italic"))

p + stat_smooth(color = "#FC4E07", fill = "#FC4E07",method = "loess")
```
