library(readr)
library(geosphere)
library(ggplot2)

Data=read_csv("Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp.csv")
Data$date=format(as.Date(Data$date), "%Y-%m-%d")
Data$daylength=daylength(43.3802, Data$date)
DataFall=subset(Data, Data$dayofyr>172)

ggplot(Data, aes(x=daylength, y=log(totalperL+.01), color=site)) + geom_point()
ggplot(Data, aes(x=daylength, y=log(totalperL+.01), color=year)) + geom_point()
ggplot(Data, aes(x=daylength, y=totalperL_diff, color=site)) + geom_point()
ggplot(Data, aes(x=daylength, y=totalperL_diff, color=year)) + geom_point()

ggplot(DataFall, aes(x=daylength, y=log(totalperL+.01), color=site)) + geom_point()
ggplot(DataFall, aes(x=daylength, y=log(totalperL+.01), color=year)) + geom_point()
ggplot(DataFall, aes(x=daylength, y=totalperL_diff, color=site)) + geom_point()
ggplot(DataFall, aes(x=daylength, y=totalperL_diff, color=year)) + geom_point()

Data$Intxn=Data$watertemp_mean*Data$daylength
DataFall$Intxn=DataFall$watertemp_mean*DataFall$daylength

ggplot(Data, aes(x=Intxn, y=log(totalperL+.01), color=site)) + geom_point()
ggplot(Data, aes(x=Intxn, y=log(totalperL+.01), color=year)) + geom_point()
ggplot(Data, aes(x=Intxn, y=totalperL_diff, color=site)) + geom_point()
ggplot(Data, aes(x=Intxn, y=totalperL_diff, color=year)) + geom_point()

ggplot(DataFall, aes(x=Intxn, y=log(totalperL+.01), color=site)) + geom_point()
ggplot(DataFall, aes(x=Intxn, y=log(totalperL+.01), color=year)) + geom_point()
ggplot(DataFall, aes(x=Intxn, y=totalperL_diff, color=site)) + geom_point()
ggplot(DataFall, aes(x=Intxn, y=totalperL_diff, color=year)) + geom_point()

ggplot(DataFall, aes(x=watertemp_mean, y=log(totalperL+.01), color=year)) + geom_point()

