##


### 1. Data exploration----------------------------------------------------------------
## a) Violation Code and Fine Amounts
#Add the violation code descriptions and fine amounts to the data file. Provide a visual overview of the top 10 most common types of violations (feel free to group them into categories if reasonable). Compare how this ranking differs if we focus on the total amount of revenue generated.

setwd("C:/Users/Qiang/Desktop/江晚映-纽约市违规停车/course_content-main/Exercises/07_parking-graded/data/parking")

parkingNYC_data <- read.csv("parkingNYC_Jan2020-Jan2021.csv")
parkingNYC_variable <- read.csv("parkingNYC_variable_descriptions.csv")

Vehicle.Body.Type.count <- as.data.frame(table(parkingNYC_data$Vehicle.Body.Type))

Vehicle.Body.Type.count.rank1 <- Vehicle.Body.Type.count[order(-Vehicle.Body.Type.count$Freq),]
Vehicle.Body.Type.count.top10 <- Vehicle.Body.Type.count.rank1[1:10,]
top10 <- as.data.frame(Vehicle.Body.Type.count.top10)
# visualization
library("ggplot2")
ggplot(data = top10,aes(x=Var1,y=Freq)) + geom_boxplot()

Vehicle.Body.Type.count.rank2 <- aggregate(Violation.Code~Vehicle.Body.Type,Vehicle.Body.Type.count,sum)


#(b)

### 2.2. Map by Precincts
library(maptools)
library("rgdal")#readODG读取空间地图数据文件
library("plyr")#用到join合并命令
library("dplyr")#用到left_join合并命令
library("ggplot2")#绘图

setwd("C:/Users/Qiang/Desktop/江晚映-纽约市违规停车/course_content-main/Exercises/07_parking-graded/data/police_precincts")
shp <- readOGR("nypp.shp",stringsAsFactors = FALSE)

mydata<-shp@data#读取空间数据文件的描述数据框
mydata1<-data.frame(mydata,id=seq(0:76)-1)#给每一行设定唯一标识，取名id

map_data <- fortify(shp)#读取空间数据文件的映射数据框
map_data1 <- join(map_data,mydata1,type="full")#将描述数据框和映射数据框通过id字段进行全连接

ggplot(data = map_data ,aes(x=long,y=lat))+geom_polygon(color="grey40")+scale_fill_gradient(low = "white",high = "red")+coord_map("polyconic")+theme(
  panel.grid=element_blank(),
  panel.background=element_blank(),
  axis.text=element_blank(),
  axis.ticks=element_blank(),
  axis.title=element_blank(),
  legend.position=c(-0.1,0.25))

### 3
## Precinct 19 identifies the Upper East Side. The data currently does not provide latitude and longitude of the violation locations (and I am not sure what these street_code variables are for).
# a)Ignoring fire hydrants






