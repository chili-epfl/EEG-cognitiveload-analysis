require(ggplot2)

source("../rollingWindows.R")

#subject <- c("lollo","lukasz")


############################### LOLLO #############################################

datatasklollo <- read.csv("task-test-lollo.txt", header=F)
dataeeglollo <- read.csv("record-lollo-[2015.05.15-11.45.11].csv", sep=";")
datagazelollo <- read.csv("gaze-test-lollo.txt")

dataeeglollo <- dataeeglollo[dataeeglollo$Time..s.>4,]
dataeeglollo$Time..s. <- dataeeglollo$Time..s. - min(dataeeglollo$Time..s.)
datarolllollo <- rollingMean(times = dataeeglollo$Time..s., values = dataeeglollo$Theta*dataeeglollo$Attention, window = 2, slide = 1)

datameanlollo <- rollingMean(times = datagazelollo$timestamp, values = datagazelollo$DiamRX, window = 2*1000*1000, slide = 1*1000*1000 )
datasdlollo <-  rollingSd(times = datagazelollo$timestamp, values = datagazelollo$DiamRX, window = 2*1000*1000, slide = 1*1000*1000 )
eyeloadlollo <- as.numeric(datameanlollo$value>median(datameanlollo$value))+as.numeric(datasdlollo$value>median(datasdlollo$value))

scaledmean <- (datameanlollo$value-min(datameanlollo$value))/(max(datameanlollo$value)-min(datameanlollo$value))
scaledeeg <- (datarolllollo$value-min(datarolllollo$value))/(max(datarolllollo$value)-(min(datarolllollo$value)))

cor.test(scaledmean,scaledeeg[1:167])

ggplot(datameanlollo,aes(x=seq_along(datameanlollo$value),y=(datameanlollo$value-min(datameanlollo$value))/(max(datameanlollo$value)-min(datameanlollo$value))))+
    geom_line()+
    geom_line(data=datarolllollo,aes(x=seq_along(datarolllollo$value),y=(datarolllollo$value-min(datarolllollo$value))/(max(datarolllollo$value)-(min(datarolllollo$value)))),col="red")

print(ccf(scaledmean,scaledeeg))



#Tipping point for lollo=35s
#ggplot(datagazelollo,aes(x=timestamp,y=DiamRX))+geom_line()+stat_smooth()
ggplot(datameanlollo,aes(x=time,y=value))+geom_line()+stat_smooth()+geom_vline(xintercept=min(datameanlollo$time)+35*1000*1000)
ggplot(datasdlollo,aes(x=time,y=value))+geom_line()+stat_smooth()+geom_vline(xintercept=min(datasdlollo$time)+35*1000*1000)
ggplot(data.frame(eyeloadlollo),aes(x=seq_along(eyeloadlollo),y=eyeloadlollo))+geom_line()+stat_smooth()+geom_vline(xintercept=8)

ggplot(dataeeglollo,aes(x=Time..s.,y=Theta))+geom_line()+stat_smooth()+geom_vline(xintercept=39)
ggplot(dataeeglollo,aes(x=Time..s.,y=Theta*Attention/100))+geom_line()+stat_smooth()+geom_vline(xintercept=39)

#Alpha is not reliable, depends on the task!
#ggplot(dataeeglollo,aes(x=Time..s.,y=Low.Alpha))+geom_line()+stat_smooth()
#ggplot(dataeeglollo,aes(x=Time..s.,y=Low.Alpha*(101-Attention)/101))+geom_line()+stat_smooth()

############## LUKASZ #####################################################


datataskluk <- read.csv("task-test-lukasz.txt", header=F)
dataeegluk <- read.csv("record-Lukasz-[2015.05.15-11.38.02].csv", sep=";")
datagazeluk <- read.csv("gaze-test-lukasz.txt")

dataeegluk <- dataeegluk[dataeegluk$Time..s.>4,]
dataeegluk$Time..s. <- dataeegluk$Time..s. - min(dataeegluk$Time..s.)
datarollluk <- rollingMean(times = dataeegluk$Time..s., values = dataeegluk$Theta*dataeegluk$Attention, window = 2, slide = 1)

datarawluk <- rollingMean(times = dataeegluk$Time..s., values = dataeegluk$Electrode, window = 2, slide = 1)



datameanluk <- rollingMean(times = datagazeluk$timestamp, values = datagazeluk$DiamRX, window = 2*1000*1000, slide = 1*1000*1000 )
datasdluk <-  rollingSd(times = datagazeluk$timestamp, values = datagazeluk$DiamRX, window = 2*1000*1000, slide = 1*1000*1000 )
eyeloadluk <- as.numeric(datameanluk$value>median(datameanluk$value))+as.numeric(datasdluk$value>median(datasdluk$value))

scaledmean <- (datameanluk$value-min(datameanluk$value))/(max(datameanluk$value)-min(datameanluk$value))
scaledeeg <- (datarollluk$value-min(datarollluk$value))/(max(datarollluk$value)-(min(datarollluk$value)))
scaledraw <- (datarawluk$value-min(datarawluk$value))/(max(datarawluk$value)-(min(datarawluk$value)))



cor.test(scaledmean,scaledeeg[1:149])

ggplot(datameanluk,aes(x=seq_along(datameanluk$value),y=(datameanluk$value-min(datameanluk$value))/(max(datameanluk$value)-min(datameanluk$value))))+
    geom_line()+
    geom_line(data=datarollluk,aes(x=seq_along(datarollluk$value),y=(datarollluk$value-min(datarollluk$value))/(max(datarollluk$value)-(min(datarollluk$value)))),col="red")

print(ccf(scaledmean,scaledeeg))

print(ccf(scaledmean,scaledraw))


#Tipping point for luk=35s
#ggplot(datagazeluk,aes(x=timestamp,y=DiamRX))+geom_line()+stat_smooth()
ggplot(datameanluk,aes(x=time,y=value))+geom_line()+stat_smooth()+geom_vline(xintercept=min(datameanluk$time)+35*1000*1000)
ggplot(datasdluk,aes(x=time,y=value))+geom_line()+stat_smooth()+geom_vline(xintercept=min(datasdluk$time)+35*1000*1000)
ggplot(data.frame(eyeloadluk),aes(x=seq_along(eyeloadluk),y=eyeloadluk))+geom_line()+stat_smooth()+geom_vline(xintercept=8)

ggplot(dataeegluk,aes(x=Time..s.,y=Theta))+geom_line()+stat_smooth()+geom_vline(xintercept=39)
ggplot(dataeegluk,aes(x=Time..s.,y=Theta*Attention/100))+geom_line()+stat_smooth()+geom_vline(xintercept=39)

#Alpha is not reliable, depends on the task!
#ggplot(dataeegluk,aes(x=Time..s.,y=Low.Alpha))+geom_line()+stat_smooth()
#ggplot(dataeegluk,aes(x=Time..s.,y=Low.Alpha*(101-Attention)/101))+geom_line()+stat_smooth()
