##############################
# The code is obtained modifying of another code made by Jethro Browell
# See his GitHub  https://github.com/jbrowell
#############################


require(data.table)
require(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

library("XML")
library("methods")
require(ggplot2)

if(!file.exists("data/TCR_StormReportsIndex.xml")){
  download.file(url = "https://www.nhc.noaa.gov/TCR_StormReportsIndex.xml",
                destfile = "data/TCR_StormReportsIndex.xml")
}

Hurricanes <- xmlToDataFrame("data/TCR_StormReportsIndex.xml")
Hurricanes <- as.data.table(Hurricanes)
Hurricanes[,dtm:=as.POSIXct(paste0(Year,"-01-01"),format="%Y-%m-%d")]

Hurricanes[,Hurricane:=FALSE]
Hurricanes[grep("Hurricane*",StormName),Hurricane:=TRUE]

setkey(Hurricanes,dtm)

ggplot(data=Hurricanes[Hurricane==T & Basin=="Atlantic" & Year<2023,],aes(x=Year)) +
  geom_bar() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# ENSO Index Data
# Source: https://psl.noaa.gov/enso/dashboard.html


nino4_mean <- data.table(read.delim("https://psl.noaa.gov/data/correlation/nina4.data",
                                    header = F,skip = 1,sep = "",nrows = 76,
                                    col.names = c("Year",stringr::str_pad(1:12,width = 2,pad = "0"))))

nino4_mean <- melt(nino4_mean,id.vars="Year",value.name = "nino4_mean")
nino4_mean[,dtm:=as.POSIXct(paste0(Year,variable,"01"),format="%YX%m%d")]

nino4_anom <- data.table(read.delim("https://psl.noaa.gov/data/correlation/nina4.anom.data",
                                    header = F,skip = 1,sep = "",nrows = 76,
                                    col.names = c("Year",stringr::str_pad(1:12,width = 2,pad = "0"))))

nino4_anom <- melt(nino4_anom,id.vars="Year",value.name = "nino4_anom")
nino4_anom[,dtm:=as.POSIXct(paste0(Year,variable,"01"),format="%YX%m%d")]

setkey(nino4_anom,dtm)

coeff = 10
mo6=60*60*24*365*0.5
Hurr_data = Hurricanes[dtm<"2023-01-01" & Basin=="Atlantic",.(Hurricanes=sum(.N)),by="dtm"]
ggplot(nino4_anom[nino4_anom!=-99.99 & dtm>="1958-01-01",],aes(x=dtm,y=nino4_anom))+
  geom_area(fill=rgb(0,0,1,0.3)) +
  geom_segment(data = Hurr_data,
               aes(x=dtm+mo6,xend=dtm+mo6,yend=0,y=Hurricanes/coeff)) +
  geom_point(data = Hurr_data,
             aes(x=dtm+mo6,y=Hurricanes/coeff)) +
  scale_y_continuous(name = "NINO-4 Index",
                     sec.axis = sec_axis(~.*coeff, name="Hurricanes")) +
  theme_bw()


## gmalss forecast
require(gamlss)

atlantic_hurricanes = Hurricanes[Hurricane==T & Basin=="Atlantic" & Year<2023,.N,by=dtm]
atlantic_hurricanes[,dtm:=as.numeric(format(dtm,"%Y"))]


model1 = gamlss(formula = N~1,
                sigma.formula = ~dtm,
                data=atlantic_hurricanes,
                family = NO())

wp(model1)



# choose a distribution on the real line 
# and save GAIC(k=c(2,4,6.4),  i.e. AIC, Chi-square and BIC.   
t1 <- chooseDist(model1,
                 type="counts") # "realplus"
# parallel="snow", ncpus=4)
# the GAIC's
t1
# the distributions which failed are with NA's 
# ordering according to  BIC
getOrder(t1,3)
model2 <- update(model1, family=names(getOrder(t1,3)[1]))
wp(model2)

plot(model2)

names(getOrder(t1,3)[1])


pre_params = predictAll(model2,newdata = data.table(dtm=2024))

dDIST <- match.fun(paste0("d",names(getOrder(t1,3)[1])))
pDIST <- match.fun(paste0("p",names(getOrder(t1,3)[1])))

n_hurricanes = 0:30
plot(x = n_hurricanes,
     y = dDIST(x = n_hurricanes,
               mu = pre_params$mu,
               # sigma = pre_params$sigma,
               # nu = pre_params$nu
               ),
     type="p")

hurricane_forecast = data.table(N=0:20,
                                P=c(dDIST(x = 0:19,
                                          mu = pre_params$mu,
                                          # sigma = pre_params$sigma,
                                          # nu = pre_params$nu
                                          ),
                                    1-pDIST(q = 19,
                                            mu = pre_params$mu,
                                            # sigma = pre_params$sigma,
                                            # nu = pre_params$nu
                                            )))

plot(hurricane_forecast,type="h")
points(hurricane_forecast)
