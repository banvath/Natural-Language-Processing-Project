######## data preparation

library(stringr)
library(data.table)
library(readr)
library(sqldf)
library(dplyr)
library(readxl)
library(tidyr)

# data import
data_t <- unique(X000000_0) 
rm(X000000_0)
data_t <- as.data.frame(sapply(data_t,  function(x) {gsub("!","",x)}))
datai<-data_t

data_t <- read_delim("C:/Users/14702/OneDrive/Desktop/Data_Inventory/000023_0", 
                        "~", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)

data_t <- unique(data_t) 
data_t <- as.data.frame(sapply(data_t,  function(x) {gsub("!","",x)}))
datai<-rbind(datai, data_t)
datai = unique(datai)


####### sentiment data cleaning
data<-unique(data)
data<-na.omit(data)

data <- data[data$stars==1 | data$stars==2 | data$stars==3 | data$stars==4 | data$stars==5,]
data <- data[str_detect(data$fullcategory, '->'),]
data <- data[data$text !='Rating provided by a verified purchaser',]
data <- data[!str_detect(data$caption, 'Star Review'),]

# different online retails
data <- data.table(data)
data<-data[,retail:=ifelse(str_detect(data$weburl, 'amazon'),'amazon',
                           ifelse(str_detect(data$weburl, 'homedepot'),'homedepot',
                                  ifelse(str_detect(data$weburl, 'walmart'),'walmart','lowes')))]
nrow(data[data$retail=='lowes'])
nrow(data[data$retail=='amazon'])
nrow(data[data$retail=='homedepot'])
nrow(data[data$retail=='walmart'])

write.csv(data, file = "C:/Users/14702/OneDrive/Desktop/SBD_data.csv")

data[,sku_number:=.N,by=c('sku')]
sku_cnt <- unique(data[,c('sku','sku_number')])
sku_cnt <- sku_cnt[order(sku_cnt[,2],decreasing=T),]
hist(sku_cnt$sku_number)
sku_overcnt <- sku_cnt[sku_cnt$sku_number>100,]
hist(sku_overcnt$sku_number)

nrow(data[data$brand=='STANLEY'])
nrow(data[data$brand=='Stanley FatMax'])

# recognize if it's SBD product or competitors'
data<-data[, brand_binary:=ifelse(brand=='STANLEY'|brand=='Stanley FatMax'|brand=='DEWALT'|
                                    brand=='Black & Decker'|brand=='Craftsman'|
                                    brand=='IRWIN'|brand=='Porter Cable'|brand=='LENOX'|
                                    brand=='BOSTITCH'|brand=='PROTO', 1,0)]
nrow(data[data$brand_binary==1])
nrow(data[data$category=='Tape Measures'])

# Tape Measures
tape_measures <- data[category=='Tape Measures']
nrow(tape_measures[brand_binary==1])

write.csv(tape_measures, file = "C:/Users/14702/OneDrive/Desktop/tapemeasure_data.csv")

# find the models in competitor dataset
SBD<-data.table(unique(SBDCompetitorsProducts$`SBD Part #`))
setkeyv(SBD,c('sku'))
setkeyv(data,c('model'))
zdata<-data[SBD,nomatch=0]
zdata<-na.omit(zdata)

# find the competitor 
competitor<-data.table(unique(SBDCompetitorsProducts$`Cross Matching Data: Competitive Part Number`))
setkeyv(competitor,c('V1'))
setkeyv(data,c('model'))
ydata<-data[competitor,nomatch=0]
ydata<-na.omit(ydata)

# find corresponding SBD part
cp<-data.table(SBDCompetitorsProducts[,c(1,3,5)])
setkeyv(cp,c('`Cross Matching Data: Competitive Part Number`'))
setkeyv(data,c('model'))
xdata<-data[cp,nomatch=0]
xdata<-na.omit(xdata)

setkeyv(xdata,c('`SBD Part #`'))
setkeyv(data,c('model'))
wdata<-xdata[data,nomatch=0]
wdata<-wdata[category==i.category]

# average rating
wdata[,sbdavg := mean(as.numeric(as.character(i.stars))), by=c('`SBD Part #`')]
wdata[,cpavg := mean(as.numeric(as.character(stars))), by=c('model')]
# wdata[,sbdcmt := paste(unique(as.character(text))),by=c('`SBD Part #`')]

dt<-wdata[,c('model','brand','text','retail','sku_number','cpavg','i.sku','SBD.Part..','Strength.of.Match','i.text','i.retail','i.sku_number','sbdavg')]
dt<-unique(dt)

dt[,sbdcmt := paste(unique(as.character(dt[,dt$i.text]))),by=c('SBD.Part..')]
nrow(dt)

textfull=vector()
dt$cpcmt='NULL'
dt$sbdcmt='NULL'

for (i in 1:196122){
  for (j in 2:196122){
    if (dt[i,1] == dt[j,1] & dt[i,3]!=dt[j,3]){
      textfull <- paste(textfull,dt[j,3],sep = "| ")
    }
  }
  textfull <- paste(textfull,dt[i,3],sep = "| ")
  dt[i,c('cpcmt')] = textfull
  print(i)
  textfull=vector()
}

colnames(dt)<-c('cp_model','cp_brand','cp_cmt','cp_retail','cp_sku_number','cp_rate','sbd_sku','sbd_model','match','sbd_cmt','sbd_retail','sbd_sku_number','sbd_rate','cpcmt_all','sbdcmt_all')

write.csv(dt, file = "C:/Users/14702/OneDrive/Desktop/SBD_Competitors.csv",row.names=FALSE)

# category level 
data = data[complete.cases(category)]
data = data[-which(substr(category, start = 1, stop = 1)=='$'),] 

data[,SBDsum:= sum(brand_binary), by=c("category")]

data_sbd = data[SBDsum!=0,]

sbd_cmt = data_sbd[,c("model","brand","text","category","stars","retail","sku_number","brand_binary","SBDsum")]
write.csv(sbd_cmt, file = "C:/Users/14702/OneDrive/Desktop/sbd_cmt.csv",row.names=FALSE)

competitor = read.csv("C:/Users/14702/OneDrive/Desktop/SBD data manipulation/SBD_Competitors.csv")

# the categories are the same for the model?

data[,product := paste(model,brand)]
data = data[order(data[,c("product")]),]
data[,model_cate := .N, by=c("product","category")]
data[,max_mc := max(model_cate), by=c("product")]

data_unmatch = data[max_mc>model_cate]
data[,m_cate:= category,by=c("product")]
data_brand = data [max_mc == model_cate]

# competitor brands
cp<- read_excel("C:/Users/14702/OneDrive/Desktop/SBD data manipulation/SBDCompetitorsProducts.xlsx")
cps = unique(cp$Manufacturer)

data_brand = data_brand[brand %in% cps | brand_binary == 1 ]
data_brand = data_brand[order(data_brand[,c("product")]),]
data_brand = data_brand[order(data_brand[,c("model_cate")],decreasing = TRUE),]

write.csv(data_brand, file = "C:/Users/14702/OneDrive/Desktop/data_brand.csv",row.names=FALSE)

tool_data = data_brand[rootcategory=="Tools"]

write.csv(tool_data, file = "C:/Users/14702/OneDrive/Desktop/tool_data.csv",row.names=FALSE)


########## inventory data

names(data) <- c("sku", "model",	"name",	"brand",	"store_id",	"calendar_dt",	"inv_5(Inventory at 5 AM)",	
                 "inv_10(Inventory at 10 AM)",	"inv_15(Inventory at 3 PM)",	
                 "inv_20Inventory at 8 PM)",	"price",	"sales_units",	"web_url",	"store_name",	
                 "city", "state",	"zip",	"is_main",	"street",	"phone",	"type")

### omit similar lines

write.csv(data, file = "C:/Users/14702/OneDrive/Desktop/inventory_data.csv",row.names=FALSE)

# inventory data cleaning
data <- data[!str_detect(data$price, 'N'),]
data <- data[!str_detect(data$sales_units, 'N'),]
store_6378<-data[data$store_id=='6378']
x<-store_6378[store_6378$sku=='205334546']
x<-x[order(x$calendar_dt),]
sku_205334546 <- data[data$sku == "205334546"]

write.csv(sku_205334546, file = "C:/Users/14702/OneDrive/Desktop/sku_205334546.csv",row.names=FALSE)

####### sale calculation

inven_data = data[data$`inv_5(Inventory at 5 AM)`!=0 | data$`inv_10(Inventory at 10 AM)`!=0 | 
                    data$`inv_15(Inventory at 3 PM)`!=0 | data$`inv_20Inventory at 8 PM)`!=0,]

inven_data = as.data.table(inven_data)
# turn to numeric
inven_data[,sales_units:= as.numeric(as.character(sales_units))]
inven_data[,price:=as.numeric(as.character(price))]
           
inven_data[,sale:= ifelse(sales_units>0,sales_units,0)]
inven_data[,daily_sale:= sum(sale),by = c("model","calendar_dt")]
inven_data[,rev:= ifelse(sales_units>0,price*sale,0)]
inven_data[,daily_revenue:= sum(rev),by = c("model","calendar_dt")]

day_sale = inven_data[,c("model","brand","calendar_dt","daily_sale","daily_revenue")]
day_sale = na.omit(day_sale)
day_sale = unique(day_sale)

write.csv(day_sale, file = "C:/Users/14702/OneDrive/Desktop/day_sale.csv",row.names=FALSE)

#### hours level
inven_data[,inv_5:= as.numeric(as.character(`inv_5(Inventory at 5 AM)`))]
inven_data[,inv_10:= as.numeric(as.character(`inv_10(Inventory at 10 AM)`))]
inven_data[,inv_15:= as.numeric(as.character(`inv_15(Inventory at 3 PM)`))]
inven_data[,inv_20:= as.numeric(as.character(`inv_20Inventory at 8 PM)`))]

inven_data[,morningd:=inv_5-inv_10]
inven_data[,morning_demand:=ifelse(morningd>0,morningd,0)]
inven_data[,morning_supply:=ifelse(morningd<0,-morningd,0)]
inven_data[,m_demand:=sum(morning_demand),by = c("model","calendar_dt")]
inven_data[,m_supply:=sum(morning_supply),by = c("model","calendar_dt")]

inven_data[,afternoond:=inv_10-inv_15]
inven_data[,afternoon_demand:=ifelse(afternoond>0,afternoond,0)]
inven_data[,afternoon_supply:=ifelse(afternoond<0,-afternoond,0)]
inven_data[,a_demand:=sum(afternoon_demand),by = c("model","calendar_dt")]
inven_data[,a_supply:=sum(afternoon_supply),by = c("model","calendar_dt")]

inven_data[,eveningd:=inv_15-inv_20]
inven_data[,evening_demand:=ifelse(eveningd>0,eveningd,0)]
inven_data[,evening_supply:=ifelse(eveningd<0,-eveningd,0)]
inven_data[,e_demand:=sum(evening_demand),by = c("model","calendar_dt")]
inven_data[,e_supply:=sum(evening_supply),by = c("model","calendar_dt")]

hour_sale = inven_data[,c("model","brand","calendar_dt","daily_sale","daily_revenue","m_demand","a_demand","e_demand")]
hour_sale = na.omit(hour_sale)
hour_sale = unique(hour_sale)
colnames(hour_sale) = c("model","brand","calendar_dt","daily_sale","daily_revenue","10am","3pm","8pm")

hour_sale_tran = gather(data=hour_sale,key=day_time,value = sale,6:8,na.rm = F)
hour_sale_tran$day_time = paste(hour_sale_tran$calendar_dt,hour_sale_tran$day_time)

hour_sale_tran = hour_sale_tran[order(hour_sale_tran$model, hour_sale_tran$day_time),]

write.csv(hour_sale_tran, file = "C:/Users/14702/OneDrive/Desktop/hour_sale.csv",row.names=FALSE)

tool_data = read.csv("C:/Users/14702/OneDrive/Desktop/tool_data.csv")
hour_model = unique(hour_sale$model)


##### time series 
library(forecast)
library(forecasting)
tst = day_sale[model=="2680-20",c(3,4)]
tst = tst[order(tst[,1]),]
xtimeseries<-ts(tst)
xtimeseriesforecasts<-forecast(xtimeseries,h=20)
plot(xtimeseriesforecasts)
