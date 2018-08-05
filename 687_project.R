#packages installed
library(data.table)
library(RgoogleMaps)
library(ggmap)
library(zipcode)
library(rworldmap)
data(zipcode)
----------------------------------------------------
#importing the datasets with selected fields
dat_feb14 <- fread("C:/Users/SWEENEY/Desktop/Syracuse/IST687/out-201402.csv", select = c(19,28,67,106:110,121,122,137:147,167:169,170,171,179,182,191,199:227,232))
dat_feb14_2<- data.frame(dat_feb14,stringsAsFactors = FALSE)
dat_feb14_2<- na.omit(dat_feb14_2)
feb14<-dat_feb14_2
feb14<-feb14[feb14$NPS_Type=="Promoter" | feb14$NPS_Type=="Detractor" | feb14$NPS_Type=="Passive",]
feb14$Month<-"February 2014"

dat_mar14 <- fread("C:/Users/SWEENEY/Desktop/Syracuse/IST687/out-201403.csv", select = c(19,28,67,106:110,121,122,137:147,167:169,170,171,179,182,191,199:227,232))
dat_mar14_2<- data.frame(dat_mar14,stringsAsFactors = FALSE)
dat_mar14_2<- na.omit(dat_mar14_2)
mar14<-dat_mar14_2
mar14<-mar14[mar14$NPS_Type=="Promoter" | mar14$NPS_Type=="Detractor" | mar14$NPS_Type=="Passive",]
mar14$Month<-"March 2014"

dat_apr14 <- fread("C:/Users/SWEENEY/Desktop/Syracuse/IST687/out-201404.csv", select = c(19,28,67,106:110,121,122,137:147,167:169,170,171,179,182,191,199:227,232))
dat_apr14_2<- data.frame(dat_apr14,stringsAsFactors = FALSE)
dat_apr14_2<- na.omit(dat_apr14_2)
apr14<-dat_apr14_2
apr14<-apr14[apr14$NPS_Type=="Promoter" | apr14$NPS_Type=="Detractor" | apr14$NPS_Type=="Passive",]
apr14$Month<-"April 2014"

#consolidating the impported datasets
quarter1<- rbind(feb14,mar14,apr14)
row.names(quarter1)<- NULL
c("LengthOfStay","PurposeOfVisit","HotelRevenue","ReservationStatus",",MemberStatus","LikelihoodToRecommend_SV","OverallSatisfaction_SV","Room_SV","HotelCondition_SV","Hospitality_SV","CheckInEase_SV","F.B_SV","City","State","US_Region","Zip Code","Country","NPS_Goal","HotelBrand","GlobalRegion","NPS_Type","Month")
View(quarter1)


quarter1$Postal.Code_PL<-clean.zipcodes(quarter1$Postal.Code_PL)
quarter1map<-merge(quarter1,zipcode,by.x ="Postal.Code_PL",by.y = "zip")
map<-get_map(location='world', zoom=4, maptype= "terrain", source='google', color='bw')
febdatamap<-ggmap(map)+geom_point(aes(x= longitude, y =latitude, color = Likelihood_Recommend_H), data = feb14, alpha = .5, na.rm = T)+ scale_color_gradient(low= "green", high = "red")
------- country analysis----------------------
map_world<- map_data("world")
map1<-ggplot() + geom_polygon(data = map_world, aes(x = long, y = lat, group = group))
map1 <- map1 + geom_map(map=map_world,map_id=febmap1$Country_PL)
map1<-map1 + expand_limits(x=us$long, y= us$lat)
map1<- map1 +geom_point(data=febmap1,aes(x=febmap1$longitude, y = febmap1$latitude, color = febmap1$Likelihood_Recommend_H))+ scale_color_gradient(low= "green", high = "red")

----------quarter1 for us----------
  quarter1_us<-quarter1map[quarter1map$Country_PL=="United States",]
----calculating mean for liklihood by us states----
mean_likelihood <- tapply(quarter1$Likelihood_Recommend_H,quarter1$State_PL,mean)
state <- rownames(mean_likelihood)
mean_likelihood_us<-data.frame(state,mean_likelihood)
mean_likelihood_us<-merge(mean_likelihood_us,zipcode,by.x='zip',by.y=)
---------------- us analysis according to mean likelihood----------------------
us<-map_data("state")  
map2<- ggplot(mean_likelihood_us, aes(map_id = state))
map<-get_map(location='united states', zoom=4, maptype= "terrain", source='google', color='bw')
map2 <- ggmap(map) + geom_point(aes(x=longitude, y = latitude, color = Likelihood_Recommend_H, size = 3), data = quarter1map)+ scale_color_gradient(low= "blue", high = "red")
map2 <- map2 + geom_point(data = mean_likelihood_us,aes(x = map1$longitude, y = map1$latitude, color=map1$median))
------------------ us analysis according to mean likelihood by zipcode------
febmap1_us<-febmap1[febmap1$Country_PL=="United States",]
febdatazip <- febmap1_us[c("Postal.Code_PL", "Likelihood_Recommend_H")]
febdatazip<-merge(febdatazip, zipcode, by.x='Postal.Code_PL', by.y='zip')
map3<- ggplot(quarter1map,aes(map_id = State_PL))
map3 <- map3 + geom_map(map=us, fill="black")
map3<- map3 + geom_point(data = quarter1map,aes(x = longitude, y = latitude, color=Likelihood_Recommend_H))
----------- heatmap----------------------
heatmap<- data.frame(quarter1[,c(7:9)])
ggplot(heatmap,aes(x=Age_Range_H,y=POV_H))+geom_tile(aes(fill=Likelihood_Recommend_H))+scale_fill_gradient(low="white",high="blue")
--------likelihood to recommend in ny---------------------
nymap <- get_map(location = 'ny', zoom = 8, color = 'bw')  
 map3<- ggmap(nymap) + geom_point(aes(x=longitude, y = latitude, color = Likelihood_Recommend_H, size = 3), data = quarter1map)+ scale_color_gradient(low= "blue", high = "red")
---------likelihood to recommend in sourthern and northern california---------------------
lamap<- get_map(location = 'la', zoom = 8, color = 'bw')   
map4<- ggmap(lamap) + geom_point(aes(x=longitude, y = latitude, color = Likelihood_Recommend_H, size = 3), data = quarter1map)+ scale_color_gradient(low= "blue", high = "red")
sf <- get_map(location = 'sf', zoom = 8, color = 'bw') 
map5<- ggmap(sf) + geom_point(aes(x=longitude, y = latitude, color = Likelihood_Recommend_H, size = 3), data = quarter1map)+ scale_color_gradient(low= "blue", high = "red")
---------------- analysis of survey on nps_type-------------
melt_df<-data.frame(quarter1map_us[,c(11:20,28)])
melt1<-melt(melt_df,id="NPS_Type")
ggplot(melt1 ,aes(x=value, y= variable , group=1))+geom_point(aes(shape=variable,size=4,color=NPS_Type))
p-d/sum(length(quarter1map_us$NPS_Type=="Promoter")+length(quarter1map_us$NPS_Type=="Detractor"))
-------------- home country guests analysis-----------------------
quarter1map_us_us<-quarter1map_us_us[1:10000,]
ggplot(quarter1map_us_us,aes(x=State_PL,y=NPS_Type))+geom_tile(aes(fill=Likelihood_Recommend_H))+scale_fill_gradient(low="white",high="blue")
---------------guest counrty analysis----------------
quarter1map_us_us<-quarter1map_us_us[1:25,]
ggplot(quarter1map_us_us,aes(x=State_PL,y=NPS_Type))+geom_tile(aes(fill=Likelihood_Recommend_H))+scale_fill_gradient(low="white",high="blue")
-----------------no of promoters per state in us----------------
 promoters<-sqldf('select count(quarter1map_us.NPS_Type),quarter1map_us.State_PL from quarter1map_us where quarter1map_us.NPS_Type=="Promoter" group by quarter1map_us.State_PL')
 View(promoters)
 promoters_df<-data.frame(promoters)
 promoters_df$State_PL<-reorder(promoters_df$State_PL,-promoters_df$count.quarter1map_us.NPS_Type.)
 ggplot(promoters_df,aes(State_PL,count.quarter1map_us.NPS_Type.))+
 geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
detractors<-sqldf('select count(quarter1map_us.NPS_Type),quarter1map_us.State_PL from quarter1map_us where quarter1map_us.NPS_Type=="Detractor" group by quarter1map_us.State_PL')
detractors_df<-data.frame(detractors)
detractors_df$State_PL<-reorder(detractors_df$State_PL,-detractors_df$count.quarter1map_us.NPS_Type.)
ggplot(detractors_df,aes(State_PL,count.quarter1map_us.NPS_Type.))+
geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
----- analysis for florida/ cali/texas---------------------
  ----------------- db for cal/fl/tx
  quarter1_us<-quarter1[quarter1$Country_PL=="United States",]
  quarter1_us_final_ca<-quarter1_us[quarter1_us$State_PL=="California",]
  quarter1_us_final_fl<-quarter1_us[quarter1_us$State_PL=="Florida",]
  
  quarter1_us_final_tx<-quarter1_us[quarter1_us$State_PL=="Texas",]
  
  --------------- a rulezz---
    dummy1<-quarter1_us_final_ca[,c(26:38,57,58)
    > dummy1$Brand_PL<-as.factor(as.factor(dummy1$Brand_PL))
  > dummy1$Region_PL<-as.factor(as.factor(dummy1$Region_PL))
  > dummy1$All.Suites_PL<-as.factor(as.factor(dummy1$All.Suites_PL))
  > dummy1$Bell.Staff_PL<-as.factor(as.factor(dummy1$Bell.Staff_PL))
  > dummy1$Boutique_PL<-as.factor(as.factor(dummy1$Boutique_PL))
  > dummy1$Business.Center_PL<-as.factor(as.factor(dummy1$Business.Center_PL))
  > dummy1$Casino_PL<-as.factor(as.factor(dummy1$Casino_PL))
  > dummy1$Conference_PL<-as.factor(as.factor(dummy1$Conference_PL))
  > dummy1$Convention_PL<-as.factor(as.factor(dummy1$Convention_PL))
  > dummy1$Dry.Cleaning_PL<-as.factor(as.factor(dummy1$Dry.Cleaning_PL))
  > dummy1$Elevators_PL<-as.factor(as.factor(dummy1$Elevators_PL))
  > dummy1$Fitness.Center_PL<-as.factor(as.factor(dummy1$Fitness.Center_PL))
  > dummy1$Fitness.Trainer_PL<-as.factor(as.factor(dummy1$Fitness.Trainer_PL))
  ruleset<-apriori(dummy1_hh,parameter = list(support= 0.6 ,confidence= 0.8))
  inspect(ruleset)
  plot(ruleset)
  [23278] {Region_PL=Americas,                                                                 
  Boutique_PL=N,                                                                      
  Business.Center_PL=Y,                                                               
  Casino_PL=N,                                                                        
  Conference_PL=N,                                                                    
  Convention_PL=N,                                                                    
  Dry.Cleaning_PL=Y,                                                                  
  Fitness.Center_PL=Y,                                                                
  Fitness.Trainer_PL=N} => {Brand_PL=Hyatt House} 0.7552395  1.0000000 1.0000000
  
  -----------------members in gold/platinum for hyatt house-------------
    > dim(p)
  [1] 38 16
  > p<-dummy1_hh_gold[dummy1_hh_gold$NPS_Type=="Promoters",]
  > dim(p)
  [1]  0 16
  > p<-dummy1_hh_gold[dummy1_hh_gold$NPS_Type=="Promoter",]
  > dim(p)
  [1] 395  16
  > p<-dummy1_hh_platinum[dummy1_hh_platinum$NPS_Type=="Promoter",]
  > d<-dummy1_hh_platinum[dummy1_hh_platinum$NPS_Type=="Detractor",]
  > dim(p)
  [1] 148  16
  > dim(d)
  [1] 12 16
  
  > p<-quarter1_us_final_ca[quarter1_us_final_ca$NPS_Type=="Promoter"& quarter1_us_final_ca$MEMBER_STATUS_R=="",]
  > dim(p)
  [1] 2445   58
  > d<-quarter1_us_final_ca[quarter1_us_final_ca$NPS_Type=="Detractor"& quarter1_us_final_ca$MEMBER_STATUS_R=="",]
  > dim(d)
  [1] 387  58
  > p<-quarter1_us_final_ca[quarter1_us_final_ca$NPS_Type=="Promoter"& quarter1_us_final_ca$MEMBER_STATUS_R=="Gold",]
  > d<-quarter1_us_final_ca[quarter1_us_final_ca$NPS_Type=="Detractor"& quarter1_us_final_ca$MEMBER_STATUS_R=="Gold",]
  > dim(d)
  [1] 299  58
  > dim(p)
  [1] 2013   58
  ------------------ revenue analysis------------
    ggplot(quarter1_us_final_ca ,aes(x=Gross_Rev_H, y=NPS_Type, group=1))+geom_point(aes(color=factor(NPS_Type)))
  sqldf('select sum(quarter1_us_final_ca.Gross_Rev_H) from quarter1_us_final_ca group by quarter1_us_final_ca.NPS_Type') 
  sum(quarter1_us_final_ca.Gross_Rev_H)
  1                              440533.1  detractor
  2                              944305.8   passive
  3                             2869721.4promoter
  ---------------- analyis of business purpose with no of promoters and detractors-----
    cal<-quarter1_us_final_ca[quarter1_us_final_ca$POV_H=="Business",]
  > table(cal$Brand_PL)
  
  Andaz   Grand Hyatt         Hyatt   Hyatt House   Hyatt Place Hyatt Regency 
  93           723           173           493           447          1683 
  Park Hyatt 
  125 
  > cal<-quarter1_us_final_ca[quarter1_us_final_ca$POV_H=="Business" & quarter1_us_final_ca$NPS_Type=="Promoter",]
  > table(cal$Brand_PL)
  
  Andaz   Grand Hyatt         Hyatt   Hyatt House   Hyatt Place Hyatt Regency 
  76           523           108           345           340          1033 
  Park Hyatt 
  92 
  > cal<-quarter1_us_final_ca[quarter1_us_final_ca$POV_H=="Business" & quarter1_us_final_ca$NPS_Type=="Detractor",]
  > table(cal$Brand_PL)
  
  Andaz   Grand Hyatt         Hyatt   Hyatt House   Hyatt Place Hyatt Regency 
  7            58            27            40            38           232 
  Park Hyatt 
  12 
  
  ------------------------------------------
    cal<-quarter1_us_final_ca[quarter1_us_final_ca$POV_H=="Business" & quarter1_us_final_ca$NPS_Type=="Promoter",]
  > View(cal)
  > cal<-quarter1_us_final_ca[quarter1_us_final_ca$POV_H=="Business" & quarter1_us_final_ca$NPS_Type=="Promoter" & quarter1_us_final_ca$Brand_PL=="Hyatt Regency",]
  > View(cal)
  > cal1<-cal[,c(3,6:8,10,11:20,59)]
  > View(cal1)
  > melt_df<-data.frame
  > melt_df<-data.frame(cal1[,c(3:16)])
  > melt1<- melt(melt_df,id="Age_Range_H")
  > View(melt1)
  >
    > melt1_df<-cal1[,c(3,7:16)]
  > melt1<- melt(melt_df,id="Age_Range_H")
  > View(melt1)
  > melt1<- melt(melt1_df,id="Age_Range_H"
                 + 
                   
                   > melt1<- melt(melt1_df,id="Age_Range_H")
                 > View(melt1)
                 > ggplot(melt1 ,aes(x=Age_Range_H, y= variable , group=1))+geom_point(aes(shape=value,size=4,color=NPS_Type))
                 Error in FUN(X[[i]], ...) : object 'NPS_Type' not found
                 > 
                   > ggplot(melt1,aes(x=Age_Range_H,y=value,group=variable,color=as.factor(variable)))+geom_line()
                 > melt1<-melt1[-c(9298:10330),]
                 > View(melt1)
                 > ggplot(melt1 ,aes(x=Age_Range_H, y= value , group=1))+geom_point(aes(shape=variable,size=4,color=NPS_Type))
                 Error in FUN(X[[i]], ...) : object 'NPS_Type' not found
                 > 
                   > ggplot(melt1 ,aes(x=Age_Range_H, y= value , group=1))+geom_point(aes(size=4,color=variable))
                 > 
                   > ggplot(melt1 ,aes(x=value, y= variable , group=1))+geom_point(aes(size=4,color=Age_Range_H))
                 -------------- analysis of hotel brand hyatt regency for pov business------------- 
                   > ggplot(melt1 ,aes(x=Age_Range_H, y= variable , group=1))+geom_point(aes(size=4,color=value))
                 -----------------gender analysis of california for all brands-------------
                   > ggplot(cal ,aes(x=Age_Range_H, y=NPS_Type, group=1))+geom_point(aes(shape=factor(Likelihood_Recommend_H),color=factor(Gender_H), size = 3))