summary(event3)

bounds.pH = c(6.3,7.9)
bounds.EC = c(940,3000)
bounds.TurbA = c(50,100)
bounds.TurbS = c(30, 122)
bounds.TempC = c(23.1,23.7)
bounds.TempA = c(20,24.6)

bounds.pH2 = c(6.3,7.9)
bounds.EC2 = c(940,3000)
bounds.TurbA2 = c(50,100)
bounds.TurbS2 = c(30, 122)
bounds.TempC2 = c(23.1,23.7)


                   

summary(event13)

Date               Time               TempC             pH            Redox            TurbS              DO             Cond     
Length:11741       Length:11741       Min.   :22.37   Min.   :6.891   Min.   :0.1140   Min.   :-3.300   Min.   :2.660   Min.   :1042  
Class :character   Class :character   1st Qu.:22.93   1st Qu.:6.994   1st Qu.:0.2080   1st Qu.:-1.900   1st Qu.:4.820   1st Qu.:1074  
Mode  :character   Mode  :character   Median :23.51   Median :7.022   Median :0.2130   Median :-1.600   Median :5.470   Median :1092  
                                      Mean   :23.74   Mean   :7.034   Mean   :0.2133   Mean   :-1.238   Mean   :5.228   Mean   :1093  
                                      3rd Qu.:24.40   3rd Qu.:7.078   3rd Qu.:0.2210   3rd Qu.:-1.000   3rd Qu.:5.860   3rd Qu.:1115  
                                      Max.   :26.45   Max.   :7.187   Max.   :0.2300   Max.   :44.900   Max.   :6.450   Max.   :1138  



bounds.pH = c(6.3,7)
bounds.EC = c(1000,1300)
bounds.TurbA = c(50,100)
bounds.TurbS = c(0, 5)
bounds.TempC = c(22,25)
bounds.TempA = c(20,24.6)

bounds.pH2 = c(6.3,7.9)
bounds.EC2 = c(940,3000)
bounds.TurbA2 = c(50,100)
bounds.TurbS2 = c(30, 122)
bounds.TempC2 = c(23.1,23.7)



#event1


bounds.pH = c(6.3,8.3)
bounds.EC = c(0,2)
bounds.TurbA = c(50,250)
bounds.TurbS = c(50,250)
bounds.TempC = c(21,22)
bounds.TempA = c(20,24.6)


sum(dataset$pH$pH[1:10] < 7)

#event10

bounds.pH2 = c(6.3,8.1)
bounds.EC2 = c(940,3000)
bounds.TurbA2 = c(50,250)
bounds.TurbS2 = c(30,600)
bounds.TempC2 = c(19,27)
bounds.Redox2 = c(-0.22,-0.36)

probs.pH = c(.025,.975,.025,.975)
probs.EC = c(.025,.975,.025,.975)
probs.TurbS = c(.025,.975,.025,.975)
probs.TempC =  c(.025,.975,.025,.975)
probs.DisOxy = c(.025,.975,.025,.975)
probs.Redox =  c(.025,.975,.025,.975)



ph.second.HIGH<- 8.9
ph.second.LOW<- 6.1

#Event 4 - Industrial

bounds.pH2 = c(6.3,8.1)
bounds.EC2 = c(9400,3000)
bounds.TurbA2 = (50,250)
bounds.TurbS2 = c(30,300)
bounds.TempC2 = c(21,23)
bounds.Redox2 = c(-0.22,-0.36)


#EVENT 5 - Rainfall


bounds.pH2 = c(6.3,8.1)
bounds.EC2 = c(.1100,.3000)
bounds.TurbA2 = c(50,250)
bounds.TurbS2 = c(30,300)
bounds.TempC2 = c(22.5,23)
bounds.Redox2 = c(-0.22,-0.36)

#EVENT 6 - Industrial


bounds.pH2 = c(6.3,8.1)
bounds.EC2 = c(1.100,2.400)
bounds.TurbA2 = c(50,250)
bounds.TurbS2 = c(30,200)
bounds.TempC2 = c(24,25)
bounds.Redox2 = c(-0.22,-0.36)

#EVENT 7 - Rainfall


bounds.pH2 = c(6.3,8.1)
bounds.EC2 = c(1.100,3.000)
bounds.TurbA2 = c(50,250)
bounds.TurbS2 = c(30,250)
bounds.TempC2 = c(24.3,25.5)
bounds.Redox2 = c(-0.22,-0.36)


#EVENT 8 -- Industrial - ph & cond
#EVENT - 9 -- same event ??


bounds.pH2 = c(6.0,8.3)
bounds.EC2 = c(1.100,3)
bounds.TurbA2 = c(50,250)
bounds.TurbS2 = c(30,200)
bounds.TempC2 = c(24,25)
bounds.Redox2 = c(-0.22,-0.36)


#EVENT - 11 -- industrial ??


bounds.pH2 = c(6.3,8.3)
bounds.E2C = c(0,2)
bounds.TurbA2 = c(50,250)
bounds.TurbS2 = c(50,250)
bounds.TempC2 = c(23.5,24.8)
bounds.TempA2 = c(23.5,24.8)


