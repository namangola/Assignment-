DLP2019<-read.csv("C:/Users/naman/Downloads/district_level_mapping_2019.csv", encoding = "UTF-8")
View(DLP2019)

#Changing Structures 
DLP2019$Division.Code<-as.numeric(DLP2019$Division.Code)
DLP2019$Major.Head.Code<-as.numeric(DLP2019$Major.Head.Code)
DLP2019$Treasury.Code<-as.factor(DLP2019$Treasury.Code)

#Removing Unnecessary columns
DLP2019 <-DLP2019[,-c(11:15,18)]

#For School Education 
library("dplyr")
DLP2019 %>% select(Division.Code,Division.Description,Grant.Number,Major.Head.Code,,Scheme.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code==2202,Scheme.Code<2202030010300)->Education19
View(Education19)
DLP2019$Scheme.Code> options("scipen"=100, "digits"=4)

#Calculation of share of expenditure on school education incurred by various department/industries
DLP2019 %>% select(Grant.Number,Grant.Head.Description,Major.Head.Code,,Scheme.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code==2202,Scheme.Code<2202030010300)->Ministries19
View(Ministries19)

##Code 2 Ministry
DLP2019 %>% select(Grant.Number,Grant.Head.Description,Major.Head.Code,,Scheme.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code==2202,Scheme.Code<2202030010300,Grant.Number==2)->Ministries219
View(Ministries219)
sum(Ministries219$Total.Expenditure.Upto.Month..November.)

##Code 48 Ministry 
DLP2019 %>% select(Grant.Number,Grant.Head.Description,Major.Head.Code,,Scheme.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code==2202,Scheme.Code<2202030010300,Grant.Number==48)->Ministries4819
View(Ministries4819)
sum(Ministries8319$Total.Expenditure.Upto.Month..November.)

##Code 71 Ministry 
DLP2019 %>% select(Grant.Number,Grant.Head.Description,Major.Head.Code,,Scheme.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code==2202,Scheme.Code<2202030010300,Grant.Number==71)->Ministries7119
View(Ministries7119)

##Code 72 Ministry 
DLP2019 %>% select(Grant.Number,Grant.Head.Description,Major.Head.Code,,Scheme.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code==2202,Scheme.Code<2202030010300,Grant.Number==72)->Ministries7219
View(Ministries7219)

##Code 81 Ministry 
DLP2019 %>% select(Grant.Number,Grant.Head.Description,Major.Head.Code,,Scheme.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code==2202,Scheme.Code<2202030010300,Grant.Number==81)->Ministries8119
View(Ministries8119)

##Code 83 Ministry 
DLP2019 %>% select(Grant.Number,Grant.Head.Description,Major.Head.Code,,Scheme.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code==2202,Scheme.Code<2202030010300,Grant.Number==83)->Ministries8319
View(Ministries8319)

#Estimating the share of capital Expenditure 
DLP2019 %>% select(Division.Code,Division.Description,Major.Head.Code,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code>=4047)->Capex
View(Capex)
sum(Capex$Total.Expenditure.Upto.Month..November.)

#Estimation of per capita expenditure on school education in each district 
##Introducing new column 
Education19 %>% filter(Division.Code==800)->AgraPerCapita
View(AgraPerCapita)
AgraPerCapita$Estimated.Population<-c(5119618)
sum(AgraPerCapita$Total.Expenditure.Upto.Month..November.)/5119618
summarize(AgraPerCapita$division.code)
print(Education19$Division.Code)
##Per Capita 
Education19 %>% filter(Division.Code==2200)-> Percapita2200
View(Percapita2200)
sum(Percapita2200$Total.Expenditure.Upto.Month..November.)/6898757
##
Education19 %>% filter(Division.Code==6300)-> Percapita6300
View(Percapita6300)
sum(Percapita6300$Total.Expenditure.Upto.Month..November.)/5145221


#To find out utilization of funds 
View(Capex)
Capex %>% filter(Division.Code==6300)->capex500019
View(capex500019)
sum(capex500019$Total.Expenditure.Upto.Month..November./sum(capex500019$Progressive.Allotment))*100

#Revenue Expenditure 
DLP2019 %>% select(Division.Code,Division.Description,Major.Head.Code,Major.Head.Description,Total.Expenditure.Upto.Month..November.,Progressive.Allotment,X..A.E) %>% filter(Major.Head.Code<=3606)->Revnueexp
View(Revnueexp)
Revnueexp %>% filter(Division.Code==2200)->rexp490019
View(rexp490019)
sum(rexp490019$Total.Expenditure.Upto.Month..November./sum(rexp490019$Progressive.Allotment))*100
