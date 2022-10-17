#Regresión 2
#Cerveza
beer=read.csv("https://raw.githubusercontent.com/asalazaradams/curso_estad/main/beer.csv")
plot(beer$PB,beer$Q)
beermodel1=lm(Q~PB,data=beer)
summary(beermodel1)
abline(beermodel1)

plot(beer$I,beer$Q)
beermodel2=lm(Q~I,data=beer)
summary(beermodel2)

beermodel3=lm(Q~PB+I,data=beer)
summary(beermodel3)

beermodel4=lm(Q~PB+I+PL+PR,data=beer)
summary(beermodel4)

#Florida

plot(fl_crime$C~fl_crime$HS, cex=fl_crime$U*.01)
modelfl1=lm(C~HS, data=fl_crime)
abline(modelfl1)
modelfl1

modelfl2=lm(C~HS+U, data=fl_crime)
modelfl2

#Variables Dummy
house=read.csv("house-selling-price.csv")
plot(house$Price,house$Size)
model.house1= lm(Price~Size,data=house)
summary(model.house1)

plot(house$Price,house$Size)
model.house2= lm(Price~Size+New,data=house)
summary(model.house2)

-40230.867+116.132*1000+57736.283*1
-40230.867+116.132*1000+57736.283*0

#con "character" o "factor"
class(German_Credit$telephone)
plot(German_Credit$loan_amount~German_Credit$loan_duration_mo)
germanmodel1=lm(loan_amount~loan_duration_mo+telephone,data=German_Credit)
summary(germanmodel1)

#más de dos categorías
plot(German_Credit$loan_amount~German_Credit$loan_duration_mo)
germanmodel2=lm(loan_amount~loan_duration_mo+gender_status,data=German_Credit)
summary(germanmodel2)
table(German_Credit$gender_status)

germanmodel3=lm(loan_amount~loan_duration_mo+job_category,data=German_Credit)
summary(germanmodel3)
table(German_Credit$job_category)

#highly skilled
1957.92+138.169*12
1957.92+138.169*12-1884.032

germanmodel3=lm(loan_amount~loan_duration_mo+job_category,data=German_Credit)
summary(germanmodel3)


germanmodel4=lm(loan_amount~loan_duration_mo+home_ownership,data=German_Credit)
summary(germanmodel4)
class(German_Credit$home_ownership)
table(German_Credit$home_ownership)

988+142*duracion-828*own-611*rent
988+142*duracion-828*1
988+142*duracion-611*1
988+142*duracion


#relaciones no lineales
#Logarítmicas
#food_exp



plot(UN_data$GDP,UN_data$Fert)
un_model1=lm(Fert~GDP,data=UN_data)
summary(un_model1)
abline(un_model1)

UN_data$logGDP=log(UN_data$GDP)

plot(UN_data$logGDP,UN_data$Fert)
un_model2=lm(Fert~logGDP,data=UN_data)
summary(un_model2)
abline(un_model2)


#estimar la fertilidad para un GDP=5000
6.75481-.49473*log(5000)

UN_data$logFert=log(UN_data$Fert)

plot(UN_data$logGDP,UN_data$logFert)

un_model3=lm(logFert~logGDP,data=UN_data)
summary(un_model3)
abline(un_model3)
exp(2.38-.18137*log(5000))


#comparar las variables Life y GDP