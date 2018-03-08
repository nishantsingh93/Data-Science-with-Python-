# Importing the dataset
house= read.csv('kc_house_data.csv',TRUE,",")



df<-subset(house,select=-c(id))
names(df)

#appropriate function to extract the year and month into separate variables
df$year=substr(df$date,0,4)

df$month=substr(df$date,5,6)


df<-subset(df,select=-c(date))

null_model<-lm(price~1,data=df)
summary(null_model)

full_model<-lm(price~.,data=df)
summary(full_model)

step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
#It makes sense that sqft_lot is an important predictor for the price of a home, but can you explain why lat (East-West) 
#is such a high predictor for the model? Conversely, why do you think long is such a low predictor?

#ans.......???


#linearmodel
lm1<-lm(price~ (sqft_living + lat + view + grade + yr_built + waterfront + 
                   bedrooms + bathrooms + zipcode + long),data=df)
summary(lm1)
#Take your top 10 predictors and create a linear model. Is this a “good” R2? Why?

#ans:   ?

#Convert the variable zipcode from numeric to a factor variable 
df$zipcode<-as.factor(df$zipcode)


null_model<-lm(price~1,data=df)
summary(null_model)

full_model<-lm(price~.,data=df)
summary(full_model)

step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

#linearmodel2
lm2<-lm(price~ (sqft_living + zipcode + waterfront + grade + view + yr_built + 
                  bedrooms + sqft_above + floors + condition),data=df)
summary(lm2)



#heteroskedesticity
lmtest::bptest(lm2)
yes 

#plots
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lm2)



#Convert the variable zipcode from numeric to a factor variable 
#df$year<-as.factor(df$year)
#df$month<-as.factor(df$month)
#df$floors<-as.factor(df$floors)
df$grade<-as.factor(df$grade)

#df$condition<-as.factor(df$condition)


null_model<-lm(price~1,data=df)
summary(null_model)

full_model<-lm(price~.,data=df)
summary(full_model)

step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

#linearmodel3
lm3<-lm(price~ ( grade + zipcode + sqft_living + waterfront + 
                   view + condition + year + yr_renovated + sqft_above + floors),data=df)
summary(lm3)


df$bedrooms<-as.factor(df$bedrooms)


null_model<-lm(price~1,data=df)
summary(null_model)

full_model<-lm(price~.,data=df)
summary(full_model)

step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")


library(MASS)
ind <- sapply(df, is.numeric)
df[ind] <- lapply(df[ind], scale)



str(df)


#linearmodel3
lm4<-lm(price~ ( grade + zipcode + sqft_living + waterfront + 
                   view +bedrooms:bathrooms + condition + year + yr_renovated + sqft_above),data=df)
summary(lm4)


# centering with 'scale()'
data = scale(Auto[,-c(9,10)])
