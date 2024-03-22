data= read.csv("C:/Users/Lovis/Desktop/documents/courses/Spring2024/Linear Regression/Project/vgsales.csv", header = TRUE)  # change the file location
#data <- subset(data, EU_Sales < mean(EU_Sales)-3*sd(EU_Sales)&mean(EU_Sales)-3*sd(EU_Sales) ) #new_df <- subset(df, b != 7 & d != 38)

library(fastDummies)  # creating dummy for the qualitative variables

data <- dummy_cols(data, 
                   select_columns = "Genre")


# Summary statistics on the data
summary(data)  # 
#Critical store, Critic_count, User_count have missing values
boxplot(Other_Sales) # change for each predictor; This shoes data has outliers

# Select predictors

Global_Sales <- data["Global_Sales"]
EU_Sales<-data["EU_Sales"]
JP_Sales<-data["JP_Sales"]
Other_Sales<-data["Other_Sales"]
NA_Sales<-data["NA_Sales"]
Genre_Action<-data["Genre_Action"]
Genre_Adventure<-data["Genre_Adventure"]
Genre_Fighting<-data["Genre_Fighting"]
Genre_Misc<-data["Genre_Misc"]
Genre_Platform<-data["Genre_Platform"]
Genre_Racing<-data["Genre_Racing"]# Remove heading from predictors
#Genre_Role-Playin<-data["Genre_Role-Playing"]
Genre_Shooter<-data["Genre_Shooter"]
Genre_Simulation<-data["Genre_Simulation"]
Genre_Sports<-data["Genre_Sports"]
Genre_Strategy<-data["Genre_Strategy"]
Genre_Puzzle<-data["Genre_Puzzle"]
Genre_Action+Genre_Adventure+Genre_Fighting+Genre_Misc+Genre_Platform+Genre_Racing+Genre_Shooter+Genre_Shooter+Genre_Simulation+Genre_Sports+Genre_Strategy+Genre_Puzzle
#Genre_Role-Playin

Global_Sales=unlist(Global_Sales)
EU_Sales=unlist(EU_Sales)
JP_Sales=unlist(JP_Sales)
Other_Sales=unlist(Other_Sales)
NA_Sales=unlist(NA_Sales)
Genre_Action=unlist(Genre_Action)
Genre_Adventure=unlist(Genre_Action)
Genre_Fighting=unlist(Genre_Fighting)
Genre_Misc=unlist(Genre_Misc)
Genre_Platform=ulist(Genre_Platform)
Genre_Racing=unlist(Genre_Racing)
Genre_Shooter=unlist(Genre_Shooter)
Genre_Shooter=unlist(Genre_Shooter)
Genre_Simulation=unlist(Genre_Simulation)
Genre_Sports=unlist(Genre_Sports)
Genre_Strategy=unlist(Genre_Strategy)
Genre_Puzzle=unlist(Genre_Puzzle)


# First Linear Model with outliers
FirstModel<-lm(Global_Sales~EU_Sales+JP_Sales+Other_Sales+NA_Sales+Genre_Action+Genre_Adventure+Genre_Fighting+Genre_Misc+Genre_Platform+Genre_Racing+Genre_Shooter+Genre_Shooter+Genre_Simulation+Genre_Sports+Genre_Strategy+Genre_Puzzle)

summary(FirstModel) # Bad model as the R square is 1 

# Replace outliers with their mean values

#combine the data frame so it is easy to remove outliers

dd<-data.frame(Global_Sales,EU_Sales,JP_Sales,Other_Sales,NA_Sales)

# remove outliers above 3 standard deviations

dd[,-1] <- lapply(dd[,-1],
                  function(x) replace(x,abs(scale(x))>3,mean(x)))

#dd<-na.omit(dd)
summary(dd) # shows outliers removed. 

Secondmodel<-lm(dd$Global_Sales~dd$JP_Sales+dd$EU_Sales+dd$NA_Sales+dd$Other_Sales+I((JP_Sales-mean(JP_Sales))^2)+I((EU_Sales-mean(EU_Sales)))^2+I((NA_Sales-mean(NA_Sales))^2)+I((Other_Sales-mean(Other_Sales))^2))
summary(Secondmodel) # R square is .8031

Thirddmodel<-lm(dd$Global_Sales~dd$JP_Sales+dd$EU_Sales+dd$NA_Sales+dd$Other_Sales+Genre_Action+Genre_Adventure+Genre_Fighting+Genre_Misc+Genre_Platform+Genre_Racing+Genre_Shooter+Genre_Shooter+Genre_Simulation+Genre_Sports+Genre_Strategy+Genre_Puzzle+I((JP_Sales-mean(JP_Sales))^2)+I((EU_Sales-mean(EU_Sales)))^2+I((NA_Sales-mean(NA_Sales))^2)+I((Other_Sales-mean(Other_Sales))^2))

ForthModel<-lm(dd$Global_Sales~dd$JP_Sales+dd$EU_Sales+dd$NA_Sales+dd$Other_Sales+Genre_Platform+Genre_Shooter+I((JP_Sales-mean(JP_Sales))^2)+I((EU_Sales-mean(EU_Sales)))^2+I((NA_Sales-mean(NA_Sales))^2)+I((Other_Sales-mean(Other_Sales))^2))
summary(ForthModel)
# Some plots


# Some plots

summary(Thirddmodel)

plot(EU_Sales,Global_Sales)


# Do the errors have Normal distribution? 

# first model # plots is very funny
res=FirstModel$residuals 
qqnorm(res)
qqline(res)

# sencond model # much better but still needs improvement
res=Secondmodel$residuals 
qqnorm(res)
qqline(res)



library(MASS)
boxcox(FirstModel)

plot(dd$JP_Sales,res^2)# Japan sales may contain outliers
                      # Eu sales my contain outliers # remove outliers 28
                      # Other Sales contain outliers

stem(Other_Sales)

# correlations

cor(EU_Sales,JP_Sales) # 0.4350682
cor(EU_Sales,Other_Sales) # 0.722
cor(JP_Sales,Other_Sales) # 0.29


                    
install.packages("fastDummies")
