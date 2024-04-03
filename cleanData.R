# Clean the dataset
data= read.csv("vgsales.csv", header = TRUE)  
data.1 <- data
data.1$User_Score <- as.numeric(data.1$User_Score)
data.1$Year_of_Release <- as.numeric(data.1$Year_of_Release)
data.clean <- data.1[!is.na(data.1$User_Score) & !is.na(data.1$Critic_Score) & data.1$Rating!= "" & !is.na(data.1$Year_of_Release), ]

summary(data.clean)
