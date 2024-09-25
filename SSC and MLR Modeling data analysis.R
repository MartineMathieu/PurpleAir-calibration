
#########################################
#########################################
## Martine Mathieu-Campbell

#Calibration of Low-Cost Particulate Matter Sensors PurpleAir: 
# Model Development for Air Quality under High Relative Humidity Conditions
#########################################
######################################### 

######################################### MODELING PROCESS

# Libraries
library(dplyr)
library(Metrics)
library(caret)
library(randomForest)
library(clustvarsel)
library(BMA)
library(cluster)
library(NbClust)
library(factoextra)

# Project directory 
dir_data <- paste0("~DATA/")

setwd(paste0(dir_data))

data_PA <- read.csv("r0_5km_processeddata_PA_AQS_Jan2021_Aug2023_Final.csv", header = TRUE)
modeling_data <- data_PA 
head(modeling_data)
names(modeling_data)
nrow(modeling_data)

# Evaluating dataset
mean(modeling_data$conc)
sd(modeling_data$conc)
sd(modeling_data_90$conc)
mean(modeling_data$A_B_Avg)
sd(modeling_data$A_B_Avg)
sd(modeling_data_90$A_B_Avg)
modeling_data$dev_AQS_PA <-  abs(modeling_data$conc - modeling_data$A_B_Avg)
mean(modeling_data$dev_AQS_PA)
rmse(modeling_data$conc, modeling_data$A_B_Avg)
mae(modeling_data$conc, modeling_data$A_B_Avg)
R2(modeling_data$conc, modeling_data$A_B_Avg) 
nb_PAsensors <- unique(modeling_data$sensor_index)
sensorPACount <- modeling_data %>% group_by(sensor_index) %>%tally()


#########################################
######################################### MLR

# USING the entire dataset
###########################

mod1 <-lm(modeling_data$conc~modeling_data$A_B_Avg)
summary(mod1)
rmse(modeling_data$conc, mod1$fitted.values)
mae(modeling_data$conc, mod1$fitted.values)
R2(modeling_data$conc, mod1$fitted.values) 
cor.test(modeling_data$conc, mod1$fitted.values,
         method = "pearson")

mod2 <-lm(modeling_data$conc~modeling_data$A_B_Avg + modeling_data$humidity_a)
summary(mod2)
rmse(modeling_data$conc, mod2$fitted.values)
mae(modeling_data$conc, mod2$fitted.values)
R2(modeling_data$conc, mod2$fitted.values) 
cor.test(modeling_data$conc, mod2$fitted.values,
         method = "pearson")

mod3 <-lm(modeling_data$conc~modeling_data$A_B_Avg + modeling_data$temp_Celsius)
summary(mod3)
rmse(modeling_data$conc, mod3$fitted.values)
mae(modeling_data$conc, mod3$fitted.values)
R2(modeling_data$conc, mod3$fitted.values) 
cor.test(modeling_data$conc, mod3$fitted.values,
         method = "pearson")
mean(mod3$fitted.values)
sd(mod3$fitted.values)


mod4 <-lm(modeling_data$conc~modeling_data$A_B_Avg + modeling_data$humidity_a + modeling_data$temp_Celsius)
summary(mod4)
rmse(modeling_data$conc, mod4$fitted.values)
mae(modeling_data$conc, mod4$fitted.values)
R2(modeling_data$conc, mod4$fitted.values) 
cor.test(modeling_data$conc, mod4$fitted.values,
         method = "pearson")
mean(mod4$fitted.values)
sd(mod4$fitted.values)

# APPLYING Barkjohn model

modelBj <- 5.72+0.524*modeling_data$A_B_Avg-0.0852*modeling_data$humidity_a
rmse(modeling_data$conc, modelBj)
mae(modeling_data$conc, modelBj)
R2(modeling_data$conc, modelBj) 
cor.test(modeling_data$conc, modelBj,
         method = "pearson")
mean(modelBj)
sd(modelBj)

# ADDING Model4 and Model Bj predicted values to our dataset
###########################
modeling_data_pred <- modeling_data
modeling_data_pred$mod4_predict <- mod4$fitted.values
modeling_data_pred$modBj_predict <- modelBj

modeling_data_pred$predicted2 <- predict.lm(mod4, modeling_data_pred)
mod4_prediction <- predict(mod4, modeling_data_pred)
mod4_performance <- data.frame( R2 = R2(mod4_prediction, modeling_data_pred$conc),
                                   RMSE = RMSE(mod4_prediction, modeling_data_pred$conc),
                                   MAE = MAE(mod4_prediction, modeling_data_pred$conc))

mod4_performance

to_sites_colfax_clean$pred_gam <- gam_all_data$fitted.values #predict.lm(gam_all_data, to_sites_colfax_clean)
to_sites_colfax_clean$pred_gam2 <- predict.gam(gam_all_data, to_sites_colfax_clean)
head(to_sites_colfax_clean)
gam_prediction <- predict(gam_all_data, to_sites_colfax_clean)
gam_performance_mod <- data.frame( R2 = R2(gam_prediction, to_sites_colfax_clean$corrPA),
                                   RMSE = RMSE(gam_prediction, to_sites_colfax_clean$corrPA),
                                   MAE = MAE(gam_prediction, to_sites_colfax_clean$corrPA))

gam_performance_mod


# APPLYING Cross validation
###########################
ctrl<-trainControl(method="LGOCV", number=25, p=0.80)

mod1LGOCV<-train(conc~A_B_Avg,data=modeling_data,method="glm",trControl=ctrl)
summary(mod1LGOCV)
print(mod1LGOCV)
cor.test(modeling_data$conc, mod1LGOCV$finalModel$fitted.values,
         method = "pearson")


mod2LGOCV<-train(conc~A_B_Avg+humidity_a,data=modeling_data,method="glm",trControl=ctrl)
summary(mod2LGOCV)
print(mod2LGOCV)
cor.test(modeling_data$conc, mod2LGOCV$finalModel$fitted.values,
         method = "pearson")

mean(mod2LGOCV$finalModel$fitted.values)
sd(mod2LGOCV$finalModel$fitted.values)


mod3LGOCV<-train(conc~A_B_Avg+temp_Celsius,data=modeling_data,method="glm",trControl=ctrl)
summary(mod3LGOCV)
print(mod3LGOCV)
cor.test(modeling_data$conc, mod3LGOCV$finalModel$fitted.values,
         method = "pearson")
mean(mod3LGOCV$finalModel$fitted.values)
sd(mod3LGOCV$finalModel$fitted.values)


mod4LGOCV<-train(conc~A_B_Avg+humidity_a+temp_Celsius,data=modeling_data,method="glm",trControl=ctrl)
summary(mod4LGOCV)
print(mod4LGOCV)
cor.test(modeling_data$conc, mod4LGOCV$finalModel$fitted.values,
         method = "pearson")
mean(mod4LGOCV$finalModel$fitted.values)
sd(mod4LGOCV$finalModel$fitted.values)


# Sensitivity analysis - APPLYING HOURLY DATA TO DAILY 90 DATA
##########################
data_PA_DAILY <- read.csv("DAILY90_r0_5km_processeddata_PA_AQS_Jan2021_Aug2023_Final.csv", header = TRUE)
modeling_data_90 <- data_PA_DAILY

# Evaluating dataset
mean(fullAQS_and_PA_data_DAILY_90_clean$conc)
sd(fullAQS_and_PA_data_DAILY_90_clean$conc)
mean(fullAQS_and_PA_data_DAILY_90_clean$A_B_Avg)
sd(fullAQS_and_PA_data_DAILY_90_clean$A_B_Avg)

# Modeling
model_1<- 3.6667550+0.4053418* modeling_data_90$A_B_Avg

rmse(modeling_data_90$conc, model_1)
mae(modeling_data_90$conc, model_1)
R2(modeling_data_90$conc, model_1)
cor.test(modeling_data_90$conc, model_1,
         method = "pearson")

model_2 <- 6.3384228+0.4143437*modeling_data_90$A_B_Avg-0.0506037*modeling_data_90$humidity_a

rmse(modeling_data_90$conc, model_2)
mae(modeling_data_90$conc, model_2)
R2(modeling_data_90$conc, model_2)
cor.test(modeling_data_90$conc, model_2,
         method = "pearson")

model_3 <- 1.7642336+0.4109897*modeling_data_90$A_B_Avg+0.0847196*modeling_data_90$temp_Celsius

model_3 <- 1.76+0.41*modeling_data_90$A_B_Avg+0.08*modeling_data_90$temp_Celsius

rmse(modeling_data_90$conc, model_3)
mae(modeling_data_90$conc, model_3)
R2(modeling_data_90$conc, model_3)
cor.test(modeling_data_90$conc, model_3,
         method = "pearson")

model_4 <- 4.3295358+0.4182906*modeling_data_90$A_B_Avg-0.0445768*modeling_data_90$humidity_a +
  +   0.0752867*modeling_data_90$temp_Celsius

model_4 <- 4.33+0.42*modeling_data_90$A_B_Avg-0.04*modeling_data_90$humidity_a +
  +   0.07*modeling_data_90$temp_Celsius

rmse(modeling_data_90$conc, model_4)
mae(modeling_data_90$conc, model_4)
R2(modeling_data_90$conc, model_4)
cor.test(modeling_data_90$conc, model_4,
         method = "pearson")

modelBj_90 <- 5.72+0.524*modeling_data_90$A_B_Avg-0.0852*modeling_data_90$humidity_a
modeling_data_90$modBj <- 5.72+0.524*modeling_data_90$A_B_Avg-0.0852*modeling_data_90$humidity_a

rmse(modeling_data_90$conc, modelBj_90)
mae(modeling_data_90$conc, modelBj_90)
R2(modeling_data_90$conc, modelBj_90)
cor.test(modeling_data_90$conc, modelBj_90,
         method = "pearson")

# GRAPH predicted concentrations - Model 4 and Model Bj
##########################

modeling_data_90$mod4 <- 4.3295358 +0.4182906*modeling_data_90$A_B_Avg-00.0445768*modeling_data_90$humidity_a +
  0.0752867*modeling_data_90$temp_Celsius

range(modeling_data_90_graph$mod4)
ggplot(modeling_data_90, aes(x = mod4, y = conc, color = humidity_a)) +
  geom_jitter() +
  scale_fill_gradient(low = "#E2F3F5",  high = "blue3", na.value = "grey50", aesthetics = "color") +
  theme_bw() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")+
  xlim(0, 60) + ylim(0,60)

modeling_data_90$modBj <- 5.72+0.524*modeling_data_90$A_B_Avg-0.0852*modeling_data_90$humidity_a

range(modeling_data_90_graph$modBj)
ggplot(modeling_data_90, aes(x = modBj, y = conc, color = humidity_a)) +
  geom_jitter() +
  scale_fill_gradient(low = "#EFF396" ,high = "green4", na.value = "grey50", aesthetics = "color") +
  theme_bw() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")+
  xlim(0, 60) + ylim(0,60)


# USING NOAA met DATA
###########################

modeling_data_NOAA <- modeling_data%>% drop_na(RH)
nrow(modeling_data_NOAA)

mod4_NOAA <-lm(modeling_data_NOAA$conc~modeling_data_NOAA$A_B_Avg + modeling_data_NOAA$RH + modeling_data_NOAA$air_temp)
summary(mod4_NOAA)
rmse(modeling_data_NOAA$conc, mod4_NOAA$fitted.values)
mae(modeling_data_nonNA$conc, mod4_NOAA$fitted.values)
R2(modeling_data_NOAA$conc, mod4_NOAA$fitted.values) 
cor.test(modeling_data_NOAA$conc, mod4_NOAA$fitted.values,
         method = "pearson")


#########################################
######################################### SSC

# FIND best variables

modeling_data_sub <- subset(modeling_data, select = c(humidity_a,temp_Celsius))
modeling_data_sub_20 <- modeling_data_sub %>% sample_frac(.2)
PA_BR_cluster3 <- clustvarsel(modeling_data_sub_20, G = 1:6)
PA_BR_cluster3

# DETERMINE number of clusters

modeling_data_sub <- select(modeling_data_sub, -3)
NbClust(modeling_data_sub, method = 'complete', index = 'all')$Best.nc
modeling_data_sub_scaled <- modeling_data_sub %>% scale() %>% as.data.frame()
NbClust(modeling_data_sub_scaled, method = 'complete', index = 'all')$Best.nc #scaled

# Silhouette method
fviz_nbclust(modeling_data_sub, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Unsupervised clustering
modeling_data_sub_Kmean <- kmeans(modeling_data_sub,2, iter.max = 8)
modeling_data_sub_Kmean$iter
modeling_data_sub_Kmean$size
modeling_data_sub_Kmean$centers

# Plot clusters
fviz_cluster(modeling_data_sub_Kmean, data = modeling_data_sub, 
             palette = c("#2E9FDF", "#40AFBB"
             ), ellipse.type = "convex", geom = "point", 
             ggtheme = theme_minimal()
)


# Supervised clustering

orderCL=order(modeling_data_sub_Kmean$cluster)
PA_data_sub <- subset(modeling_data, select = c(A_B_Avg, 
                                                humidity_a,temp_Celsius, dewPoint, conc))

grpPA_BR_df <- data.frame(
  modeling_data_sub_Kmean$cluster[orderCL],
  PA_data_sub$A_B_Avg[orderCL],
  PA_data_sub$humidity_a[orderCL],PA_data_sub$temp_Celsius[orderCL],PA_data_sub$dewPoint[orderCL] 
  ,PA_data_sub$conc[orderCL])

# CL 1
grpPA_BR_df1 <- subset(grpPA_BR_df,modeling_data_sub_Kmean.cluster.orderCL.==1)
range(grpPA_BR_df1$PA_data_sub.humidity_a.orderCL.)
range(grpPA_BR_df1$PA_data_sub.temp_Celsius.orderCL.)
range(grpPA_BR_df1$PA_data_sub.dewPoint.orderCL.)

# CL 2
grpPA_BR_df2 <- subset(grpPA_BR_df,modeling_data_sub_Kmean.cluster.orderCL.==2)
range(grpPA_BR_df2$PA_data_sub.humidity_a.orderCL.)
range(grpPA_BR_df2$PA_data_sub.temp_Celsius.orderCL.)
range(grpPA_BR_df2$PA_data_sub.dewPoint.orderCL.)

grpPA_df1 <- subset(PA_data_sub,humidity_a<=50)
grpPA_df1$level <- "a"
grpPA_df2 <- subset(PA_data_sub,humidity_a>50)
grpPA_df2$level <- "b"

# MLR for each cluster and estimation of performance metrics

grp1_mod <-lm(grpPA_df1$conc~grpPA_df1$A_B_Avg +
                grpPA_df1$humidity_a + grpPA_df1$temp_Celsius)
summary(grp1_mod)

grpPA_df1$PApredict <- predict.lm(grp1_mod, grpPA_df1)
grp1_prediction <- predict(grp1_mod, grpPA_df1)
grp1_performance_mod <- data.frame( R2 = R2(grp1_prediction, grpPA_df1$conc),
                                    RMSE = RMSE(grp1_prediction, grpPA_df1$conc),
                                    MAE = MAE(grp1_prediction, grpPA_df1$conc))

grp1_performance_mod


grp1_cor_PA_HV <- cor.test(grpPA_df1$conc, grpPA_df1$PApredict, 
                          method = "pearson")
grp1_cor_PA_HV

mean(grpPA_df1$PApredict)
sd(grpPA_df1$PApredict)

#####
grp2_mod <-lm(grpPA_df2$conc~grpPA_df2$A_B_Avg +
                grpPA_df2$humidity_a + grpPA_df2$temp_Celsius)

summary(grp2_mod)

grpPA_df2$PApredict <- predict.glm(grp2_mod, grpPA_df2)
grp2_prediction <- predict(grp2_mod, grpPA_df2)

grp2_performance_mod <- data.frame( R2 = R2(grp2_prediction, grpPA_df2$conc),
                                    RMSE = RMSE(grp2_prediction, grpPA_df2$conc),
                                    MAE = MAE(grp2_prediction, grpPA_df2$conc))

grp2_performance_mod

grp2_cor_PA_HV <- cor.test(grpPA_df2$conc, grpPA_df2$PApredict, 
                           method = "pearson")
grp2_cor_PA_HV

mean(grpPA_df2$PApredict)
sd(grpPA_df2$PApredict)

#Cross validation - SSC
###########################

ctrl<-trainControl(method="LGOCV", p=0.80, number = 25)
mod1LGOCV<-train(conc~A_B_Avg+humidity_a+temp_Celsius,data=grpPA_df1,method="glm",trControl=ctrl)
summary(mod1LGOCV)
print(mod1LGOCV)
cor.test(grpPA_df1$conc, mod1LGOCV$finalModel$fitted.values,
         method = "pearson")

mod2LGOCV<-train(conc~A_B_Avg+humidity_a+temp_Celsius,data=grpPA_df2,method="glm",trControl=ctrl)
summary(mod2LGOCV)
print(mod2LGOCV)
cor.test(grpPA_df2$conc, mod2LGOCV$finalModel$fitted.values,
         method = "pearson")


# Sensitivity analysis - APPLYING HOURLY DATA TO DAILY 90 DATA
##########################

# GRP 1
grpPA_df1_DAILY <- subset(modeling_data_90,humidity_a<=50)
grpPA_df1_DAILY$level <- "a"

grpPA_df1_DAILY$PApredict <- 2.738732+ 0.425834*grpPA_df1_DAILY$A_B_Avg-0.008944*grpPA_df1_DAILY$humidity_a + 0.079210*grpPA_df1_DAILY$temp_Celsius

# computing model performance metrics
data.frame( R2 = R2(grpPA_df1_DAILY$conc, grpPA_df1_DAILY$PApredict),
            RMSE = RMSE(grpPA_df1_DAILY$conc, grpPA_df1_DAILY$PApredict),
            MAE = MAE(grpPA_df1_DAILY$conc, grpPA_df1_DAILY$PApredict))
cor.test(grpPA_df1_DAILY$conc, grpPA_df1_DAILY$PApredict, 
         method = "pearson")

# GRP 2
grpPA_df2_DAILY <- subset(modeling_data_90,humidity_a>50) 
grpPA_df2_DAILY$level <- "b"

grpPA_df2_DAILY$PApredict <- 7.230374 + 0.412683*grpPA_df2_DAILY$A_B_Avg-0.085278*grpPA_df2_DAILY$humidity_a + 0.070655*grpPA_df2_DAILY$temp_Celsius

# computing model performance metrics
data.frame( R2 = R2(grpPA_df2_DAILY$conc, grpPA_df2_DAILY$PApredict),
            RMSE = RMSE(grpPA_df2_DAILY$conc, grpPA_df2_DAILY$PApredict),
            MAE = MAE(grpPA_df2_DAILY$conc, grpPA_df2_DAILY$PApredict))
cor.test(grpPA_df2_DAILY$conc, grpPA_df2_DAILY$PApredict, 
         method = "pearson")

# GRAPH predicted concentrations - Clusters 1 and 2
##########################

ggplot(grpPA_df1_DAILY, aes(x =PApredict, y = conc, color = humidity_a)) +
  geom_jitter() +
  scale_fill_gradient(low = "#E2F3F5",  high = "blue3", na.value = "grey50", aesthetics = "color") +
  theme_bw() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")+
  xlim(0, 60) + ylim(0,60)


ggplot(grpPA_df2_DAILY, aes(x =PApredict, y = conc, color = humidity_a)) +
  geom_jitter() +
  scale_fill_gradient(low = "#EFF396"#"#FFF59D" 
                      ,high = "green4"
                      , na.value = "grey50", aesthetics = "color") +
  theme_bw() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")+
  xlim(0, 60) + ylim(0,60)

grpPA_dfAll_DAILY <- rbind(grpPA_df1_DAILY, grpPA_df2_DAILY)
ggplot(grpPA_dfAll_DAILY, aes(x = PApredict, y = conc, color=level)) +
  geom_point() +
  scale_color_manual(name = "level",
                     values = c("a" = alpha('blue', 0.2),#"blue",
                                "b" = alpha('green', 0.2)
                     ))+
  #scale_fill_gradient(colours = c("blue","green"))+
  geom_smooth(data = grpPA_dfAll_DAILY[grpPA_dfAll_DAILY$humidity_a<=50,], col="blue3", method = "lm",se=FALSE) +
  
  geom_smooth(data = grpPA_dfAll_DAILY[grpPA_dfAll_DAILY$humidity_a>50,], col="green4", method = "lm",se=FALSE)+
  xlim(0, 60) + ylim(0,60)

# GRAPH predicted concentrations - SSC-Mod4- ModBj- Raw PA
##########################
ggplot(grpPA_dfAll_DAILY, aes(x = PApredict, y = conc)) +
  geom_point(aes(x=grpPA_dfAll_DAILY$A_B_Avg, y= grpPA_dfAll_DAILY$conc
  ), legend=  TRUE, colour=alpha('black', 0.2)) +
  geom_point(aes(x=grpPA_dfAll_DAILY$modBj, y= grpPA_dfAll_DAILY$conc
  ), legend=  TRUE, colour=alpha('pink', 0.2)) +
  geom_point(aes(x=grpPA_dfAll_DAILY$mod4, y= grpPA_dfAll_DAILY$conc
  ), legend=  TRUE, colour=alpha('blue', 0.2)) +
  geom_point(aes(x=grpPA_dfAll_DAILY$PApredict, y= grpPA_dfAll_DAILY$conc
  ), legend=  TRUE, colour=alpha('green', 0.2)) +
  
  geom_smooth(aes(grpPA_dfAll_DAILY$mod4,grpPA_dfAll_DAILY$conc),colour=alpha('blue3'),method="lm",se=FALSE)+
  geom_smooth(aes(grpPA_dfAll_DAILY$PApredict,grpPA_dfAll_DAILY$conc),colour=alpha('green4'),method="lm",se=FALSE)+
  geom_smooth(aes(grpPA_dfAll_DAILY$modBj,grpPA_dfAll_DAILY$conc),colour=alpha('pink3'),method="lm",se=FALSE)+
  geom_smooth(aes(grpPA_dfAll_DAILY$A_B_Avg,grpPA_dfAll_DAILY$conc),colour=alpha('gray60'),method="lm",se=FALSE)+
  xlim(0, 60) + ylim(0,60)
################################################################################## END 


