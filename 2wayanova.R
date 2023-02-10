library(readxl)
my_data<-read_excel("Exampleforclass.xlsx")

keeps<- c("FISH", "SPP", "LAKE", "FL")
twoway_data_subset <- my_data[keeps]

twoway_data_subset[,4] <- log(twoway_data_subset[4])
names(twoway_data_subset)[4] <- "LFL"

#Filter dataset
twoway_data_subset <- twoway_data_subset[!(twoway_data_subset$LFL < 5.5),]

View(twoway_data_subset)

subset_two<- subset(twoway_data_subset, !SPP == "LKTR")
subset_two <- subset(subset_two, !SPP == "LNSC")
View(subset_two)

#str(twoway_data_subset)
#twoway_data_subset$SPP <- as.factor(twoway_data_subset$SPP)

#No interaction term
anova1 <- aov(LFL ~ SPP + LAKE, data = twoway_data_subset)
summary(anova1)


aov1_resid <- residuals(object=anova1)
shapiro.test(x=aov1_resid)

plot(anova1)

#With interaction term
anova2 <- aov(LFL ~ SPP + LAKE + SPP:LAKE, data = twoway_data_subset)
summary(anova2)

aov2_resid <- residuals(object = anova2)
shapiro.test(x=aov2_resid)
plot(anova2)


tukeysanova2<-TukeyHSD(anova2)
plot(tukeysanova2)
model.tables(anova2, "mean")

#Using subset with only WALL, LKWH, NRPK
subset_two <- subset_two[!(subset_two$LFL < 5.5),]
summary(subset_two)
anova3 <- aov(LFL ~ SPP + LAKE + SPP:LAKE, data = subset_two)
summary(anova3)

plot(anova3)

aov3_resid <- residuals(object=anova3)
shapiro.test(x=aov3_resid)

#Getting R squared value with lm

LM <- lm(LFL ~ SPP + LAKE + SPP:LAKE, data=twoway_data_subset)
summary(LM) #should be the same as using aov()
anova(LM)

#interaction.plot(x.factor = twoway_data_subset$LAKE, trace.factor = twoway_data_subset$SPP, response = twoway_data_subset$LFL, fun=mean, col=c("blue","red","green"), fixed = TRUE)
interaction.plot(x.factor = subset_two$LAKE, trace.factor = subset_two$SPP, response = subset_two$LFL, type = 'b', fun=mean, col=c("blue","red","green"), pch = c(19,17,15))
