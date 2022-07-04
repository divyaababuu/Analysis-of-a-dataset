seg.df<-read.csv("1_Demographics.csv", stringsAsFactors = TRUE)

head(seg.df,n=8)
summary(seg.df, digits=2)
seg.df$gender<-ifelse(seg.df$Gender=="2",0,1)
seg.dist<-dist(seg.df)
as.matrix(seg.dist)[1:5,1:5]

seg.hc<-hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc),h=4)$lower[[1]])
rect.hclust(seg.hc, k=4, border = "red")
seg.hc.segment<-cutree(seg.hc,k=4)
table(seg.hc.segment)

library(cluster)
clusplot(seg.df, seg.hc.segment,
         color = TRUE, #color the groups
         shade = TRUE, #shade the ellipses for group membership
         labels = 4, #label only the groups, not the individual points
         lines = 0, #omit distance lines between groups
         main = "Hierarchical cluster plot", # figure title
)

aggregate(seg.df, list(seg.hc.segment), mean) #mean

set.seed(54321) #inorder to run k means, you have to run seed value
seg.k <- kmeans(seg.df, centers = 4)
aggregate(seg.df,list(seg.k$cluster), mean)
boxplot(seg.df$Salary~seg.k$cluster,ylab="Salary",xlab="Cluster/Segment") #median
boxplot(seg.df$Choco_Consumption~seg.k$cluster,ylab="Choco_Consumption",xlab="Cluster/Segment")
clusplot(seg.df, seg.k$cluster,
         color = TRUE, #color the groups
         shade = TRUE, #shade the ellipses for group membership
         labels = 4, #label only the groups, not the individual points
         lines = 0, #omit distance lines between groups
         main = "K-means cluster plot", # figure title
)

library(mclust)
seg.mc <- Mclust(seg.df)
summary(seg.mc) #compare the BIC value for statistical significance - the lower the value, the better
seg.mc4<-Mclust(seg.df,G=4) #increase the number of clusters to 4
summary(seg.mc4) #compare the loglikelihood value for statistical significance - the higher the value, the better
BIC(seg.mc, seg.mc4)
aggregate(seg.df, list(seg.mc$classification), mean)
library(cluster)
clusplot(seg.df, seg.mc$classification, color = TRUE, shade = TRUE,
         labels = 4, lines = 0, main = "Model-based cluster plot")

# Factor Analysis:
library(corrplot)
library(gplots)
brand.ratings <- read.csv("2_chocolate_rating.csv", stringsAsFactors = TRUE)
head(brand.ratings)
summary(brand.ratings)
str(brand.ratings)
brand.sc <- brand.ratings
cor(brand.sc[,6:14])

corrplot(cor(brand.sc[,6:14]))
corrplot(cor(brand.sc[,6:14]), order = "hclust")
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
brand.mean
rownames(brand.mean) <- brand.mean[, 1]
brand.mean

#principal component analysis
brand.pc<- princomp(brand.mean, cor = TRUE)
summary(brand.pc)
plot(brand.pc,type="l") # scree plot
loadings(brand.pc)
brand.pc$scores
biplot(brand.pc, main = "Brand positioning")


#Conjoint Analysis
library("xtable") # processing of regression output
library("knitr") # used for report compilation and table display
library("ggplot2") # very popular plotting library ggplot2
library("mlogit") # multinomial logit
library(lattice)
library("caret")
cbc.df<-read.csv("5_conjoint.csv", stringsAsFactors = TRUE)
str(cbc.df)
head(cbc.df)
summary(cbc.df)
xtabs(Choice~Price, data=cbc.df)
xtabs(Choice~Salary, data=cbc.df)
cbc.df$Origin <- relevel(cbc.df$Origin, ref = "Peru")
cbc.df$Energy <- relevel(cbc.df$Energy, ref = "Low")
cbc.df$Manufacture <- relevel(cbc.df$Manufacture, ref = "Developed")
cbc.df$Nuts <- relevel(cbc.df$Nuts, ref = "No")
cbc.df$Tokens <- relevel(cbc.df$Tokens, ref = "No")
cbc.df$Organic <- relevel(cbc.df$Organic, ref = "No")
cbc.df$Premium <- relevel(cbc.df$Premium, ref = "No")
cbc.df$Fairtrade <- relevel(cbc.df$Fairtrade, ref = "Yes")
cbc.df$Sugar <- relevel(cbc.df$Sugar, ref = "High")

library(dfidx)
cbc.mlogit <- dfidx(cbc.df, choice="Choice",
                    idx=list(c("Choice_id", "Consumer_id"), "Alternative"))
library(mlogit)
model<-mlogit(Choice ~ 0+Origin+Energy+Manufacture+Nuts+Tokens+Organic+Premium+Fairtrade+Sugar+Price, data=cbc.mlogit)
library(knitr)
kable(summary(model)$CoefTable)
model.constraint <-mlogit(Choice ~ 0+Origin, data = cbc.mlogit)
lrtest(model, model.constraint)
kable(head(predict(model,cbc.mlogit)))
predicted_alternative <- apply(predict(model,cbc.mlogit),1,which.max)
selected_alternative <- cbc.mlogit$Alternative[cbc.mlogit$Choice>0]
library(caret)
confusionMatrix(table(predicted_alternative,selected_alternative),positive = "1")

model<-mlogit(Choice ~ 0+Origin+Manufacture+Energy+Nuts+Organic+Tokens+Premium+Fairtrade+Sugar+Price, data=cbc.mlogit)
summary(model)
kable(summary(model)$CoefTable)

#willingness to pay 
coef(model)["PremiumYes"] /(-coef(model)["Price"])
coef(model)["TokensDonate"] /(-coef(model)["Price"])
coef(model)["EnergyHigh"] / (-coef(model)["Price"])
coef(model)["OrganicYes"] /(-coef(model)["Price"])

#MBA
library(arules)
retail.raw <- readLines("6_groceries.dat")
summary(retail.raw)
retail.list <- strsplit(retail.raw, ",")
names(retail.list) <- paste("Trans", 1:length(retail.list))
str(retail.list)
library(car)
some(retail.list)
rm(retail.raw)
library(arules)
retail.trans <- as(retail.list ,"transactions")
summary(retail.trans)
rm(retail.list)
groc.rules <- apriori(retail.trans, parameter = list(supp=0.01, conf=0.3, target="rules"))

inspect(subset(groc.rules, lift > 45))

library(arulesViz)
plot(groc.rules)
library(plotly)
plot(groc.rules, engine = "plotly")

groc.hi <- head(sort(groc.rules, by="lift"), 15)
inspect(groc.hi)
plot(groc.hi, method="graph")
summary(groc.hi)

#resource trade-offs
#marketing response model
library(readxl)
library(dplyr)
spending.data <- read.csv("7_advertising.csv")
str(spending.data)
#radio
plot(spending.data$radio, spending.data$sales)
cor(spending.data$radio, spending.data$sales) #positive correlation
#magazines
plot(spending.data$magazines, spending.data$sales)
cor(spending.data$magazines, spending.data$sales) #positive correlation
#SM
plot(spending.data$social_media, spending.data$sales)
cor(spending.data$social_media, spending.data$sales) #positive correlation
#search_ads
plot(spending.data$search_ads, spending.data$sales)
cor(spending.data$search_ads, spending.data$sales) #positive correlation
#tv
plot(spending.data$tv, spending.data$sales)
cor(spending.data$tv, spending.data$sales) #positive correlation
#newspaper
plot(spending.data$newspaper, spending.data$sales)
cor(spending.data$newspaper, spending.data$sales) #positive correlation

#Simple linear regression
regression <- lm(sales ~ radio, data = spending.data)
summary(regression)
#magazines
regression <- lm(sales ~ magazines, data = spending.data)
summary(regression)
#SM
regression <- lm(sales ~ social_media, data = spending.data)
summary(regression)
#search ads
regression <- lm(sales ~ search_ads, data = spending.data)
summary(regression)
#tv
regression <- lm(sales ~ tv, data = spending.data)
summary(regression)
#newspapers
regression <- lm(sales ~ newspaper, data = spending.data)
summary(regression)
#Multiple linear regression
regression <- lm(sales ~ radio + tv + social_media + search_ads, data=spending.data)
summary(regression)

#Elasticity
mean(spending.data$radio)
mean(spending.data$sales)
15.56 * (23.22/1402.25) #A 1% increase in radio advertising results in a 0.25% increase in sales

mean(spending.data$tv)
mean(spending.data$sales)
4.6634 * (146.9295/1402.25)

#log regression
plot(spending.data$tv, spending.data$sales)
summary(spending.data$sales)
summary(spending.data$radio)
summary(spending.data$magazines)
summary(spending.data$social_media)
summary(spending.data$search_ads)
summary(spending.data$tv)
summary(spending.data$newspaper)
regression <- lm(log(sales) ~ log(radio+0.01) + log(magazines) + log(social_media) + log(search_ads) + log(tv) + log(newspaper),
                 data=spending.data)
summary(regression)

#synergy
regression <- lm(log(sales) ~ log(radio+0.01) + log(magazines) + log(social_media) + log(search_ads) + log(tv) + log(newspaper)
                 + log(radio+0.01)*log(tv), data=spending.data)
summary(regression)

#marketing experiment
library("multcomp")
ab.df <- read.csv("8_clickstream.csv", stringsAsFactors = TRUE)
summary(ab.df)
str(ab.df)
mean(ab.df$time_spent[ab.df$condition == "taste"])
mean(ab.df$time_spent[ab.df$condition == "quality"])

table(ab.df$condition, ab.df$clicked_article)
table(ab.df$condition, ab.df$clicked_like)
table(ab.df$condition, ab.df$clicked_share)

#histogram
library(lattice)
histogram(~ ab.df$clicked_article | condition, data = ab.df)
histogram(~ ab.df$clicked_like | condition, data = ab.df)
histogram(~ ab.df$clicked_share | condition, data = ab.df)

table(ab.df$clicked_article, ab.df$condition)
chisq.test(table(ab.df$clicked_article, ab.df$condition))

table(ab.df$clicked_like, ab.df$condition)
chisq.test(table(ab.df$clicked_like, ab.df$condition))

table(ab.df$clicked_share, ab.df$condition)
chisq.test(table(ab.df$clicked_share, ab.df$condition))

ad.aov.con <- aov(ab.df$time_spent ~ condition, data = ab.df)
anova(ad.aov.con)
anova(aov(time_spent_homepage_sec ~ clicked_article + condition, data = ab.df))

library(multcomp)
ad.aov <- aov (time_spent_homepage_sec ~ 0 + clicked_article, data = ab.df)
glht(ad.aov)
plot(glht(ad.aov),
     xlab = "Total time_spent_homepage_sec", main = "Average time_spent_homepage_sec by clicked_article (95% CI)", cex.axis = 0.8)
