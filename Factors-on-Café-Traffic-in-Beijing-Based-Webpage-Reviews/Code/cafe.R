library(readxl)
cafe <- read_excel("cafe.xls", sheet = "Sheet1")
summary(cafe)

#因变量
#数值特征
library(fBasics)
basicStats(cafe$visit)
#直方图
par(family='STKaiti')
hist(cafe$visit, breaks = 12,col = "red3",xlab = "月评论数",ylab = "频数",main = "月评论数直方图")
#箱线图
par(family='STKaiti')
boxplot(cafe$visit,main = "月评论数箱线图",col = "red3")

#定量自变量:
#数值特征
library(fBasics)
myvars <- c("comments","consume","star")
basicStats(cafe[myvars])
#相关分析
#散点图
par(family = "STKaiti")
plot(cafe$comments,cafe$visit,xlab = "总评论数",ylab = "月评论数")
plot(cafe$consume,cafe$visit,xlab = "人均消费",ylab = "月评论数")
plot(cafe$promo,cafe$visit,xlab = "团购数量",ylab = "月评论数")
#散点图矩阵
myvars1 <- c("comments","consume","visit")
pairs(cafe[myvars1])
#pearson相关系数
cor(cafe$visit,cafe$comments)
cor(cafe$visit,cafe$consume)
cor(cafe$visit,cafe$star)

#定性变量:地理位置、类别
#单变量描述:
par(family='STKaiti')
fre = table(cafe$add)                                                                               
plot(fre,xlab = "类别",ylab = "频数",col = "red3",main = "咖啡厅位置频数条形图")
fre = table(cafe$category) 
lal = paste(names(fre), round(fre/sum(fre)*100, 2), "%", sep = "")
pie(fre, labels = lal, main = "各类别样本量占比饼图") 
#分组箱线图
boxplot(cafe$visit ~ cafe$shop,col = "red3",xlab = "是否处于热门商区", ylab = "月评论数", main = "是否处于热门商区的月评论数箱线图")
boxplot(cafe$visit ~ cafe$class,col = "red3",xlab = "是否为综合型咖啡厅", ylab = "月评论数", main = "是否为综合型咖啡厅的月评论数箱线图")
boxplot(cafe$visit ~ cafe$promo,col = "red3",xlab = "是否提供团购优惠", ylab = "月评论数", main = "是否提供团购优惠的月评论数箱线图")


#回归
model = lm(cafe$visit ~ cafe$comments + cafe$consume + cafe$promo + cafe$star + cafe$shop + cafe$class)
summary(model)
#回归诊断
par(mfrow=c(2,2))
plot(model)
library(car)
vif(model)

#单因素方差检验:
#三个假设:
#方差检验:
anova1 = aov(cafe$visit ~ cafe$shop)
summary(anova1)
#不显著
anova1 = aov(cafe$visit ~ cafe$class)
summary(anova1)
#显著
