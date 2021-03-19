library(dplyr)
library(ggplot2)

spec <- read.csv(file = "spec_done.csv")
head(spec)
table(spec$industry)

head(spec %>% arrange(sales))
summary(spec$sales)
# 490억
# 1563억
# 5000억

head(spec)

length(spec$sales)
spec <- spec %>% mutate(quantile = ifelse(sales > 500000000000, "3q",
                                          ifelse(sales > 150000000000, "2q", "1q")))

ggplot(data = spec, aes(x = quantile, y = spec_index, fill = quantile)) + geom_boxplot() +
  labs(x = "매출액", y = "스펙지수", title = "매출액별 합격자 스펙지수의 분포")

ggplot(data = spec, aes(x = quantile, y = achieve, fill = quantile)) + geom_boxplot() +
  labs(x = "매출액", y = "학업성취도", title = "매출액별 합격자 학업 성취도의 분포")

ggplot(data = spec, aes(x = quantile, y = language, fill = quantile)) + geom_boxplot() +
  labs(x = "매출액", y = "외국어", title = "매출액별 합격자 외국어 능력의 분포")

ggplot(data = spec, aes(x = quantile, y = skill, fill = quantile)) + geom_boxplot() +
  labs(x = "매출액", y = "전문능력", title = "매출액별 합격자 전문능력의 분포")

ggplot(data = spec, aes(x = quantile, y = activity, fill = quantile)) + geom_boxplot() +
  labs(x = "매출액", y = "대외활동", title = "매출액별 합격자 대외활동의 분포")


str(spec$quantile)
spec$quantile <- as.factor(spec$quantile)

summary(aov(achieve ~ quantile, data = spec))
summary(aov(language ~ quantile, data = spec))
summary(aov(skill ~ quantile, data = spec))
summary(aov(activity ~ quantile, data = spec))
summary(aov(spec_index ~ quantile, data = spec))


ggplot(data = spec, aes(x = sales, y = spec_index)) + geom_point() + stat_smooth(col = "red") + coord_cartesian(xlim=c(0, 100000000000))
ggplot(data = spec, aes(x = sales, y = achieve)) + geom_point() + stat_smooth() + coord_cartesian(ylim=c(0, 100000000000))
ggplot(data = spec, aes(x = sales, y = language)) + geom_point() + stat_smooth() + coord_cartesian(xlim=c(0, 100000000000))
ggplot(data = spec, aes(x = sales, y = activity)) + geom_point() + stat_smooth() + coord_cartesian(xlim=c(0, 100000000000))
ggplot(data = spec, aes(x = sales, y = skill)) + geom_point() + stat_smooth() + coord_cartesian(xlim=c(0, 100000000000))


spec.lm <- lm(sales ~ achieve + language + activity + skill, data = spec)
