library(dplyr)
library(ggplot2)
library(fmsb)

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
  labs(x = "매출액", y = "외국어", title = "매출액별 합격자 외국어 능력의 분포") +
  coord_cartesian(ylim=c(4, 10))

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


# radar chart
spec_3q <- spec %>% filter(quantile == "3q") %>% select("spec_index", "achieve", "language", "skill", "activity") 
spec_2q <- spec %>% filter(quantile == "2q") %>% select("spec_index", "achieve", "language", "skill", "activity")
spec_1q <- spec %>% filter(quantile == "1q") %>% select("spec_index", "achieve", "language", "skill", "activity")

q3 <- c(mean(spec_3q$spec_index), mean(spec_3q$achieve), mean(spec_3q$language),
        mean(spec_3q$skill), mean(spec_3q$activity))

q2 <- c(mean(spec_2q$spec_index), mean(spec_2q$achieve), mean(spec_2q$language),
        mean(spec_2q$skill), mean(spec_2q$activity))

q1 <- c(mean(spec_1q$spec_index), mean(spec_1q$achieve), mean(spec_1q$language),
        mean(spec_1q$skill), mean(spec_1q$activity))

range(spec$spec_index)
max.spec <- c(300, 10, 10, 10, 5)
min.spec <- c(200, 5, 5, 5, 0)

q3_df <- as.data.frame(rbind(max.spec, min.spec, q3))
q2_df <- as.data.frame(rbind(max.spec, min.spec, q2))
q1_df <- as.data.frame(rbind(max.spec, min.spec, q1))

names(q3_df) <- c("스펙지수", "학업성취도", "외국어", "전문능력", "대외활동")
names(q2_df) <- c("스펙지수", "학업성취도", "외국어", "전문능력", "대외활동")
names(q1_df) <- c("스펙지수", "학업성취도", "외국어", "전문능력", "대외활동")

?par
par(mfrow = c(1,3), mar = c(5, 2, 5, 2))
radarchart(q1_df,
           pcol='lightcoral',             # 다각형 선의 색 
           pfcol=rgb(1, 0.5, 0.5, 0.5),  # 다각형 내부 색
           plwd=3,                       # 다각형 선의 두께
           cglcol='darkgrey',                # 거미줄의 색
           cglty=1,                      # 거미줄의 타입
           cglwd=0.8,                    # 거미줄의 두께
           axistype=2,                   # 축의 레이블 타입
           seg = 4,                        # 축의 눈금 분할                         
           axislabcol = 'black',            # 축의 레이블 색
           caxislabels = seq(0,10,2.5),    # 축의 레이블 값
           paxislabels = c(300, 10, 10, 10, 5), 
           title = "매출액 500억 미만")
radarchart(q2_df,
           pcol='mediumaquamarine',             # 다각형 선의 색 
           pfcol=rgb(0.5, 1, 0.5, 0.5),  # 다각형 내부 색
           plwd=3,                       # 다각형 선의 두께
           cglcol='darkgrey',                # 거미줄의 색
           cglty=1,                      # 거미줄의 타입
           cglwd=0.8,                    # 거미줄의 두께
           axistype=2,                   # 축의 레이블 타입
           seg = 4,                        # 축의 눈금 분할                         
           axislabcol = 'black',            # 축의 레이블 색
           caxislabels = seq(0,10,2.5),    # 축의 레이블 값
           paxislabels = c(300, 10, 10, 10, 5), 
           title = "매출액 1500억 이상 5000억 미만",)
radarchart(q3_df,
           pcol='lightslateblue',             # 다각형 선의 색 
           pfcol=rgb(0.5, 0.5, 1, 0.5),  # 다각형 내부 색
           plwd=3,                       # 다각형 선의 두께
           cglcol='darkgrey',                # 거미줄의 색
           cglty=1,                      # 거미줄의 타입
           cglwd=0.8,                    # 거미줄의 두께
           axistype=2,                   # 축의 레이블 타입
           seg = 4,                        # 축의 눈금 분할                         
           axislabcol = 'black',            # 축의 레이블 색
           caxislabels = seq(0,10,2.5),    # 축의 레이블 값
           paxislabels = c(300, 10, 10, 10, 5), 
           title = "매출액 5000억 이상",)
?radarchart

ggplot(data = spec, aes(x = sales, y = spec_index)) + geom_point() + stat_smooth(col = "red") + coord_cartesian(xlim=c(0, 100000000000))
ggplot(data = spec, aes(x = sales, y = achieve)) + geom_point() + stat_smooth() + coord_cartesian(ylim=c(0, 100000000000))
ggplot(data = spec, aes(x = sales, y = language)) + geom_point() + stat_smooth() + coord_cartesian(xlim=c(0, 100000000000))
ggplot(data = spec, aes(x = sales, y = activity)) + geom_point() + stat_smooth() + coord_cartesian(xlim=c(0, 100000000000))
ggplot(data = spec, aes(x = sales, y = skill)) + geom_point() + stat_smooth() + coord_cartesian(xlim=c(0, 100000000000))


spec.lm <- lm(sales ~ achieve + language + activity + skill, data = spec)
step(spec.lm, direction = "backward")
start.lm <- lm(sales~1, data = spec)
step(start.lm, scope = list(lower = start.lm, upper = spec.lm), direction = "both")

summary(spec.lm)
summary(lm(sales ~ achieve + activity, data = spec))


# industry 와의 상관 관계
head(sort(table(spec$industry), decreasing = T), 30)
ind30_name <- names(head(sort(table(spec$industry), decreasing = T), 10))
ind30_name
spec_ind <- spec %>% filter(spec$industry %in% ind30_name)
table(spec_ind$industry, spec_ind$quantile)

summary(lm(sales ~ achieve + language + activity + skill, data = spec_ind))


# spec_index
range(spec$spec_index)
summary(spec$spec_index)
summary(lm(spec_index ~ achieve + language + activity + skill, data = spec))



