library(dplyr)

spec <- read.csv(file = "spec.csv")

head(spec)
str(spec)

# x 변수 삭제
spec <- spec %>% select(company, industry, sales, achieve, language, skill, activity, spec_index)

# sales를 숫자로 변환하기 위해 데이터 확인
head(spec$sales, 15)
length(spec$sales)

for (i in 1:2785) {
  spec$sales1[i] <- strsplit(spec$sales, split = " ")[[i]][1]
  spec$sales2[i] <- strsplit(spec$sales, split = " ")[[i]][2]
  spec$sales3[i] <- strsplit(spec$sales, split = " ")[[i]][3]
}

head(spec)

table(gsub(",|[[:digit:]]|원|이상", "", spec$sales1), useNA = "always")
table(gsub(",|[[:digit:]]|원|이상", "", spec$sales2), useNA = "always")
table(gsub(",|[[:digit:]]|원|이상", "", spec$sales3), useNA = "always")

spec$company[spec$sales2 == "억" & !is.na(spec$sales2)]
spec$sales1[spec$sales2 == "억" & !is.na(spec$sales2)] <- c("5000억", "1002억")
spec$sales1[spec$sales2 == "억" & !is.na(spec$sales2)]

# sales3 먼저 숫자로 변환
spec$sales3 <- gsub(",|원", "", spec$sales3)
spec$sales3[!is.na(spec$sales3)]

# 한글로 된 숫자 단위 확인
table(gsub("[[:digit:]]", "", spec$sales3), useNA = "always")

# 만, 억을 숫자로 변환
spec$sales3 <- gsub("만", "0000", spec$sales3)
spec$sales3 <- gsub("억", "00000000", spec$sales3)
spec$sales3[!is.na(spec$sales3)]

# 데이터 타입을 숫자로 변환
spec$ sales3 <- as.numeric(spec$sales3)


# sales2 숫자로 변환
table(gsub(",|[[:digit:]]|원|이상", "", spec$sales2), useNA = "always")
spec$sales2

spec$sales2 <- gsub(",|원", "", spec$sales2)
table(gsub("[[:digit:]]", "", spec$sales2), useNA = "always")
spec$sales2 <- gsub("백", "00", spec$sales2)
spec$sales2 <- gsub("백", "00", spec$sales2)

table(gsub("[[:digit:]]", "", spec$sales2), useNA = "always")
spec$sales2[gsub("[[:digit:]]", "", spec$sales2) == "천억" & !is.na(spec$sales2)]
spec$sales2 <- gsub("천억", "00000000000", spec$sales2)

table(gsub("[[:digit:]]", "", spec$sales2), useNA = "always")
spec$sales2[gsub("[[:digit:]]", "", spec$sales2) == "천만" & !is.na(spec$sales2)]
spec$sales2 <- gsub("천만", "0000000", spec$sales2)

table(gsub("[[:digit:]]", "", spec$sales2), useNA = "always")
spec$sales2[gsub("[[:digit:]]", "", spec$sales2) == "억천" & !is.na(spec$sales2)]
spec$sales2 <- gsub("억천", "00001000", spec$sales2)

table(gsub("[[:digit:]]", "", spec$sales2), useNA = "always")
spec$sales2[gsub("[[:digit:]]", "", spec$sales2) == "천" & !is.na(spec$sales2)]
spec$sales2 <- gsub("천", "000", spec$sales2)
spec$sales2 <- gsub("만", "0000", spec$sales2)
spec$sales2 <- gsub("억", "00000000", spec$sales2)
spec$sales2 <- gsub("조", "000000000000", spec$sales2)

table(gsub("[[:digit:]]", "", spec$sales2), useNA = "always")
spec$sales2 <- as.numeric(spec$sales2)
spec$sales2



# sales1 숫자로 변환
spec$sales1 <- gsub(",|원|이상", "", spec$sales1)
table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")

spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억천백만" & !is.na(spec$sales1)]
spec$sales1 <- gsub("억4천4백만", "억4400만", spec$sales1)


table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "백십억" & !is.na(spec$sales1)]
spec$sales1 <- gsub("십", "", spec$sales1)

spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "백만" & !is.na(spec$sales1)]
spec$sales1 <- gsub("(39966백만)|(14377백만)|(163376백만)|(21782백만)", "", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "천백억" & !is.na(spec$sales1)]
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "백억" & !is.na(spec$sales1)]

spec$sales1 <- gsub("400백억", "400억", spec$sales1)
spec$sales1 <- gsub("2백23억", "223억", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "천백억" & !is.na(spec$sales1)]
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "백억" & !is.na(spec$sales1)]
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억백만" & !is.na(spec$sales1)]

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1 <- gsub("백", "00", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억천만" & !is.na(spec$sales1)]
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "조천억" & !is.na(spec$sales1)]
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "조천억_" & !is.na(spec$sales1)]
spec$sales1 <- gsub("억_2017", "억", spec$sales1)


table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "천" & !is.na(spec$sales1)]
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "천억" & !is.na(spec$sales1)]
spec$sales1 <- gsub("천800억", "800억", spec$sales1)


table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "천억" & !is.na(spec$sales1)]
spec$sales1 <- gsub("천600억", "600억", spec$sales1)
spec$sales1 <- gsub("7천234억", "7234억", spec$sales1)
spec$sales1 <- gsub("3천68억", "3068억", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1 <- gsub("천", "000", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "만" & !is.na(spec$sales1)]
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억만" & !is.na(spec$sales1)] <- gsub("억","",spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억만" & !is.na(spec$sales1)])


table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "만" & !is.na(spec$sales1)]
spec$sales1 <- gsub("만", "0000", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == ".억" & !is.na(spec$sales1)] <-
  gsub(".3억", "30000000", spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == ".억" & !is.na(spec$sales1)])

spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "(``)억" & !is.na(spec$sales1)] <-
  gsub("06``|[[:punct:]]", "", spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "(``)억" & !is.na(spec$sales1)])

spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억()" & !is.na(spec$sales1)] <-
  gsub("억(2017)", "억", spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억()" & !is.na(spec$sales1)])

spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억(년" & !is.na(spec$sales1)]
spec$sales1 <- gsub("2019년|[[:punct:]]", "", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억억" & !is.na(spec$sales1)]
spec$sales1 <- gsub("600억800억", "800억", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억여" & !is.na(spec$sales1)]
spec$sales1 <- gsub("여", "", spec$sales1)

spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "조억" & !is.na(spec$sales1)]
spec$sales1 <- gsub("1조239억", "10239억", spec$sales1)
spec$sales1 <- gsub("1조148억", "10148억", spec$sales1)
spec$sales1 <- gsub("1조506억", "10506억", spec$sales1)
spec$sales1 <- gsub("1조277억", "10277억", spec$sales1)

!table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "조억" & !is.na(spec$sales1)] <-
  gsub("조", "", spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "조억" & !is.na(spec$sales1)])

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억" & !is.na(spec$sales1)]
spec$sales1 <- gsub("527억2017", "527억", spec$sales1)

table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "억" & !is.na(spec$sales1)]
spec$sales1[gsub("[[:digit:]]", "", spec$sales1) == "조" & !is.na(spec$sales1)]
spec$sales1 <- gsub("조", "000000000000", spec$sales1)
spec$sales1 <- gsub("억", "00000000", spec$sales1)


table(gsub("[[:digit:]]", "", spec$sales1), useNA = "always")

# sales1 숫자 타입으로 변환
spec$sales1 <- as.numeric(spec$sales1)

spec$sales1[is.na(spec$sales1)] <- 0
spec$sales2[is.na(spec$sales2)] <- 0
spec$sales3[is.na(spec$sales3)] <- 0


spec <- spec %>% mutate(sales_new = spec$sales1 + spec$sales2 + spec$sales3)
head(spec)

spec <- spec %>% select(company, industry, sales, achieve, language, skill, activity, spec_index, sales_new)
spec$sales_new[spec$sales_new == 0] <- NA
spec_na <- spec %>% 
spec <- spec[complete.cases(spec),]
spec <- spec %>% select(company, industry, sales = sales_new, achieve, language, skill, activity, spec_index)
head(spec)

write.csv(spec_na, file = "project/spec_na.csv")
write.csv(spec, file = "project/spec_done.csv")
