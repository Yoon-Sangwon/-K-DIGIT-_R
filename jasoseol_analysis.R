jasoseol <- read.csv("jasoseol.csv")
View(jasoseol)

library(dplyr)
table(jasoseol$star)

jasoseol1 <- jasoseol %>% select("star", "Q1", "Q2", "Q3", "Q4", "Q5") %>%
  filter(star >= 3)
jasoseol2 <- jasoseol %>% select("star", "Q1", "Q2", "Q3", "Q4", "Q5") %>%
  filter(star <= 2)

jasoseol1$Q1[2]
jasoseol1$Q2[5]

# 자소서 스크래핑 시 좋은점 아쉬운점 글자수가 같이 됐으므로
# 다른 어떤걸 삭제하기 전에 미리 해준다.
jasoseol1$Q1 <- gsub("좋은점", "", jasoseol1$Q1)
jasoseol1$Q2 <- gsub("좋은점", "", jasoseol1$Q2)
jasoseol1$Q3 <- gsub("좋은점", "", jasoseol1$Q3)
jasoseol1$Q4 <- gsub("좋은점", "", jasoseol1$Q4)
jasoseol1$Q5 <- gsub("좋은점", "", jasoseol1$Q5)
jasoseol1$Q1 <- gsub("아쉬운점", "", jasoseol1$Q1)
jasoseol1$Q2 <- gsub("아쉬운점", "", jasoseol1$Q2)
jasoseol1$Q3 <- gsub("아쉬운점", "", jasoseol1$Q3)
jasoseol1$Q4 <- gsub("아쉬운점", "", jasoseol1$Q4)
jasoseol1$Q5 <- gsub("아쉬운점", "", jasoseol1$Q5)
jasoseol1$Q1 <- gsub("글자수", "", jasoseol1$Q1)
jasoseol1$Q2 <- gsub("글자수", "", jasoseol1$Q2)
jasoseol1$Q3 <- gsub("글자수", "", jasoseol1$Q3)
jasoseol1$Q4 <- gsub("글자수", "", jasoseol1$Q4)
jasoseol1$Q5 <- gsub("글자수", "", jasoseol1$Q5)

jasoseol1$Q1[2]
jasoseol1$Q2[5]

# 개행문자, 특수문자 등을 삭제한다.
jasoseol1$Q1 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol1$Q1)
jasoseol1$Q1[2]
jasoseol1$Q2 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol1$Q2)
jasoseol1$Q3 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol1$Q3)
jasoseol1$Q4 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol1$Q4)
jasoseol1$Q5 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol1$Q5)
jasoseol1$Q5[116]

# 숫자, 영어 등은 분석에 유의미하지 않기 때문에 삭제한다.
jasoseol1$Q1 <- gsub("[[:digit:]]|[A-z]", "", jasoseol1$Q1)
jasoseol1$Q2 <- gsub("[[:digit:]]|[A-z]", "", jasoseol1$Q2)
jasoseol1$Q3 <- gsub("[[:digit:]]|[A-z]", "", jasoseol1$Q3)
jasoseol1$Q4 <- gsub("[[:digit:]]|[A-z]", "", jasoseol1$Q4)
jasoseol1$Q5 <- gsub("[[:digit:]]|[A-z]", "", jasoseol1$Q5)

jasoseol1$Q1[2]
jasoseol1$Q2[5]



# 같은 것을 jasoseol2 에도 적용
jasoseol2$Q1[2]

jasoseol2$Q1 <- gsub("좋은점", "", jasoseol2$Q1)
jasoseol2$Q2 <- gsub("좋은점", "", jasoseol2$Q2)
jasoseol2$Q3 <- gsub("좋은점", "", jasoseol2$Q3)
jasoseol2$Q4 <- gsub("좋은점", "", jasoseol2$Q4)
jasoseol2$Q5 <- gsub("좋은점", "", jasoseol2$Q5)
jasoseol2$Q1 <- gsub("아쉬운점", "", jasoseol2$Q1)
jasoseol2$Q2 <- gsub("아쉬운점", "", jasoseol2$Q2)
jasoseol2$Q3 <- gsub("아쉬운점", "", jasoseol2$Q3)
jasoseol2$Q4 <- gsub("아쉬운점", "", jasoseol2$Q4)
jasoseol2$Q5 <- gsub("아쉬운점", "", jasoseol2$Q5)
jasoseol2$Q1 <- gsub("글자수", "", jasoseol2$Q1)
jasoseol2$Q2 <- gsub("글자수", "", jasoseol2$Q2)
jasoseol2$Q3 <- gsub("글자수", "", jasoseol2$Q3)
jasoseol2$Q4 <- gsub("글자수", "", jasoseol2$Q4)
jasoseol2$Q5 <- gsub("글자수", "", jasoseol2$Q5)


jasoseol2$Q1 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol2$Q1)
jasoseol2$Q2 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol2$Q2)
jasoseol2$Q3 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol2$Q3)
jasoseol2$Q4 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol2$Q4)
jasoseol2$Q5 <- gsub("[[:cntrl:]]|[[:punct:]]", "", jasoseol2$Q5)


jasoseol2$Q1 <- gsub("[[:digit:]]|[A-z]", "", jasoseol2$Q1)
jasoseol2$Q2 <- gsub("[[:digit:]]|[A-z]", "", jasoseol2$Q2)
jasoseol2$Q3 <- gsub("[[:digit:]]|[A-z]", "", jasoseol2$Q3)
jasoseol2$Q4 <- gsub("[[:digit:]]|[A-z]", "", jasoseol2$Q4)
jasoseol2$Q5 <- gsub("[[:digit:]]|[A-z]", "", jasoseol2$Q5)

jasoseol2$Q1[2]

# Q1 ~ Q5 까지를 병합
View(jasoseol1)
jasoseol1_total <- rbind(jasoseol1$Q1, jasoseol1$Q2, jasoseol1$Q3, jasoseol1$Q4, jasoseol1$Q5)
jasoseol1_total <- jasoseol1_total[!is.na(jasoseol1_total)]
length(jasoseol1_total)

jasoseol2_total <- rbind(jasoseol2$Q1, jasoseol2$Q2, jasoseol2$Q3, jasoseol2$Q4, jasoseol2$Q5)
jasoseol2_total <- jasoseol2_total[!is.na(jasoseol2_total)]
length(jasoseol2_total)

library(KoNLP)
jasoseol1_noun <- extractNoun(jasoseol1_total)
jasoseol1_noun <- unlist(jasoseol1_noun)
jasoseol1_noun <- Filter(function(x) {(nchar(x) > 1)}, jasoseol1_noun)
jasoseol1_table <- table(jasoseol1_noun)
head(sort(jasoseol1_table, decreasing = T), 30)

jasoseol2_noun <- extractNoun(jasoseol2_total)
jasoseol2_noun <- unlist(jasoseol2_noun)
jasoseol2_noun <- Filter(function(x) {(nchar(x) > 1)}, jasoseol2_noun)
jasoseol2_table <- table(jasoseol2_noun)
head(sort(jasoseol2_table, decreasing = T), 30)

