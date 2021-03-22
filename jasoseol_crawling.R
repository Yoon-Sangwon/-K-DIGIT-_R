library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()


# 크롤링
url1 <- "https://www.jobkorea.co.kr/starter/PassAssay?Page="
url2 <- "&OrderBy=0&FavorCo_Stat=0&isSaved=1&Pass_An_Stat=0"
# star <- NULL
# text <- NULL
# text1 <- NULL
# text2 <- NULL
# text3 <- NULL
# text4 <- NULL
# text5 <- NULL

for (i in 24:30){
  url <- paste0(url1, i, url2)
  remDr$navigate(url)
  Sys.sleep(4)
  for (j in 1:20){
    jasoseol_url <- remDr$findElement(using = "css selector",
                                      paste0("li:nth-child(", j, ") > div.txBx > p > a > span"))
    jasoseol_url$clickElement()
    Sys.sleep(4)
    # 평점 가져오기
    jasoseol_star <- remDr$findElements(using = "css selector",
                                        paste0("div.adviceTotal > div > span"))
    if (length(sapply(jasoseol_star, function(x) x$getElementText())) == 0) {
      star <- c(star, NA)
    }
    else {
      star <- c(star, unlist(sapply(jasoseol_star, function(x) x$getElementText())))
    }
    # 접힌 질문 펼치기
    q3 <- remDr$findElements(using = "css selector", "dl > dt:nth-child(5) > button > span.arr.stSpImg")
    sapply(q3, function(x) x$clickElement())
    q4 <- remDr$findElements(using = "css selector", "dl > dt:nth-child(7) > button > span.arr.stSpImg")
    sapply(q4, function(x) x$clickElement())
    q5 <- remDr$findElements(using = "css selector", "dl > dt:nth-child(9) > button > span.arr.stSpImg")
    sapply(q5, function(x) x$clickElement())
    # 자소서 text 긁어오기
    for (k in seq(2, 10, 2)) {
      jasoseol_text <- remDr$findElements(using = "css selector",
                                         paste0("dd:nth-child(", k, ") > div.tx"))
      text <- c(text, unlist(sapply(jasoseol_text, function(x) x$getElementText())))
    }
    text1 <- c(text1, text[1])
    text2 <- c(text2, text[2])
    text3 <- c(text3, text[3])
    text4 <- c(text4, text[4])
    text5 <- c(text5, text[5])
    text <- NULL
    # 목록으로 돌아가기
    list <- remDr$findElement(using = "css selector", "#container > div.stContainer > div.viewBtns > a.linkList")
    list$clickElement()
    Sys.sleep(4)
  }
}

# 134
length(text1)
length(text2)
length(text3)
length(text4)
length(text5)
length(star)


tail(text1,1)
tail(text2,1)
tail(text3,1)
tail(text4,1)
tail(text5,1)
table(star)

jasoseol <- data.frame(star, text1, text2, text3, text4, text5)
names(jasoseol) <- c("star", "Q1", "Q2", "Q3", "Q4", "Q5")
head(jasoseol)

write.csv(jasoseol, file = "jasoseol.csv")
