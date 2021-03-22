library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
remDr$navigate("https://www.jobkorea.co.kr/starter/PassAssay?Page=1&OrderBy=0&FavorCo_Stat=0&isSaved=1&Pass_An_Stat=0")

# 클릭 자소서
jasoseol_url <- remDr$findElement(using = "css selector", "li:nth-child(1) > div.txBx > p > a > span")
jasoseol_url$clickElement()
Sys.sleep(1)

# 평점
jasoseol_star <- remDr$findElement(using = "css selector", "div.adviceTotal > div > span")
jasoseol_star$getElementText()


# 자소서 내용
jasoseol_text <- remDr$findElement(using = "css selector", "dd:nth-child(2) > div.tx")
jasoseol_text$getElementText()
#container > div.stContainer > div.selfQnaWrap > dl > dd:nth-child(4) > div.tx


# 가려진 질문3, 4
q3 <- remDr$findElement(using = "css selector", "dl > dt:nth-child(5) > button > span.arr.stSpImg")
q3$clickElement()

q4 <- remDr$findElement(using = "css selector", "dl > dt:nth-child(7) > button > span.arr.stSpImg")
q4$clickElement()

jasoseol_text <- remDr$findElement(using = "css selector", "dd:nth-child(8) > div.tx")
jasoseol_text$getElementText()

# 목록으로 돌아가기
list <- remDr$findElement(using = "css selector", "#container > div.stContainer > div.viewBtns > a.linkList")
list$clickElement()
