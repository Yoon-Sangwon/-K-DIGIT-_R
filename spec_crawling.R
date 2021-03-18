# 합격 스펙 크롤링 (정적)
library(rvest)

# jobkorea 합격스펙 HTML 읽어오기
spec <- read_html('https://www.jobkorea.co.kr/starter/spec?IsFavorOn=0&IsAlumniOn=0&Page=1', encoding = "UTF-8")


###################
#   데이터 확인   #
###################


# 기업 이름
html_text(html_nodes(spec, "li:nth-child(1) > div.coWrap > dl > dt > a.tit"))
html_text(html_nodes(spec, "li:nth-child(10) > div.coWrap > dl > dt > a.tit"))

# 산업 종류
html_text(html_nodes(spec, "li:nth-child(1) > div.coWrap > dl > dd:nth-child(2) > span"))
html_text(html_nodes(spec, "li:nth-child(10) > div.coWrap > dl > dd:nth-child(2) > span"))

# 매출액
html_text(html_nodes(spec, "li:nth-child(1) > div.coWrap > dl > dd:nth-child(3) > span"))
html_text(html_nodes(spec, "li:nth-child(10) > div.coWrap > dl > dd:nth-child(3) > span"))

# 학업성취도
html_text(html_nodes(spec, "li:nth-child(1) > div.listBarGraph > div > ul > li.item01 > span > span"))
html_text(html_nodes(spec, "li:nth-child(10) > div.listBarGraph > div > ul > li.item01 > span > span"))

# 외국어
html_text(html_nodes(spec, "li:nth-child(1) > div.listBarGraph > div > ul > li.item02 > span > span"))
html_text(html_nodes(spec, "li:nth-child(10) > div.listBarGraph > div > ul > li.item02 > span > span"))

# 전문능력
html_text(html_nodes(spec, "li:nth-child(1) > div.listBarGraph > div > ul > li.item03 > span > span"))
html_text(html_nodes(spec, "li:nth-child(10) > div.listBarGraph > div > ul > li.item03 > span > span"))

# 대외활동
html_text(html_nodes(spec, "li:nth-child(1) > div.listBarGraph > div > ul > li.item04 > span > span"))
html_text(html_nodes(spec, "li:nth-child(10) > div.listBarGraph > div > ul > li.item04 > span > span"))

# 스펙지수
html_text(html_nodes(spec, "li:nth-child(1) > div.spWrap > a > span"))
html_text(html_nodes(spec, "li:nth-child(10) > div.spWrap > a > span"))


#########################
#   크롤링 프로그래밍   #
#########################

spec_url <- "https://www.jobkorea.co.kr/starter/spec?IsFavorOn=0&IsAlumniOn=0&Page="
equal <- "li:nth-child("
cmpy_css <- ") > div.coWrap > dl > dt > a.tit"
idty_css <- ") > div.coWrap > dl > dd:nth-child(2) > span"
sales_css <- ") > div.coWrap > dl > dd:nth-child(3) > span"
achv_css <- ") > div.listBarGraph > div > ul > li.item01 > span > span"
lang_css <- ") > div.listBarGraph > div > ul > li.item02 > span > span"
skill_css <- ") > div.listBarGraph > div > ul > li.item03 > span > span"
act_css <- ") > div.listBarGraph > div > ul > li.item04 > span > span"
idx_css <- ") > div.spWrap > a > span"

company <- NULL
industry <- NULL
sales <- NULL
achieve <- NULL
language <- NULL
skill <- NULL
activity <- NULL
spec_index <- NULL

for (i in 1:279) {
  spec <- read_html(paste0(spec_url, i), encoding = "UTF-8")
  for (j in 1:10) {
    company <- c(company, html_text(html_nodes(spec, paste0(equal, j, cmpy_css))))
    industry <- c(industry, html_text(html_nodes(spec, paste0(equal, j, idty_css))))
    sales <- c(sales, html_text(html_nodes(spec, paste0(equal, j , sales_css))))
    achieve <- c(achieve, html_text(html_nodes(spec, paste0(equal, j , achv_css))))
    language <- c(language, html_text(html_nodes(spec, paste0(equal, j , lang_css))))
    skill <- c(skill, html_text(html_nodes(spec, paste0(equal, j , skill_css))))
    activity <- c(activity, html_text(html_nodes(spec, paste0(equal, j , act_css))))
    spec_index <- c(spec_index, html_text(html_nodes(spec, paste0(equal, j , idx_css))))
  }
  Sys.sleep(1)
}

spec_data <- data.frame(company, industry, sales, achieve, language, skill, activity, spec_index)
head(spec_data)
write.csv(spec_data, file = "spec.csv")

# https://www.jobkorea.co.kr/starter/spec?IsFavorOn=0&IsAlumniOn=0&Page=1
# https://www.jobkorea.co.kr/starter/spec?IsFavorOn=0&IsAlumniOn=0&Page=279