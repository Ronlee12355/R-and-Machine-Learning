library(XML)
library(RCurl)
myHttpheader <- c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)
data=read.table("E:/repeat_paper/aaa.txt",stringsAsFactors = F)
num<-nrow(data)
tt=list(c(1:num))
result<-data.frame(data,tt)
for(i in 1:num){
  a=c("http://regulomedb.org/GWAS/",result$V1[i],"_r2thr0.8_all.html")
  url1 <-paste(a[1],a[2],a[3],sep = "")
  webpage1 <- getURL(url1,httpheader=myHttpheader)
  pagetree1 <- htmlTreeParse(webpage1,encoding="UTF-8", error=function(...){}, useInternalNodes = TRUE,trim=TRUE)
  node1<-getNodeSet(pagetree1, "//*[@id='snp_content']/p[2]/a[2]/text()")
  trait<-sapply(node1,xmlValue)
  result$X1.4724[i]<-trait
  Sys.sleep(0.01)
}
re=data.frame(result)
write.table(re,file = "E:/repeat_paper/snp_trait.txt",col.names = F,row.names = F,quote=F,sep="\t")
