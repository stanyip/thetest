library(stringr) # regular expression
library(dplyr) # data manipulation
date0 = c("20170824","20170824","20170824","20170824","20170824","20170824","20170824","20170824","20170824","20170824")
time0 = c("10:28:13","10:28:13","10:28:13","10:28:14","10:28:14","10:28:15","10:28:17","10:28:17","10:28:17","10:28:17")
size0 = c(13500,7050,18400,6543,150,24130,3401,5017,7210,10)
url = c("/media/thumbnail/fd1140af02.jpg","/media/thumbnail/8b527f7ca8.jpg","/media/thumbnail/b2670bc44f.jpg",
        "/media/thumbnail/bdb3b41b8b.jpg","/media/main.js","/media/thumbnail/8124c8c648.jpg","/media/thumbnail/2ab5f8593a.jpg",
        "/media/thumbnail/64f6312f93.jpg","/media/thumbnail/458f99f4d3.jpg","/media/style.css")
sampledataq2 = data.frame(date=date0,time=time0,size=size0,url=url)
write.table(sampledataq2,"q2.csv",row.names=F,sep = "\t")
dat =read.table("q2.csv",sep="\t",header=T) # read the data from a tab separated log file
dat = mutate(dat,datetime = str_replace_all(paste0(date,time),"(:|-|\\s)",""))
sum(filter(dat,str_detect(url,"(\\.jpg)"), datetime <= "20170825235959", datetime > "20170824000000")$size)
