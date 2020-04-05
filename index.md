# Welcome to GitHub Pages


### Pendahuluan

Awal tahun 2020 sampai saat ini, dunia sedang digencarkan oleh suatu wabah virus yakni virus COVID-19. Namun, merebaknya wabah virus ini di Indonesia baru dimulai pada awal februari, hingga 2 minggu terakhir (sejak artikel ini ditulis) pemerintah menetapkan kebijakan untuk melakukan social distancing.

Dengan diberlakukannya social distancing, aktivitas di luar rumah semakin berkurang, banyak kantor yang diliburkan, begitupula dengan sekolah. Sehingga banyak orang - orang yang memilih "rebahan di rumah aja". Aktivitas yang biasanya dilakukan adalah bermain sosial media, salah satunya twitter.

Twitter lagi booming nih mengenai #dirumahaja, sehingga saya tertarik untuk melakukan eksplorasi data mengenai hashtag ini.

*Note : sebelumnya sudah dilakukan proses crawling data pada twitter, dengan mengambil 1000 tweet yang berhashtag #dirumahaja*

### Input Data
```{r}
setwd("D:\\DS\\R\\twitter")
data.frame<-read.csv("data.frame.csv", header=T)
```
```{r pressure, echo=FALSE}
library(RColorBrewer)
library(wordcloud)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```
