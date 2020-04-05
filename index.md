# Twitter Sentiment Analysis with R : Word Cloud


## Pendahuluan

Awal tahun 2020 sampai saat ini, dunia sedang digencarkan oleh suatu wabah virus yakni virus COVID-19. Namun, merebaknya wabah virus ini di Indonesia baru dimulai pada awal februari, hingga 2 minggu terakhir (sejak artikel ini ditulis) pemerintah menetapkan kebijakan untuk melakukan social distancing.

Dengan diberlakukannya social distancing, aktivitas di luar rumah semakin berkurang, banyak kantor yang diliburkan, begitupula dengan sekolah. Sehingga banyak orang - orang yang memilih "rebahan di rumah aja". Aktivitas yang biasanya dilakukan adalah bermain sosial media, salah satunya twitter.

Twitter lagi booming nih mengenai #dirumahaja, sehingga saya tertarik untuk melakukan eksplorasi data mengenai hashtag ini.

Pertama - tama, dilakukan pengambilan data dari twitter berupa 1000 tweet dengan hashtag #dirumahaja. Sebelumnya dilakukan setup authorization pada twitter sehingga mendapatkan api_key, api_secret_ access_token, dan access_sekret dari twitter developer.
https://developer.twitter.com/en

## Data Crawling Twitter
```{r}
#setup authorization
api_key<-"*******************************************"
api_secret<-"*******************************************"
access_token <- "*******************************************"
access_secret<- "*******************************************"
setup_twitter_oauth(api_key, api_secret, access_token, access_secret)

#mengambil data dari twitter
data<-searchTwitter("#dirumahaja", n=1000, lang="id")
data.frame<-do.call("rbind", lapply(data, as.data.frame))
View(data.frame)

#mengexport data frame
write.csv(data.frame, file="D:\\DS\\R\\twitter\\data.frame.csv", row.names=F)
```

## Data Cleaning
```{r}
library(tm)

#untuk melakukan cleaning data text diubah ke bentuk corpus
df<-VCorpus(VectorSource(data.frame$text))

#mengubah simbol jadi spasi
toSpace<-content_transformer(function(x, pattern) gsub(pattern," ",x))
df<-tm_map(df, toSpace, "@")
df<-tm_map(df, toSpace, "/")
df<-tm_map(df, toSpace,"\\|")

#menyeragamkan huruf ke dalam huruf kecil
df<-tm_map(df, content_transformer(tolower))

#menghapus tanda baca
df<-tm_map(df, toSpace, "[[:punct:]]")

#menghapus angka
df<-tm_map(df, toSpace, "[[:digit:]]")

#menghapus spasi yang berlebih
df<-tm_map(df, stripWhitespace)

#menghapus URL web
removeURL<-function(x) gsub("http[[:alnum:]]*", " ",x)
df<-tm_map(df, removeURL)

#menghapus RT
removeRT <- function(y) gsub("RT ", "", y)
df <- tm_map(df, removeRT)

#menghapus karakter selain huruf dan spasi
removeNumFuct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
df<-tm_map(df, removeNumFuct)

#menghapus stopwords
file_stop <- file("stopwords.txt",open = "r")
id_stopwords <- readLines(file_stop, warn=FALSE)
close(file_stop)
id_stopwords = c(id_stopwords, "amp")
df<-tm_map(df, removeWords, id_stopwords)

#menghapus kata
df<-tm_map(df, removeWords, c("dirumahaja","nih", "aja", "gak", "yuk", "engga", "youtub", "imunita","kalo","tau", "gimana", "sambatan", "yradianto", "biar", "kali","laoli", "nya", "charg", "gue", "utk", "onlin", "bikin", "ngapain", "ngak", "wkwkwk", "memantik", "ra...", "updat", "ancharyadi", "yasonna", "laoli", "wvojdfiqib","fpksdprri","decemb", "grati","bintangforza","kgrqeti", "gita", "mandiricard"))

#stem
library(SnowballC)
df<-tm_map(df, stemDocument)

#membuat data frame untuk data yang sudah di cleaning
df<-tm_map(df,PlainTextDocument)
dtm <- TermDocumentMatrix(df)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
View(d)
```

## Word Cloud
```{r}
library(RColorBrewer)
library(wordcloud)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```
![Rplot02](https://user-images.githubusercontent.com/60332569/78465665-18b47480-772b-11ea-906e-35c1c2d7ff18.png)


Berikut Top 10 kata yang banyak digunakan dalam #dirumahaja :

```{r}
library(dplyr)
data_new<-top_n(d,10)
data_new
```
Tabel :

|word     |freq     |
|---------|---------|
|corona	|87       |			
|covid	|85	|		
|anak	|58	|		
|udah	|55	|		
|rumah	|51	|		
|minggu	|50	|		
|virus	|50	|		
|lakukan	|43	|		
|pagi	|41	|		
|motion	|38	|


## Histogram

![Rplot03](https://user-images.githubusercontent.com/60332569/78465751-11419b00-772c-11ea-9344-eebed3a80646.png)


*Yulika Trisna 4/4/2020*
