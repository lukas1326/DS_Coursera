Sys.setlocale("LC_ALL",locale = "Ukrainian")

library(pdftools)
download.file("https://www.gubkin.ru/intranet/docs_smk/ISO_9001-2015(R).pdf","iso.pdf")
text<- read_lines("New Text Document.txt")
listISO<-grep("-\\s\\ISO\\s\\d{4}|-\\s\\ISO/TR\\s\\d{5}",text)
ISOtext<-vector()
for (i in 1:17){
        k<-listISO[i+1]-1
        m<-listISO[i]
        zeta<-str_flatten(text[m:k],collapse = " ")
        ISOtext <-c(ISOtext,zeta)
}
ISOtext[18]<-str_flatten(text[listISO[18]:109],collapse = " ")
df<-data.frame(matrix(nrow=18,ncol=2))
df[,1]<-str_extract(ISOtext,"-\\s\\ISO\\s\\d{4,5}|-\\s\\ISO/TR\\s\\d{5}")
df[,2]<-str_remove(ISOtext,"-\\s\\ISO\\s\\d{4,5}|-\\s\\ISO/TR\\s\\d{5}")
df[,1]<-str_remove(df[,1],"- ")


listISO<-grep("\\[\\d",text)
ISOtext<-vector()
for (i in 1:22){
        k<-listISO[i+1]-1
        m<-listISO[i]
        zeta<-str_flatten(text[m:k],collapse = " ")
        ISOtext <-c(ISOtext,zeta)
}
ISOtext[23]<-str_flatten(text[listISO[23]:141],collapse = " ")
df<-data.frame(matrix(nrow=23,ncol=2))
df[,1]<-str_extract(ISOtext,"\\S+\\s\\d+")
df[,2]<-str_remove(ISOtext,df[,1])
df[,2]<-str_remove(df[,2],"\\[\\d+\\]\\s\\,\\s")
