##function *vuldarities* is scrapping from wiki the vulgary words and phrasis into vector
##the output assign in var<-vulgarities()

vulgarities<-function(){
        library(rvest)
        library(htmltools)
        url='https://en.wiktionary.org/wiki/Category:English_vulgarities'
        main_url<-"https://en.wiktionary.org"
        vulgarities<-vector()
                # repeat used for the loop,could not deal with *for_loop error*(
                repeat{
                        names=read_html(url)%>%
                                html_nodes(xpath='//*[@class="mw-category-group"]') %>% 
                                html_text(trim = TRUE) %>%
                                strsplit(split = "\n") %>%
                                unlist()
                        vulgarities<-c(vulgarities,names)
                        nexturl<-read_html(url)%>%html_nodes('a')#list with all links
                        n_page <-grep("next page",nexturl)#list in the big list with text "next page"
                if (length(n_page)==0){break}
                        zz<-nexturl[n_page]
                        url<-paste0(main_url,xml_attrs(zz)[[1]][[1]])#a strange method to get url from the list,but working
                }

        vulgarities<- replace(vulgarities,vulgarities %in% LETTERS,NA)#substitute big alphabet
        vulgarities <- vulgarities[complete.cases(vulgarities)]#substitute big alphabet
        return(vulgarities)
}

##write the file line by line
fileConn <- file("vulgarities.txt")    
writeLines(vulgarities, fileConn)    
close(fileConn)