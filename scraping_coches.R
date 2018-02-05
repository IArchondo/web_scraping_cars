library(stringr)
library(dplyr)
library(rvest)
library(RCurl)
library(XML)


urls <- "https://www.coches.com/coches-segunda-mano/coches-ocasion-en-madrid.htm"

for (i in 1:50){
url <- "https://www.coches.com/coches-segunda-mano/coches-ocasion-en-madrid.htm"
urls[i+1] <- paste(url,"?page=",i,sep="")
}

get_urls <- function(url){
html <- paste(readLines(url),collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"")
a <- matched[[1]]
urls_base <- a[34:73,2]
}

urls_base <- lapply(urls,get_urls) %>% unlist

i <- 0
f.get_car_data <- function(url_c){
  i <<- i+1
  print(i)
  if (i%%100==0){  #avoid overpowering the site
    print("pausa pedagogica")
    Sys.sleep(60)
  }
  print(url_c)
  dat <- html_nodes(read_html(url_c),'strong') %>%  html_text #we filter the cars characteristics
  titulo <- (html_nodes(read_html(url_c),'.cc_model_price') %>% html_text %>% str_split("        ",4) 
             %>% unlist)[c(1,3)] #and the cars type and subtype
  if (length(titulo)==0){} else{ #we control for the ads that have been recently taken out of the platform
  match <- grep(" l",dat2)[1] #in order to avoid errors due to bold letters used in the description, we
                              #locate the start of the second table of characteristics
  
  df.datos <- data.frame(nombre=titulo[1],subnombre=titulo[2],
                         precio=dat[19],year=dat[20],potencia=dat[21],
                         km=dat[22],combustible=dat[23],puertas=dat[24],
                         long=dat[match+1],alt=dat[match+2],anch=dat[match+3],peso=dat[match+7],
                         url=url_c)
  print(paste(titulo[1],"--> listo",sep=" "))
  return(df.datos)
  }
}

df.result <- lapply(urls_base,f.get_car_data) %>% bind_rows



prueba <- c("https://www.coches.com/coches-segunda-mano/ocasion-hyundai-ix35-20crdi-gls-style-sky-nav-4x4-en-madrid.htm?id=2914074",
            url_c,
            "https://www.coches.com/coches-segunda-mano/ocasion-audi-q2-14-tfsi-cod-design-edition-s-tronic-150-en-madrid.htm?id=2912481")
url_c <- "https://www.coches.com/coches-segunda-mano/ocasion-volvo-xc60-d3-kinetic-aut-en-madrid.htm?id=2910998"

l.prueb <- lapply(prueba,f.get_car_data) %>% bind_rows()

lapply(c(1,2,3,4,5,6),metr)

