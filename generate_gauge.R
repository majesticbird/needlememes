library(rvest)
library(stringr)

# pull argusdusty's numbers
arghouse <- read_html('https://elections.argusdusty.com/20181106/USHouse')
argsenate <- read_html('https://elections.argusdusty.com/20181106/USSen')

houseprob <- arghouse %>% html_nodes("p:nth-child(10)") %>% html_text(houseprob) %>% str_extract("\\d+\\.\\d+")
senprob <- argsenate %>% html_nodes("p:nth-child(10)") %>% html_text(houseprob) %>% str_extract("\\d+\\.\\d+")

# plots
house <- gg.gauge(as.numeric(houseprob), breaks=c(0,25,50,60,70,80,85,90,95,100))
senate <- gg.gauge.2(as.numeric(senprob), breaks=c(0,25,50,60,70,80,85,90,95,100))
