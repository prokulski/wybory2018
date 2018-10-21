setwd("~/RProjects/Wybory2018")

library(tidyverse)
library(rvest)
library(glue)

kody_TERYT <- read_csv2("~/RProjects/!mapy_shp/TERC_Urzedowy_2018-08-30.csv")

kody_TERYT <- kody_TERYT %>%
  filter(!is.na(GMI)) %>%
  mutate(TERYT = paste0(WOJ, POW, GMI))


get_obwod <- function(TERYT) {

  url <- glue("https://wybory2018.pkw.gov.pl/pl/geografia/{TERYT}#pollstations")

  page <- read_html(url)

  tabelka <- page %>%
    html_nodes("main.content") %>%
    html_nodes("div.container") %>%
    html_nodes("div.tab_box_pollstations") %>%
    html_node("table") %>%
    html_table() %>%
    .[[1]] %>%
    mutate(TERYT = TERYT)

  return(tabelka[, -5])
}


tabela <-  tibble()

i <- 0



j <- i + 1
for(i in j:nrow(kody_TERYT)) {
  tabelka <- get_obwod(kody_TERYT$TERYT[i])
  tabela <- bind_rows(tabela, tabelka)
}


kody_TERYT[i, ]

tabela <- distinct(tabela)



saveRDS(tabela, "lista_okregow.RDS")

