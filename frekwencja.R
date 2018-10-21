library(tidyverse)
library(lubridate)
library(rvest)
library(glue)

library(sf)


theme_set(theme_minimal() +
                theme(plot.title = element_text(family = NULL, face = "bold", size = 18, color = "black", hjust = 0.5),
                      plot.subtitle = element_text(family = NULL, face = "plain", size = 9, color = "black"),
                      plot.caption = element_text(family = NULL, face = "italic", size = 9, color = "darkgray"),
                      panel.background = element_rect(fill="#efefef", color="#efefef"),
                      plot.background = element_rect(fill = "white", color="white"),
                      strip.text.x = element_text(face = "bold"),
                      axis.text = element_blank()))

gminy_mapa <- read_sf("~/RProjects/!mapy_shp/gminy.shp") %>%
  select(jpt_kod_je, geometry)

powiaty_mapa <- read_sf("~/RProjects/!mapy_shp/powiaty.shp") %>%
  select(jpt_kod_je, geometry)

wojewodztwa <- read_sf("~/RProjects/!mapy_shp/wojewodztwa.shp") %>%
  select(jpt_kod_je, jpt_nazwa_, geometry)


lista_powiatow <- read_csv2("~/RProjects/!mapy_shp/TERC_Urzedowy_2018-08-30.csv")

lista_gmin <- lista_powiatow %>%
  filter(!is.na(GMI)) %>%
  mutate(TERYT = sprintf("%s%s%s", WOJ, POW, GMI)) %>%
  select(TERYT, NAZWA) %>%
  distinct(TERYT, .keep_all = TRUE)


lista_powiatow <- lista_powiatow %>%
  filter(!is.na(GMI)) %>%
  mutate(TERYT = sprintf("%s%s00", WOJ, POW)) %>%
  select(TERYT, NAZWA) %>%
  distinct(TERYT, .keep_all = TRUE)

# poprawka na Warszawę
lista_powiatow[str_sub(lista_powiatow$TERYT, 1, 4) == "1465", "TERYT"] <- "146501"


getFrekwencja <- function(TERYT) {
  Sys.sleep(sample(seq(0.1, 1, 0.1), size = 1))

  page_url <- glue("https://wybory2018.pkw.gov.pl/pl/frekwencja/{TERYT}#f2")

  page <- read_html(page_url)

  trs <- page %>%
    html_node("table.stat_table_dt") %>%
    html_node("tbody") %>%
    html_nodes("tr")

  gmina <- tibble()

  for(i in 1:length(trs)) {
    tds <- trs[[i]] %>% html_nodes("td")

    gm_TERYT <- tds[[1]] %>% html_node("a") %>% html_attr("href") %>% str_replace_all("/pl/frekwencja/|#f2", "")
    gm_frekwencja <- tds[[6]] %>% html_text() %>% str_replace_all("\\%", "") %>% as.numeric()

    gmina <- bind_rows(gmina, tibble(TERYT = gm_TERYT, frekwencja = gm_frekwencja))
  }

  return(gmina)
}


wyniki_frek <- tibble()

for(i in 1:nrow(lista_powiatow)) {
  cat(sprintf("\r%d / %d", i, nrow(lista_powiatow)))
  wyniki_frek <- bind_rows(wyniki_frek,
                           getFrekwencja(as.character(lista_powiatow[i, "TERYT"])))

}

wyniki_frek <- distinct(wyniki_frek)

# poprawka dla warszawy (uśrednienie do całej z dzielnic)
wyniki_frek_wawa <- wyniki_frek %>%
  filter(str_sub(TERYT, 1, 5) == "14650")

wyniki_frek <- bind_rows(wyniki_frek,
                         tibble(TERYT = "146501",
                                frekwencja = mean(wyniki_frek_wawa$frekwencja)))




# na poziomie gmin

wyniki_mapa <- left_join(gminy_mapa %>% mutate(jpt_kod_je = str_sub(jpt_kod_je, 1, 6)),
                         wyniki_frek,
                         by = c("jpt_kod_je" = "TERYT"))

ggplot() +
  geom_sf(data = wyniki_mapa,
          aes(fill = frekwencja), size = 0.1, color = "gray40") +
  geom_sf(data = wojewodztwa, fill = NA, color = "gray90", size = 0.4) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Frekwencja w wyborach samorządowych 2018\nna poziomie gmin",
       subtitle = "Dane z godziny 17:00",
       caption = "(c) Lukasz Prokulski, fb.com/DaneAnalizy",
       fill = "Frekwencja")



# na poziomie powiatów (uśrednione z gmin)

wyniki_pow <- wyniki_frek %>%
  mutate(TERYT = str_sub(TERYT, 1, 4)) %>%
  group_by(TERYT) %>%
  summarise(m_frek = mean(frekwencja)) %>%
  ungroup()

wyniki_pow_mapa <- left_join(wyniki_pow,
                             powiaty_mapa,
                             by = c("TERYT" = "jpt_kod_je"))

ggplot() +
  geom_sf(data = wyniki_pow_mapa,
          aes(fill = m_frek), size = 0.1, color = "gray40") +
  geom_sf(data = wojewodztwa, fill = NA, color = "gray90", size = 0.4) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Frekwencja w wyborach samorządowych 2018\nna poziomie powiatów",
       subtitle = "Dane z godziny 17:00, uśrednione z danych gminnych do powiatów",
       caption = "(c) Lukasz Prokulski, fb.com/DaneAnalizy",
       fill = "Frekwencja")