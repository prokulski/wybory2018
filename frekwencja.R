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

  page_url <- glue("https://wybory2018.pkw.gov.pl/pl/frekwencja/{TERYT}#f1000000")

  page <- read_html(page_url)

  trs <- page %>%
    html_node("table.stat_table_dt") %>%
    html_node("tbody") %>%
    html_nodes("tr")

  gmina <- tibble()

  for(i in 1:length(trs)) {
    tds <- trs[[i]] %>% html_nodes("td")

    gm_TERYT <- tds[[1]] %>% html_node("a") %>% html_attr("href") %>% str_replace_all("/pl/frekwencja/|#f1000000", "")
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
wyniki_frek <- bind_rows(wyniki_frek,
                         tibble(TERYT = "146501",
                                frekwencja = wyniki_frek %>%
                                  filter(str_sub(TERYT, 1, 5) == "14650") %>%
                                  pull(frekwencja) %>%
                                  mean()))


# poprawka na gminę 320304 (likwidowana) - uśrednienie do całego powiatu
wyniki_frek <- bind_rows(wyniki_frek,
                         tibble(TERYT = "320304",
                                frekwencja = wyniki_frek %>%
                                  filter(str_sub(TERYT, 1, 4) == "3203") %>%
                                  pull(frekwencja) %>%
                                  mean()))


# dodajemy poziomy
wyniki_frek <- wyniki_frek %>%
  mutate(frekwencja_przed = cut(frekwencja, breaks = seq(0, 100, 10)))


# ile jest przedziałów?
n_przed <- wyniki_frek %>% count(frekwencja_przed) %>% filter(n != 0) %>% nrow()

# na poziomie gmin - dodanie danych do mapy
wyniki_mapa <- left_join(gminy_mapa %>% mutate(jpt_kod_je = str_sub(jpt_kod_je, 1, 6)),
                         wyniki_frek,
                         by = c("jpt_kod_je" = "TERYT"))

# dane ciągłe
p1 <- ggplot() +
  geom_sf(data = wyniki_mapa,
          aes(fill = frekwencja), size = 0.1, color = "gray40") +
  geom_sf(data = powiaty_mapa, fill = NA, color = "gray70", size = 0.2) +
  geom_sf(data = wojewodztwa, fill = NA, color = "gray90", size = 0.4) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Frekwencja w wyborach samorządowych 2018\nna poziomie gmin",
       subtitle = "Dane końcowe",
       caption = "(c) Lukasz Prokulski, fb.com/DaneAnalizy",
       fill = "Frekwencja")

ggsave(p1, file = "frekwencja_gminy_1.png", width = 7, height = 7, dpi = 300)


# przedziałami
p2 <- ggplot() +
  geom_sf(data = wyniki_mapa,
          aes(fill = frekwencja_przed), size = 0.1, color = "gray40") +
  geom_sf(data = powiaty_mapa, fill = NA, color = "gray30", size = 0.2) +
  geom_sf(data = wojewodztwa, fill = NA, color = "gray10", size = 0.4) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(n_przed, "RdYlBu"))) +
  labs(title = "Frekwencja w wyborach samorządowych 2018\nna poziomie gmin",
       subtitle = "Dane końcowe",
       caption = "(c) Lukasz Prokulski, fb.com/DaneAnalizy",
       fill = "Frekwencja")

ggsave(p2, file = "frekwencja_gminy_2.png", width = 7, height = 7, dpi = 300)



# na poziomie powiatów (uśrednione z gmin)
wyniki_pow <- wyniki_frek %>%
  mutate(TERYT = str_sub(TERYT, 1, 4)) %>%
  group_by(TERYT) %>%
  summarise(m_frek = mean(frekwencja)) %>%
  ungroup() %>%
  mutate(frekwencja_przed = cut(m_frek, breaks = seq(0, 100, 10)))



wyniki_pow_mapa <- left_join(wyniki_pow,
                             powiaty_mapa,
                             by = c("TERYT" = "jpt_kod_je"))

# ciągłe
p3 <- ggplot() +
  geom_sf(data = wyniki_pow_mapa,
          aes(fill = m_frek), size = 0.1, color = "gray40") +
  geom_sf(data = wojewodztwa, fill = NA, color = "gray90", size = 0.4) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Frekwencja w wyborach samorządowych 2018\nna poziomie powiatów",
       subtitle = "Dane końcowe, uśrednione z danych gminnych do powiatów",
       caption = "(c) Lukasz Prokulski, fb.com/DaneAnalizy",
       fill = "Frekwencja")

ggsave(p3, file = "frekwencja_powiaty_1.png", width = 7, height = 7, dpi = 300)


# ile jest przedziałów?
n_przed <- wyniki_pow %>% count(frekwencja_przed) %>% filter(n != 0) %>% nrow()


# przedziałami
p4 <- ggplot() +
  geom_sf(data = wyniki_pow_mapa,
          aes(fill = frekwencja_przed), size = 0.1, color = "gray40") +
  geom_sf(data = wojewodztwa, fill = NA, color = "gray10", size = 0.4) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(n_przed, "RdYlBu"))) +
  labs(title = "Frekwencja w wyborach samorządowych 2018\nna poziomie powiatów",
       subtitle = "Dane końcowe, uśrednione z danych gminnych do powiatów",
       caption = "(c) Lukasz Prokulski, fb.com/DaneAnalizy",
       fill = "Frekwencja")

ggsave(p4, file = "frekwencja_powiaty_2.png", width = 7, height = 7, dpi = 300)
