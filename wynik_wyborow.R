library(tidyverse)
library(sf)

# plik z mapą gmin
map_file_name <- "~/RProjects/!mapy_shp/gminy.shp"


# dane pobrane z https://wybory2018.pkw.gov.pl/pl/dane-w-arkuszach
# Wyniki wyborów samorządowych - protokoły

# scieżka do pliku z wynikami wyborów do Sejmików
file_name <- "wyniki_wyborow/sw.csv"

df <- read_csv2(file_name)



glosy <- df[, c(2, 23, 24, 28)] %>%
  set_names(c("TERYT", "kart_wydanych", "glosow_niewaznych", "glosow_waznych")) %>%
  mutate(TERYT = str_sub(TERYT, 1, 6)) %>%
  mutate(TERYT = if_else(str_detect(TERYT, "1465"), "146501", TERYT)) %>%
  distinct() %>%
  group_by(TERYT) %>%
  summarise(glosow_waznych = sum(glosow_waznych),
            glosow_niewaznych = sum(glosow_niewaznych),
            kart_wydanych = sum(kart_wydanych)) %>%
  ungroup() %>%
  mutate(procent_waznych = 100*glosow_waznych/kart_wydanych,
         procent_niewaznych = 100*glosow_niewaznych/kart_wydanych,)


wynik <- df[, c(2, 29, 30)] %>%
  set_names(c("TERYT", "lista", "glosow_na_liste")) %>%
  mutate(TERYT = str_sub(TERYT, 1, 6)) %>%
  mutate(TERYT = if_else(str_detect(TERYT, "1465"), "146501", TERYT)) %>%
  group_by(TERYT, lista) %>%
  summarise(glosow_na_liste = sum(glosow_na_liste)) %>%
  ungroup() %>%
  left_join(glosy, by = "TERYT") %>%
  mutate(procent_na_liste = 100*glosow_na_liste/kart_wydanych)


polska_mapa <- read_sf(map_file_name) %>%
  select(TERYT = jpt_kod_je, geometry) %>%
  mutate(TERYT = str_sub(TERYT, 1, 6))


wynik_plot <- left_join(polska_mapa,
                        wynik %>%
                          select(TERYT, lista, glosow_na_liste, procent_na_liste) %>%
                          distinct(),
                        by = "TERYT")


glosy_plot <- left_join(polska_mapa,
                        glosy %>%
                          distinct(),
                        by = "TERYT")


# wynik popracia
plot <- ggplot() +
  geom_sf(data = polska_mapa,
          size = 0.1, color = "gray90", fill = "white") +
  geom_sf(data = wynik_plot %>%
            filter(!is.na(lista)),
          aes(fill = procent_na_liste), size = 0.1, color = "gray90") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_void() +
  facet_wrap(~lista)

ggsave(plot, file = "winiki_list.png", width=20, height=20, dpi=300)


# pierwsze miejsca
wynik_plot %>%
  group_by(TERYT) %>%
  top_n(1, procent_na_liste) %>%
  ggplot() +
  geom_sf(aes(fill = lista), size = 0.1, color = "gray90") +
  theme_void()

# drugie miejsca
wynik_plot %>%
  group_by(TERYT) %>%
  arrange(desc(procent_na_liste)) %>%
  mutate(n = row_number()) %>%
  filter(n == 2) %>%
  ungroup() %>%
  ggplot() +
  geom_sf(aes(fill = lista), size = 0.1, color = "gray90") +
  theme_void()


# glosy niewazne
glosy_plot %>%
  ggplot() +
  geom_sf(aes(fill = procent_niewaznych), size = 0.1, color = "gray90") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_void()
