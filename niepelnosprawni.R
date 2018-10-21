setwd("~/RProjects/Wybory2018")

library(tidyverse)
library(sf)

okregi <- readRDS("lista_okregow.RDS")
colnames(okregi) <- c("nr_obwodu", "adres", "typ_obwodu", "niepelnosprawni", "teryt")

niepelnosprawni <- okregi %>%
  count(teryt, niepelnosprawni) %>%
  ungroup() %>%
  group_by(teryt) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(niepelnosprawni == "Tak") %>%
  select(teryt, p)

niepelnosprawni

gminy <- read_sf("~/RProjects/!mapy_shp/gminy.shp")

niepelnosprawni <- left_join(gminy %>%
                               select(jpt_kod_je, geometry) %>%
                               mutate(jpt_kod_je = str_sub(jpt_kod_je, 1, 6)),
                             niepelnosprawni,
                             by = c("jpt_kod_je" = "teryt"))


p <- ggplot(niepelnosprawni) +
  geom_sf(aes(fill = p), color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Dostępność lokali wyborczych dla osób niepełnosprawnych\nna poziomie gmin",
       fill = "% dostępnych\nlokali w gminie") +
  theme(legend.position = "bottom")

ggsave("niepelnosprawni.png", plot = p,
       width = 8, height = 6, units = "in", dpi = 300)
