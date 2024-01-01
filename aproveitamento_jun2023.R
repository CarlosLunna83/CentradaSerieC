# Aproveitamento das equipes da série C no mês de Junho de 2023 (considerando só jogos oficiais)

# Objetivo:
# - Criar script para automatizar aproveitamento dos times da série C, mês a mês

# Vídeos auxiliares do gráfico https://www.youtube.com/watch?v=oY8ZzmRwqxs
#                              https://www.youtube.com/watch?v=_mvT1kxkx60
#                              https://www.youtube.com/watch?v=8h8-7Z31Wj0
# Post auxiliar do gráfico https://pt.stackoverflow.com/questions/407685/gr%C3%A1fico-de-barras-no-ggplot-no-r

library(tidyverse)
library(ggplot2)
library(readODS)
library(janitor)
library(scales)
library(installr)


times_serieC2023_geral <- read_ods("Central da Serie C 2023/jogos dos times da serie C 2023.ods")

colnames(times_serieC2023_geral)


aproveitamento_times_seriec_jun23 <- times_serieC2023_geral %>%
  select(time, pontos_ganhos, meses, qtd_unidade) %>%
  filter(str_detect(meses, "junho")) %>%
  group_by(time) %>%
  summarise(pontos_ganhos_geral = sum(pontos_ganhos),
            qtd_pontos_disputados = sum(qtd_unidade)*3,
            ranking_aproveitamento = pontos_ganhos_geral/qtd_pontos_disputados)%>%
  arrange(desc(ranking_aproveitamento))

aproveitamento_times_seriec_jun23



# Gráfico:


aproveitamento_times_seriec_jun23$time <- factor(aproveitamento_times_seriec_jun23$time, 
                                                 levels = unique(aproveitamento_times_seriec_jun23$time))

aproveitamento_times_seriec_jun23 %>%
  ggplot(aes(reorder(x = time, ranking_aproveitamento), y = ranking_aproveitamento)) +
  geom_bar(stat = "identity", fill = "gray", color = "black", alpha = 0.5) +
  coord_flip() +  
  scale_y_continuous(labels = percent_format()) +
  geom_label(aes(label = percent(ranking_aproveitamento, accuracy = 0.1)),
             position = position_dodge(0.9), 
             vjust = 0.5, size = 2.5, hjust = 0.5) +
  labs(title = "Aproveitamento times da Série C em Junho 2023",
       x = "Times",
       y = "Aproveitamento")


# Jogos disputados por cada equipe em Junho 2023:

jogos_disputados_jun23 <- times_serieC2023_geral %>%
  select(time, meses, qtd_unidade) %>%
  filter(str_detect(meses, "junho")) %>%
  group_by(time) %>%
  summarise(jogos_disputados = sum(qtd_unidade)) %>%
  arrange(desc(jogos_disputados))

jogos_disputados_jun23
