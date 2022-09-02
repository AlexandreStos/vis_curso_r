# Trabalho final de vizualização de dados ---------------------------------


# base de dados do #tidytuesday do dia 08-10-2019 ------------------------------------
# tema: International Powerlifting


# habilitando as bibliotecas ----------------------------------------------

library(tidyverse)
install.packages("gganimate")
library(gganimate)
installed.packages("ggdark")
devtools::install_github("nsgrantham/ggdark")

install.packages("ggplot2")

# baixando a base ---------------------------------------------------------

lev_peso <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv"
  )

# manipulando a base


# removendo atletas desclassificados por doping ou excluidos por outros motivos ---------------------------

lev_peso_1 <- lev_peso |>
  filter(
    place != "DD", 
    place != "DQ", 
    place != "NS",
    place != "G"
)


# removendo lev_peso ------------------------------------------------------

rm(lev_peso)

# Renomeando as colunas em português (rm= repetição máxima)------------

lev_peso_1 <- lev_peso_1 |>
  rename(
    rm_supino_kg = best3bench_kg,
    rm_agach_kg = best3squat_kg,
    rm_terra_kg = best3deadlift_kg,
    idade = age,
    sexo = sex,
    nome = name,
    faixa_etaria = age_class,
    divisao = division,
    data_evento = date,
    federacao = federation, 
    massa_kg = bodyweight_kg,
    categ_peso = weight_class_kg,
    colocacao = place
  )

# criando as colunas de carga relativa (rm/massa corporal) ----------------

lev_peso_1 <- lev_peso_1 |>
  mutate(
    carga_rel_sup = rm_supino_kg / massa_kg,
    carga_rel_agach = rm_agach_kg / massa_kg,
    carga_rel_terra = rm_terra_kg / massa_kg
  ) 

# arredondando as cargas relativas com a função scales ----------------------------------------

lev_peso_1 <- lev_peso_1 |>
  mutate(
    carga_rel_sup = scales::number(carga_rel_sup, accuracy = 0.01, decimal.mark = "."),
    carga_rel_agach = scales::number(
      carga_rel_agach,
      accuracy = 0.01,
      decimal.mark = "."
    ),
    carga_rel_terra = scales::number(
      carga_rel_terra,
      accuracy = 0.01,
      decimal.mark = "."
    )
  )
  

# Criando a coluna ano da competição extraindo com lubridate::year() ----------------------------------------------------

lev_peso_1 <- lev_peso_1 |>
  mutate(ano = lubridate::year(data_evento))

# transformando ano em fator ----------------------------------------------

lev_peso_1 <- lev_peso_1 |> 
  mutate(ano = as.factor(ano))

# class(lev_peso_1$ano) 


# transformando colunas cargas em número ----------------------------------

lev_peso_1 <- lev_peso_1 |>
  mutate(
    carga_rel_agach = as.double(carga_rel_agach),
    carga_rel_sup = as.double(carga_rel_sup),
    carga_rel_terra = as.double(carga_rel_terra)
  )

# carga relativa por ano --------------------------------------------------

lev_peso_1 |>
  select(carga_rel_agach, carga_rel_sup, carga_rel_terra, ano) |>
  group_by(ano) |>
  summarise(
    `Média` = mean(carga_rel_agach, na.rm = TRUE),
    `Desvio padrão` = sd(carga_rel_agach, na.rm = TRUE),
    Max = max(carga_rel_agach, na.rm = TRUE)
  ) 

# carga relativa por ano --------------------------------------------------

  
lev_peso_1 |>
  select(carga_rel_agach, carga_rel_sup, carga_rel_terra, ano) |>
  ggplot()+
  aes(x= ano, y= carga_rel_agach, fill= carga_rel_agach, colours(distinct = "blue" ))+
  geom_boxplot()+
  scale_y_continuous(
    breaks = seq(0,7,1),)+
  scale_x_discrete(
    breaks = seq(1973,2019,7),)+
  labs(x= " Anos",
       y= "Carga relativa agachamento",
       subtitle = "Arrecadação somada dos 3 principais jogadores da Forbes")+
  labs(title = "Maiores arrecadações no futebol", hjust = 1)+
theme_gray() 


# correlação agachamento x massa corporal ---------------------------------

g2 <- lev_peso_1 |>
  select(carga_rel_agach, carga_rel_sup, carga_rel_terra, ano, massa_kg, rm_agach_kg) |>
  ggplot()+
  aes(x = massa_kg, y= rm_agach_kg, color = rm_agach_kg)+
  geom_point(size=2)+
  geom_smooth(method=lm, color="#013440", size=0.9)+
  scale_y_continuous(
    breaks = seq(0,650,50),)+
  scale_x_continuous(
    breaks = seq(0,200,20),)+
  labs(x= "Massa corpora (kg)",
       y= "Carga máxima (kg)",
       subtitle = "Correlação entre carga maxíma e massa corporal no agachamento")+
  labs(title = "Resultados de campeonatos de levantamento de peso", vjust = 0.5)+
  theme_gray()
  
# correlação supino x massa corporal ---------------------------------

g3 <- lev_peso_1 |>
  select(massa_kg, rm_agach_kg, rm_supino_kg, idade) |>
  ggplot()+
  aes(x = massa_kg, y= rm_supino_kg, color = rm_supino_kg)+
  geom_point(size=2)+
  geom_smooth(method=lm, color="#013440", size=0.9)+
  scale_y_continuous(
    breaks = seq(0,650,50),)+
  scale_x_continuous(
    breaks = seq(0,200,20),)+
  labs(x= "Massa corpora (kg)",
       y= "Carga máxima supino (kg)",
       subtitle = "Correlação entre carga máxima e massa corporal no supino")+
  labs(title = "Resultados de campeonatos de levantamento de peso", vjust = 0.5)+
  theme_gray() 


# correlação supino x massa corporal ---------------------------------

g4 <- lev_peso_1 |>
  select(massa_kg, rm_terra_kg, rm_supino_kg, idade) |>
  ggplot()+
  aes(x = massa_kg, y= rm_terra_kg, color = rm_terra_kg)+
  geom_point(size=2)+
  geom_smooth(method=lm, color="#013440", size=0.9)+
  scale_y_continuous(
    breaks = seq(0,650,50),)+
  scale_x_continuous(
    breaks = seq(0,200,20),)+
  labs(x= "Massa corpora (kg)",
       y= "Carga máxima levantamento terra (kg)",
       subtitle = "Correlação entre carga máxima e massa corporal no levantamento terra")+
  labs(title = "Resultados de campeonatos de levantamento de peso", vjust = 0.5)+
  theme_gray()

# correlação idade e carga maxima no terra ---------------------------------

a1 <-  lev_peso_1 |>
  select(massa_kg, rm_terra_kg, rm_supino_kg, rm_agach_kg, idade) |>
  filter(idade > 10) |> 
  ggplot()+
  aes(x = idade, y= rm_terra_kg, color = rm_terra_kg, show.legend = FALSE)+
  geom_line(size= 4.5)+
   scale_y_continuous(
    breaks = seq(0,650,50),)+
  scale_x_continuous(
    breaks = seq(10,90,5),)+
  labs(x= "Idade (anos)",
       y= "Carga máxima levantamento terra (kg)",
       title = "Resultados de campeonatos de levantamento de peso", hjust = 0.5)+
  theme_gray()
a1 + dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 12) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank())+
  transition_time(idade)

lev_peso_1 |> 
  select(idade, rm_terra_kg) |> 
  ggplot()+
  aes(x = idade, y = rm_terra_kg, color= "blue")+
  geom_point(size=4)+
  scale_y_continuous(
    breaks = seq(0,650,50),)+
  scale_x_continuous(
    breaks = seq(10,90,5),)+
  labs(x= "Idade (anos)",
       y= "Carga máxima no agachamento (kg)",
       title = "Resultados de campeonatos de levantamento de peso", hjust = 0.5)+
  theme_gray()+  
  transition_time(idade) 

lev_peso_1 |>
  select(rm_agach_kg, idade, sexo) |>
  filter(idade > 10, sexo =="M") |> 
  group_by(idade) |> 
  summarise("Carga máxima" = round(mean(rm_agach_kg,na.rm=TRUE))) |>
  select("Idade"= idade, `Carga máxima`) |> 
  ggplot()+
  aes(x = Idade, y= `Carga máxima`, show.legend = FALSE)+
  geom_line(size=1)+
  scale_y_continuous(
    breaks = seq(0,650,50),
    labels = paste(seq(0,650,50), "kg"))+
  scale_x_continuous(
    breaks = seq(10,90,5),)+
  labs(x= "Idade (anos)",
       y= "Carga máxima",
       title = "Resultados do agachamento na IPF", hjust = 0.5)+
  theme_gray()+
  transition_time(Idade)+
  geom_polygon(aes(x=Idade, y=`Carga máxima`))
   

 ggplot(airquality, aes(Day, Temp)) +
  geom_point(aes(colour = factor(Month))) +
  transition_time(Day)

 lev_peso_1 |>
   select(rm_agach_kg, idade, sexo) |>
   filter(idade > 10, sexo =="M") |> 
   group_by(idade) |> 
   summarise("Carga máxima" = round(mean(rm_agach_kg,na.rm=TRUE))) |>
   select("Idade"= idade, `Carga máxima`) |> 
   group_by(Idade) |> 
   ggplot()+
   aes(x = Idade, y= `Carga máxima`, show.legend = FALSE)+
   geom_point(size= 4.5)+
     labs(x= "Idade (anos)",
        y= "Carga máxima levantamento terra (kg)",
        title = "Resultados de campeonatos de levantamento de peso", hjust = 0.5)+
   theme_gray()+
   transition_time(Idade)
