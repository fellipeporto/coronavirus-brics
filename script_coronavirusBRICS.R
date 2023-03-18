
install.packages('ggthemes')
install.packages('extrafont')
install.packages('tidyverse')
# carregando os pacotes


library(dplyr)
library(zoo)
library(lubridate)
library(patchwork)
library(coronavirus)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(tidyverse)

font_import()

# explorando os dados 

data("coronavirus")
head(coronavirus)

## vendo casos totais

coronavirus %>%
  filter(type == 'confirmed') %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  print(n = 30)

## casos totais da china

coronavirus %>%
  filter(type == 'confirmed', country == 'China') %>%
  summarise(total_cases = sum(cases))
 

## evolução de casos confirmados nos países do brics

#### filtrando os dados apenas para os países do brics
brics_cases <- coronavirus %>%
  filter(country %in% c('Brazil','Russia','India', 'South Africa'), type == 'confirmed')%>%
  select(date, country, type, cases) %>%
  tibble::as.tibble()

china_casos <- coronavirus %>%
  filter(country %in% c('China'), type == 'confirmed')%>%
  select(date, country, type, cases)%>%
  group_by(date)%>%
  mutate(tot_cases = sum(cases)) %>%
  select(-cases) %>%
  distinct()%>%
  rename(cases = tot_cases) %>%
  tbl_df()

brics_cases <- brics_cases %>% rbind(china_casos) %>%
  mutate(casos_acumulados = cumsum(cases))



#### plotando 

brics_cases %>% 
  ggplot(aes(x = date, y = cases, col = country))+
  geom_line(size = 1)+
  scale_color_discrete(name = 'Países', labels = c('Brasil', 'China', 'Índia', 'Rússia', 'África do Sul'))+
  theme_tufte()+
  theme(text = element_text(family = 'Rubik'), legend.position = 'top')+
  labs(title = 'Evolução de casos dos países do BRICS', x = 'Data', y = 'Casos')
    


## numero de óbitos 

brics_deaths <- coronavirus %>%
  filter(country %in% c('Brazil','Russia','India', 'South Africa'), type == 'death')%>%
  select(date, country, type, cases) %>%
  tibble::as.tibble()

china_mortes <- coronavirus %>%
  filter(country %in% c('China'), type == 'death')%>%
  select(date, country, type, cases)%>%
  group_by(date)%>%
  mutate(tot_cases = sum(cases)) %>%
  select(-cases) %>%
  distinct()%>%
  rename(cases = tot_cases) %>%
  tbl_df()

brics_deaths <- brics_deaths %>% rbind(china_mortes) %>%
  mutate(casos_acumulados = cumsum(cases))

#### plotando

brics_deaths %>% 
  ggplot(aes(x = date, y = cases, col = country))+
  geom_line(size = 1)+
  scale_color_discrete(name = 'Países', labels = c('Brasil', 'China', 'Índia', 'Rússia', 'África do Sul'))+
  theme_tufte()+
  theme(text = element_text(family = 'Rubik'), legend.position = 'top')+
  labs(title = 'Evolução de óbitos dos países do BRICS', x = 'Data', y = 'Mortes')


## média móvel por pais 


brics_deaths %>%
  group_by(country) %>%
  mutate(mediaMovel = zoo::rollmean(cases, k = 7, fill = NA))%>%
  tbl_df()%>%
  ggplot(aes(x=date)) +  
  geom_bar(aes(y = cases), stat = "identity", fill = "grey") +
  geom_line(mapping=aes(x = date, y = mediaMovel), color="red")+
  facet_wrap(~country, ncol = 2, scales = 'free')+
  theme_tufte()+
  theme(text = element_text(family = 'Rubik'))+
  labs(title = "Óbitos nos países do BRICS",
       subtitle = 'Média móvel de 7 dias',
       y = "Óbitos",
       x = "Data")

# total mortes por país

total_br <-  coronavirus %>%
  filter(country == 'Brazil', type == 'death') %>%
  group_by(country) %>%
  summarise(total = sum(cases)) %>%
  as.data.frame(t(total_br))
  
total_chi <-  coronavirus %>%
  filter(country == 'China', type == 'death') %>%
  group_by(country) %>%
  summarise(total = sum(cases)) %>%
  as.data.frame(t(total_chi))

total_ru <-  coronavirus %>%
  filter(country == 'Russia', type == 'death') %>%
  group_by(country) %>%
  summarise(total = sum(cases))

total_af <-  coronavirus %>%
  filter(country == 'South Africa', type == 'death') %>%
  group_by(country) %>%
  summarise(total = sum(cases))

total_in <-  coronavirus %>%
  filter(country == 'India', type == 'death') %>%
  group_by(country) %>%
  summarise(total = sum(cases))

totalMortes <- print(c(total_br, total_in, total_ru, total_in, total_chi))

## mortes por milhão 

hab_br <- 215967714
hab_in <- 1180251000
hab_chi <- 1338612968
hab_af <- 49320500
hab_ru <- 141927297

mMilhao_br <- (total_br[,2]/hab_br)*1000000
mMilhao_in <- (total_in[,2]/hab_in)*1000000
mMilhao_chi <- (total_chi[,2]/hab_chi)*1000000
mMilhao_af <- (total_af[,2]/hab_af)*1000000
mMilhao_ru <- (total_ru[,2]/hab_ru)*1000000


#### plotando 

mMilhao_brics <- as.data.frame(c(mMilhao_br, mMilhao_af, mMilhao_ru, mMilhao_in, mMilhao_chi)) 

mMilhao_brics <- mMilhao_brics %>%
  rename(Brasil = X783.371721941734,
         AfricadoSul  = total,
         Russia =  total.1,
         India = total.2,
         China = X3.54247277843494)

rownames(mMilhao_brics) <- 'Mortes por Milhão'

mMilhao_brics


# taxa de letalidade 

taxa_let <- coronavirus %>%
  filter(country %in% c('Brazil', 'Russia', 'India', 'China', 'South Africa'), type %in% c('confirmed', 'death'))%>%
  summarise(total_casos = sum(confirmed), total_mortes = sum(death))%>%
  summarise(taxa_letalidade = total_mortes / total_casos)


taxa_let <- coronavirus %>%
  filter(country %in% c('Brazil', 'Russia', 'India', 'China', 'South Africa'), type %in% c('confirmed', 'death'))%>%
summarise(total_casos = sum(filter(type == 'confirmed'), total_mortes = sum(filter(type == 'death'))))




# teste

### filtrando os países do brics do db 

brics_cases <- coronavirus %>%
  filter(country %in% c('Brazil', 'China', 'Russia', 'South Africa', 'India'), 
         type == 'confirmed') %>%
  group_by(type, country) %>%
  mutate(casos_acumulados = cumsum(cases))

brics_cases %>%
  filter(country == 'China')%>%
  ggplot(aes(x = date, y = casos_acumulados))+
  geom_line()


brics_cases %>%
  filter(country == 'Brazil')%>%
  ggplot(aes(x = date, y = casos_acumulados))+
  geom_line()







casos_china <- coronavirus %>% 
  filter(country == 'Brazil', type == 'confirmed')%>%
  group_by(type, country)%>%
  summarise(total_casos = cumsum(cases))

plot_chi <- casos_china %>% ggplot()+
  geom_line(mapping = aes(x = date, y = total_casos), color = 'blue')

plot_chi


