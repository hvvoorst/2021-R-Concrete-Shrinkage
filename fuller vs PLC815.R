'

fuller vs plc 815


'


library(ggplot2)
library(plotly)
library(stringr)
library(dplyr)


#importeer ingredienten
path <- "C:/Users/HAVO/Desktop/Han R repo/_Shrinkage/"
setwd(path)


df_ingr <- readRDS("alleingredienten.rds")

df_preQC2 <- readRDS("dfqc2.rds")


#get avarage values for 2016 and beyond kwaliteiten
df <- df_preQC2 %>%
  filter(datum > as.Date("2015-12-31")) %>%
  group_by(Recept,KortKwal) %>%
  summarise(
    n = n(),
    H2O  = round(mean(H2O, na.rm=T),1),
    kdv110 = round(mean(kdv110,na.rm=T),1),
    kdv815 = round(mean(kdv815,na.rm=T),1),
    ds110  = round(mean(ds110, na.rm=T),2),
    ds815  = round(mean(ds815, na.rm=T),2),
    plc110  = round(mean(plc110, na.rm=T),2),
    plc815  = round(mean(plc815, na.rm=T),2)
  ) %>% 
  filter(
    !grepl("Ad|Vx|Cs|VHT-Putty|premix|Ct",KortKwal),
    !grepl("^M",KortKwal),
    n >= 3
  ) %>%
  rename(kwal = KortKwal)


#join ingredients and recipes by recipe ID
df_joined <- inner_join(df, df_ingr, by = "Recept")
names(df_joined) <- tolower(names(df_joined))
df_joined $omschrijving <- tolower(df_joined $omschrijving)



###################################maak een mooie pie-chart
df_pred %>%
  filter(grepl("140",kwal)) %>%
  ggplot(aes(x = "", y = fractie, fill=reorder(ingr,-pos))) +
  geom_col() +
  geom_text(aes(y=pos,label=ingr),hjust ="middle",position = position_jitter(height = 0.3),size=3) +
  coord_polar(theta="y",start=0) + 
  facet_wrap(rep_kwal ~ .) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) 


library(reprex)


################### fuller vs plc 815
names(df_pred)
search <- "rw fuller"
df_pred %>%
  filter(grepl(search,omschrijving)) %>%
  ggplot() +
  geom_density2d_filled(aes(x=fractie,y=plc815)) +
  geom_point(aes(x=fractie,y=plc815),shape=3,size=.5)



