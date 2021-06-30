'

plot PLC als functie van Si en Mg-gehalte 


'


library(readxl)
library(dplyr)
library(ggplot2)


#PART I. -------LOAD DATA---------------------


#importeer ingredienten
path <- "C:/Users/HAVO/Desktop/Han R repo/_Shrinkage/"
setwd(path)

df_preQC2 <- readRDS("dfqc2.rds")


#select only columns of interest
drop = c('klant','plc110','plc1000',
         'SiC','Fe2O3','TiO2','K2O','Na2O','P2O3','BaO')
     

df2 <- df %>%
  select(-drop,-contains("mm"))

names(df2) <- tolower(names(df2))

df3 <- df2 %>%
  filter(
    !grepl("Adhesiet",kwaliteit),
    jaar >=2017
    ) %>%
  group_by(recept,kwaliteit) %>%
  summarise(
    n=n(),
    plc815=round(mean(plc815, na.rm=T),2),
    kdv815=round(mean(kdv815, na.rm=T),1),
    ds815=round(mean(ds815, na.rm=T),2),
    sio2=round(mean(sio2, na.rm=T),1),
    cao=round(mean(cao, na.rm=T),1),
    mgo=round(mean(mgo, na.rm=T),1),
    al2o3=round(mean(al2o3, na.rm=T),1)
  ) %>%
  filter(n>=2)

names(df3)

p <- df3 %>%
  ggplot(aes(x=sio2,y=plc815,color=mgo,size=cao,label=kwaliteit)) + 
  geom_point() +
  geom_text(size=4,color="black",alpha=0.5,nudge_y=-0.003 ) +
  xlab("SiO2 content (wt%)") + 
  ylab("PLC815°C (%)") + 
  scale_color_gradientn(colors=c("yellow","#23b81d","blue"),na.value = "grey50") + 
  labs(title="PLC815°C as function of SiO2 content and density") 


View(df3)

df3 %>%
  filter(
    !is.nan(plc815),
    !is.nan(sio2),
    !is.nan(mgo)
  ) %>%
  ggplot(aes(x=sio2,y=ds815,z=plc815)) + 
  stat_density2d_filled()

#geom_contour_filled does not function with irregulara data


library(plotly)
ggplotly(p)



#importeer ingredienten
setwd("\\\\2501-2012-D/users$/HAVO/Documents/R/Rprojects/DataFromDatasheets/2021-04 Ingredienten/")
df_ingr <- readRDS("AlleIngredienten.rds")
names(df_ingr) <- tolower(names(df_ingr))

dfjoin <- left_join(df3,df_ingr,by="recept")

dfjoin2 <- dfjoin %>%
  filter(!is.na(omschrijving),
         !grepl("Mortel",kwaliteit)
         ) %>%
  group_by(recept) %>%
  mutate(q=hoeveelheid/sum(hoeveelheid)) %>%
  ungroup() %>%
  select(-c(12:14)) %>%
  filter(grepl("RW|lkem",omschrijving))

dfj_p <- dfjoin2 %>%
  ggplot(aes(x=q,y=plc815)) + geom_density_2d_filled() + geom_point(alpha=0.3) + xlab("fractie Füller/Elkem") + ylab("PLC815°C (%)") +
  labs(title = "PLC815°C als functie van fractie microsilica",subtitle = "gemiddelde waardes van recepten vanaf 2017")

dfj_p

?geom_density_2d_filled


getwd()
#PART IV. ------TERMINATE CODE-------------------------

rm(list = ls())
cat("\014") #or cntrl+L
if(!is.null(dev.list())) dev.off()
gc()





