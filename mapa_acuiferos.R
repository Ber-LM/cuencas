##########################
## Mapa Valle de México ##

rm(list=ls())

library (dplyr)
library (Hmisc)
library (stringr)
library (tidyverse)
library (ggplot2)
library (broom)
library (rgdal)
library (texreg)
library (rgeos)
library (RJSONIO)
library (sp)
library (sf)
library (rgdal)
library (raster)

setwd("C:/Users/Admin/Documents/Inputs")
options(scipen=999)

## Datos poligonales INEGI | Divisiones políticas
# Municipios
setwd("C:/Users/Admin/Documents/Inputs/mg_2020_integrado")
capa_municipios <- readOGR("conjunto_de_datos", layer="00mun")
capa_municipios@proj4string

capa_municipios_2 <- spTransform(capa_municipios, CRS("+proj=longlat +datum=WGS84"))
capa_municipios_2@proj4string
rm(capa_municipios)

capa_municipios_df <- tidy(capa_municipios_2, region="CVEGEO")
# rm(capa_municipios_2)

# Entidades
setwd("C:/Users/Admin/Documents/Inputs/mg_2020_integrado")
capa_entidades <- readOGR("conjunto_de_datos", layer="00ent")
capa_entidades@proj4string

capa_entidades_2 <- spTransform(capa_entidades, CRS("+proj=longlat +datum=WGS84"))
capa_entidades_2@proj4string
rm(capa_entidades)

capa_entidades_df <- tidy(capa_entidades_2, region="CVEGEO")
rm(capa_entidades_2)

capa_municipios_df_cl <- capa_municipios_df %>% 
  mutate(ENT = substring(id, 1,2)) %>% 
  filter(ENT == c("06", "09", "11", "12", "13", 
                  "15", "16", "17", "21",
                  "22", "29", "30")) %>% 
  dplyr::select(-ENT)

## Datos poligonales INEGI | Cuencas
## Cuenca Tecolutla
setwd("C:/Users/Admin/Documents/Inputs/702825006931_s/RH27/RH27B/")
capa_tecolutla_raw <- readOGR("RH27Ba", layer="RH27Ba_hl")
capa_tecolutla_raw@proj4string

capa_tecolutla <- spTransform(capa_tecolutla_raw, CRS("+proj=longlat +datum=WGS84"))
capa_tecolutla@proj4string

mun_tecolutla <- raster::intersect(capa_municipios_2, capa_tecolutla)
plot(mun_tecolutla)
mun_tecolutla_lis <- mun_tecolutla$CVEGEO

# plot(capa_tecolutla)
rm(capa_tecolutla_raw)

capa_tecolutla_df <- tidy(capa_tecolutla, region="ID")
rm(capa_tecolutla)

## Subcuenca Alto Amacuzac
setwd("C:/Users/Admin/Documents/Inputs/889463129745_s/RH18/RH18F/")
capa_amacuzac_raw <- readOGR("RH18Ff", layer="RH18Ff_hl")
capa_amacuzac_raw@proj4string

capa_amacuzac <- spTransform(capa_amacuzac_raw, CRS("+proj=longlat +datum=WGS84"))
capa_amacuzac@proj4string

mun_amacuzac <- raster::intersect(capa_municipios_2, capa_amacuzac)
plot(mun_amacuzac)
mun_amacuzac_lis <- mun_amacuzac$CVEGEO

plot(capa_amacuzac)
rm(capa_amacuzac_raw)

capa_amacuzac_df <- tidy(capa_amacuzac, region="ID")
rm(capa_amacuzac)

## Subcuenca del R. Tula
setwd("C:/Users/Admin/Documents/Inputs/889463131069_s/RH26/RH26D/")
capa_tula_raw <- readOGR("RH26Dj", layer="RH26Dj_hl")
capa_tula_raw@proj4string

capa_tula <- spTransform(capa_tula_raw, CRS("+proj=longlat +datum=WGS84"))
capa_tula@proj4string

mun_tula <- raster::intersect(capa_municipios_2, capa_tula)
plot(mun_tula)
mun_tula_lis <- mun_tula$CVEGEO

plot(capa_tula)
rm(capa_tula_raw)

capa_tula_df <- tidy(capa_tula, region="ID")
rm(capa_tula)

## Subcuenca del R. Cutzamala
# setwd("C:/Users/Admin/Documents/Inputs/702825006887_s/RH18/RH18G/")
# capa_cutz_raw <- readOGR("RH18Ga", layer="RH18Ga_hl")
# capa_cutz_raw@proj4string
# 
# capa_cutz <- spTransform(capa_cutz_raw, CRS("+proj=longlat +datum=WGS84"))
# capa_cutz@proj4string
# 
# plot(capa_cutz)
# rm(capa_cutz_raw)
# 
# capa_cutz_df <- tidy(capa_cutz, region="ID")
# rm(capa_cutz)

## Subcuenca del R. Pololoapan
setwd("C:/Users/Admin/Documents/Inputs/702825006935_s/RH28/RH28A/")
capa_polo_raw <- readOGR("RH28Aa", layer="RH28Aa_hl")
capa_polo_raw@proj4string

capa_polo <- spTransform(capa_polo_raw, CRS("+proj=longlat +datum=WGS84"))
capa_polo@proj4string

mun_polo <- raster::intersect(capa_municipios_2, capa_polo)
plot(mun_polo)
mun_polo_lis <- mun_polo$CVEGEO

plot(capa_polo)
rm(capa_polo_raw)

capa_polo_df <- tidy(capa_polo, region="ID")
rm(capa_polo)

## Datos INEGI | Población por cuenca
# Población 2020
inegi_2020_raw <- read.csv("C:/Users/Admin/Documents/Inputs/censo_inegi_2020.csv")

inegi_2020 <- inegi_2020_raw %>% 
  dplyr::select(1:10) %>% 
  rename(ENT_NOM = NOM_ENT,
         MUN_NOM = NOM_MUN,
         LOC_NOM = NOM_LOC,
         ENT = ENTIDAD,
         POBTOT_2020 = POBTOT) %>% 
  mutate(CVE_MUN = str_pad(MUN, 3, pad = "0")) %>% 
  mutate(CVEGEO = paste(ENT, CVE_MUN, sep="")) %>% 
  dplyr::select(-LONGITUD, -ALTITUD, -LATITUD, 
                -ENT, -MUN, -LOC, -CVE_MUN,
                -ENT_NOM, -MUN_NOM, -LOC_NOM) %>% 
  group_by(CVEGEO) %>% 
  summarise(poblacion = sum(POBTOT_2020))

## Población 1980
inegi_1980_raw <- read.csv("C:/Users/Admin/Documents/Inputs/inegi_1980.csv")
# glimpse(inegi_1980_raw)

inegi_1980 <- inegi_1980_raw %>% 
  filter(MUN != "Total") %>% 
  mutate(ENT = substring(ENT,1,2),
         MUN = substring(MUN,1,3)) %>% 
  mutate(CVEGEO = paste(ENT, MUN, sep=""))

# rm(inegi_2020_raw)

coord <- read.csv("C:/Users/Admin/Documents/Inputs/AGEEML_20211022230495.csv")
coord_cl <- coord %>% 
  dplyr::select(CVE_ENT, CVE_MUN, CVE_LOC, NOM_ENT, NOM_MUN, NOM_LOC, LAT_DECIMAL, LONGITUD) %>%
  rename(LON_DECIMAL = LONGITUD) %>% 
  mutate(if_municipio = ifelse(CVE_LOC == 1, 1, 0)) %>%
  filter(if_municipio == 1) %>% 
  dplyr::select(-NOM_LOC, -if_municipio, -CVE_LOC) %>% 
  rename(estado = NOM_ENT, municipio = NOM_MUN) %>% 
  mutate(CVE_MUN = str_pad(CVE_MUN, 3, pad = "0")) %>% 
  mutate(CVEGEO = paste(CVE_ENT, CVE_MUN, sep=""))

mun_cuencas_lis_2020 <- coord_cl %>% 
  filter(CVEGEO %in% c(mun_amacuzac_lis, 
                     mun_tecolutla_lis, 
                     mun_polo_lis, 
                     mun_tula_lis)) %>% 
  dplyr::select(-CVE_MUN, CVE_ENT) %>% 
  full_join(inegi_2020, "CVEGEO") %>% 
  drop_na()

mun_cuencas_lis_1980 <- coord_cl %>% 
  filter(CVEGEO %in% c(mun_amacuzac_lis, 
                       mun_tecolutla_lis, 
                       mun_polo_lis, 
                       mun_tula_lis)) %>% 
  dplyr::select(-CVE_MUN, CVE_ENT) %>% 
  full_join(inegi_1980, "CVEGEO") %>% 
  drop_na()

## Mapas combinados
## Todos combinados (VAMOS)
## 2020
ggplot(capa_entidades_df)+  
  geom_polygon(aes(x=long, 
                   y=lat, 
                   group=group),
               colour = "grey50", alpha=0)+
  geom_path(capa_amacuzac_df, 
            mapping = aes(x = long,
                          y = lat, 
                          group = id),
            colour = "royalblue", size = 0.0005)+
  geom_path(capa_tecolutla_df, 
            mapping = aes(x = long,
                          y = lat, 
                          group = id),
            colour = "royalblue", size = 0.0005)+
  geom_path(capa_tula_df, 
            mapping = aes(x = long,
                          y = lat, 
                          group = id),
            colour = "royalblue", size = 0.0005)+
  geom_path(capa_polo_df, 
            mapping = aes(x = long,
                          y = lat, 
                          group = id),
            colour = "royalblue", size = 0.0005)+
  geom_point(data=mun_cuencas_lis_2020,
             aes(x=LAT_DECIMAL, 
                 y=LON_DECIMAL,
                 size = poblacion),
             colour="red", 
             fill = "pink", 
             pch=21,
             alpha=I(0.75))+
  scale_size_continuous(limits = c(0,2000000), 
                        range = c(1,40),
                        breaks = c(100000,500000,1500000),
                        labels = c("100,000", 
                                   "500,000", 
                                   "1,500,000"))+
  coord_sf(xlim = c(-100.5, -94.9), 
           ylim = c(17.9, 21), expand = F)+
  theme_bw()+
  theme(legend.position = c(0.85,0.675))+
  theme(legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue"),
        legend.margin=margin(c(5,5,5,0)),
        legend.title.align=0.5,
        legend.text = element_text(margin = margin(l = 0.1, unit = 'cm')))+
  labs(size="Población — 2020",
       title = "Cambio en la población habitante de una cuenca y tres subcuencas del área del centro de México (1980-2020)",
       subtitle ="Cuenca del Alto Amacuzac, subcuenca Tecolutla, subcuenca Río Pololoapan, subcuenca del Río Tula",
       caption = "Elaboración: Bernardo L. Mc Kelligan
       Datos: INEGI (1980, 2010, 2020)")+
  theme(legend.key = element_blank())

## 1980
ggplot(capa_entidades_df)+  
  geom_polygon(aes(x=long, 
                   y=lat, 
                   group=group),
               colour = "grey50", alpha=0)+
  geom_path(capa_amacuzac_df,
            mapping = aes(x = long,
                          y = lat,
                          group = id),
            colour = "royalblue", size = 0.0005)+
  geom_path(capa_tecolutla_df,
            mapping = aes(x = long,
                          y = lat,
                          group = id),
            colour = "royalblue", size = 0.0005)+
  geom_path(capa_tula_df,
            mapping = aes(x = long,
                          y = lat,
                          group = id),
            colour = "royalblue", size = 0.0005)+
  geom_path(capa_polo_df,
            mapping = aes(x = long,
                          y = lat,
                          group = id),
            colour = "royalblue", size = 0.0005)+
  geom_point(data=mun_cuencas_lis_1980,
             aes(x=LAT_DECIMAL,
                 y=LON_DECIMAL, 
                 size = Pob_tot),
             colour="red", 
             fill = "pink", 
             pch=21,
             alpha=I(0.75))+
  scale_size_continuous(limits = c(0,2000000),
                        range = c(1,40),
                        breaks = c(100000,500000,1500000),
                        labels = c("100,000", 
                                   "500,000", 
                                   "1,500,000"))+
  coord_sf(xlim = c(-100.5, -94.9), 
           ylim = c(17.9, 21), expand = F)+
  theme_bw()+
  theme(legend.position = c(0.85,0.675))+
  theme(legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue"),
        legend.margin=margin(c(5,5,5,0)),
        legend.title.align=0.5,
        legend.text = element_text(margin = margin(l = 0.1, unit = 'cm')))+
  labs(size="Población — 1980",
       title = "Cambio en la población habitante de una cuenca y tres subcuencas del área del centro de México (1980-2020)",
       subtitle ="Cuenca del Alto Amacuzac, subcuenca Tecolutla, subcuenca Río Pololoapan, subcuenca del Río Tula",
       caption = "Elaboración: Bernardo L. Mc Kelligan
       Datos: INEGI (1980, 2010, 2020)")+
  theme(legend.key = element_blank())

max(mun_cuencas_lis_1980$Pob_tot)   # 357071
max(mun_cuencas_lis_2020$poblacion) # 1821244
