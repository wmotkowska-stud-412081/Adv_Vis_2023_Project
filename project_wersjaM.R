############################################################
################# PROJECT ADV VIS IN R #####################
############################################################
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(cowplot)
library(sf)
library(ggmap)
library(leaflet)
library(ggbump)
library(wesanderson)

#### DATA PREPARATION ####

# 263 K rows of data
data <- read_csv("/Users/meggie/Documents/WNE 2/Advanced Visualisation in R/project/data/Gender_StatsData.csv") %>% 
  select(-67)

head(data)

# We see that data provides not only countries but also division depending on regions of world

regions <- data %>% 
  select(`Country Name`, `Country Code`) %>% 
  distinct() %>% 
  slice(1:48)

regions$`Country Name`

countries <- data %>% 
  select(`Country Name`, `Country Code`) %>% 
  distinct() %>% 
  slice(49:n())

head(countries)

# Available variables
indicators <- data %>% 
  select(`Indicator Name`, `Indicator Code`) %>% 
  distinct()

head(indicators)

# Division of variables by topic
series <- read_csv("data/Gender_StatsSeries.csv") %>% 
  select(1, 2, 3, 4, 6)

head(series)

vis.data <- data %>% 
  select(-c(1, 3)) %>% 
  pivot_longer(.,
               cols = -c(`Country Code`, `Indicator Code`),
               names_to = "Year",
               values_to = "Value") %>% 
  pivot_wider(.,
              names_from = `Indicator Code`,
              values_from = Value) %>% 
  mutate(Year = as.Date(Year, format = "%Y"))

head(vis.data)



#### VISUALIZATION FOR WHOLE WORLD ####

# Dataset only concerning world variables (filtering by country code)
world <- vis.data %>% filter(`Country Code` == "WLD") 
not.na <- world %>% is.na() %>% colSums() %>% data.frame() %>%  filter(. < 50) %>% row.names()
world <- world %>% select(not.na)
str(world)
world.indicators <- indicators %>% filter(`Indicator Code` %in% not.na[-c(1,2)])
rm(not.na)

# First vis
description <- "The dynamics in modern world are rapidly changing for women. There is more women employers in labor market."
world.vis1 <- world %>% filter(!is.na(SL.EMP.MPYR.FE.ZS))
ind <- world.indicators$`Indicator Name`[world.indicators$`Indicator Code` == "SL.EMP.MPYR.FE.ZS"]
ggplot(data = world.vis1, aes(x = Year, y = SL.EMP.MPYR.FE.ZS)) +
  labs(title = description,
       subtitle = paste(" World:", ind),
       y = paste("%", str_split(ind, ",")[[1]][1])) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  geom_line(colour = "purple", linewidth = 2) +
  theme_minimal()

ggsave(path = "plots", filename = "vis1.png")

world.agg <- world %>% 
  # filter(Year > "2000-01-01") %>% 
  mutate(Years_5 = floor_date(Year, years(5))) %>% 
  group_by(Years_5) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  ungroup()

world.vis2 <- world.agg %>% 
  select(Years_5, SH.MMR.RISK.ZS) %>% 
  filter(!is.na(SH.MMR.RISK.ZS))

description <- "Health of women in labour is taken care of better with time."
ind <- world.indicators$`Indicator Name`[world.indicators$`Indicator Code` == "SH.MMR.RISK.ZS"]
ggplot(data = world.vis2, aes(x = Years_5, y = SH.MMR.RISK.ZS, 
                              fill = ifelse(Years_5 > "2010-01-01", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual("legend", values = c("Highlighted" = "purple", "Normal" = "gray")) +
  labs(title = description,
       subtitle = paste("World: mean of", ind),
       y = "Percentage",
       x = "Years (mean of following 5 years)") +
  theme_minimal() +
  geom_line(colour = "purple", linewidth = 2) +
  geom_text(aes(label = round(SH.MMR.RISK.ZS, 2)), vjust = 4, color = 'white', size = 10) + 
  theme(legend.position = "none")   

ggsave(path = "plots", filename = "vis2.png")

##FILTER BY REGION
world1 <- vis.data %>% filter(`Country Code` %in% c("EAS","LCN","SAS", "ECS", "MEA", "NAC", "SSF")) 
not.na1 <- world1 %>% is.na() %>% colSums() %>% data.frame() %>%  filter(. < 50) %>% row.names()
world1 <- world1 %>% select(not.na1)
str(world1)
world1.indicators <- indicators %>% filter(`Indicator Code` %in% not.na1[-c(1,2)])
rm(not.na1)

# JOB RECRIUTMENT - MAP
###Recruitment process the same for women as for men across regions over time
world1.visM2 <- vis.data %>% select('Year', 'Country Code', 'SG.GET.JOBS.EQ')
world1.visM2$Year <- as.numeric(format(world1.visM2$Year,'%Y'))
world1.visM2 <- world1.visM2 %>% filter(Year %in% (2009:2019))
world1.visM2 <- world1.visM2 %>% filter(Year ==2009)

map.names <- ne_countries(scale = "medium", returnclass = "sf")
map.names <- map.names %>% select('adm0_a3', 'admin')
map.names <- map.names %>% rename( `Country Code`= adm0_a3,region = admin)

map.data.vis <- left_join(map.names, world1.visM2, by="Country Code")
map.data.vis <- map.data.vis %>% filter(!is.na(map.data.vis$region))

description.visM2 <- "Countries where a woman can get a job in the same way as a man"
ind.visM2 <- world.indicators$`Indicator Name`[world.indicators$`Indicator Code` == "SG.GET.JOBS.EQ"]

ggplot(data = map.data.vis) + geom_sf(aes(fill =  factor(SG.GET.JOBS.EQ)))+
  labs(title = description.visM2, subtitle = "Year: 2009")+
  scale_fill_manual(name = "Can a woman get a job in the same way as a man", labels = c("No", "Yes", "NA"),values=c("purple", "green", "grey"))+
  theme(plot.background = element_rect(fill = "#ead1dcff"),
        plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 10))

# Employment
####Wage and salaried workers, female (% of female employment) SL.EMP.WORK.FE.ZS

world1.salaries <- vis.data %>% select('Year', 'Country Code', 'SL.EMP.WORK.FE.ZS')
world1.salaries <- world1.salaries %>% filter(`Country Code` %in% c("EAS","LCN","SAS", "ECS", "MEA", "NAC", "SSF")) 
world1.salaries <- world1.salaries %>% filter(!is.na(world1.salaries$SL.EMP.WORK.FE.ZS))
world1.salaries <- left_join(world1.salaries, regions, by="Country Code")

description.salaries <- "Wage and salaried workers, female (% of female employment)"
ind.salaries <- world.indicators$`Indicator Name`[world.indicators$`Indicator Code` == "SL.EMP.WORK.FE.ZS"]
min <- min(world1.salaries$Year)
max <- max(world1.salaries$Year)
ggplot(data = world1.salaries, aes(x = Year, y = SL.EMP.WORK.FE.ZS, colour = `Country Name` )) +
  labs(title = description.salaries,
       subtitle = paste(" World:", ind.salaries),
       y = paste("%", str_split(ind.salaries, ",")[[1]][1])) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min, max)) +
  geom_point(size=6) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#c9daf8ff"),
        plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 15))

#Unemployment SL.UEM.TOTL.FE.ZS
world1.unemployment <- vis.data %>% select('Year', 'Country Code', 'SL.UEM.TOTL.FE.ZS')
world1.unemployment <- world1.unemployment %>% filter(`Country Code` %in% c("EAS","LCN","SAS", "ECS", "MEA", "NAC", "SSF")) 
world1.unemployment <- world1.unemployment %>% filter(!is.na(world1.unemployment$SL.UEM.TOTL.FE.ZS))
world1.unemployment <- world1.unemployment %>% filter(!is.na(world1.unemployment$SL.UEM.TOTL.FE.ZS))
world1.unemployment <- left_join(world1.unemployment, regions, by="Country Code")

description.unemployment <- "% of unemployed women by region"
ind.unemployment <- world.indicators$`Indicator Name`[world.indicators$`Indicator Code` == "SL.UEM.TOTL.FE.ZS"]
min <- min(world1.unemployment$Year)
max <- max(world1.unemployment$Year)
ggplot(data = world1.unemployment, aes(x = Year, y = SL.UEM.TOTL.FE.ZS, colour = `Country Name`)) +
  labs(title = description.unemployment,
       subtitle = paste(" World:", ind.unemployment),
       y = paste("%", str_split(ind.unemployment, ",")[[1]][1])) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min, max)) +
  geom_line(size =3) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#c9daf8ff"),
        plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 15))

#Eduaction 
world1.vis.school <- vis.data %>% select('Year', 'Country Code', 'SE.PRM.ENRL.FE.ZS')
world1.vis.school$Year <- as.numeric(format(world1.vis.school$Year,'%Y'))
world1.vis.school <- world1.vis.school %>% filter(Year %in% (2009:2019))
world1.vis.school <- world1.vis.school %>% filter(`Country Code` %in% c("EAS","LCN","SAS", "ECS", "MEA", "NAC", "SSF")) 
world1.vis.school <- left_join(world1.vis.school, regions, by="Country Code")
world1.vis.school <- world1.vis.school %>% filter(!is.na(world1.vis.school$`Country Code`))

world1.vis.school <- world1.vis.school %>% 
  group_by(Year) %>% 
  mutate(rank = rank(SE.PRM.ENRL.FE.ZS, ties.method = "random")) %>% 
  ungroup()

description.vis.school <- "Ranking of regions according to % of females in primary schools"
ind.vis.school <- world.indicators$`Indicator Name`[world.indicators$`Indicator Code` == "SE.PRM.ENRL.FE.ZS"]

ggplot(world1.vis.school, aes(Year, rank, color = `Country Name`)) +
  labs(title = description.vis.school, subtitle = ind.vis.school)+
  geom_point(size = 2) +
  geom_bump(size = 1, smooth = 8) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(
    panel.grid.major = element_blank()) +
  labs(y = "Rank",
       x = NULL) +
  scale_y_reverse(breaks = min(world1.vis.school$rank):max(world1.vis.school$rank))+
  scale_x_continuous(breaks = min(world1.vis.school$Year):max(world1.vis.school$Year))+
  theme(plot.background = element_rect(fill = "#fce5cdff"),
        plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 15))

#Control over household
world1.vis.household2 <- vis.data %>% select('Year', 'Country Code', 'SG.DMK.PRCH.HB.ZS', 'SG.DMK.PRCH.WF.ZS','SG.DMK.PRCH.OT.ZS', 'SG.DMK.PRCH.SE.ZS', 'SG.DMK.PRCH.WH.ZS'  )
world1.vis.household2$Year <- as.numeric(format(world1.vis.household2$Year,'%Y'))
world1.vis.household2 <- world1.vis.household2 %>% filter(Year %in% (2012:2018))
world1.vis.household2 <- world1.vis.household2 %>% filter(!is.na(world1.vis.household2$SG.DMK.PRCH.HB.ZS))

world1.vis.household2.H <- world1.vis.household2 %>% select('Year', 'Country Code', 'SG.DMK.PRCH.HB.ZS')
world1.vis.household2.H$category <- ("mainly husband")
world1.vis.household2.H <- world1.vis.household2.H %>% rename( value= SG.DMK.PRCH.HB.ZS)

world1.vis.household2.W <- world1.vis.household2 %>% select('Year', 'Country Code', 'SG.DMK.PRCH.WF.ZS')
world1.vis.household2.W$category <- ("mainly wife")
world1.vis.household2.W <- world1.vis.household2.W %>% rename( value= SG.DMK.PRCH.WF.ZS)


world1.vis.household2.O <- world1.vis.household2 %>% select('Year', 'Country Code', 'SG.DMK.PRCH.OT.ZS')
world1.vis.household2.O$category <- ("other")
world1.vis.household2.O <- world1.vis.household2.O %>% rename( value= SG.DMK.PRCH.OT.ZS)

world1.vis.household2.S <- world1.vis.household2 %>% select('Year', 'Country Code', 'SG.DMK.PRCH.SE.ZS')
world1.vis.household2.S$category <- ("someone else")
world1.vis.household2.S <- world1.vis.household2.S %>% rename( value= SG.DMK.PRCH.SE.ZS)

world1.vis.household2.J <- world1.vis.household2 %>% select('Year', 'Country Code', 'SG.DMK.PRCH.WH.ZS')
world1.vis.household2.J$category <- ("wife and husband jointly")
world1.vis.household2.J <- world1.vis.household2.J %>% rename( value= SG.DMK.PRCH.WH.ZS)


world1.vis.household2 <- rbind(world1.vis.household2.H, world1.vis.household2.W, world1.vis.household2.O,world1.vis.household2.S,world1.vis.household2.J)

map.names <- ne_countries(scale = "medium", returnclass = "sf")
map.names <- map.names %>% select('adm0_a3', 'region_wb')
map.names <- map.names %>% rename( `Country Code`= adm0_a3,region = region_wb)

world1.vis.household2 <- left_join(map.names, world1.vis.household2, by="Country Code")
world1.vis.household2 <- world1.vis.household2 %>% filter(!is.na(world1.vis.household2$category))

description.vis.household <- "Decision maker about major household purchases (age 15-49)"

ggplot(world1.vis.household2, aes(x = Year, y = value, fill = category)) +
  geom_col(position = "fill")+
  labs(title = description.vis.household)+
  ylab("% of women age 15-49")+
  facet_wrap(~region, scales = "free")+
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(
    panel.grid.major = element_blank())+
  scale_x_continuous(breaks = min(world1.vis.household2$Year):max(world1.vis.household2$Year))+
  theme(plot.background = element_rect(fill = "#d9ead3ff"),
        plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 15))
