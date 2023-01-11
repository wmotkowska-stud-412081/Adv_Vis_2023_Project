############################################################
################# PROJECT ADV VIS IN R #####################
############################################################
library(tidyverse)
library(lubridate)
library(gridExtra)
library(treemapify)

#### DATA PREPARATION ####

# 263 K rows of data
data <- read_csv("data/Gender_StatsData.csv") %>% 
    select(-67)

head(data)

data[data$`Indicator Code` == "SG.GET.JOBS.EQ",] %>% 
    filter(!is.na(`2010`)) %>% 
    select(1, 2, 3, 4, 40, 41, 42, 43)

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

unique(series$Topic)

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

vis.data %>% 
    filter(!is.na(`SG.GET.JOBS.EQ`)) %>% 
    select(1, 2, 3, 4, 40, 41, 42, 43)

#### VISUALIZATION FOR WHOLE WORLD ####

# Dataset only concerning world variables (filtering by country code)
world <- vis.data %>% filter(`Country Code` == "WLD") 
not.na <- world %>% is.na() %>% colSums() %>% data.frame() %>%  filter(. < 50) %>% row.names()
world <- world %>% select(not.na)
str(world)
world.indicators <- indicators %>% filter(`Indicator Code` %in% not.na[-c(1,2)])
rm(not.na)

# First vis
description <- "The dynamics in modern world are rapidly changing for women. \nThere is more women employers in labour market."
world.vis1 <- world %>% filter(!is.na(SL.EMP.MPYR.FE.ZS))
ind <- world.indicators$`Indicator Name`[world.indicators$`Indicator Code` == "SL.EMP.MPYR.FE.ZS"]
ggplot(data = world.vis1, aes(x = Year, y = SL.EMP.MPYR.FE.ZS)) +
    labs(title = description,
         subtitle = paste(" World:", ind),
         y = paste("%", str_split(ind, ",")[[1]][1])) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    geom_line(colour = "purple", linewidth = 2) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#f2dae7"),
          plot.title = element_text(size = 20, face = "bold"),
          text = element_text(size = 15))


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

description <- "Health of women in labor is taken care of better with time."
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
    geom_text(aes(label = round(SH.MMR.RISK.ZS, 2)), vjust = 4, color = 'white', size = 10) + 
    theme(legend.position = "none", 
          plot.background = element_rect(fill = "#f5f5f5"),
          plot.title = element_text(size = 20, face = "bold"),
          text = element_text(size = 15))   

ggsave(path = "plots", filename = "vis2.png")

rm(list = ls(pattern = "world"))

# VISUALIZATIONS FOR REGIONS

regions.data <- vis.data %>% 
    filter(`Country Code` %in% regions$`Country Code`)
not.na <- regions.data %>% is.na() %>% colSums() %>% data.frame() %>%  filter(. < 50) %>% row.names()
regions.data <- regions.data %>% select(not.na)
str(regions.data)
regions.indicators <- indicators %>% filter(`Indicator Code` %in% not.na[-c(1,2)])
rm(not.na)

age <- regions.indicators %>% 
    filter(str_detect(`Indicator Name`, "Population ages")) %>% 
    separate(`Indicator Name`, into = c("rm1", "sex"), sep = ",") %>% 
    filter(!is.na(sex)) %>% 
    separate(rm1, into = c("rm1", "rm2", "age"), sep = " ") %>% 
    select(-c(rm1, rm2)) %>% 
    filter(age != "0-14" & age != "65" & age != "15-64")

age.vis <- regions.data %>% 
    select(`Country Code`, Year, age$`Indicator Code`) %>% 
    left_join(regions) %>% 
    na.omit() %>% 
    select(-`Country Code`) %>% 
    pivot_longer(.,
                 cols = -c(`Country Name`, Year),
        names_to = "Indicator Code",
        values_to = "Population") %>% 
    left_join(age) %>% 
    filter(`Country Name` == "European Union") %>% 
    filter(sex != " total")

age.vis.euro1 <- age.vis %>% 
    filter(Year == "1960-01-11")

age.vis.euro2 <- age.vis %>% 
    filter(Year == "2020-01-11")

ind <- paste(age.vis.euro1$`Country Name`[1], ": Population", year(age.vis.euro1$`Year`[1]))
euro1 <- ggplot(age.vis.euro1, aes(x = Population, y = age, fill = factor(sex))) +
    scale_fill_manual(name = "Sex", labels = c("Women", "Men"), values = c("purple", "gray")) +
    geom_col() + coord_flip() + theme_minimal() +
    labs(title  = ind,
         y = "Age group")  +
    theme(legend.position = "top", 
          plot.background = element_rect(fill = "#f5f5f5"),
          plot.title = element_text(size = 20, face = "bold"),
          text = element_text(size = 15))   
euro1

ind <- paste(age.vis.euro2$`Country Name`[1], ": Population", year(age.vis.euro2$`Year`[1]))
euro2 <- ggplot(age.vis.euro2, aes(x = Population, y = age, fill = factor(sex))) +
    scale_fill_manual(values = c("purple", "gray")) +
    geom_col() +
    coord_flip() + 
    theme_minimal() +
    labs(title  = ind,
         y = "Age group")  +
    theme(legend.position = "none", 
          plot.background = element_rect(fill = "#f5f5f5"),
          plot.title = element_text(size = 20, face = "bold"),
          text = element_text(size = 15))

g <- arrangeGrob(euro1, euro2, nrow=2)

ggsave(path = "plots", filename = "vis3.png", g)

rm(list = ls(pattern = "age"))
rm(list = ls(pattern = "euro"))
rm(list = ls(pattern = "regions"))
rm(g)


# VISUALIZATIONS FOR COUNTRIES

countries.data <- vis.data %>% 
    filter(`Country Code` %in% countries$`Country Code`)
not.na <- countries.data %>% is.na() %>% colSums() %>% data.frame() %>%  filter(. < 15000) %>% row.names()
countries.data <- countries.data %>% select(not.na)
str(countries.data)
countries.indicators <- indicators %>% filter(`Indicator Code` %in% not.na[-c(1,2)])
rm(not.na)

edu <- countries.indicators %>% 
    filter(str_detect(`Indicator Name`, "Educational attainment")) %>% 
    separate(`Indicator Name`, into = c("rm1", "level", "rm2", "sex"), sep = ",") %>% 
    filter(!is.na(sex) & sex != " total (%) (cumulative)") %>% 
    select(-c(rm1, rm2)) %>% 
    separate(sex, into = c("rm3","sex", "rm1", "rm2"), sep = " ") %>% 
    select(-c(rm1, rm2, rm3)) %>% 
    mutate(level = trimws(str_replace(level, "at least ", "")))

edu.vis <- countries.data %>% 
    select(`Country Code`, Year, edu$`Indicator Code`) %>%  
    na.omit() %>% 
    select(-`Country Code`) %>% 
    arrange(Year) %>%                                                           # last twenty years
    pivot_longer(.,
                 cols = -c(Year),
        names_to = "Indicator Code",
        values_to = "Population") %>% 
    left_join(edu) %>% 
    group_by(level, sex) %>% 
    summarise(Population = mean(Population)) %>% 
    ungroup() %>% 
    arrange(Population) %>% 
    mutate(fe = ifelse(sex == "female", 1, 0))

edu1 <- ggplot(edu.vis, aes(area = Population, fill = factor(sex), label = level, subgroup = fe, subgroup2 = level)) +
  scale_fill_manual(name = "Sex", labels = c("Women", "Men"), values = c("purple", "gray")) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = T) + 
  geom_treemap_subgroup_border(colour = "#f7ede1") +
  geom_treemap_subgroup2_border(colour = "#f7ede1") +
  theme(legend.position = 'top') +
  labs(title = "Distribution of edcation levels averaged throughout countries around the world.")+
  theme(plot.background = element_rect(fill = "#f7ede1"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.background = element_rect(fill = "#f7ede1"),
        text = element_text(size = 15))
edu1

ggsave(path = "plots", filename = "vis4.png")


edu2 <- ggplot(edu.vis, aes(x = sex, y = level)) + # nowa baza danych
  geom_tile(aes(fill = Population), color = 'white', show.legend = F) +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_light() + 
  geom_text(aes(label = round(Population)), size = 5, fontface = 'bold', color = 'white') +
  labs(title = "Educational attainment population 25+ \n(%) (cumulative) dependent on Sex") +
  theme(panel.grid = element_blank())+
  theme(plot.background = element_rect(fill = "#f7ede1"),
        plot.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 15))

ggsave(path = "plots", filename = "vis5.png")

rm(list = ls(pattern = "edu"))
rm(description, ind, g)
