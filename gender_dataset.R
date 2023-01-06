############################################################
################# PROJECT ADV VIS IN R #####################
############################################################
library(tidyverse)
library(lubridate)

#### DATA PREPARATION ####

# 263 K rows of data
data <- read_csv("data/Gender_StatsData.csv") %>% 
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
description <- "The dynamics in modern world are rapidly changing for women. There is more women employers in labour market."
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
    geom_text(aes(label = round(SH.MMR.RISK.ZS, 2)), vjust = 4, color = 'white', size = 10) + 
    theme(legend.position = "none")   

ggsave(path = "plots", filename = "vis2.png")
