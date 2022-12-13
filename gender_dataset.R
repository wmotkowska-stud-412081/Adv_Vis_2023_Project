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

head(regions)

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

#### FIRST VISUALIZATION ####

description <- "\nIn the entire world the percentage of working age population has been steadily decling for 60 years. The population is growing older. \nThis is only one of the major changes that have been happening rapidly in the last centuary. The world has never changed so quickly! \nMany changes are also visible depending on gender. The inequalities are lessening. We will see that in other visualisations. \n"

world <- vis.data %>% filter(`Country Code` == "WLD")
ind <- indicators$`Indicator Name`[indicators$`Indicator Code` == "SP.POP.DPND"]

ggplot(data = world, aes(x = Year, y = SP.POP.DPND)) +
    labs(title = paste("World", ind),
         subtitle = description,
         y = str_split(ind, " ")[[1]][1]) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    geom_point(colour = "green") +
    theme_light()

