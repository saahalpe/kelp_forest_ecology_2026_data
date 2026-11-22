 



rm(list = ls())
setwd("~/Desktop/Prelim Urchin Kelp Data (11.8.2026)")
library('tidyverse')
library('dplyr')
library('pwr')
library('janitor')
library('ggplot2')
library('googlesheets4')


#load data
benthic <- read_sheet("https://docs.google.com/spreadsheets/d/1xivguVxttXVC1Yz83h3K8c41RDC9AEV4739YHuQfYyk/edit?gid=1088361403#gid=1088361403", 
                      sheet = "urchin_survey") %>% janitor::clean_names()

kelp <- read_sheet("https://docs.google.com/spreadsheets/d/1xivguVxttXVC1Yz83h3K8c41RDC9AEV4739YHuQfYyk/edit?gid=0#gid=0",
                   sheet = "kelp_survey") %>% janitor::clean_names()

dissection <- read_sheet("https://docs.google.com/spreadsheets/d/1Wp6LemJsfO6sMovPOI-i-HT8lBvW8MMFQt_WZnWKsFw/edit?gid=0#gid=0",
                   sheet = "dissection") %>% janitor::clean_names()

#build dissection data
dissect_build1 <- dissection%>%
                mutate(location=ifelse(meter_mark!="190","persistent","transition")) 

################################################################################
#plot GSI by location and species
p1 <- ggplot(dissect_build1, aes(x=location, y=gsi_index))+
        geom_boxplot()
p1

p2 <- ggplot(dissect_build1, aes(x=location, y=gsi_index))+
      geom_boxplot()+
      facet_wrap(~species)
p2

################################################################################
#model to test gonad significance
gsi_mod1 <- lm(gsi_index ~ location * species, data = dissect_build1)

summary(gsi_mod1)
anova(gsi_mod1)
emm <- emmeans::emmeans(gsi_mod1, ~ location | species)
pairs(emm, adjust="tukey")


#process data
hist(kelp$macro_stipe_count)

p <- ggplot(data = benthic, aes(x = site, y = macro_stipe_count))+
  geom_boxplot(aes(fill = site, group = site))
p

