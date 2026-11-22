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

#build urchin swath data
urchin_build1 <- benthic%>%
               mutate(location=ifelse(meter_mark!="190","persistent","transition"))

#build kelp swath data
kelp_build1 <- kelp%>%
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

#plot urchin size by location and species
p3 <- ggplot(urchin_build1, aes(x=location, y=size_cm))+
    geom_boxplot()
p3

p4 <- ggplot(urchin_build1, aes(x=location, y=size_cm))+
      geom_boxplot()+
      facet_wrap(~species)
p4

#plot kelp count by location
hist(kelp$macro_stipe_count)

p <- ggplot(data = kelp, aes(x = meter_mark, y = macro_stipe_count))+
  geom_boxplot(aes(fill = meter_mark, group = meter_mark))
p

################################################################################
#model to test gonad significance
gsi_mod1 <- lm(gsi_index ~ location * species, data = dissect_build1)

summary(gsi_mod1)
anova(gsi_mod1)
emm <- emmeans::emmeans(gsi_mod1, ~ location | species)
pairs(emm, adjust="tukey")


#model to test kelp density significance
kelp_mod1 <- lm(macro_stipe_count ~ location, data = kelp_build1)

summary(kelp_mod1)

kelp_mod2 <- lm(number_individuals ~ location, data = kelp_build1)

summary(kelp_mod2)

#model to test urchin swath data
urchin_mod1 <- lm(size_cm ~ location * species, data = urchin_build1)

summary(urchin_mod1)
anova(urchin_mod1)



