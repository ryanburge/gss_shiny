library(tidyverse)
library(extrafont)
library(car)


## Death Penalty


cap <- gss %>% select(year, bible, reltrad, partyid, cappun) %>% na.omit()

cap$cappun <- Recode(cap$cappun, "1=1; else=0")


death <- cap %>%  group_by(year, reltrad) %>% 
  count(cappun) %>% mutate(pct = prop.table(n)) %>%  
  filter(cappun ==1) %>% select(year, pct)

d1 <- cap %>%  group_by(year) %>% 
  count(cappun) %>% mutate(pct = prop.table(n)) %>% 
  filter(cappun ==1) %>% select(year, pct)


death$reltrad <- Recode(death$reltrad, "1='Evangelical Protestants';
                       2='Mainline Protestants';
                      3='Black Protestants';
                      4='Catholic'; 
                      5='Jewish';
                      6= 'Other Faith';
                      7= 'No Faith'", as.factor = TRUE)


ggplot(death, aes(x=year, y=pct*100, color = reltrad, label = reltrad)) +  
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + labs(x= "Year", y = "% in Favor", title = "Support for the Death Penalty ", caption = "Data from the GSS (1984-2016)")+
  geom_smooth(se = FALSE) + 
  scale_x_continuous(breaks = seq(min(death$year), max(death$year), by = 2)) +
  scale_color_manual(values = c("#F8766D", "#CD9600", "darkmagenta", "#00BE67","yellow", "turquoise", "violet", "black"))


## Crime Spending 


crime <- gss %>% select(year, bible, reltrad, partyid, natcrime) %>% na.omit()

crime$natcrime <- Recode(crime$natcrime, "1=1; else=0")


crim <- crime %>%  group_by(year, reltrad) %>% 
  count(natcrime) %>% mutate(pct = prop.table(n)) %>%  
  filter(natcrime ==1) %>% select(year, pct) %>% ungroup(reltrad) %>%  mutate(reltrad = as.numeric(reltrad))

crim1 <- crime %>%  group_by(year) %>% 
  count(natcrime) %>% mutate(pct = prop.table(n)) %>%  
  filter(natcrime ==1) %>% select(year, pct) %>% mutate(reltrad = c("Entire Sample")) 

crim <- bind_rows(crim, crim1)


crim$reltrad <- Recode(crim$reltrad, "1='Evangelical Protestants';
                       2='Mainline Protestants';
                        3='Black Protestants';
                        4='Catholic'; 
                        5='Jewish';
                        6= 'Other Faith';
                        7= 'No Faith'", as.factor = TRUE)

ggplot(crim, aes(x=year, y=pct*100, color = reltrad, label = reltrad)) +  
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + labs(x= "Year", y = "% Saying Too Little", title = "The US is Spending Too Little on Halting the Rising Crime Rate ", caption = "Data from the GSS (1984-2016)")+
  geom_smooth(se = FALSE) + 
  scale_x_continuous(breaks = seq(min(crim$year), max(crim$year), by = 2)) +
  scale_color_manual(values = c("#F8766D", "#CD9600","black", "darkmagenta", "#00BE67","yellow", "turquoise", "violet"))


## Drug Spending

drugs <- gss %>% select(year, bible, reltrad, partyid, natdrug) %>% na.omit()

drugs$natdrug <- Recode(drugs$natdrug, "1=1; else=0")


d1 <- drugs %>%  group_by(year, reltrad) %>% 
  count(natdrug) %>% mutate(pct = prop.table(n)) %>%  
  filter(natdrug ==1) %>% select(year, pct)

d1$reltrad <- Recode(d1$reltrad, "1='Evangelical Protestants';
                       2='Mainline Protestants';
                     3='Black Protestants';
                     4='Catholic'; 
                     5='Jewish';
                     6= 'Other Faith';
                     7= 'No Faith'", as.factor = TRUE)

d2 <- drugs %>%  group_by(year) %>% 
  count(natdrug) %>% mutate(pct = prop.table(n)) %>%  
  filter(natdrug ==1) %>% select(year, pct) %>% mutate(reltrad = c("Entire Sample")) %>% rbind(d1)



ggplot(d2, aes(x=year, y=pct*100, color = reltrad, label = reltrad)) +  
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + labs(x= "Year", y = "% Saying Too Little", title = "The US is Spending Too Little on Dealing with Drug Addiction ", caption = "Data from the GSS (1984-2016)")+
  geom_smooth(se = FALSE) + 
  scale_x_continuous(breaks = seq(min(crim$year), max(crim$year), by = 2)) +
  scale_color_manual(values = c("#F8766D", "#CD9600","black", "darkmagenta", "#00BE67","yellow", "turquoise", "violet"))