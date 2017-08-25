library(tidyverse)
library(janitor)
library(extrafont)
library(car)
library(VennDiagram)
library(haven)
library(scales)




gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")

gss <- gss %>% 
  mutate(prolife = recode(abany, "2=1; else=0")) %>% 
  mutate(nodp = recode(cappun, "2=1; else=0"))  %>% 
  mutate(prolife = as.numeric(prolife), nodp = as.numeric(nodp)) %>% 
  mutate(prolife = recode(prolife, "1= 'Pro-life'; 0 ='Pro-Choice'")) %>% 
  mutate(nodp = recode(nodp, "1 = 'Anti Death Peanlty'; 0 = 'Pro Death Penalty'"))


grid.newpage()
draw.pairwise.venn(area1= 14796, area2 = 20800, cross.area=5480, category = c("Anti Death Penalty", "Anti Abortion"), lty = rep("blank", 2), fill= c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0, 0), cat.dist = rep(0.025, 2))
                                                                                                                                
gss <- gss %>% 
  mutate(prolife = recode(abany, "2=1; else=0")) %>% 
  mutate(nodp = recode(cappun, "2=1; else=0"))  %>% 
  mutate(prolife = as.numeric(prolife), nodp = as.numeric(nodp)) 

gss <- gss %>% mutate(sui = recode(suicide1, "2=1 ; else =0"))

year <- gss %>% filter(nodp ==1 & prolife ==1 & sui ==1) %>% 
  group_by(year) %>% 
  count(wt= wtssall) %>% 
  mutate(pct = prop.table(n))

ggplot(year, aes(x=year, y= pct)) + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Year", y = "Percent of Sample", title = "What Percentage of Americans Have a Consistent Life Ethic?", caption = "Data: GSS (1977-2016)") + 
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=40, family="KerkisSans")) + geom_smooth()

ggsave(file="consistent_life_ethic.png", type = "cairo-png", width = 20, height =12)


ab <- gss %>% 
  group_by(year) %>% 
  count(prolife, wt= wtssall) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(prolife ==1) %>% 
  mutate(label = c("Anti Abortion"))

dp <- gss %>% 
  group_by(year) %>% 
  count(nodp, wt= wtssall) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(nodp ==1) %>% 
  mutate(label = c("Anti Death Penalty"))


sui <- gss %>% 
  group_by(year) %>% 
  count(sui, wt= wtssall) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(sui == 1) %>% 
  mutate(label = c("Anti Assisted Suicide"))

both <- bind_rows(ab, dp, sui) %>% select(year, pct, label)


ggplot(both, aes(x=year, y= pct, group = label, color = label)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Year", y = "Percent of Sample", title = "Ethic of Life Public Opinion", caption = "Data: GSS (1977-2016)") + 
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=40, family="KerkisSans")) + geom_smooth()

ggsave(file="abortion_and_death.png", type = "cairo-png", width = 20, height =12)


rel <- gss %>% filter(sui ==1 & nodp ==1 & prolife ==1) %>% group_by(reltrad) %>% count(wt =wtssall) %>% rename(total =n )
reltotal <- gss %>% group_by(reltrad) %>% count()
reltrad <- left_join(rel, reltotal) %>% mutate(pct = total/n) %>% na.omit()
reltrad <- reltrad %>% mutate(pct = as.numeric(pct))

reltrad$reltrad <- Recode(reltrad$reltrad, "1='Evangelical';
                       2='Mainline';
                       3='Black Prot.';
                       4='Catholic'; 
                       5='Jewish';
                       6= 'Other Faith';
                       7= 'No Faith'", as.factor = TRUE)

ggplot(reltrad, aes(x=fct_inorder(reltrad), y=pct)) + geom_col(fill = "deepskyblue4", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Tradition", y = "Percent of Sample", title = "Ethic of Life By Religious Tradition", caption = "Data: GSS (1977-2016)") + 
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=40, family="KerkisSans")) 

ggsave(file="life_ethic_by_tradition.png", type = "cairo-png", width = 20, height =12)


attend <- gss %>% filter(sui ==1 & nodp ==1 & prolife ==1) %>% group_by(attend) %>% count(wt =wtssall) %>% rename(total =n ) %>%   mutate(pct = prop.table(total))  %>% na.omit()

attend$attend <- Recode(attend$attend, "0= 'Never';
                       1='Less Than Yearly';
                       2='Once a Year';
                       3='Several Times a Year';
                       4='Once a Month'; 
                       5='2-3x a Month';
                       6= 'Nearly Every Week';
                       7= 'Weekly';
                       8= 'More than Weekly'", as.factor = TRUE)


ggplot(attend, aes(x=fct_inorder(attend), y=pct)) + geom_col(fill = "deepskyblue4", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Religious Attendance", y = "Percent of Sample", title = "Ethic of Life By Religious Attendance", caption = "Data: GSS (1977-2016)") + 
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) 

ggsave(file="life_ethic_by_attendance.png", type = "cairo-png", width = 20, height =12)






