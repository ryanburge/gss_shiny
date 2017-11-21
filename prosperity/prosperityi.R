library(readr)
library(janitor)
library(scales)
library(extrafont)
library(forcats)


prosper <- read_csv("C:/Users/Ryan Burge/Desktop/prosper.csv") %>% clean_names()

prosper <- prosper %>% mutate(p1 = recode(rdscwlth, "0 = NA; 8 = NA; 9 = NA"))
prosper <- prosper %>% mutate(p2 = recode(rdschlth, "0 = NA; 8 = NA; 9 = NA"))

prosper <- prosper %>% mutate(p1 = p1 -1)
prosper <- prosper %>% mutate(p2 = p2 -1)


prosper <- prosper %>% mutate(pros = p1 + p2) %>% mutate(pros = pros/8)
graph <- prosper %>% filter(pros != "NA") %>% tabyl(pros)


ggplot(graph, aes(x=pros, y=percent)) + geom_col( fill = "dodgerblue1", color = "black") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x= "Prosperity Gospel Scale", y= "Percent of Sample", title = "Adherence to the Prosperity Gospel", caption = "Data: 2012 GSS Cultural Module")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) 

ggsave(file="pros_distribution.png", type = "cairo-png", width = 20, height =12)

library(haven)
prosper <- read_dta("D:/gss_shiny/prosper_reltrad.dta")


g1 <- prosper %>%
  group_by(reltrad) %>%
  summarise(mean= mean(pros, na.rm = TRUE),
            sd = sd(pros, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% mutate(reltrad = Recode(reltrad, "1='Evangelical';
                                                                                                 2='Mainline ';
                                                                                                 3='Black Prot.';
                                                                                                 4='Catholic'; 
                                                                                                 5='Jewish';
                                                                                                 6= 'Other Faith';
                                                                                                 7= 'No Faith'", as.factor = TRUE))


g1 %>% filter(reltrad != "NA") %>% 
  ggplot(., aes(x=mean, y=fct_inorder(reltrad))) + geom_point() +  geom_errorbarh(aes(xmin = lower, xmax=upper), height=0) + 
  labs(x= "Mean Prosperity Score", y= "Religious Tradition", title = "Adherence to the Prosperity Gospel", caption = "Data: 2012 GSS Cultural Module")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))
  

ggsave(file="prospersity_reltrad.png", type = "cairo-png", width = 20, height =12)

prosper <- prosper %>% mutate(ideo = recode(partyid, "0:2=1; 3=2; 4:6=3; else =NA"))

g2 <- prosper %>%
  group_by(ideo) %>%
  summarise(mean= mean(pros, na.rm = TRUE),
            sd = sd(pros, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% mutate(ideo = Recode(ideo, "1='Democrat';
                                                                                                 2='Independent ';
                                                                                                 3='Republican'", as.factor = TRUE))




g2 %>% filter(ideo != "NA") %>%  
  ggplot(., aes(x=mean, y=fct_inorder(ideo))) + geom_point() +  geom_errorbarh(aes(xmin = lower, xmax=upper), height=0) + 
  labs(x= "Mean Prosperity Score", y= "Political Ideology", title = "Adherence to the Prosperity Gospel", caption = "Data: 2012 GSS Cultural Module")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="prospersity_ideo.png", type = "cairo-png", width = 20, height =12)


g2 <- prosper %>%
  group_by(ideo) %>%
  summarise(mean= mean(pros, na.rm = TRUE),
            sd = sd(pros, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% mutate(ideo = Recode(ideo, "1='Democrat';
                                                                                                 2='Independent ';
                                                                                                 3='Republican'", as.factor = TRUE))




g2 %>% filter(ideo != "NA") %>%  
  ggplot(., aes(x=mean, y=fct_inorder(ideo))) + geom_point() +  geom_errorbarh(aes(xmin = lower, xmax=upper), height=0) + 
  labs(x= "Mean Prosperity Score", y= "Political Ideology", title = "Adherence to the Prosperity Gospel", caption = "Data: 2012 GSS Cultural Module")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="prospersity_ideo.png", type = "cairo-png", width = 20, height =12)


g2 <- prosper %>%
  group_by(ideo) %>%
  summarise(mean= mean(pros, na.rm = TRUE),
            sd = sd(pros, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% mutate(ideo = Recode(ideo, "1='Democrat';
                                                                                                 2='Independent ';
                                                                                                 3='Republican'", as.factor = TRUE))

prosper <- prosper %>% 
  mutate(inc = recode(income06, "1:8='Less than $10,000'; 9:12='$10,000 to $20,000'; 13:16 ='$20,000 to $35,000'; 17:18= '$35,000 to $50,000'; 19:20 ='$50,000 to $75,000'; 21:22= '$75,000 to $110,000'; 23:24= '$110,000 to $150,000'; 25= '0ver $150,000'; else='NA'"))


g3 <- prosper %>%
  group_by(inc) %>%
  summarise(mean= mean(pros, na.rm = TRUE),
            sd = sd(pros, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper = mean + qt(1 - (0.05 / 2), n - 1) * se) 

g3$inc <- fct_relevel(g3$inc, "Less than $10,000", "$10,000 to $20,000", "$20,000 to $35,000", "$35,000 to $50,000", "$50,000 to $75,000", "$75,000 to $110,000", "$110,000 to $150,000", "0ver $150,000")


g3 %>% filter(inc != "NA") %>%  
  ggplot(., aes(x=mean, y=inc)) + geom_point(shape=21, size =4, aes(fill = factor(inc)), show.legend =  FALSE) + 
  geom_errorbarh(aes(xmin = lower, xmax=upper,colour = factor(inc)), height=0, size = 1, show.legend = FALSE) + 
  labs(x= "Mean Prosperity Score", y= "Household Income", title = "Household Income and the Prosperity Gospel", caption = "Data: 2012 GSS Cultural Module")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="prospersity_inc.png", type = "cairo-png", width = 20, height =12)



prosper <- prosper %>% mutate(white  = recode(race, "1=1; else=0"))

reg <- prosper %>% filter(pros != "NA")

reg1 <- lm(partyid ~ pros*white, data = reg)

p1 <- seq(range(reg$pros)[1], range(reg$pros)[2], length.out = 11)
p2 <- seq(range(reg$white)[1], range(reg$white)[2], length.out = 2)

## HOW TO MAKE A MARGIN PLOT 

reg1 <- lm(partyid ~ pros*white, data = reg)


b <- predict(reg1, newdata = reg, interval = "confidence", level=.76)
c <- cbind(reg, b)

ggplot(c, aes(pros, fit, group=white, color = as.factor(white), label = white)) +
  geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25) 

c %>% mutate(white = recode(white, "1 = 'White'; 0 = 'Not White'")) %>% 
ggplot(., aes(pros, fit, group=white, color = as.factor(white), label = white)) +
  geom_line() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr, width = .01)) + theme(legend.position="bottom") +  
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.title = element_blank()) +
  labs(x ="Prosperity Gospel Index", y = "Prediction of Republican PID", title = "Interaction of Prosperity and Race on Party ID")

ggsave(file="marginsplot.png", type = "cairo-png", width = 20, height =12)













  