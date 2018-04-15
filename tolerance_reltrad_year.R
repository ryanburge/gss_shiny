library(fst)
library(tidyverse)
library(car)

gss <- read.fst("C://gss.fst")


gss$atheist1 <- gss$spkath
gss$atheist1 <- recode(gss$atheist1, "1=1; 2=0; else=0")
gss$atheist2 <- gss$colath
gss$atheist2 <- recode(gss$atheist2, "4=1; 5=0; else=0")
gss$atheist3 <- gss$libath
gss$atheist3 <- recode(gss$atheist3, "2=1; 1=0; else=0")
gss$racist1 <- gss$spkrac
gss$racist1 <- recode(gss$racist1, "1=1; 2=0; else=0")
gss$racist2 <- gss$colrac
gss$racist2 <- recode(gss$racist2, "4=1; 5=0; else=0")
gss$racist3 <- gss$librac
gss$racist3 <- recode(gss$racist3, "2=1; 1=0; else=0")
gss$mili1 <- gss$spkmil
gss$mili1 <- recode(gss$mili1, "1=1; 2=0; else=0")
gss$mili2 <- gss$colmil
gss$mili2 <- recode(gss$mili2, "4=1; 5=0; else=0")
gss$mili3 <- gss$libmil
gss$mili3 <- recode(gss$mili3, "2=1; 1=0; else=0")
gss$comm1 <- gss$spkcom
gss$comm1 <- recode(gss$comm1, "1=1; 2=0; else=0")
gss$comm2 <- gss$colcom
gss$comm2 <- recode(gss$comm2, "5=1; 4=0; else=0")
gss$comm3 <- gss$libcom
gss$comm3 <- recode(gss$comm3, "2=1; 1=0; else=0")
gss$homo1 <- gss$spkhomo
gss$homo1 <- recode(gss$homo1, "1=1; 2=0; else=0")
gss$homo2 <- gss$colhomo
gss$homo2 <- recode(gss$homo2, "4=1; 5=0; else=0")
gss$homo3 <- gss$libhomo
gss$homo3 <- recode(gss$homo3, "2=1; 1=0; else=0")

gss$tolerance <- gss$atheist1 + gss$atheist2 + gss$atheist3 + gss$racis1t + gss$racist2 + gss$racist3 + gss$comm1 + gss$comm2 + gss$comm3 + gss$mili1 + gss$mili2 + gss$mili3 + gss$homo1 + gss$homo2 + gss$homo3

gss$tolerance <- gss$tolerance/15

gss <- gss %>% 
  mutate(ath_tol = atheist1 + atheist2 + atheist3) %>% 
  mutate(rac_tol = racist1 + racist2 + racist3) %>% 
  mutate(mil_tol = mili1 + mili2 + mili3) %>% 
  mutate(com_tol = comm1 + comm2 + comm3) %>% 
  mutate(homo_tol = homo1 + homo2 + homo3) %>% 
  mutate(tolerance = ath_tol + rac_tol + mil_tol + com_tol + homo_tol) %>% 
  mutate(tolerance = tolerance/15)



tolerance <- gss  %>% 
  group_by(reltrad, year) %>% 
  summarise_each(funs(mean), mean_ath = ath_tol, mean_rac = rac_tol, mean_mil = mil_tol, mean_com = com_tol, mean_homo = homo_tol)

tolerance <- tolerance %>% melt(id = c("reltrad", "year")) %>% na.omit()

tolerance_all <- gss  %>% 
  group_by(year) %>% 
  summarise_each(funs(mean), mean_ath = ath_tol, mean_rac = rac_tol, mean_mil = mil_tol, mean_com = com_tol, mean_homo = homo_tol) %>% 
  mutate(reltrad = c("Entire Sample"))
         
tolerance_all <- tolerance_all %>% melt(id = c("reltrad", "year"))

tolerance <- tolerance %>% 
  mutate(reltrad = recode(reltrad, "3 = 'Black Protestant';
                                    4 = 'Catholic';
                                    1 = 'Evangelical';
                                    2 = 'Mainline';
                                    5 = 'Jewish';
                                    7 = 'No Faith';
                                    6 = 'Other Faith'")) 

tol_total <- bind_rows(tolerance, tolerance_all)

tol_total <- tol_total %>% 
  mutate(variable = recode(variable, "'mean_ath' = 'Atheist';
                                      'mean_rac' = 'Racist';
                                      'mean_mil' = 'Militarist';
                                      'mean_com' = 'Communist';
                                      'mean_homo' = 'Homosexual'"))

tol_total %>% 
  filter(value != 0) %>% 
  filter(reltrad == "Evangelical" | reltrad == "Entire Sample" | reltrad == "No Faith" | reltrad == "Catholic") %>% 
  na.omit() %>% 
  ggplot(., aes(x=year, y=value, group = reltrad, label = reltrad, color = factor(reltrad))) + geom_point() +
  geom_smooth(se = FALSE) + facet_wrap(~variable, ncol =3) + long_rb()  +
  theme(
    legend.position = c(.93, .05),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) + theme(legend.text = element_text(size =36)) + scale_color_lancet() +
  labs(x = "Year", y = "<-- Less Tolerance: More Tolerance -->", title ="Changes in Tolerance by Religious Tradition", caption = "Data: GSS (1972-2016)")


ggsave(file="tolerance_reltrad_year.png", type = "cairo-png", width = 22, height = 12)


tol_total %>% 
  filter(value != 0) %>% 
   filter(reltrad == "Evangelical") %>% 
  na.omit() %>% 
  ggplot(., aes(x=year, y=value, group = variable, label = variable, color = factor(variable))) + geom_point() +
  geom_smooth(se = FALSE) + long_rb()  +
  scale_color_d3() +
  labs(x = "Year", y = "<-- Less Tolerance: More Tolerance -->", title ="Changes in Evangelical Tolerance", caption = "Data: GSS (1972-2016)")


ggsave(file="tolerance_evangelical_year.png", type = "cairo-png", width = 22, height = 12)


