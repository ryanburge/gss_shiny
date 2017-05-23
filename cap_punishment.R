cap <- gss %>% select(year, bible, reltrad, partyid, cappun) %>% na.omit()

cap$cappun <- Recode(cap$cappun, "1=1; else=0")


death <- cap %>%  group_by(year, reltrad) %>% count(cappun) %>% mutate(pct = prop.table(n)) %>%  filter(cappun ==1) %>% select(year, pct)

d1 <- cap %>%  group_by(year) %>% count(cappun) %>% mutate(pct = prop.table(n)) %>%  filter(cappun ==1) %>% select(year, pct)


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
  scale_x_continuous(breaks = seq(min(lit$year), max(lit$year), by = 2)) +
  scale_color_manual(values = c("#F8766D", "#CD9600", "darkmagenta", "#00BE67","yellow", "turquoise", "violet", "black"))