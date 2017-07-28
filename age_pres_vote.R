
fil <- gss %>% filter(race ==1 & evangelical ==1 &  age <= 35)

p68 <- fil %>% 
  filter(complete.cases(pres68)) %>% 
  count(pres68, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres68 == 2) %>% 
  mutate(year = c("1968")) %>% 
  select(weight, year)

p72 <- fil %>% 
  filter(complete.cases(pres72)) %>% 
  count(pres72, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres72 == 2) %>% 
  mutate(year = c("1972")) %>% 
  select(weight, year)

p76 <- fil %>% 
  filter(complete.cases(pres76)) %>% 
  count(pres76, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres76 == 2) %>% 
  mutate(year = c("1976")) %>% 
  select(weight, year)

p80 <- fil %>% 
  filter(complete.cases(pres80)) %>% 
  count(pres80, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres80 == 2) %>% 
  mutate(year = c("1980")) %>% 
  select(weight, year)

p84 <- fil %>% 
  filter(complete.cases(pres84)) %>% 
  count(pres84, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres84 == 2) %>% 
  mutate(year = c("1984")) %>% 
  select(weight, year)

p88 <- fil %>% 
  filter(complete.cases(pres88)) %>% 
  count(pres88, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres88 == 2) %>% 
  mutate(year = c("1988")) %>% 
  select(weight, year)

p92 <- fil %>% 
  filter(complete.cases(pres92)) %>% 
  count(pres92, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres92 == 2) %>% 
  mutate(year = c("1992")) %>% 
  select(weight, year)

p96 <- fil %>% 
  filter(complete.cases(pres96)) %>% 
  count(pres96, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres96 == 2) %>% 
  mutate(year = c("1996")) %>% 
  select(weight, year)


p00 <- fil %>% 
  filter(complete.cases(pres00)) %>% 
  count(pres00, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres00 == 2) %>% 
  mutate(year = c("2000")) %>% 
  select(weight, year)

p04 <- fil %>% 
  filter(complete.cases(pres04)) %>% 
  count(pres04, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres04 == 2) %>% 
  mutate(year = c("2004")) %>% 
  select(weight, year)

p08 <- fil %>% 
  filter(complete.cases(pres08)) %>% 
  count(pres08, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres08 == 2) %>% 
  mutate(year = c("2008"))%>% 
  select(weight, year)

p12 <- fil %>% 
  filter(complete.cases(pres12)) %>% 
  count(pres12, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres12 == 2) %>% 
  mutate(year = c("2012"))%>% 
  select(weight, year)

young <- bind_rows(p68, p72, p76, p80, p84, p88, p92, p96, p00, p04, p08, p12) %>% mutate(type = c("Under 35"))

fil <- gss %>% filter(race ==1 & evangelical ==1 &  age > 35 & age < 55)

p68 <- fil %>% 
  filter(complete.cases(pres68)) %>% 
  count(pres68, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres68 == 2) %>% 
  mutate(year = c("1968")) %>% 
  select(weight, year)

p72 <- fil %>% 
  filter(complete.cases(pres72)) %>% 
  count(pres72, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres72 == 2) %>% 
  mutate(year = c("1972")) %>% 
  select(weight, year)

p76 <- fil %>% 
  filter(complete.cases(pres76)) %>% 
  count(pres76, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres76 == 2) %>% 
  mutate(year = c("1976")) %>% 
  select(weight, year)

p80 <- fil %>% 
  filter(complete.cases(pres80)) %>% 
  count(pres80, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres80 == 2) %>% 
  mutate(year = c("1980")) %>% 
  select(weight, year)

p84 <- fil %>% 
  filter(complete.cases(pres84)) %>% 
  count(pres84, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres84 == 2) %>% 
  mutate(year = c("1984")) %>% 
  select(weight, year)

p88 <- fil %>% 
  filter(complete.cases(pres88)) %>% 
  count(pres88, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres88 == 2) %>% 
  mutate(year = c("1988")) %>% 
  select(weight, year)

p92 <- fil %>% 
  filter(complete.cases(pres92)) %>% 
  count(pres92, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres92 == 2) %>% 
  mutate(year = c("1992")) %>% 
  select(weight, year)

p96 <- fil %>% 
  filter(complete.cases(pres96)) %>% 
  count(pres96, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres96 == 2) %>% 
  mutate(year = c("1996")) %>% 
  select(weight, year)


p00 <- fil %>% 
  filter(complete.cases(pres00)) %>% 
  count(pres00, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres00 == 2) %>% 
  mutate(year = c("2000")) %>% 
  select(weight, year)

p04 <- fil %>% 
  filter(complete.cases(pres04)) %>% 
  count(pres04, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres04 == 2) %>% 
  mutate(year = c("2004")) %>% 
  select(weight, year)

p08 <- fil %>% 
  filter(complete.cases(pres08)) %>% 
  count(pres08, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres08 == 2) %>% 
  mutate(year = c("2008"))%>% 
  select(weight, year)

p12 <- fil %>% 
  filter(complete.cases(pres12)) %>% 
  count(pres12, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres12 == 2) %>% 
  mutate(year = c("2012"))%>% 
  select(weight, year)

mid <- bind_rows(p68, p72, p76, p80, p84, p88, p92, p96, p00, p04, p08, p12) %>% mutate(type = c("36-54"))

fil <- gss %>% filter(race ==1 & evangelical ==1 &  age > 55 & age < 64)

p68 <- fil %>% 
  filter(complete.cases(pres68)) %>% 
  count(pres68, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres68 == 2) %>% 
  mutate(year = c("1968")) %>% 
  select(weight, year)

p72 <- fil %>% 
  filter(complete.cases(pres72)) %>% 
  count(pres72, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres72 == 2) %>% 
  mutate(year = c("1972")) %>% 
  select(weight, year)

p76 <- fil %>% 
  filter(complete.cases(pres76)) %>% 
  count(pres76, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres76 == 2) %>% 
  mutate(year = c("1976")) %>% 
  select(weight, year)

p80 <- fil %>% 
  filter(complete.cases(pres80)) %>% 
  count(pres80, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres80 == 2) %>% 
  mutate(year = c("1980")) %>% 
  select(weight, year)

p84 <- fil %>% 
  filter(complete.cases(pres84)) %>% 
  count(pres84, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres84 == 2) %>% 
  mutate(year = c("1984")) %>% 
  select(weight, year)

p88 <- fil %>% 
  filter(complete.cases(pres88)) %>% 
  count(pres88, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres88 == 2) %>% 
  mutate(year = c("1988")) %>% 
  select(weight, year)

p92 <- fil %>% 
  filter(complete.cases(pres92)) %>% 
  count(pres92, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres92 == 2) %>% 
  mutate(year = c("1992")) %>% 
  select(weight, year)

p96 <- fil %>% 
  filter(complete.cases(pres96)) %>% 
  count(pres96, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres96 == 2) %>% 
  mutate(year = c("1996")) %>% 
  select(weight, year)


p00 <- fil %>% 
  filter(complete.cases(pres00)) %>% 
  count(pres00, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres00 == 2) %>% 
  mutate(year = c("2000")) %>% 
  select(weight, year)

p04 <- fil %>% 
  filter(complete.cases(pres04)) %>% 
  count(pres04, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres04 == 2) %>% 
  mutate(year = c("2004")) %>% 
  select(weight, year)

p08 <- fil %>% 
  filter(complete.cases(pres08)) %>% 
  count(pres08, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres08 == 2) %>% 
  mutate(year = c("2008"))%>% 
  select(weight, year)

p12 <- fil %>% 
  filter(complete.cases(pres12)) %>% 
  count(pres12, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres12 == 2) %>% 
  mutate(year = c("2012"))%>% 
  select(weight, year)

midplus <- bind_rows(p68, p72, p76, p80, p84, p88, p92, p96, p00, p04, p08, p12) %>% mutate(type = c("55-64"))


fil <- gss %>% filter(race ==1 & evangelical ==1 &  age < 65)

p68 <- fil %>% 
  filter(complete.cases(pres68)) %>% 
  count(pres68, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres68 == 2) %>% 
  mutate(year = c("1968")) %>% 
  select(weight, year)

p72 <- fil %>% 
  filter(complete.cases(pres72)) %>% 
  count(pres72, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres72 == 2) %>% 
  mutate(year = c("1972")) %>% 
  select(weight, year)

p76 <- fil %>% 
  filter(complete.cases(pres76)) %>% 
  count(pres76, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres76 == 2) %>% 
  mutate(year = c("1976")) %>% 
  select(weight, year)

p80 <- fil %>% 
  filter(complete.cases(pres80)) %>% 
  count(pres80, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres80 == 2) %>% 
  mutate(year = c("1980")) %>% 
  select(weight, year)

p84 <- fil %>% 
  filter(complete.cases(pres84)) %>% 
  count(pres84, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres84 == 2) %>% 
  mutate(year = c("1984")) %>% 
  select(weight, year)

p88 <- fil %>% 
  filter(complete.cases(pres88)) %>% 
  count(pres88, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres88 == 2) %>% 
  mutate(year = c("1988")) %>% 
  select(weight, year)

p92 <- fil %>% 
  filter(complete.cases(pres92)) %>% 
  count(pres92, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres92 == 2) %>% 
  mutate(year = c("1992")) %>% 
  select(weight, year)

p96 <- fil %>% 
  filter(complete.cases(pres96)) %>% 
  count(pres96, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres96 == 2) %>% 
  mutate(year = c("1996")) %>% 
  select(weight, year)


p00 <- fil %>% 
  filter(complete.cases(pres00)) %>% 
  count(pres00, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres00 == 2) %>% 
  mutate(year = c("2000")) %>% 
  select(weight, year)

p04 <- fil %>% 
  filter(complete.cases(pres04)) %>% 
  count(pres04, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres04 == 2) %>% 
  mutate(year = c("2004")) %>% 
  select(weight, year)

p08 <- fil %>% 
  filter(complete.cases(pres08)) %>% 
  count(pres08, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres08 == 2) %>% 
  mutate(year = c("2008"))%>% 
  select(weight, year)

p12 <- fil %>% 
  filter(complete.cases(pres12)) %>% 
  count(pres12, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres12 == 2) %>% 
  mutate(year = c("2012"))%>% 
  select(weight, year)

old <- bind_rows(p68, p72, p76, p80, p84, p88, p92, p96, p00, p04, p08, p12) %>% mutate(type = c("Over 65"))


total <- bind_rows(young, mid, midplus, old)

total$type <- factor(total$type, levels=unique(total$type))

ggplot(total, aes(x=year, y=weight, group = type, fill = type, color = type)) + 
  geom_line() + scale_y_continuous(labels = scales::percent) +
  labs(x ="Year", y = "% for the Republican Candidate", title = "White Evangelical Protestants") + theme(legend.title=element_blank()) 


fil <- gss %>% filter(race ==1 & evangelical ==1 &  age > 35)

p68 <- fil %>% 
  filter(complete.cases(pres68)) %>% 
  count(pres68, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres68 == 2) %>% 
  mutate(year = c("1968")) %>% 
  select(weight, year)

p72 <- fil %>% 
  filter(complete.cases(pres72)) %>% 
  count(pres72, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres72 == 2) %>% 
  mutate(year = c("1972")) %>% 
  select(weight, year)

p76 <- fil %>% 
  filter(complete.cases(pres76)) %>% 
  count(pres76, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres76 == 2) %>% 
  mutate(year = c("1976")) %>% 
  select(weight, year)

p80 <- fil %>% 
  filter(complete.cases(pres80)) %>% 
  count(pres80, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres80 == 2) %>% 
  mutate(year = c("1980")) %>% 
  select(weight, year)

p84 <- fil %>% 
  filter(complete.cases(pres84)) %>% 
  count(pres84, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres84 == 2) %>% 
  mutate(year = c("1984")) %>% 
  select(weight, year)

p88 <- fil %>% 
  filter(complete.cases(pres88)) %>% 
  count(pres88, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres88 == 2) %>% 
  mutate(year = c("1988")) %>% 
  select(weight, year)

p92 <- fil %>% 
  filter(complete.cases(pres92)) %>% 
  count(pres92, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres92 == 2) %>% 
  mutate(year = c("1992")) %>% 
  select(weight, year)

p96 <- fil %>% 
  filter(complete.cases(pres96)) %>% 
  count(pres96, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres96 == 2) %>% 
  mutate(year = c("1996")) %>% 
  select(weight, year)


p00 <- fil %>% 
  filter(complete.cases(pres00)) %>% 
  count(pres00, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres00 == 2) %>% 
  mutate(year = c("2000")) %>% 
  select(weight, year)

p04 <- fil %>% 
  filter(complete.cases(pres04)) %>% 
  count(pres04, wt = wtssall) %>% 
  mutate(weight = prop.table(n)) %>% 
  filter(pres04 == 2) %>% 
  mutate(year = c("2004")) %>% 
  select(weight, year)

p08 <- fil %>% 
  filter(complete.cases(pres08)) %>% 
  count(pres08, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres08 == 2) %>% 
  mutate(year = c("2008"))%>% 
  select(weight, year)

p12 <- fil %>% 
  filter(complete.cases(pres12)) %>% 
  count(pres12, wt = wtssall) %>% 
  mutate(weight = prop.table(n))%>% 
  filter(pres12 == 2) %>% 
  mutate(year = c("2012"))%>% 
  select(weight, year)

over <- bind_rows(p68, p72, p76, p80, p84, p88, p92, p96, p00, p04, p08, p12) %>% mutate(type = c("Over 35"))

two <- bind_rows(young, over)

ggplot(two, aes(x=year, y=weight, group = type, fill = type, color = type)) + 
  geom_line(size = 1.25) + scale_y_continuous(labels = scales::percent) +
  labs(x ="Year", y = "% for the Republican Candidate", title = "White Evangelical Protestants", caption = "Data: GSS (1968-2016)") + theme(legend.title=element_blank()) 


