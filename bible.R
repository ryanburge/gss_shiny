gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")


bible <- gss %>% select(year, bible, reltrad, partyid, cappun)
bible$literal <- Recode(bible$bible, "1=1; else=0")
bible$inspired <- Recode(bible$bible, "2=1; else=0")
bible$fables <- Recode(bible$bible, "3=1; else=0")

bible <- na.omit(bible)

literal <- bible %>%  group_by(year) %>% summarise(mean = mean(literal, na.rm = TRUE)) 

inspired <- bible %>% group_by(year, reltrad) %>% summarise(mean = mean(inspired, na.rm = TRUE)) 

fables <- bible %>% group_by(year, reltrad) %>% summarise(mean = mean(fables, na.rm = TRUE)) 

literal$label <- c("Literal Word of God")
inspired$label <- c("Inspired Word of God")
fables$label <- c("Book of Fables")



plot <- rbind(literal, inspired, fables)

plot$reltrad <- Recode(plot$reltrad, "1='Evangelical Protestants';
                       2='Mainline Protestants';
                      3='Black Protestants';
                      4='Catholic'; 
                      5='Jewish';
                      6= 'Other Faith';
                      7= 'No Faith'", as.factor = TRUE)


plot <- plot %>% filter(mean >0)

plot %>% ggplot(., aes(x=year, y=mean*100, color = label, label = label)) + geom_point(aes(group=label), size = 1.5)   +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=26, family="KerkisSans")) + labs(x= "Year", y = "% of Respondents", title = "Respondents' View of the Bible", caption = "Data from the GSS (1984-2016)")+
  scale_color_manual(values = c("#F8766D", "#CD9600", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) + geom_smooth() + facet_grid(. ~ fct_inorder(reltrad))

ggsave(file="graph1.png", type = "cairo-png", width = 15, height = 10)


### Party ID Charts

bible <- gss %>% select(year, bible, reltrad, partyid) %>% na.omit()
bible$repub <- Recode(bible$partyid, "4:6=1; else=0")
bible$literal <- Recode(bible$bible, "1=1; else=0")

lit <- bible %>% filter(literal ==1) %>% group_by(year, reltrad) %>% summarise(repub = mean(repub)) %>% ungroup(year, reltrad)


plot1 <- bible %>% group_by(year, reltrad) %>% 
  summarise(rep = mean(repub, na.rm = TRUE)) %>% 
  mutate(label = c("Partisanship")) %>% 
  ungroup(year, reltrad)

plot <- bible %>% group_by(year, reltrad) %>% 
  summarise(rep = mean(literal, na.rm = TRUE)) %>% 
  mutate(label = c("Literalism")) %>% 
  ungroup(year, reltrad) %>% bind_rows(plot1)


plot$reltrad <- Recode(plot$reltrad, "1='Evangelical Protestants';
2='Mainline Protestants';
3='Black Protestants';
4='Catholic';
5='Jewish';
6= 'Other Faith';
7= 'No Faith'", as.factor = TRUE)


lit %>% ggplot(., aes(x=year, y=repub*100, color = reltrad, label = reltrad)) +  
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=30, family="KerkisSans")) + labs(x= "Year", y = "% Republican ID", title = "The Political Ideology of Biblical Literalists ", caption = "Data from the GSS (1984-2016)")+
  geom_smooth(se = FALSE) + 
  scale_x_continuous(breaks = seq(min(lit$year), max(lit$year), by = 2)) +
  scale_color_manual(values = c("#F8766D", "#CD9600", "black", "darkmagenta", "#00BE67","yellow", "turquoise", "violet"))

ggsave(file="literal_PID_1.png", type = "cairo-png", width = 15, height = 10)

plot %>% ggplot(., aes(x=year, y=rep*100, color = label, label = label)) + geom_point(aes(group=label), size = 1.5)   +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=26, family="KerkisSans")) + labs(x= "Year", y = "% Literalists or % Republican ID", title = "How Has Partisanship and Religious Conservatism Change Over Time? ", caption = "Data from the GSS (1984-2016)")+
  scale_color_manual(values = c("#F8766D", "#CD9600", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) + geom_smooth() + facet_grid(. ~ fct_inorder(reltrad))

ggsave(file="graph3.png", type = "cairo-png", width = 15, height = 10)