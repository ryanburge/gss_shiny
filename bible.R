gss <- read_dta("C:/Users/Ryan Burge/Desktop/GSS7216_R1a.dta")
bible <- gss %>% select(year, bible)
bible$literal <- Recode(bible$bible, "1=1; else=0")
bible$inspired <- Recode(bible$bible, "2=1; else=0")
bible$fables <- Recode(bible$bible, "3=1; else=0")

bible <- na.omit(bible)

literal <- bible %>%  group_by(year) %>% summarise(mean = mean(literal, na.rm = TRUE)) 

inspired <- bible %>% group_by(year) %>% summarise(mean = mean(inspired, na.rm = TRUE)) 

fables <- bible %>% group_by(year) %>% summarise(mean = mean(fables, na.rm = TRUE)) 

literal$label <- c("Literal Word of God")
inspired$label <- c("Inspired Word of God")
fables$label <- c("Book of Fables")



plot <- rbind(literal, inspired, fables)

plot <- plot %>% filter(mean >0)

ggplot(plot, aes(x=year, y=mean*100, color = label, label = label)) + geom_point(aes(group=label), size = 1.5)   +
  theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + labs(x= "Year", y = "% of Respondents", title = "Respondents' View of the Bible", caption = "Data from the GSS (1984-2016)")+
  scale_color_manual(values = c("#F8766D", "#CD9600", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) + geom_smooth()