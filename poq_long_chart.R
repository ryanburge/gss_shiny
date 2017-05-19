gss <- read_dta("D:/GSS_reltrad.dta", encoding = NULL)
gss16 <- read_dta("D:/cces/data/gss16.dta")


gss$bagain <- Recode(gss$reborn, "1=1;else=0")
gss$white <- Recode(gss$race, "1=1;else=0")
gss$protestant <- Recode(gss$relig, "1=1;else=0")


gss$whtevan <- gss$evangelical + gss$white
gss$whtbaprot <- gss$bagain + gss$white + gss$protestant

whtevan <- gss %>% filter(whtevan ==2 & year >=2004) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm=TRUE))
whtbaprot <- gss %>% filter(whtbaprot ==3 & year >=2004) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm=TRUE))
whtevan$label <- c("White Evangelical Protestant")
whtbaprot$label <- c("White BA + Prot.")


gss16$bagain <- Recode(gss16$reborn, "1=1;else=0")
gss16$white <- Recode(gss16$race, "1=1;else=0")
gss16$protestant <- Recode(gss16$relig, "1=1;else=0")


gss16$whtevan <- gss16$evangelical + gss16$white
gss16$whtbaprot <- gss16$bagain + gss16$white + gss16$protestant

whtevan16 <- gss16 %>% filter(whtevan ==2 & year >=2004) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm=TRUE))
whtbaprot16 <- gss16 %>% filter(whtbaprot ==3 & year >=2004) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm=TRUE))
whtevan16$label <- c("White Evangelical Protestant")
whtbaprot16$label <- c("White BA + Prot.")

pid <- rbind(whtbaprot, whtevan, whtbaprot16, whtevan16)



ggplot(pid, aes(x=year, y=mean, color = label, label = label)) + geom_line(aes(group=label), size = 1.5) + 
  coord_flip() + scale_y_continuous(limits = c(0,6), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14, family="KerkisSans")) + labs(x= "Year", y = "Party Identification", title = "How has Partisanship Shifted Over Time?", caption = "Data from the GSS (1972-2016)")+
  scale_color_manual(values = c("black", "gray", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"))

ggsave(file="long_compare_poq.png", type = "cairo-png", width = 9, height = 10)

