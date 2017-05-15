GSS_reltrad <- read_dta("D:/GSS_reltrad.dta")
gss <- GSS_reltrad %>% select(year, evangelical, mainline, catholic, blackprot, jewish, otherfaith, nofaith, reltrad, partyid, race, attend)
gss$white <- Recode(gss$race, "1=1;else=0")
gss$whtevan <- gss$white + gss$evangelical
gss$whtevan <- Recode(gss$whtevan, "2=1; else=0")
gss$wkattend <- Recode(gss$attend, "7:8=1;else=0")
gss$evanatt <- Recode(gss$evanatt, "2=1; else=0")

evatt <- gss %>% filter(evanatt ==1) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE)) %>% ungroup(year)
evwht <- gss %>% filter(whtevan ==1) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE)) %>% ungroup(year)
evan <- gss %>% filter(evangelical ==1) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE)) %>% ungroup(year)
evatt$label <- c("Weekly Attending Evangelical")
evwht$label <- c("White Evangelical")
evan$label <- c("Not Black Evangelical")
pid <- rbind(evatt, evwht, evan)

evan2 <- gss %>% filter(evangelical ==1 & white ==1 & wkattend ==1) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE)) %>% ungroup(year)
evan2$label <- c("Weekly Attending White Evangelical")
pid <- rbind(pid, evan2)

sbc <- GSS_reltrad %>% filter(denom ==14 & race ==1) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE)) %>% ungroup(year)
sbc$label <- c("White So. Baptist")


sbc16 <- gss16 %>% filter(denom ==14 & race ==1) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE)) %>% ungroup(year)
sbc16$label <- c("White So. Baptist")


umc <- GSS_reltrad %>% filter(denom ==22 & race ==1) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE)) %>% ungroup(year)
umc$label <- c("White United Methodist")


umc16 <- gss16 %>% filter(denom ==14 & race ==1) %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE)) %>% ungroup(year)
umc16$label <- c("White United Methodist")

ggplot(pid, aes(x=year, y=mean, color = label, label = label)) + geom_line(aes(group=label), size = 1.5) + 
  coord_flip() + 
  scale_y_continuous(limits = c(0,6), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + labs(x= "Year", y = "Party Identification", title = "How has Partisanship Shifted Over Time?", caption = "Data from the GSS (1972-2016)")+
  scale_color_manual(values = c("#F8766D", "#CD9600", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) 