library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(scales)
library(car)
library(extrafont)

gss <- read_csv("pid.csv")
gss16 <- read_dta("D:/cces/data/gss16.dta")
gss$X1 <- NULL
gss16 <- gss16 %>%  select(year, evangelical, mainline, catholic, blackprot, jewish, otherfaith, nofaith, reltrad, partyid)
gss <- rbind(gss, gss16)

pid <- gss %>% group_by(reltrad, year) %>% summarise(mean = mean(partyid, na.rm = TRUE))

pid$reltrad <- Recode(pid$reltrad, "1='Evangelical Protestants';
2='Mainline Protestants';
3='Black Protestants';
4='Catholic';
5='Jewish';
6= 'Other Faith';
7= 'No Faith'; else = NA")

year <- gss %>% group_by(year) %>% summarise(mean = mean(partyid, na.rm = TRUE))
year$reltrad <- c("Entire Sample")
year <- year[,c(3,1,2)]
pid <- pid %>% ungroup(reltrad)
pid <- rbind(pid, year)

pid <- na.omit(pid)

ggplot(pid, aes(x=year, y=mean, color = reltrad, label = reltrad)) + geom_line(aes(group=reltrad), size = 1.5) + 
  coord_flip() + scale_y_continuous(breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + labs(x= "Year", y = "Party Identification", title = "How has Partisanship Shifted Over Time?", caption = "Data from the GSS (1972-2016)")+
  scale_color_manual(values = c("#F8766D", "#CD9600", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"))

ggsave(file="long.png", type = "cairo-png", width = 9, height = 10)

