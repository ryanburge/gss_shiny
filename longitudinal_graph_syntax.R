library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(scales)
library(car)
library(extrafont)

library(haven)
gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")

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

ggplot(pid, aes(x=year, y=mean, color = reltrad, label = reltrad)) + geom_point(aes(group=reltrad), size = 1.5) + 
  coord_flip() + scale_y_continuous(breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + labs(x= "Year", y = "Party Identification", title = "How has Partisanship Shifted Over Time?", caption = "Data from the GSS (1972-2016)")+
  scale_color_manual(values = c("#F8766D", "#CD9600", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) + geom_smooth()

ggsave(file="long.png", type = "cairo-png", width = 9, height = 10)


gss <- gss_reltrad %>% 
  group_by(year) %>% 
  count(reltrad, wt = wtssall) %>% 
  mutate(mean = prop.table(n)) %>% 
  mutate(reltrad =as.numeric(reltrad)) %>% 
  na.omit()

gss$reltrad <- Recode(gss$reltrad, "1='Evangelical Protestants';
2='Mainline Protestants';
3='Black Protestants';
4='Catholic';
5='Jewish';
6= 'Other Faith';
7= 'No Faith'; else = NA")

### NOW WITH ANIMATION


i0<-nrow(gss)



oopt = ani.options(interval = 0.1)
saveGIF({for (i in 1:i0) {
  g<- ggplot(gss, aes(x=year, y=mean*100, color = reltrad, label = reltrad)) + geom_smooth(data=gss[1:i,], aes(group=reltrad), se = FALSE) + 
    theme(legend.title = element_blank()) +
    scale_x_continuous(limits = c(1972,2016)) +
    scale_y_continuous(limits = c(0,30)) +
    theme(legend.position="bottom") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text=element_text(size=28, family="KerkisSans")) + labs(x= "Year", y = "Percent of Population", title = "How has Religious Demography Shifted Over Time?", caption = "Data from the GSS (1972-2016)")+
    scale_color_manual(values = c("yellow3", "black", "darkmagenta", "#00BE67","firebrick1", "#00BFC4", "dodgerblue4")) 
  
  print(g)
  ani.pause()
}
  #Add a bunch of images to pause at end
  
  for (i2 in 1:30) {
   
   g<- ggplot(gss, aes(x=year, y=mean*100, color = reltrad, label = reltrad)) + geom_smooth(data=gss[1:i,], aes(group=reltrad), se = FALSE) + 
      theme(legend.title = element_blank()) +
      theme(legend.position="bottom") +
     scale_x_continuous(limits = c(1972,2016)) +
     scale_y_continuous(limits = c(0,30)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text=element_text(size=28, family="KerkisSans")) + labs(x= "Year", y = "Percent of Population", title = "How has Religious Demography Shifted Over Time?", caption = "Data from the GSS (1972-2016)")+
      scale_color_manual(values = c("yellow3", "black", "darkmagenta", "#00BE67","firebrick1", "#00BFC4", "dodgerblue4")) 
    
      
    print(g)
    ani.pause()
  }
},movie.name="your_awesome_gif.gif",ani.width = 800, ani.height = 500)

ggplot(gss, aes(x=year, y=mean*100, color = reltrad, label = reltrad)) + geom_smooth(data=gss[1:i,], aes(group=reltrad), se = FALSE) + 
  theme(legend.title = element_blank()) +
  scale_x_continuous(limits = c(1972,2016)) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + labs(x= "Year", y = "Percent of Population", title = "How has Religious Demography Shifted Over Time?", caption = "Data from the GSS (1972-2016)")+
  scale_color_manual(values = c("yellow3", "black", "darkmagenta", "#00BE67","firebrick1", "#00BFC4", "dodgerblue4")) 

ggsave(file="population_shifts_overall.png", type = "cairo-png", width = 15, height = 10)


## Doing a Gender Plot 

plot <- gss %>% filter(reltrad ==1 & race ==1 & partyid <=6) %>%  group_by(sex, year) %>% summarise(mean = mean(partyid, na.rm = TRUE))

plot$sex <- as.numeric(plot$sex)

plot$sex <- Recode(plot$sex, "1='White Evangelical Male';
2='White Evangelical Female';
else = NA")

plot2 <- gss %>% filter(race ==1 & partyid <=6) %>%  group_by(sex, year) %>% summarise(mean = mean(partyid, na.rm = TRUE))

plot2$sex <- as.numeric(plot2$sex)

plot2$sex <- Recode(plot2$sex, "1='White Male';
2='White Female';
else = NA")

plot3 <- bind_rows(plot, plot2)

plot3$sex <- fct_relevel(plot3$sex, "White Female", "White Male", "White Evangelical Female", "White Evangelical Male")

ggplot(plot3, aes(x=year, y=mean, color = sex, label = sex)) + 
  coord_flip() + scale_y_continuous(limits= c(2,5), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Dem.", "Independent", "Ind., Near Rep.", "Not Strong Republican", "Strong Republican")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + labs(x= "Year", y = "Party Identification", title = "How Has Partisanship Shifted Over Time?", caption = "Data from the GSS (1972-2016)")+
  geom_smooth(se = FALSE, size =2) + scale_color_manual(values = c("mediumpurple1", "seagreen1", "mediumpurple4", "seagreen4","firebrick1", "#00BFC4", "dodgerblue4"))  

ggsave(file="gender_pid_shift.png", type = "cairo-png", width = 15, height = 10)


