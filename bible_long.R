
gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")


bible <- gss %>% select(year, bible, reltrad, partyid, cappun)
bible$literal <- Recode(bible$bible, "1=1; else=0")
bible$inspired <- Recode(bible$bible, "2=1; else=0")
bible$fables <- Recode(bible$bible, "3=1; else=0")

bible <- na.omit(bible)

literal <- bible %>%  group_by(year) %>% summarise(mean = mean(literal, na.rm = TRUE)) 

theme_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 40, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =24),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =24), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =18)
)
  
}




literal %>% ggplot(., aes(x=year, y=mean*100)) + geom_point(colour = "black", size =2, shape =21, stroke =2, show.legend = F)   + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Year", y = "% of Respondents", title = "What Percentage of Americans are Literalists?", caption = "Data from the GSS (1984-2016)")+
  geom_line(colour = "firebrick3", size = 1.25) + theme_rb() + ylim(25, 50)

ggsave(file="D://cces/literalist_long.png", type = "cairo-png", width = 20, height =12)

