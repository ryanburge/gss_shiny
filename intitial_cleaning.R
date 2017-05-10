library(readr)
library(dplyr)
library(haven)

gss <- read_dta("D:/GSS_reltrad.dta", encoding = NULL)
small_gss <- select(gss, year, evangelical, mainline, catholic, blackprot, jewish, otherfaith, nofaith, reltrad, partyid)
small_gss$partyid[small_gss$partyid == 7] <- NA

#write.csv(small_gss, "raphael.csv")

#gss72 <- filter(small_gss, year==1972)
#crosstab(gss72$evangelical, gss72$partyid)
#crosstab(gss72$mainline, gss72$partyid)
#crosstab(gss72$blackprot, gss72$partyid)
#crosstab(gss72$catholic, gss72$partyid)
#crosstab(gss72$jewish, gss72$partyid)
#crosstab(gss72$otherfaith, gss72$partyid)
#crosstab(gss72$nofaith, gss72$partyid)

#test$partyid <- factor(test$partyid, levels=unique(test$partyid))
#test %>%  filter(reltrad == "evangelical") %>% ggplot(aes(x= partyid, y=count)) + geom_bar(stat = "identity")



small_gss$partyid <- factor(small_gss$partyid, levels = c(0,1,2,3,4,5, 6),labels = c("Strong Democrat", "Moderate Democrat", "Lean Democrat", "Independent","Lean Republican", "Moderate Republican", "Strong Republican"))
na_gss <- na.omit(small_gss)

na_gss %>%  filter(mainline == 1 & year == 1972) %>% ggplot(aes(x= partyid, fill=factor(partyid)), color= factor(partyid)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="RdBu", direction=-1) + theme(legend.position="none")  + xlab("Party Identification") + ylab("Number of Respondents")