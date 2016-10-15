gss <- read_dta("D:/GSS_reltrad.dta", encoding = NULL)
small_gss <- select(gss, year, evangelical, mainline, catholic, blackprot, jewish, otherfaith, nofaith, reltrad, partyid)
write.csv(small_gss, "raphael.csv")
