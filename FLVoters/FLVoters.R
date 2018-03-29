#Load Data 
cnames<-read.csv("cnames.csv")
FLCensusDem<-read.csv("FLCensusDem.csv")
FLVoters<-read.csv("FLVoters.csv")
FLCensusVTD<-read.csv("FLCensusVTD.csv")

#1) split the data by races

white <- subset(FLVoters, subset = (race == "white"))

#2) match the florida data and the census data using surname

w.index <- match(white$surname, cnames$surname)

head(w.indx)

#3) For the sample of whites, the maximum of conditional probabilities should be the conditional probability of being white given the surname 

    #max{pctwhite, pctblack, pctapi, pcthispanic}

vars<-c("pctwhite", "pctwhite", "pctapi", "pctaian", "pct2prace", "pcthispanic")

#4) the success rates are defined as the instances when these two are indeed the same 

comparison <- apply(cnames[w.index,vars], 1, max) == cnames$pcrwhite[w.indx]

comparison

#5) repeat the process for 3 and 4 for other races 
