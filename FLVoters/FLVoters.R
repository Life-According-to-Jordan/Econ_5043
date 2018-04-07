'''
Note that there were some parts that were buggy
'''

#Load Data 

#census data 
cnames<-read.csv("cnames.csv")
head(cnames)

#Florida census data 
FLCensus<-read.csv("FLCensusDem.csv")
head(FLCensus)
FLVoters<-read.csv("FLVoters.csv")
head(FLVoters)
FLCensusVTD<-read.csv("FLCensusVTD.csv")
head(FLCensusVTD)

#1) split the data by races
white <- subset(FLVoters, subset = (race == "white"))

#2) match the florida data and the census data using surname
w.index <- match(white$surname, cnames$surname)
head(w.index)

#3) For the sample of whites, the maximum of conditional probabilities should be the conditional 
#   probability of being white given the surname max{pctwhite, pctblack, pctapi, pcthispanic}

vars<-c("pctwhite", "pctwhite", "pctapi", "pctaian", "pct2prace", "pcthispanic")

#4) the success rates are defined as the instances when these two are indeed the same 
comparison <- apply(cnames[w.index,vars], 1, max) == cnames$pctwhite[w.index]

head(comparison)

#Calculations on data 
#matrix_voters<-matrix(c(1:5, 11:15), nrow = 5, ncol = 2)

#mean of the rows 
#apply(matrix_voters, 1, mean)

#mean of the columns
#apply(matrix_voters, 2, mean)

#use function on data 
#divide all values by 2 
#apply(matrix_voters, 1:2, function(x) x/2)

#5) repeat the process for 3 and 4 for other races 

#Black
black <- subset(FLVoters, subset = (race == "black"))
w.index.b <- match(black$surname, cnames$surname)
comparison.b <- apply(cnames[w.index,vars], 1, max) == cnames$pctblack[w.index]
head(comparison.b)

#Asian
asian <- subset(FLVoters, subset = (race == "asian"))
a.index <- match(asian$surname, cnames$surname)
comparison.a <- apply(cnames[a.index,vars], 1, max) == cnames$pctaian[a.index]
head(comparison.a)

#Hispanic
hispanic <- subset(FLVoters, subset = (race == "hispanic"))
h.index <- match(hispanic$surname, cnames$surname)
comparison.h <- apply(cnames[h.index,vars], 1, max) == cnames$pcthispanic[h.index]
head(comparison.h)

#Backing out information from compiled information
#P[race|surname,residence] = P[surname|race, residence] * P[race|residence] / P[surname|residence]

#relaxing the model we can solve for the alternative function
#we are missing P[surname|race, residence] but can approximate it using P[surname|race]
#this means, that we assume that residence has no impact on surname 

#Question 1
#P[race | surname] and P[surname] from Census data 
race.prop <- apply(FLCensus[,c("white", "black", "api", "hispanic", "others")],
                   2,
                   weighted.mean,
                   weights = FLCensus$total.pop)

##############################
#  Race Prop not populating
##############################
race.prop

total.count<-sum(cnames$count)

cnames$names.white <- (cnames$pctwhite/100) * (cnames$count/total.count)/race.prop["white"]

cnames$names.black <- (cnames$pctblack/100) * (cnames$count/total.count)/race.prop["black"]

cnames$names.hispanic <- (cnames$pcthispanic/100) * (cnames$count/total.count)/race.prop["hispanic"]

cnames$names.api <- (cnames$pctapi/100) * (cnames$count/total.count)/race.prop["api"]

cnames$names.other <- (cnames$pctothers/100) * (cnames$count/total.count)/race.prop["others"]

#Merge data together 
#P[race|surname,residence] = P[surname|race, residence] * P[race|residence] / P[surname|residence]
FLVoters2<-merge(x=FLVoters, y=FLCensus, by=c("county", "VTD"), all = FALSE)

head(FLVoters2)

index2<- match(FLVoters2$surname, cnames$surname)

FLVoters2$name.residence <- cnames$name.white[index2]*FLVoters2$white +
                            cnames$name.black[index2]*FLVoters2$black +
                            cnames$name.hispanic[index2]*FLVoters2$hispanic +
                            cnames$name.api[index2]*FLVoters2$api +
                            cnames$name.others[index2]*FLVoters2$others

FLVoters2$predict 

vars2<- c("predict.white", "predict.black", "predict.hispanic", "predict.api", "predict.others")

whites2<-subset(FLVoters2, subset = (race == "white"))
mean(apply(white2[, vars2], 1, max) == whites2$predict.white)

black<-subset(FLVoters2, subset = (race == "black"))
mean(apply(blacks[, vars2], 1, max) == blacks2$predict.black)
