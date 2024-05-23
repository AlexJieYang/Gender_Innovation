library(fixest)
library(data.table)
data <- read.csv('E:/NETDATA/SciSciNet/1950-2020/Paperdata/Gender/p4reg.csv')

library(dplyr)

data <- data %>%
  mutate(D = ifelse(Disruption5 > 0, 1, 0),
         Novelty = ifelse(Atyp > 0, 1, 0),
         Female = ifelse(`P.gf.` > 0.5, 1, 0),
         Team_Size2 = ifelse(Team_Size <= 10, Team_Size, 11),
         Age2 = ifelse(Age <= 20, Age, 21),)

data1 <- data %>%
  filter(AuthorSequenceNumber == 1)

reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2, data1, split=~Year, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg111.csv")
reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~FieldID, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg112.csv")
reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~Team_Size2, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg113.csv")
reg1<-feglm(Citation_Count5~Female+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~Age2, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg114.csv")
reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~AffRank, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg115.csv")


reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2, data1, split=~Year, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg211.csv")
reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~FieldID, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg212.csv")
reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~Team_Size2, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg213.csv")
reg1<-feglm(Novelty~Female+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~Age2, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg214.csv")
reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~AffRank, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg215.csv")


reg1<-feglm(D~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2, data1, split=~Year, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg311.csv")
reg1<-feglm(D~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~FieldID, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg312.csv")
reg1<-feglm(D~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~Team_Size2, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg313.csv")
reg1<-feglm(D~Female+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~Age2, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg314.csv")
reg1<-feglm(D~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data1, split=~AffRank, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg315.csv")

########################################################################
########################################################################
data2 <- data %>%
  filter(AuthorSequenceNumber != 1)


reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2, data2, split=~Year, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg121.csv")
reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~FieldID, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg122.csv")
reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~Team_Size2, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg123.csv")
reg1<-feglm(Citation_Count5~Female+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~Age2, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg124.csv")
reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~AffRank, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg125.csv")

reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2, data2, split=~Year, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg221.csv")
reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~FieldID, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg222.csv")
reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~Team_Size2, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg223.csv")
reg1<-feglm(Novelty~Female+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~Age2, family="binomial")
write.csv(etable(reg1) , "E:/reg224.csv")
reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~AffRank, family="binomial")
write.csv(etable(reg1) , "E:/reg225.csv")

reg1<-feglm(D~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2, data2, split=~Year, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg321.csv")
reg1<-feglm(D~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~FieldID, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg322.csv")
reg1<-feglm(D~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~Team_Size2, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg323.csv")
reg1<-feglm(D~Female+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~Age2, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg324.csv")
reg1<-feglm(D~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID2+Year, data2, split=~AffRank, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg325.csv")

############################################################################################
############################################################################################
data3 <- data %>%
  filter(CountryID < 100)
reg1<-feglm(Citation_Count5~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID+Year, data3, split=~CountryID, se="hetero", family="poisson")
write.csv(etable(reg1) , "E:/reg16.csv")
reg1<-feglm(Novelty~Female+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+log(Team_Size)+InterDisTeam+International+Funding+FocalField|FieldID+Year, data3, split=~CountryID, se="hetero", family="binomial")
write.csv(etable(reg1) , "E:/reg26.csv")

############################################################################################
############################################################################################
reg1<-feglm(Citation_Count5~Female|FieldID2+Year, data1, se="hetero", family="poisson")
reg2<-feglm(Citation_Count5~Female+log(Team_Size)+InterDisTeam+International|FieldID2+Year, data1, se="hetero", family="poisson")
reg3<-feglm(Citation_Count5~Female+log(Team_Size)+InterDisTeam+International+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+Funding+FocalField|FieldID2+Year, data1, se="hetero", family="poisson")
reg4<-feglm(Citation_Count5~Female|FieldID2+Year, data2, se="hetero", family="poisson")
reg5<-feglm(Citation_Count5~Female+log(Team_Size)+InterDisTeam+International|FieldID2+Year, data2, se="hetero", family="poisson")
reg6<-feglm(Citation_Count5~Female+log(Team_Size)+InterDisTeam+International+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+Funding+FocalField|FieldID2+Year, data2, se="hetero", family="poisson")
write.csv(etable(reg1,reg2,reg3,reg4,reg5,reg6) ,  "E:/reg1.csv")

reg1<-feglm(Novelty~Female|FieldID2+Year, data1, se="hetero")
reg2<-feglm(Novelty~Female+log(Team_Size)+InterDisTeam+International|FieldID2+Year, data1, se="hetero")
reg3<-feglm(Novelty~Female+log(Team_Size)+InterDisTeam+International+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+Funding+FocalField|FieldID2+Year, data1, se="hetero")
reg4<-feglm(Novelty~Female|FieldID2+Year, data2, se="hetero")
reg5<-feglm(Novelty~Female+log(Team_Size)+InterDisTeam+International|FieldID2+Year, data2, se="hetero")
reg6<-feglm(Novelty~Female+log(Team_Size)+InterDisTeam+International+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+Funding+FocalField|FieldID2+Year, data2, se="hetero")
write.csv(etable(reg1,reg2,reg3,reg4,reg5,reg6) , "E:/reg22.csv")

reg1<-feglm(Novelty~Female|FieldID2+Year, data1, se="hetero")
reg2<-feglm(Novelty~Female+log(Team_Size)+InterDisTeam+International|FieldID2+Year, data1, se="hetero")
reg3<-feglm(Novelty~Female+log(Team_Size)+InterDisTeam+International+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+Funding+FocalField|FieldID2+Year, data1, se="hetero")
reg4<-feglm(Novelty~Female|FieldID2+Year, data2, se="hetero")
reg5<-feglm(Novelty~Female+log(Team_Size)+InterDisTeam+International|FieldID2+Year, data2, se="hetero")
reg6<-feglm(Novelty~Female+log(Team_Size)+InterDisTeam+International+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+Funding+FocalField|FieldID2+Year, data2, se="hetero")
write.csv(etable(reg1,reg2,reg3,reg4,reg5,reg6) , "E:/reg22.csv")

reg1<-feglm(D~Female|FieldID2+Year, data1, se="hetero", family="binomial")
reg2<-feglm(D~Female+log(Team_Size)+InterDisTeam+International|FieldID2+Year, data1, se="hetero", family="binomial")
reg3<-feglm(D~Female+log(Team_Size)+InterDisTeam+International+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+Funding+FocalField|FieldID2+Year, data1, se="hetero", family="binomial")
reg4<-feglm(D~Female|FieldID2+Year, data2, se="hetero", family="binomial")
reg5<-feglm(D~Female+log(Team_Size)+InterDisTeam+International|FieldID2+Year, data2, se="hetero", family="binomial")
reg6<-feglm(D~Female+log(Team_Size)+InterDisTeam+International+log(Age+1)+log(Past_Pub+1)+log(Past_Hit+1)+Funding+FocalField|FieldID2+Year, data2, se="hetero", family="binomial")
write.csv(etable(reg1,reg2,reg3,reg4,reg5,reg6) ,  "E:/reg3.csv")