#
# Read data from CSV file
amano <- read.csv("amanodata.csv",header=T)

# Example for cross tab

gen_race <- table(amano$Gender, amano$Race)
gen_race

barplot(gen_race,beside=TRUE,legend=c("Female","Male"))

# Example of cross tab for multiple selection.
# In the sample data, Q4 is a multiple selection asking like
# "Where did you take JF?" and selections are "sushi","teppanyaki","ramen","takoyaki"

# count answers for "NO"
q4_no <- 1:4
q4_no[1] <- table(amano$sushi)[1]
q4_no[2] <- table(amano$teppanyaki)[1]
q4_no[3] <- table(amano$ramen)[1]
q4_no[4] <- table(amano$takoyaki)[1]
q4_no

# count answers for "YES"
q4_yes <- 1:4
q4_yes[1] <- table(amano$sushi)[2]
q4_yes[2] <- table(amano$teppanyaki)[2]
q4_yes[3] <- table(amano$ramen)[2]
q4_yes[4] <- table(amano$takoyaki)[2]
q4_yes

q4 <- matrix(1,nrow=4,ncol=2)
q4[,1] <- q4_no
q4[,2] <- q4_yes

colnames(q4) <- c("No","Yes")
rownames(q4) <- c("sushi","teppan","ramen","tako")
q4

# q4[,2] is the number for "Yes"
barplot(q4[,2],col=heat.colors(nrow(q4)))

#
# Chi square test 
chisq.test(q4[,2])

#
#  Cross table for Gender and Q4

q4_gen <- matrix(1:8,nrow=2,ncol=4)
q4_gen[,1] <- table(amano$Gender, amano$sushi)[,2]
q4_gen[,2] <- table(amano$Gender, amano$teppanyaki)[,2]
q4_gen[,3] <- table(amano$Gender, amano$ramen)[,2]
q4_gen[,4] <- table(amano$Gender, amano$takoyaki)[,2]


rownames(q4_gen) <- c("Female","Male")
colnames(q4_gen) <- c("sushi","teppan","ramen","takoyaki")
q4_gen

barplot(q4_gen, col=heat.colors(nrow(q4_gen)),beside=TRUE)
chisq.test(q4_gen)

#
#  Cross table for Occupatio<->Q4

q4_occ <- matrix(1:24,nrow=6,ncol=4)
q4_occ[,1] <- table(amano$n_occupation, amano$sushi)[,2]
q4_occ[,2] <- table(amano$n_occupation, amano$teppanyaki)[,2]
q4_occ[,3] <- table(amano$n_occupation, amano$ramen)[,2]
q4_occ[,4] <- table(amano$n_occupation, amano$takoyaki)[,2]

rownames(q4_occ) <- 
  c("Goverment","JPcompany","PrivateSector","Self-employed","Student","Unemployed")
colnames(q4_occ) <- c("sushi","teppan","ramen","takoyaki")

q4_occ
barplot(q4_occ, col=heat.colors(nrow(q4_occ)),  beside=TRUE)

chisq.test(q4_occ)

#
# q6 : I prefer consuming JP food to another food (answering in 1 to 5)

t.test(amano$q6)
amano$q6
# LM 

attach(amano)

res_lm <- lm(q6 ~ n_gender)
summary(res_lm)

res_lm <- lm(q6 ~ n_occupation)
summary(res_lm)

res_lm <- lm(q6 ~ n_occupation + sushi + teppanyaki + ramen + takoyaki)
summary(res_lm)

res_lm <- lm(q6 ~ n_q5 + n_occupation + sushi + teppanyaki + ramen + takoyaki)
summary(res_lm)

#
# q8 : I believe JP food is safety

t.test(q8)

res_lm <- lm(q8 ~ n_gender)
summary(res_lm)

res_lm <- lm(q8 ~ n_occupation)
summary(res_lm)

res_lm <- lm(q8 ~ sushi + teppanyaki + ramen + takoyaki)
summary(res_lm)

res_lm <- lm(q8 ~ n_q5 + sushi + teppanyaki + ramen + takoyaki)
summary(res_lm)

#
# Simple Correspondence analysis

library ( MASS )

corresp(q4_occ,nf=2)
biplot( corresp ( q4_occ, nf = 2 ), xlim=c(-0.5,0.5), ylim=c(-0.5,0.5))

# eigenvalue
corresp_q4_occ$cor
ev_corresp_q4_occ <- corresp_q4_occ$cor^2
contrib <- round( 100 * ev_corresp_q4_occ / sum(ev_corresp_q4_occ), 1 )
contrib

#
# Simple correspondece analysis for wage and q4

q4_wage <- matrix(1:16,nrow=4,ncol=4)
q4_wage[,1] <- table(n_wage,sushi)[,2]
q4_wage[,2] <- table(n_wage,teppanyaki)[,2]
q4_wage[,3] <- table(n_wage,ramen)[,2]
q4_wage[,4] <- table(n_wage,takoyaki)[,2]
rownames(q4_wage) <- c("below RM1500", "RM1500-2500", "RM2500-3500", "Above RM3500")
colnames(q4_wage) <- c("sushi","teppan","ramen","takoyaki")
q4_wage

biplot( corresp ( q4_wage, nf = 2 ))

##########################
# ANOVA
##########################

# q6 : I prefer consuming JF to another food product (1 to 5)
# OneWay ANOVA 

tapply (amano$q6, amano$Gender, mean)
anova(aov(amano$q6 ~ amano$Gender))

tapply (amano$q6, amano$Race, mean)
anova(aov(amano$q6 ~ amano$Race))

# q1 : Have you ever bought JF? (Yes or No)
tapply (amano$q6, amano$q1, mean)
anova(aov(amano$q6 ~ amano$q1))

# q8 : I believe JF is healthy (1 to 5)
tapply (amano$q8, amano$q1, mean)
anova(aov(amano$q8 ~ amano$q1))

tapply (amano$q8, amano$n_wage, mean)
anova(aov(amano$q8 ~ amano$n_wage))

# q9 : I believe JF is safe (1 to 5)
tapply (amano$q9, amano$q1, mean)
anova(aov(amano$q9 ~ amano$q1))

tapply (amano$q9, amano$n_wage, mean)
anova(aov(amano$q9 ~ amano$n_wage))

# TwoWay ANOVA
anova(aov(amano$q9 ~ amano$n_wage + amano$q8))
cor(amano$q8, amano$q9)
cor(amano$q9, amano$n_wage)
cor(amano$q8, amano$n_wage)

# q10 : I believe JF is expensive. (1 to 5)
anova(aov(amano$q10 ~ amano$n_wage + amano$q9))

# q12 : HALAL certification is important for JF. (1 to 5)
# ANOVA + TukeyHSD
tapply (amano$q12, amano$Race, mean)
anova(aov(amano$q12 ~ amano$Race))
TukeyHSD(aov(amano$q12 ~ amano$Race))

# Factor analysis for q6 to q12
# column number of q6 is 22
# col num of q12 is 28
#
# q6 : Preference of JR
# q7 : Preference of JF
# q8 : Healthiness
# q9 : Safety
# q10 : Expensiveness
# q11 : Favour
# q12 : HALAL

res_fa <- factanal(amano[,22:28],factors=2)
res_fa

res_fa <- factanal(amano[,22:27],factors=2)
res_fa

res_fa <- factanal(amano[,22:27],factors=2,rotation="varimax")
res_fa

res_fa <- factanal(amano[,22:27],factors=2,rotation="promax")
res_fa

par(mfrow = c(1, 2))
barplot(res_fa$loadings[,1],main="Factor 1",ylim=c(0,1))
barplot(res_fa$loadings[,2],main="Factor 2",ylim=c(0,1))

par(mfrow = c(1, 1))

fix(res_fa)

#
# Principal component analysis
# Use prcomp. princomp is not good in some cases.

res_pr <- prcomp(amano[,22:27],scale=T)
res_pr
summary(res_pr)
biplot(res_pr,xlim=c(-0.2,0.2),ylim=c(-0.2,0.2))

fix(res_pr)

#
# Cluster analysis

# q5 : Visit to Japan,  amano[,21]
# q6 : Preference of JR
# q7 : Preference of JF
# q8 : Healthiness
# q9 : Safty
# q10 : Expensiveness
# q11 : Favour
# q12 : HALAL

############################
## CLUSTER ANALYSIS 
########################
amano_dist <- dist(amano[,21:28])
amano_hc <- hclust(amano_dist)
plot(amano_hc,hang=-1)


amano_clust <- cutree(amano_hc,k=4)
amano_clust
table(amano_clust)

# Scaling and clustering 
amano_scale <- scale(amano[,21:28])
amano_scale_d <- dist(amano_scale)
amano_scale_hc <- hclust(amano_scale_d)
plot(amano_scale_hc,hang=-1)

amano_cluster <- cutree(amano_scale_hc,k=4)
table(amano_cluster)

# Discuss about the charecteritic of each cluster
os <- cbind(amano[,21:28],amano_cluster)
head(os)

by(os[,1:8],os[,9],mean)
table(os[,9])


#
# If "by" function faied, use this for calculation of mean in each cluster
mean_clust <- matrix(1,nrow=4,ncol=8)
for(i in 1:8){mean_clust[,i] <- tapply(os[,i],os[,9],mean)}
rownames(mean_clust) <- c("Group 1", "Group 2", "Group 3", "Group 4")
colnames(mean_clust) <- c("q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12")
mean_clust

par(mfrow=c(1,4))
for(i in 1:4){barplot(mean_clust[i,2:8]-3,main=i)}
par(mfrow=c(1,1))
