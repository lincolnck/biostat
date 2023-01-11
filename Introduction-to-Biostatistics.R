##############################################
#
#  Introduction-to-Biostatistics.R
#     Author: Allison N. Tegge (ategge@vt.edu)
#     Date: February 8, 2022
#     Source: https://www.sheffield.ac.uk/mash/statistics/datasets
#
##############################################


# read in the dataset ####
data = read.csv("Birthweight_reduced_kg_R.csv", 
                header=T, 
                sep=',')

###############################################
# look at the data to ensure it was read in correctly ####
View(data)

###############################################
# summarize variables ####
summary(data)

###############################################
# but overall, the previous output is not  ####
# super informative
# let's explore our data more

# distribution of length of baby (cm)
summary(data$Length)
hist(data$Length, nclass = 40)

# distribution of Birthweight (kg)
summary(data$Birthweight)
hist(data$Birthweight, nclass = 20)

# distribution of maternal number of cigarettes smoked per day
summary(data$mnocig)
hist(data$mnocig,nclass = 10)
# is this data skewed? 

# distribution of gestation (weeks)
hist(data$Gestation)

################################################
# We hypothesize that the average gestation ####
#   is less than 40 weeks
# H0: the average gestatopm age is >= 40
# HA: the average gestation age is < 40

# by hand
m.gest = mean(data$Gestation)
sd.gest = sd(data$Gestation)
n.gest = length(data$Gestation)

t.obs = (m.gest - 40)/(sd.gest/sqrt(n.gest))
pt(t.obs, df=n.gest-1, lower.tail = T)

# using the function
t.test(data$Gestation, mu = 40, alternative = "less")

#################################################
# We hypothesize that average gestation will ####
#   be different between babies of mothers that
#   smoked cigarettes and babies of mothers that
#   did not smoke cigarettes.
# H0: difference in average gestation of babies of
#     mothers that did and did not smoke cigarettes = 0
# HA: difference in average gestation of babies of
#     mothers that did and did not smoke cigarettes != 0

# distribution of mothers that smoked
table(data$smoker)

# before we run the analysis, let's look at our data
boxplot(data$Gestation~data$smoker)

# to test our hypothesis, we will perform a two-sample t-test
t.test(data$Gestation~data$smoker)

###################################################
# We hypothesize that average birthweight will ####
#   be different between babies of mothers that
#   smoked cigarettes and babies of mothers that
#   did not smoke cigarettes.
# H0: difference in average birthweight of babies of
#     mothers that did and did not smoke cigarettes = 0
# HA: difference in average birthweight of babies of
#     mothers that did and did not smoke cigarettes != 0

# before we run the analysis, let's look at our data
boxplot(data$Birthweight~data$smoker)

# to test our hypothesis, we will perform a two-sample t-test
t.test(data$Birthweight~data$smoker) # for reference: .37kg = .83lbs

###################################################
# We hypothesize that average birthweight will ####
# be associated with birth length
# H0: correlation between birthweight and length = 0
# HA: correlation between birthweight and length != 0

# look at the association: is it linear?
plot(data$Length, data$Birthweight, 
     xlab="length",ylab="weight")

# test the correlation
cor.test(data$Length,data$Birthweight)

###################################################
# We hypothesize that average birth length will ####
# predict birthweight
# H0: beta_length = 0
# HA: beta_length != 0

# perform simple (univariate) linear regression
fit.length.weight = lm(Birthweight~Length, data=data)
summary(fit.length.weight)

###################################################
# We hypothesize that average birth length and maternal smoking will ####
# predict birthweight
# H0: beta_length = 0
# HA: beta_length != 0
# H0: beta_smoking = 0
# HA: beta_smoking != 0


# perform multiple linear regression
fit.length.smoker.weight = lm(Birthweight~Length + smoker, data=data)
summary(fit.length.smoker.weight)
