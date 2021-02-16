#We are interested in the possibile association beween gun ownership and income.
gincome <- subset(gss, owngun =="Yes")$coninc
ngincome <- subset(gss, owngun == "No")$coninc
summary(gincome)
hist(gincome)
summary(ngincome)
hist(ngincome)
#The mean income of gun owners is `r `
plot(gss$coninc ~ gss$owngun)
plot(gss$owngun ~ gss$coninc)

#we see a clear difference in the mean income of the two groups.Do gun owners have a 
# mean higher income than people who do not own guns?

# Conditions for inference :

# We check the two conditions necessary to apply the t-distribution to the
# difference in sample means. (1) Because the data come from a simple random
# sample and consist of less than 10% of all such cases, the observations are
# independent. Additionally, while each distribution is strongly skewed, the large
# sample sizes are enough compensation to model each mean
# separately using a t-distribution.  (2) The independence reasoning applied in (1) also
# ensures the observations in each sample are independent. Since both conditions
# are satisfied, the difference in sample means may be modeled using a
# t-distribution.”

gss1 <- subset(gss, owngun != "Refused")
gss2 <- droplevels(gss1)

inference(y = coninc, x = owngun, data = gss2, statistic = "mean", 
          type = "ht", null = 0, alternative = "twosided", method = "theoretical")

inference(y = owngun, x= income06, data = gss2, statistic = "proportion", success = "Yes",
          type = "ht",alternative = "greater", method = "theoretical")
# There is definitely a relationship between income and gun ownership.

inference(y = coninc, x = owngun, data = gss2, statistic = "mean", 
          type = "ci", alternative = "twosided", method = "theoretical")
# We are 95% confident that the annual income of guns' owners is on average 
# 8262.1757 to 9839.8451 dollars higher than the income of people who do not own guns.


