# distribution of income
hist(gss$coninc)
hist(table(gss$coninc))

# association between education (ind. variable) and income (dep. variable)
plot(gss$coninc ~ gss$educ)
plot(gss$coninc ~ gss$degree)

#there is definitely a positive relationship
inc_s <- ggplot(gss, aes(educ, coninc))
inc_s + geom_smooth()
i <- ggplot(gss, aes(educ, coninc, color = degree))
i + geom_point()
i + geom_boxplot()

#religion and income
ri <- ggplot(gss, aes(relig, coninc))
ri + geom_boxplot()

# relationship between degree and gun ownership -----
deg_gun <-subset(gss, select = c(degree,owngun))
p_deg_gun <- table(deg_gun) / sum(table(deg_gun$degree))
plot(deg_gun)
plot(gss$degree ~ gss$owngun)
plot(gss2$degree ~ gss2$owngun)
deg_gun <- ggplot(gss)
deg_gun + geom_mosaic(aes(x = product(owngun), fill=degree))

inference(y = owngun, x = degree, data = gss2, statistic = "proportion", success = "Yes",
          type = "ht", null = 0, alternative = "greater", method = "theoretical")

# income and gun ownership 
plot(gss$owngun ~ gss$coninc, xlab = "Income", ylab= "Gun Ownership")
plot(gss$coninc ~ gss$owngun, xlab = "Gun Ownership", ylab="Income")

#are richer people more prone towards guns?
gincome <- subset(gss, owngun =="Yes")$coninc
ngincome <- subset(gss, owngun == "No")$coninc
inference(y = owngun, x = coninc, data = gss2, statistic = "proportion", success = "Yes",
          type = "ht",alternative = "greater", method = "theoretical")

#is there an upward trend in tv hours ?
ggplot(gss, aes(year, tvhours)) +
geom_smooth()

# income trend

ggplot(gss, aes(year, coninc)) +
geom_smooth()
