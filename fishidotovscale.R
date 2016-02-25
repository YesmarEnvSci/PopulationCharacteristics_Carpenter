############################
# otovsscale.R
# PopulationCharacteristics_Carpenter.Rproj

# Plots and compares scales and otolith ages

# Files sourced:
# Files produced:

# Created February 5, 2016
# A Putt
#############################

source("fishidimport.r")
head(ages)

# Run a linear model to add to the plot
m <- lm(ages$otoage ~ ages$scaleage)
a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
r2 <- signif(summary(m)$r.squared, digits=2)
textlab <- paste("y = ",b,"x + ",a,"; R2 = ",r2, sep="")
print(textlab)

windows()
ggplot(ages, aes(otoage,scaleage))+
  geom_point(position = position_jitter(w = 0.3, h = 0.3))+
  labs(x="Otolith Age", y="Scale Age") +
  xlim(0,7) + ylim(0,7) +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,color="black") +
  annotate("text", x = 1.5, y = 6.5, label = textlab, color="black", size = 5, parse=FALSE) +
  theme_bw()

# Check out some diagnostics
windows()
ggplot(m, aes(.fitted, .resid)) + 
geom_point() +
stat_smooth(method="loess") +
geom_hline(yintercept=0, col="red", linetype="dashed") +
labs(x= "Fitted values", y="Residuals") +
ggtitle("Residual vs Fitted Plot") + theme_bw()

windows()
resid(m)
qqnorm(resid(m)) # A quantile normal plot - good for checking normality
qqline(resid(m))

# Test whether the model is different from a 1:1 line
confint(m) # Is one included in the confidence interval? It is, but the intercepts differ.

nullm <- lm((ages$scaleage-ages$otoage)~0) # This model is y=1x+0
fullm <- lm((ages$scaleage-ages$otoage)~ages$otoage) # This model is y=ax+b...it just has to look like this to go into the anova
anova(nullm,fullm) # Null hypothesis is that the model fits are the same
# If p < 0.05, then the data do not support the alternative hyp that the coefficients are not both zero
# (i.e. effectively the models differ)

# Test for the correlation coefficient
cor.test(ages$otoage,ages$scaleage,alternative="two.sided",method="pearson")


