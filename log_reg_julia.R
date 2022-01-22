#### Logistic regression ####

# Notes:
# Use link = log in the binomial reg to get the ARRs, as per this
# https://www.r-bloggers.com/2020/08/simulations-comparing-interaction-for-adjusted-risk-ratios-versus-adjusted-odds-ratios/

# Model example:
# r1=glm(disease~rf1+rf2+grp+rf1*rf2+rf1*grp+rf2*grp,
#        + family=binomial(link=log))

# ARRs example:
# x1=exp(confint.default(r1))
# rr=exp(coef(r1))
# round(cbind(rr,x1),digits=2)