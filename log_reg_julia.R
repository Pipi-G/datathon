#### Logistic regression ####

# Notes:
# Use link = log in the binomial reg to get the ARRs, as per here
# https://www.r-bloggers.com/2020/08/simulations-comparing-interaction-for-adjusted-risk-ratios-versus-adjusted-odds-ratios/

# Model example:
# r1=glm(disease~rf1+rf2+grp+rf1*rf2+rf1*grp+rf2*grp,
#        + family=binomial(link=log))

# ARRs example:
# x1=exp(confint.default(r1))
# rr=exp(coef(r1))
# round(cbind(rr,x1),digits=2)

library(tidyverse)
library(lmerTest)
library(sjPlot)

### Load and clean data ---------------------------------------------------------------------------------------------------------------------------------------
PPH <- read_csv("PPH.csv")

predictor_vars <- c("year",
                    "race",
                    "zipinc_qrtl",
                    "pay1",
                    "hosp_locteach",
                    "age",
                    "hosp_region"
                    # "hosp_nis" # Tried using as a random intercept in a mixed model, but got a weird error
)

cat_vars <- c("year",
              "race",
              "zipinc_qrtl",
              "pay1",
              "hosp_locteach",
              # "age",
              "hosp_region",
              "hosp_nis" # Tried using as a random intercept in a mixed model, but got a weird error
              )

PPH <- PPH %>%
  mutate_at(cat_vars, as.factor)



### Models ----------------------------------------------------------------------------------------------------------------------------
# This function creates a table with all the unadjusted RRs
# It runs the univariate model and returns the uRRs + confidence intervals
get_uRRs <- function(predictor) {
  fml <- reformulate(predictor, "SMM")
  model <- glm(fml, 
               data = PPH, 
               family = "binomial"(link=log))
  summary(model)
  confint <- exp(confint.default(model))
  RR <- exp(coef(model))
  df <- round(cbind(RR, confint), digits=2) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(lower_ci = `2.5 %`,
           upper_ci = `97.5 %`)
  return(df)
}

# Apply function to each predictor individually
uRRs <- lapply(predictor_vars, function(x) get_uRRs(x)) %>%
  bind_rows()

# Get adjusted RRs
adj_fml <- reformulate(predictor_vars, "SMM")
model <- glm(adj_fml,
             data = PPH, 
             family = "binomial"(link=log))
summary(model)
confint <- exp(confint.default(model))
aRR <- exp(coef(model))
aRRs <- round(cbind(aRR, confint), digits=2) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(a_lower_ci = `2.5 %`,
         a_upper_ci = `97.5 %`)

# Join a and u
all_RRs <- full_join(uRRs, aRRs) %>%
  # Remove aRRs for intercept rows
  mutate(aRR = ifelse(rowname == "(Intercept)", NA, aRR),
         a_lower_ci = ifelse(rowname == "(Intercept)", NA, a_lower_ci),
         a_upper_ci = ifelse(rowname == "(Intercept)", NA, a_upper_ci))

write_csv(all_RRs, "all_RRs.csv")

