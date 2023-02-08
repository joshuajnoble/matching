library(pacman)
library(causalweight)
library(data.table)
library(pacman)
library(optmatch)
library(MatchIt)
library(marginaleffects)
library(lmtest)
library(gt)

dat <- read.csv("data/reading_data.csv")


mean(dat[dat$reading_program == 1,]$scores) - mean(dat[dat$reading_program == 0,]$scores)

graph_sections(dat)

# glm formula: predict reading program with pre_k
predict_reading <- glm(reading_program ~ class_size + pre_k,family="binomial", data = dat)
summary(predict_reading)

unmatched <- lm(scores ~ class_size + reading_program + pre_k, data=dat)
summary(unmatched)

summary(lm(scores ~ class_size + pre_k, data = dat[reading_program == 0,]))
summary(lm(scores ~ class_size + pre_k, data = dat[reading_program == 1,]))

summary(matchit(reading_program ~ class_size + pre_k, data = dat, method = NULL, distance = "glm"))

match <- matchit(reading_program ~ class_size + pre_k, data = dat, method = "optimal", distance = "glm")
summary(match)

match_data = match.data(match)

lm_matched =lm(scores ~ class_size + reading_program + pre_k, data=match_data)
summary(lm_matched)

summary(comparisons(lm_matched, vcov = ~subclass, variables = c("reading_program", "pre_k")))
summary(comparisons(unmatched, variables = c("reading_program", "pre_k")))

p <- predictions(
  lm_matched,
  variables = list(reading_program = c(0,1)),
  newdata = datagrid(pre_k = unique, class_size = range))

p_dt <- data.table("predicted" = p$predicted, "error" = p$std.error, "reading_program" = p$reading_program, "pre-K" = p$pre_k, "class_size" = p$class_size)

tab_header(
  gt(p_dt),
  title = "Predicted Scores from Matched Data"
)

p <- predictions(
  unmatched,
  variables = list(reading_program = c(0,1)),
  newdata = datagrid(pre_k = unique, class_size = range))

p_dt <- data.table("predicted" = p$predicted, "error" = p$std.error, "reading_program" = p$reading_program, "pre-K" = p$pre_k, "class_size" = p$class_size)

tab_header(
  gt(p_dt),
  title = "Predicted Scores from Unmatched Data"
  )
