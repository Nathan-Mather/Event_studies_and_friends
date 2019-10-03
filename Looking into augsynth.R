
library(augsynth)
library(broom)

browseVignettes("augsynth")

lsf.str("package:augsynth")


library(magrittr)
library(dplyr)
library(Synth)
data(basque)


basque %>%
  mutate(
    treatment=case_when(year < 1975 ~ 0,
                        regionno != 17 ~ 0,
                        regionno == 17 ~ 1) # Basque after 1975 is treated
  ) %>%
  filter(regionno != 1) -> basque

syn <- augsynth(gdpcap ~ treatment, regionno, year, 1975, basque,
                progfunc="None", weightfunc="SCM")

summary(syn)
plot(syn)
tidy(syn)

#================================#
# ==== augsynth line by line ====
#================================#


call_name <- match.call()
form <- Formula::Formula(form)
unit <- enquo(unit)
time <- enquo(time)
outcome <- terms(formula(form, rhs = 1))[[2]]
trt <- terms(formula(form, rhs = 1))[[3]]
wide <- format_data(outcome, trt, unit, time, t_int, data)
synth_data <- do.call(format_synth, wide)
if (length(form)[2] == 2) {
  cov_agg <- c(function(x) mean(x, na.rm = T))
  cov_form <- update(formula(delete.response(terms(form, 
                                                   rhs = 2, data = data))), ~. - 1)
  pre_data <- data %>% filter(!!(time) < t_int)
  Z <- model.matrix(cov_form, model.frame(cov_form, pre_data, 
                                          na.action = NULL)) %>% data.frame() %>% mutate(unit = pull(pre_data, 
                                                                                                     !!unit)) %>% group_by(unit) %>% summarise_all(cov_agg) %>% 
    select(-unit) %>% as.matrix()
}
else {
  Z <- NULL
}
if (progfunc == "Ridge") {
  if (weightfunc == "SCM") {
    augsynth <- do.call(fit_ridgeaug_formatted, c(list(wide_data = wide, 
                                                       synth_data = synth_data, Z = Z), opts_out, opts_weights))
  }
  else if (weightfunc == "None") {
    augsynth <- do.call(fit_ridgeaug_formatted, c(list(wide_data = wide, 
                                                       synth_data = synth_data, Z = Z, ridge = T, scm = F), 
                                                  opts_out, opts_weights))
  }
}
else if (progfunc == "None") {
  augsynth <- do.call(fit_ridgeaug_formatted, c(list(wide_data = wide, 
                                                     synth_data = synth_data, Z = Z, ridge = F, scm = T), 
                                                opts_weights))
}
else {
  augsynth <- fit_augsyn(wide, synth_data, progfunc, weightfunc, 
                         opts_out, opts_weights)
}
augsynth$data <- wide
augsynth$data$time <- data %>% distinct(!!time) %>% pull(!!time)
augsynth$data$Z <- Z
augsynth$t_int <- t_int
augsynth$progfunc <- progfunc
augsynth$weightfunc <- weightfunc
augsynth$call <- call_name
class(augsynth) <- "augsynth"
return(augsynth)
}