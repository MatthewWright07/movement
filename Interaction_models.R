
library(effects)
library(effectsize)
library(ggeffects)
library(ggpubr)

#---- dragon score and relative strength ----

# does age correlate with predictor or outcome variable?

cor.test(data$age_exact_years, data$dragon_score)
cor.test(data$age_exact_years, data$relative_strength_imtp)


# in this case no - so don't include age. 

# basic correlation 

cor.test(data$dragon_score, data$relative_strength_imtp)
t.test(data$relative_strength_imtp ~ data$sex)
t.test(data$dragon_score ~ data$sex)
t.test(data$skill_score ~ data$sex)
t.test(data$time ~ data$sex)
t.test(data$time_score ~ data$sex)
t.test(data$product_score ~ data$sex)
t.test(data$process_score ~ data$sex)
t.test(data$Maturity_offset ~ data$sex)

# base linear model
lm_base <- lm(dragon_score ~ relative_strength_imtp, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(dragon_score ~ relative_strength_imtp + sex, data)
lm_interaction <- lm(dragon_score ~ relative_strength_imtp * sex, data)

summary(lm_interaction)
confint(lm_interaction)

compare_performance(lm_base, lm_addsex, lm_interaction, rank = TRUE)
anova(lm_addsex, lm_interaction)

plot(allEffects(lm_interaction))

eff <- ggpredict(
  lm_interaction,
  terms = c("relative_strength_imtp", "sex")
)

ds_rel <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "IMTP Relative Peak Force (N/kg)",
    y = "Dragon Challenge Score (AU)",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

ds_rel


#---- dragon score and absolute strength ----

# does age correlate with predictor or outcome variable?

cor.test(data$age_exact_years, data$dragon_score)
cor.test(data$age_exact_years, data$peak_force_imtp)

cor.test(data$mass, data$dragon_score)
cor.test(data$mass, data$peak_force_imtp)

# in this case no - so don't include age. 

# basic correlation 

cor.test(data$dragon_score, data$peak_force_imtp)

# base linear model
lm_base <- lm(dragon_score ~ peak_force_imtp, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(dragon_score ~ peak_force_imtp + sex, data)
lm_interaction <- lm(dragon_score ~ peak_force_imtp * sex, data)

summary(lm_interaction)
confint(lm_interaction)

lm_int_age <- lm(dragon_score ~ peak_force_imtp * sex + age_exact_years, data)
summary(lm_int_age)
anova(lm_interaction, lm_int_age,lm_base)


standardize_parameters(lm_interaction)
effectsize(lm_interaction)


compare_performance(lm_base, lm_addsex,lm_interaction, lm_int_age, rank = TRUE)
anova(lm_addsex, lm_interaction)

plot(allEffects(lm_interaction))



eff <- ggpredict(
  lm_interaction,
  terms = c("peak_force_imtp", "sex")
)

ds_abs <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "IMTP Peak Force (N)",
    y = "",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()



#---- dragon score and power ----

# does age correlate with predictor or outcome variable?

cor.test(data$age_exact_years, data$dragon_score)
cor.test(data$age_exact_years, data$jump_height_cmj)

# in this case no - so don't include age. 

# basic correlation 

cor.test(data$dragon_score, data$jump_height_cmj)

# base linear model
lm_base <- lm(dragon_score ~jump_height_cmj, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(dragon_score ~ jump_height_cmj + sex, data)
lm_interaction <- lm(dragon_score ~ jump_height_cmj * sex, data)

summary(lm_interaction)
confint(lm_interaction)


standardize_parameters(lm_interaction)
effectsize(lm_interaction)


compare_performance(lm_base, lm_addsex,lm_interaction, rank = TRUE)
anova(lm_addsex, lm_interaction)

plot(allEffects(lm_interaction))

eff <- ggpredict(
  lm_addsex,
  terms = c("jump_height_cmj", "sex")
)

ds_jh <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "Jump height (cm)",
    y = "",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

ds_jh

ggarrange(ds_rel, ds_abs, ds_jh, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("A", "B", "C"))



#---- dragon time and relative strength ----

# does age correlate with predictor or outcome variable?

cor.test(data$age_exact_years, data$time)
cor.test(data$age_exact_years, data$relative_strength_imtp)

cor.test(data$Maturity_offset, data$time)
cor.test(data$Maturity_offset, data$relative_strength_imtp)

# in this case no - so don't include age BUT Maturity offset does! 

# basic correlation 

cor.test(data$time, data$relative_strength_imtp)

# base linear model
lm_base <- lm(time ~ relative_strength_imtp, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(time ~ relative_strength_imtp + sex, data)
lm_interaction <- lm(time ~ relative_strength_imtp * sex, data)

summary(lm_interaction)
confint(lm_interaction)

compare_performance(lm_base, lm_addsex, lm_interaction, rank = TRUE)
anova(lm_addsex, lm_interaction)

plot(allEffects(lm_interaction))
plot(allEffects(lm_addsex))


lm_int_age <- lm(time ~ relative_strength_imtp + sex + Maturity_offset, data)
summary(lm_int_age)

anova(lm_base, lm_addsex, lm_interaction, lm_int_age)
anova(lm_addsex , lm_int_age)
compare_performance(lm_base, lm_addsex, lm_interaction, lm_int_age,  rank = TRUE)


eff <- ggpredict(
  lm_addsex,
  terms = c("relative_strength_imtp", "sex")
)

dt_rel <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "IMTP Relative Peak Force (N/kg)",
    y = "Dragon Challenge Time (s)",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

dt_rel

#---- dragon time and absolute strength ----

# does age correlate with predictor or outcome variable?

cor.test(data$age_exact_years, data$time)
cor.test(data$age_exact_years, data$peak_force_imtp)

# in this case no - so don't include age. 

# basic correlation 

cor.test(data$time, data$peak_force_imtp)

# base linear model
lm_base <- lm(time ~ peak_force_imtp, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(time ~ peak_force_imtp + sex, data)
lm_interaction <- lm(time ~ peak_force_imtp * sex, data)

summary(lm_addsex)
confint(lm_addsex)


standardize_parameters(lm_interaction)
effectsize(lm_interaction)


compare_performance(lm_base, lm_addsex,lm_interaction,  rank = TRUE)
anova(lm_addsex, lm_interaction)

plot(allEffects(lm_interaction))



eff <- ggpredict(
  lm_addsex,
  terms = c("peak_force_imtp", "sex")
)

dt_abs <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "IMTP Peak Force (N)",
    y = "Dragon Challenge Time (s)",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

dt_abs


#---- dragon time and power ----



# in this case no - so don't include age. 

# basic correlation 

cor.test(data$time, data$jump_height_cmj)

# base linear model
lm_base <- lm(time ~jump_height_cmj, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(time ~ jump_height_cmj + sex, data)
lm_interaction <- lm(time ~ jump_height_cmj * sex, data)

compare_performance(lm_base, lm_addsex,lm_interaction, rank = TRUE)
anova(lm_addsex, lm_interaction)

summary(lm_addsex)
confint(lm_addsex)


standardize_parameters(lm_interaction)
effectsize(lm_interaction)




plot(allEffects(lm_addsex))

eff <- ggpredict(
  lm_interaction,
  terms = c("jump_height_cmj", "sex")
)

dt_jh <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "Jump height (cm)",
    y = "Dragon Challenge Time (s)",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

dt_jh

ggarrange(dt_rel, dt_abs, dt_jh, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")

#---- dragon skill score and relative strength ----

# does age correlate with predictor or outcome variable?

cor.test(data$age_exact_years, data$skill_score)
cor.test(data$age_exact_years, data$relative_strength_imtp)

# in this case no - so don't include age. 

# basic correlation 

cor.test(data$skill_score, data$relative_strength_imtp)

# base linear model
lm_base <- lm(skill_score ~ relative_strength_imtp, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(skill_score ~ relative_strength_imtp + sex, data)
lm_interaction <- lm(skill_score ~ relative_strength_imtp * sex, data)

compare_performance(lm_base, lm_addsex, lm_interaction, rank = TRUE)
anova(lm_addsex, lm_interaction)


summary(lm_interaction)
confint(lm_interaction)


plot(allEffects(lm_interaction))

eff <- ggpredict(
  lm_interaction,
  terms = c("relative_strength_imtp", "sex")
)

ss_rel <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "IMTP Relative Peak Force (N/kg)",
    y = "Dragon Challenge Skill Score (AU)",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

ss_rel


#---- dragon skill score and absolute strength ----



# in this case no - so don't include age. 

# basic correlation 

cor.test(data$skill_score, data$peak_force_imtp)

# base linear model
lm_base <- lm(skill_score ~ peak_force_imtp, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(skill_score ~ peak_force_imtp + sex, data)
lm_interaction <- lm(skill_score ~ peak_force_imtp * sex, data)

compare_performance(lm_base, lm_addsex,lm_interaction,  rank = TRUE)
anova(lm_addsex, lm_interaction)

summary(lm_interaction)
confint(lm_interaction)


standardize_parameters(lm_interaction)
effectsize(lm_interaction)


plot(allEffects(lm_interaction))



eff <- ggpredict(
  lm_interaction,
  terms = c("peak_force_imtp", "sex")
)

ss_abs <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "IMTP Peak Force (N)",
    y = "Dragon Challenge Skill Score (AU)",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

ss_abs 

#---- dragon score and power ----

# does age correlate with predictor or outcome variable?

cor.test(data$age_exact_years, data$dragon_score)
cor.test(data$age_exact_years, data$jump_height_cmj)

# in this case no - so don't include age. 

# basic correlation 

cor.test(data$dragon_score, data$jump_height_cmj)

# base linear model
lm_base <- lm(dragon_score ~jump_height_cmj, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(dragon_score ~ jump_height_cmj + sex, data)
lm_interaction <- lm(dragon_score ~ jump_height_cmj * sex, data)

summary(lm_interaction)
confint(lm_interaction)


standardize_parameters(lm_interaction)
effectsize(lm_interaction)


compare_performance(lm_base, lm_addsex,lm_interaction, rank = TRUE)
anova(lm_base, lm_addsex, lm_interaction)

plot(allEffects(lm_interaction))

eff <- ggpredict(
  lm_interaction,
  terms = c("jump_height_cmj", "sex")
)

ds_jh <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "Jump height (cm)",
    y = "Dragon Challenge Score",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

ds_jh

ggarrange(ds_rel, ds_abs, ds_jh, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")


#---- dragon skill score and power ----

# does age correlate with predictor or outcome variable?


# in this case no - so don't include age. 

# basic correlation 

cor.test(data$skill_score, data$jump_height_cmj)

# base linear model
lm_base <- lm(skill_score ~jump_height_cmj, data)
summary(lm_base)

# consider co variate

lm_addsex <- lm(skill_score ~ jump_height_cmj + sex, data)
lm_interaction <- lm(skill_score ~ jump_height_cmj * sex, data)


compare_performance(lm_base, lm_addsex,lm_interaction, rank = TRUE)
anova(lm_base, lm_addsex, lm_interaction)

summary(lm_interaction)
confint(lm_interaction)

summary(lm_addsex)
confint(lm_addsex)

standardize_parameters(lm_addsex)
effectsize(lm_interaction)


plot(allEffects(lm_addsex))

eff <- ggpredict(
  lm_addsex,
  terms = c("jump_height_cmj", "sex")
)

ss_jh <- ggplot(eff, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  labs(
    x = "Jump height (cm)",
    y = "Dragon Skill Score (AU)",
    colour = "Sex",
    fill = "Sex"
  ) +
  theme_classic()

ss_jh

ggarrange(ss_rel, ss_abs, ss_jh, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("A", "B", "C"))

cor.test(data$skill_score, data$mass)
lm_base <- lm(skill_score ~ mass , data)

summary(lm_base)

# consider co variate

lm_addsex <- lm(skill_score ~ mass  + sex, data)
lm_interaction <- lm(skill_score ~ mass  * sex, data)


compare_performance(lm_base, lm_addsex,lm_interaction, rank = TRUE)
anova(lm_base, lm_addsex, lm_interaction)

summary(lm_interaction)
confint(lm_interaction)

plot(allEffects(lm_interaction))


