library(tidyverse)
library(survival)

# Load data
data(pbc)

# Create endpoints and follow-up times
pbc <- pbc %>%
  mutate(
    event = as.numeric(status > 0),               # 0 = censor, 1 = transplant, 2 = death
    futime = time / 365.25,                       # Follow-up time in years
    event6 = if_else(futime > 6, 0, event),       # Events for 6 years of follow-up
    futime6 = if_else(futime > 6, 6, futime),     # Follow-up time capped at 6 years
    event5 = if_else(futime > 5, 0, event),       # Events for 5 years of follow-up
    futime5 = if_else(futime > 5, 5, futime),     # Follow-up time capped at 5 years
    log.bili = log(bili),                         # Log-transformed variables
    log.albumin = log(albumin),
    log.protime = log(protime),
    build = if_else(id <= 312, "Build", "Test")   # Data partitioning
  ) %>%
  filter(!is.na(bili + albumin + protime + edema + age)) # Remove subjects with missing data

# Split data into Build and Test sets
pbc_build <- pbc %>% filter(build == "Build")
pbc_test <- pbc %>% filter(build == "Test")

# Fit Cox proportional hazards model
coxfit6 <- coxph(
  Surv(futime6, event6) ~ log.bili + log.albumin + age + log.protime + edema,
  data = pbc_build
)

# Get 5-year survival predictions for Build and Test data
pred5_build <- 1 - as.numeric(summary(survfit(coxfit6, newdata = pbc_build), times = 5)$surv)
pred5_test <- 1 - as.numeric(summary(survfit(coxfit6, newdata = pbc_test), times = 5)$surv)

# Calibration plot
tibble(
  pred = pred5_build,
  event = pbc_build$event6
) %>%
  ggplot(aes(pred, event)) +
  geom_rug(sides = "tb", color = "grey") +
  geom_smooth(method = "loess", color = "black", se = TRUE, fullrange = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "longdash") +
  labs(x = "Predicted risk",
       y = "Observed proportion") +
  theme_bw() +
  coord_equal(xlim = c(0,1), ylim = c(0,1)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) 



# Additional steps: Subset and transform data for time-dependent Cox model
temp <- pbc %>%
  filter(id <= 312) %>%
  select(id:sex, stage, event = event6, futime = futime6) # Baseline data

pbcseq_transformed <- pbcseq %>%
  mutate(
    day = day / 365.25,
    log.bili = log(bili),                         # Log-transformed variables
    log.albumin = log(albumin),
    log.protime = log(protime)
  )

# Create a time-dependent dataset
pbc2 <- tmerge(
  temp, temp, id = id,
  death = event(futime, event)
) %>%
  tmerge(
    data2 = pbcseq_transformed, id = id,
    edema = tdc(day, edema),
    log.bili = tdc(day, log.bili),
    log.albumin = tdc(day, log.albumin),
    log.protime = tdc(day, log.protime),
    alk.phos = tdc(day, alk.phos)
  )

# Fit Cox models
tv_cox <- coxph(Surv(tstart, tstop, death) ~ log.bili + log.albumin + age + log.protime + edema, 
              data = pbc2)

# Summarize Cox models
summary(tv_cox)

pred5_tv <- 1 - as.numeric(summary(survfit(tv_cox, newdata = pbc2), times = 5)$surv)

tibble(
  id = pbc2$id,
  pred = pred5_tv,
  event = pbc2$event
) %>%
  ggplot(aes(pred, event)) +
  geom_rug(sides = "tb", color = "grey") +
  geom_smooth(method = "loess", color = "black", se = TRUE, fullrange = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "longdash") +
  labs(x = "Predicted risk",
       y = "Observed proportion") +
  theme_bw() +
  coord_equal(xlim = c(0,1), ylim = c(0,1)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) 

