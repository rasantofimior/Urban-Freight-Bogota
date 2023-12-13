     
#########################################################
# Trabajo final economia urbana 2023-2                  #
# autor: Rafael Andres Santofimio Rivera                #
# cod. 201924300                                        #
# Uniandes                                              #
#########################################################

require(pacman)

p_load(
  DeclareDesign,
  fabricatr,
  tidyverse,
)

library("DIDmultiplegt") # Load the package into memory (required each new session)
library(rdss) # for helper functions

# Semilla para que los resultados sean replicables
set.seed(10101) 

#Clean the workspace
#rm(list=ls())

# Datos de entrada
N_units <- 45705
N_time_periods <- 2
N_sample <- 15000 

# Diseño del modelo (M)
model <-
declare_model(
  units = add_level(
    N = N_units, 
    U_unit = rnorm(N), 
    D_unit = if_else(U_unit > median(U_unit), 1, 0),
    D_time = 1
  ),
  periods = add_level(
    N = N_time_periods,
    U_time = rnorm(N),
    nest = FALSE
  ),
  unit_period = cross_levels(
    by = join_using(units, periods), 
    U = rnorm(N),
    DC = runif(N, min = 0, max = 1200),
    D_FTA = runif(N, min = 0, max = 1200),
    T_Ind = sample(c(1, 2, 3, 4, 5), N, replace = TRUE),
    Est = sample(c(1, 2, 3, 4, 5, 6), N, replace = TRUE),
    D_CBD = runif(N, min = 0, max = 17000),
    Age = sample(0:120, N, replace = TRUE),
    Area = runif(N, min = 20, max = 5000),
    Y0 = log(runif(N, min = 500000, max = 20000000)), # Potential outcome under control
    Y1 = Y0 + log(abs(rnorm(N, mean = 2))),
    potential_outcomes(Y ~ U + U_unit + U_time + DC + D_FTA + T_Ind + Est + D_CBD +
                         Age + Area + Y0 + D * (Y1 - Y0) + (as.numeric(periods)) * (Y1 - Y0) * 2 + D * (0.58* (D_time - as.numeric(periods))), 
                       conditions = list(D = c(0, 1))),
    D = if_else(D_unit == 1 & as.numeric(periods) >= D_time, 1, 0)
  )
)


#Mx1 <- model()

# Indagacion (I)
inquiry <-   declare_inquiry(
  ATT = mean(Y_D_1 - Y_D_0), 
  subset = D == 1
)

# Estrategia de datos (D)
sampling <- declare_sampling(
  S = draw_rs(N = 91410, n = N_sample)
)

#assignment <-  declare_assignment(D=complete_ra(N,prob=0.5))

measurement <-declare_measurement(Y = reveal_outcomes(Y ~ D)) 

# Estrategia de respuesta (A)
answer_strategy <-declare_estimator(
    Y ~ D + U + U_unit + U_time + DC + D_FTA + T_Ind + Est + D_CBD +
      Age + Area + Y0,
    .method = lm_robust,
    .summary = tidy,
    term = "D",
    inquiry = c("ATT"),
    label = "OLS"
  )

#+ declare_estimator(Y ~ D,    .summary = tidy, term = "D", inquiry = "ATT", label = "DIM") 

# Combina todos los componentes del diseño
multi_arm_design <-
  model +
  inquiry +
  sampling +
#  assignment +
  measurement +
  answer_strategy

# Diagnostico
diagnosis <-
  diagnose_design(multi_arm_design,
                  sims = 30,
                  bootstrap_sims = 100)
reshape_diagnosis(diagnosis)

#draw_estimates(multi_arm_design)

simulations_df <- get_simulations(diagnosis)

simulations_df |> 
  group_by(design, inquiry, estimator, term, outcome) |>
  summarize(
    bias = mean(estimate - estimand),
    rmse = sqrt(mean((estimate - estimand)^2)),
    power = mean(p.value <= 0.1),
    coverage = mean(estimand <= conf.high & estimand >= conf.low)
  )


multiple_desing<-redesign(multi_arm_design,att=c(0.2, 0.4,0.6,0.8),
                          sample_size=c(2000,5000,10000,15000))

diagnose_design(multiple_desing,sims=100)



################

# first create summary for vertical lines
summary_df <- 
  simulations_df |> 
  group_by(estimator) |> 
  summarize(estimand = mean(estimand))

# then plot simulations
ggplot(simulations_df) +
  geom_histogram(aes(estimate),
                 bins = 50, fill = "#72B4F3") +
  geom_vline(data = summary_df,
             aes(xintercept = estimand),
             lty = "dashed", color = "#C6227F") +
  annotate("text", y = 100, x = 0, label = "Estimand",
           color = "#C6227F", hjust = 0.1) +
  facet_wrap(~ estimator) +
  labs(x = "Estimate", y = "Count of simulations") +
  theme_minimal()

## power curve

simulations_df <- 
  diagnose_design(multi_arm_design) |> 
  get_simulations() |> 
  mutate(significant = if_else(p.value <= 0.05, 1, 0))

ggplot(simulations_df) + 
  stat_smooth(aes(estimand, significant), method = 'loess', color = "#3564ED", fill = "#72B4F3", formula = 'y ~ x') +
  geom_hline(yintercept = 0.8, color = "#C6227F", linetype = "dashed") +
  annotate("text", x = 0, y = 0.85, label = "Conventional power threshold = 0.8", hjust = 0, color = "#C6227F") + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "none") +
  labs(x = "Model parameter: true effect size",
       y = "Diagnosand: statistical power") +
  theme_minimal()