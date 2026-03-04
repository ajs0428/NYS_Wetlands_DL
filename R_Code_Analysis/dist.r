library(lme4)
library(lmerTest)
library(dplyr)

ch4 <- readxl::read_xlsx("FieldData/CH4_flux_combined_2023_2024.xlsx", sheet = 1)
veg <- readxl::read_xlsx("FieldData/veg_cover_combined_2023_2024_only_1m_x_1m.xlsx", sheet = 1)

dat <- ch4 |> dplyr::left_join(y = veg, by = join_by(Unique_ID)) |> 
    mutate(across(where(is.character), ~ as.factor(.x)),
           Graminoids = case_when(Graminoids == ">75" ~ 100,
                                  Graminoids == "51-75" ~ 75,
                                  Graminoids == "26-50" ~ 50,
                                  Graminoids == "6-25" ~ 25,
                                  Graminoids == "1-5" ~ 5,
                                  Graminoids == "0" ~ 0,
                                  .default = 0))

hist(dat$CH4_mmol_hr_m2)
plot(dat$CH4_mmol_hr_m2 ~ dat$Graminoids)

mod <- lmer(CH4_mmol_hr_m2**(1/11) ~ Graminoids*Ecoregion + (1|Site_ID), data = dat, REML = FALSE)
summary(mod)
plot(resid(mod) ~ fitted(mod))
qqnorm(dat$CH4_mmol_hr_m2**(1/11))
qqline(dat$CH4_mmol_hr_m2**(1/11))
