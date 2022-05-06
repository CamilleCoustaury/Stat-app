
# ---------- Pour SSS ---------------------------
# Age + Wave
fd_SSS_age <-plm(sclddr ~ age + wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
                           data=df_kept,
                          index=c("idauniq", "wave"),
                           method = "within")
summary(fd_SSS_age)

# Age + Wave + Wealth
fd_SSS_age_wealth <-plm(sclddr ~ age + wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9 +
                          ihs_wealth_inflation,
                 data=df_kept,
                 index=c("idauniq", "wave"),
                 method = "within")
summary(fd_SSS_age_wealth)

# Age + Wave + Income
fd_SSS_age_income <-plm(sclddr ~ age + wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9 +
                          log_income_inflation,
                        data=df_kept,
                        index=c("idauniq", "wave"),
                        method = "within")
summary(fd_SSS_age_income)


# ---------- Pour SRH ---------------------------
# Age + Wave
fd_SRH_age <-plm(srh_hrs ~ age + wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9,
                        data=df_kept,
                        index=c("idauniq", "wave"),
                        method = "within")
summary(fd_SRH_age)

# Age + Wave + Wealth
fd_SRH_age_wealth <-plm(srh_hrs ~ age + wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9 +
                          ihs_wealth_inflation,
                        data=df_kept,
                        index=c("idauniq", "wave"),
                        method = "within")
summary(fd_SRH_age_wealth)

# Age + Wave + Income
fd_SRH_age_income <-plm(srh_hrs ~ age + wave2 + wave4 + wave5 + wave6 + wave7 + wave8 + wave9 +
                          log_income_inflation,
                        data=df_kept,
                        index=c("idauniq", "wave"),
                        method = "within")
summary(fd_SRH_age_income)
