
load('results/100_PreprocessedData.Rdata')

x_ax_lim = c(0, max(preprocessed_data$DurInMonths))
breaks = 12
x_label = 'Time in Months'


### 

surv_general <- survfit(Surv(time = preprocessed_data$DurInMonths,
                             event = preprocessed_data$Terminated),
                        data = preprocessed_data)

ggsurvplot(surv_general, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           conf.int = TRUE,
           # risk.table = TRUE,
           # cumcensor = TRUE
           ) %>%
  ggsave(filename = '/results/200_SurvGeneral.png')


#### Survival by Sex ---------------------------------------------------------------------------------------------------

surv_sex <- survfit(Surv(time = preprocessed_data$DurInMonths,
                         event = preprocessed_data$Terminated) ~ Sex,
                    data = preprocessed_data)

ggsurvplot(surv_sex, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)



#### Survilas by Shareholder -------------------------------------------------------------------------------------------

surv_shareholder <- survfit(Surv(time = preprocessed_data$DurInMonths,
                                 event = preprocessed_data$Terminated) ~ ShareholderInOtherCompanies,
                            data = preprocessed_data)

ggsurvplot(surv_shareholder, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)


#### Survival by Licences ----------------------------------------------------------------------------------------------

surv_licences <- survfit(Surv(time = preprocessed_data$DurInMonths,
                              event = preprocessed_data$Terminated) ~ HasLicences,
                         data = preprocessed_data)

ggsurvplot(surv_licences, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)
#### Survival by HasPolishCitizenship ----------------------------------------------------------------------------------

surv_polish_citizen <- survfit(Surv(time = preprocessed_data$DurInMonths,
                              event = preprocessed_data$Terminated) ~ HasPolishCitizenship,
                         data = preprocessed_data)


ggsurvplot(surv_polish_citizen, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)


#### Survival by IsPhoneNo ----------------------------------------------------------------------------------

surv_is_phone <- survfit(Surv(time = preprocessed_data$DurInMonths,
                                    event = preprocessed_data$Terminated) ~ IsPhoneNo,
                               data = preprocessed_data)


ggsurvplot(surv_is_phone, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)


#### Survival by IsWWW ----------------------------------------------------------------------------------

surv_is_www <- survfit(Surv(time = preprocessed_data$DurInMonths,
                              event = preprocessed_data$Terminated) ~ IsWWW,
                         data = preprocessed_data)


ggsurvplot(surv_is_www, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)


#### Survival by IsWWW ----------------------------------------------------------------------------------

surv_is_email <- survfit(Surv(time = preprocessed_data$DurInMonths,
                            event = preprocessed_data$Terminated) ~ IsEmail,
                       data = preprocessed_data)


ggsurvplot(surv_is_email, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)


#### Survival by CommunityProperty -------------------------------------------------------------------------------------

surv_comm_proper <- survfit(Surv(time = preprocessed_data$DurInMonths,
                              event = preprocessed_data$Terminated) ~ CommunityProperty,
                         data = preprocessed_data)


ggsurvplot(surv_comm_proper, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)


#### Survilas by Voivodeship -------------------------------------------------------------------------------------------

surv_voiv <- Surv(time = preprocessed_data$DurInMonths, event = preprocessed_data$Terminated)

voiv <- preprocessed_data$MainAddressVoivodeship

foivFM <- mergeFactors(response = surv_voiv, factor = voiv, family = 'survival')

ggsurvplot(surv_voiv, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           linetype = 'dashed',
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)


#### Survilas by MAinAndCorrespondence ---------------------------------------------------------------------------------------------------


surv_main_corres <- survfit(Surv(time = preprocessed_data$DurInMonths,
                                 event = preprocessed_data$Terminated) ~ MainAndCorrespondenceAreTheSame,
                            data = preprocessed_data)

ggsurvplot(surv_main_corres, 
           data = preprocessed_data, 
           break.time.by = breaks, 
           ggtheme = theme_bw(),
           xlim = x_ax_lim,
           xlab = x_label,
           pval = TRUE,
           risk.table = TRUE)



