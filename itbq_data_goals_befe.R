# stack 2017 itbq data
# for import into SharePoint to allow facilities to select own improvement goals
# for BE and FE


# LOAD PACKAGES ###############################################################

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(scales)


# LOAD FUNCTIONS ##############################################################


# CREATE A FUNCTION so can use for dmg, whse, and mmo -------------------------
# !!! need lower_sres_cutoff from approx line 750

plot_resids_fn <- function(fndat, fnmetricname, fnlowercutoff){
  
  fn_plot <- ggplot(dat = fndat, aes(x = facnum, y = studentres, color = region)) + 
    geom_point() + 
    
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    geom_hline(yintercept = 0, linetype = 2) +
    geom_hline(yintercept = c(-3, 3), linetype = 2, color = 'red') +  # adds line at +/-3 sigma
    geom_hline(yintercept = fnlowercutoff, linetype = 1, color = 'purple')  # adds a line at lower 10% of group
  
  if (fnmetricname == 'dmg'){
    
    metrictitle <- 'DAMAGES'
    fn_plot <- fn_plot + facet_grid(carrclass ~ businessunit + z_pinnacle_group, scales = 'free')
    
  } else if (fnmetricname == 'whse'){
    
    metrictitle <- 'WAREHOUSE ERRORS'
    fn_plot <- fn_plot + facet_grid(carrclass ~ businessunit + z_pinnacle_group, scales = 'free')
    
  }else if (fnmetricname == 'mmo'){
    
    metrictitle <- 'MARKOUTS'
    fn_plot <- fn_plot + facet_grid(. ~ businessunit + z_pinnacle_group, scales = 'free')
    
  }
  
  fn_plot <- fn_plot +     
    ggtitle(paste(metrictitle, '- Studentized Residuals by Carrier Class, Facility within Pinnacle Group'), 
            subtitle = 'Each point represents one month in 2017 per facility')
  
  return(fn_plot)
  
}  # end plot_resids_fn



# LOAD DATA ###################################################################

# load itbq data -------------------------------------------------------------

fname1 <- "itbq_facility_report_2017_data.xlsx"
fsheet1 <- "itbq_facility_report_2017_all"

dat <- read_excel(fname1, fsheet1) %>% clean_names()

glimpse(dat)



# load facility data ----------------------------------------------------------

fname2 <- "facility.xlsx"
fsheet2 <- "Sheet1"

dat_facinfo <- read_excel(fname2, fsheet2) %>% clean_names()

glimpse(dat_facinfo)



# MERGE DATA ##################################################################

dmerge <- merge()



# common column names ---------------------------------------------------------

# # EXPORT df_q_metrics2 for upload into SharePoint -----------------------------
# 
# library(writexl)
# 
# write_xlsx(df_q_metrics2, 'itbq_for_2018_goal_setting_DELETE.xlsx')



###############################################################################
# EXPLORE GRAPHICALLY 
###############################################################################

# plots of ship lines by facility within pinnacle group


### CHECKING OUTCOMES
# names(df_q_metrics)
# with(df_q_metrics, table(z_pinnacle_group))
# 
# levels(factor(df_q_metrics$z_pinnacle_group))
# 
# 
# # check carrclass across metric types
# dim( dplyr::filter(df_q_metrics, carrclass == 'Total') )
# with( dplyr::filter(df_q_metrics, carrclass == 'Total'), table(metric_type, carrclass) )
# with( dplyr::filter(df_q_metrics, carrclass != 'Total'), table(metric_type, carrclass) )
# 
# 
# # check for NA entries in pinnacle group
# dim( dplyr::filter(df_q_metrics, z_pinnacle_group %in% c("G1","G2","G3","G4","G5")) )
# dim( dplyr::filter(df_q_metrics, !(z_pinnacle_group %in% c("G1","G2","G3","G4","G5"))) )


# plot ship_lines by facility -------------------------------------------------
# (carrclass == 'Total' only)
plot1 <- ggplot(dat_itbq_metric[dat_itbq_metric$carrclass == 'Total',], 
                aes(x = facnum, y = ship_lines, color = z_region)) + 
  geom_point() + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(~ z_pinnacle_group, scales = 'free_x') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle('Ship Lines by Facility within Pinnacle Group')

plot1


# plot ship_lines by facility for all carrier classes -------------------------
# (carrclass != EFD FSU) filtered out earlier
plot2 <- ggplot(dat_itbq_metric, 
                aes(x = facnum, y = ship_lines, color = z_region)) + 
  geom_point() + 
  scale_y_continuous(labels = comma) + 
  facet_grid(carrclass ~ z_pinnacle_group, scales = 'free') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle('Ship Lines by Carrier Class, Facility within Pinnacle Group',
          subtitle = 'Each point represents one month in 2017 per facility')

plot2



###############################################################################
# REGRESSION MODELING 
# (is actually ANCOVA but will call it regression for field familiarity)
###############################################################################


# process:
# for each metric
# create model -> metric_prop ~ carrclass + factor(month) + business_unit + region + peer_group



# BE-FE DAMAGES ###############################################################
# create model -> metric_prop ~ carrclass + factor(month) + business_unit + region + peer_group

# dat_dmg_metric
# names(dat_dmg_metric)
# dim(dat_dmg_metric)
# dim(dat_dmg_fit)


# remove NA values from metric_prop
dat_dmg_fit <- dat_dmg_metric %>% dplyr::filter(!is.na(metric_prop))


# create the model withOUT ship lines
fit_dmga <- aov(metric_prop ~ factor(carrclass) + factor(yearmonth) + 
                  factor(businessunit) + factor(region) + 
                  factor(z_pinnacle_group),
                data = dat_dmg_fit)

summary(fit_dmga)


# create the model with ship lines
fit_dmgb <- aov(metric_prop ~ factor(carrclass) + factor(yearmonth) + 
                 factor(businessunit) + factor(region) + 
                 factor(z_pinnacle_group) + ship_lines,
               data = dat_dmg_fit)

summary(fit_dmgb)


# compare the two models
anova(fit_dmga, fit_dmgb)


### we see that there is a signif diff, so use the model with ship lines
fit_dmg <- fit_dmgb


# str(fit_dmg)

# # TRY LOG(Y)
# fit_dmglog <- aov(log(metric_prop) ~ factor(carrclass) + factor(yearmonth) + 
#                  factor(businessunit) + factor(region) + 
#                  factor(z_pinnacle_group),
#                data = dat_dmg_fit)
# 
# summary(fit_dmglog)




# add studentized residuals to data for plotting, review
dat_dmg_fit$studentres <- rstudent(fit_dmg)
# dat_dmg_fit$studentres <- rstudent(fit_dmglog)


# filter for observation with studentized residual ~= -60
dat_dmg_fit2 <- dat_dmg_fit %>% dplyr::filter(abs(studentres) < 50 )


##### TO PLOT USE THE FUNCTION INSTEAD
# !!! need lower_sres_cutoff from approx line 750
# 
# plot_dmgres <- ggplot(dat_dmg_fit2, aes(x = facnum, y = studentres, color = region)) + 
#   geom_point() + 
#   facet_grid(carrclass ~ businessunit + z_pinnacle_group, scales = 'free') + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_hline(yintercept = c(-3, 3), linetype = 2, color = 'red') +  # adds line at +/-3 sigma
#   geom_hline(yintercept = lower_sres_cutoff, linetype = 1, color = 'purple') +  # adds a line at lower 10% of group
#   ggtitle('DAMAGES - Studentized Residuals by Carrier Class, Facility within Pinnacle Group',
#           subtitle = 'Each point represents one month in 2017 per facility')
# plot_dmgres
#
# library(plotly)
# ggplotly(plot_dmgres)


# plot the studentized residuals
plot_resids_fn(dat_dmg_fit2, 'dmg', lower_sres_cutoff)


# see 'IMPROVEMENT' section below


# BE-FE WAREHOUSE ERRORS ######################################################
# create model -> metric_prop ~ carrclass + factor(month) + business_unit + region + peer_group

# remove NA values from metric_prop
dat_whse_fit <- dat_whse_metric %>% dplyr::filter(!is.na(metric_prop))


# create the model withOUT ship lines
fit_whsea <- aov(metric_prop ~ factor(carrclass) + factor(yearmonth) + factor(businessunit) + factor(region) + factor(z_pinnacle_group),
               data = dat_whse_fit)

summary(fit_whsea)


# create the model WITH ship lines
fit_whseb <- aov(metric_prop ~ factor(carrclass) + factor(yearmonth) + factor(businessunit) + 
                  factor(region) + factor(z_pinnacle_group) + ship_lines,
                data = dat_whse_fit)

summary(fit_whseb)


# compare the two models
anova(fit_whsea, fit_whseb)


### we see that there is NO signif diff, so use the model with ship lines
fit_whse <- fit_whsea



# add studentized residuals to data for plotting, review
dat_whse_fit$studentres <- rstudent(fit_whse)


# filter for observation with studentized residual ~= -60
dat_whse_fit2 <- dat_whse_fit %>% dplyr::filter(abs(studentres) < 50)


# plot the studentized residuals
plot_resids_fn(dat_whse_fit2, 'whse', lower_sres_cutoff)



# see 'IMPROVEMENT' section below



# BE-FE MARKOUTS ##############################################################
# create model -> metric_prop ~ factor(month) + business_unit + region + peer_group

# remove NA values from metric_prop
dat_mmo_fit <- dat_mmo_metric %>% dplyr::filter(!is.na(metric_prop))


# create the model withOUT ship lines
fit_mmoa <- aov(metric_prop ~ factor(yearmonth) + factor(businessunit) + factor(region) + 
                 factor(z_pinnacle_group),
                data = dat_mmo_fit)

summary(fit_mmoa)


# create the model WITH ship lines
fit_mmob <- aov(metric_prop ~ factor(yearmonth) + factor(businessunit) + factor(region) + 
                  factor(z_pinnacle_group),
                data = dat_mmo_fit)

summary(fit_mmob)


# add studentized residuals to data for plotting, review
dat_mmo_fit$studentres <- rstudent(fit_mmo)


# plot the studentized residuals
plot_resids_fn(dat_mmo_fit, 'mmo', lower_sres_cutoff)



# see 'IMPROVEMENT' section below


#==============================================================================
# DIAGNOSTICS =================================================================
#==============================================================================

# SELECT ONE
fit <- fit_dmg; dat <- dat_dmg_fit
fit <- fit_whse; dat <- dat_whse_fit
fit <- fit_mmo; dat <- dat_mmo_fit


fit <- fit_dmglog


###############################################################################
# MODEL DIAGNOSTICS
###############################################################################

library(car)
library(lmtest)
library(vrtest)


# model results ---------------------------------------------------------------

print(summary(fit))




# plot fitted vs residuals ----------------------------------------------------

fitted.y = fit$fitted


# plot residuals vs fitted
plot(fitted.y, residuals(fit), ylim = c(-0.05, 0.05))
title("Residuals vs Fitted Values")
grid()




# normality diagnostics -------------------------------------------------------

# test normality using Shapiro-Wilks test 
res.shapiro = shapiro.test(residuals(fit))

print(res.shapiro)

if (res.shapiro$p.value < 0.05){
  print("Nonnormally distributed residuals")} else {
    print("Normally distributed residuals")
  }



# normality plot of residuals -------------------------------------------------

# exploring very large studentized residual
restmp <- rstudent(fit)[abs(rstudent(fit)) < 50]
qqPlot(restmp, main = "Normal Plot of Residuals")
hist(restmp)


qqPlot(residuals(fit), main = "Normal Plot of Residuals")

hist(residuals(fit))



resfit2 <- residuals(fit)[abs(residuals(fit)) < 8]

qqPlot(resfit2, main = "Normal Plot of Residuals")

hist(resfit2)



# family of influence measures ------------------------------------------------

infl.fit = influence.measures(fit)

print(summary(infl.fit))

id.infl = which(apply(infl.fit$is.inf, 1, any))

print("Most influential observations:")
print(id.infl)




# Cook's Distance -------------------------------------------------------------

# find unusual Cooks Distance

fit.cook = cooks.distance(fit)

print("Influential Cooks D")
print(which(fit.cook > 3*mean(fit.cook)))




# plot Cook's diagnostics -----------------------------------------------------

fit.cook = cooks.distance(fit)

id.c = which(fit.cook > 3*mean(fit.cook))

# plot Cooks Distance
plot(fit.cook)
abline(h = c(1,3)*mean(fit.cook), col = 2)
title("Cook's Distance")
grid()

if (length(id.c) > 0){ text(id.c, fit.cook[id.c], rownames(dat)[id.c], pos = 2, xpd = TRUE) }



# print studentized residuals diagnostics -------------------------------------

# STUDENTIZED RESIDUALS
fit.studres = rstudent(fit)

print("Noteworthy studentized residuals")
print(which(abs(fit.studres) > 3))




# plot studentized residuals diagnostics -------------------------------------

fit.studres = rstudent(fit)

id.sr = which(abs(fit.studres) > 3)

# plot studentized residuals
plot(rstudent(fit), ylim = c(-4,4))
abline(h = c(-3,+3), col = 'red', lty = 3)
title('Studentized Residuals')
grid()


if (length(id.sr) > 0){ text(id.sr, fit.studres[id.sr], rownames(dat)[id.sr], pos = 2, xpd = TRUE) }


dat_dmg_fit[id.sr, ]


# print leverage diagnostics --------------------------------------------------

# LEVERAGE based on HAT MATRIX
fit.hat = hatvalues(fit)

print("Noteworthy leverage values")
print(which(fit.hat > 3*mean(fit.hat)))



# plot leverage diagnostics ---------------------------------------------------

fit.hat = hatvalues(fit)

id.h = which(fit.hat > 3*mean(fit.hat))


# plot leverage
plot(fit.hat)
abline(h = c(1,3)*mean(fit.hat), col = 2)
title('Leverage')
grid()


if (length(id.h) > 0){ text(id.h, fit.hat[id.h], rownames(dat)[id.h], pos = 2, xpd = TRUE) }



# print functional form diagnostics -------------------------------------------

# test for functional form
# conditional mean of residuals equal to zero
# using the RESET test

res.fform = resettest(fit)

if (res.fform$p.value < 0.05){
  print("Functional Form Misspecified [E(e|X) <> 0]")} else {
    print("Functional Form Adequate [E(e|X) = 0]")
  }



# print constant var diagnostics ----------------------------------------------

# test for heteroskedasticity
# using the Breusch-Pagan test

res.bp = bptest(fit)
res.bp$p.value

if (res.bp$p.value < 0.05){
  print("Residuals have NON-constant variance")} else {
    print("Residuals have constant variance")
  }


# print autocorrelation diagnostics -------------------------------------------

# test for autocorrelation
# the Box-Ljung test only tests for specific individual lags

res.box = Box.test(residuals(fit), lag = 1, type = "Ljung-Box")

print("Box-Ljung Test for autocorrelation")

if (res.box$p.value < 0.05){
  print("Residuals NOT independent (autocorrelation)")} else {
    print("Residuals independent (no autocorrelation)")
  }



# the Auto.Q function uses a portmanteau test with multi-lags (10 by default)

portm.multilag = Auto.Q(y = residuals(fit))


print("Portmanteau Test for autocorrelation")

if (portm.multilag$Pvalue < 0.05){
  print("Residuals NOT independent (autocorrelation)")} else {
    print("Residuals independent (no autocorrelation)")
  }




###############################################################################
# IMPROVEMENT
###############################################################################

# uses data fed to the models above, plus studentized residuals

dim(dat_dmg_fit)
dim(dat_whse_fit)
dim(dat_mmo_fit)

# METHOD
# take every row where studentized residual < k (set k separately)
# impr_factor <- -1 * k_val * (rstudent value)
# improvement = number_errors - (impr_factor*number_errors)

names(dat_dmg_fit)



# METHOD 1 --------------------------------------------------------------------
# k_val will likely be greater than zero and less than or equal to one
# e.g., k_val = (0,1])


k_area_fn <- function(kv_fn, sr_fn){
  area_left_of_kv <- pnorm(kv_fn)
  area_left_of_sr <- pnorm(sr_fn)
  ka_fn <- (area_left_of_kv - area_left_of_sr)
  return(c(ka_fn, area_left_of_kv, area_left_of_sr))
}


k_val = -1
k_stdpct = 0.06


### Example ----------
# tmpsr = -5
# k_area_fn(k_val, tmpsr)[1]


# TO VISUALIZE ----------
bottom_x_pct = 0.1  # DM's bottom 10 percent expressed as a z-score for studentized residuals
lower_sres_cutoff <- qnorm(p = bottom_x_pct)

xtmp = seq(-5, 5, by = 0.1)
ytmp = dnorm(xtmp)

dftmp = data.frame(x = xtmp, y = ytmp)

ggplot(dftmp, aes(x = x, y = y)) +
  geom_line(color = 'blue') +
  geom_vline(xintercept = lower_cutoff, color = 'red') +
  geom_area(stat = "function", fun = dnorm, fill = "lightcoral", xlim = c(-5, lower_cutoff))
  #geom_vline(xintercept = c(k_val, tmpsr), color = 'red')


dftmp2 <- dftmp[dftmp$x <= 0,]

ggplot(dftmp, aes(x = x, y = y)) +
  geom_line(color = 'blue') +
  geom_vline(xintercept = c(k_val, tmpsr), color = 'red')



# ATTEMPT TO APPLY AN "IMPROVEMENT FACTOR" 

dat_dmg_fit2 <- dat_dmg_fit %>% 
  mutate(k_val = k_val, 
         k_stdpct = k_stdpct,
         area_left_of_kv = pnorm(k_val),
         area_left_of_sr = pnorm(studentres),
         area_diff = area_left_of_kv - area_left_of_sr,
         #area_prop_left = area_diff,
         k_area_val = k_area_fn(k_val, studentres)[1], 
         impr_factor = case_when(studentres < k_val ~ k_area_val * k_stdpct, 
                                 studentres >= k_val ~ 0),
         subtract_val = metric_count*impr_factor,
         new_metric_count = trunc(metric_count - metric_count*impr_factor))


# METHOD 2 --------------------------------------------------------------------

# see section setting lower_sres_cutoff above
impr_pct <- 0.04 # applied to the count of credits


## applies impr_pct when studentres is low AND when the 2017 actual was less than goal
# cond1 = TRUE; cond2 = TRUE
# cond1 = TRUE; cond2 = FALSE
# cond1 = FALSE; cond2 = TRUE
# cond1 = FALSE; cond2 = FALSE
# 
# (cond1) & (cond2)
# !((cond1) & (cond2))


# initialize 2018 metric goal based on 2017 goal
dat_dmg_git_method2 <- dat_dmg_fit %>% 
  mutate(impr_factor = 0,
         metric_goal_2018 = metric_goal_2017,
         new_metric_count = (1 - metric_goal_2018*ship_lines))


dat_dmg_fit_method2 <- dat_dmg_fit %>% 
  dplyr::filter(metric_to_goal < 0) %>% 
  
  # value of credit counts to subtract from last year's credit counts
  mutate(impr_factor = impr_pct,
         subtract_val = trunc(metric_count * impr_factor),
         new_metric_count = metric_count - subtract_val,
         
         # calculate 2018 new goal in pct (as proportion)
         metric_goal_2018 = 1 - (new_metric_count/ship_lines),

         # double check math - recalc 2017 goal and compare to original value
         metric_2017_check = 1 - (metric_count/ship_lines),

         # diff in 2017 and 2018 goals (in proportion)
         goal_diff = metric_goal_2017 - metric_goal_2018
         )  # end mutate
  

  # # sets an improvement is studentized residual is less than cutoff and last year metric was LT goal
  # mutate(impr_factor = case_when((studentres < lower_sres_cutoff) ~ impr_pct, 
  #                                (studentres >= lower_sres_cutoff) ~ 0),
  #        
  #        # requires an improvement unless either no impr_factor OR last year was at/over goal
  #        met_goal_2017_ind = case_when((metric_to_goal < 0) ~ 1,
  #                                      (metric_to_goal >= 0) ~ 0),
  #        
  #        # value of credit counts to subtract from last year's credit counts
  #        subtract_val = trunc(metric_count * impr_factor * met_goal_2017_ind),
  #        
  #        # proposed 2018 credit count assuming same lines as 2017
  #        # if met/beat goal in 2017 then assign 2017 credits as those related to 2017 goal
  #        new_metric_count = case_when((metric_to_goal < 0) ~ metric_count - subtract_val,
  #                                     (metric_to_goal >= 0) ~ metric_goal_2017 * ship_lines),
  #        
  #        # calculate 2018 new goal in pct (as proportion)
  #        metric_goal_2018 = 1 - (new_metric_count/ship_lines),
  #        
  #        # double check math - recalc 2017 goal and compare to original value
  #        metric_2017_check = 1 - (metric_count/ship_lines),
  #        
  #        # diff in 2017 and 2018 goals (in proportion)
  #        goal_diff = metric_goal_2017 - metric_goal_2018
  #        )  # end mutate


plot_dmg_impr <- ggplot(dat_dmg_fit_method2, aes(x = facnum, y = studentres, shape = region)) + 
  geom_point(aes(color = factor(impr_factor))) + 
  facet_grid(carrclass ~ z_pinnacle_group, scales = 'free') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = c(0, lower_sres_cutoff), linetype = 2) +
  ggtitle('DAMAGES - Studentized Residuals by Carrier Class, Facility within Pinnacle Group',
          subtitle = 'Each point represents one month in 2017 per facility')

plot_dmg_impr


hist(dat_dmg_fit2$goal_diff)




sel_facnum = '046'

plot_dmg_impr2 <- ggplot(dat_dmg_fit_method2[dat_dmg_fit_method2$facnum == sel_facnum, ], aes(shape = factor(impr_factor))) + 
  geom_point(aes(x = yearmonth, y = metric_prop), color = 'black') +
  geom_point(aes(x = yearmonth, y = metric_goal_2017), color = 'green', position = position_nudge(x = -0.1)) + 
  geom_point(aes(x = yearmonth, y = metric_goal_2018), color = 'red', position = position_nudge(x = 0.1)) + 
  # geom_jitter() + 
  facet_grid(carrclass ~ ., scales = 'free') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle('DAMAGES - 2017 Goal vs New Goal by Carrier Class, Facility')

plot_dmg_impr2



# METHOD 3 --------------------------------------------------------------------

# #select the top 5 total amounts per dateymd (and item category 4).
# 
# #Re-arrange order of rows in clms of existing plot
# datTA_plot2 <- arrange(datTA_plot2, dateymd, desc(tot_amt)) 
# 
# #Create new dataframe with counter (index or rank) column by year_month based on the ordering from above
# datTA_plot2 %>% group_by(dateymd) %>% mutate(rank = row_number(dateymd)) -> datTA_plot3
# 
# #Grab the top 5 counters and all of their related info
# datTA_plot_final <- datTA_plot3[datTA_plot3$rank <= 5, ]




# END CODE ####################################################################