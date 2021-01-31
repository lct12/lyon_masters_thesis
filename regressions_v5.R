#Author: Lauren Thomas
#Date Created: 01/06/2020
#Last Updated: 13/06/2020
#Purpose: regressions and output


#begin by reshaping data from wide to long 
regression_data <- gather(edited_data, 'rep_american', 'dem_american', 'rep_brit', 'dem_brit',
                       'rep_french', 'dem_french', key = "opponent", value = "decision" )
#select only variables necessary for regressions
regression_data <- select(regression_data, ResponseId, instructions_flag, age, pol_aff_str, check_flag, sex, race, hisp, hh_income,
                          expend, educ, pol_aff, pred_rep_american, pred_dem_american, pred_rep_brit,
                          pred_dem_brit, pred_rep_french, pred_dem_french, priming_treatment,
                          total_correct, overall_pat, polt_q_avg, pol_aff2, pol_aff3,
                          pol_aff4, pol_group1, pol_group2, pol_group3, ind_other, opponent, decision
)
#rename pol_aff to pol_aff1
regression_data <- select(regression_data, pol_aff1 = pol_aff, everything())

#generate variable equal to prediction of opponent's decision (equal to pred_dem_american when
#opponent = dem_american, equal to pred_rep_american when opponent = rep_american, etc.)
regression_data <- mutate(regression_data, pred_decision = ifelse(opponent == "rep_american", 
                          pred_rep_american, 
                          ifelse(opponent == "dem_american", pred_dem_american, 
                                 ifelse(opponent == "rep_brit", pred_rep_brit,
                                ifelse(opponent == "dem_brit", pred_dem_brit, 
                                ifelse(opponent == "rep_french", pred_rep_french,
                                pred_dem_french))))))
#drop pred_... vars
regression_data <- select(regression_data, -pred_rep_american, -pred_dem_american, 
                          -pred_rep_brit, -pred_dem_brit, -pred_rep_french, -pred_dem_french)
#convert categorical variables to factors
regression_data <- mutate(regression_data, pol_aff1 = as.factor(pol_aff1), pol_aff2 = as.factor(pol_aff2),
                          pol_aff3 = as.factor(pol_aff3), pol_aff4 = as.factor(pol_aff4),
                          pol_group1 = as.factor(pol_group1), pol_group2 = as.factor(pol_group2),
                          pol_group3 = as.factor(pol_group3))
#create flag indicating if opponent is same or different political/nationality 
regression_data <- mutate(regression_data, same_pol = ifelse((opponent == "rep_american" | 
                                                     opponent == "rep_brit" | opponent == "rep_french")
                                                    & pol_aff_str == "Republican",1,ifelse((opponent == "dem_american"
                                                             | opponent == "dem_brit" 
                                                             | opponent == "dem_french")
                                                               & pol_aff_str == "Democrat",1,0)),
                          same_pol2 = ifelse((opponent == "rep_american" | 
                                                opponent == "rep_brit" | opponent == "rep_french")
                                             & pol_aff2 == 0,1,ifelse((opponent == "dem_american"
                                                                                     | opponent == "dem_brit" 
                                                                                     | opponent == "dem_french")
                                                                                    & pol_aff2 == 1,1,0)),
                          same_pol3 = ifelse((opponent == "rep_american" | 
                                                opponent == "rep_brit" | opponent == "rep_french")
                                             & pol_aff3 == 0,1,ifelse((opponent == "dem_american"
                                                                       | opponent == "dem_brit" 
                                                                       | opponent == "dem_french")
                                                                      & pol_aff3 == 1,1,0)),
                          same_pol4 = ifelse((opponent == "rep_american" | 
                                                opponent == "rep_brit" | opponent == "rep_french")
                                             & pol_aff4 == 0,1,ifelse((opponent == "dem_american"
                                                                       | opponent == "dem_brit" 
                                                                       | opponent == "dem_french")
                                                                      & pol_aff4 == 1,1,0)),
                          same_nat = ifelse(opponent == "dem_american" | opponent == "rep_american",
                                            1,0))

#now create flag for I = s,s; I = s, o; I = o, s; and I = o, o
regression_data <- mutate(regression_data, ss_flag = ifelse(same_pol == 1 & same_nat == 1, 1,0),
                          so_flag = ifelse(same_pol == 1 & same_nat == 0,1,0),
                          os_flag = ifelse(same_pol == 0 & same_nat == 1,1,0),
                          oo_flag = ifelse(same_pol == 0 & same_nat == 0,1,0))
#create flag for when opponent = dem_american, dem_french, dem_brit
regression_data <-mutate(regression_data, left_wing = ifelse(opponent == "dem_american" |
                                                               opponent == "dem_brit" |
                                                               opponent == "dem_french",1,0))

regression_data <- mutate(regression_data, opp_numeric = ifelse(opponent == "rep_american",1,ifelse(
                                          opponent=="dem_american",2,ifelse(opponent=="rep_brit",3,
                                          ifelse(opponent == "dem_brit",4,ifelse(opponent=="rep_french",5,6
                                                                                 )))
)))

#create data frames for republicans only, dems only, dem-leaning independents and rep-leaning independents
rep_only <- filter(regression_data, pol_group1 == 1)
rep_ind <- filter(regression_data, pol_group1 == 2)
dem_ind <- filter(regression_data, pol_group1 == 3)
dem_only <- filter(regression_data, pol_group1 == 4)

#create clustered standard error and output formula
cluster_se <-  function(a,b,c) {
  vcov_subject <- cluster.vcov(a, b$ResponseId)
  m1 <- coeftest(a, vcov_subject)
  stargazer(m1, title = c, out = paste("LaTeX/tables/regression/",c, ".tex"))
}

#create exponent (for odds ratio) and output formula


#Hypothesis 1: a = 1 more likely when Player B is left-wing (people more likely to cooperate with left-wing players)

#controls: age, sex, race, check_flag, hisp, hh_income, expend, educ, total_correct, same_pol, 
#ind_other flag
#include clustered standard errors
hyp1a <-glm(pred_decision ~left_wing + same_pol + ind_other, data= regression_data, 
            family = "binomial")
cluster_se(hyp1a, regression_data, "hyp1a")
sink(file = paste("log_output/odds_ratio"), append = F)
print("hyp1a")
exp(coefficients(hyp1a))
rsq(hyp1a, adj=T)
sink()


hyp1b <-glm(pred_decision ~left_wing + age + sex + race +hisp + hh_income + expend + educ
            +same_pol + ind_other
            + check_flag + instructions_flag  + overall_pat, data= regression_data, family = "binomial")
cluster_se(hyp1b, regression_data, "hyp1b")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp1b")
exp(coefficients(hyp1b))
rsq(hyp1b, adj=T)
sink()

hyp1c <-glm(pred_decision ~left_wing + age + sex + race +hisp + hh_income + expend + educ
            +same_pol2
            + check_flag + instructions_flag  + overall_pat, data= regression_data, family = "binomial")
cluster_se(hyp1c, regression_data, "hyp1c")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp1c")
exp(coefficients(hyp1c))
rsq(hyp1c, adj=T)
sink()

hyp1d <-glm(pred_decision ~left_wing + age + sex + race +hisp + hh_income + expend + educ
            +same_pol3
            + check_flag + instructions_flag  + overall_pat, data= regression_data, family = "binomial")
cluster_se(hyp1d, regression_data, "hyp1d")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp1d")
exp(coefficients(hyp1d))
rsq(hyp1d, adj=T)
sink()

hyp1e <-glm(pred_decision ~left_wing + age + sex + race +hisp + hh_income + expend + educ
            +same_pol4 
            + check_flag + instructions_flag  + overall_pat, data= regression_data, family = "binomial")
cluster_se(hyp1e, regression_data, "hyp1e")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp1e")
exp(coefficients(hyp1e))
rsq(hyp1e, adj=T)
sink()


#Hypothesis 2: Priming treatment should increase a(I) relative to control
americans_only <- filter(regression_data, same_nat == 1)
hyp2a <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment, data= americans_only, family = "binomial")
cluster_se(hyp2a, americans_only, "hyp2a")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2a")
exp(coefficients(hyp2a))
rsq(hyp2a, adj=T)
sink()


hyp2b <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment 
            + ind_other + check_flag + instructions_flag + overall_pat
            + age + sex + race +hisp + hh_income + expend + educ
            , data= americans_only, family = "binomial")
cluster_se(hyp2b, americans_only, "hyp2b")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2b")
exp(coefficients(hyp2b))
rsq(hyp2b, adj=T)
sink()


#use diff measures of same_pol
hyp2b_pol2 <-glm(decision ~priming_treatment + same_pol2 + same_pol2*priming_treatment 
             + check_flag + instructions_flag + overall_pat
            + age + sex + race +hisp + hh_income + expend + educ
            , data= americans_only, family = "binomial")
cluster_se(hyp2b_pol2, americans_only, "hyp2b_pol2")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2b_pol2")
exp(coefficients(hyp2b_pol2))
rsq(hyp2b_pol2, adj=T)
sink()


hyp2b_pol3 <-glm(decision ~priming_treatment + same_pol3 + same_pol3*priming_treatment 
                  + check_flag + instructions_flag + overall_pat
                 + age + sex + race +hisp + hh_income + expend + educ
                 , data= americans_only, family = "binomial")
cluster_se(hyp2b_pol3, americans_only, "hyp2b_pol3")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2b_pol3")
exp(coefficients(hyp2b_pol3))
rsq(hyp2b_pol3, adj=T)
sink()


hyp2b_pol4 <-glm(decision ~priming_treatment + same_pol4 + same_pol4*priming_treatment 
                  + check_flag + instructions_flag + overall_pat
                 + age + sex + race +hisp + hh_income + expend + educ
                 , data= americans_only, family = "binomial")
cluster_se(hyp2b_pol4, americans_only, "hyp2b_pol4")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2b_pol4")
exp(coefficients(hyp2b_pol4))
rsq(hyp2b_pol4, adj=T)
sink()



#reps only
hyp2c <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment, 
            data= rep_only, family = "binomial")
cluster_se(hyp2c, rep_only, "hyp2c")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2c")
exp(coefficients(hyp2c))
rsq(hyp2c, adj=T)
sink()


hyp2d <-glm(decision ~priming_treatment + same_pol +  same_pol*priming_treatment  
              + check_flag + instructions_flag + overall_pat
            + age + sex + race +hisp + hh_income + expend + educ, data= rep_only, family = "binomial")
cluster_se(hyp2d, rep_only, "hyp2d")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2d")
exp(coefficients(hyp2d))
rsq(hyp2d, adj=T)
sink()


#Republican-leaning independents
hyp2e <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment, data= rep_ind, family = "binomial")
cluster_se(hyp2e, rep_ind, "hyp2e")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2e")
exp(coefficients(hyp2e))
rsq(hyp2e, adj=T)
sink()


hyp2f <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment 
            + check_flag + instructions_flag + overall_pat
            + age  + race + hh_income + expend + educ, data= rep_ind, family = "binomial")
cluster_se(hyp2f, rep_ind, "hyp2f")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2f")
exp(coefficients(hyp2f))
rsq(hyp2f, adj=T)
sink()


#Democratic-leaning independents
hyp2g <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment
            , data= dem_ind, family = "binomial")
cluster_se(hyp2g, dem_ind, "hyp2g")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2g")
exp(coefficients(hyp2g))
rsq(hyp2g, adj=T)
sink()


hyp2h <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment
             + check_flag + instructions_flag + overall_pat
            + age + sex + race +hisp + hh_income + expend + educ, data= dem_ind, family = "binomial")
cluster_se(hyp2h, dem_ind, "hyp2h")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2h")
exp(coefficients(hyp2h))
rsq(hyp2h, adj=T)
sink()


#Democratic only
hyp2i <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment, data= dem_only, family = "binomial")
cluster_se(hyp2i, dem_only, "hyp2i")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2i")
exp(coefficients(hyp2i))
rsq(hyp2i, adj=T)
sink()


hyp2j <-glm(decision ~priming_treatment + same_pol + same_pol*priming_treatment
             + check_flag + instructions_flag + overall_pat
            + age + sex + race +hisp + hh_income + expend + educ, data= dem_only, family = "binomial")
cluster_se(hyp2j, dem_only, "hyp2j")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp2j")
exp(coefficients(hyp2j))
rsq(hyp2j, adj=T)
sink()


#Hypotheses 4 & 5: subjects' decisions driven more by statistical discrimination than
# taste-based discrimination/a(I) higher for politics than nationality
hyp5a <- glm(decision~ same_pol + pred_decision + same_nat + same_pol*same_nat, data = regression_data, family = "binomial")
cluster_se(hyp5a, regression_data, "hyp5a")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5a")
exp(coefficients(hyp5a))
rsq(hyp5a, adj=T)
sink()


hyp5b <- glm(decision~ same_pol + pred_decision + same_nat + same_pol*same_nat
             + ind_other + check_flag + instructions_flag + overall_pat
             + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp5b, regression_data, "hyp5b")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5b")
exp(coefficients(hyp5b))
rsq(hyp5b, adj=T)
sink()


#use diff measures of pol affiliation
hyp5b_pol2 <- glm(decision~ same_pol2 + pred_decision + same_nat + same_pol2*same_nat
              + check_flag + instructions_flag + overall_pat
             + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp5b_pol2, regression_data, "hyp5b_pol2")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5b_pol2")
exp(coefficients(hyp5b_pol2))
rsq(hyp5b_pol2, adj=T)
sink()


hyp5b_pol3 <- glm(decision~ same_pol3 + pred_decision + same_nat + same_pol3*same_nat
                   + check_flag + instructions_flag + overall_pat
                  + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp5b_pol3, regression_data, "hyp5b_pol3")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5b_pol3")
exp(coefficients(hyp5b_pol3))
rsq(hyp5b_pol3, adj=T)
sink()


hyp5b_pol4 <- glm(decision~ same_pol4 + pred_decision + same_nat + same_pol4*same_nat
                   + check_flag + instructions_flag + overall_pat
                  + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp5b_pol4, regression_data, "hyp5b_pol4")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5b_pol4")
exp(coefficients(hyp5b_pol4))
rsq(hyp5b_pol4, adj=T)
sink()



#try for different groups
#rep
hyp5c <- glm(decision~ same_pol + pred_decision + same_nat + same_pol*same_nat
             , data = rep_only, family = "binomial")
cluster_se(hyp5c, rep_only, "hyp5c")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5c")
exp(coefficients(hyp5c))
rsq(hyp5c, adj=T)
sink()


hyp5d <- glm(decision~ same_pol + pred_decision + same_nat + same_pol*same_nat
             + check_flag + instructions_flag + overall_pat +
             + age + sex + race +hisp + hh_income + expend + educ, data = rep_only, family = "binomial")
cluster_se(hyp5d, rep_only, "hyp5d")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5d")
exp(coefficients(hyp5d))
rsq(hyp5d, adj=T)
sink()


#rep_ind
hyp5e <- glm(decision~ same_pol + pred_decision + same_nat + same_pol*same_nat
             , data = rep_ind, family = "binomial")
cluster_se(hyp5e, rep_ind, "hyp5e")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5e")
exp(coefficients(hyp5e))
rsq(hyp5e, adj=T)
sink()


#nobody is hispanic or female, so must take out those in the regression
hyp5f <- glm(decision~ same_pol + pred_decision + same_nat + same_pol*same_nat 
             +check_flag + instructions_flag + overall_pat 
             + age + race + hh_income + expend + educ, data = rep_ind, family = "binomial")
cluster_se(hyp5f, rep_ind, "hyp5f")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5f")
exp(coefficients(hyp5f))
rsq(hyp5f, adj=T)
sink()


#dem_ind
hyp5g <- glm(decision~ same_pol + pred_decision + same_nat + same_pol*same_nat, 
             data = dem_ind, family = "binomial")
cluster_se(hyp5g, dem_ind, "hyp5g")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5g")
exp(coefficients(hyp5g))
rsq(hyp5g, adj=T)
sink()


hyp5h <- glm(decision~ same_pol + pred_decision + same_nat + same_pol*same_nat
             +check_flag + instructions_flag + overall_pat
             + age + sex + race +hisp + hh_income + expend + educ
            , data = dem_ind, family = "binomial")
cluster_se(hyp5h, dem_ind, "hyp5h")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5h")
exp(coefficients(hyp5h))
rsq(hyp5h, adj=T)
sink()


#dem only
hyp5i <- glm(decision~ same_pol + pred_decision + same_nat + 
               same_pol*same_nat, data = dem_only, family = "binomial")
cluster_se(hyp5i, dem_only, "hyp5i")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5i")
exp(coefficients(hyp5i))
rsq(hyp5i, adj=T)
sink()


hyp5j <- glm(decision~ same_pol + pred_decision + same_nat + 
               check_flag + instructions_flag +overall_pat
               + same_pol*same_nat+ age + sex + race +hisp + hh_income + expend + educ
             , data = dem_only, family = "binomial")
cluster_se(hyp5j, dem_only, "hyp5j")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp5j")
exp(coefficients(hyp5j))
rsq(hyp5j, adj=T)
sink()


#Hypothesis 3: a(I)[s,s] stronger than other ones
hyp3a <- glm(decision~ ss_flag + os_flag +so_flag + oo_flag + pred_decision, data = regression_data, family = "binomial")
cluster_se(hyp3a, regression_data, "hyp3a")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp3a")
exp(coefficients(hyp3a))
rsq(hyp3a, adj=T)
sink()


hyp3b <- glm(decision~ ss_flag + os_flag +so_flag + oo_flag + pred_decision 
             + ind_other + check_flag + instructions_flag + overall_pat
             + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp3b, regression_data, "hyp3b")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp3b")
exp(coefficients(hyp3b))
rsq(hyp3b, adj=T)
sink()



#Hypothesis 6: LW subjects cooperate more than right-wing ones, all else equal
hyp6a <- glm(decision ~ priming_treatment + same_pol +same_nat 
             + same_pol*priming_treatment + same_pol*same_nat + same_nat*priming_treatment
             + same_pol*priming_treatment*same_nat + pred_decision + polt_q_avg, data = regression_data, family = "binomial")
cluster_se(hyp6a, regression_data, "hyp6a")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp6a")
exp(coefficients(hyp6a))
rsq(hyp6a, adj=T)
sink()


hyp6b <- glm(decision ~ priming_treatment + same_pol +same_nat 
             + same_pol*priming_treatment + same_pol*same_nat + same_nat*priming_treatment
             + same_pol*priming_treatment*same_nat + pred_decision +  polt_q_avg + 
             + ind_other + check_flag + instructions_flag + overall_pat
             + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp6b, regression_data, "hyp6b")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp6b")
exp(coefficients(hyp6b))
rsq(hyp6b, adj=T)
sink()


#using different measures of political affiliation
hyp6c <- glm(decision ~ priming_treatment + same_pol +same_nat 
             + same_pol*priming_treatment + same_pol*same_nat + same_nat*priming_treatment
             + same_pol*priming_treatment*same_nat + pred_decision + pol_aff2 
             + ind_other + check_flag + instructions_flag + overall_pat
             + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp6c, regression_data, "hyp6c")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp6c")
exp(coefficients(hyp6c))
rsq(hyp6c, adj=T)
sink()


hyp6d <- glm(decision ~ priming_treatment + same_pol +same_nat 
             + same_pol*priming_treatment + same_pol*same_nat + same_nat*priming_treatment
             + same_pol*priming_treatment*same_nat + pred_decision + pol_aff3
             + ind_other + check_flag + instructions_flag + overall_pat
             + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp6d, regression_data, "hyp6d")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp6d")
exp(coefficients(hyp6d))
rsq(hyp6d, adj=T)
sink()


hyp6e <- glm(decision ~ priming_treatment + same_pol +same_nat 
             + same_pol*priming_treatment + same_pol*same_nat + same_nat*priming_treatment
             + same_pol*priming_treatment*same_nat
             + pred_decision + pol_aff4 
             + ind_other + check_flag + instructions_flag + overall_pat
             + age + sex + race +hisp + hh_income + expend + educ, data = regression_data, family = "binomial")
cluster_se(hyp6e, regression_data, "hyp6e")
sink(file = paste("log_output/odds_ratio"), append = T)
print("hyp6e")
exp(coefficients(hyp6e))
rsq(hyp6e, adj=T)
sink()

sink(file = "log_output/power_test")
#power test
pwr.f2.test(u=1, v=53, f2=0.02)
pwr.f2.test(u=1, v=53, f2=0.15)
pwr.f2.test(u=1, v=53, f2=0.35)
sink()

  
