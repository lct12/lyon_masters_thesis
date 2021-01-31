#Author: Lauren Thomas
#Date Created: 24/05/2020
#Last Updated: 31/05/2020
#Purpose: Import the data from Qualtrics (in Excel) and clean it

#import raw data
raw_data <- read.csv("data/raw_data_2.csv")

#delete unnecessary variables
edited_data <- select(raw_data, -Q1,-Status, -IPAddress, -Duration..in.seconds., -Finished,
                      -RecordedDate, -RecipientLastName, -RecipientFirstName, -RecipientEmail,
                      -ExternalReference, -LocationLatitude, -LocationLongitude, -DistributionChannel, -UserLanguage)
#rename other vars
edited_data <-select(edited_data, ResponseId, StartDate, EndDate, Progress, instructions_flag = flag, sex = Q4, race = Q5, hisp = Q6, nat = Q7,
                     majority_country = Q8, age = Q9, hh_income = Q10, expend = Q11, 
                     educ = Q12, pol_aff_str = Q14, rep_goals = Q15_1, dem_goals = Q15_2, 
                     cong_elect_str = Q16, sp_healthcare = Q17_1, pub_opt = Q17_2, free_college = Q17_3,
                     milt_spending_reverse = Q17_4, photo_ID_reverse = Q17_5, gnd = Q17_6, carbon_tax = Q17_7,
                     gun_restrict = Q17_8, stu_loans = Q17_9, priming_values = Q19,
                     priming_culture = Q20, priming_public_transit = Q21, priming_photo_event = Q25, 
                     priming_marines = Q26, priming_photo_month = Q27, control_tv_shows = Q29,
                     control_favorite_tv = Q30, control_public_transit = Q31, control_artist = Q34,
                     control_museum = Q35, control_year = Q36, check1 = Q38_1, check2 = Q38_2, check3 = Q46_1,
                     check4 = Q46_2, rep_american_str = Q39_1, dem_american_str = Q39_2, rep_brit_str = Q39_3,
                     dem_brit_str = Q39_4, rep_french_str = Q39_5, dem_french_str = Q39_6,
                     pred_rep_american_str = Q41_1, pred_dem_american_str = Q41_2, pred_rep_brit_str = Q41_3,
                     pred_dem_brit_str = Q41_4, pred_rep_french_str = Q41_5, pred_dem_french_str = Q41_6,
                     pat1 = Q42_1, pat2 = Q42_2, pat3 = Q42_3, pat4_reverse = Q42_4, pat5 = Q42_5,email = Q43
)

#Progress = 100 for all observations so only keep cases where this is true
edited_data <- filter(edited_data, Progress == 100)
#make edited data a data table (easier to work with)
setDT(edited_data)

#combine all mixed/other race
edited_data[edited_data$race == "Black or African-American,White or Caucasian,Native American or Alaskan Native"
                 | edited_data$race == "White or Caucasian,Asian" | 
              edited_data$race == "Other"]$race <- "Mixed/Other"

#flag participants who answer at least one of the four check questions wrong
edited_data$check_flag <- 1
edited_data[edited_data$check1 == 5 & edited_data$check2 == 4 & edited_data$check3 == 1 & edited_data$check4 == 3]$check_flag <- 0

#political affilitation
edited_data$pol_aff <- 1
edited_data$pol_aff[edited_data$pol_aff_str == "Democrat" ] <- 2
edited_data$pol_aff[edited_data$pol_aff_str == "Independent" ] <- 3
edited_data$pol_aff[edited_data$pol_aff_str == "Other" ] <- 4
#congressional elections
edited_data$cong_elect <- 0
edited_data$cong_elect[edited_data$cong_elect_str == "Democratic Party"] <- 1
#republican american choice
edited_data$rep_american <- 1
edited_data$rep_american[edited_data$rep_american_str == 'YOUR Choice: "B"'] <- 0
#democratic american choice
edited_data$dem_american <- 1
edited_data$dem_american[edited_data$dem_american_str == 'YOUR Choice: "B"'] <- 0
#republican brit choice
edited_data$rep_brit <- 1
edited_data$rep_brit[edited_data$rep_brit_str == 'YOUR Choice: "B"'] <- 0
#dem brit choice
edited_data$dem_brit <- 1
edited_data$dem_brit[edited_data$dem_brit_str == 'YOUR Choice: "B"'] <- 0
#rep french
edited_data$rep_french <- 1
edited_data$rep_french[edited_data$rep_french_str == 'YOUR Choice: "B"'] <- 0
#dem french
edited_data$dem_french <- 1
edited_data$dem_french[edited_data$dem_french_str == 'YOUR Choice: "B"'] <- 0
#predict opponents' choice
#rep american
#republican american choice
edited_data$pred_rep_american <- 1
edited_data$pred_rep_american[edited_data$pred_rep_american_str == 'THEIR Choice: "B"'] <- 0
#democratic american choice
edited_data$pred_dem_american <- 1
edited_data$pred_dem_american[edited_data$pred_dem_american_str == 'THEIR Choice: "B"'] <- 0
#republican brit choice
edited_data$pred_rep_brit <- 1
edited_data$pred_rep_brit[edited_data$pred_rep_brit_str == 'THEIR Choice: "B"'] <- 0
#dem brit choice
edited_data$pred_dem_brit <- 1
edited_data$pred_dem_brit[edited_data$pred_dem_brit_str == 'THEIR Choice: "B"'] <- 0
#rep french
edited_data$pred_rep_french <- 1
edited_data$pred_rep_french[edited_data$pred_rep_french_str == 'THEIR Choice: "B"'] <- 0
#dem french
edited_data$pred_dem_french <- 1
edited_data$pred_dem_french[edited_data$pred_dem_french_str == 'THEIR Choice: "B"'] <- 0


#control or priming treatment?
#all control got vincent van gogh right
edited_data$priming_treatment <- 1
edited_data[edited_data$control_artist == "Vincent Van Gogh"]$priming_treatment <- 0

#priming number of questions correct
edited_data$priming_q1[edited_data$priming_treatment == 1] <- 0
edited_data$priming_q1[edited_data$priming_treatment == 1
                       & edited_data$priming_photo_event == "The raising of the flag during the Battle of Iwo Jima"] <- 1

edited_data$priming_q2[edited_data$priming_treatment == 1] <- 0
edited_data$priming_q2[edited_data$priming_treatment == 1
                       & edited_data$priming_marines == "Three"] <- 1

edited_data$priming_q3[edited_data$priming_treatment == 1] <- 0
edited_data$priming_q3[edited_data$priming_treatment == 1
                       & edited_data$priming_photo_month == "February"] <- 1

edited_data <- mutate(edited_data, priming_correct = priming_q1 + priming_q2 + priming_q3)

#control number of questions correct
#priming number of questions correct
edited_data$control_q1[edited_data$priming_treatment == 0] <- 0
edited_data$control_q1[edited_data$priming_treatment == 0
                       & edited_data$control_artist == "Vincent Van Gogh"] <- 1

edited_data$control_q2[edited_data$priming_treatment == 0] <- 0
edited_data$control_q2[edited_data$priming_treatment == 0
                       & edited_data$control_museum == "The Museum of Modern Art"] <- 1

edited_data$control_q3[edited_data$priming_treatment == 0] <- 0
edited_data$control_q3[edited_data$priming_treatment == 0
                       & edited_data$control_year == "1889"] <- 1

edited_data$control_correct <- edited_data$control_q1 + edited_data$control_q2 + edited_data$control_q3

#total number of correct
edited_data <- mutate(edited_data, total_correct = ifelse(priming_treatment == 1, priming_correct, control_correct))

#Patriotism question four is reverse-coded
#combine and normalize patriotism
edited_data <-mutate(edited_data, pat4 = 100-pat4_reverse,
                     overall_pat = (pat1 + pat2 + pat3 + pat4 + pat5)/5,
                     pat_norm = normalize(overall_pat, method = "standardize"))
#political questions: higher - more left-wing, need to recode military spending
#and photo ID question to reflect this
#combine political identity questions
edited_data <- mutate(edited_data, milt_spending = 100-milt_spending_reverse,
                      photo_ID = 100-photo_ID_reverse,
                      polt_q_avg = (sp_healthcare+pub_opt+free_college+milt_spending
                                    + photo_ID + gnd + carbon_tax + gun_restrict
                                    + stu_loans)/9)
                      
#robustness checks--assigning people to parties based on 
#1. who they vote for, 2. which party's goals they agree with, 3. political questionnaire
#0 = Republican, 1 = Democratic
edited_data <- mutate(edited_data, pol_aff2 = ifelse(cong_elect == 1, 1,0), 
                      pol_aff3 = ifelse(dem_goals > rep_goals,1,0),
                      pol_aff4 = ifelse(polt_q_avg >= 50, 1, 0)
)
#if rep goals = dem goals, then assign polaff3 based on their responses to cong_elect
edited_data$pol_aff3[edited_data$dem_goals == edited_data$rep_goals 
                                             & edited_data$cong_elect == 0] <- 0
edited_data$pol_aff3[edited_data$dem_goals == edited_data$rep_goals 
                                             & edited_data$cong_elect == 1] <- 1


#dem-leaning independents, rep-leaning independents
#1 = self-identified Republican, #2 = Republican-leaning self-identified independent/other, 
#3 - Democratic-leaning self-identified independent/other, #4 - self-identified Democrats
#three types -- one using "which party would you vote for" 
#one using which party's goals you agree more with, and the last the pol't questionnaire
  edited_data <- mutate(edited_data, pol_group1 = ifelse(pol_aff==1,1,ifelse((pol_aff==3|pol_aff==4)
                                                                            & cong_elect == 0,2,
                                                         ifelse((pol_aff==3|pol_aff==4)& cong_elect ==1,3,
                                                         ifelse(pol_aff==2,4,NA)))),
                        pol_group2 = ifelse(pol_aff==1,1,ifelse((pol_aff==3|pol_aff==4)
                                                                & pol_aff3 == 0,2,
                                                                ifelse((pol_aff==3|pol_aff==4)& pol_aff3 ==1,3,
                                                                       ifelse(pol_aff==2,4,NA)))),
                        pol_group3 = ifelse(pol_aff==1,1,ifelse((pol_aff==3|pol_aff==4)
                                                                & pol_aff4 == 0,2,
                                                                ifelse((pol_aff==3|pol_aff==4)& pol_aff4 ==1,3,
                                                                       ifelse(pol_aff==2,4,NA))))
                        )
                                                       

#independent/other political affiliation flag
edited_data <- mutate(edited_data, ind_other = ifelse(pol_aff == 3 | pol_aff == 4, 1, 0))









