##### ARTICLE - CONTINUOUS ALTITUDE AND CONCUSSIONS #####

# Load packages
pacman::p_load(tidyverse, readxl, lubridate, fuzzyjoin, epitools, patchwork, lme4)

# Load data
  # Read in list of concussions
  concussions_final <- read_csv("./Concussions_Altitude_ShinyApp/concussion_list_public.csv") %>% 
    filter(wk_occur != 0) %>%   #Regular season only
    select(-...1)
    
  
  # Read in game info
  games2 <- read_csv("./Concussions_Altitude_ShinyApp/game_list_public.csv") %>% 
    select(-...1) %>% 
    mutate(cut_644 = case_when(elev >= 644 ~ 1,
                               TRUE ~ 0))

# Introductory Analyses
  
  #Sum up concussion counts by team, week, and season
  t <- concussions_final %>% 
    group_by(Year, wk_occur, Team) %>% 
    summarize(n_conc = n()) %>% 
    filter(wk_occur != 0)
  
  #How many concussions in 2012-13? For comparison with Myer paper
  concussions_final %>%
    filter(Year <= 2013) %>%
    count(Player, Year) %>% # Include to get number of distinct players
                            # Ends up being 269 distinct players, or 286 distinct player-seasons
    nrow() #300 total concussions vs. 300 for Myer paper and 302 from Smoliga's Frontline database
  
    #Counts by year
    concussions_final %>%
      group_by(Year) %>%
      summarize(n = n())
    
  
  
  # Check for zero-inflation in data
  lambda <- mean(games2$n_conc)
  
  exp(lambda*-1) # ~57% of games should have 0 concussions
  
  zeroinfl_test <- games2 %>% 
    mutate(zero_conc = case_when(n_conc == 0 ~ 1,
                                 TRUE ~ 0))
  mean(zeroinfl_test$zero_conc) # True proportion is 59%. Minimal issue.
  rm(zeroinfl_test)





# Concussion vs. altitude analysis

altitude_1213 <- games2 %>% 
  filter(Year <= 2013) %>%  #2015 if including Frontline data, 2013 for Myer comparison, >= 2014 for seeing if Myer replicates
  mutate(elev = case_when(elev == 0 ~ 1,
                   TRUE ~ elev),
         elev_100 = elev/100) %>% 
  group_by(Location, elev, elev_100) %>% 
  summarize(n_conc = sum(n_conc),
            n_gms = n(),
            n_gms_w_conc = sum(conc_bin)) %>% 
  mutate(rate = n_conc/n_gms,
         lcl = rate - sqrt(n_conc/n_gms),
         ucl = rate + sqrt(n_conc/n_gms)) %>% 
  ungroup()

altitude_1215 <- games2 %>% 
  filter(Year <= 2015) %>%  #2015 if including Frontline data, 2013 for Myer comparison, >= 2014 for seeing if Myer replicates
  mutate(elev = case_when(elev == 0 ~ 1,
                          TRUE ~ elev),
         elev_100 = elev/100) %>% 
  group_by(Location, elev, elev_100) %>% 
  summarize(n_conc = sum(n_conc),
            n_gms = n(),
            n_gms_w_conc = sum(conc_bin)) %>% 
  mutate(rate = n_conc/n_gms,
         lcl = rate - sqrt(n_conc/n_gms),
         ucl = rate + sqrt(n_conc/n_gms)) %>% 
  ungroup()

altitude_1415 <- games2 %>% 
  filter(Year %in% c(2014, 2015)) %>%  #2015 if including Frontline data, 2013 for Myer comparison, >= 2014 for seeing if Myer replicates
  mutate(elev = case_when(elev == 0 ~ 1,
                          TRUE ~ elev),
         elev_100 = elev/100) %>% 
  group_by(Location, elev, elev_100) %>% 
  summarize(n_conc = sum(n_conc),
            n_gms = n(),
            n_gms_w_conc = sum(conc_bin)) %>% 
  mutate(rate = n_conc/n_gms,
         lcl = rate - sqrt(n_conc/n_gms),
         ucl = rate + sqrt(n_conc/n_gms)) %>% 
  ungroup()

altitude_1219 <- games2 %>% 
  filter(Year <= 2019) %>%  #2015 if including Frontline data, 2013 for Myer comparison, >= 2014 for seeing if Myer replicates
  mutate(elev = case_when(elev == 0 ~ 1,
                          TRUE ~ elev),
         elev_100 = elev/100) %>% 
  group_by(Location, elev, elev_100) %>% 
  summarize(n_conc = sum(n_conc),
            n_gms = n(),
            n_gms_w_conc = sum(conc_bin)) %>% 
  mutate(rate = n_conc/n_gms,
         lcl = rate - sqrt(n_conc/n_gms),
         ucl = rate + sqrt(n_conc/n_gms)) %>% 
  ungroup()

    # # Table of altitude by stadium
    # stadium_alts <- games2 %>% 
    #   group_by(Location, elev) %>% 
    #   summarize(start_year = min(Year)) %>% 
    #   ungroup()
    # 
    # write.csv(stadium_alts, "./stadium_alts.csv")

altitude_1419 <- games2 %>% 
  filter(Year >= 2014) %>%  #2015 if including Frontline data, 2013 for Myer comparison, >= 2014 for seeing if Myer replicates
  mutate(elev = case_when(elev == 0 ~ 1,
                          TRUE ~ elev),
         elev_100 = elev/100) %>% 
  group_by(Location, elev, elev_100) %>% 
  summarize(n_conc = sum(n_conc),
            n_gms = n(),
            n_gms_w_conc = sum(conc_bin)) %>% 
  mutate(rate = n_conc/n_gms,
         lcl = rate - sqrt(n_conc/n_gms),
         ucl = rate + sqrt(n_conc/n_gms)) %>% 
  ungroup()







#Continuous altitude plots

(plot_1213 <- altitude_1213 %>% 
  filter(n_gms >= 4) %>% 
  ggplot(aes(x = elev, y = rate)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point() +
  #geom_text(aes(label = Location)) +
  #geom_errorbar(aes(ymin = lcl, ymax = ucl)) +
  scale_x_continuous(trans = "log10") +
  ylab("Concussions Per Game") +
  xlab("Stadium Elevation, Feet (Log10 Scale)") +
  ggtitle("A. 2012-13") +
  geom_vline(aes(xintercept = 644), color = "red", linetype = "dashed") +
    ylim(c(0.1, 1.1)))

(plot_1215 <- altitude_1215 %>% 
    filter(n_gms >= 4) %>% 
    ggplot(aes(x = elev, y = rate)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    geom_point() +
    #geom_text(aes(label = Location)) +
    #geom_errorbar(aes(ymin = lcl, ymax = ucl)) +
    scale_x_continuous(trans = "log10") +
    ylab("Concussions Per Game") +
    xlab("Stadium Elevation, Feet (Log10 Scale)") +
    ggtitle("B. 2012-15") +
    geom_vline(aes(xintercept = 644), color = "red", linetype = "dashed") +
    ylim(c(0.1, 1.1)))

(plot_1415 <- altitude_1415 %>% 
    filter(n_gms >= 4) %>% 
    ggplot(aes(x = elev, y = rate)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    geom_point() +
    #geom_text(aes(label = Location)) +
    #geom_errorbar(aes(ymin = lcl, ymax = ucl)) +
    scale_x_continuous(trans = "log10") +
    ylab("Concussions Per Game") +
    xlab("Stadium Elevation, Feet (Log10 Scale)") +
    ggtitle("E. 2014-15") +
    geom_vline(aes(xintercept = 644), color = "red", linetype = "dashed") +
    ylim(c(0.1, 1.1)))

(plot_1219 <- altitude_1219 %>% 
    filter(n_gms >= 4) %>% 
    ggplot(aes(x = elev, y = rate)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    geom_point() +
    #geom_text(aes(label = Location)) +
    #geom_errorbar(aes(ymin = lcl, ymax = ucl)) +
    scale_x_continuous(trans = "log10") +
    ylab("Concussions Per Game") +
    xlab("Stadium Elevation, Feet (Log10 Scale)") +
    ggtitle("D. 2012-19") +
    geom_vline(aes(xintercept = 644), color = "red", linetype = "dashed") +
    ylim(c(0.1, 1.1)))

(plot_1419 <- altitude_1419 %>% 
    filter(n_gms >= 4) %>% 
    ggplot(aes(x = elev, y = rate)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    geom_point() +
    #geom_text(aes(label = Location)) +
    #geom_errorbar(aes(ymin = lcl, ymax = ucl)) +
    scale_x_continuous(trans = "log10") +
    ylab("Concussions Per Game") +
    xlab("Stadium Elevation, Feet (Log10 Scale)") +
    ggtitle("C. 2014-19") +
    geom_vline(aes(xintercept = 644), color = "red", linetype = "dashed") +
    ylim(c(0.1, 1.1)))

(fig1 <- plot_1213 + plot_1215 + plot_1419 + plot_1219 +
  plot_annotation(caption = "The solid lines on each plot are a fitted GAM; the shaded area is its 95% confidence interval. 
                  The vertical dashed lines demarcate the 644 ft (196m) cutpoint used in prior literature."))
ggsave("./fig1.png", plot = fig1)


# Crude IRR calculations
alt_irr <- games2 %>%
  filter(Year <= 2013) %>%
  mutate(cut_644 = as.factor(cut_644)) %>% 
  group_by(cut_644) %>% 
  summarize(concussions = sum(n_conc),
            n_games = n())

rateratio(alt_irr$concussions, alt_irr$n_games)








## Logistic and Poisson Models

# Create dataframe with 1 line per team-game (2 lines per game)
temp_home <- games2 %>% 
  select(-Away_Team) %>% 
  rename(Team = Home_Team)

temp_away <- games2 %>% 
  select(-Home_Team) %>% 
  rename(Team = Away_Team)

team_games <- rbind(temp_home, temp_away) %>% 
  select(-n_conc, -conc_bin) %>% 
  left_join(t, by = c("Team", "Week" = "wk_occur", "Year" = "Year")) %>% 
  mutate(n_conc = replace_na(n_conc, 0),
         conc_bin = case_when(n_conc >= 1 ~ 1,
                              TRUE ~ 0),
         cut_644 = case_when(elev >= 644 ~ 1,
                             TRUE ~ 0),
         elev_100 = elev/100,
         Year_c = Year - 2012,
         Home_Away = case_when(Location == Team ~ "Home",
                               TRUE ~ "Away"))

rm(temp_home, temp_away)


# Logistic regression for 1+ concussions per game

  # Cutpoint
  mlogit_1 <- glmer(conc_bin ~ cut_644 + (1|Team)
                    #+ Year_c + Week
                    , 
                  data = filter(team_games, Year <= 2013),
                  family = "binomial"(link = "logit"))
  
  mlogit_1
  exp(fixef(mlogit_1))
  exp(confint(mlogit_1, method = "Wald"))

  #Continuous
  mlogit_2 <- glmer(conc_bin ~ elev_100 + (1|Team)
                    #+ Year_c + Week
                    , 
                  data = filter(team_games, Year >= 2014),
                  family = "binomial"(link = "logit"))
  
  mlogit_2
  exp(fixef(mlogit_2))
  exp(confint(mlogit_2, method = "Wald"))


# Poisson regression for concussion rates. Offsets not needed because every row is 1 team-game.

  # Cutpoint
  mpoiss_1 <- glmer(n_conc ~ cut_644 + (1|Team)
                    #+ Year_c + Week
                    , data = filter(team_games, Year <= 2013),
                  family = "poisson"(link = "log"))
  
  mpoiss_1
  exp(fixef(mpoiss_1))
  exp(confint(mpoiss_1, method = "Wald"))
  
    # Check for overdispersion (from http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-for-overdispersioncomputing-overdispersion-factor)
    overdisp_fun <- function(model) {
      rdf <- df.residual(model)
      rp <- residuals(model,type="pearson")
      Pearson.chisq <- sum(rp^2)
      prat <- Pearson.chisq/rdf
      pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
      c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
    }
    
    overdisp_fun(mpoiss_1) # Significant evidence of overdispersion when use whole dataset, but test may
                           # be overpowered and ratio is quite close to 1.0
    
      # Try a negative binomial model to compare to Poisson above
      m1 <- glm.nb(n_conc ~ cut_644, data = filter(team_games, Year <= 2013))
      summary(m1)
      exp(coef(m1))
      exp(confint(m1))

        # Cook's Distance
        
        # linear <- lm(n_conc ~ cut_644, data = filter(team_games, Year <= 2013))
        # 
        # temp <- filter(team_games, Year <= 2013)
        # a <- influence.measures(linear)
        # b <- a[[1]]
        # ols_plot_cooksd_bar(linear)

  # Continuous
  mpoiss_2 <- glmer(n_conc ~ elev_100 + (1|Team)
                    #+ Year_c + Week
                    , data = filter(team_games, Year <= 2013),
                    family = "poisson"(link = "log"))
  
  mpoiss_2
  exp(fixef(mpoiss_2))
  exp(confint(mpoiss_2, method = "Wald"))
  
    # Check for overdispersion
    overdisp_fun(mpoiss_2) # Significant evidence of overdispersion when use whole dataset, but test may
                           # be overpowered and ratio is quite close to 1.0
    
    # Try a negative binomial model to compare to Poisson above
    m2 <- glm.nb(n_conc ~ elev_100, data = filter(team_games, Year <= 2013))
    summary(m2)
    exp(coef(m2))
    exp(confint(m2)) # Extremely similar results






