library(tidyverse)
library(lubridate)
library(ggplot2)

# read in the data set
portfolio <- read_csv('data/portfolio.csv')[,c(-1)]
glimpse(portfolio)
# portfolio data set contains 6 columns and 10 rows

profile <- read_csv('data/profile.csv')[,c(-1)]
glimpse(profile)
# profile data set contains 5 columns and 17000 rows

transcript <- read_csv('data/transcript.csv')[,c(-1)]
glimpse(transcript)
# transcript data set contains 4 columns and 306534 rows

#############################clean portfolio data###############################

# cleaning plan:{
# 1. rename id to offer_id, reward to offer_reward
# 2. one hot encoding  offer_type
# 3. one hot encoding channels}

# one hot encoding offer_type
portfolio <- portfolio %>% 
  rename(offer_id = id, offer_reward = reward) %>%
  mutate(offer_bogo = case_when(offer_type == 'bogo' ~ 1,
                                offer_type != 'bogo' ~ 0),
         offer_discount = case_when(offer_type == 'discount' ~ 1,
                                    offer_type != 'discount' ~ 0),
         offer_information = case_when(offer_type == 'informational' ~ 1,
                                offer_type != 'informational' ~0))

# one hot encoding channels
portfolio <- portfolio %>%
  mutate(channels = gsub("\\[|\\]", "", channels),
         channel_email = str_detect(channels, "email"),
         channel_web = str_detect(channels, "web"),
         channel_mobile = str_detect(channels, "mobile"),
         channel_social = str_detect(channels, "social")) %>%
  mutate_at(c("channel_email", "channel_web", "channel_mobile", "channel_social"), as.integer) %>%
  select(-channels, -offer_type)

#############################clean profile data#################################

# clean plan:
# 1. transform became_member_on into correct date_time as member_start_date
# 2. generate three columns based on member_start_date: year, month, weekday
# 3. drop missing values
# 4. one hot encoding gender 

# transform member start date into days as a member 
profile <- profile %>% 
  rename(customer_id = id) %>%
  mutate(member_start_date = ymd(became_member_on),
         member_days = ymd("18-08-01")-member_start_date) %>%
  select(-became_member_on)

# count missing values by column
profile_na_count <-sapply(profile, function(x) sum(is.na(x)))
profile_na_count
# columns with missing values are gender (2175) and income (2175), I choose to drop profiles with missingness. 
profile <- profile %>% drop_na()
cat('The percentage of profiles that are removed is', (17000-14825)/17000)

# one hot encoding gender
profile <- profile %>%
  mutate(gender_male = case_when(gender == 'M' ~ 1,
                                 gender != 'M' ~ 0),
         gender_female = case_when(gender == 'F' ~ 1,
                                   gender != 'F' ~ 0),
         gender_other = case_when(gender == 'O' ~ 1,
                                  gender != 'O' ~ 0)) %>%
  select(-gender)

#############################clean transcript data##############################

# clean plan:
# 1. rename person as customer_id
# 2. remove observations in transcript that do not appear in profile data
# 3. split transcript data into transcript_transaction and transcript_offer
# 4. one hot encoding event variable in transcript_offer data

# rename person into customer_id
transcript <- transcript %>% 
  rename(customer_id = person) %>% 
  select(-value)

# delete observations that does not appear in profile data
transcript <- transcript %>%
  inner_join(profile, by = 'customer_id') %>%
  select(customer_id, event, time, value_type, actual_value)

# split out transaction data from transcript data
transcript_transaction <- transcript %>% 
  filter(event == 'transaction') %>%
  mutate(actual_value = as.numeric(actual_value)) %>%
  rename(amount = actual_value) %>%
  select(-event, -value_type)

# split out offer data from transcript data
transcript_offer <- transcript %>% 
  filter(event != 'transaction') %>%
  rename(offer_id = actual_value) %>%
  select(-value_type)

# one hot encoding event
transcript_offer <- transcript_offer %>%
  mutate(offer_received = case_when(event == 'offer received' ~ 1,
                                    event != 'offer received' ~ 0),
         offer_viewed = case_when(event == 'offer viewed' ~ 1,
                                  event != 'offer viewed' ~ 0),
         offer_completed = case_when(event == 'offer completed' ~ 1,
                                     event != 'offer completed' ~ 0))

# generate total number of transaction, total amount spent and average amount spent aggregated by customer level
transcript_transaction <- transcript_transaction %>%
  group_by(customer_id) %>%
  summarise(num_transactions = n(),
            total_amount_spent = sum(amount),
            avg_amount_spent = round(total_amount_spent/num_transactions,2)) %>% 
  select(customer_id, num_transactions, avg_amount_spent) %>%
  ungroup()

#############################SUMMARIZE ALL DATASET TOGETHER######################

# join offer data and portfolio data together
offer_portfolio <- left_join(x = transcript_offer, y = portfolio, by = "offer_id")
# count missing values for each column
sapply(offer_portfolio, function(x) sum(is.na(x)))

# Step1: count number of offers received, viewed, completed aggregated by each customer each offer and each event.
offer_portfolio_aggregated <- offer_portfolio %>%
  group_by(customer_id, offer_id, event) %>%
  mutate(offer_received = sum(offer_received),
         offer_viewed = sum(offer_viewed),
         offer_completed = sum(offer_completed))%>%
  ungroup()

# Step2: summarize all information of each offer provided to each customer in one row.
offer_portfolio_aggregated <- offer_portfolio_aggregated %>%
  group_by(customer_id, offer_id) %>%
  summarise(offer_received = max(offer_received),
         offer_viewed = max(offer_viewed),
         offer_completed = max(offer_completed),
         offer_reward = max(offer_reward),
         difficulty = max(difficulty),
         duration = max(duration),
         offer_bogo = max(offer_bogo),
         offer_discount = max(offer_discount),
         offer_information = max(offer_information),
         channel_email = max(channel_email),
         channel_web = max(channel_web),
         channel_mobile = max(channel_mobile),
         channel_social = max(channel_social)) %>%
  ungroup()

# Step3: adjust some of the metrics due to repeated offer for some customers
offer_portfolio_aggregated <- offer_portfolio_aggregated %>%
  mutate(offer_reward = offer_received * offer_reward,
         difficulty = offer_received*difficulty,
         offer_bogo = offer_received*offer_bogo,
         offer_discount = offer_received*offer_discount,
         offer_information = offer_received*offer_information,
         channel_email = offer_received*channel_email,
         channel_mobile = offer_received*channel_mobile,
         channel_social = offer_received*channel_social,
         channel_web = offer_received*channel_web) %>%
  select(-duration)

# Step4: generate general offer information aggregated by customer level
top_comp1 <- offer_portfolio_aggregated %>%
  group_by(customer_id) %>%
  summarise(total_offer_received = sum(offer_received),
            total_offer_viewed = sum(offer_viewed),
            total_offer_completed = sum(offer_completed),
            avg_offer_reward = round(sum(offer_reward)/sum(offer_received),2),
            avg_offer_difficulty = round(sum(difficulty)/sum(offer_received),2),
            offer_bogo_cnt = sum(offer_bogo),
            offer_discount_cnt = sum(offer_discount),
            offer_information_cnt = sum(offer_information),
            channel_email_cnt = sum(channel_email),
            channel_web_cnt = sum(channel_web),
            channel_mobile_cnt = sum(channel_mobile),
            channel_social_cnt = sum(channel_social),
            total_vr = round(total_offer_viewed/total_offer_received,2),
            total_cr = round(total_offer_completed/na_if(total_offer_viewed,0),2)) %>%
  ungroup()
# since some customers do not view the offer, the conversion rate for these customers should be zero
top_comp1 <- top_comp1 %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# 2. Bogo Offers Performance
top_comp2 <- offer_portfolio %>%
  filter(offer_bogo == 1) %>%
  select(customer_id, offer_id, offer_received, offer_viewed, offer_completed, offer_bogo) %>%
  group_by(customer_id) %>%
  summarise(offer_bogo_received = sum(offer_received),
            offer_bogo_viewed = sum(offer_viewed),
            offer_bogo_completed = sum(offer_completed),
            offer_bogo_vr = round(offer_bogo_viewed/offer_bogo_received,2),
            offer_bogo_cr = round(offer_bogo_completed/na_if(offer_bogo_viewed,0),2)) %>%
  ungroup()
top_comp2 <- top_comp2 %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  select(customer_id, offer_bogo_vr, offer_bogo_cr)

# 3. Discount Offers Performance
top_comp3 <- offer_portfolio %>%
  filter(offer_discount == 1) %>%
  select(customer_id, offer_id, offer_received, offer_viewed, offer_completed, offer_discount) %>%
  group_by(customer_id) %>%
  summarise(offer_discount_received = sum(offer_received),
            offer_discount_viewed = sum(offer_viewed),
            offer_discount_completed = sum(offer_completed),
            offer_discount_vr = round(offer_discount_viewed/offer_discount_received,2),
            offer_discount_cr = round(offer_discount_completed/na_if(offer_discount_viewed,0),2)) %>%
  ungroup()
top_comp3 <- top_comp3 %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  select(customer_id, offer_discount_vr, offer_discount_cr)

# 4. Information Offers Performance
top_comp4 <- offer_portfolio %>%
  filter(offer_information == 1) %>%
  select(customer_id, offer_id, offer_received, offer_viewed, offer_completed, offer_information) %>%
  group_by(customer_id) %>%
  summarise(offer_information_received = sum(offer_received),
            offer_information_viewed = sum(offer_viewed),
            offer_information_completed = sum(offer_completed),
            offer_information_vr = round(offer_information_viewed/offer_information_received,2)) %>%
  ungroup()
top_comp4 <- top_comp4 %>% select(customer_id, offer_information_vr)

# 5. join top_comp1, top_comp2, top_comp3, top_comp4 together
top_comp <- top_comp1 %>% left_join(top_comp2, by = 'customer_id')
top_comp <- top_comp %>% left_join(top_comp3, by = 'customer_id')
top_comp <- top_comp %>% left_join(top_comp4, by = 'customer_id')
# replace na with 0
top_comp <- top_comp %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))

# 6. join top_comp and transaction data together
offer_transaction <- top_comp %>% 
  left_join(transcript_transaction, by = 'customer_id') %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# 7. join with profile data 
profile_offer_transaction <- profile %>% 
  left_join(offer_transaction, by = 'customer_id') %>%
  select(-member_start_date) %>%
  relocate(customer_id, .before = age) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# output final data set
write.csv(profile_offer_transaction, 'data/profile_offer_transaction.csv')





