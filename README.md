## starbucks customer segmentation

### Data Description:
- Portfolio data: 10 offers and 6 columns of offer information
- profile data: 17000 customers and 5 columns of customer informations
- transcript data: 306648 events and 4 columns

### Data Cleansing:
- Portfolio data:
  - Rename id variable as offer_id for the convenience of further process of merging. 
  - One hot encoding on channel and offer type variable.
- Profile data:
  - Rename id variable as customer_id for the convenience of further process of merging.
  - Transform became_member_on into number of days that the person has been a member
  - Drop missing values and outliers together due to the loss of customer information
  - One hot encoding on gender variable
- Transcript data:	
  - Rename person variable as customer_id for the convenience of further process of merging.
  - Remove observations in transcript that do not appear in profile data
  - Split transcript data into transcript_transaction and transcript_offer based on event variable.
  - One hot encoding event variable in transcript_offer data set.

### Data Preprocessing:
- Generate 21 features group by customer levels and perform left join to generate final data set for modelling. 
  - total_offer_received: total number of offers received by each customer
  - total_offer_viewed: total number of offers viewed by each customer
  - total_offer_completed: total number of offers completed by each customer
  - total_vr: total view rate = total_offer_viewed/total_offer_received
  - total_cr: total completion rate = total_offer_completed/total_offer_viewed
  - avg_offer_reward
  - avg_offer_difficulty
  - offer_bogo_cnt: number of buy-one-get-one received by each customer
  - offer_discount_cnt: number of discount offer received by each customer
  - offer_information_cnt: number of informational offer received by each customer
  - offer_bogo_vr: buy-one-get-one offer view rate by each customer
  - offer_bogo_cr: buy-one-get-one offer completion rate by each customer
  - offer_discount_vr: discount offer view rate by each customer
  - offer_discount_cr: discount offer completion rate by each customer
  - offer_information_vr: informational offer view rate by each customer
  - channel_email_cnt: number of offer received through email by each customer
  - channel_web_cnt: number of offer received through web by each customer
  - channel_mobile_cnt: number of offer received through mobile by each customer
  - channel_social_cnt: number of offer received through social media by each customer
  - num_transactions: the total number of monetary transaction performed by each customer
  - avg_amount_spent: the average amount of money spent by each customer across all transactions
- 6 existing features in customer level:
  - age
  - income 
  - member_days
  - gender_male (0 or 1)
  - gender_female (0 or 1)
  - gender_other (0 or 1)
  
### Model
- Principal Component Analysis
  - check standard deviation of each predictor variables: 
    - Predictor variables have very different standard deviations. 
    - Therefore, we need to standardize each variable before performing principal component analysis.
![alt text](https://github.com/Qingyang666/starbucks-customer-segmentation/blob/main/figures/scree_plot1.png)
![alt text](https://github.com/Qingyang666/starbucks-customer-segmentation/blob/main/figures/scree_plot2.png)
- Cluster Analysis Part 1
  - Hierachical cluster analysis using euclidean distance
    - single linkage
    - complete linkage
    - average linkage
    - centroid linkage
    - ward linkage
- Dendrogram for each linkage method

  ![alt text](https://github.com/Qingyang666/starbucks-customer-segmentation/blob/main/figures/single_linkage.png)
  ![alt text](https://github.com/Qingyang666/starbucks-customer-segmentation/blob/main/figures/complete_linkage.png)
  ![alt text](https://github.com/Qingyang666/starbucks-customer-segmentation/blob/main/figures/average_linkage.png)
  ![alt text](https://github.com/Qingyang666/starbucks-customer-segmentation/blob/main/figures/central_linkage.png)
  ![alt text](https://github.com/Qingyang666/starbucks-customer-segmentation/blob/main/figures/ward_linkage.png)
