QVI_data <- QVI_data %>%
  mutate(month_ID = format(DATE, "%Y%m"))

## Filter the dataset for the stores of interest
QVI_data_sub <- QVI_data %>% 
  filter(STORE_NBR %in% c(77, 86, 88))
head(QVI_data_sub)

## Calculate monthly overall sales revenue
monthly_sales_revenue <- QVI_data_sub %>%
  group_by(STORE_NBR, month_ID) %>%
  summarize(total_sales = sum(TOT_SALES))
head(monthly_sales_revenue)

## Calculate monthly number of customers
monthly_customers <- QVI_data_sub %>%
  distinct(STORE_NBR, LYLTY_CARD_NBR, month_ID) %>%
  group_by(STORE_NBR, month_ID) %>%
  summarize(num_customers = n())
head(monthly_customers)

## Calculate monthly number of transactions per customer
monthly_transactions_per_customer <- QVI_data_sub %>%
  distinct(STORE_NBR, LYLTY_CARD_NBR, month_ID, TXN_ID) %>%
  group_by(STORE_NBR, month_ID) %>%
  summarize(num_transactions = n()) %>%
  left_join(monthly_customers, by = c("STORE_NBR", "month_ID")) %>%
  mutate(transactions_per_customer = num_transactions / num_customers)

## We define the measure calculations to use during the analysis.
### Convert data to a data.table object
setDT(QVI_data)
measureOvertime <- QVI_data[, .(totSales  = sum(TOT_SALES),
                                    ## Number of Customers 
                                    nCustomers = uniqueN(LYLTY_CARD_NBR) ,
                                    ## Number of unique or individual customers per Loyalty card
                                    nTxnPerCust = uniqueN(TXN_ID)/ uniqueN(LYLTY_CARD_NBR) ,
                                    ## Number of Chips sold per unique customers
                                    nChipsPerTxn = sum(TOT_SALES)/uniqueN(TXN_ID) ,
                                    ## Average Price Per Unit of Chips
                                    avgPricePerUnit = mean(TOT_SALES / PROD_QTY) )
                                , by = .(STORE_NBR, month_ID)][order(STORE_NBR, month_ID)]
head(measureOvertime) ## displaying the data.table measureOvertime

## Filter to the pre-trial period and stores with full observation periods

## Selecting all the stores with values that have 12 rows, i.e. stores that have a full observation period of 12 months. It gives a NUM[] vector.
storesWithFullObs <- unique(measureOvertime[,.N,STORE_NBR])[N == 12, STORE_NBR]
## Filtering the store data to Pre-Trial Period
preTrialMeasures <- measureOvertime[month_ID < 201902 & STORE_NBR %in% storesWithFullObs,]
head(storesWithFullObs)
head(preTrialMeasures)

## Create a function to calculate correlation for a measure, looping through each control store.
##Let's define inputTable as a metric table with potential comparison stores, metricCol as the store metric used to calculate correlation on, and storeComparison
##as the store number of the trial store.

calculateCorrelation <- function(inputTable, metricCol, storeComparison){
  
  calcCorrTable = data.table(Store1 = numeric(), 
                             Store2 = numeric(),
                             corr_measure = numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for(i in storeNumbers) {
    
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "corr_measure" = cor(inputTable[STORE_NBR == storeComparison, eval(metricCol)],
                                                        inputTable[STORE_NBR == i, eval(metricCol)]))
    
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
    
  }
  
  return(calcCorrTable)
  
}


## Making a standardized metric based on the absolute difference between the trial store's performance and each control store's performance.

## Create a function to calculate a standardized magnitude distance for a measure,
## looping through each control store

calculateMagnitudeDistance <- function(inputTable,  metricCol, storeComparison){
  
  calcDistTable = data.table(Store1 = numeric(),
                             Store2 = numeric(),
                             YEARMONTH = numeric(),
                             measure = numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for(i in storeNumbers) {
    
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "YEARMONTH" = inputTable[STORE_NBR == storeComparison, month_ID],
                                   "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)] -
                                                     inputTable[STORE_NBR == i, eval(metricCol)]))
    
    calcDistTable <- rbind(calcDistTable, calculatedMeasure, use.names=FALSE)
    
  }
  
  #### Standardize the magnitude distance so that the measure ranges from 0 to 1
  
  minMaxDist <- calcDistTable[, .(minDist = min(measure), 
                                  maxDist = max(measure)),
                              by = c("Store1", "YEARMONTH")]
  
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)),
                              by = .(Store1, Store2)]
  
  return(finalDistTable)
  
  
}


## Using the function created earlier to calculate correlations against store 77 using total sales and number of customers.
trial_store <- 77 # c(77L, 86L, 88L)  we have to use %%in%% for storeComparison
corr_nSales <- calculateCorrelation(preTrialMeasures , quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures , quote(nCustomers), trial_store)
head(corr_nCustomers)
head(corr_nSales)

## Calculation of Standardized measure for trial stores 
magnitude_nSales <- calculateMagnitudeDistance( preTrialMeasures , quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures , quote(nCustomers), trial_store)
head(magnitude_nSales)
head(magnitude_nCustomers)

##Create a combined score composed of correlation and magnitude, by first merging the correlations table with the magnitude table.

## merge the correlation and magnitude tables
merged_nSales <- merge(corr_nSales , magnitude_nSales, by = c("Store1", "Store2"))
merged_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))
head(merged_nSales)
head(merged_nCustomers)

# calculate the combined score for total sales
corr_weight <- 0.5
mag_weight <- 0.5

# calculate the combined score for total sales with A simple average on the scores would be 0.5 * corr_measure + 0.5 * mag_measure
merged_nSales[ , score_nSales := corr_weight*corr_measure + mag_weight*mag_measure]
head(merged_nSales)

# calculate the combined score for number of customers
merged_nCustomers[ , score_nCustomers := corr_weight*corr_measure + mag_weight*mag_measure]
head(merged_nCustomers)

## Combine scores across the drivers by first merging our sales scores and customer scores into a single table

score_Control <- merge(merged_nSales[, .(Store1, Store2, score_nSales)], merged_nCustomers[, .(Store1,Store2, score_nCustomers)], by = c("Store1", "Store2"))
score_Control[, finalControlScore := score_nSales * 0.5 + score_nCustomers * 0.5]
head(score_Control)

## Selecting the Store with the highest score as it would be the most similar to the trial store.

## Select control store based on the highest matching store (closest to 1 but not exactly 1)
control_store <- score_Control[Store1 == trial_store][order(-finalControlScore)][2, Store2]
paste(control_store)

##Now that we have found a control store, let's check visually if the drivers are indeed similar in the period before the trial.

## Total Sales :

##Visual checks on trends based on the drivers


str(measureOvertime)
## First as we see the month_ID is in chr instead of integer we have to convert it to integer

## Convert the month_ID column from character to integer
measureOvertime[, month_ID := as.integer(month_ID)]

## Check the data type of the month_ID column
str(measureOvertime$month_ID)

measureOverTimeSales <- copy(measureOvertime)

## Now we measure the past sales 
pastSales <- measureOverTimeSales[ , Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                          ifelse(STORE_NBR == control_store, "Control", "Other stores"))
                                   ][, totSales := mean(totSales), by = c("month_ID", 
                                                                          "Store_type")
                                     ][, TransactionMonth := as.Date(paste(month_ID %/% 100, month_ID %% 100, 1 , sep = "-"), "%Y-%m-%d")
                                       ][month_ID < 201903,]
## Displaying the tables created
head(pastSales)
str(pastSales)

## Now we will plot the trends based on the drivers in past sales
ggplot(pastSales , aes(TransactionMonth,  totSales , color =  Store_type)) + geom_line() + labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

## We can see how both of our selected trial and control store have sort of the same trend when we plot them together

## Number Of Customers :

measureOvertimeCusts <- copy(measureOvertime)

## Visual checks on customer count trends by comparing the trial store to the control store and other stores.

pastCustomers <- measureOvertimeCusts[ , Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                              ifelse(STORE_NBR == control_store, "Control", "Other stores"))
                                       ][, totSales := mean(totSales), by = c("month_ID", 
                                                                              "Store_type")
                                       ][, TransactionMonth := as.Date(paste(month_ID %/% 100, month_ID %% 100, 1 , sep = "-"), "%Y-%m-%d")
                                         ][month_ID < 201903,][order(TransactionMonth), ]


## Displaying the tables created
head(pastCustomers)
str(pastCustomers)

## Now we will plot the trends based on the drivers in past number of customers 
ggplot(pastCustomers , aes(TransactionMonth,  nCustomers , color =  Store_type)) + geom_line() + labs(x = "Month of operation", y = "Number of Customers", title = "Total Number of Customers by month")
ggplot(data = pastCustomers) + 
  geom_line(mapping = aes(x = TransactionMonth , y = nCustomers , color =  Store_type))

summary(pastCustomers[pastCustomers$Store_type == "Other stores", "nCustomers"])

## Assessment of trial

## Scaling the control store's sales to a level similar to control for any differences between the two stores outside of the trial period.

## Scale pre-trial control sales to match pre-trial trial store sale

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & month_ID < 201902 , sum(totSales)]/preTrialMeasures[STORE_NBR == control_store & month_ID < 201902 , sum(totSales)]

## Apply the scaling factor

scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store,
                                           ][ , controlSales := totSales * scalingFactorForControlSales]


## Calculating the percentage difference between scaled control sales and trial sales

percentageDiff <- merge( scaledControlSales[, c("month_ID", "controlSales")],
                         measureOvertime[STORE_NBR == trial_store, c("month_ID", "totSales")],
                         by = "month_ID"
                           )[ , percentageDiff := abs(controlSales - totSales)/controlSales]



## Taking the standard deviation as the first step to check our null hypothesis

stdDev <- sd(percentageDiff[month_ID < 201902 , percentageDiff])

## Note that there are 8 months in the pre-trial period as dof = n - 1, therefore  8 - 1 = 7 degrees of freedom.

degreesOfFreedom <- 7 

## Calculating the t-values for the trial months. 
## Note: The test statistic here is (x - u)/standard deviation
## The value 0 in the expression (percentageDiff - 0)/stdDev represents the null hypothesis that there is no difference between the trial and control stores.

percentageDiff[ , tValue := (percentageDiff - 0)/stdDev
                ][ , TransactionMonth := as.Date(paste(month_ID %/% 100,month_ID %% 100, 1,sep = "-"), "%Y-%m-%d")
                     ][month_ID < 201905 & month_ID > 201901, .(TransactionMonth, tValue)]

## Calculating the 95th percentile of the t-distribution with degrees of freedom to compare with our t-value

qt(0.95, df = degreesOfFreedom)

## Visualizing the sales of control store , sales of the trial stores and the 95th percentile value of sales of the control store.
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
                                  ][, totSales := mean(totSales), by = c("month_ID", "Store_type")
                                    ][, TransactionMonth := as.Date(paste(month_ID %/% 100, month_ID %% 100, 1, 
                                      sep = "-"), "%Y-%m-%d")][Store_type %in% c("Trial", "Control")]

## Control store 95th percentile
pastSales_Conrtols95 <- pastSales[Store_type == "Control", 
                                  ][, totSales := totSales * (1 + stdDev * 2)
                                    ][, Store_type := "Conrol 95th % confidence interval"]
head(pastSales_Conrtols95)

## Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
                                 ][, totSales := totSales * (1 - stdDev * 2)
                                   ][, Store_type := "Control 5th % confidence interval"]
head(pastSales_Controls5)

## Combining everything 
trialAssessment <- rbind(pastSales, pastSales_Conrtols95, pastSales_Controls5)
head(trialAssessment)

## Plotting everything in one graph 

ggplot(data = trialAssessment ,mapping = aes(x = TransactionMonth , y = totSales , color = Store_type) ) +
  geom_rect(data = trialAssessment[month_ID < 201905 & month_ID > 201901 ,],
            mapping = aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax = Inf, color  = NULL), show.legend = FALSE) +
  geom_line() + 
  labs(x = "Month of Operation", y = "Total sales" ,title =  "Total sales by month")

## Scaling for Number of Customers

scalingFactoForControlCust <- preTrialMeasures[STORE_NBR == trial_store & month_ID < 201902, sum(nCustomers)
                                               ]/preTrialMeasures[STORE_NBR == control_store & month_ID < 201902, sum(nCustomers)]

## Apply the scaling factor

scaledControlCustomers <- measureOvertimeCusts[STORE_NBR == control_store, 
                                               ][, controlCustomers := nCustomers*scalingFactoForControlCust]


## Calculate the percentage difference between scaled control sales and trial sales

percentageDiffCust <- merge(scaledControlCustomers[, c("month_ID", "controlCustomers")],
                            measureOvertimeCusts[STORE_NBR == trial_store, c("nCustomers", "month_ID")],
                            by = "month_ID")[, percentageDiff := abs(controlCustomers - nCustomers)/ controlCustomers]


# Let's again see if the difference is significant visually

## As our null hypothesis is that the trial period is the same as the pre-trial period,
## let's take the standard deviation based on the scaled percentage difference in the
## pre-trial period
stdDev2 <- sd(percentageDiffCust[month_ID < 201902, percentageDiff])

degreesOfFreedom <- 7

## Trial and control store number of customers
pastCustomers <- measureOvertimeCusts[, nCusts := mean(nCustomers), 
                                      by = c("month_ID", "Store_type")
                                      ][Store_type %in% c("Trial", "Control")]

## Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control", 
                                          ][, nCusts := nCusts * (1 + stdDev2*2)][, Store_type := "Control 95th % CI"]


## Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
                                         ][, nCusts := nCusts * (1 - stdDev2*2)][, Store_type := "Control 5th % CI"]

trialAssessmentCust <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

## Plotting these in one nice graph
ggplot(trialAssessmentCust, aes(x = TransactionMonth, y = nCusts, color = Store_type)) +
  geom_rect(data = trialAssessmentCust[month_ID < 201905 & month_ID > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth),
                ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() + labs(x = "Month of operation", y = "Total number of customers", 
                     title = "Total no. of customers per month")

## For Trial Store : 86

trial_store86 <- 86

## Calculating the correlations of total sales and number of customers 
corr_nSales86 <- calculateCorrelation( preTrialMeasures, quote(totSales), trial_store86) 
corr_nCustomers86 <- calculateCorrelation( preTrialMeasures, quote(nCustomers), trial_store86)

## Calculating the Normalized measure of total sales and number of customers 
magnitude_nSales86 <- calculateMagnitudeDistance( preTrialMeasures, quote(totSales), trial_store86)
magnitude_nCustomer86 <- calculateMagnitudeDistance( preTrialMeasures, quote(nCustomers), trial_store86)

## Creating a score based on the both correlation and normalization by taking a simple average 

## For Sales 

score_nSales86 <- merge(corr_nSales86, 
                        magnitude_nCustomer86, 
                        by = c("Store1", "Store2"))[, scoreNSales := (corr_measure * corr_weight) + 
                                                      (mag_measure * mag_weight)]


## For Number of Customers 

score_nCustomer86 <- merge(corr_nCustomers86, magnitude_nCustomer86, 
                           by = c("Store1", "Store2"))[, scoreNCust := (corr_measure * corr_weight) +
                                                         (mag_measure * mag_weight)]


## Combining the scores across all the drivers
score_Control86 <- merge(score_nSales86, 
                         score_nCustomer86, 
                         by = c("Store1", "Store2"))[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

## Selecting the control store for Trial store 86 by selecting the highest score but not the trial store itself

control_store86 <- score_Control86[Store1 == trial_store86, ][order(-finalControlScore)][2, Store2]

control_store86


## Visualizing the trends of Trial, Control & Other Stores

measureOverTimeSales86 <- copy(measureOvertime)

## For past Sales
pastSales86 <- measureOverTimeSales86[, Store_type := ifelse(STORE_NBR == trial_store86, "Trial",
                                                           ifelse(STORE_NBR == control_store86, "Control", "Other stores"))
                                    ][, totSales := mean(totSales),by = c("month_ID", "Store_type")
                                      ][, TransactionMonth := as.Date(paste(month_ID %/% 100, month_ID %% 100, 1, sep = "-"),"%Y-%m-%d")
                                        ][month_ID < 201903, ]

## Plotting the Sales trending
ggplot(pastSales86, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Months", y = "Total sales", 
       title = "Average total sales by month for trial store 86 and control store 155 for pre-trial period")


## For Number of Customers

measureOverTimeCusts86 <- copy(measureOvertime)

## For Number Customers 
pastCustomers86 <- measureOverTimeCusts86[, Store_type := ifelse(STORE_NBR == trial_store86, "Trial",
                                                               ifelse(STORE_NBR == control_store86, "Control", "Other stores"))
                                          ][, numberCustomers := mean(nCustomers), by = c("month_ID", "Store_type")
                                            ][, TransactionMonth := as.Date(paste(month_ID %/% 100, month_ID %% 100, 1, sep = "-"),"%Y-%m-%d")
                                              ][month_ID < 201903, ]

## Plotting
ggplot(pastCustomers86, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of Operation", y = "Number of customers", title = "Average number of customers by month for trail store 86 and control store 155 for the pre_trial preiod")

## Assessment of Trial store 86 

## Scaling the control stores

scalingFactorForControlSales86 <- preTrialMeasures[STORE_NBR == trial_store86 & 
                                                     month_ID < 201902, 
                                                   sum(totSales)
                                                  ]/preTrialMeasures[STORE_NBR == control_store86 & month_ID < 201902, sum(totSales)]

## Applying our scaling factor on control store sales

scaledControlSales86 <- measureOverTimeSales86[STORE_NBR == control_store86, 
                                             ][, controlSales := totSales * scalingFactorForControlSales86]

## Calculate the percentage difference between scaled control store sales and trial store sales
percentageDiff86 <- merge(scaledControlSales86[, c("month_ID", "controlSales")],
                          measureOvertime[STORE_NBR == trial_store86, c("month_ID", "totSales")],
                          by = "month_ID")[, percentageDiff := abs(controlSales - totSales)/controlSales]


## Taking the standard deviation

stdDevSales86 <- sd(percentageDiff86[ month_ID < 201902, percentageDiff])

## Getting all the past sales of Trial and Control Store
pastSales86 <- measureOverTimeSales86[, totSales := mean(totSales), by = c("month_ID", "Store_type")
                                        ][Store_type %in% c("Trial", "Control"), ]

## Control store 95th percentile
pastSales86_Controls95 <- pastSales86[Store_type == "Control",
                                      ][, totSales := totSales * (1 + stdDevSales86 * 2)
                                        ][, Store_type := "Control Store 95% confidence interval"]

## Control store 5th percentile
pastSales86_Controls5 <- pastSales86[Store_type == "Control",
                                     ][, totSales := totSales * (1 - stdDevSales86 * 2)
                                       ][, Store_type := "Control 5% confidence interval"]

## Binding together of pastSales86, Controls95, Controls5 together
trialAssessmentSales86 <- rbind(pastSales86, pastSales86_Controls5, pastSales86_Controls95)


## Plot these all in one graph
ggplot(trialAssessmentSales86, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessmentSales86[ month_ID < 201905 & month_ID > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth),
                ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Average total sales", title = "Average total sales per month")


## For Number of customers :

## Scaling pre-trial control store for number of customer to match pre-trial trial store.

scalingFactorForControlCust86 <- preTrialMeasures[STORE_NBR == trial_store86 & 
                                                    month_ID < 201902, 
                                                  sum(nCustomers)
                                                  ]/preTrialMeasures[STORE_NBR == control_store86 & month_ID < 201902, sum(nCustomers)]


## Applying scaling factor to the control stores for number of customers

scaledControlCustomers86 <- measureOverTimeCusts86[STORE_NBR == control_store86, 
                                                 ][, controlCustomers := nCustomers * scalingFactorForControlCust86]

## Calculate the percentage difference
percentageDiffCust86 <- merge(scaledControlCustomers86[, c("month_ID", "controlCustomers")],
                              measureOvertime[STORE_NBR == trial_store86, c("month_ID", "nCustomers")],
                              by = "month_ID")[, percentageDiff := abs(controlCustomers - nCustomers) / controlCustomers]

## Taking the standard deviation 

stdDevCust86 <- sd(percentageDiffCust86[month_ID < 201902, percentageDiff])

## Trial and control store number of customers
pastCustomers86 <- measureOverTimeCusts86[, nCusts := mean(nCustomers), by = c("month_ID", "Store_type")
                                        ][Store_type %in% c("Trial", "Control"), ]



#### Control 95th percentile
pastCustomers86_Controls95 <- pastCustomers86[Store_type == "Control",
                                              ][, nCusts := nCusts * (1 + (stdDevCust86 * 2))
                                                ][, Store_type := "Control Store 95% confidence interval"]

#### Control 5th percentile 
pastCustomers86_Controls5 <- pastCustomers86[Store_type == "Control",
                                             ][, nCusts := nCusts * (1 - (stdDevCust86 * 2))
                                               ][, Store_type := "Control Store 5% confidence interval"]

#### Row bind pastCustomers86, pastCustomers86_Controls95, pastCustomers_Controls5 
trialAssessmentCust86 <- rbind(pastCustomers86, pastCustomers86_Controls5, pastCustomers86_Controls95)

## Visualizing the trends of number of customers for trial store 86, control store 155 and other stores
ggplot(trialAssessmentCust86, aes(TransactionMonth, nCusts, color = Store_type)) + 
  geom_rect(data = trialAssessmentCust86[month_ID < 201905 & month_ID > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), 
                ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() + 
  labs(x = "Month", y = "Average number of customers", title = "Average number of customers per month")


## Trial Store 88 :

trial_store88 <- 88

## Calculating the correlations of total sales and number of customers
corr_nSales88 <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store88)
corr_nCustomers88 <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store88)

## Calculating the Normalized measure of total sales and number of customers
magnitude_nSales88 <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store88)
magnitude_nCustomers88 <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store88)


## Creating a score based on the both correlation and normalization by taking a simple average 

## For Sales
score_NSales88 <- merge(corr_nSales88, magnitude_nSales88, 
                        by = c("Store1", "Store2")
                        )[, scoreNSales := (corr_measure*corr_weight) +(mag_measure * mag_weight)]


## For Number of Customers
score_NCustomers88 <- merge(corr_nCustomers88, magnitude_nCustomers88, 
                            by = c("Store1", "Store2")
                            )[, scoreNCust :=(corr_measure * corr_weight) + (mag_measure * mag_weight)]

## Combining the scores across all the drivers
score_Control88 <- merge(score_NSales88, score_NCustomers88, by = c("Store1", "Store2")
                         )[, finalControlScore := (scoreNSales * 0.5) + (scoreNCust * 0.5)]

## Selecting the Control Store for trial store 86

## Selecting the control store for Trial store 86 by selecting the highest score but not the trial store itself
control_store88 <- score_Control88[order(-finalControlScore)][2, Store2]

control_store88

## Visualizing the trends of Trial, Control & Other Stores for Trial Store 88

measureOverTimeSales88 <- copy(measureOvertime)

## For past Sales
pastSales88 <- measureOverTimeSales88[, Store_type := ifelse(STORE_NBR == trial_store88, "Trial",
                                                             ifelse(STORE_NBR == control_store88, "Control", "Other stores"))
                                      ][, totSales := mean(totSales),by = c("month_ID", "Store_type")
                                        ][, TransactionMonth := as.Date(paste(month_ID %/% 100, month_ID %% 100, 1, sep = "-"),"%Y-%m-%d")
                                          ][month_ID < 201903, ]

## Plotting the Sales trending
ggplot(pastSales88, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Months", y = "Total sales", 
       title = "Average total sales by month for trial store 88 and control store 237 for pre-trial period")


## For Number of Customers

measureOverTimeCusts88 <- copy(measureOvertime)

## For Number Customers 
pastCustomers88 <- measureOverTimeCusts88[, Store_type := ifelse(STORE_NBR == trial_store88, "Trial",
                                                                 ifelse(STORE_NBR == control_store88, "Control", "Other stores"))
                                          ][, numberCustomers := mean(nCustomers), by = c("month_ID", "Store_type")
                                            ][, TransactionMonth := as.Date(paste(month_ID %/% 100, month_ID %% 100, 1, sep = "-"),"%Y-%m-%d")
                                              ][month_ID < 201903, ]

## Plotting
ggplot(pastCustomers88, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of Operation", y = "Number of customers", title = "Average number of customers by month for trail store 88 and control store 237 for the pre_trial preiod")

## Trial Assessment for trial store 88 :

## Scaling the control stores

scalingFactorForControlSales88 <- preTrialMeasures[STORE_NBR == trial_store88 & 
                                                     month_ID < 201902, 
                                                   sum(totSales)
                                                   ]/preTrialMeasures[STORE_NBR == control_store88 & month_ID < 201902, sum(totSales)]

## Applying our scaling factor on control store sales

scaledControlSales88 <- measureOverTimeSales88[STORE_NBR == control_store88, 
                                               ][, controlSales := totSales * scalingFactorForControlSales88]


## After scaling we are going to find out the percentage difference of the total sales of control and trial stores


## Calculate the percentage difference between scaled control store sales and trial store sales
percentageDiff88 <- merge(scaledControlSales88[, c("month_ID", "controlSales")],
                          measureOvertime[STORE_NBR == trial_store88, c("month_ID", "totSales")],
                          by = "month_ID")[, percentageDiff := abs(controlSales - totSales)/controlSales]


## Taking the standard deviation
stdDevSales88 <- sd(percentageDiff88[ month_ID < 201902, percentageDiff])

## Getting all the past sales of Trial and Control Store
pastSales88 <- measureOverTimeSales88[, totSales := mean(totSales), by = c("month_ID", "Store_type")
                                      ][Store_type %in% c("Trial", "Control"), ]

## Control store 95th percentile
pastSales88_Controls95 <- pastSales88[Store_type == "Control",
                                      ][, totSales := totSales * (1 + stdDevSales88 * 2)
                                        ][, Store_type := "Control Store 95% confidence interval"]

## Control store 5th percentile
pastSales88_Controls5 <- pastSales88[Store_type == "Control",
                                     ][, totSales := totSales * (1 - stdDevSales88 * 2)
                                       ][, Store_type := "Control 5% confidence interval"]

## Binding together of pastSales88, Controls95, Controls5 together
trialAssessmentSales88 <- rbind(pastSales88, pastSales88_Controls5, pastSales88_Controls95)

## Plot these all in one graph
ggplot(trialAssessmentSales88, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessmentSales88[ month_ID < 201905 & month_ID > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth),
                ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Average total sales", title = "Average total sales per month")


## Scaling pre-trial control store for number of customer to match pre-trial trial store.

scalingFactorForControlCust88 <- preTrialMeasures[STORE_NBR == trial_store88 & 
                                                    month_ID < 201902, 
                                                  sum(nCustomers)
                                                  ]/preTrialMeasures[STORE_NBR == control_store88 & month_ID < 201902, sum(nCustomers)]


## Applying scaling factor to the control stores for number of customers

scaledControlCustomers88 <- measureOverTimeCusts88[STORE_NBR == control_store88, 
                                                   ][, controlCustomers := nCustomers * scalingFactorForControlCust88]


## Calculate the percentage difference
percentageDiffCust88 <- merge(scaledControlCustomers88[, c("month_ID", "controlCustomers")],
                              measureOvertime[STORE_NBR == trial_store88, c("month_ID", "nCustomers")],
                              by = "month_ID")[, percentageDiff := abs(controlCustomers - nCustomers) / controlCustomers]

## Taking the standard deviation
stdDevCust88 <- sd(percentageDiffCust88[month_ID < 201902, percentageDiff])



## Trial and control store number of customers
pastCustomers88 <- measureOverTimeCusts88[, nCusts := mean(nCustomers), by = c("month_ID", "Store_type")
                                          ][Store_type %in% c("Trial", "Control"), ]



## Control 95th percentile
pastCustomers88_Controls95 <- pastCustomers88[Store_type == "Control",
                                              ][, nCusts := nCusts * (1 + (stdDevCust88 * 2))
                                                ][, Store_type := "Control Store 95% confidence interval"]

## Control 5th percentile 
pastCustomers88_Controls5 <- pastCustomers88[Store_type == "Control",
                                             ][, nCusts := nCusts * (1 - (stdDevCust88 * 2))
                                               ][, Store_type := "Control Store 5% confidence interval"]

## Row bind pastCustomers88, pastCustomers88_Controls95, pastCustomers88_Controls5 
trialAssessmentCust88 <- rbind(pastCustomers88, pastCustomers88_Controls5, pastCustomers88_Controls95)

## Visualizing the trends of number of customers for trial store 86, control store 155 and other stores

## Plotting the trends of number of customers for the trial period 
ggplot(trialAssessmentCust88, aes(TransactionMonth, nCusts, color = Store_type)) + 
  geom_rect(data = trialAssessmentCust88[month_ID < 201905 & month_ID > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), 
                ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() + 
  labs(x = "Month", y = "Average number of customers", title = "Average number of customers per month")



