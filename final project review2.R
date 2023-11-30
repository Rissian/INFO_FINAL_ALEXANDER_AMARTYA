#
# 0. loading libraries
#

library(dplyr)
library(stringr)
library(ggplot2)
library(testthat)
library(tidyverse)


#
# 1. load datasets
#
income_df <- read.csv("ACSST1Y2022.S1901-2023-11-28T011511.csv") 
pollution_df <- read.csv("pollution_2000_2022.csv") 


# 2. simplyfy dataframes


# (1)
#
income_df <- income_df[!grepl('Margin', income_df$Family),]
income_df <- income_df[grepl('Households', income_df$Family),]
income_df <- subset(income_df, select = -c(X, Household.income.in.the.past.12.months, Family.income.in.the.past.12.months, Nonfamily.income.in.the.past.12.months))
  
#
income_df$State <- str_extract(income_df$Family, paste(state.name, collapse = '|'))
income_df <- subset(income_df, select = -c(Family))
View(income_df) 

#
# 2.

pollution_df <- pollution_df[grepl('2022', pollution_df$Date),]
pollution_df <- subset(pollution_df, select = -c(X, Address, O3.1st.Max.Hour, CO.1st.Max.Hour, SO2.1st.Max.Hour, NO2.1st.Max.Hour))
#pollution_df <- select(pollution_df, c(State, CO.Mean  )) 
View(pollution_df)

#
# 3. creating new parameters on pollution data

co_df<- select(pollution_df, c(State, CO.Mean  )) 
co_df<- group_by(co_df, State)
co_df<- summarise(co_df, AverageCO2=mean(CO.Mean ) )
co_df<- mutate(co_df, "high_pollution"=
                 ifelse(AverageCO2>quantile(AverageCO2, .5), "Yes", "No")
)  
View(co_df)
table(co_df$high_pollution)


pollution_df <- mutate(pollution_df, "high_pollution"=
                         ifelse(CO.Mean>quantile(CO.Mean, .5), "Yes", "No")
)



#
# 4. creating new parameters on income data

income_df <- mutate(
  income_df,
  Prop_Households_below25K =
    as.numeric(str_replace(income_df$"Less.than..10.000", "%$", ""))+
    as.numeric(str_replace(income_df$"X.10.000.to..14.999", "%$", ""))+
    as.numeric(str_replace(income_df$"X.15.000.to..24.999", "%$", ""))
)
#
View(income_df)

#
household_income_summary <- select(income_df, c(State,  Prop_Households_below25K))
View(household_income_summary)


#
# 5. 
#
df <- left_join(income_df, pollution_df, by= 'State')
View(df)





#write.csv(df, "C:\\Users\\Enquero User\\Documents\\R\\final_project_df_two.csv")
