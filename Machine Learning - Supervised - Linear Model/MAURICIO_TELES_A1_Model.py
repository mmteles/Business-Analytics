
# Student Name : MAURICIO TELES 
# Cohort       : COHORT 4 - DIVISADERO

# Note: You are only allowed to submit ONE final model for this assignment.


################################################################################
# Import Packages
################################################################################

# use this space for all of your package imports
import pandas as pd # data science essentials
import matplotlib.pyplot as plt # data visualization
import seaborn as sns # enhanced data visualization
import statsmodels.formula.api as smf # regression modeling
from sklearn.model_selection import train_test_split # train/test split
from sklearn.linear_model import Lasso # Lasso linear models
from sklearn.preprocessing import StandardScaler # standard scaler

################################################################################
# Load Data
################################################################################

# use this space to load the original dataset
# MAKE SURE TO SAVE THE ORIGINAL FILE AS original_df
# Example: original_df = pd.read_excel('Apprentice_Chef_Dataset.xlsx')

original_df = pd.read_excel('Apprentice_Chef_Dataset.xlsx')

################################################################################
# Feature Engineering and (optional) Dataset Standardization
################################################################################

# use this space for all of the feature engineering that is required for your
# final model

REVENUE_HI = 2300
AVG_TIME_PER_SITE_VISIT_HI = 250
TOTAL_MEALS_ORDERED_CHANGE_HI     = 300
MEDIAN_MEAL_RATING_CHANGE_HI      = 4

# REVENUE
original_df['OUT_REVENUE'] = 0
condition_hi = original_df.loc[0:,'OUT_REVENUE'][original_df['REVENUE'] > REVENUE_HI]

original_df['OUT_REVENUE'].replace(to_replace = condition_hi,
                                           value      = 1,
                                           inplace    = True)

# AVG_TIME_PER_SITE_VISIT
original_df['OUT_AVG_TIME_PER_SITE_VISIT'] = 0
condition_hi = original_df.loc[0:,'OUT_AVG_TIME_PER_SITE_VISIT'] \
                    [original_df['AVG_TIME_PER_SITE_VISIT'] > AVG_TIME_PER_SITE_VISIT_HI]

original_df['OUT_AVG_TIME_PER_SITE_VISIT'].replace(to_replace = condition_hi,
                                           value      = 1,
                                           inplace    = True)

# TOTAL_MEALS_ORDERED
original_df['CHANGE_TOTAL_MEALS_ORDERED'] = 0
condition = original_df.loc[0:,'CHANGE_TOTAL_MEALS_ORDERED'] \
                    [original_df['TOTAL_MEALS_ORDERED'] > TOTAL_MEALS_ORDERED_CHANGE_HI]

original_df['CHANGE_TOTAL_MEALS_ORDERED'].replace(to_replace = condition,
                                           value      = 1,
                                           inplace    = True)

# MEDIAN_MEAL_RATING
original_df['CHANGE_MEDIAN_MEAL_RATING'] = 0
condition = original_df.loc[0:,'CHANGE_MEDIAN_MEAL_RATING'] \
                    [original_df['MEDIAN_MEAL_RATING'] > MEDIAN_MEAL_RATING_CHANGE_HI]

original_df['CHANGE_MEDIAN_MEAL_RATING'].replace(to_replace = condition,
                                           value      = 1,
                                           inplace    = True)

# New columns based on independent variables
# WEEKLY_PLAN
original_df['FLAG_WEEKLY_PLAN'] = 0
condition = original_df.loc[0:,'FLAG_WEEKLY_PLAN'] \
                    [original_df['WEEKLY_PLAN'] != 0]

original_df['FLAG_WEEKLY_PLAN'].replace(to_replace = condition,
                                 value      = 1,
                                 inplace    = True)

# if your final model requires dataset standardization, do this here as well

# declaring set of x-variables
x_variables = ['TOTAL_MEALS_ORDERED',
               'UNIQUE_MEALS_PURCH',
               'PRODUCT_CATEGORIES_VIEWED',
               'AVG_PREP_VID_TIME',
               'LARGEST_ORDER_SIZE',
               'MASTER_CLASSES_ATTENDED',
               'MEDIAN_MEAL_RATING',
               'TOTAL_PHOTOS_VIEWED',
               'OUT_REVENUE',
               'OUT_AVG_TIME_PER_SITE_VISIT',
               'CHANGE_TOTAL_MEALS_ORDERED',
               'CHANGE_MEDIAN_MEAL_RATING',
               'FLAG_WEEKLY_PLAN']     
                
# preparing explanatory variable data
original_df_data   = original_df.loc[ : , x_variables]

# preparing response variable data
original_df_target = original_df.loc[:, 'REVENUE']

# INSTANTIATING a StandardScaler() object
scaler = StandardScaler()

# FITTING the scaler with housing_data
scaler.fit(original_df_data)

# TRANSFORMING our data after fit
X_scaled = scaler.transform(original_df_data)

# converting scaled data into a DataFrame
X_scaled_df = pd.DataFrame(X_scaled)

# checking the results
X_scaled_df.describe().round(2)

X_scaled_df.columns = original_df_data.columns

################################################################################
# Train/Test Split
################################################################################

# use this space to set up testing and validation sets using train/test split

# this is the exact code we were using before
X_train, X_test, y_train, y_test = train_test_split(
            X_scaled_df,
            original_df_target,
            test_size = 0.25,
            random_state = 222)

# Note: Be sure to set test_size = 0.25

################################################################################
# Final Model (instantiate, fit, and predict)
################################################################################

# use this space to instantiate, fit, and predict on your final model
mod = Lasso()
mod_fit = mod.fit(X_train, y_train)

# PREDICTING on new data
mod_pred = mod_fit.predict(X_test)

mod_score_test  = mod.score(X_test, y_test)

################################################################################
# Final Model Score (score)
################################################################################

# use this space to score your final model on the testing set
# MAKE SURE TO SAVE YOUR TEST SCORE AS test_score
# Example: test_score = final_model.score(X_test, y_test)

test_score = mod.score(X_test, y_test)

print(test_score)
