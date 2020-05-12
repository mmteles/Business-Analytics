
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
from sklearn.metrics import confusion_matrix         # confusion matrix
from sklearn.metrics import roc_auc_score            # auc score
from sklearn.ensemble import GradientBoostingClassifier # gbm


################################################################################
#   Creates variable to use as random_state
################################################################################

user_random_state = 222

################################################################################
# Load Data
################################################################################

# use this space to load the original dataset
# MAKE SURE TO SAVE THE ORIGINAL FILE AS original_df
# Example: original_df = pd.read_excel('Apprentice_Chef_Dataset.xlsx')

original_df = pd.read_excel('Apprentice_Chef_Dataset.xlsx')
chef = original_df

############################################################################## 
# STEP 1: splitting personal emails
##############################################################################

# placeholder list
placeholder_lst = []

# looping over each email address
for index, col in chef.iterrows():
    
    # splitting email domain at '@'
    split_email = chef.loc[index, 'EMAIL'].split(sep = '@')
    
    # appending placeholder_lst with the results
    placeholder_lst.append(split_email)
    
# converting placeholder_lst into a DataFrame 
email_df = pd.DataFrame(placeholder_lst)

##############################################################################
# STEP 2: concatenating with original DataFrame
##############################################################################
# renaming column to concatenate
email_df.columns = ['0' , 'personal_email_domain']

# concatenating personal_email_domain with friends DataFrame
chef = pd.concat([chef, email_df['personal_email_domain']],
                     axis = 1)

##############################################################################
# email domain types
##############################################################################
personal_email_domains = ['@gmail.com', '@yahoo.com', '@protonmail.com']
junk_email_domains  = ['@me.com', '@aol.com', '@hotmail.com', '@live.com', '@msn.com', '@passport.com']
professional_email_domains = ['@mmm.com', '@amex.com', '@apple.com', '@boeing.com', 
                              '@caterpillar.com', '@chevron.com', '@cisco.com', '@cocacola.com',
                              '@disney.com', '@dupont.com', '@exxon.com', '@ge.org', '@goldmansacs.com', 
                              '@homedepot.com', '@ibm.com', '@intel.com', '@jnj.com', '@jpmorgan.com', 
                              '@mcdonalds.com', '@merck.com', '@microsoft.com', '@nike.com', 
                              '@pfizer.com', '@pg.com', '@travelers.com', '@unitedtech.com', 
                              '@unitedhealth.com', '@verizon.com', '@visa.com', '@walmart.com']

# placeholder list
placeholder_lst = []

# looping to group observations by domain type
for domain in chef['personal_email_domain']:
    
    if '@' + domain in personal_email_domains:
        placeholder_lst.append('personal')
        
    elif '@' + domain in junk_email_domains:
        placeholder_lst.append('junk')

    elif '@' + domain in professional_email_domains:
        placeholder_lst.append('professional')

    else:
            print('Unknown')

# concatenating with original DataFrame
chef['domain_group'] = pd.Series(placeholder_lst)

# one hot encoding categorical variables
one_hot_domain_group = pd.get_dummies(chef['domain_group'])

# dropping categorical variables after they've been encoded
chef = chef.drop('domain_group', axis = 1)

# joining codings together
chef = chef.join([one_hot_domain_group])

################################################################################
# Feature Engineering and (optional) Dataset Standardization
################################################################################

# use this space for all of the feature engineering that is required for your
# final model

##############################################################################
# setting outlier thresholds
##############################################################################

CANCELLATIONS_BEFORE_NOON_HI = 1    # More than 1 cancellation before noon
CANCELLATIONS_AFTER_NOON_HI = 0     # Has cancellations after noon

##############################################################################
## Feature Engineering - create flags for intervals for continuous data     ##
##############################################################################
## function to handle the data                                              ##
##############################################################################
def flag_columns (column, hi_cut, low_cut = 0):
    chef['FLAG_'+column] = 0
    condition_hi = chef.loc[0:,'FLAG_'+column][chef[column] > hi_cut]
    condition_lo = chef.loc[0:,'FLAG_'+column][chef[column] < low_cut]
    chef['FLAG_'+column].replace(to_replace = condition_hi,
                                               value      = 1,
                                               inplace    = True)
    chef['FLAG_'+column].replace(to_replace = condition_lo,
                                               value      = 1,
                                               inplace    = True)

flag_columns('CANCELLATIONS_BEFORE_NOON', CANCELLATIONS_BEFORE_NOON_HI)
flag_columns('CANCELLATIONS_AFTER_NOON', CANCELLATIONS_AFTER_NOON_HI)

# declaring set of x-variables
##############################################################################
# Creates the dictionary
##############################################################################
candidate_dict = {
# significant variables only
 'logit_sig'    : ['MOBILE_NUMBER',
                    'TASTES_AND_PREFERENCES',
                    'PC_LOGINS',
                    'FOLLOWED_RECOMMENDATIONS_PCT',
                    'junk',
                    'professional',
                    'FLAG_CANCELLATIONS_BEFORE_NOON',
                    'FLAG_CANCELLATIONS_AFTER_NOON']
}
##############################################################################
# Prepare the dataframe with the significant variables to run with the models
##############################################################################

chef_data   = chef.loc[ : , candidate_dict['logit_sig']]

# preparing response variable data
chef_target = chef.loc[:, 'CROSS_SELL_SUCCESS']

################################################################################
# Train/Test Split
################################################################################

# use this space to set up testing and validation sets using train/test split


# preparing training and testing sets
X_train, X_test, y_train, y_test = train_test_split(
            chef_data,
            chef_target,
            test_size = 0.25,
            random_state = user_random_state)

# Note: Be sure to set test_size = 0.25

################################################################################
# Final Model (instantiate, fit, and predict)
################################################################################

# use this space to instantiate, fit, and predict on your final model
mod = GradientBoostingClassifier(loss          = 'deviance',
                                  learning_rate = 0.1,
                                  n_estimators  = 100,
                                  criterion     = 'friedman_mse',
                                  max_depth     = 3,
                                  warm_start    = False,
                                  random_state  = user_random_state)

mod_fit = mod.fit(X_train, y_train)

# PREDICTING on new data
mod_pred = mod_fit.predict(X_test)

mod_score_train = mod.score(X_train, y_train).round(4)
mod_score_test  = mod.score(X_test, y_test).round(4)
mod_auc_score   = roc_auc_score(y_true  = y_test, y_score = mod_pred).round(4)
mod_con_matrix = confusion_matrix(y_true = y_test, y_pred = mod_pred)

################################################################################
# Final Model Score (score)
################################################################################

# use this space to score your final model on the testing set
# MAKE SURE TO SAVE YOUR TEST SCORE AS test_score
# Example: test_score = final_model.score(X_test, y_test)

test_score = mod_auc_score

print(test_score)
