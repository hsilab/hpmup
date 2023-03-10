# Reference: https://blog.naver.com/PostView.nhn?isHttpsRedirect=true&blogId=gdpresent&logNo=221730873049&redirect=Dlog&widgetTypeCall=true&directAccess=false
# Step 1: Importing packages
import math 
import random
import numpy as np 
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)
import pandas as pd 
import winsound # Need alert notification to program completion 
import time # Using timer to measure duration of cross validation 
# Model selection importing 
from sklearn.model_selection import train_test_split # This function handles splitting of the data into training & evaluating sets 
from sklearn.model_selection import KFold, GridSearchCV
# Data preprocessing importing 
from sklearn.preprocessing import MinMaxScaler, StandardScaler # Scales data to fit within the training data min & max  
# Different means of feature selection imported 
from sklearn.feature_selection import RFE, RFECV # Iteratively eliminates non-contributory features 
# from sklearn.feature_selection import chi2
from sklearn.feature_selection import SelectKBest, mutual_info_classif # f_classif
# from sklearn.feature_selection import SelectFromModel
from sklearn.feature_selection import SequentialFeatureSelector as SFS
# Miscellaneous importing 
from sklearn.pipeline import Pipeline
from sklearn.tree import DecisionTreeClassifier 
from sklearn.ensemble import RandomForestClassifier 
from sklearn.metrics import roc_auc_score, f1_score, precision_score, recall_score
from sklearn.svm import SVC 
from joblib import parallel_backend

print("Sourcing Started.")

def preprocess(dfx):
    df = dfx.copy()   
    # Deleting some columns from the data
    # It loads selected columns into the new data set by negating the columns listed in the input of the function
    new_df = df[df.columns[~df.columns.isin(['TLX_raw', 'cluster_2', 'cluster_3', 'cluster_4', 'hist_4', 'nC', 'nP', 'nM', 'tct_CPM', 'mem_load'])]]    
    # Returns the new data frame as the output of the called function preprocess(dfx)
    return new_df
  
#inputFile =  'C:/Users/Kyle Crusius/Desktop/MLCode/clusters_final_CRT.csv'
#outputFile = 'C:/Users/Kyle Crusius/Desktop/MLCode/SVM_11_CRT_empty.csv'
#config to training in csv data
X_test = []
y_test = []
gm1 = 0
gm2 = 0
gm3 = 0
gm4 = 0
def runTraining(inputFile, outputFile):
    print("Training Started.")
    global X_test
    global y_test
    global gm1, gm2, gm3, gm4
    with parallel_backend("threading", n_jobs= 1): # to stop parallel threading

        targets = ['cluster_2', 'cluster_3', 'cluster_4', 'hist_4']  # Runs all 4 target variables

        for j in range(0,4):
            if j==0: 
                print("cluster_2")
            elif j==1: 
                print("cluster_3")
            elif j==2: 
                print("cluster_4")
            else: 
                print("hist_4") 
            # Step 2: Loading and splitting the data


            # Note this was the filepath used locally at home 
            # filepath = "\Documents"
            # Issue with switching to the 'clusers_that_you_need_to_use' is that there's CATEGORICAL data in it instead of 
            # continuous numerical data 
            crt_raw = pd.read_csv(inputFile)

            # Adjust categorical variables with one-hot encoding 
            df = pd.get_dummies(crt_raw['config']) 
            crt_raw = crt_raw.drop('config', axis = 1) 
            crt_raw = crt_raw.join(df)
            # print(crt_raw) # Verify success of one-hot
            
            # Prepping data
            X = preprocess(crt_raw)
            target_cluster = targets[j]
            y = crt_raw[target_cluster].copy()


            # The below section: Divides the data into features (x) & targets (y) in a 70-30 ratio. 
            X_train, X_test, y_train, y_test = train_test_split( X, y, test_size=0.3, random_state=2)
            # The below code outputs characteristic shapes of the above synthesized variables
            # print(X_train.shape, X_test.shape, y_train.shape, y_test.shape)
            # print(crt_raw['config'])
            # print(y)
            
            # Defines the estimator for use in the RFE & FFS feature selection methods
            # Exactly the same as the base_model, but defined differently to preserve 
            # code structure for later modeling 
            estimator_SVC = SVC(random_state=0, probability = True, kernel = 'linear')
            estimator_SVC.fit(X_train, y_train)
            
            for i in range(1, 2, 1): 
                if i == 0: 
                    print("===RFE===")
                    for v in range(1, 9, 1): # RFECV feature selection over 13 features (1 to 14)  
                        # Step 3a: Feature selection via RFE only 

                        # The below function selects features via RECURSIVE FEATURE ELIMINATION
                        slct_rfe = RFE(estimator=estimator_SVC, n_features_to_select = v) 

                        # Step 3b: Building a PIPELINE

                        # Scaling of data to fit within boundaries -- more necessary for things like NN & SVM than random forest
                        # i.e. Random forest is invariant to scaling 
                        # This is like normalizing the numerical data 
                        scaler = StandardScaler()

                        # Classifier -- baseline classification divisions from which the algorithm iteratively moves forwards; change model type here 
                        base_model = SVC(random_state=0, probability = True, kernel = 'linear')

                        # Pipeline -- This is the random forest model, streamlined into one place so we can change it by minimal code 
                        pipe = Pipeline([('scaler', scaler)
                                        , ('feature_selector', slct_rfe)                 
                                        , ('base_model', base_model)])

                        # Step 4: Building cross-validation rule and a grid for hyperparameter tuning
                        kfold = KFold(n_splits = 10, shuffle=True, random_state=33) # False
                        regularization_param = [0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
                        
                        param_grid ={
                            'base_model__C' : regularization_param, # Iterates over regularization values
                            # 'base_model__gamma' : [0.001, 0.01, 0.1] # Iterates over gamma values
                            # 'base_model__kernel' : ['xxxxx'] # This would iterate through different kernels
                                    # Not necessary as long as kernel is specified 
                        }

                        # Step 5: Running grid search -- pulls the model & the hyperparameters together to evaluate optimal HP settings
                        startTime = time.time()
                        grid_model = GridSearchCV(estimator=pipe,
                                                param_grid=param_grid,
                                                cv=kfold,
                                                #iid=True,
                                                n_jobs=-1).fit(X_train, y_train)

                        gridSearchTime = time.time() - startTime
                        print('Time to complete grid search: ' + str(gridSearchTime) + ' seconds')
                        #print('Time to complete grid search: ' + str(gridSearchTime/60) + ' minutes')
                        #winsound.Beep(440,150) # Chime for completion

                        # Printing results of cross-validation, optimal hyperparameters for training, and testing accuracy of the program 
                        cv_score = grid_model.best_score_
                        op_hyperparameters = grid_model.best_params_
                        train_accuracy = grid_model.score(X_train, y_train)
                        test_accuracy = grid_model.score(X_test, y_test)
                        if target_cluster == 'cluster_2': 
                            auc = roc_auc_score(y_test, grid_model.predict_proba(X_test)[::, 1], average = 'macro', multi_class = 'ovr') # [::, 1]
                        else: 
                            auc = roc_auc_score(y_test, grid_model.predict_proba(X_test), average = 'macro', multi_class = 'ovr') # [::, 1]
                        f1 = f1_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)
                        precision = precision_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)
                        recall = recall_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)

                        # Calculate feature importances 
                        svc = base_model
                        svc.fit(X_train, y_train)
                        coefs = svc.coef_ 
                        finalFeatureIndices = grid_model.best_estimator_.named_steps["feature_selector"].get_support(indices=True)
                        sel_features = X.columns[finalFeatureIndices]

                        # Writing algorithm for data organization
                        currentData = pd.read_csv(outputFile)
                        # Overwriting each time with data frame adds excess row -- delete before rewrite
                        currentData = currentData.drop('Unnamed: 0', 1)
                        
                        newData = [target_cluster, 'RFE', len(X.columns[finalFeatureIndices]), op_hyperparameters['base_model__C'], 
                        cv_score, op_hyperparameters, train_accuracy, test_accuracy, auc, f1, precision, recall]
                        for k in range(0, 8, 1):
                            newData.append(coefs[0, k])
                        for k in range(0, 8, 1):
                            newStr = currentData.columns[20 + k]
                            if any(newStr[5:] in s for s in sel_features):
                                newData.append(1)
                            else: 
                                newData.append(0)

                        # Writing to the data frame
                        currentData.loc[len(currentData.index)] = newData

                        # Now I just need to send it back to the csv
                        currentData.to_csv(outputFile)

                elif i == 1: 
                    print("===KBEST===")
                    for v in range(1, 9, 1): # 1, 14
                        # Step 3a: Feature selection via KBest method

                        # The below function selects features based on the k-best scoring by the metric selected 
                        slct_KB = SelectKBest(mutual_info_classif, k=v) 

                        # Step 3b: Building a PIPELINE

                        # Scaling of data to fit within boundaries -- more necessary for things like NN & SVM than random forest
                        # i.e. Random forest is invariant to scaling 
                        # This is like normalizing the numerical data 
                        scaler = StandardScaler()

                        # Classifier -- baseline classification divisions from which the algorithm iteratively moves forwards; change model type here 
                        base_model = SVC(random_state=0, probability = True, kernel = 'linear')

                        # Pipeline -- This is the random forest model, streamlined into one place so we can change it by minimal code 
                        pipe = Pipeline([('scaler', scaler)
                                        , ('feature_selector', slct_KB)                 
                                        , ('base_model', base_model)])

                        # Step 4: Building cross-validation rule and a grid for hyperparameter tuning
                        kfold = KFold(n_splits = 10, shuffle=True, random_state=33) # False
                        regularization_param = [0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
                        
                        param_grid ={
                            'base_model__C' : regularization_param, # Iterates over regularization values
                            # 'base_model__gamma' : [0.001, 0.01, 0.1] # Iterates over gamma values
                            # 'base_model__kernel' : ['xxxxx'] # This would iterate through different kernels
                                    # Not necessary as long as kernel is specified 
                        }

                        # Step 5: Running grid search -- pulls the model & the hyperparameters together to evaluate optimal HP settings
                        startTime = time.time()
                        grid_model = GridSearchCV(estimator=pipe,
                                                param_grid=param_grid,
                                                cv=kfold,
                                                #iid=True,
                                                n_jobs=-1).fit(X_train, y_train)

                        gridSearchTime = time.time() - startTime
                        print('Time to complete grid search: ' + (str(round(gridSearchTime,2))+"0")[0:4] + ' seconds')
                        #print('Time to complete grid search: ' + str(gridSearchTime/60) + ' minutes')
                        #winsound.Beep(440,750) # Chime for completion

                        # Printing results of cross-validation, optimal hyperparameters for training, and testing accuracy of the program 
                        cv_score = grid_model.best_score_
                        op_hyperparameters = grid_model.best_params_
                        train_accuracy = grid_model.score(X_train, y_train)
                        test_accuracy = grid_model.score(X_test, y_test)
                        if target_cluster == 'cluster_2': 
                            auc = roc_auc_score(y_test, grid_model.predict_proba(X_test)[::, 1], average = 'macro', multi_class = 'ovr') # [::, 1]
                        else: 
                            auc = roc_auc_score(y_test, grid_model.predict_proba(X_test), average = 'macro', multi_class = 'ovr') # [::, 1]
                        f1 = f1_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)
                        precision = precision_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)
                        recall = recall_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)

                        # Calculate feature importances 
                        svc = base_model
                        svc.fit(X_train, y_train)
                        coefs = svc.coef_ 
                        finalFeatureIndices = grid_model.best_estimator_.named_steps["feature_selector"].get_support(indices=True)
                        sel_features = X.columns[finalFeatureIndices]

                        # Writing algorithm for data organization
                        currentData = pd.read_csv(outputFile)
                        # Overwriting each time with data frame adds excess row -- delete before rewrite
                        currentData = currentData.drop('Unnamed: 0', 1)
                        
                        newData = [target_cluster, 'K-Best', len(X.columns[finalFeatureIndices]), op_hyperparameters['base_model__C'], 
                        cv_score, op_hyperparameters, train_accuracy, test_accuracy, auc, f1, precision, recall]
                        for k in range(0, 8, 1):
                            newData.append(coefs[0, k])
                        for k in range(0, 8, 1):
                            newStr = currentData.columns[20 + k]
                            if any(newStr[5:] in s for s in sel_features):
                                newData.append(1)
                            else: 
                                newData.append(0)

                        # Writing to the data frame
                        currentData.loc[len(currentData.index)] = newData

                        # Now I just need to send it back to the csv
                        currentData.to_csv(outputFile)
                        
                        #Save gridmodel to global
                        if(j == 0):
                            gm1 = grid_model
                        elif(j == 1):
                            gm2 = grid_model
                        elif(j == 2):
                            gm3 = grid_model
                        else:
                            gm4 = grid_model

                elif i == 2: 
                    print("===FFS===")
                    for v in range(1, 8, 1): # FFS cannot perform feature selection for a specified 13 features(i.e. max) --> Thus excluded 
                        # 1 to 13
                        # Step 3a: Feature selection via FFS method only 

                        # Forward feature selection
                        slct_ffs = SFS(estimator_SVC, n_features_to_select = v, direction = 'forward', cv = 2, n_jobs = -1) # sklearn version of SFS

                        # Step 3b: Building a PIPELINE

                        # Scaling of data to fit within boundaries -- more necessary for things like NN & SVM than random forest
                        # i.e. Random forest is invariant to scaling 
                        # This is like normalizing the numerical data 
                        scaler = StandardScaler()

                        # Classifier -- baseline classification divisions from which the algorithm iteratively moves forwards; change model type here 
                        base_model = SVC(random_state=0, probability = True, kernel = 'linear')

                        # Pipeline -- This is the random forest model, streamlined into one place so we can change it by minimal code 
                        pipe = Pipeline([('scaler', scaler)
                                        , ('feature_selector', slct_ffs)                 
                                        , ('base_model', base_model)])

                        # Step 4: Building cross-validation rule and a grid for hyperparameter tuning
                        kfold = KFold(n_splits = 10, shuffle=True, random_state=33) # False
                        regularization_param = [0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
                        
                        param_grid ={
                            'base_model__C' : regularization_param, # Iterates over regularization values
                            # 'base_model__gamma' : [0.001, 0.01, 0.1] # Iterates over gamma values
                            # 'base_model__kernel' : ['xxxxx'] # This would iterate through different kernels
                                    # Not necessary as long as kernel is specified 
                        }

                        # Step 5: Running grid search -- pulls the model & the hyperparameters together to evaluate optimal HP settings
                        startTime = time.time()
                        grid_model = GridSearchCV(estimator=pipe,
                                                param_grid=param_grid,
                                                cv=kfold,
                                                #iid=True,
                                                n_jobs=-1).fit(X_train, y_train)

                        gridSearchTime = time.time() - startTime
                        print('Time to complete grid search: ' + str(gridSearchTime) + ' seconds')
                        #print('Time to complete grid search: ' + str(gridSearchTime/60) + ' minutes')
                        #winsound.Beep(440,750) # Chime for completion

                        # Printing results of cross-validation, optimal hyperparameters for training, and testing accuracy of the program 
                        cv_score = grid_model.best_score_
                        op_hyperparameters = grid_model.best_params_
                        train_accuracy = grid_model.score(X_train, y_train)
                        test_accuracy = grid_model.score(X_test, y_test)
                        if target_cluster == 'cluster_2': 
                            auc = roc_auc_score(y_test, grid_model.predict_proba(X_test)[::, 1], average = 'macro', multi_class = 'ovr') # [::, 1]
                        else: 
                            auc = roc_auc_score(y_test, grid_model.predict_proba(X_test), average = 'macro', multi_class = 'ovr') # [::, 1]
                        f1 = f1_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)
                        precision = precision_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)
                        recall = recall_score(y_test, grid_model.predict(X_test), average = 'macro', zero_division = 0)

                        # Calculate feature importances 
                        svc = base_model
                        svc.fit(X_train, y_train)
                        coefs = svc.coef_ 
                        finalFeatureIndices = grid_model.best_estimator_.named_steps["feature_selector"].get_support(indices=True)
                        sel_features = X.columns[finalFeatureIndices]

                        # Writing algorithm for data organization
                        currentData = pd.read_csv(outputFile)
                        # Overwriting each time with data frame adds excess row -- delete before rewrite
                        currentData = currentData.drop('Unnamed: 0', 1)
                        
                        newData = [target_cluster, 'FFS', len(X.columns[finalFeatureIndices]), op_hyperparameters['base_model__C'], 
                        cv_score, op_hyperparameters, train_accuracy, test_accuracy, auc, f1, precision, recall]
                        for k in range(0, 8, 1):
                            newData.append(coefs[0, k])
                        for k in range(0, 8, 1):
                            newStr = currentData.columns[20 + k]
                            if any(newStr[5:] in s for s in sel_features):
                                newData.append(1)
                            else: 
                                newData.append(0)

                        # Writing to the data frame
                        currentData.loc[len(currentData.index)] = newData

                        # Now I just need to send it back to the csv
                        currentData.to_csv(outputFile)
        
        # End of program
        print("Training Completed")
        print()

def predictSingle(a, b, c, d, e, f, g, h):
    print("Testing Started.")
    dataList = [a, b, c, d, e, f, g, h]
    Kyle_X = np.array([dataList])
    Kyle_X.reshape(-1,1)
    print("Testing Completed.")
    return (gm1.predict(Kyle_X)[0],gm2.predict(Kyle_X)[0],gm3.predict(Kyle_X)[0],gm4.predict(Kyle_X)[0])

print("Sourcing Completed.")

# testList = np.array([[0.1371,11,3.809,8,2,7,6.4,1.7]])
# #testList.reshape(-1,1)        
# runTraining('data/clusters_final_CRT.csv','data/SVM_11_CRT_empty.csv')
# #print(testList[0][0])
# #print(gm1.predict(testList)[0])
# #Example Usage
# print(predictSingle([0.1371,11,3.809,8,2,7,6.4,1.7]))
# return predictSingle([0.1371,11,3.809,8,2,7,6.4,1.7])

#Example Function for R
#
#
#library(reticulate)
#py_install("pandas") # can use py_install for missing libraries
#py_install("sklearn")
#reticulate::source_python(system.file("python","rRet.py",package = "hpmpd")) # to run the python code
#runTraining(inputFile,outputFile)
#predictSingle(eightMemberList)
#
    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
