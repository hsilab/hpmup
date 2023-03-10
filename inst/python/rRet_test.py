import numpy as np
import pickle
import pandas as pd

# load the model from disk
# loaded_model = pickle.load(open('inst/python/nb_cluster_2_K-Best_13_3470851.sav', 'rb'))
# loaded_model = pickle.load(open('inst/python/nb_7_cluster_2_FFS_5_2805833.sav', 'rb'))       # this path is for HPM-UP

# NB
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_14_cluster_2_RFE_1_9474401.sav', 'rb'))       # row id 1
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_13_cluster_2_RFE_3_974462.sav', 'rb'))       # row id 3
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_11_cluster_2_RFE_1_6366753.sav', 'rb'))       # row id 11
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_10_cluster_2_RFE_2_799293.sav', 'rb'))       # row id 2
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_9_cluster_2_RFE_1_4716904.sav', 'rb'))       # row id 12
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_8_cluster_2_RFE_1_8580767.sav', 'rb'))       # row id 13
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_6_cluster_2_RFE_7_5391527.sav', 'rb'))       # row id 7
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_4_cluster_2_K-Best_2_3085516.sav', 'rb'))       # row id 15
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_3_cluster_2_FFS_5_9766400.sav', 'rb'))       # row id 311
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_12_cluster_2_FFS_6_9964514.sav', 'rb'))       # row id 32  this path is for independent testing: 0.7037
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_7_cluster_2_FFS_5_2805833.sav', 'rb'))       # row id 31  this path is for independent testing: 0.963
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_5_cluster_2_RFE_13_3320592.sav', 'rb'))       # row id 131  this path is for independent testing: 0.778
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_2_cluster_2_FFS_5_2831635.sav', 'rb'))       # row id 312   this path is for independent testing: 0.8148 - this was 60%
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_1_cluster_2_RFE_9_4836313.sav', 'rb'))        # row id 9  this path is for independent testing: 0.7778
loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/nb_0_cluster_2_K-Best_4_9156777.sav', 'rb'))       # row id 17  this path is for independent testing: 0.8889

# RF
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_14_cluster_2_FFS_1_4274905.sav', 'rb'))  # row 27
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_13_cluster_2_RFE-RandomForestClassifier()_2_612540.sav', 'rb'))  # row 2
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_12_cluster_2_K-Best_5_4865731.sav', 'rb'))  # row 18
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_11_cluster_2_K-Best_1_7920952.sav', 'rb'))  # row 14
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_10_cluster_2_K-Best_1_6692870.sav', 'rb'))  # row 141
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_9_cluster_2_K-Best_2_1056087.sav', 'rb'))  # row 15
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_8_cluster_2_RFE-RandomForestClassifier()_1_2490760.sav', 'rb'))  # row 1
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_7_cluster_2_RFE-RandomForestClassifier()_1_5531967.sav', 'rb'))  # row 11
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_6_cluster_2_K-Best_12_937844.sav', 'rb'))  # row 25
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_5_cluster_2_RFE-RandomForestClassifier()_2_1833099.sav', 'rb'))  # row 21
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_4_cluster_2_K-Best_1_5241828.sav', 'rb'))  # row 142
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_3_cluster_2_RFE-RandomForestClassifier()_2_2769050.sav', 'rb'))  # row 22
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_2_cluster_2_K-Best_6_4990576.sav', 'rb'))  # row 19
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_1_cluster_2_K-Best_4_5066342.sav', 'rb'))  # row 17
# loaded_model = pickle.load(open('D:/JP_project_test/hpmpd_v1.1/inst/python/rf_0_cluster_2_K-Best_7_7697635.sav', 'rb'))  # row 143




print("--- Load the model is ok ---")

def predictSingle(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, CC, DC, PR):
    # print("Testing Started.")
    data = {'pcps': [pcps],
        'blink': [blink],
        'perf': [perf],
        'tct_cycle': [tct_cycle],
        'nC': [nC],
        'nP': [nP],
        'nM': [nM],
        'tct_CPM': [tct_CPM],
        'mem_load': [mem_load],
        'training': [training],
        'CC': [CC],
        'DC': [DC],
        'PR': [PR]}
    df = pd.DataFrame(data)
    # print("Testing Completed.")
    return (loaded_model.predict(df))

print("--- Read rRet_test is ok ---")

