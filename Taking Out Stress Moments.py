#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

@author: mahnooshsadeghi
"""


####taking out stress moments for everyone in windows of 2 minutes

import pandas as pd
import glob

path = r'/Users/mahnooshsadeghi/Desktop/PTSD/PTSD Data/Kalman Imputed Data' # use your path
all_files = glob.glob(path + "/*.csv")
    

import os
os.getcwd()
os.chdir('/Users/mahnooshsadeghi/Desktop/PTSD/PTSD Data/Stress Moments after Imputation') 

windownumber=1
participant=1

for filename in all_files:
    slidingstress=pd.DataFrame()
    T=pd.DataFrame()
    l=df.index[df['ptsd_moment'] == 'STRESSMOMENT'].tolist()
    for i in l:
        #df = pd.read_csv(filename, index_col=None, header=0)
        T= df[i-60:i+60]
        T['windowNo']=windownumber
        T['participant']=participant
        slidingstress=pd.concat([T,slidingstress])
        windownumber=windownumber+1
        name= str(participant)
    slidingstress.to_csv(participant.csv)
    participant=participant+1
