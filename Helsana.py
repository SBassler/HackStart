#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 19 22:09:24 2021

@author: bassler
"""

import subprocess
import time
import json
from pprint import pprint
#import tifffile as tiff
import PIL.Image as Image
import numpy as np
import cv2
from os import listdir
from os.path import isfile, join
from random import shuffle
from collections import Counter
import h5py
import csv

user_ids = subprocess.check_output(f"curl --location --request GET 'https://hackapi.azurewebsites.net/api/users' --header 'x-functions-key: WJpDAQqpIbZNa7ANLrlZIzShYYUszrfRNMbdjQv6g66RdW1JLaVAaQ=='", shell=True)
# this is not safe in general, but in this case we know that the output of the above api call is in the format of a Python list
user_ids = eval(user_ids)
print(len(user_ids))
recognized_activity_dict = dict()
basic_activity_dict = dict()
activity_details_dict = dict()
time_list_dict = dict()
full_data = []

for user_id in user_ids[:]:
    time.sleep(1)
    activies = subprocess.check_output(f"curl --location --request GET 'https://hackapi.azurewebsites.net/api/activities?userId={user_id}' --header 'x-functions-key: WJpDAQqpIbZNa7ANLrlZIzShYYUszrfRNMbdjQv6g66RdW1JLaVAaQ=='", shell=True)
#    pprint(json.loads(activies))
    
    test = json.loads(activies)
    recognized_activity = []
    basic_activity = []
    activity_details = []
    time_list = []


    for t in test:
        recognized_activity.append(t['recognizedActivity'])
        basic_activity.append(t['basicActivity'])
        time_list.append(t['activityTime'])
        activity_details.append(t['activityDetails'])
        full_data.append(str(user_id)+'_'+str(t['activityTime'])+'_'+str(t['basicActivity'])+'_'+str(t['activityDetails'])+'_'+str(t['recognizedActivity']))
    
    recognized_activity_dict [user_id] = recognized_activity
    basic_activity_dict [user_id] = basic_activity
    activity_details_dict [user_id] = activity_details
    time_list_dict [user_id] = time_list




with open ("/Users/bassler/Desktop/Combined_data.csv", "w") as csv_file:
    writer=csv.writer(csv_file, delimiter ="\t")
    for value in full_data:
        writer.writerow([value])




with open ("/Users/bassler/Desktop/Recognized_activity.csv", "w") as csv_file:
    writer=csv.writer(csv_file, delimiter ="\t")
    for key, value in recognized_activity_dict.items():
        writer.writerow([key, value])

with open ("/Users/bassler/Desktop/Basic_activity.csv", "w") as csv_file:
    writer=csv.writer(csv_file, delimiter ="\t")
    for key, value in basic_activity_dict.items():
        writer.writerow([key, value])
    
with open ("/Users/bassler/Desktop/Activity_time.csv", "w") as csv_file:
    writer=csv.writer(csv_file, delimiter ="\t")
    for key, value in time_list_dict.items():
        writer.writerow([key, value])  
        
with open ("/Users/bassler/Desktop/Activity_details.csv", "w") as csv_file:
    writer=csv.writer(csv_file, delimiter ="\t")
    for key, value in activity_details_dict.items():
        writer.writerow([key, value])  
        
        
        