#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 12:32:38 2021

@author: Natalie Biderman

@title: create stimuli data frame
"""

# import packages

import glob
import os
import pandas as pd

# ============================================================================
# Category data lists
# ============================================================================

is_practice_data = 1

if (is_practice_data==1):
    stims_folder = '../Category_learning/Task/Stimuli/Practice_stims';
    file_name = "practice_category_learning_stimuli"
    var_name = "practice_category_learning_stimuli"
else:
    stims_folder = '../Category_learning/Task/Stimuli/Experimental_stims';
    file_name = "category_learning_stimuli"
    var_name = "category_learning_stimuli"


# load stimuli
paths = glob.glob(stims_folder + '**/*/*', recursive=True) # extract all the list of items recursively
task_stims = [f for f in paths if os.path.isfile(f)]
test_paths = [f for f in paths if os.path.isdir(f)]
# remove duplicates test items (we only keep the subfolders that include the wording "TestItems")
def Filter(string, substr):
    return [str for str in string if
             any(sub not in str for sub in substr)]
test_paths = Filter(test_paths, "Z_Duplicate_")
# load images from test paths
test_stims = []
for path in test_paths:
    for file in os.listdir(path):
        test_stims.append(os.path.join(path, file));

# concatenate
stims = task_stims + test_stims;


# extract category and img from path 
stims = pd.Series(stims)
stims = stims.str.extract(stims_folder + "/(?P<category>.*?)/(?P<img>.*)")
stims = stims.assign(is_test_item = 0);
for i in range(len(stims)):
    if "TestItems" in stims.loc[i,"img"]:
        extracted_img = stims.loc[i,"img"].split("/");
        stims.loc[i,"is_test_item"] = "test_item";
        stims.loc[i,"img"] = extracted_img[1];

# arrange by category 
stims = stims.sort_values(by = ["category", "is_test_item"]).reset_index()#.drop("index",axis=1);

# remove duplicate exemplars 
stims_clean = stims.drop_duplicates(subset = "img").reset_index()

# add examplar numbering
start_ind=1
stims_clean.loc[0,"exemplar"] = stims_clean.loc[0,"category"] + str(1) # we manually assign the first trial, to allow looping from second trial
for t in range(1,len(stims_clean)):
    if stims_clean.loc[t,"category"] == stims_clean.loc[t-1,"category"]:
        start_ind = start_ind + 1
    else:
        start_ind = 1;
    stims_clean.loc[t,"exemplar"] = stims_clean.loc[t,"category"] + str(start_ind)

# add ranking of stimuli 
# (ranking taken from Talia Konkle's website - https://konklab.fas.harvard.edu/, based on data published at Konkle, Brady, Alvarez, & Oliva, 2010, JEP:General.)
rankings = pd.read_csv("../Category_learning/Task/Stimuli/MM2-Ranks.csv").rename(columns={"Category":"category"})
stims_clean = stims_clean.merge(rankings, on = "category")

# save data frame as csv and jason files (for java script)

stims_clean.to_csv("../Category_learning/Task/Stimuli/Stimuli_list/" + file_name + ".csv")
stims_clean.to_json("../Category_learning/Task/Stimuli/Stimuli_list/" + file_name + ".json")

# to make things easier in javascript, we manually convert the data frame into js formatting
stims_text = str()
for index, row in stims_clean.iterrows():
    stims_text = '{}[{}, "{}", "{}", "{}", "{}", {}, {}, {}, {}, {}],\n '.format(
        stims_text, 
        index,
        stims_clean.loc[index, "category"],
        stims_clean.loc[index, "exemplar"],
        stims_clean.loc[index, "img"],
        stims_clean.loc[index, "is_test_item"],
        stims_clean.loc[index, "kinds"],
        stims_clean.loc[index, "shape"],
        stims_clean.loc[index, "color"],
        stims_clean.loc[index, "overall perceptual (upright)"],
        stims_clean.loc[index, "overall perceptual (inverted)"])
 
# change the last characters of the string to end the list
stims_text = stims_text[:len(stims_text) - 3]
stims_text = "var " + var_name + " = \n[" + stims_text

# add text to explain the column names
explain = '// columns:\n // 0. index\n // 1. category\n // 2. exemplar\n // 3. image name\n // 4. is the stimulus a test item\n // The following parameters were taken from Talia Konkle website - https://konklab.fas.harvard.edu/, based on data published at Konkle, Brady, Alvarez, & Oliva, 2010, JEP:General.)\n // 5. kinds - how many types of exemplars within this category (for motor - cars, bicycle, etc.)\n // 6. shape - how many shapes in this category\n // 7. color\n // 8. overall perceptual (upright)\n // 9. overall perceptual (inverted)\n\n'
stims_text = explain + stims_text

# save file
with open("../Category_learning/Task/Stimuli/Stimuli_list/" + file_name + ".js", 'w') as f:
    f.write(stims_text)



# ============================================================================
# Size data lists
# ============================================================================

is_practice_data = 1

if (is_practice_data==1):
    stims_folder = '../Size_judgement/Task/Stimuli/Size_stims/Practice';
    file_name = "size_practice_stimuli"
    var_name = "size_practice_stimuli"
else:
    stims_folder = '../Size_judgement/Task/Stimuli/Size_stims';
    file_name = "size_stimuli"
    var_name = "size_stimuli"

# load stimuli
paths = glob.glob(stims_folder + '**/*/*/*', recursive=True) # extract all the list of items recursively
task_stims = [f for f in paths if os.path.isfile(f)]
test_paths = [f for f in paths if os.path.isdir(f)]
# remove duplicates test items (we only keep the subfolders that include the wording "TestItems")
def Filter(string, substr):
    return [str for str in string if
             any(sub not in str for sub in substr)]
test_paths = Filter(test_paths, "Z_Duplicate_")
# load images from test paths
test_stims = []
for path in test_paths:
    for file in os.listdir(path):
        test_stims.append(os.path.join(path, file));

# concatenate
stims = task_stims + test_stims;


# extract category and img from path 
stims = pd.Series(stims)
stims = stims.str.extract(stims_folder + "/(?P<size>.*?)/(?P<category>.*?)/(?P<img>.*)")
stims = stims.assign(is_test_item = 0);
for i in range(len(stims)):
    if "TestItems" in stims.loc[i,"img"]:
        extracted_img = stims.loc[i,"img"].split("/");
        stims.loc[i,"is_test_item"] = "test_item";
        stims.loc[i,"img"] = extracted_img[1];

# arrange by category 
stims = stims.sort_values(by = ["size","category", "is_test_item"]).reset_index()#.drop("index",axis=1);

# remove duplicate exemplars 
stims_clean = stims.drop_duplicates(subset = "img").reset_index()

# add examplar numbering
start_ind=1
stims_clean.loc[0,"exemplar"] = stims_clean.loc[0,"category"] + str(1) # we manually assign the first trial, to allow looping from second trial
for t in range(1,len(stims_clean)):
    if stims_clean.loc[t,"category"] == stims_clean.loc[t-1,"category"]:
        start_ind = start_ind + 1
    else:
        start_ind = 1;
    stims_clean.loc[t,"exemplar"] = stims_clean.loc[t,"category"] + str(start_ind)

# add ranking of stimuli 
# (ranking taken from Talia Konkle's website - https://konklab.fas.harvard.edu/, based on data published at Konkle, Brady, Alvarez, & Oliva, 2010, JEP:General.)
rankings = pd.read_csv("../Size_judgement/Task/Stimuli/MM2-Ranks.csv").rename(columns={"Category":"category"})
stims_clean = stims_clean.merge(rankings, on = "category")

# save data frame as csv file

stims_clean.to_csv("../Size_judgement/Task/Stimuli/Stimuli_list/" + file_name + ".csv")

# to make things easier in javascript, we manually convert the data frame into js formatting
stims_text = str()
for index, row in stims_clean.iterrows():
    stims_text = '{}[{}, "{}", "{}", "{}", "{}", "{}", {}, {}, {}, {}, {}],\n '.format(
        stims_text, 
        index,
        stims_clean.loc[index, "size"],
        stims_clean.loc[index, "category"],
        stims_clean.loc[index, "exemplar"],
        stims_clean.loc[index, "img"],
        stims_clean.loc[index, "is_test_item"],
        stims_clean.loc[index, "kinds"],
        stims_clean.loc[index, "shape"],
        stims_clean.loc[index, "color"],
        stims_clean.loc[index, "overall perceptual (upright)"],
        stims_clean.loc[index, "overall perceptual (inverted)"])
 
# change the last characters of the string to end the list
stims_text = stims_text[:len(stims_text) - 3] + '] '
stims_text = "var " + var_name + " = \n[" + stims_text

# add text to explain the column names
explain = '// columns:\n // 0. index\n // 1. size\n // 2. category\n // 3. exemplar\n // 4. image name\n // 5. is the stimulus a test item\n // The following parameters were taken from Talia Konkle website - https://konklab.fas.harvard.edu/, based on data published at Konkle, Brady, Alvarez, & Oliva, 2010, JEP:General.)\n // 6. kinds - how many types of exemplars within this category (for motor - cars, bicycle, etc.)\n // 7. shape - how many shapes in this category\n // 8. color\n // 9. overall perceptual (upright)\n // 10. overall perceptual (inverted)\n\n'
stims_text = explain + stims_text

# save file
with open("../Size_judgement/Task/Stimuli/Stimuli_list/" + file_name + ".js", 'w') as f:
    f.write(stims_text)

