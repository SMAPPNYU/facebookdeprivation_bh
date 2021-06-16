'''
This script takes in Census lists for first names and ethnicities and calculates network ethnic diversity
for a list of panel respondents who have provided their Facebook friends lists.

Data in:
 - names.csv -- a list of ethnicities and popular names within that ethnicity
 - network_data.json -- the Facebook-provided JSON of friends lists
 
Data out:
 - network_diversity_v1.csv -- the calculated network diversity based on cross-ethnic names being downweighted 
 - network_diversity_v2.csv -- the calculated network diversity based on unweighted name counts
'''

# imports for the script
import collections
import datetime
import glob
import itertools
import json
import os
import re

import pandas as pd

# import census data
df = pd.read_csv('/scratch/olympus/projects/bosnia_experiments/names_v2.csv', sep='\t')

# lowercase the names for matching and replace non-standard characters
def lower(s):
    if isinstance(s, str):
        s = s.replace(u'\xa0', '').lower()
        s = s.encode('utf-8').replace(b'\xc3\xa5\xc2\xbe', b'z').decode()
        s = s.encode('utf-8').replace(b'\xc3\xa5\xc2\xa0', b'dz').decode()
        s = s.encode('utf-8').replace(b'\xc3\xa5', b's').decode()
        s = s.encode('utf-8').replace(b'\xc2\xa1', b'').decode()
        return s
    return s

# detect whether or not a string contains cyrillic characters
def has_cyrillic(text):
    return bool(re.search('[\u0400-\u04FF]', text))

# load friends data 
def load_data(user):
    
    # date of start of experiment
    START = datetime.datetime(2019, 7, 8)

    # if the file is a JSON
    if '.json' in user:
        base = os.path.basename(user).replace('.json', '').replace('friends_', '')
        user_json = json.load(open(user, 'r'))
        user_list = []
        for item in user_json['friends']:
            # only include friends from before the start of the experiment
            timestamp = datetime.datetime.utcfromtimestamp(item['timestamp'])
            if timestamp < START:
                user_list.append(item['name'])
                
        return base, user_list
    
    # if the file is a .txt list
    if '.txt' in user:
        user_list = open(user, 'r').readlines()
        base = os.path.basename(user).replace('.txt', '').replace('friends_', '')
        return base, user_list

    
# rename columns
df.rename({
    'croat_male':'croatian_male',
    'croat_female':'croatian_female',
    'serb_male':'serbian_male',
    'serb_female':'serbian_female'
}, axis=1, inplace=True)

# declare columns to keep
columns = ['bosniak_female', 'bosniak_male', 'croatian_male', 'croatian_female',
       'serbian_male', 'serbian_female']


# apply normalization to each of the state columns
bosniak_male = df['bosniak_male'].apply(lower).dropna().tolist()
bosniak_female = df['bosniak_female'].apply(lower).dropna().tolist()
serbian_male = df['serbian_male'].apply(lower).dropna().tolist()
serbian_female = df['serbian_female'].apply(lower).dropna().tolist()
croatian_female = df['croatian_female'].apply(lower).dropna().tolist()
croatian_male = df['croatian_male'].apply(lower).dropna().tolist()
bosniak_croat = df['bosniak_croat'].apply(lower).dropna().tolist()
bosniak_serb = df['bosniak_serb'].apply(lower).dropna().tolist()
serb_croat = df['serb_croat'].apply(lower).dropna().tolist()

# create dictionary of number of times a name appears for weighting 
all_names = dict(collections.Counter(itertools.chain.from_iterable([df[col].apply(lower).dropna().tolist() for col in columns])))
for k, v in all_names.items():
    all_names[k] = 1/v
    
# load user JSONs of Facebook Friends
user_jsons = (glob.glob('/scratch/olympus/projects/bosnia_experiments/network_data/*.json') + 
              glob.glob('/scratch/olympus/projects/bosnia_experiments/network_data/*.txt'))
            
# only friends that existed as of the experiment start date


match_bosniak = []
match_serbian = []
match_croatian = []
not_matched = []

user_dicts = []

# for each user, count the number of friends that appear in each ethnic group according to the census list
for user in user_jsons:
    base, user_json = load_data(user)
    
    if user_json:
    
        person = {'user':base,
                  'bosniak':0,
                  'serbian':0,
                  'croatian':0,
                  'not_matched':[]
                 }

        # for each friend, normalize the name, return which census group the name falls into
        for name in user_json:
            #timestamp = datetime.datetime.utcfromtimestamp(name['timestamp'])
            if timestamp > START:
                continue

            first_name = name.split(' ')[0].lower()
            first_name = lower(first_name)
            no_match = True

            # if the name has cyrillic characters or appears in serb census lists, match to serbian
            if has_cyrillic(first_name) or first_name in serbian_male or first_name in serbian_female:
                try:
                    person['serbian'] += all_names.get(first_name)
                    match_serbian.append(first_name)
                    no_match = False
                except:
                    continue

            # if name appears in bosniak census list, match to bosniak
            if first_name in bosniak_male or first_name in bosniak_female:
                person['bosniak'] += all_names.get(first_name)
                match_bosniak.append(first_name)
                no_match = False

            # if the name appears in croat census list, match to croat
            if first_name in croatian_male or first_name in croatian_female:
                person['croatian'] += all_names.get(first_name)
                match_croatian.append(first_name)
                no_match = False

            # if the name appears in no census list, do not match to an ethnicity
            if no_match:
                person['not_matched'].append(first_name)
                not_matched.append(first_name)


        # determine the number of friends that were matched to an ethnicity, total number of friends, and % of friends matched
        person['matched_count'] = sum([person[value] for value in ['bosniak', 'serbian', 'croatian']])
        person['total_friends'] = len(user_json)
        person['matched_percentage'] = 100*person['matched_count']/person['total_friends']
        person['percentage_unmatched'] = 100 - person['matched_percentage']
        
        # determine percentage of each ethnicity in user's friend network
        if person['matched_count'] == 0:
            person['percentage_bosniak'] = 0
            person['percentage_serbian'] = 0
            person['percentage_croatian'] = 0

        else:
            person['percentage_bosniak'] = 100*person['bosniak']/person['matched_count']
            person['percentage_serbian'] = 100*person['serbian']/person['matched_count']
            person['percentage_croatian'] = 100*person['croatian']/person['matched_count']

            
        # add to list of user data
        user_dicts.append(person)
        
# create dataframe of network diversity scores -- output to csv
df = pd.DataFrame(user_dicts)
df.to_csv('/scratch/olympus/projects/bosnia_experiments/network_diversity_v1.csv', index=False)

# calcualte network diversity where names appearing across ethnicities are not downweighted 
# load in census data and create lists of names for each ethnicity/multiethnic name
df = pd.read_csv('/scratch/olympus/projects/bosnia_experiments/names_v2.csv', sep='\t')

bosniak = df['bosniak_male'].apply(lower).dropna().tolist() + df['bosniak_female'].apply(lower).dropna().tolist()
serbian = df['serb_male'].apply(lower).dropna().tolist() + df['serb_female'].apply(lower).dropna().tolist()
croatian = df['croat_female'].apply(lower).dropna().tolist() + df['croat_male'].apply(lower).dropna().tolist()
bosniak_croat = df['bosniak_croat'].apply(lower).dropna().tolist()
bosniak_serb = df['bosniak_serb'].apply(lower).dropna().tolist()
serb_croat = df['serb_croat'].apply(lower).dropna().tolist()

bosniak = set(bosniak) - set(itertools.chain.from_iterable([serbian, croatian, bosniak_croat, bosniak_serb, serb_croat]))
serbian = set(serbian) - set(itertools.chain.from_iterable([bosniak, croatian, bosniak_croat, bosniak_serb, serb_croat]))
croatian = set(croatian) - set(itertools.chain.from_iterable([bosniak, serbian, bosniak_croat, bosniak_serb, serb_croat]))
bosniak_croat = set(bosniak_croat) - set(itertools.chain.from_iterable([bosniak, serbian, croatian, bosniak_serb, serb_croat]))
bosniak_serb = set(bosniak_serb) - set(itertools.chain.from_iterable([bosniak, serbian, croatian, bosniak_croat, serb_croat]))
serb_croat = set(serb_croat) - set(itertools.chain.from_iterable([bosniak, serbian, croatian, bosniak_croat, bosniak_serb]))

user_dicts = []
match_bosniak = []
match_serbian = []
match_croatian = []
match_bosniak_croat = []
match_bosniak_serb = []
match_serb_croat = []
not_matched = []

# for each user, iterate through their friends list and calculate network diversity
for user in user_jsons:
    base, user_json = load_data(user)
    
    person = {'user':base,
              'serbian':0,
              'bosniak':0,
              'croatian':0,
              'bosniak_croat':0,
              'bosniak_serb':0,
              'serb_croat':0,
              'not_matched':[]
             }
    
    if user_json:
        
        # for name in the friends list, check which ethnicity the name appears in and increment the 
        # user's count for network diversity estimation
        for name in user_json:
            #timestamp = datetime.datetime.utcfromtimestamp(name['timestamp'])
            if timestamp > START:
                continue

            # normalize the name 
            first_name = name.split(' ')[0].lower()
            first_name = lower(first_name)
            no_match = True
            check_indiv = True

            # if bosniak-croat name, implement that count
            if first_name in bosniak_croat:
                person['bosniak_croat'] += 1
                match_bosniak_croat.append(first_name)
                no_match = False
                check_indiv = False

            # if bosniak serb name, increment that count 
            if first_name in bosniak_serb:
                person['bosniak_serb'] += 1
                match_bosniak_serb.append(first_name)
                no_match = False
                check_indiv = False
                
            # if serb croat, increment that count
            if first_name in serb_croat:
                person['serb_croat'] += 1
                match_serb_croat.append(first_name)
                no_match = False
                check_indiv = False

            # check for single-ethnicity names
            if check_indiv:

                # if containing cyrillic or in serbian list, add serbian
                if has_cyrillic(first_name) or first_name in serbian:
                    person['serbian'] += 1
                    match_serbian.append(first_name)
                    no_match = False

                # if bosniak, add bosniak
                if first_name in bosniak:
                    person['bosniak'] += 1
                    match_bosniak.append(first_name)
                    no_match = False

                # if croatian, add croat
                if first_name in croatian:
                    person['croatian'] += 1
                    match_croatian.append(first_name)
                    no_match = False

            # append to not matched if name in no lists
            if no_match:
                person['not_matched'].append(first_name)
                not_matched.append(first_name)


        # count number of names that matched, percentage matched, and percentage unmatched
        person['matched_count'] = sum([person[value] for value in ['bosniak', 'serbian', 'croatian',
                                                                   'bosniak_croat', 'bosniak_serb', 'serb_croat']])
        person['total_friends'] = len(user_json)
        person['matched_percentage'] = 100*person['matched_count']/person['total_friends']
        person['percentage_unmatched'] = 100 - person['matched_percentage']
        
        if person['matched_count'] == 0:
            person['percentage_bosniak'] = 0
            person['percentage_serbian'] = 0
            person['percentage_croatian'] = 0
            person['percentage_bosniak_croat'] = 0
            person['percentage_bosniak_serb'] = 0
            person['percentage_serb_croat'] = 0

        else:
            person['percentage_bosniak'] = 100*person['bosniak']/person['matched_count']
            person['percentage_serbian'] = 100*person['serbian']/person['matched_count']
            person['percentage_croatian'] = 100*person['croatian']/person['matched_count']
            person['percentage_bosniak_croat'] = 100*person['bosniak_croat']/person['matched_count']
            person['percentage_bosniak_serb'] = 100*person['bosniak_serb']/person['matched_count']
            person['percentage_serb_croat'] = 100*person['serb_croat']/person['matched_count']

        
        # append to list
        user_dicts.append(person)
        
        
# output network diversity 
df = pd.DataFrame(user_dicts)
df.to_csv('/scratch/olympus/projects/bosnia_experiments/network_diversity_v2.csv', index=False)
