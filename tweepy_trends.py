#!/usr/bin/env python
# -*- coding: utf-8 -*-

import tweepy

consumer_key=""
consumer_secret=""
access_token=""
access_token_secret=""

# OAuth process, using the keys and tokens
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)
trends1 = api.trends_place(1) # from the end of your code

# trends1 is a list with only one element in it, which is a
# dict which we'll put in data.
data = trends1[0]

# grab the trends
trends = data['trends']

# grab the name from each trend
names = [trend['name'] for trend in trends]

# put all the names together with a ' ' separating them
trendsName = ' '.join(names)
print(trendsName)

# Ex response
#PolandNeedsWWATour #DownloadElyarFox #DünMürteciBugünHaşhaşi #GalatasaraylılıkNedir #KnowTheTruth Tameni Video Anisa Rahma Mikaeel Manado JDT
