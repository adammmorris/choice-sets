# -*- coding: utf-8 -*-
"""
Created on Thu Jan 26 10:18:44 2017

@author: adam
"""

import numpy as np
import scipy.stats as st
import matplotlib.pyplot as plt
import string

#words_all = np.array(['screep', 'sursed', 'chooth', 'zaley', 'scrigged', 'froped', 'dunched',
#'vated', 'poats', 'steaves', 'mickered', 'blicket', 'wugged', 'fipping', 'cleepers',
#'wooked', 'roaking', 'droofer', 'cheedle', 'nardly', 'chaxed', 'thocky', 'zoring',
#'glundle', 'flemmering', 'tinger', 'snave', 'plibbered', 'meekin', 'mudley', 'hotchack','jaddeared','murrads'])
words_all = np.array(['basket', 'community', 'chocolate', 'ladder', 'military', 'machine', 'traffic', 'mountain', 'doorway', 'sunlight', 'bookshelf',
	'structure', 'wedding', 'keyboard', 'fortune', 'helicopter', 'stomach', 'elephant', 'football', 'umbrella']);
 
np.random.seed(124)
#words_ind = np.random.choice(np.arange(np.size(words_all)), 15, False)
words_ind = np.array([12, 16,  3,  1, 10, 19, 18,  8, 15,  6, 11,  5, 13, 0, 9])
words = words_all[words_ind]

# (1) - good
lset = ['s','g','e','f','b','n','l']
r1 = [sum([1 if c in lset else 0 for c in word]) for word in words]
#plt.hist(4 * r1)

# (2) - good
lset = ['t','p','o','k','i','c','h']
r2 = [sum([1 if c in lset else 0 for c in word]) for word in words]
#plt.hist(4 * r2)

# (3) - bad
lset = ['u','a','w','d','r','m','y']
r3 = [sum([1 if c in lset else 0 for c in word]) for word in words]
#plt.hist(6 * r3)

# (4) - bad
# num of letters whose subsequent letter is later in alphabet
r4 = [sum([1 if word[cind] < word[cind+1] else 0 for cind in range(len(word)-1)]) for word in words]
#plt.hist(5 * r4)

# (5) - good
# num of letters in alphabet which come after the third letter of the word
r5 = [26 - string.ascii_lowercase.index(word[2]) for word in words]
#plt.hist(r5)

# (6) - good
# num of letters in alphabet which come before the second letter of the word
r6 = [string.ascii_lowercase.index(word[1]) for word in words]
#plt.hist(r6)

# (7) - good
# num of letters in alphabet which come in between 4th and 5th letter of word
r7 = [abs(string.ascii_lowercase.index(word[4]) - string.ascii_lowercase.index(word[3])) for word in words]
#plt.hist(r7)

# (8) - good
# num of letters in alphabet which come in between 5th and 6th letters of word
r8 = [abs(string.ascii_lowercase.index(word[5]) - string.ascii_lowercase.index(word[4])) for word in words]
#plt.hist(r8)

# (9) - bad
# num of pairs of letters in word that are only one apart in alphabet
r9 = [sum(
        [
            sum([1 if string.ascii_lowercase.index(word[cind2]) - string.ascii_lowercase.index(word[cind]) == 1 else 0 for cind2 in range(len(word))])
            for cind in range(len(word))
        ])
    for word in words]
#plt.hist(3 * r9)

# (10) - good
# number of vertical lines
#r9_all = np.array([3, 3, 5, 3, 4, 5, 7, 3, 2, 3, 8, 6, 2, 7, 6, 3, 5, 5, 7, 6, 4, 5, 4, 6, 12, 6, 3, 9, 8, 6, 6, 6, 5])
r10 = np.array([4, 9, 5, 5, 8, 8, 5, 8, 3, 7, 7, 5, 6, 6, 6, 9, 5, 9, 5, 8])
r10 = r10[words_ind]
#plt.hist(3 * 10)

# (11) - good
# number of horizontal lines
r11 = np.array([5, 3, 7, 5, 7, 7, 8, 4, 1, 6, 7, 5, 6, 4, 6, 11, 3, 10, 6, 6])
r11 = r11[words_ind]

wordlen = [len(word) for word in words]

# range: 0-27
full = (r1, r2, r3, r5, r6, r7, r8, r10, r11)
k = len(full)
cors = np.zeros((k, k))
ps = np.zeros((k, k))
for i in range(k):
    for j in range(k):
        if i != j:
            (cors[i,j], ps[i,j]) = st.pearsonr(full[i], full[j])
            
full