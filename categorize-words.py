#!/usr/bin/env python3

import nltk
import re
import json
from nltk.corpus import wordnet as wn

with open('wordcloud.html') as f:
    wordCloudHtml = f.read()

colorWords = {}
for match in re.finditer('const (\w+) \= (\[.*?\])', wordCloudHtml):
    colorName = match.group(1)
    colorData = match.group(2)
    colorJson = json.loads(colorData)
    colorWords[colorName] = [item['text'] for item in colorJson]

def categorizeWords(wordList, depth=2):
    cats = {}
    for word in wordList:
        synsets = wn.synsets(word, pos=wn.NOUN)
        if len(synsets) != 0 and synsets is not None:
            synset = synsets[0]
        else:
            continue
        cat = getHypernymLevelN(synset, depth).name()
        if cat in cats:
            cats[cat].append(word)
        else:
            cats[cat] = [word]
    return cats

def getHypernymLevelN(synset, n):
    while synset.min_depth() > n:
        hypernyms = synset.hypernyms()
        if len(hypernyms) > 0:
            synset = hypernyms[0]
        else:
            break
    return synset

if __name__ == "__main__":
    colorCats = {color: categorizeWords(colorWords[color])
                 for color in colorWords}
    print(colorCats['green'])
