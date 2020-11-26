#!/usr/bin/env python3

import nltk
import re
import json
from nltk.corpus import wordnet as wn
import plotly.express as px
import pandas as pd

def categorizeWords(wordData, minDepth=5, maxDepth=0):
    wordsAndCats = []
    for pair in wordData:
        word = pair['text']
        val = pair['size']
        synsets = wn.synsets(word, pos=wn.NOUN)
        if len(synsets) != 0 and synsets is not None:
            synset = getSynset(word, synsets)
        else:
            continue
        wordCat = [val, word]
        depth = minDepth
        while depth > maxDepth:
            cat = getHypernymLevelN(synset, depth).name()
            wordCat.append(cat)
            depth -= 1
        # if cat in cats:
        #     cats[cat].append(word)
        # else:
        #     cats[cat] = [word]
        wordsAndCats.append(wordCat)
    return wordsAndCats

def getSynset(word, synsets):
    """ Guesses the best synset based on no criteria. """
    # Just some manual correction of bad guesses
    if word in ['eyes']:
        return synsets[1] # Green eyes are rarely the abstraction
    elif word in ['curls']:
        return synsets[2] # Brown curls are usually hair
    else:
        return synsets[0]

def getHypernymLevelN(synset, n):
    while synset.min_depth() > n:
        hypernyms = synset.hypernyms()
        if len(hypernyms) > 0:
            synset = hypernyms[0]
        else:
            break
    return synset

def makeChart(colorWithCats, name):
    df = pd.DataFrame(colorWithCats) # columns=cols)
    print(df)
    fig = px.treemap(df, path=df.columns[-1:0:-1],
                     values=0,
                     color=0,
                     color_continuous_scale=getColorScale(name),
                     color_continuous_midpoint=df[0].mean(),
                     title='Nouns and their WordNet categories commonly associated with the adjective ' + name
                     )
    with open(name+'.html', 'w') as f:
        f.write(fig.to_html())

def getColorScale(color):
    withColorScales = ['blue', 'green', 'orange', 'purple', 'red']
    exceptions = {'gray': 'Greys', 'brown': 'Brwnyl', "yellow": "solar",
                   "white": "gray", "black": "Greys", "bluish": "Blues",
                  "reddish": "Reds", "greenish":"Greens", "violet": "Purples"}

    if color in withColorScales:
        return color.capitalize() + 's'
    elif color in exceptions:
        return exceptions[color]
    else:
        return 'Plotly3'

if __name__ == "__main__":
    with open('../wordcloud.html') as f:
        wordCloudHtml = f.read()

    colorWords = {}
    for match in re.finditer('const (\w+) \= (\[.*?\])', wordCloudHtml):
        colorName = match.group(1)
        colorData = match.group(2)
        colorJson = json.loads(colorData)
        colorWords[colorName] = colorJson

    colorCats = {color: categorizeWords(colorWords[color])
         for color in colorWords}

    for color in colorWords:
        makeChart(colorCats[color], name=color)
