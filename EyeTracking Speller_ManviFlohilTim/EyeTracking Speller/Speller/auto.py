import os
import autocomplete
from autocomplete import models

def initAutoComplete():
  if not os.path.isfile('./test.pkl'):
    with open('brown.txt', 'r') as corpusFile:
      data=corpusFile.read()
    models.train_models(data, os.getcwd() + '/test.pkl')
  models.load_models(os.getcwd() + '/test.pkl')


def getAutoComplete(curr, last = ''):
  if last != '':
    results = autocomplete.predict(last, curr, 1)
    if not results:
        results = autocomplete.predict_currword(curr, 1)
  else:
    results = autocomplete.predict_currword(curr, 1)
  if not results:
    return '???'
  return results[0][0]

initAutoComplete()
print(getAutoComplete('s', 'blue'))

