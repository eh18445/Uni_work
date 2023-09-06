# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np


#data for men and women
dog = [207,234]
cat = [282,242]
bird = [241,232]

#total for animal type
dog.append(np.sum(dog))
cat.append(np.sum(cat))
bird.append(np.sum(bird))

#total for gender
total = np.zeros(3)
for i in range(3):
    total[i] = dog[i]+cat[i]+bird[i]

#create contingency table
data = {
        "dog":dog,
        "cat":cat,
        "bird":bird,
        "total":total}

contingency = pd.DataFrame(data,index=["men","women","total"])

print("Contingency Table")
print(contingency)
print('')

#Create expected value table
alpha = 0.05
critical = 5.991

grand_total = contingency.loc["total","total"]

expected = pd.DataFrame(data,index=["men","women","total"])

column = list(expected)

row = expected.index.values

for i in range(3):
    for j in range(2):
        expected.loc[row[j],column[i]] = (contingency.loc[row[j],column[3]]*contingency.loc[row[2],column[i]])/grand_total

print("Expected Value Table")
print(expected)
print('')

#calculate degrees of freedom
DoF = (len(row)-2)*(len(column)-2)

#chi-square table
chi_table = []

for i in range(3):
    for j in range(2):
        observed = contingency.loc[row[j],column[i]]
        calculated = expected.loc[row[j],column[i]]
        print(observed,calculated)
        chi_table.append((observed-calculated)**2/(calculated))

chi_square = np.sum(chi_table)
print(chi_table)
print("Calculated Chi-Square:",chi_square)
print("Critical Chi-Square",critical)
print('')

if chi_square <= critical:
    H0 = True
else:
    H0 = False
print("The null hypothesis holds:",H0)




#alternative method:
from scipy.stats import chi2_contingency
  
# defining the table
data = [[207, 282, 241], [234, 242, 232]]
stat, p, dof, expected = chi2_contingency(data)
  
# interpret p-value
print("p value is " + str(p))
if p <= alpha:
    print('Dependent (reject H0)')
else:
    print('Independent (H0 holds true)')






