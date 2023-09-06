# -*- coding: utf-8 -*-
"""
Created on Fri Oct 18 11:13:06 2019

@author: -
"""

import math

def MyArcTan(x,N):
    """
    The arctangent function using the taylor expansion for a 
    given value of x and summed up to N terms.
    """
    #Ans is the answer that will be returned depending on the value of x
    
    #S is a list of numbers to be summed together
    S = []
        
    #starting value of n
    n = 0
    #Function when |x|<=1
    if x <= 1 and x >= -1:
        #increment the value of n and add until n=N
        while n <= N:
            y = ((-1)**n) * (x**(2*n+1)) * ((2*n+1)**(-1))
            S.append(y)
            n += 1
            Ans = sum(S)
    
    elif x > 1 or x < -1:
        #increment n but instead using 1/x instaed
        while n <= N:
            z = ((-1)**n) * (x**(-2*n-1)) * ((2*n+1)**(-1))
            S.append(z)
            n += 1
                
        if x > 1:
            Ans = 0.5*(math.pi) - sum(S)
        elif x < -1:
            Ans = -0.5*(math.pi) - sum(S)
    
    return Ans


MyInput = '0'
while MyInput != 'q':
    MyInput = input('Enter a choice, "a", "b", "c" or "q" to quit: ')
    print('You entered the choice: ',MyInput)

    if MyInput == 'a':
        print('You have chosen part (a)')
        Input_x = input('Enter a value for x (floating point number): ')
        x = float(Input_x)
        
        #This makes sure that the value of N inputed is a positive integer
        u = 0
        while u != "q":
            Input_N = input('Enter a value for N (positive integer): ')
            N = int(Input_N)
            if N >= 0:
                u = "q"
            elif N < 0:
                print("This isn't a valid input")

        print('The answer is: ',MyArcTan(x,N))

    elif MyInput == 'b':
        print('You have chosen part (b)')
        
        #This also makes sure N is a positive integer
        u = 0
        while u != "q": 
            Input_N = input('Enter a value for N (positive integer): ')
            N = int(Input_N)
            if N >= 0:
                u = "q"
            elif N < 0:
                print("This isn't a valid input")

        
        #starting value for x that will increment up to x = 2
        x = -2
        
        #Make Table
        print("-------------------------------------------------------------------------")
        #header of the table
        print("|", "{:5}".format("x"), "{:20}".format("MyArcTan(x,N)"), 
              "{:20}".format("math.atan(x)"), "{:21}".format("difference"), "|")
        print("-------------------------------------------------------------------------")
        
        while x <= 2:
            #loop which gives each row in table where x increments by 0.1
            print("|", "{:4.1f}".format(x), "{:20}".format(MyArcTan(x,N)), 
                  "{:20}".format(math.atan(x)), 
                  "{:22}".format(abs(MyArcTan(x,N) - math.atan(x))), "|")
            x *= 10
            x += 1
            x /= 10
        print("-------------------------------------------------------------------------")
        
        
    elif MyInput != 'q':
        print('This is not a valid choice')

print('You have chosen to finish - goodbye.')