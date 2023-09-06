# -*- coding: utf-8 -*-
"""
Created on Thu Nov 14 14:29:11 2019

@author: Daniel
"""

import numpy as np
import matplotlib.pyplot as plt

#The constants used
C = 1.15
rho0 = 1.2
A = 0.9329
k0 = 0.5*C*rho0*A
m = 73
g = 9.81
y0 = 1000
vy0 = 0
h = 7.64*1000

#Calculate the maximum t value when y = 0:
T = (m*(k0*g)**(-1))**0.5*np.arccosh(np.exp(k0*y0*m**(-1)))

def Func_y(t):
      
    y = y0 - (m/k0)*np.log(np.cosh(t*((k0*g)/m)**0.5))   
    return y
    
def Func_vy(t):
    
    vy = -(m*g*k0**(-1))**0.5*np.tanh(t*(k0*g*m**(-1))**0.5)
    return vy

MyInput = '0'
while MyInput != 'q':
    MyInput = input('Enter a choice, "a", "b", "c", "d" or "q" to quit: ')
    print('You entered the choice: ',MyInput)

    if MyInput == 'a':
        print('You have chosen part (a)')
        #plot the analytical predictions for y and vy
        
        #y
        plt.title("Position against time starting at 1000m")
        NumPoints = 200
        
        #Plot the height against time
        xmin = 0.0
        xmax = T
        xvals = np.linspace(xmin,xmax,NumPoints)
        yvals = np.zeros(NumPoints)
        for i in range(NumPoints):
            yvals[i] = Func_y(xvals[i])
        plt.plot(xvals,yvals)
        plt.xlabel("Time (s)")
        plt.ylabel("Height (m)")
        plt.show()
        
        #vy
        plt.title("velocity against time starting stationary")
        
        #This graph uses the same range for the x-axis
        #Plot of the velocity against time
        vyvals = np.zeros(NumPoints)
        for i in range(NumPoints):
            vyvals[i] = Func_vy(xvals[i])
        plt.plot(xvals,vyvals)
        plt.xlabel("Time (s)")
        plt.ylabel("velocity (m/s)")
        plt.show()
               
    elif MyInput == 'b':
        print('You have chosen part (b)')
        #solve using euler method
        
        #The time iteration dt
        dt = 0.01
        
        #arrays for each variable
        t = []
        vy = []
        y = []
        
        t2 = 0
        y2 = 1000
        vy2 = 0
        
        i = 0
        #euler method where t,y,vy are increased and appended onto the arrays 
        while y2 >= 0:
            t.append(t2)
            y.append(y2)
            vy.append(vy2)
            
            t2 = t[i]+dt
            y2 = y[i]+(dt)*(vy[i])
            vy2 = vy[i]-(dt)*(g+(k0/m)*vy[i]*abs(vy[i]))
            
            i += 1
        
        #Plot the height against time
        plt.title("Position against time starting at 1000m using euler method")
        plt.plot(t,y)
        plt.xlabel("Time (s)")
        plt.ylabel("Height (m)")
        plt.show()
        
        #Plot of the velocity against time
        plt.title("velocity against time starting stationary using the euler method")
        plt.plot(t,vy)
        plt.xlabel("Time (s)")
        plt.ylabel("velocity (m/s)")
        plt.show()
        
    elif MyInput == 'c':
        print('You have chosen part (c)')
        #add air density as a function of height
        
         #The time iteration dt
        dt = 0.01
        
        #arrays for each variable
        t = []
        vy = []
        y = []
        
        i = 0
        t2 = 0
        y2 =  39045
        vy2 = 0
        
        #euler method where t,y,vy are increased and appended onto the arrays 
        while y2 >= 0:
            #append current t2,y2,vy2,for current i
            t.append(t2)
            y.append(y2)
            vy.append(vy2)
            
            #increase t2,y2,vy2 for next term (i+1)
            t2 = t[i]+dt
            y2 = y[i]+(dt)*(vy[i])
            vy2 = vy[i]-(dt)*(g+((0.5*C*rho0*np.exp(-y[i]/h)*A)/m)*vy[i]*abs(vy[i]))
            
            #iterate i
            i += 1
        
        #Plot the height against time
        plt.title("Position against time starting at 1000m using euler method with varying air density")
        plt.plot(t,y)
        plt.xlabel("Time (s)")
        plt.ylabel("Height (m)")
        plt.show()
        
        #Plot of the velocity against time
        plt.title("velocity against time starting stationary using the euler method with varying air density")
        plt.plot(t,vy)
        plt.xlabel("Time (s)")
        plt.ylabel("velocity (m/s)")
        plt.show()
            
    elif MyInput == 'd':
        print('You have chosen part (d)')
        #vary jump parameters and see effect on vy max and total time
        
        #Enter values for CA/m
        Input_x = input('Enter a value for CA/m: ')
        x = float(Input_x)

        #Enter value for initial hight(y0)
        Input_y0 = input('Enter a value for initial height: ')
        y0 = float(Input_y0)
        
        #Using Euler mthod as above
        t = []
        vy = []
        y = []
        
        i = 0
        t2 = 0
        y2 = y0
        vy2 = 0
        
        #euler method where t,y,vy are increased and appended onto the arrays 
        while y2 >= 0:
            #append current t2,y2,vy2,for current i
            t.append(t2)
            y.append(y2)
            vy.append(vy2)
            
            #increase t2,y2,vy2 for next term (i+1)
            t2 = t[i]+dt
            y2 = y[i]+(dt)*(vy[i])
            vy2 = vy[i]-(dt)*(g+(0.5*rho0*np.exp(-y[i]/h)*x)*vy[i]*abs(vy[i]))
            
            #iterate i
            i += 1
            
        print("Total jump duration:",t[len(t)-1],"s")
        
        min = np.amin(vy)
        
        print("Maximum speed achieved:",abs(min),"m/s")
        
    elif MyInput != 'q':
        print('This is not a valid choice')
        
print('You have chosen to finish - goodbye.')