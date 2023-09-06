# -*- coding: utf-8 -*-
"""
Created on Wed Feb  5 11:56:56 2020

@author: Daniel Johnstone
"""
import numpy as np
import matplotlib.pyplot as plt

c = 3*10**8
e0 = 8.85*10**(-12)

def INT(x,ap,z,N):
    """
    This function calculates the integral used in all parts where x is the screen coordinate,
    ap is the aperture size, z is the distance of the screen and N is the number of
    points used in the integral.
    """
    
    #F is an array (1,2,4,2,...,2,4,1)
    F = np.ones(N+1)
    for i in range(N):
        if i == 0 or i == (N):
            F[i] = 1
        elif (i%2) == 0:
            F[i] = 2
        elif (i%2) == 1:
            F[i] = 4
            
    
    #h is space between points used in integral
    #ap is aperture size and N the number of points
    h = ap/N
    
    #xdash is the array with each value of x'
    xdash = np.linspace(-ap/2,ap/2,N+1)
    
    #f is the function being integrated
    f = np.ones(N+1, dtype=complex)
    for i in range(N):
        f[i] = np.exp(1j*k*((x-xdash[i])**2)*((2*z)**(-1)))
        
    #S is the series where each value is the product of f and F at each point.
    S = np.zeros(N+1, dtype=complex)
    for i in range(N):
        S[i] = F[i]*f[i]
        
    S = np.sum(S)
    INT = h*S*3**(-1)
    return INT
    
MyInput = '0'
while MyInput != 'q':
    MyInput = input('Enter a choice, "a" for 1D integral, "b" to enter values for aperture size and distance to screen, "c" for a 2D integral or "q" to quit: ')
    print('You entered the choice: ',MyInput)

    if MyInput == 'a':
        print('You have chosen part (a).')
        #1D integral
        print('Calculating the intensity at 2cm from the aperture...')
        
        #Values and constants used
        lambd = 1*10**(-6)
        k = 2*np.pi*lambd**(-1)
        E0 = 0.1
        ap = 2*10**(-5)
        z = 2*10**(-2)
        N = 100
        #Nx is the number of points on the screen
        Nx = 1000
        xmin = -5*10**(-3)
        xmax = 5*10**(-3)
        
        #x is the array of x values used in the graph
        x = np.linspace(xmin,xmax,Nx)
        #E is the array calculated from the INT function and I is calculated from this
        E = np.zeros(Nx, dtype=complex)
        I = np.zeros(Nx)
        
        for i in range(Nx):
            E[i] = k*E0*INT(x[i],ap,z,N)*(2*np.pi*z)**(-1)
            I[i] = (np.abs(E[i]))**2*c*e0
        
        #plot of the graph
        plt.plot(x,I)
        plt.xlabel("x (m)")
        plt.ylabel("Relative Intensity (W/m)")
        plt.show()
        
        #plt.savefig('1Dgraph(a).jpg')
        
    elif MyInput == 'b':
        print('You have chosen part (b)')
        
        print("Enter values for aperture size, screen distance and N")
        
        #This makes sure that the input for aperture separation is positive
        u=0
        while u != "q":
            Input_ap = input('Enter a value for aperture size in metres (positive floating point number): ')            
            ap = float(Input_ap)
            if ap >= 0:
                u = "q"
            if ap < 0:
                print("This isn't a valid input as it is negative")
        
        #This makes sure the value for screen distance is positive
        u=0
        while u != "q":
            Input_z = input('Enter a value for screen distance in metres (positive floating point number): ')
            z = float(Input_z)
            if z >= 0:
                u = "q"
            if z < 0:
                print("This isn't a valid input as it is negative")
            
        #This makes sure that the value of N inputed is a positive integer and even
        u = 0
        while u != "q":
            Input_N = input('Enter a value for N (even positive integer): ')
            N = int(Input_N)
            if N >= 0 and N%2==0:
                u = "q"
            elif N < 0 and N%2==1:
                print("This isn't a valid input as it is negative and odd")
            elif N%2==1:
                print("This isn't a valid input as it is odd")
            elif N < 0:
                print("This isn't a valid input as it is negative")
            
        print('Calculating the intensity at', z, 'metres from the aperture...')
        
        #constants
        lambd = 1*10**(-6)
        k = 2*np.pi*lambd**(-1)
        E0 = 0.1
        
        Nx = 1000
        xmin = -5*10**(-3)
        xmax = 5*10**(-3)
        x = np.linspace(xmin,xmax,Nx)
        E = np.zeros(Nx, dtype=complex)
        I = np.zeros(Nx)
        
        for i in range(Nx):
            E[i] = k*E0*INT(x[i],ap,z,N)*(2*np.pi*z)**(-1)
            I[i] = (np.abs(E[i]))**2*c*e0
        
        plt.plot(x,I)
        plt.xlabel("x (m)")
        plt.ylabel("Relative Intensity (W/m)")
        plt.show()
        
        #plt.savefig('1Dgraph(b)2.jpg')
        
    elif MyInput == 'c':
        print('You have chosen part (c)')
        
        #constants
        lambd = 1*10**(-6)
        k = 2*np.pi*lambd**(-1)
        E0 = 1000
        ap = 2*10**(-5)
        z = 2*10**(-2)
        N = 100
        Nx = 100
        
        print('Calculating the intensity at', z, 'metres from the aperture...')
        
        #min and max values for x and y
        xmin = -4*10**(-3)
        xmax = 4*10**(-3)
        
        x = np.linspace(xmin,xmax,Nx)
        y = np.linspace(xmin,xmax,Nx)
        
        E = np.zeros((Nx,Nx), dtype=complex)
        I = np.zeros((Nx,Nx))
        
        #This will give the intensity in 2 dimensions
        for i in range(Nx):
            for j in range(Nx):
                E[i,j] = k*E0*INT(x[i],ap,z,N)*INT(y[j],ap,z,N)*(2*np.pi*z)**(-1)
                I[i,j] = (np.abs(E[i,j]))**2*c*e0
            
        plt.imshow(I, origin='lower', extent=[xmin,xmax,xmin,xmax])
        plt.xlabel("x position (m)")
        plt.ylabel("y position (m)")
        plt.colorbar(label="Relative Intensity (W/m^2)")
        #plt.show()
        
        plt.savefig('2Dgraph(c).jpg')

print('You have chosen to finish - goodbye.')