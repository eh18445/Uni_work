# -*- coding: utf-8 -*-
"""
Created on Sun Mar  6 10:35:53 2022

@author: in17746
"""

class NotSoRandom(object):
    def seed(self, a=3):
        """Seed the world's most mysterious random number generator."""
        self.seedval = a
    def random(self):
        """Look, random numbers!"""
        self.seedval = (self.seedval * 3) % 13
        return self.seedval

_inst = NotSoRandom()
seed = _inst.seed
random = _inst.random