#!/usr/bin/env python3
# -*- encoding: utf-8 -*-

YEARS=34
TARGET=25000
ACCUMULATED=20003
RATE=0.0945

def getSum(n, rate):
	s=0
	for i in range(1, n):
		s+=pow(1+rate, i)
	return s

targetAccum = TARGET * 10 * 12
annualContribution = (targetAccum - ACCUMULATED*pow(1+RATE, YEARS))/getSum(YEARS, RATE)

print('Expected Annual Contribution:')
print("{:.2f}".format(annualContribution))