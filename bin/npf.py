#!/usr/bin/env python3
# -*- encoding: utf-8 -*-

YEARS_ACCUM=34
TARGET=21_000/3
ACCUMULATED=20612.88 
RATE=0.095
INFLATION=0.1
YEARS_PAYOFF=20

def sigma_sum(start, end, expression):
    return sum(expression(i) for i in range(start, end))

def accum_benefit(a, k):
	if k==1: return (a-payoff(1))*RATE
	return (a - sigma_sum(1, k, lambda t: payoff(t)) + sigma_sum(1, k-1,lambda i: accum_benefit(a, i)))*RATE

def payoff(k):
	return TARGET*pow(1+INFLATION, YEARS_ACCUM+k)

payoff_sum = sigma_sum(1, YEARS_PAYOFF, lambda k: payoff(k))

def getTargetAccum(x1, x2):
	eps = 1
	def f(x):
		return x + sigma_sum(1, YEARS_PAYOFF, lambda k:accum_benefit(x, k)) - payoff_sum
	while True:
		x = x2 - f(x2) * (x2-x1)/(f(x2)-f(x1))
		if(abs(f(x))<eps): return x
		x2=x
		x1=x2

targetAccum = getTargetAccum(payoff_sum/1000, payoff_sum*1000)

annualContribution = (targetAccum - ACCUMULATED*pow(1+RATE, YEARS_ACCUM))/sigma_sum(1,YEARS_ACCUM,lambda n: pow(1+RATE,n))

print('Expected Annual Contribution:')
print("{:.2f}".format(annualContribution))