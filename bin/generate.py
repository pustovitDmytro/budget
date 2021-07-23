#!/usr/bin/env python3
# -*- encoding: utf-8 -*-
from abc import ABCMeta
import re
import os
import csv
from datetime import date,datetime
import random

BANKS = [ "PrivatBank", 'monobank', "oschadbank", "Raiffeisen Bank Aval", "Crédit Agricole", "Pumb", "Ukr Exim", "Alfa"]
BANK_HOLDINGS = ["Current Account", "Savings Account", "Short Deposit", "Long Deposit"] 
BANK_CURRENCIES = ['UAH', 'USD', 'EUR', 'CHF']
BANK_CURRENCIES_WEIGHTS = (10, 10, 5, 2)
UNIQUE_HOLDINGS = {
  "Cash": { "CURRENCIES": ["UAH", "USD"], "PLACES": ["Cash"] },
  "Domestic Bonds": { "CURRENCIES": ["UAH", "EUR"], "PLACES": ["Freedom Finance", "Interactive brokers", "PrivatBank"] },
  "Shares": { "CURRENCIES": [ "USD" ], "PLACES": [ "Interactive brokers" ] },
  "Cryptocurrency": { "CURRENCIES": ["BTC"], "PLACES": ["Interactive brokers"] },
  "Bank Metals": { "CURRENCIES": ['GLD'], "PLACES": ["PrivatBank", 'Crédit Agricole', "Interactive brokers"] }
}

EXPENSES = ['Eat', 'Public transport', 'Entertain', 'health', 'tech', 'stuff', 'charity', 'rent']
PROFIT_TYPES = ["Savings Account", "Short Deposit", "Long Deposit", "Domestic Bonds", "Shares" ]
INCOME = ['Salary']
MIXED = ['Gifts']

CURRENCY_RATIO = {
	'UAH': 45, 
	'USD': 2, 
	'EUR': 1,
	'CHF': 0.2,
	'GLD': .5*1/1000,
	'BTC': .5*1/30000,
}

CURRENCIES = CURRENCY_RATIO.keys()

FULL_COVERAGE_BANK_ITEMS = len(BANKS)*len(BANK_HOLDINGS)*len(BANK_CURRENCIES)
BANK_ITEMS = 30

def last_day_of_month(any_day): #https://stackoverflow.com/questions/42950/how-to-get-the-last-day-of-the-month
    next_month = any_day.replace(day=28) + datetime.timedelta(days=4)
    return next_month - datetime.timedelta(days=next_month.day)

print('FULL_COVERAGE_BANK_ITEMS:', FULL_COVERAGE_BANK_ITEMS)

def shorten(value):
	return "".join(map(lambda x: x[:4].capitalize(), value.split()))

def transponse_dicts(dicts, main='Name'):
	keys = dicts[0].keys()
	res = []
	for key in keys:
		item ={}
		for dict in dicts:
			item[dict[main]] = dict[key]
		res.append(item)
	return res

class Generator():
	__metaclass__ = ABCMeta
	def __init__(self, period=15, seed = 1000, flowsBonus=1.05, delta = 1.005):
		self.period = period
		self.seed = seed
		self.delta = delta
		self.flowsBonus = flowsBonus
		self.today = date.today()
	
	def generate_diff(self, prev):
		diff = {}
		for currency in CURRENCIES:
			diff[currency] = prev[currency] * random.gauss(self.delta, 0.1)
		return diff

	def seed_abs(self):
		absolute = {}
		for currency, ratio in CURRENCY_RATIO.items():
			absolute[currency] = self.seed*ratio
		return absolute

	def generate_abs(self, prev, diff):
		absolute = {}
		for currency in CURRENCIES:
			absolute[currency] = prev[currency] + diff[currency]
		return absolute

	def generate(self):
		self.expected = [{ 'abs': self.seed_abs() }]
		for i in range(1, self.period):
			item = { 'diff': self.generate_diff(self.expected[i-1]['abs']) }
			item['abs'] = self.generate_abs(self.expected[i-1]['abs'], item['diff'])
			self.expected.append(item)

		self.generate_holdings()
		self.generate_flows()

	def get_holding_name(self, *args):
		if args[0]==args[1]:
			return self.get_holding_name(*args[1:])
		return " ".join(list(map(lambda x: shorten(x), args)))
	
	def generate_flows(self):
		self.flowsHeaders = []
		self.flowsData = []

		directTypes = EXPENSES + INCOME + MIXED
		orderedCurrencies = random.choices(BANK_CURRENCIES, weights=BANK_CURRENCIES_WEIGHTS, k=len(directTypes))
		for i, expense in enumerate(directTypes):
			self.flowsHeaders.append({
				'Name': expense,
				"Currency": orderedCurrencies[i],
				'Source': None
			})
		for holding in self.holdingsHeaders:
			if holding['Type'] in PROFIT_TYPES:
				self.flowsHeaders.append({
					'Name': holding['Name'],
					"Currency": holding['Currency'],
					'Source': holding['Name']
				})

		print('FLOWS_HEADERS: ', len(self.flowsHeaders))

		# DATA
		for expect in self.expected:
			if 'diff' not in expect.keys(): continue
			diff = expect['diff']
			item={}
			for currency in CURRENCIES:
				filteredHeaders = list(filter(lambda h: h['Currency'] == currency, self.flowsHeaders))
				distribution = list(map(lambda x: -random.random() if x['Name'] in EXPENSES else random.random(), filteredHeaders))
				normalizedDistribution = [x/sum(distribution) for x in distribution]
				for ind, header in enumerate(filteredHeaders): item[header['Name']] = normalizedDistribution[ind] * diff[currency]
			self.flowsData.append(item)


	def generate_holdings(self):
		self.holdingsData=[]
		self.holdingsHeaders=[]
		
		# HEADERS
		for holding, value in UNIQUE_HOLDINGS.items():
			for currency in value['CURRENCIES']:
				for place in value['PLACES']:
					header = {
						"Name": self.get_holding_name(place, holding, currency),
						"Currency": currency,
						"Type": holding,
						"Place": place
					}
					self.holdingsHeaders.append(header)

		orderedCurrencies = random.choices(BANK_CURRENCIES, weights=BANK_CURRENCIES_WEIGHTS, k=BANK_ITEMS)
		orderedHoldings = random.choices(BANK_HOLDINGS, k=BANK_ITEMS)
		orderedBanks = random.choices(BANKS, k=BANK_ITEMS)
		for i in range(BANK_ITEMS):
			header = {
				"Name": 'Account *' + str(random.randint(1000, 9999)),
				"Currency": orderedCurrencies[i],
				"Type": orderedHoldings[i],
				"Place": orderedBanks[i]
			}
			self.holdingsHeaders.append(header)
		

		print('HOLDINGS_HEADERS: ', len(self.holdingsHeaders))

		# DATA
		for expect in self.expected:
			abs = expect['abs']
			item={}
			for currency in CURRENCIES:
				filteredHeaders = list(filter(lambda h: h['Currency'] == currency, self.holdingsHeaders))
				distribution = list(map(lambda x: random.random(), filteredHeaders))
				normalizedDistribution = [x/sum(distribution) for x in distribution]
				for ind, header in enumerate(filteredHeaders): item[header['Name']] = normalizedDistribution[ind] * abs[currency]
			self.holdingsData.append(item)
	
	def save(self, folder):
		with open(folder+'/flows.csv', 'w', encoding='utf-8') as csvfile:
			fieldnames =  [x['Name'] for x in self.flowsHeaders]
			writer = csv.DictWriter(csvfile, fieldnames=fieldnames,delimiter =",")

			for row in transponse_dicts(self.flowsHeaders): writer.writerow(row)
			for row in self.flowsData: writer.writerow(row)

		with open(folder+'/holdings.csv', 'w', encoding='utf-8') as csvfile:
			fieldnames =  [x['Name'] for x in self.holdingsHeaders]
			writer = csv.DictWriter(csvfile, fieldnames=fieldnames,delimiter =",")

			for row in transponse_dicts(self.holdingsHeaders): writer.writerow(row)
			for row in self.holdingsData: writer.writerow(row)

gen = Generator()
gen.generate()
gen.save('./files')