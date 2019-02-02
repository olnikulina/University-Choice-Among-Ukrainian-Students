import requests
from bs4 import BeautifulSoup

import sqlite3

def parse_stud(url):
	request = requests.get(url)
	request.encoding = 'UTF-8'

	soup = BeautifulSoup(request.text, 'lxml')
	stud = soup.find('div', attrs = {'class': 'row'})

	studs = []
	
	for tr in stud.table.tbody.findAll('tr'):
		stud_dict = {
			'order': '',
			'name': '',
			'priority': '',
			'mark': '',
			'status': '',
			'school_mark': '',
			'ukr_mark': '',
			'all_marks': ''
		}

		tds = [td.text.strip() for td in tr.findAll('td')]

		stud_dict['order'] = tds[0]
		stud_dict['name'] = tds[1]
		stud_dict['priority'] = tds[2]
		stud_dict['mark'] = tds[3]
		stud_dict['status'] = tds[4]
		for line in tds[5].split('\n'):
			line_splitted = line.split(': ')
			if line_splitted[0] == 'Середній бал документа про освіту':
				stud_dict['school_mark'] = line_splitted[1]
			elif line_splitted[0] == 'Українська мова та література (ЗНО)':
				stud_dict['ukr_mark'] = line_splitted[1]

		stud_dict['all_marks'] = '/'.join(tds[5].split('\n')) #'/'.join(tds[5].split()))
		stud_dict['quota'] = tds[6].strip()
		stud_dict['orig'] = tds[7].strip()

		studs.append(stud_dict)

	return studs

		


def parse_vnz(url, region):
	request = requests.get('http://vstup.info/2018' + url)
	request.encoding = 'UTF-8'

	soup = BeautifulSoup(request.text, 'lxml')
	about = soup.find('table', attrs = {'id': 'about'})

	vnz_dict = {
		'name': '',
		'type': '',
		'ownership': ''
	}

	tds = [[td.text for td in row.findAll('td')] for row in about.findAll('tr')]
	vnz_dict['name'] = tds[0][1] 
	vnz_dict['type'] = tds[1][1] 
	vnz_dict['ownership'] = tds[2][1] 

	soup = BeautifulSoup(request.text.replace('<br>', '\n'), 'lxml')
	specialties = soup.find('table', attrs = {'id': 'denna2'})
	
	if specialties is not None:
		for spec in specialties.tbody.findAll('tr'):
			rows = [sp for sp in spec.findAll('td')]

			s = {
				'field': '',
				'specialty': '',
				'faculty': '',
				'type': '',
				'enrollment': '',
				'term': '',
				'licence': '',
				'edu_prog': '',
				'min_dz': '',
				'max_dz': '',
				'contact': '',
				'marks': ''
			}

			items = rows[0].text.split('\n')

			if items[0] != "Магістр (на основі ПЗСО 11кл.)":
				continue

			for i in range(len(items)):
				splitted_item = items[i].split(': ')
				if i == 0:
					s['level'] = items[i]

				if len(splitted_item) <= 1:
					continue
				
				if splitted_item[0] == '':
					pass
				elif splitted_item[0] == 'Галузь':
					s['field'] = splitted_item[1]
				elif splitted_item[0] == 'Спеціальність':
					s['specialty'] = splitted_item[1]
				elif splitted_item[0] == 'Факультет':
					s['faculty'] = splitted_item[1]
				elif splitted_item[0] == 'Тип пропозиції':
					s['type'] = splitted_item[1]
				elif splitted_item[0].split(' ')[0] == 'Зарахування':
					s['enrollment'] = splitted_item[0]
				elif splitted_item[0] == 'Термін навчання':
					s['term'] = splitted_item[1]
				elif splitted_item[0] == 'Ліцензійний обсяг':
					s['licence'] = splitted_item[1]
				elif splitted_item[0] == 'Освітня програма':
					s['edu_prog'] = splitted_item[1]
				elif splitted_item[0] == 'Мінімальний обсяг держ замовлення':
					s['min_dz'] = splitted_item[1]
				elif splitted_item[0] == 'Максимальний обсяг держ замовлення':
					s['max_dz'] = splitted_item[1]
				elif splitted_item[0] == 'Контракт':
					s['contact'] = splitted_item[1]

			s['marks'] = '/'.join([r.text for r in rows[2].findAll('li')])

			#добавить разбалловку		
			studs = parse_stud('http://vstup.info/2018' + rows[1].a['href'][1:])
			if studs is None:
				continue

			# print(studs)

			for stud_dict in studs:
				print(stud_dict['order'], stud_dict['name'], stud_dict['priority'], stud_dict['mark'], stud_dict['status'], stud_dict['school_mark'], stud_dict['ukr_mark'], stud_dict['all_marks'],
				stud_dict['quota'], rows[1].a['href'][1:], stud_dict['orig'], s['level'], s['field'], s['specialty'], s['faculty'], s['type'], s['enrollment'], s['term'], s['licence'], s['min_dz'], s['max_dz'], s['contact'], s['marks'],
				url, vnz_dict['name'], vnz_dict['type'], vnz_dict['ownership'], region, sep=';')


	print('\n------------------------------------------\n')

	# if specialties is not None:
	# 	for tb in specialties.findAll('tbody'):
	# 		print(tb)
			#findAll('tr', attrs = {'id': 'denna1'}))

def parse_region(url, region):
	request = requests.get(url)
	request.encoding = 'UTF-8'
	soup_vnz = BeautifulSoup(request.text, 'html.parser')

	univ_table = soup_vnz.find('table', attrs = {'id':'vnzt0'})
	if univ_table is not None:
		if univ_table.findAll('td') is not None:
			for td in univ_table.findAll('td'):
				url = 'http://vstup.info'+td.a['href']
				parse_vnz(td.a['href'][1:], region)
				# request = requests.get(url)
				# request.encoding = 'UTF-8'

	academ_table = soup_vnz.find('table', attrs = {'id':'vnzt1'})
	if academ_table is not None:
		if academ_table.findAll('td') is not None:
			for td in academ_table.findAll('td'):
				url = 'http://vstup.info'+td.a['href']
				parse_vnz(td.a['href'][1:], region)
				# request = requests.get(url)
				# request.encoding = 'UTF-8'

	inst_table = soup_vnz.find('table', attrs = {'id':'vnzt2'})
	if inst_table is not None:
		if inst_table.findAll('td') is not None:
			for td in inst_table.findAll('td'):
				url = 'http://vstup.info'+td.a['href'] 
				parse_vnz(td.a['href'][1:], region)
				# request = requests.get(url)
				# request.encoding = 'UTF-8'

	vidokr_table = soup_vnz.find('table', attrs = {'id':'vnzt6'})
	if vidokr_table is not None:
		if vidokr_table.findAll('td') is not None:
			for td in vidokr_table.findAll('td'):
				url = 'http://vstup.info'+td.a['href']
				parse_vnz(td.a['href'][1:], region)
				# request = requests.get(url)
				# request.encoding = 'UTF-8'

	hz_table = soup_vnz.find('table', attrs = {'id':'vnzt3'})
	if hz_table is not None:
		if hz_table.findAll('td') is not None:
			for td in hz_table.findAll('td'):
				url = 'http://vstup.info'+td.a['href']
				parse_vnz(td.a['href'][1:], region)


	kol_table = soup_vnz.find('table', attrs = {'id':'vnzt4'})
	if kol_table is not None:
		if kol_table.findAll('td') is not None:
			for td in kol_table.findAll('td'):
				url = 'http://vstup.info'+td.a['href']
				parse_vnz(td.a['href'][1:], region)


	uch_table = soup_vnz.find('table', attrs = {'id':'vnzt5'})
	if uch_table is not None:
		if uch_table.findAll('td') is not None:
			for td in uch_table.findAll('td'):
				url = 'http://vstup.info'+td.a['href']
				parse_vnz(td.a['href'][1:], region)

	

	# request = requests.get(url)
	# request.encoding = 'UTF-8'
	# soup_inst = BeautifulSoup(request.text, 'html.parser')
	# inst_table = soup_inst.find('table', attrs = {'id':'vnzt2'})

	# for td in inst_table.findAll('td'):
	# 	url = 'http://vstup.info'+td.a['href']
	# 	print(td.a['href'], td.a['title'])


	return

def parse_start_page(url = 'http://vstup.info/'):
	request = requests.get(url)
	request.encoding = 'UTF-8'

	soup = BeautifulSoup(request.text, 'html.parser')
	start_page = soup.find('table', attrs = {'id': '2018abet'})
	for a in start_page.findAll('a'):
		if a['href'][1] == '2': # проверка на рекламу
			href = a['href']
			title = a['title'].split(': ')[1]

			parse_region('http://vstup.info' + href, title)
	
	
	print('FINISH')
	return




parse_start_page(url='http://vstup.info/')




# parse_regions(url = 'http://vstup.info/') 