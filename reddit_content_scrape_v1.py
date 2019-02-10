from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import csv
import re
import pandas as pd

# Windows users need to specify the path to chrome driver you just downloaded.
# You need to unzip the zipfile first and move the .exe file to any folder you want.
# driver = webdriver.Chrome(r'path\to\where\you\download\the\chromedriver.exe')
driver = webdriver.Chrome()

# Windows users need to open the file using 'wb'
csv_file = open('content.csv', 'w', encoding='utf-8')
writer = csv.writer(csv_file)

df = pd.read_csv("links.csv")
links = list(df.links)
count = 0
for link in links:
	try:
		count = count + 1
		print('count=',count)
		print('link=',link)

		#Search through each link
		driver.get(link)

		#Find all post
		posts = driver.find_elements_by_xpath('//div[contains(@data-type,"comment")]')
		print(len(posts))	

		#Grab Post
		for post in posts:
			text = post.find_element_by_xpath('.//div[contains(@class,"md")]').text
			author = post.get_attribute("data-author")
			datetime = post.find_element_by_xpath('.//p[@class="tagline"]/time').get_attribute('datetime')
			post_dict = {}

			post_dict['text'] = text
			post_dict['author'] = author
			post_dict['datetime'] = datetime


			writer.writerow(post_dict.values())

	
	except Exception as e:
			print(e)
			csv_file.close()
			driver.close()
			break

csv_file.close()
driver.close()


			

