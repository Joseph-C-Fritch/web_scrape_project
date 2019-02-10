from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import csv
import re

# Windows users need to specify the path to chrome driver you just downloaded.
# You need to unzip the zipfile first and move the .exe file to any folder you want.
# driver = webdriver.Chrome(r'path\to\where\you\download\the\chromedriver.exe')
driver = webdriver.Chrome()

#Search for #XRP
driver.get('https://old.reddit.com/r/Ripple/search?q=Questions+and+Price+Predictions&restrict_sr=on')


# Windows users need to open the file using 'wb'
csv_file = open('posts.csv', 'w', encoding='utf-8')
writer = csv.writer(csv_file)

#initiate variables and index
links= []
index = 0
while index < 10:
	try:
		#Count & print
		index = index + 1
		print(index)

		#Get links on current page
		#wait_post = WebDriverWait(driver, 20)
		#posts = wait_post.until(EC.presence_of_all_elements_located((By.XPATH,'//a[contains(@href,"daily_ripplexrp_discussion_thread")]')))
		posts = driver.find_elements_by_xpath('//a[contains(@href,"daily_ripplexrp_discussion_thread")]')

		#Remove duplicates and update list
		for post in posts:
			links.append(post.get_attribute("href"))
		links = list(set(links))
		print(len(links))
	 

		#Click on next page
		next_button = driver.find_element_by_xpath('//a[contains(@rel,"nofollow next")]')
		next_button.click()


	except Exception as e:
			print(e)
			csv_file.close()
			driver.close()
			break

#Print Out Links
for link in links:
	#Set up link dictionary
	links_dict = {}
	links_dict['link'] = link
	writer.writerow(links_dict.values())

csv_file.close()
driver.close()


			

