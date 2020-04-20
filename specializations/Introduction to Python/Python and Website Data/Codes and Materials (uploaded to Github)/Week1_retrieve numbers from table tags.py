# Sample
import urllib.request, urllib.parse, urllib.error
from bs4 import BeautifulSoup
import ssl
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE
url = input('Enter - ')
html = urllib.request.urlopen(url, context=ctx).read()
soup = BeautifulSoup(html, 'html.parser')
import re
nums = list()
tags = soup('span')
for tag in tags:
# transfer before retrieving numbers from tags
    tag = str(tag)
    num = re.findall("([0-9]+)", tag)
    for add in num:
        cell = int(add)
    nums.append(cell)
print(sum(nums))

# Assignment
import urllib.request, urllib.parse, urllib.error
from bs4 import BeautifulSoup
import ssl
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE
url = input('Enter - ')
html = urllib.request.urlopen(url, context=ctx).read()
soup = BeautifulSoup(html, 'html.parser')
import re
nums = list()
tags = soup('span')
for tag in tags:
    tag = str(tag)
    num = re.findall("([0-9]+)", tag)
    for add in num:
        cell = int(add)
    nums.append(cell)
print(sum(nums))
