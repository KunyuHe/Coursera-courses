# Sample

import urllib.request, urllib.parse, urllib.error
import xml.etree.ElementTree as ET
# retrieve data from input url
url = input('Enter location: ')
print('Retrieving', url)
uh = urllib.request.urlopen(url)
data = uh.read()
print('Retrieved', len(data), 'characters')

total = 0
tree = ET.fromstring(data)
results = tree.findall("./comments/comment")
print("Count:", len(results))
for num in results:
    num = int(num.find('count').text)
    total = total + num
print("Sum:", total)
