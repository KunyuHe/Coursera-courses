import urllib.request, urllib.parse, urllib.error
import json

# read data from web page
url = input('Enter url - ')
print('Enter Location:', url)
print('Retrieving:', url)
uh = urllib.request.urlopen(url)
data = uh.read()
print('Retrieved:', len(data), 'characters')

total = 0
info = json.loads(data)
users = info["comments"]
print("Count:", len(users))
for user in users:
    count = int(user["count"])
    total = total + count
print("Sum:", total)
