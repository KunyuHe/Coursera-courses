import urllib.request, urllib.parse, urllib.error
import json

# add an API endpoint which has a static subset of the GoogleGeo Data
serviceurl = 'http://py4e-data.dr-chuck.net/geojson?'

while True:
    address = input('Enter location: ')
    print('Enter location: ', address)
    if len(address) < 1: break

    url = serviceurl + urllib.parse.urlencode({'address': address})

    print('Retrieving', url)
    uh = urllib.request.urlopen(url)
    data = uh.read().decode()
    print('Retrieved', len(data), 'characters')

    try:
        js = json.loads(data)
    except:
        js = None
    if not js or 'status' not in js or js['status'] != 'OK':
        print('!!! Failure To Browse Address !!!')
        continue

    placeid = js['results'][0]['place_id']
    print('Place id:', placeid)
