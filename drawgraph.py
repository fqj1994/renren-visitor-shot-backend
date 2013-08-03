#!/usr/bin/env python
# coding: utf-8
import sys
import Image
import ImageFont
import ImageDraw
import requests
import urllib
import os
from StringIO import StringIO
info = open(sys.argv[1],'r').read().split('\n')
uid = info[0]
token = info[1]
target = info[2]
users = [i.split(' ') for i in info[3:]]
while users[-1] == ['']:
    users = users[:-1]
avatars = []
for i in users:
    timg = Image.open(StringIO(requests.get(i[2]).content))
    avatars.append(timg.resize((30, 30)))

img = Image.new('RGB', (250, 80))
for x in xrange(250):
    for y in xrange(80):
        img.putpixel((x, y), (255, 255, 255))

j = 0
for i in avatars:
    if j == 7:
        break
    for x in xrange(30):
        for y in xrange(30):
            v = i.getpixel((x, y))
            img.putpixel((5 + 35 * j + x, y + 40), v)
    j += 1

font = ImageFont.truetype("wqy-microhei.ttc", 12)
fonthei = ImageFont.truetype("wqy-zenhei.ttc", 12)
draw = ImageDraw.Draw(img)
draw.text((5, 13), unicode("最近来访 ", 'UTF-8'), (0, 0, 0), font=fonthei)
draw.text((58, 14), unicode(target, 'UTF-8'), (0, 0, 0), font=font)

img.save(uid + ".png")

aid = None


try:
    albs = requests.post('https://api.renren.com/restserver.do',
                data={
                    'method': 'photos.getAlbums',
                    'v': '1.0',
                    'access_token': urllib.unquote_plus(token),
                    'format': 'json',
                    'uid': uid
                }
                ).json()
    for i in albs:
        if i['name'] == u"来访截图":
            aid = i['aid']
except:
    pass

data = {
    "v": "1.0",
    "access_token": urllib.unquote_plus(token),
    'format': 'json',
    'method': 'photos.upload',
    'caption': ''.join(["@" + urllib.unquote_plus(p[1]) + "(" + p[0] + ") " for p in users[0:7]]),
}
if aid:
    data['aid'] = aid

pic = requests.post("https://api.renren.com/restserver.do", data=data,
                    files = {
                        'upload': ('upload.png', open(uid + '.png').read())
                    }
                    ).json()

j = 0
for user in users[0:7]:
    print requests.post("https://api.renren.com/restserver.do", data={
        "v": "1.0",
        "access_token": urllib.unquote_plus(token),
        "format": 'json',
        'method': 'photos.tag',
        'photo_id': pic['pid'],
        'owner_id': uid,
        'photo_width': 250,
        'photo_height': 80,
        'frame_width': 30,
        'frame_height': 30,
        'tagged_user_id': user[0],
        'top': '40',
        'left': 5 + j * 35
    }).json()
    j += 1

os.unlink(uid + ".png")
os.unlink(uid + ".txt")
