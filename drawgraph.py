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
res = requests.post('https://api.renren.com/restserver.do',
                    data={
                        'method': 'users.getInfo',
                        'v': '1.0',
                        'access_token': token,
                        'format': 'json',
                        'uids': ','.join(map(lambda t: t[0], users)),
                        'fields': 'uid,name,mainurl'
                    }
                    ).json()
for i in users:
    for j in res:
        if i[0] == j['uid']:
            i[2] = j['mainurl']
for i in users:
    timg = Image.open(StringIO(requests.get(i[2]).content))
    avatars.append(timg.resize((60, 60), Image.ANTIALIAS))

img = Image.new('RGB', (500, 160))
for x in xrange(500):
    for y in xrange(160):
        img.putpixel((x, y), (255, 255, 255))

j = 0
for i in avatars:
    if j == 7:
        break
    for x in xrange(60):
        for y in xrange(60):
            v = i.getpixel((x, y))
            img.putpixel((10 + 70 * j + x, y + 80), v)
    j += 1

font = ImageFont.truetype("wqy-microhei.ttc", 24)
draw = ImageDraw.Draw(img)
draw.text((116, 28), unicode(target, 'UTF-8'), (0, 0, 0), font=font)
zuijinlaifang = Image.open(open('word-zuijinlaifang.png'))
width, height = zuijinlaifang.size
for i in range(5, 5 + width):
    for j in range(27, 27 + height):
        img.putpixel((i, j), zuijinlaifang.getpixel((i - 5, j - 27)))

img.save(uid + ".png")

aid = None


try:
    albs = requests.post('https://api.renren.com/restserver.do',
                data={
                    'method': 'photos.getAlbums',
                    'v': '1.0',
                    'access_token': token,
                    'format': 'json',
                    'uid': uid,
                    'count': '1000'
                }
                ).json()
    for i in albs:
        if i['name'] == u"来访截图":
            aid = i['aid']
    if not aid:
        for i in range(3):
            albs = requests.post('https://api.renren.com/v2/album/put',
                    data={
                        'access_token': token,
                        'name': u'来访截图'
                    }
                    ).json()
            if u'response' in albs and u'id' in albs[u'response']:
                aid = albs[u'response'][u'id']
                if aid:
                    break
except:
    pass

data = {
    "v": "1.0",
    "access_token": token,
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
    requests.post("https://api.renren.com/restserver.do", data={
        "v": "1.0",
        "access_token": token,
        "format": 'json',
        'method': 'photos.tag',
        'photo_id': pic['pid'],
        'owner_id': uid,
        'photo_width': 500,
        'photo_height': 160,
        'frame_width': 60,
        'frame_height': 60,
        'tagged_user_id': user[0],
        'top': '80',
        'left': 10 + j * 70
    }).json()
    j += 1

os.unlink(uid + ".png")
os.unlink(uid + ".txt")
