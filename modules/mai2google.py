#!/usr/bin/env python3

import json
import sys
import subprocess
import datetime


cname = sys.argv[1]

year = datetime.datetime.now().year

schedule = json.loads(sys.stdin.read())

subprocess.run(['sh', '-c', "yes y | gcalcli --calendar="+cname+" delete '*' 00:00"])

for date, lectures in schedule.items():
  for lecture in lectures:
    place = lecture.get('place') or ""
    if place[:2:] == '--':
      place = ""
    args = ['gcalcli', 
            '--calendar='+cname, 
            'add', 
            '--title', lecture['name'], 
            '--where',  place,
            '--when', str(year)+"-"+('-'.join(date.split('.')[::-1])) + ' ' + lecture['start'],
            '--duration', "90",
            '--description', (lecture.get('type') or "Неизвестный тип") + ", " + (lecture.get('lecturer') or "Неизвестный преподаватель"),
            '--reminder', '15m'
    ]
    print(args)
    subprocess.run(args)
