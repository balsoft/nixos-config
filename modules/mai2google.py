#!/usr/bin/env python3

import json
import sys
import subprocess


schedule = json.loads(sys.stdin.read())

subprocess.run(['sh', '-c', "yes y | gcalcli --calendar=MAI delete '*'"])

for date, lectures in schedule.items():
  for lecture in lectures:
    place = lecture.get('place') or ""
    if place[:2:] == '--':
      place = ""
    args = ['gcalcli', 
            '--calendar=MAI', 
            'add', 
            '--title', lecture['name'], 
            '--where',  place,
            '--when', date + ' ' + lecture['start'],
            '--duration', "90",
            '--description', (lecture.get('type') or "Неизвестный тип") + ", " + (lecture.get('lecturer') or "Неизвестный преподаватель"),
            '--reminder', '5m'
    ]
    print(args)
    subprocess.run(args)

