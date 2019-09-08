#!/usr/bin/env nix-shell
#!nix-shell -p "python3.withPackages (ps: with ps; [beautifulsoup4 html5lib flask ruamel_yaml])" -i python3

from bs4 import BeautifulSoup
from flask import Flask, jsonify
import sys
import json
import ruamel.yaml

import urllib.request
import urllib.parse


from ruamel.yaml import YAML
from ruamel.yaml.compat import StringIO

class MyYAML(ruamel.yaml.YAML):
  def dump(self, data, stream=None, **kw):
    inefficient = False
    if stream is None:
      inefficient = True
      stream = StringIO()
      YAML.dump(self, data, stream, **kw)
    if inefficient:
      return stream.getvalue()

yml = MyYAML()   # or typ='safe'/'unsafe' etc

app = Flask(__name__)


def get_schedule(group):
  fp = urllib.request.urlopen("https://mai.ru/education/schedule/detail.php?group="+urllib.parse.quote(group))
  mybytes = fp.read()

  html = mybytes.decode("utf8")
  fp.close()

  parsed_html = BeautifulSoup(html, "html5lib")
  days_html = parsed_html.find_all("div", "sc-table-day")

  schedule = {}

  for day in days_html:
    date = day.find("div", "sc-day-header").text[:5:]
    lectures_html = day.find("div", "sc-table-detail-container").find_all("div", "sc-table-row")
    lectures = []
    for lecture in lectures_html:
      l = {}
      times = lecture.find("div", "sc-item-time").text
      start = times[:5:]
      end = times[-5::]
      name = lecture.find("span", "sc-title").text
      _type = lecture.find("div", "sc-item-type").text
      l = {'name': name, 'start': start, 'end': end, 'type': _type }
      try:
        l.update({'lecturer': lecture.find("span", "sc-lecturer").text})
      except:
        pass
      try:
        l.update({'place': lecture.find("div", "sc-item-location").text[1::]})
      except:
        pass
      lectures.append(l)
    schedule.update({date: lectures})
  return schedule

@app.route('/json/<group>')
def schedule_json(group):
  schedule = get_schedule(group)
  response = app.response_class(
    response=json.dumps(schedule, ensure_ascii=False),
    status=200,
    mimetype='application/json'
  )
  return response

@app.route('/<group>')
def schedule_readable(group):
  schedule = get_schedule(group)
  response = app.response_class(
    response=yml.dump(schedule),
    status=200,
    mimetype='application/x-yaml'
  )
  return response

if __name__ == '__main__':
  app.run('127.0.0.1', '1337')

