#!/usr/bin/env nix-shell
#!nix-shell -p "python3.withPackages (ps: with ps; [flask])" -p pandoc -i python3

from flask import Flask, request
import sys
import subprocess
import json

app = Flask(__name__)

f = "/var/lib/important/index.md"

@app.route('/')
def index():
  content = open(f).read()
  return "<title>Редактирование index.md</title><body><form action='/post' method=POST><textarea cols=150 rows=60 name='content'>" + content + "</textarea><br><input type=submit /></form></body>"

@app.route('/post', methods = ['POST'])
def post():
  open(f, "w").write(request.form.get('content'))
  subprocess.run(["pandoc", "/var/lib/important/index.md", "-o", "/var/lib/important/index.html"])
  return ("", 303, {'Location': '/'})

if __name__ == "__main__":
  app.run('0.0.0.0', 1338)
