#!/usr/bin/env nix-shell
#!nix-shell -p "python3.withPackages (ps: with ps; [flask])" -p pandoc -i python3

from flask import Flask, request
import sys
import subprocess
import json

app = Flask(__name__)

f = "/var/lib/important/"

@app.route('/')
def index():
  return ("Redirecting to index.md", 303, {"Location": "/index.md"})

@app.route('/<name>', methods = ["GET"])
def edit(name):
  content = open(f + name).read()
  return "<title>Редактирование index.md</title><body><form action='/" + name + "' method=POST><textarea cols=150 rows=60 name='content'>" + content + "</textarea><br><input type=submit /></form></body>"

@app.route('/<name>', methods = ['POST'])
def post(name):
  open(f + name, "w").write(request.form.get('content'))
  if name == "index.md":
    subprocess.run(["pandoc", "--css", "/var/lib/important/pandoc.css", "/var/lib/important/index.md", "-o", "/var/lib/important/index.html"])
  return ("", 303, {'Location': '/'+name})

if __name__ == "__main__":
  app.run('127.0.0.1', 1338)
