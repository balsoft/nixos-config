{ python3, iconfont, config, ... }: ''
  #!${python3}/bin/python3
  import imaplib
  obj = imaplib.IMAP4_SSL('imap.${config.secrets.mail.host}', 993)
  obj.login("${config.secrets.mail.user}", "${config.secrets.mail.password}")
  obj.select()
  l = len(obj.search(None, 'unseen')[1][0].split())
  print('<span font="${iconfont}"></span> %s' % str(l))
  exit(33 if l != 0 else 0)
''
