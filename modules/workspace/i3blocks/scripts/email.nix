{ python3, iconfont, config, ... }: ''
  #!${python3}/bin/python3
  import imaplib
  obj = imaplib.IMAP4_SSL('balsoft.ru', 993)
  obj.login("balsoft@balsoft.ru", "${config.secrets.gnome-keyring-envsubst-email.decrypted}")
  obj.select()
  l = len(obj.search(None, 'unseen')[1][0].split())
  if l == 0:
    print('')
  else:
    print('<span font="${iconfont}">﯍</span> %s' % str(l))
    exit(33)
''
