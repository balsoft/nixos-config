{ pkgs, config, ... }: {
  home-manager.users.balsoft = {
    home.packages = [ pkgs.sylpheed ];
    wayland.windowManager.sway.config.startup = [
      {
        command = "${pkgs.sylpheed}/bin/sylpheed";
      }
    ];
    home.activation."accountrc" = {
      data = ''
        $DRY_RUN_CMD rm -f ~/.sylpheed-2.0/accountrc
        $DRY_RUN_CMD cp ~/.sylpheed-2.0/accountrc.home ~/.sylpheed-2.0/accountrc
        $DRY_RUN_CMD chmod 700 ~/.sylpheed-2.0/accountrc
      '';
      before = [ ];
      after = [ "linkGeneration" ];
    };
    home.file.".sylpheed-2.0/accountrc.home".text = ''
      [Account: 1]
      account_name=balsoft@balsoft.ru
      is_default=1
      name=Alexander Bantyev
      address=balsoft@balsoft.ru
      organization=
      protocol=3
      receive_server=balsoft.ru
      smtp_server=balsoft.ru
      nntp_server=
      use_nntp_auth=0
      user_id=balsoft@balsoft.ru
      password=${config.secrets.mail.password or ""}
      inbox=inbox
      use_apop_auth=0
      remove_mail=1
      message_leave_time=7
      get_all_mail=0
      enable_size_limit=0
      size_limit=1024
      filter_on_receive=1
      imap_check_inbox_only=0
      imap_filter_inbox_on_receive=0
      imap_auth_method=0
      max_nntp_articles=300
      receive_at_get_all=1
      add_date=1
      generate_msgid=1
      add_custom_header=0
      use_smtp_auth=1
      smtp_auth_method=0
      smtp_user_id=
      smtp_password=
      pop_before_smtp=0
      signature_type=2
      signature_path=/home/balsoft/.signature
      signature_name=Default
      signature_name2=Russian
      signature_name3=Short
      signature_name4=
      signature_name5=
      signature_name6=
      signature_name7=
      signature_name8=
      signature_name9=
      signature_name10=
      signature_text=Александр Бантьев /Alexander Bantyev/ aka balsoft\n\nNix DevOPS/SRE at serokell.io\n\n<balsoft@balsoft.ru>\n<alexander.bantyev@serokell.io>\n\nmatrix://@balsoft:balsoft.ru \n(https://matrix.to/#/@balsoft:balsoft.ru)\nhttps://t.me/balsoft\nhttps://github.com/balsoft\n
      signature_text2=Александр Бантьев (balsoft)\n\nNix DevOPS/SRE в serokell.io\n\n<balsoft@balsoft.ru>\n<alexander.bantyev@serokell.io>\n\nmatrix://@balsoft:balsoft.ru \n(https://matrix.to/#/@balsoft:balsoft.ru)\nhttps://t.me/balsoft\nhttps://github.com/balsoft\n
      signature_text3=Alexander Bantyev aka balsoft <balsoft@balsoft.ru>
      signature_text4=
      signature_text5=
      signature_text6=
      signature_text7=
      signature_text8=
      signature_text9=
      signature_text10=
      signature_before_quote=0
      set_autocc=0
      auto_cc=
      set_autobcc=0
      auto_bcc=
      set_autoreplyto=0
      auto_replyto=
      default_sign=1
      default_encrypt=0
      encrypt_reply=1
      encrypt_to_self=1
      ascii_armored=0
      clearsign=0
      sign_key=0
      sign_key_id=
      ssl_pop=0
      ssl_imap=2
      ssl_nntp=0
      ssl_smtp=2
      use_nonblocking_ssl=1
      use_socks=0
      use_socks_for_recv=1
      use_socks_for_send=1
      socks_type=1
      proxy_host=
      proxy_port=1080
      use_proxy_auth=0
      proxy_name=
      proxy_pass=
      set_smtpport=0
      smtp_port=25
      set_popport=0
      pop_port=110
      set_imapport=0
      imap_port=143
      set_nntpport=0
      nntp_port=119
      set_domain=0
      domain=
      imap_directory=
      imap_clear_cache_on_exit=0
      set_sent_folder=0
      sent_folder=
      set_draft_folder=0
      draft_folder=
      set_queue_folder=0
      queue_folder=
      set_trash_folder=0
      trash_folder=
    '';
  };
}
