{pkgs, lib, config, ...}:
with import ../../../support.nix {inherit lib;};
let scripts = import ./scripts;
    thm = config.themes.colors;
in
{
  home-manager.users.balsoft =
  {
    xdg.configFile."albert/albert.conf".text = genIni
    {
      General = {
        frontendId = "org.albert.frontend.qmlboxmodel";
        hotkey = "Meta+Space";
        showTray = false;
        telemetry = true;
        terminal = "${pkgs.konsole}/bin/konsole -e";
        incrementalSort = true;
      };
      "org.albert.extension.applications".enabled = true;
      "org.albert.extension.files" = {
        enabled = true;
        filters = "application/*, image/*, directory/*, text/*";  
      };
      "org.albert.extension.firefoxbookmarks".enabled = true;
      "org.albert.extension.mpris".enabled = true;
      "org.albert.extension.python" = {
        enabled = true;
        enabled_modules = "Python, Wikipedia, win, Kill, qalc, nix, translate";
      };
      "org.albert.extension.ssh".enabled = true;
      "org.albert.extension.system" = {
          enabled = true;
          logout = "i3-msg exit";
          lock = "${pkgs.i3lock-fancy}/bin/i3lock-fancy";
          reboot = "reboot";
          shutdown = "shutdown now";    
      };
      "org.albert.extension.terminal".enabled = true;
      "org.albert.extension.websearch".enabled = true;
      "org.albert.frontend.qmlboxmodel" = {
        enabled = true;
        alwaysOnTop = true;
        clearOnHide = false;
        hideOnClose = true;
        hideOnFocusLoss = true;
        showCentered = true;
        stylePath="${pkgs.albert}/share/albert/org.albert.frontend.qmlboxmodel/styles/BoxModel/MainComponent.qml";
      };
    };
    xdg.configFile."albert/org.albert.frontend.qmlboxmodel/style_properties.ini".text = genIni 
    {
      BoxModel =
      {
        animation_duration = 0;
        background_color = thm.bg;
        border_color = thm.dark;
        shadow_color = "#70000000";
        border_size = 1;
        icon_size = 46;
        input_fontsize = 28;
        item_description_fontsize = 20;
        item_title_fontsize = 24;
        max_items = 10;
        padding = 6;
        radius = 8;
        settingsbutton_size = 10;
        spacing = 6;
        window_width = 1000;
      };
    };
    xdg.dataFile = builtins.mapAttrs
    (name: value:
    {
      target = "albert/org.albert.extension.python/modules/${name}.py";
      source = value;
    }
    )
    scripts;
  };
}
