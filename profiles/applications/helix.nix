{ config, pkgs, ... }: {
  defaultApplications.editor = {
    cmd = "${pkgs.helix}/bin/helix";
    desktop = "helix";
  };
  home-manager.users.balsoft = {
    xdg.configFile."helix/languages.toml".text = ''
      [[language]]
      name = "nix"
      formatter = { command = "nixfmt", args = [] }
    '';
    programs.helix = {
      enable = true;
      settings = {
        theme = "base16";
        editor.lsp.display-messages = true;
        keys = {
          insert = {
            up = "no_op";
            down = "no_op";
            left = "no_op";
            right = "no_op";
            pageup = "no_op";
            pagedown = "no_op";
            home = "no_op";
            end = "no_op";
          };
          normal = {
            "A-q" = [
              "goto_prev_paragraph"
              "goto_next_paragraph"
              "trim_selections"
              ":reflow 80"
              "collapse_selection"
            ];
            "A-h" = "select_prev_sibling";
            "A-j" = "shrink_selection";
            "A-k" = "expand_selection";
            "A-l" = "select_next_sibling";
            "V" = [ "select_mode" "extend_to_line_bounds" ];
          };
        };
      };
      themes.base16 =
        with (builtins.mapAttrs (_: v: "#${v}") config.themes.colors); rec {
          "ui.menu" = { bg = base01; };
          "ui.menu.selected" = {
            bg = base00;
            modifiers = [ "reversed" ];
          };
          "ui.linenr" = {
            fg = base02;
            bg = base00;
          };
          "ui.popup" = { modifiers = [ "reversed" ]; };
          "ui.linenr.selected" = {
            fg = base05;
            bg = base00;
            modifiers = [ "bold" ];
          };
          "ui.selection" = {
            fg = base00;
            bg = base0D;
          };
          "ui.selection.primary" = { modifiers = [ "reversed" ]; };
          "comment" = { fg = base03; };
          "ui.statusline" = {
            fg = base05;
            bg = base02;
          };
          "ui.statusline.inactive" = {
            fg = base02;
            bg = base05;
          };
          "ui.help" = {
            fg = base01;
            bg = base05;
          };
          "ui.cursor" = { modifiers = [ "reversed" ]; };
          "ui.cursor.insert" = {
            bg = base01;
            modifiers = [ "reversed" ];
          };
          "variable" = base05;
          "variable.builtin" = {
            fg = base06;
            modifiers = [ "bold" ];
          };
          "constant.numeric" = base0C;
          "constant" = base0A;
          "constant.builtin" = base0A;
          "constant.character" = {
            fg = base0C;
            modifiers = [ "bold" ];
          };
          "attributes" = base0A;
          "type" = {
            fg = base05;
            modifiers = [ "bold" ];
          };
          "ui.cursor.match" = { bg = base01; };
          "string" = base0B;
          "variable.other.member" = base0B;
          "constant.character.escape" = base0E;
          "function" = base05;
          "constructor" = base0A;
          "special" = base0F;
          "keyword" = {
            fg = base0D;
            modifiers = [ "bold" ];
          };
          "markup.heading" = {
            fg = base0A;
            modifiers = [ "bold" ];
          };
          "label" = base0B;
          "namespace" = base0F;
          "diff.plus" = base0B;
          "diff.delta" = base0A;
          "diff.minus" = base08;
          "diagnostic" = {
            modifiers = [ "italic" ];
            underline = { style = "line"; };
          };
          "diagnostic.error" = { fg = base08; } // diagnostic;
          "diagnostic.warning" = { fg = base09; } // diagnostic;
          "ui.gutter" = { bg = base00; };
          "info" = base0D;
          "hint" = base02;
          "debug" = base02;
          "warning" = base09;
          "error" = base08;
          "indent" = "none";
          "highlight" = base09;
        };
    };
  };
}