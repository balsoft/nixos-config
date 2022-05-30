{ linkFarm }:
{ base00, base01, base02, base03, base04, base05, base06, base07, base08, base09
, base0A, base0B, base0C, base0D, base0E, base0F, }:
let
  theme = {
    "theme/generated.json" = __toJSON {
      "$schema" = "vscode://schemas/color-theme";
      colors = {
        "activityBar.activeBackground" = "#${base00}";
        "activityBar.background" = "#${base00}";
        "activityBar.dropBackground" = "#${base07}";
        "activityBar.foreground" = "#${base05}";
        "activityBar.inactiveForeground" = "#${base03}";
        "activityBarBadge.background" = "#${base0D}";
        "activityBarBadge.foreground" = "#${base07}";
        "badge.background" = "#${base00}";
        "badge.foreground" = "#${base05}";
        "breadcrumb.activeSelectionForeground" = "#${base07}";
        "breadcrumb.background" = "#${base00}";
        "breadcrumb.focusForeground" = "#${base06}";
        "breadcrumb.foreground" = "#${base05}";
        "breadcrumbPicker.background" = "#${base00}";
        "button.background" = "#${base00}";
        "button.foreground" = "#${base07}";
        "button.hoverBackground" = "#${base04}";
        "button.secondaryBackground" = "#${base0E}";
        "button.secondaryForeground" = "#${base07}";
        "button.secondaryHoverBackground" = "#${base04}";
        "charts.blue" = "#${base0D}";
        "charts.foreground" = "#${base05}";
        "charts.green" = "#${base0B}";
        "charts.lines" = "#${base05}";
        "charts.orange" = "#${base09}";
        "charts.purple" = "#${base0E}";
        "charts.red" = "#${base08}";
        "charts.yellow" = "#${base0A}";
        "checkbox.background" = "#${base00}";
        "checkbox.foreground" = "#${base05}";
        "debugConsole.errorForeground" = "#${base08}";
        "debugConsole.infoForeground" = "#${base05}";
        "debugConsole.sourceForeground" = "#${base05}";
        "debugConsole.warningForeground" = "#${base0A}";
        "debugConsoleInputIcon.foreground" = "#${base05}";
        "debugExceptionWidget.background" = "#${base00}";
        "debugIcon.breakpointCurrentStackframeForeground" = "#${base0A}";
        "debugIcon.breakpointDisabledForeground" = "#${base04}";
        "debugIcon.breakpointForeground" = "#${base08}";
        "debugIcon.breakpointStackframeForeground" = "#${base0F}";
        "debugIcon.breakpointUnverifiedForeground" = "#${base02}";
        "debugIcon.continueForeground" = "#${base0B}";
        "debugIcon.disconnectForeground" = "#${base08}";
        "debugIcon.pauseForeground" = "#${base0D}";
        "debugIcon.restartForeground" = "#${base0B}";
        "debugIcon.startForeground" = "#${base0B}";
        "debugIcon.stepBackForeground" = "#${base0F}";
        "debugIcon.stepIntoForeground" = "#${base0C}";
        "debugIcon.stepOutForeground" = "#${base0E}";
        "debugIcon.stepOverForeground" = "#${base0D}";
        "debugIcon.stopForeground" = "#${base08}";
        "debugTokenExpression.boolean" = "#${base09}";
        "debugTokenExpression.error" = "#${base08}";
        "debugTokenExpression.name" = "#${base0E}";
        "debugTokenExpression.number" = "#${base09}";
        "debugTokenExpression.string" = "#${base0B}";
        "debugTokenExpression.value" = "#${base05}";
        "debugToolBar.background" = "#${base00}";
        "debugView.stateLabelBackground" = "#${base0D}";
        "debugView.stateLabelForeground" = "#${base07}";
        "debugView.valueChangedHighlight" = "#${base0D}";
        descriptionForeground = "#${base03}";
        "diffEditor.diagonalFill" = "#${base02}";
        "diffEditor.insertedTextBackground" = "#${base0B}20";
        "diffEditor.removedTextBackground" = "#${base08}20";
        "dropdown.background" = "#${base00}";
        "dropdown.foreground" = "#${base05}";
        "dropdown.listBackground" = "#${base00}";
        "editor.background" = "#${base00}";
        "editor.findMatchBackground" = "#${base0A}6f";
        "editor.findMatchHighlightBackground" = "#${base09}6f";
        "editor.findRangeHighlightBackground" = "#${base00}6f";
        "editor.foreground" = "#${base05}";
        "editor.hoverHighlightBackground" = "#${base02}6f";
        "editor.inactiveSelectionBackground" = "#${base02}";
        "editor.lineHighlightBackground" = "#${base00}";
        "editor.rangeHighlightBackground" = "#${base00}6f";
        "editor.selectionBackground" = "#${base01}";
        "editor.selectionHighlightBackground" = "#${base00}";
        "editor.snippetFinalTabstopHighlightBackground" = "#${base03}";
        "editor.snippetTabstopHighlightBackground" = "#${base02}";
        "editor.wordHighlightBackground" = "#${base02}6f";
        "editor.wordHighlightStrongBackground" = "#${base03}6f";
        "editorBracketMatch.background" = "#${base02}";
        "editorCodeLens.foreground" = "#${base02}";
        "editorCursor.foreground" = "#${base05}";
        "editorError.foreground" = "#${base08}";
        "editorGroup.background" = "#${base00}";
        "editorGroup.dropBackground" = "#${base02}6f";
        "editorGroup.emptyBackground" = "#${base00}";
        "editorGroupHeader.noTabsBackground" = "#${base00}";
        "editorGroupHeader.tabsBackground" = "#${base00}";
        "editorGutter.addedBackground" = "#${base0B}";
        "editorGutter.background" = "#${base00}";
        "editorGutter.commentRangeForeground" = "#${base04}";
        "editorGutter.deletedBackground" = "#${base08}";
        "editorGutter.foldingControlForeground" = "#${base05}";
        "editorGutter.modifiedBackground" = "#${base0E}";
        "editorHint.foreground" = "#${base0D}";
        "editorHoverWidget.background" = "#${base00}";
        "editorHoverWidget.foreground" = "#${base05}";
        "editorIndentGuide.activeBackground" = "#${base04}";
        "editorIndentGuide.background" = "#${base03}";
        "editorInfo.foreground" = "#${base0C}";
        "editorLightBulb.foreground" = "#${base0A}";
        "editorLightBulbAutoFix.foreground" = "#${base0D}";
        "editorLineNumber.activeForeground" = "#${base04}";
        "editorLineNumber.foreground" = "#${base03}";
        "editorLink.activeForeground" = "#${base0D}";
        "editorMarkerNavigation.background" = "#${base00}";
        "editorMarkerNavigationError.background" = "#${base08}";
        "editorMarkerNavigationInfo.background" = "#${base0D}";
        "editorMarkerNavigationWarning.background" = "#${base0A}";
        "editorOverviewRuler.addedForeground" = "#${base0B}";
        "editorOverviewRuler.bracketMatchForeground" = "#${base06}";
        "editorOverviewRuler.commonContentForeground" = "#${base0F}";
        "editorOverviewRuler.currentContentForeground" = "#${base0D}";
        "editorOverviewRuler.deletedForeground" = "#${base08}";
        "editorOverviewRuler.errorForeground" = "#${base08}";
        "editorOverviewRuler.findMatchForeground" = "#${base0A}6f";
        "editorOverviewRuler.incomingContentForeground" = "#${base0B}";
        "editorOverviewRuler.infoForeground" = "#${base0C}";
        "editorOverviewRuler.modifiedForeground" = "#${base0E}";
        "editorOverviewRuler.rangeHighlightForeground" = "#${base03}6f";
        "editorOverviewRuler.selectionHighlightForeground" = "#${base02}6f";
        "editorOverviewRuler.warningForeground" = "#${base0A}";
        "editorOverviewRuler.wordHighlightForeground" = "#${base07}6f";
        "editorOverviewRuler.wordHighlightStrongForeground" = "#${base0D}6f";
        "editorPane.background" = "#${base00}";
        "editorRuler.foreground" = "#${base03}";
        "editorSuggestWidget.background" = "#${base00}";
        "editorSuggestWidget.foreground" = "#${base05}";
        "editorSuggestWidget.highlightForeground" = "#${base0D}";
        "editorSuggestWidget.selectedBackground" = "#${base02}";
        "editorWarning.foreground" = "#${base0A}";
        "editorWhitespace.foreground" = "#${base03}";
        "editorWidget.background" = "#${base00}";
        "editorWidget.foreground" = "#${base05}";
        errorForeground = "#${base08}";
        "extensionBadge.remoteBackground" = "#${base09}";
        "extensionBadge.remoteForeground" = "#${base07}";
        "extensionButton.prominentBackground" = "#${base0B}";
        "extensionButton.prominentForeground" = "#${base07}";
        "extensionButton.prominentHoverBackground" = "#${base02}";
        foreground = "#${base05}";
        "gitDecoration.addedResourceForeground" = "#${base0B}";
        "gitDecoration.conflictingResourceForeground" = "#${base0A}";
        "gitDecoration.deletedResourceForeground" = "#${base08}";
        "gitDecoration.ignoredResourceForeground" = "#${base03}";
        "gitDecoration.modifiedResourceForeground" = "#${base0E}";
        "gitDecoration.stageDeletedResourceForeground" = "#${base08}";
        "gitDecoration.stageModifiedResourceForeground" = "#${base0E}";
        "gitDecoration.submoduleResourceForeground" = "#${base0F}";
        "gitDecoration.untrackedResourceForeground" = "#${base09}";
        "icon.foreground" = "#${base04}";
        "input.background" = "#${base00}";
        "input.foreground" = "#${base05}";
        "input.placeholderForeground" = "#${base03}";
        "inputOption.activeBackground" = "#${base02}";
        "inputOption.activeBorder" = "#${base09}";
        "inputOption.activeForeground" = "#${base05}";
        "inputValidation.errorBackground" = "#${base08}";
        "inputValidation.errorBorder" = "#${base08}";
        "inputValidation.errorForeground" = "#${base05}";
        "inputValidation.infoBackground" = "#${base0D}";
        "inputValidation.infoBorder" = "#${base0D}";
        "inputValidation.infoForeground" = "#${base05}";
        "inputValidation.warningBackground" = "#${base0A}";
        "inputValidation.warningBorder" = "#${base0A}";
        "inputValidation.warningForeground" = "#${base05}";
        "list.activeSelectionBackground" = "#${base01}";
        "list.activeSelectionForeground" = "#${base05}";
        "list.dropBackground" = "#${base07}";
        "list.errorForeground" = "#${base08}";
        "list.filterMatchBackground" = "#${base02}";
        "list.focusBackground" = "#${base02}";
        "list.focusForeground" = "#${base05}";
        "list.highlightForeground" = "#${base07}";
        "list.hoverBackground" = "#${base03}";
        "list.hoverForeground" = "#${base05}";
        "list.inactiveFocusBackground" = "#${base02}";
        "list.inactiveSelectionBackground" = "#${base02}";
        "list.inactiveSelectionForeground" = "#${base05}";
        "list.invalidItemForeground" = "#${base08}";
        "list.warningForeground" = "#${base0A}";
        "listFilterWidget.background" = "#${base00}";
        "listFilterWidget.noMatchesOutline" = "#${base08}";
        "menu.background" = "#${base00}";
        "menu.foreground" = "#${base05}";
        "menu.selectionBackground" = "#${base02}";
        "menu.selectionForeground" = "#${base05}";
        "menu.separatorBackground" = "#${base07}";
        "menubar.selectionBackground" = "#${base00}";
        "menubar.selectionForeground" = "#${base05}";
        "merge.currentContentBackground" = "#${base0D}40";
        "merge.currentHeaderBackground" = "#${base0D}40";
        "merge.incomingContentBackground" = "#${base0B}60";
        "merge.incomingHeaderBackground" = "#${base0B}60";
        "minimap.background" = "#${base00}";
        "minimap.errorHighlight" = "#${base08}";
        "minimap.findMatchHighlight" = "#${base0A}6f";
        "minimap.selectionHighlight" = "#${base02}6f";
        "minimap.warningHighlight" = "#${base0A}";
        "minimapGutter.addedBackground" = "#${base0B}";
        "minimapGutter.deletedBackground" = "#${base08}";
        "minimapGutter.modifiedBackground" = "#${base0E}";
        "notebook.rowHoverBackground" = "#${base00}";
        "notification.background" = "#${base02}";
        "notification.buttonBackground" = "#${base0D}";
        "notification.buttonForeground" = "#${base07}";
        "notification.buttonHoverBackground" = "#${base02}";
        "notification.errorBackground" = "#${base08}";
        "notification.errorForeground" = "#${base07}";
        "notification.foreground" = "#${base05}";
        "notification.infoBackground" = "#${base0C}";
        "notification.infoForeground" = "#${base07}";
        "notification.warningBackground" = "#${base0A}";
        "notification.warningForeground" = "#${base07}";
        "notificationCenterHeader.background" = "#${base00}";
        "notificationCenterHeader.foreground" = "#${base05}";
        "notificationLink.foreground" = "#${base0D}";
        "notifications.background" = "#${base02}";
        "notifications.foreground" = "#${base05}";
        "notificationsErrorIcon.foreground" = "#${base08}";
        "notificationsInfoIcon.foreground" = "#${base0D}";
        "notificationsWarningIcon.foreground" = "#${base0A}";
        "panel.background" = "#${base00}";
        "panel.dropBackground" = "#${base00}6f";
        "panelTitle.activeForeground" = "#${base05}";
        "panelTitle.inactiveForeground" = "#${base03}";
        "peekViewEditor.background" = "#${base00}";
        "peekViewEditor.matchHighlightBackground" = "#${base09}6f";
        "peekViewEditorGutter.background" = "#${base00}";
        "peekViewResult.background" = "#${base00}";
        "peekViewResult.fileForeground" = "#${base05}";
        "peekViewResult.lineForeground" = "#${base03}";
        "peekViewResult.matchHighlightBackground" = "#${base09}6f";
        "peekViewResult.selectionBackground" = "#${base02}";
        "peekViewResult.selectionForeground" = "#${base05}";
        "peekViewTitle.background" = "#${base02}";
        "peekViewTitleDescription.foreground" = "#${base03}";
        "peekViewTitleLabel.foreground" = "#${base05}";
        "pickerGroup.foreground" = "#${base03}";
        "problemsErrorIcon.foreground" = "#${base08}";
        "problemsInfoIcon.foreground" = "#${base0C}";
        "problemsWarningIcon.foreground" = "#${base0A}";
        "progressBar.background" = "#${base03}";
        "quickInput.background" = "#${base00}";
        "quickInput.foreground" = "#${base05}";
        "scrollbar.shadow" = "#${base00}";
        "scrollbarSlider.activeBackground" = "#${base04}6f";
        "scrollbarSlider.background" = "#${base02}6f";
        "scrollbarSlider.hoverBackground" = "#${base03}6f";
        "selection.background" = "#${base01}";
        "settings.checkboxBackground" = "#${base00}";
        "settings.checkboxForeground" = "#${base05}";
        "settings.dropdownBackground" = "#${base00}";
        "settings.dropdownForeground" = "#${base05}";
        "settings.focusedRowBackground" = "#${base02}";
        "settings.headerForeground" = "#${base05}";
        "settings.modifiedItemForeground" = "#${base0D}";
        "settings.modifiedItemIndicator" = "#${base0D}";
        "settings.numberInputBackground" = "#${base00}";
        "settings.numberInputForeground" = "#${base05}";
        "settings.textInputBackground" = "#${base00}";
        "settings.textInputForeground" = "#${base05}";
        "sideBar.background" = "#${base00}";
        "sideBar.dropBackground" = "#${base01}6f";
        "sideBar.foreground" = "#${base05}";
        "sideBarSectionHeader.background" = "#${base00}";
        "sideBarSectionHeader.foreground" = "#${base05}";
        "sideBarTitle.foreground" = "#${base05}";
        "statusBar.background" = "#${base0D}";
        "statusBar.debuggingBackground" = "#${base09}";
        "statusBar.debuggingForeground" = "#${base07}";
        "statusBar.foreground" = "#${base07}";
        "statusBar.noFolderBackground" = "#${base0E}";
        "statusBar.noFolderForeground" = "#${base07}";
        "statusBarItem.activeBackground" = "#${base03}";
        "statusBarItem.errorBackground" = "#${base08}";
        "statusBarItem.errorForeground" = "#${base07}";
        "statusBarItem.hoverBackground" = "#${base02}";
        "statusBarItem.prominentBackground" = "#${base0E}";
        "statusBarItem.prominentForeground" = "#${base07}";
        "statusBarItem.prominentHoverBackground" = "#${base08}";
        "statusBarItem.remoteBackground" = "#${base0B}";
        "statusBarItem.remoteForeground" = "#${base07}";
        "symbolIcon.arrayForeground" = "#${base05}";
        "symbolIcon.booleanForeground" = "#${base09}";
        "symbolIcon.classForeground" = "#${base0A}";
        "symbolIcon.colorForeground" = "#f0f";
        "symbolIcon.constantForeground" = "#${base09}";
        "symbolIcon.constructorForeground" = "#${base0D}";
        "symbolIcon.enumeratorForeground" = "#${base09}";
        "symbolIcon.enumeratorMemberForeground" = "#${base0D}";
        "symbolIcon.eventForeground" = "#${base0A}";
        "symbolIcon.fieldForeground" = "#${base08}";
        "symbolIcon.fileForeground" = "#${base05}";
        "symbolIcon.folderForeground" = "#${base05}";
        "symbolIcon.functionForeground" = "#${base0D}";
        "symbolIcon.interfaceForeground" = "#${base0D}";
        "symbolIcon.keyForeground" = "#f0f";
        "symbolIcon.keywordForeground" = "#${base0E}";
        "symbolIcon.methodForeground" = "#${base0D}";
        "symbolIcon.moduleForeground" = "#${base05}";
        "symbolIcon.namespaceForeground" = "#${base05}";
        "symbolIcon.nullForeground" = "#${base0F}";
        "symbolIcon.numberForeground" = "#${base09}";
        "symbolIcon.objectForeground" = "#f0f";
        "symbolIcon.operatorForeground" = "#f0f";
        "symbolIcon.packageForeground" = "#f0f";
        "symbolIcon.propertyForeground" = "#${base05}";
        "symbolIcon.referenceForeground" = "#f0f";
        "symbolIcon.snippetForeground" = "#${base05}";
        "symbolIcon.stringForeground" = "#${base0B}";
        "symbolIcon.structForeground" = "#${base0A}";
        "symbolIcon.textForeground" = "#${base05}";
        "symbolIcon.typeParameterForeground" = "#f0f";
        "symbolIcon.unitForeground" = "#f0f";
        "symbolIcon.variableForeground" = "#${base08}";
        "tab.activeBackground" = "#${base01}";
        "tab.activeForeground" = "#${base05}";
        "tab.activeModifiedBorder" = "#${base0D}";
        "tab.hoverBackground" = "#${base02}";
        "tab.inactiveBackground" = "#${base00}";
        "tab.inactiveForeground" = "#${base03}";
        "tab.inactiveModifiedBorder" = "#${base0D}";
        "tab.unfocusedActiveBackground" = "#${base00}";
        "tab.unfocusedActiveForeground" = "#${base04}";
        "tab.unfocusedActiveModifiedBorder" = "#${base0D}";
        "tab.unfocusedHoverBackground" = "#${base02}";
        "tab.unfocusedInactiveForeground" = "#${base03}";
        "tab.unfocusedInactiveModifiedBorder" = "#${base0D}";
        "terminal.ansiBlack" = "#${base00}";
        "terminal.ansiBlue" = "#${base0D}";
        "terminal.ansiBrightBlack" = "#${base03}";
        "terminal.ansiBrightBlue" = "#${base0D}";
        "terminal.ansiBrightCyan" = "#${base0C}";
        "terminal.ansiBrightGreen" = "#${base0B}";
        "terminal.ansiBrightMagenta" = "#${base0E}";
        "terminal.ansiBrightRed" = "#${base08}";
        "terminal.ansiBrightWhite" = "#${base07}";
        "terminal.ansiBrightYellow" = "#${base0A}";
        "terminal.ansiCyan" = "#${base0C}";
        "terminal.ansiGreen" = "#${base0B}";
        "terminal.ansiMagenta" = "#${base0E}";
        "terminal.ansiRed" = "#${base08}";
        "terminal.ansiWhite" = "#${base05}";
        "terminal.ansiYellow" = "#${base0A}";
        "terminal.background" = "#${base00}";
        "terminal.foreground" = "#${base05}";
        "terminalCursor.foreground" = "#${base05}";
        "textBlockQuote.background" = "#${base00}";
        "textBlockQuote.border" = "#${base0D}";
        "textCodeBlock.background" = "#${base00}";
        "textLink.activeForeground" = "#${base0C}";
        "textLink.foreground" = "#${base0D}";
        "textPreformat.foreground" = "#${base0D}";
        "textSeparator.foreground" = "#f0f";
        "titleBar.activeBackground" = "#${base01}";
        "titleBar.activeForeground" = "#${base05}";
        "titleBar.inactiveBackground" = "#${base00}";
        "titleBar.inactiveForeground" = "#${base03}";
        "tree.indentGuidesStroke" = "#${base05}";
        "walkThrough.embeddedEditorBackground" = "#${base00}";
        "welcomePage.background" = "#${base00}";
        "welcomePage.buttonBackground" = "#${base00}";
        "welcomePage.buttonHoverBackground" = "#${base02}";
        "widget.shadow" = "#${base00}";
      };
      name = "Balsoft's generated theme";
      tokenColors = [
        {
          name = "Comment";
          scope = [ "comment" "punctuation.definition.comment" ];
          settings = {
            fontStyle = "italic";
            foreground = "#${base03}";
          };
        }
        {
          name = "Variables, Parameters";
          scope = [
            "variable"
            "string constant.other.placeholder"
            "entity.name.variable.parameter"
            "entity.name.variable.local"
            "variable.parameter"
          ];
          settings = { foreground = "#${base05}"; };
        }
        {
          name = "Properties";
          scope = [ "variable.other.object.property" ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Colors";
          scope = [ "constant.other.color" ];
          settings = { foreground = "#${base0B}"; };
        }
        {
          name = "Invalid";
          scope = [ "invalid" "invalid.illegal" ];
          settings = { foreground = "#${base08}"; };
        }
        {
          name = "Invalid - Deprecated";
          scope = [ "invalid.deprecated" ];
          settings = { foreground = "#${base0F}"; };
        }
        {
          name = "Keyword, Storage";
          scope = [ "keyword" "storage.modifier" ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          name = "Keyword Control";
          scope = [
            "keyword.control"
            "keyword.control.flow"
            "keyword.control.from"
            "keyword.control.import"
            "keyword.control.as"
          ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          name = "Keyword";
          scope = [
            "keyword.other.using"
            "keyword.other.namespace"
            "keyword.other.class"
            "keyword.other.new"
            "keyword.other.event"
            "keyword.other.this"
            "keyword.other.await"
            "keyword.other.var"
            "keyword.other.package"
            "keyword.other.import"
            "variable.language.this"
            "storage.type.ts"
          ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          name = "Types, Primitives";
          scope = [ "keyword.type" "storage.type.primitive" ];
          settings = { foreground = "#${base0C}"; };
        }
        {
          name = "Function";
          scope = [ "storage.type.function" ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Operator, Misc";
          scope = [
            "constant.other.color"
            "punctuation"
            "punctuation.section.class.end"
            "meta.tag"
            "punctuation.definition.tag"
            "punctuation.separator.inheritance.php"
            "punctuation.definition.tag.html"
            "punctuation.definition.tag.begin.html"
            "punctuation.definition.tag.end.html"
            "keyword.other.template"
            "keyword.other.substitution"
          ];
          settings = { foreground = "#${base04}"; };
        }
        {
          name = "Embedded";
          scope = [ "punctuation.section.embedded" "variable.interpolation" ];
          settings = { foreground = "#${base0F}"; };
        }
        {
          name = "Tag";
          scope =
            [ "entity.name.tag" "meta.tag.sgml" "markup.deleted.git_gutter" ];
          settings = { foreground = "#${base08}"; };
        }
        {
          name = "Function, Special Method";
          scope = [
            "entity.name.function"
            "meta.function-call"
            "variable.function"
            "support.function"
            "keyword.other.special-method"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Block Level Variables";
          scope = [ "meta.block variable.other" ];
          settings = { foreground = "#${base08}"; };
        }
        {
          name = "Other Variable, String Link";
          scope = [ "support.other.variable" "string.other.link" ];
          settings = { foreground = "#${base08}"; };
        }
        {
          name = "Number, Constant, Function Argument, Tag Attribute, Embedded";
          scope = [
            "constant.numeric"
            "constant.language"
            "support.constant"
            "constant.character"
            "constant.escape"
            "keyword.other.unit"
            "keyword.other"
          ];
          settings = { foreground = "#${base09}"; };
        }
        {
          name = "String, Symbols, Inherited Class, Markup Heading";
          scope = [
            "string"
            "constant.other.symbol"
            "constant.other.key"
            "entity.other.inherited-class"
            "markup.heading"
            "markup.inserted.git_gutter"
            "meta.group.braces.curly constant.other.object.key.js string.unquoted.label.js"
          ];
          settings = {
            fontStyle = "";
            foreground = "#${base0B}";
          };
        }
        {
          name = "Class, Support";
          scope = [
            "entity.name"
            "support.type"
            "support.class"
            "support.other.namespace.use.php"
            "meta.use.php"
            "support.other.namespace.php"
            "markup.changed.git_gutter"
            "support.type.sys-types"
          ];
          settings = { foreground = "#${base0A}"; };
        }
        {
          name = "Storage Type, Import Class";
          scope = [
            "storage.type"
            "storage.modifier.package"
            "storage.modifier.import"
          ];
          settings = { foreground = "#${base0A}"; };
        }
        {
          name = "Fields";
          scope = [ "entity.name.variable.field" ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Entity Types";
          scope = [ "support.type" ];
          settings = { foreground = "#${base0C}"; };
        }
        {
          name = "CSS Class and Support";
          scope = [
            "source.css support.type.property-name"
            "source.sass support.type.property-name"
            "source.scss support.type.property-name"
            "source.less support.type.property-name"
            "source.stylus support.type.property-name"
            "source.postcss support.type.property-name"
          ];
          settings = { foreground = "#${base0C}"; };
        }
        {
          name = "Sub-methods";
          scope = [
            "entity.name.module.js"
            "variable.import.parameter.js"
            "variable.other.class.js"
          ];
          settings = { foreground = "#${base08}"; };
        }
        {
          name = "Language methods";
          scope = [ "variable.language" ];
          settings = {
            fontStyle = "italic";
            foreground = "#${base08}";
          };
        }
        {
          name = "entity.name.method.js";
          scope = [ "entity.name.method.js" ];
          settings = {
            fontStyle = "italic";
            foreground = "#${base0D}";
          };
        }
        {
          name = "meta.method.js";
          scope = [
            "meta.class-method.js entity.name.function.js"
            "variable.function.constructor"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Attributes";
          scope = [ "entity.other.attribute-name" ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "HTML Attributes";
          scope = [
            "text.html.basic entity.other.attribute-name.html"
            "text.html.basic entity.other.attribute-name"
          ];
          settings = {
            fontStyle = "italic";
            foreground = "#${base0A}";
          };
        }
        {
          name = "CSS Classes";
          scope = [ "entity.other.attribute-name.class" ];
          settings = { foreground = "#${base0A}"; };
        }
        {
          name = "CSS ID's";
          scope = [ "source.sass keyword.control" ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Inserted";
          scope = [ "markup.inserted" ];
          settings = { foreground = "#${base0B}"; };
        }
        {
          name = "Deleted";
          scope = [ "markup.deleted" ];
          settings = { foreground = "#${base08}"; };
        }
        {
          name = "Changed";
          scope = [ "markup.changed" ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          name = "Regular Expressions";
          scope = [ "string.regexp" ];
          settings = { foreground = "#${base0C}"; };
        }
        {
          name = "Escape Characters";
          scope = [ "constant.character.escape" ];
          settings = { foreground = "#${base0C}"; };
        }
        {
          name = "URL";
          scope = [ "*url*" "*link*" "*uri*" ];
          settings = { fontStyle = "underline"; };
        }
        {
          name = "Decorators";
          scope = [
            "tag.decorator.js entity.name.tag.js"
            "tag.decorator.js punctuation.definition.tag.js"
          ];
          settings = {
            fontStyle = "italic";
            foreground = "#${base0D}";
          };
        }
        {
          name = "ES7 Bind Operator";
          scope = [
            "source.js constant.other.object.key.js string.unquoted.label.js"
          ];
          settings = {
            fontStyle = "italic";
            foreground = "#${base0E}";
          };
        }
        {
          name = "JSON Key - Level 0";
          scope = [
            "source.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "JSON Key - Level 1";
          scope = [
            "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "JSON Key - Level 2";
          scope = [
            "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "JSON Key - Level 3";
          scope = [
            "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "JSON Key - Level 4";
          scope = [
            "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "JSON Key - Level 5";
          scope = [
            "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "JSON Key - Level 6";
          scope = [
            "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "JSON Key - Level 7";
          scope = [
            "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "JSON Key - Level 8";
          scope = [
            "source.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json meta.structure.dictionary.value.json meta.structure.dictionary.json support.type.property-name.json"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Markdown - Plain";
          scope = [
            "text.html.markdown"
            "punctuation.definition.list_item.markdown"
          ];
          settings = { foreground = "#${base05}"; };
        }
        {
          name = "Markdown - Markup Raw Inline";
          scope = [ "text.html.markdown markup.inline.raw.markdown" ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          name = "Markdown - Markup Raw Inline Punctuation";
          scope = [
            "text.html.markdown markup.inline.raw.markdown punctuation.definition.raw.markdown"
          ];
          settings = { foreground = "#${base0C}"; };
        }
        {
          name = "Markdown - Line Break";
          scope = [ "text.html.markdown meta.dummy.line-break" ];
          settings = { foreground = "#${base03}"; };
        }
        {
          name = "Markdown - Heading";
          scope = [
            "markdown.heading"
            "markup.heading | markup.heading entity.name"
            "markup.heading.markdown punctuation.definition.heading.markdown"
          ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Markup - Italic";
          scope = [ "markup.italic" ];
          settings = {
            fontStyle = "italic";
            foreground = "#${base08}";
          };
        }
        {
          name = "Markup - Bold";
          scope = [ "markup.bold" "markup.bold string" ];
          settings = {
            fontStyle = "bold";
            foreground = "#${base08}";
          };
        }
        {
          name = "Markup - Bold-Italic";
          scope = [
            "markup.bold markup.italic"
            "markup.italic markup.bold"
            "markup.quote markup.bold"
            "markup.bold markup.italic string"
            "markup.italic markup.bold string"
            "markup.quote markup.bold string"
          ];
          settings = {
            fontStyle = "bold";
            foreground = "#${base08}";
          };
        }
        {
          name = "Markup - Underline";
          scope = [ "markup.underline" ];
          settings = {
            fontStyle = "underline";
            foreground = "#${base09}";
          };
        }
        {
          name = "Markdown - Blockquote";
          scope = [ "markup.quote punctuation.definition.blockquote.markdown" ];
          settings = { foreground = "#${base0C}"; };
        }
        {
          name = "Markup - Quote";
          scope = [ "markup.quote" ];
          settings = { fontStyle = "italic"; };
        }
        {
          name = "Markdown - Link";
          scope = [ "string.other.link.title.markdown" ];
          settings = { foreground = "#${base0D}"; };
        }
        {
          name = "Markdown - Link Description";
          scope = [ "string.other.link.description.title.markdown" ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          name = "Markdown - Link Anchor";
          scope = [ "constant.other.reference.link.markdown" ];
          settings = { foreground = "#${base0A}"; };
        }
        {
          name = "Markup - Raw Block";
          scope = [ "markup.raw.block" ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          name = "Markdown - Raw Block Fenced";
          scope = [ "markup.raw.block.fenced.markdown" ];
          settings = { foreground = "#00000050"; };
        }
        {
          name = "Markdown - Fenced Bode Block";
          scope = [ "punctuation.definition.fenced.markdown" ];
          settings = { foreground = "#00000050"; };
        }
        {
          name = "Markdown - Fenced Code Block Variable";
          scope = [
            "markup.raw.block.fenced.markdown"
            "variable.language.fenced.markdown"
          ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          name = "Markdown - Fenced Language";
          scope = [ "variable.language.fenced.markdown" ];
          settings = { foreground = "#${base08}"; };
        }
        {
          name = "Markdown - Separator";
          scope = [ "meta.separator" ];
          settings = {
            fontStyle = "bold";
            foreground = "#${base0C}";
          };
        }
        {
          name = "Markup - Table";
          scope = [ "markup.table" ];
          settings = { foreground = "#${base0E}"; };
        }
        {
          scope = "token.info-token";
          settings = { foreground = "#${base0D}"; };
        }
        {
          scope = "token.warn-token";
          settings = { foreground = "#${base0A}"; };
        }
        {
          scope = "token.error-token";
          settings = { foreground = "#${base08}"; };
        }
        {
          scope = "token.debug-token";
          settings = { foreground = "#${base0E}"; };
        }
      ];
      type = "dark";
    };
    "package.json" = __toJSON {
      name = "theme";
      displayName = "Balsoft's generated theme";
      version = "0.0.0";
      publisher = "balsoft";
      engines.vscode = "^1.22.0";
      contributes.themes = [{
        label = "Balsoft's generated theme";
        uiTheme = "vs-dark";
        path = "./theme/generated.json";
      }];
      capabilities = {
        untrustedWorkspaces.supported = true;
        virtualWorkspaces = true;
      };
    };
  };
in with builtins;
linkFarm "balsoft.theme" (attrValues (mapAttrs (name: value: {
  name = "share/vscode/extensions/balsoft.theme/${name}";
  path = toFile (baseNameOf name) value;
}) theme))
