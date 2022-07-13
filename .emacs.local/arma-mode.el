;;; arma-mode.el --- Major Mode for editing arma files source code -*- lexical-binding: t -*-
(require 'rx)

(defconst arma-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; C/C++ style comments
	(modify-syntax-entry ?/ ". 124b")
	(modify-syntax-entry ?* ". 23")
	(modify-syntax-entry ?\n "> b")
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"")
    (syntax-table))
  "Syntax table for `arma-mode'.")

;; https://alexschroeder.ch/geocities/kensanata/colors.html
(defface bis-functions-arma-face `((t (:foreground "SeaGreen1")))  "SeaGreen1")
(defvar bis-functions-arma-face 'bis-functions-arma-face 
  "Variable for face `bis-functions-arma-face'.")

(defface magic-variables-face `((t (:foreground "magenta1")))  "magenta1")
(defvar magic-variables-face 'magic-variables-face 
  "Variable for face `magic-variables-face'.")


(defconst arma-arrays-commands
  (rx (or bol space)
      (group (or
              "append"
              "apply"
              "arrayIntersect"
              "count"
              "createHashMapFromArray"
              "deleteAt"
              "deleteRange"
              "find"
              "findAny"
              "findIf"
              "flatten"
              "forEach"
              "in"
              "insert"
              "isArray"
              "isEqualTypeAll"
              "isEqualTypeArray"
              "param"
              "params"
              "parseSimpleArray"
              "pushBack"
              "pushBackUnique"
              "resize"
              "reverse"
              "select"
              "selectMax"
              "selectMin"
              "selectRandom"
              "selectRandomWeighted"
              "set"
              "sort"
              "toArray"
              "toString"
              )
             (and (zero-or-more word)))
      space))

(defconst arma-config-commands
  (rx (or bol space)
      (group (or
              "campaignConfigFile"
              "configClasses"
              "configFile"
              "configHierarchy"
              "configName"
              "configNull"
              "configOf"
              "configProperties"
              "configSourceAddonList"
              "count"
              "getArray"
              "getMissionConfig"
              "getMissionConfigValue"
              "getNumber"
              "getText"
              "getTextRaw"
              "inheritsFrom"
              "isArray"
              "isClass"
              "isNumber"
              "isText"
              "loadConfig"
              "missionConfigFile"
              "select"
              )
             (and (zero-or-more word)))
      space))

(defconst arma-server-execution-commands
  (rx (or bol space)
      (group (or
              "addCuratorAddons"
              "addCuratorCameraArea"
              "addCuratorEditableObjects"
              "addCuratorEditingArea"
              "addCuratorPoints"
              "addPlayerScores"
              "addScore"
              "addScoreSide"
              "admin"
              "allowCuratorLogicIgnoreAreas"
              "allUsers"
              "assignCurator"
              "copyToClipboard"
              "didJIPOwner"
              "enableSimulationGlobal"
              "estimatedTimeLeft"
              "forceWeatherChange"
              "getMissionLayerEntities"
              "getUserInfo"
              "groupOwner"
              "hideObjectGlobal"
              "onPlayerConnected"
              "onPlayerDisconnected"
              "owner"
              "radioChannelCreate"
              "removeAllCuratorAddons"
              "removeAllCuratorCameraAreas"
              "removeAllCuratorEditingAreas"
              "removeCuratorAddons"
              "removeCuratorCameraArea"
              "removeCuratorEditableObjects"
              "removeCuratorEditingArea"
              "serverCommand"
              "serverNamespace"
              "setCuratorCameraAreaCeiling"
              "setCuratorCoef"
              "setCuratorEditingAreaType"
              "setCuratorWaypointCost"
              "setFog"
              "setFriend"
              "setGroupOwner"
              "setMaxLoad"
              "setOwner"
              "setRain"
              "setShotParents"
              "setTerrainHeight"
              "setTimeMultiplier"
              "setWaypointBehaviour"
              "setWaypointFormation"
              "setWaypointSpeed"
              "setWindDir"
              "skipTime"
              "turretOwner"
              "unassignCurator"
              )
             (and (zero-or-more word)))
      space))

(defconst arma-program-flow-commands
  (rx (or bol space)
      (group (or
              "assert"
              "break"
              "breakOut"
              "breakTo"
              "breakWith"
              "call"
              "canSuspend"
              "case"
              "catch"
              "continue"
              "continueWith"
              "default"
              "do"
              "else"
              "exec"
              "execFSM"
              "execVM"
              "exit"
              "exitWith"
              "fileExists"
              "for"
              "forEach"
              "forEachMember"
              "forEachMemberAgent"
              "forEachMemberTeam"
              "from"
              "goto"
              "halt"
              "if"
              "isUIContext"
              "loadFile"
              "scopeName"
              "scriptDone"
              "scriptName"
              "sleep"
              "spawn"
              "step"
              "switch"
              "terminate"
              "then"
              "throw"
              "to"
              "try"
              "uiSleep"
              "waitUntil"
              "while"
              "with"
              )
             (and (zero-or-more word)))
      space))

;; https://community.bistudio.com/wiki/Magic_Variables
(defconst arma-magic-variables
  (rx (or bol space)
      (group (or
               "_this"
               "_x"
               "_y"
               "_exception"
               "_forEachIndex"
               "_thisArgs"
               "_thisEventHandler"
               "_thisScriptedEventHandler"
               "_thisEvent"
               "_thisFSM"
               "_thisScript"
               "_time"
               "_fnc_scriptName"
               "_fnc_scriptNameParent"
               "this"
               "thisList"
               "thisTrigger"
              )
             (and (zero-or-more word)))
      space))

(defconst arma-font-lock-defaults
  `(
    ;; Single quote characters
    ("\\('[[:word:]]\\)\\>" . font-lock-constant-face)
    ;; Arma BIS functions
    ("\s\\(BIS_fnc_\\w+\\)" . bis-functions-arma-face)
    ;; Custom functions
    ("\s\\(\\w+_fnc_\\w+\\)" . font-lock-function-name-face)

    ;; Arma commands
    (,arma-magic-variables 1 magic-variables-face)
    (,arma-program-flow-commands 1 font-lock-keyword-face)
    (,arma-arrays-commands 1 font-lock-builtin-face)
    (,arma-config-commands 1 font-lock-builtin-face)
    (,arma-server-execution-commands 1 font-lock-builtin-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)
    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Local variables
    ;; https://stackoverflow.com/questions/4355071/elisp-regexp-match-group-if-followed-by-other-regexp
    ("\\(^\\|\s\\|\t\\|\\[\\|\s(\\)\\(_\\w+\\)" 2 font-lock-variable-name-face)
    ))

;;;###autoload
(define-derived-mode arma-mode prog-mode "arma"
  "Major Mode for editing arma source code."
  :syntax-table arma-mode-syntax-table
  (setq font-lock-defaults '(arma-font-lock-defaults))
  (setq-local comment-start "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sqf\\'" . arma-mode))

(provide 'arma-mode)
