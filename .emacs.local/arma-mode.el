;;; arma-mode.el --- Emacs major mode for the SQF language.

;; Copyright (C) 2003-2015 Free Software Foundation, Inc.

;; Author: sux <iamthesux@gmail.com>
;; Version: 0.1
;; Keywords: arma, sqf
;; URL: https://github.com/iamthesux/arma-mode

;;; Commentary:

;; This package provides a major mode to highlight and indent
;; the SQF scripting language.

;;; Code:

;; (if (featurep 'sqf-mode)
;;     (unload-feature 'sqf-mode))

;;(load "./words")

(require 'font-lock)
(require 'cc-mode)

(defvar sqf-mode-builtins
  '("and"
    "do"
    "else"
    "exit"
    "exitWith"
    "false"
    "for"
    "forEach"
    "forEachMember"
    "forEachMemberAgent"
    "forEachMemberTeam"
    "from"
    "if"
    "in"
    "nil"
    "not"
    "or"
    "then"
    "to"
    "true"
    "try"
    "waitUntil"
    "with"
    "while"
    ))

(defvar sqf-mode-keywords
  '("abs"
    "accTime"
    "acos"
    "action"
    "actionKeys"
    "actionKeysImages"
    "actionKeysNames"
    "actionKeysNamesArray"
    "actionName"
    "activateAddons"
    "activatedAddons"
    "activateKey"
    "addAction"
    "addBackpack"
    "addBackpackCargo"
    "addBackpackCargoGlobal"
    "addBackpackGlobal"
    "addCamShake"
    "addCuratorAddons"
    "addCuratorCameraArea"
    "addCuratorEditableObjects"
    "addCuratorEditingArea"
    "addCuratorPoints"
    "addEditorObject"
    "addEventHandler"
    "addGoggles"
    "addGroupIcon"
    "addHandgunItem"
    "addHeadgear"
    "addItem"
    "addItemCargo"
    "addItemCargoGlobal"
    "addItemPool"
    "addItemToBackpack"
    "addItemToUniform"
    "addItemToVest"
    "addLiveStats"
    "addMagazine"
    "addMagazine array"
    "addMagazineAmmoCargo"
    "addMagazineCargo"
    "addMagazineCargoGlobal"
    "addMagazineGlobal"
    "addMagazinePool"
    "addMagazines"
    "addMagazineTurret"
    "addMenu"
    "addMenuItem"
    "addMissionEventHandler"
    "addMPEventHandler"
    "addMusicEventHandler"
    "addPrimaryWeaponItem"
    "addPublicVariableEventHandler"
    "addRating"
    "addResources"
    "addScore"
    "addScoreSide"
    "addSecondaryWeaponItem"
    "addSwitchableUnit"
    "addTeamMember"
    "addToRemainsCollector"
    "addUniform"
    "addVehicle"
    "addVest"
    "addWaypoint"
    "addWeapon"
    "addWeaponCargo"
    "addWeaponCargoGlobal"
    "addWeaponGlobal"
    "addWeaponPool"
    "addWeaponTurret"
    "agent"
    "agents"
    "AGLToASL"
    "aimedAtTarget"
    "aimPos"
    "airDensityRTD"
    "airportSide"
    "AISFinishHeal"
    "alive"
    "allControls"
    "allCurators"
    "allDead"
    "allDeadMen"
    "allDisplays"
    "allGroups"
    "allMapMarkers"
    "allMines"
    "allMissionObjects"
    "allow3DMode"
    "allowCrewInImmobile"
    "allowCuratorLogicIgnoreAreas"
    "allowDamage"
    "allowDammage"
    "allowFileOperations"
    "allowFleeing"
    "allowGetIn"
    "allPlayers"
    "allSites"
    "allTurrets"
    "allUnits"
    "allUnitsUAV"
    "allVariables"
    "ammo"
    "animate"
    "animateDoor"
    "animationPhase"
    "animationState"
    "append"
    "armoryPoints"
    "arrayIntersect"
    "asin"
    "ASLToATL"
    "ASLToAGL"
    "assert"
    "assignAsCargo"
    "assignAsCargoIndex"
    "assignAsCommander"
    "assignAsDriver"
    "assignAsGunner"
    "assignAsTurret"
    "assignCurator"
    "assignedCargo"
    "assignedCommander"
    "assignedDriver"
    "assignedGunner"
    "assignedItems"
    "assignedTarget"
    "assignedTeam"
    "assignedVehicle"
    "assignedVehicleRole"
    "assignItem"
    "assignTeam"
    "assignToAirport"
    "atan"
    "atan2"
    "atg"
    "ATLToASL"
    "attachedObject"
    "attachedObjects"
    "attachedTo"
    "attachObject"
    "attachTo"
    "attackEnabled"
    "backpack"
    "backpackCargo"
    "backpackContainer"
    "backpackItems"
    "backpackMagazines"
    "backpackSpaceFor"
    "behaviour"
    "benchmark"
    "binocular"
    "blufor"
    "boundingBox"
    "boundingBoxReal"
    "boundingCenter"
    "breakOut"
    "breakTo"
    "briefingName"
    "buildingExit"
    "buildingPos"
    "buttonAction"
    "buttonSetAction"
    "cadetMode"
    "call"
    "callExtension"
    "camCommand"
    "camCommit"
    "camCommitPrepared"
    "camCommitted"
    "camConstuctionSetParams"
    "camCreate"
    "camDestroy"
    "cameraEffect"
    "cameraEffectEnableHUD"
    "cameraInterest"
    "cameraOn"
    "cameraView"
    "campaignConfigFile"
    "camPreload"
    "camPreloaded"
    "camPrepareBank"
    "camPrepareDir"
    "camPrepareDive"
    "camPrepareFocus"
    "camPrepareFov"
    "camPrepareFovRange"
    "camPreparePos"
    "camPrepareRelPos"
    "camPrepareTarget"
    "camSetBank"
    "camSetDir"
    "camSetDive"
    "camSetFocus"
    "camSetFov"
    "camSetFovRange"
    "camSetPos"
    "camSetRelPos"
    "camSetTarget"
    "camTarget"
    "camUseNVG"
    "canAdd"
    "canAddItemToBackpack"
    "canAddItemToUniform"
    "canAddItemToVest"
    "cancelSimpleTaskDestination"
    "canFire"
    "canMove"
    "canSlingLoad"
    "canStand"
    "canUnloadInCombat"
    "captive"
    "captiveNum"
    "case"
    "catch"
    "cbChecked"
    "cbSetChecked"
    "ceil"
    "cheatsEnabled"
    "checkAIFeature"
    "civilian"
    "className"
    "clearAllItemsFromBackpack"
    "clearBackpackCargo"
    "clearBackpackCargoGlobal"
    "clearGroupIcons"
    "clearItemCargo"
    "clearItemCargoGlobal"
    "clearItemPool"
    "clearMagazineCargo"
    "clearMagazineCargoGlobal"
    "clearMagazinePool"
    "clearOverlay"
    "clearRadio"
    "clearWeaponCargo"
    "clearWeaponCargoGlobal"
    "clearWeaponPool"
    "closeDialog"
    "closeDisplay"
    "closeOverlay"
    "collapseObjectTree"
    "combatMode"
    "commandArtilleryFire"
    "commandChat"
    "commander"
    "commandFire"
    "commandFollow"
    "commandFSM"
    "commandGetOut"
    "commandingMenu"
    "commandMove"
    "commandRadio"
    "commandStop"
    "commandTarget"
    "commandWatch"
    "comment"
    "commitOverlay"
    "compile"
    "compileFinal"
    "completedFSM"
    "composeText"
    "configClasses"
    "configFile"
    "configHierarchy"
    "configName"
    "configProperties"
    "configSourceMod"
    "configSourceModList"
    "connectTerminalToUAV"
    "controlNull"
    "controlsGroupCtrl"
    "copyFromClipboard"
    "copyToClipboard"
    "copyWaypoints"
    "cos"
    "count"
    "countEnemy"
    "countFriendly"
    "countSide"
    "countType"
    "countUnknown"
    "createAgent"
    "createCenter"
    "createDialog"
    "createDiaryLink"
    "createDiaryRecord"
    "createDiarySubject"
    "createDisplay"
    "createGearDialog"
    "createGroup"
    "createGuardedPoint"
    "createLocation"
    "createMarker"
    "createMarkerLocal"
    "createMenu"
    "createMine"
    "createMissionDisplay"
    "createSimpleTask"
    "createSite"
    "createSoundSource"
    "createTask"
    "createTeam"
    "createTrigger"
    "createUnit"
    "createUnit array"
    "createVehicle"
    "createVehicle array"
    "createVehicleCrew"
    "createVehicleLocal"
    "crew"
    "ctrlActivate"
    "ctrlAddEventHandler"
    "ctrlAutoScrollDelay"
    "ctrlAutoScrollRewind"
    "ctrlAutoScrollSpeed"
    "ctrlChecked"
    "ctrlClassName"
    "ctrlCommit"
    "ctrlCommitted"
    "ctrlCreate"
    "ctrlDelete"
    "ctrlEnable"
    "ctrlEnabled"
    "ctrlFade"
    "ctrlHTMLLoaded"
    "ctrlIDC"
    "ctrlIDD"
    "ctrlMapAnimAdd"
    "ctrlMapAnimClear"
    "ctrlMapAnimCommit"
    "ctrlMapAnimDone"
    "ctrlMapCursor"
    "ctrlMapMouseOver"
    "ctrlMapScale"
    "ctrlMapScreenToWorld"
    "ctrlMapWorldToScreen"
    "ctrlModel"
    "ctrlModelDirAndUp"
    "ctrlModelScale"
    "ctrlParent"
    "ctrlPosition"
    "ctrlRemoveAllEventHandlers"
    "ctrlRemoveEventHandler"
    "ctrlScale"
    "ctrlSetActiveColor"
    "ctrlSetAutoScrollDelay"
    "ctrlSetAutoScrollRewind"
    "ctrlSetAutoScrollSpeed"
    "ctrlSetBackgroundColor"
    "ctrlSetChecked"
    "ctrlSetEventHandler"
    "ctrlSetFade"
    "ctrlSetFocus"
    "ctrlSetFont"
    "ctrlSetFontH1"
    "ctrlSetFontH1B"
    "ctrlSetFontH2"
    "ctrlSetFontH2B"
    "ctrlSetFontH3"
    "ctrlSetFontH3B"
    "ctrlSetFontH4"
    "ctrlSetFontH4B"
    "ctrlSetFontH5"
    "ctrlSetFontH5B"
    "ctrlSetFontH6"
    "ctrlSetFontH6B"
    "ctrlSetFontHeight"
    "ctrlSetFontHeightH1"
    "ctrlSetFontHeightH2"
    "ctrlSetFontHeightH3"
    "ctrlSetFontHeightH4"
    "ctrlSetFontHeightH5"
    "ctrlSetFontHeightH6"
    "ctrlSetFontP"
    "ctrlSetFontPB"
    "ctrlSetForegroundColor"
    "ctrlSetModel"
    "ctrlSetModelDirAndUp"
    "ctrlSetModelScale"
    "ctrlSetPosition"
    "ctrlSetScale"
    "ctrlSetStructuredText"
    "ctrlSetText"
    "ctrlSetTextColor"
    "ctrlSetTooltip"
    "ctrlSetTooltipColorBox"
    "ctrlSetTooltipColorShade"
    "ctrlSetTooltipColorText"
    "ctrlShow"
    "ctrlShown"
    "ctrlText"
    "ctrlTextHeight"
    "ctrlType"
    "ctrlVisible"
    "curatorAddons"
    "curatorCamera"
    "curatorCameraArea"
    "curatorCameraAreaCeiling"
    "curatorCoef"
    "curatorEditableObjects"
    "curatorEditingArea"
    "curatorEditingAreaType"
    "curatorMouseOver"
    "curatorPoints"
    "curatorRegisteredObjects"
    "curatorSelected"
    "curatorWaypointCost"
    "currentCommand"
    "currentMagazine"
    "currentMagazineDetail"
    "currentMagazineDetailTurret"
    "currentMagazineTurret"
    "currentMuzzle"
    "currentNamespace"
    "currentTask"
    "currentTasks"
    "currentThrowable"
    "currentVisionMode"
    "currentWaypoint"
    "currentWeapon"
    "currentWeaponMode"
    "currentWeaponTurret"
    "currentZeroing"
    "cursorTarget"
    "customChat"
    "customRadio"
    "cutFadeOut"
    "cutObj"
    "cutRsc"
    "cutText"
    "damage"
    "date"
    "dateToNumber"
    "daytime"
    "deActivateKey"
    "debriefingText"
    "debugFSM"
    "debugLog"
    "default"
    "deg"
    "deleteAt"
    "deleteCenter"
    "deleteCollection"
    "deleteEditorObject"
    "deleteGroup"
    "deleteIdentity"
    "deleteLocation"
    "deleteMarker"
    "deleteMarkerLocal"
    "deleteRange"
    "deleteResources"
    "deleteSite"
    "deleteStatus"
    "deleteTeam"
    "deleteVehicle"
    "deleteVehicleCrew"
    "deleteWaypoint"
    "detach"
    "detectedMines"
    "diag_activeSQFScripts"
    "diag_captureFrame"
    "diag_captureSlowFrame"
    "diag_fps"
    "diag_fpsmin"
    "diag_frameno"
    "diag_log"
    "diag_logSlowFrame"
    "diag_tickTime"
    "dialog"
    "diarySubjectExists"
    "didJIP"
    "didJIPOwner"
    "difficulty"
    "difficultyEnabled"
    "difficultyEnabledRTD"
    "direction"
    "directSay"
    "disableAI"
    "disableCollisionWith"
    "disableConversation"
    "disableDebriefingStats"
    "disableNVGEquipment"
    "disableRemoteSensors"
    "disableSerialization"
    "disableTIEquipment"
    "disableUAVConnectability"
    "disableUserInput"
    "displayAddEventHandler"
    "displayCtrl"
    "displayNull"
    "displayRemoveAllEventHandlers"
    "displayRemoveEventHandler"
    "displaySetEventHandler"
    "dissolveTeam"
    "distance2D"
    "distance"
    "distanceSqr"
    "distributionRegion"
    "doArtilleryFire"
    "doFire"
    "doFollow"
    "doFSM"
    "doGetOut"
    "doMove"
    "doorPhase"
    "doStop"
    "doTarget"
    "doWatch"
    "drawArrow"
    "drawEllipse"
    "drawIcon"
    "drawIcon3D"
    "drawLine"
    "drawLine3D"
    "drawLink"
    "drawLocation"
    "drawRectangle"
    "driver"
    "drop"
    "east"
    "echo"
    "editObject"
    "editorSetEventHandler"
    "effectiveCommander"
    "emptyPositions"
    "enableAI"
    "enableAIFeature"
    "enableAttack"
    "enableCamShake"
    "enableCaustics"
    "enableCollisionWith"
    "enableCopilot"
    "enableDebriefingStats"
    "enableDiagLegend"
    "enableEndDialog"
    "enableEngineArtillery"
    "enableEnvironment"
    "enableFatigue"
    "enableGunLights"
    "enableIRLasers"
    "enableMimics"
    "enablePersonTurret"
    "enableRadio"
    "enableReload"
    "enableRopeAttach"
    "enableSatNormalOnDetail"
    "enableSaving"
    "enableSentences"
    "enableSimulation"
    "enableSimulationGlobal"
    "enableTeamSwitch"
    "enableUAVConnectability"
    "enableUAVWaypoints"
    "endLoadingScreen"
    "endMission"
    "engineOn"
    "enginesIsOnRTD"
    "enginesRpmRTD"
    "enginesTorqueRTD"
    "entities"
    "estimatedEndServerTime"
    "estimatedTimeLeft"
    "evalObjectArgument"
    "everyBackpack"
    "everyContainer"
    "exec"
    "execEditorScript"
    "execFSM"
    "execVM"
    "exp"
    "expectedDestination"
    "eyeDirection"
    "eyePos"
    "face"
    "faction"
    "fadeMusic"
    "fadeRadio"
    "fadeSound"
    "fadeSpeech"
    "failMission"
    "fillWeaponsFromPool"
    "find"
    "findCover"
    "findDisplay"
    "findEditorObject"
    "findEmptyPosition"
    "findEmptyPositionReady"
    "findNearestEnemy"
    "finishMissionInit"
    "finite"
    "fire"
    "fireAtTarget"
    "firstBackpack"
    "flag"
    "flagOwner"
    "fleeing"
    "floor"
    "flyInHeight"
    "fog"
    "fogForecast"
    "fogParams"
    "forceAddUniform"
    "forceEnd"
    "forceMap"
    "forceRespawn"
    "forceSpeed"
    "forceWalk"
    "forceWeaponFire"
    "forceWeatherChange"
    "format"
    "formation"
    "formationDirection"
    "formationLeader"
    "formationMembers"
    "formationPosition"
    "formationTask"
    "formatText"
    "formLeader"
    "freeLook"
    "fromEditor"
    "fuel"
    "fullCrew"
    "gearSlotAmmoCount"
    "gearSlotData"
    "getAllHitPointsDamage"
    "getAmmoCargo"
    "getArray"
    "getArtilleryAmmo"
    "getArtilleryComputerSettings"
    "getArtilleryETA"
    "getAssignedCuratorLogic"
    "getAssignedCuratorUnit"
    "getBackpackCargo"
    "getBleedingRemaining"
    "getBurningValue"
    "getCargoIndex"
    "getCenterOfMass"
    "getClientState"
    "getConnectedUAV"
    "getDammage"
    "getDescription"
    "getDir"
    "getDirVisual"
    "getDLCs"
    "getEditorCamera"
    "getEditorMode"
    "getEditorObjectScope"
    "getElevationOffset"
    "getFatigue"
    "getFriend"
    "getFSMVariable"
    "getFuelCargo"
    "getGroupIcon"
    "getGroupIconParams"
    "getGroupIcons"
    "getHideFrom"
    "getHit"
    "getHitIndex"
    "getHitPointDamage"
    "getItemCargo"
    "getMagazineCargo"
    "getMarkerColor"
    "getMarkerPos"
    "getMarkerSize"
    "getMarkerType"
    "getMass"
    "getModelInfo"
    "getNumber"
    "getObjectArgument"
    "getObjectChildren"
    "getObjectDLC"
    "getObjectMaterials"
    "getObjectProxy"
    "getObjectTextures"
    "getObjectType"
    "getObjectViewDistance"
    "getOxygenRemaining"
    "getPersonUsedDLCs"
    "getPlayerUID"
    "getPos"
    "getPosASL"
    "getPosASLVisual"
    "getPosASLW"
    "getPosATL"
    "getPosATLVisual"
    "getPosVisual"
    "getPosWorld"
    "getRepairCargo"
    "getRemoteSensorsDisabled"
    "getResolution"
    "getShadowDistance"
    "getSlingLoad"
    "getSpeed"
    "getSuppression"
    "getTerrainHeightASL"
    "getText"
    "getVariable"
    "getWeaponCargo"
    "getWPPos"
    "glanceAt"
    "globalChat"
    "globalRadio"
    "goggles"
    "goto"
    "group"
    "groupChat"
    "groupFromNetId"
    "groupIconSelectable"
    "groupIconsVisible"
    "groupId"
    "groupOwner"
    "groupRadio"
    "groupSelectedUnits"
    "groupSelectUnit"
    "grpNull"
    "gunner"
    "gusts"
    "halt"
    "handgunItems"
    "handgunMagazine"
    "handgunWeapon"
    "handsHit"
    "hasInterface"
    "hasWeapon"
    "hcAllGroups"
    "hcGroupParams"
    "hcLeader"
    "hcRemoveAllGroups"
    "hcRemoveGroup"
    "hcSelected"
    "hcSelectGroup"
    "hcSetGroup"
    "hcShowBar"
    "hcShownBar"
    "headgear"
    "hideBody"
    "hideObject"
    "hideObjectGlobal"
    "hint"
    "hintC"
    "hintCadet"
    "hintSilent"
    "hmd"
    "hostMission"
    "htmlLoad"
    "HUDMovementLevels"
    "humidity"
    "image"
    "importAllGroups"
    "importance"
    "incapacitatedState"
    "independent"
    "inflame"
    "inflamed"
    "inGameUISetEventHandler"
    "inheritsFrom"
    "initAmbientLife"
    "inputAction"
    "inRangeOfArtillery"
    "insertEditorObject"
    "intersect"
    "isAbleToBreathe"
    "isAgent"
    "isArray"
    "isAutoHoverOn"
    "isAutonomous"
    "isAutotest"
    "isBleeding"
    "isBurning"
    "isClass"
    "isCollisionLightOn"
    "isCopilotEnabled"
    "isDedicated"
    "isDLCAvailable"
    "isEngineOn"
    "isEqualTo"
    "isFlashlightOn"
    "isFlatEmpty"
    "isForcedWalk"
    "isFormationLeader"
    "isHidden"
    "isInRemainsCollector"
    "isInstructorFigureEnabled"
    "isIRLaserOn"
    "isKeyActive"
    "isKindOf"
    "isLightOn"
    "isLocalized"
    "isManualFire"
    "isMarkedForCollection"
    "isMultiplayer"
    "isNil"
    "isNull"
    "isNumber"
    "isObjectHidden"
    "isObjectRTD"
    "isOnRoad"
    "isPipEnabled"
    "isPlayer"
    "isRealTime"
    "isServer"
    "isShowing3DIcons"
    "isSteamMission"
    "isStreamFriendlyUIEnabled"
    "isText"
    "isTouchingGround"
    "isTutHintsEnabled"
    "isUAVConnectable"
    "isUAVConnected"
    "isUniformAllowed"
    "isWalking"
    "isWeaponDeployed"
    "isWeaponRested"
    "itemCargo"
    "items"
    "itemsWithMagazines"
    "join"
    "joinAs"
    "joinAsSilent"
    "joinSilent"
    "joinString"
    "kbAddDatabase"
    "kbAddDatabaseTargets"
    "kbAddTopic"
    "kbHasTopic"
    "kbReact"
    "kbRemoveTopic"
    "kbTell"
    "kbWasSaid"
    "keyImage"
    "keyName"
    "knowsAbout"
    "land"
    "landAt"
    "landResult"
    "language"
    "laserTarget"
    "lbAdd"
    "lbClear"
    "lbColor"
    "lbCurSel"
    "lbData"
    "lbDelete"
    "lbIsSelected"
    "lbPicture"
    "lbSelection"
    "lbSetColor"
    "lbSetCurSel"
    "lbSetData"
    "lbSetPicture"
    "lbSetPictureColor"
    "lbSetPictureColorDisabled"
    "lbSetPictureColorSelected"
    "lbSetSelectColor"
    "lbSetSelectColorRight"
    "lbSetSelected"
    "lbSetTooltip"
    "lbSetValue"
    "lbSize"
    "lbSort"
    "lbSortByValue"
    "lbText"
    "lbValue"
    "leader"
    "leaderboardDeInit"
    "leaderboardGetRows"
    "leaderboardInit"
    "leaveVehicle"
    "libraryCredits"
    "libraryDisclaimers"
    "lifeState"
    "lightAttachObject"
    "lightDetachObject"
    "lightIsOn"
    "lightnings"
    "limitSpeed"
    "linearConversion"
    "lineBreak"
    "lineIntersects"
    "lineIntersectsObjs"
    "lineIntersectsSurfaces"
    "lineIntersectsWith"
    "linkItem"
    "list"
    "listObjects"
    "ln"
    "lnbAddArray"
    "lnbAddColumn"
    "lnbAddRow"
    "lnbClear"
    "lnbColor"
    "lnbCurSelRow"
    "lnbData"
    "lnbDeleteColumn"
    "lnbDeleteRow"
    "lnbGetColumnsPosition"
    "lnbPicture"
    "lnbSetColor"
    "lnbSetColumnsPos"
    "lnbSetCurSelRow"
    "lnbSetData"
    "lnbSetPicture"
    "lnbSetText"
    "lnbSetValue"
    "lnbSize"
    "lnbText"
    "lnbValue"
    "load"
    "loadAbs"
    "loadBackpack"
    "loadFile"
    "loadGame"
    "loadIdentity"
    "loadMagazine"
    "loadOverlay"
    "loadStatus"
    "loadUniform"
    "loadVest"
    "local"
    "localize"
    "locationNull"
    "locationPosition"
    "lock"
    "lockCameraTo"
    "lockCargo"
    "lockDriver"
    "locked"
    "lockedCargo"
    "lockedDriver"
    "lockedTurret"
    "lockTurret"
    "lockWP"
    "log"
    "logEntities"
    "lookAt"
    "lookAtPos"
    "magazineCargo"
    "magazines"
    "magazinesAllTurrets"
    "magazinesAmmo"
    "magazinesAmmoCargo"
    "magazinesAmmoFull"
    "magazinesDetail"
    "magazinesDetailBackpack"
    "magazinesDetailUniform"
    "magazinesDetailVest"
    "magazinesTurret"
    "magazineTurretAmmo"
    "mapAnimAdd"
    "mapAnimClear"
    "mapAnimCommit"
    "mapAnimDone"
    "mapCenterOnCamera"
    "mapGridPosition"
    "markAsFinishedOnSteam"
    "markerAlpha"
    "markerBrush"
    "markerColor"
    "markerDir"
    "markerPos"
    "markerShape"
    "markerSize"
    "markerText"
    "markerType"
    "max"
    "members"
    "min"
    "mineActive"
    "mineDetectedBy"
    "missionConfigFile"
    "missionName"
    "missionNamespace"
    "missionStart"
    "mod"
    "modelToWorld"
    "modelToWorldVisual"
    "moonIntensity"
    "morale"
    "move"
    "moveInAny"
    "moveInCargo"
    "moveInCommander"
    "moveInDriver"
    "moveInGunner"
    "moveInTurret"
    "moveObjectToEnd"
    "moveOut"
    "moveTime"
    "moveTo"
    "moveToCompleted"
    "moveToFailed"
    "musicVolume"
    "name"
    "name location"
    "nameSound"
    "nearEntities"
    "nearestBuilding"
    "nearestLocation"
    "nearestLocations"
    "nearestLocationWithDubbing"
    "nearestObject"
    "nearestObjects"
    "nearObjects"
    "nearObjectsReady"
    "nearRoads"
    "nearSupplies"
    "nearTargets"
    "needReload"
    "netId"
    "netObjNull"
    "newOverlay"
    "nextMenuItemIndex"
    "nextWeatherChange"
    "nMenuItems"
    "numberToDate"
    "objectCurators"
    "objectFromNetId"
    "objectParent"
    "objNull"
    "objStatus"
    "onBriefingGroup"
    "onBriefingNotes"
    "onBriefingPlan"
    "onBriefingTeamSwitch"
    "onCommandModeChanged"
    "onDoubleClick"
    "onEachFrame"
    "onGroupIconClick"
    "onGroupIconOverEnter"
    "onGroupIconOverLeave"
    "onHCGroupSelectionChanged"
    "onMapSingleClick"
    "onPlayerConnected"
    "onPlayerDisconnected"
    "onPreloadFinished"
    "onPreloadStarted"
    "onShowNewObject"
    "onTeamSwitch"
    "openCuratorInterface"
    "openMap"
    "openYoutubeVideo"
    "opfor"
    "orderGetIn"
    "overcast"
    "overcastForecast"
    "owner"
    "param"
    "params"
    "parseNumber"
    "parseText"
    "parsingNamespace"
    "particlesQuality"
    "pi"
    "pickWeaponPool"
    "pitch"
    "playableSlotsNumber"
    "playableUnits"
    "playAction"
    "playActionNow"
    "player"
    "playerRespawnTime"
    "playerSide"
    "playersNumber"
    "playGesture"
    "playMission"
    "playMove"
    "playMoveNow"
    "playMusic"
    "playScriptedMission"
    "playSound"
    "playSound3D"
    "position"
    "positionCameraToWorld"
    "posScreenToWorld"
    "posWorldToScreen"
    "ppEffectAdjust"
    "ppEffectCommit"
    "ppEffectCommitted"
    "ppEffectCreate"
    "ppEffectDestroy"
    "ppEffectEnable"
    "ppEffectForceInNVG"
    "precision"
    "preloadCamera"
    "preloadObject"
    "preloadSound"
    "preloadTitleObj"
    "preloadTitleRsc"
    "preprocessFile"
    "preprocessFileLineNumbers"
    "primaryWeapon"
    "primaryWeaponItems"
    "primaryWeaponMagazine"
    "priority"
    "private"
    "processDiaryLink"
    "productVersion"
    "profileName"
    "profileNamespace"
    "profileNameSteam"
    "progressLoadingScreen"
    "progressPosition"
    "progressSetPosition"
    "publicVariable"
    "publicVariableClient"
    "publicVariableServer"
    "pushBack"
    "putWeaponPool"
    "queryItemsPool"
    "queryMagazinePool"
    "queryWeaponPool"
    "rad"
    "radioChannelAdd"
    "radioChannelCreate"
    "radioChannelRemove"
    "radioChannelSetCallSign"
    "radioChannelSetLabel"
    "radioVolume"
    "rain"
    "rainbow"
    "random"
    "rank"
    "rankId"
    "rating"
    "rectangular"
    "registeredTasks"
    "registerTask"
    "reload"
    "reloadEnabled"
    "remoteControl"
    "remoteExec"
    "remoteExecCall"
    "removeAction"
    "removeAllActions"
    "removeAllAssignedItems"
    "removeAllContainers"
    "removeAllCuratorAddons"
    "removeAllCuratorCameraAreas"
    "removeAllCuratorEditingAreas"
    "removeAllEventHandlers"
    "removeAllHandgunItems"
    "removeAllItems"
    "removeAllItemsWithMagazines"
    "removeAllMissionEventHandlers"
    "removeAllMPEventHandlers"
    "removeAllMusicEventHandlers"
    "removeAllPrimaryWeaponItems"
    "removeAllWeapons"
    "removeBackpack"
    "removeBackpackGlobal"
    "removeCuratorAddons"
    "removeCuratorCameraArea"
    "removeCuratorEditableObjects"
    "removeCuratorEditingArea"
    "removeDrawIcon"
    "removeDrawLinks"
    "removeEventHandler"
    "removeFromRemainsCollector"
    "removeGoggles"
    "removeGroupIcon"
    "removeHandgunItem"
    "removeHeadgear"
    "removeItem"
    "removeItemFromBackpack"
    "removeItemFromUniform"
    "removeItemFromVest"
    "removeItems"
    "removeMagazine"
    "removeMagazineGlobal"
    "removeMagazines"
    "removeMagazinesTurret"
    "removeMagazineTurret"
    "removeMenuItem"
    "removeMissionEventHandler"
    "removeMPEventHandler"
    "removeMusicEventHandler"
    "removePrimaryWeaponItem"
    "removeSecondaryWeaponItem"
    "removeSimpleTask"
    "removeSwitchableUnit"
    "removeTeamMember"
    "removeUniform"
    "removeVest"
    "removeWeapon"
    "removeWeaponGlobal"
    "removeWeaponTurret"
    "requiredVersion"
    "resetCamShake"
    "resetSubgroupDirection"
    "resistance"
    "resize"
    "resources"
    "respawnVehicle"
    "restartEditorCamera"
    "reveal"
    "revealMine"
    "reverse"
    "reversedMouseY"
    "roadsConnectedTo"
    "roleDescription"
    "ropeAttachedObjects"
    "ropeAttachedTo"
    "ropeAttachEnabled"
    "ropeAttachTo"
    "ropeCreate"
    "ropeCut"
    "ropeEndPosition"
    "ropeLength"
    "ropes"
    "ropeUnwind"
    "ropeUnwound"
    "rotorsForcesRTD"
    "rotorsRpmRTD"
    "round"
    "runInitScript"
    "safeZoneH"
    "safeZoneW"
    "safeZoneWAbs"
    "safeZoneX"
    "safeZoneXAbs"
    "safeZoneY"
    "saveGame"
    "saveIdentity"
    "saveJoysticks"
    "saveOverlay"
    "saveProfileNamespace"
    "saveStatus"
    "saveVar"
    "savingEnabled"
    "say"
    "say2D"
    "say3D"
    "scopeName"
    "score"
    "scoreSide"
    "screenToWorld"
    "scriptDone"
    "scriptName"
    "scriptNull"
    "scudState"
    "secondaryWeapon"
    "secondaryWeaponItems"
    "secondaryWeaponMagazine"
    "select"
    "selectBestPlaces"
    "selectDiarySubject"
    "selectedEditorObjects"
    "selectEditorObject"
    "selectionPosition"
    "selectLeader"
    "selectNoPlayer"
    "selectPlayer"
    "selectWeapon"
    "selectWeaponTurret"
    "sendAUMessage"
    "sendSimpleCommand"
    "sendTask"
    "sendTaskResult"
    "sendUDPMessage"
    "serverCommand"
    "serverCommandAvailable"
    "serverCommandExecutable"
    "serverName"
    "serverTime"
    "set"
    "setAccTime"
    "setAirportSide"
    "setAmmo"
    "setAmmoCargo"
    "setAperture"
    "setApertureNew"
    "setArmoryPoints"
    "setAttributes"
    "setAutonomous"
    "setBehaviour"
    "setBleedingRemaining"
    "setCameraInterest"
    "setCamShakeDefParams"
    "setCamShakeParams"
    "setCamUseTi"
    "setCaptive"
    "setCenterOfMass"
    "setCollisionLight"
    "setCombatMode"
    "setCompassOscillation"
    "setCuratorCameraAreaCeiling"
    "setCuratorCoef"
    "setCuratorEditingAreaType"
    "setCuratorWaypointCost"
    "setCurrentTask"
    "setCurrentWaypoint"
    "setDamage"
    "setDammage"
    "setDate"
    "setDebriefingText"
    "setDefaultCamera"
    "setDestination"
    "setDir"
    "setDirection"
    "setDrawIcon"
    "setDropInterval"
    "setEditorMode"
    "setEditorObjectScope"
    "setEffectCondition"
    "setFace"
    "setFaceAnimation"
    "setFatigue"
    "setFlagOwner"
    "setFlagSide"
    "setFlagTexture"
    "setFog"
    "setFog array"
    "setFormation"
    "setFormationTask"
    "setFormDir"
    "setFriend"
    "setFromEditor"
    "setFSMVariable"
    "setFuel"
    "setFuelCargo"
    "setGroupIcon"
    "setGroupIconParams"
    "setGroupIconsSelectable"
    "setGroupIconsVisible"
    "setGroupId"
    "setGroupIdGlobal"
    "setGroupOwner"
    "setGusts"
    "setHideBehind"
    "setHit"
    "setHitIndex"
    "setHitPointDamage"
    "setHorizonParallaxCoef"
    "setHUDMovementLevels"
    "setIdentity"
    "setImportance"
    "setLeader"
    "setLightAmbient"
    "setLightAttenuation"
    "setLightBrightness"
    "setLightColor"
    "setLightDayLight"
    "setLightFlareMaxDistance"
    "setLightFlareSize"
    "setLightIntensity"
    "setLightnings"
    "setLightUseFlare"
    "setLocalWindParams"
    "setMagazineTurretAmmo"
    "setMarkerAlpha"
    "setMarkerAlphaLocal"
    "setMarkerBrush"
    "setMarkerBrushLocal"
    "setMarkerColor"
    "setMarkerColorLocal"
    "setMarkerDir"
    "setMarkerDirLocal"
    "setMarkerPos"
    "setMarkerPosLocal"
    "setMarkerShape"
    "setMarkerShapeLocal"
    "setMarkerSize"
    "setMarkerSizeLocal"
    "setMarkerText"
    "setMarkerTextLocal"
    "setMarkerType"
    "setMarkerTypeLocal"
    "setMass"
    "setMimic"
    "setMousePosition"
    "setMusicEffect"
    "setMusicEventHandler"
    "setName"
    "setNameSound"
    "setObjectArguments"
    "setObjectMaterial"
    "setObjectProxy"
    "setObjectTexture"
    "setObjectTextureGlobal"
    "setObjectViewDistance"
    "setOvercast"
    "setOwner"
    "setOxygenRemaining"
    "setParticleCircle"
    "setParticleClass"
    "setParticleFire"
    "setParticleParams"
    "setParticleRandom"
    "setPilotLight"
    "setPiPEffect"
    "setPitch"
    "setPlayable"
    "setPlayerRespawnTime"
    "setPos"
    "setPosASL"
    "setPosASL2"
    "setPosASLW"
    "setPosATL"
    "setPosition"
    "setPosWorld"
    "setRadioMsg"
    "setRain"
    "setRainbow"
    "setRandomLip"
    "setRank"
    "setRectangular"
    "setRepairCargo"
    "setShadowDistance"
    "setSide"
    "setSimpleTaskDescription"
    "setSimpleTaskDestination"
    "setSimpleTaskTarget"
    "setSimulWeatherLayers"
    "setSize"
    "setSkill"
    "setSkill array"
    "setSlingLoad"
    "setSoundEffect"
    "setSpeaker"
    "setSpeech"
    "setSpeedMode"
    "setStatValue"
    "setSuppression"
    "setSystemOfUnits"
    "setTargetAge"
    "setTaskResult"
    "setTaskState"
    "setTerrainGrid"
    "setText"
    "setTimeMultiplier"
    "setTitleEffect"
    "setTriggerActivation"
    "setTriggerArea"
    "setTriggerStatements"
    "setTriggerText"
    "setTriggerTimeout"
    "setTriggerType"
    "setType"
    "setUnconscious"
    "setUnitAbility"
    "setUnitPos"
    "setUnitPosWeak"
    "setUnitRank"
    "setUnitRecoilCoefficient"
    "setUnloadInCombat"
    "setUserActionText"
    "setVariable"
    "setVectorDir"
    "setVectorDirAndUp"
    "setVectorUp"
    "setVehicleAmmo"
    "setVehicleAmmoDef"
    "setVehicleArmor"
    "setVehicleId"
    "setVehicleLock"
    "setVehiclePosition"
    "setVehicleTiPars"
    "setVehicleVarName"
    "setVelocity"
    "setVelocityTransformation"
    "setViewDistance"
    "setVisibleIfTreeCollapsed"
    "setWaves"
    "setWaypointBehaviour"
    "setWaypointCombatMode"
    "setWaypointCompletionRadius"
    "setWaypointDescription"
    "setWaypointFormation"
    "setWaypointHousePosition"
    "setWaypointLoiterRadius"
    "setWaypointLoiterType"
    "setWaypointName"
    "setWaypointPosition"
    "setWaypointScript"
    "setWaypointSpeed"
    "setWaypointStatements"
    "setWaypointTimeout"
    "setWaypointType"
    "setWaypointVisible"
    "setWeaponReloadingTime"
    "setWind"
    "setWindDir"
    "setWindForce"
    "setWindStr"
    "setWPPos"
    "show3DIcons"
    "showChat"
    "showCinemaBorder"
    "showCommandingMenu"
    "showCompass"
    "showCuratorCompass"
    "showGPS"
    "showHUD"
    "showLegend"
    "showMap"
    "shownArtilleryComputer"
    "shownChat"
    "shownCompass"
    "shownCuratorCompass"
    "showNewEditorObject"
    "shownGPS"
    "shownHUD"
    "shownMap"
    "shownPad"
    "shownRadio"
    "shownUAVFeed"
    "shownWarrant"
    "shownWatch"
    "showPad"
    "showRadio"
    "showSubtitles"
    "showUAVFeed"
    "showWarrant"
    "showWatch"
    "showWaypoint"
    "side"
    "sideChat"
    "sideEnemy"
    "sideFriendly"
    "sideLogic"
    "sideRadio"
    "sideUnknown"
    "simpleTasks"
    "simulationEnabled"
    "simulCloudDensity"
    "simulCloudOcclusion"
    "simulInClouds"
    "simulWeatherSync"
    "sin"
    "size"
    "sizeOf"
    "skill"
    "skillFinal"
    "skipTime"
    "sleep"
    "sliderPosition"
    "sliderRange"
    "sliderSetPosition"
    "sliderSetRange"
    "sliderSetSpeed"
    "sliderSpeed"
    "slingLoadAssistantShown"
    "soldierMagazines"
    "someAmmo"
    "sort"
    "soundVolume"
    "spawn"
    "speaker"
    "speed"
    "speedMode"
    "splitString"
    "sqrt"
    "squadParams"
    "stance"
    "startLoadingScreen"
    "step"
    "stop"
    "stopped"
    "str"
    "sunOrMoon"
    "supportInfo"
    "suppressFor"
    "surfaceIsWater"
    "surfaceNormal"
    "surfaceType"
    "swimInDepth"
    "switch"
    "switchableUnits"
    "switchAction"
    "switchCamera"
    "switchGesture"
    "switchLight"
    "switchMove"
    "synchronizedObjects"
    "synchronizedTriggers"
    "synchronizedWaypoints"
    "synchronizeObjectsAdd"
    "synchronizeObjectsRemove"
    "synchronizeTrigger"
    "synchronizeWaypoint"
    "synchronizeWaypoint trigger"
    "systemChat"
    "systemOfUnits"
    "tan"
    "targetKnowledge"
    "targetsAggregate"
    "targetsQuery"
    "taskChildren"
    "taskCompleted"
    "taskDescription"
    "taskDestination"
    "taskHint"
    "taskNull"
    "taskParent"
    "taskResult"
    "taskState"
    "teamMember"
    "teamMemberNull"
    "teamName"
    "teams"
    "teamSwitch"
    "teamSwitchEnabled"
    "teamType"
    "terminate"
    "terrainIntersect"
    "terrainIntersectASL"
    "text"
    "text location"
    "textLog"
    "textLogFormat"
    "tg"
    "throw"
    "time"
    "timeMultiplier"
    "titleCut"
    "titleFadeOut"
    "titleObj"
    "titleRsc"
    "titleText"
    "toArray"
    "toLower"
    "toString"
    "toUpper"
    "triggerActivated"
    "triggerActivation"
    "triggerArea"
    "triggerAttachedVehicle"
    "triggerAttachObject"
    "triggerAttachVehicle"
    "triggerStatements"
    "triggerText"
    "triggerTimeout"
    "triggerTimeoutCurrent"
    "triggerType"
    "turretLocal"
    "turretOwner"
    "turretUnit"
    "tvAdd"
    "tvClear"
    "tvCollapse"
    "tvCount"
    "tvCurSel"
    "tvData"
    "tvDelete"
    "tvExpand"
    "tvPicture"
    "tvSetCurSel"
    "tvSetData"
    "tvSetPicture"
    "tvSetValue"
    "tvSort"
    "tvSortByValue"
    "tvText"
    "tvValue"
    "type"
    "typeName"
    "typeOf"
    "UAVControl"
    "uiNamespace"
    "uiSleep"
    "unassignCurator"
    "unassignItem"
    "unassignTeam"
    "unassignVehicle"
    "underwater"
    "uniform"
    "uniformContainer"
    "uniformItems"
    "uniformMagazines"
    "unitAddons"
    "unitBackpack"
    "unitPos"
    "unitReady"
    "unitRecoilCoefficient"
    "units"
    "unitsBelowHeight"
    "unlinkItem"
    "unlockAchievement"
    "unregisterTask"
    "updateDrawIcon"
    "updateMenuItem"
    "updateObjectTree"
    "useAudioTimeForMoves"
    "vectorAdd"
    "vectorCos"
    "vectorCrossProduct"
    "vectorDiff"
    "vectorDir"
    "vectorDirVisual"
    "vectorDistance"
    "vectorDistanceSqr"
    "vectorDotProduct"
    "vectorFromTo"
    "vectorMagnitude"
    "vectorMagnitudeSqr"
    "vectorMultiply"
    "vectorNormalized"
    "vectorUp"
    "vectorUpVisual"
    "vehicle"
    "vehicleChat"
    "vehicleRadio"
    "vehicles"
    "vehicleVarName"
    "velocity"
    "velocityModelSpace"
    "verifySignature"
    "vest"
    "vestContainer"
    "vestItems"
    "vestMagazines"
    "viewDistance"
    "visibleCompass"
    "visibleGPS"
    "visibleMap"
    "visiblePosition"
    "visiblePositionASL"
    "visibleWatch"
    "waves"
    "waypointAttachedObject"
    "waypointAttachedVehicle"
    "waypointAttachObject"
    "waypointAttachVehicle"
    "waypointBehaviour"
    "waypointCombatMode"
    "waypointCompletionRadius"
    "waypointDescription"
    "waypointFormation"
    "waypointHousePosition"
    "waypointLoiterRadius"
    "waypointLoiterType"
    "waypointName"
    "waypointPosition"
    "waypoints"
    "waypointScript"
    "waypointsEnabledUAV"
    "waypointShow"
    "waypointSpeed"
    "waypointStatements"
    "waypointTimeout"
    "waypointTimeoutCurrent"
    "waypointType"
    "waypointVisible"
    "weaponAccessories"
    "weaponCargo"
    "weaponDirection"
    "weaponLowered"
    "weapons"
    "weaponsItems"
    "weaponsItemsCargo"
    "weaponState"
    "weaponsTurret"
    "weightRTD"
    "west"
    "WFSideText"
    "wind"
    "windDir"
    "windStr"
    "wingsForcesRTD"
    "worldName"
    "worldSize"
    "worldToModel"
    "worldToModelVisual"
    "worldToScreen"
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;;;;;;;;
;; Indentation functions

(defun sqf-mode-substatement-open (langelem)
  "Lineup hanging braces"
  (save-excursion
    ;;(edebug)
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-beginning-of-statement 1)
    (back-to-indentation)
    ;;(message (concat "substatement-open: " (number-to-string (current-column))))
    (vector (current-column))))

(defun sqf-mode-statement-block-intro (langelem)
  "indent braces"
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-beginning-of-statement 1)
    ;; TODO if line is comment-intro go back 2
    (back-to-indentation)
    ;; (message (concat "block-intro: " (number-to-string (current-column))))
    (vector (+ c-basic-offset (current-column)))))

(defun sqf-mode-block-close (langelem)
  "close braces"
  (save-excursion
    (back-to-indentation)
    (forward-char)
    (backward-sexp)
    (back-to-indentation)
    ;; (message (concat "block-close: " (number-to-string (current-column))))
    (vector (current-column))))

(defun sqf-mode-defun-close (langelem)
  "close braces"
  (save-excursion
    (back-to-indentation)
    (c-beginning-of-defun 1)
    (back-to-indentation)
    ;; (message (concat "defun-close: " (number-to-string (current-column))))
    (vector (current-column))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;;;;;;;;
;; define the mode hook
(defvar sqf-mode-hook nil)

;; section for keybinds
(defvar sqf-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Add bindings which are only useful for SQF.
    map)
  "Keymap used in sqf-mode buffers")

(defvar sqf-font-lock-defaults
  `(,(append c-font-lock-keywords-1
          `(( ,(regexp-opt sqf-mode-builtins 'words) . font-lock-builtin-face)
            ( ,(regexp-opt sqf-mode-keywords 'words) . font-lock-keyword-face)
           ))
    nil 1))

(defvar sqf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?' "\"" st)
    st)
  "Syntax table for `sqf-mode'.")

;; (put 'sqf-mode 'c-mode-prefix "sqf-")
;; (defvar sqf-mode-syntax-table
;;   (funcall (c-lang-const c-make-mode-syntax-table sqf))
;;   "Syntax table used in sqf-mode buffers.")

;; ;;;###autoload
;; (defconst sqf-c-style
;;   `((c-offsets-alist . ((substatement-open . sqf-mode-substatement-open)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sqf\\'" . sqf-mode))

;;;###autoload
(define-derived-mode sqf-mode c++-mode "SQF"
  "Major mode for the SQF language.
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `sqf-mode-hook'.

Key bindings:
\\{sqf-mode-map}"
  ;; (c-initialize-cc-mode t)
  :syntax-table sqf-mode-syntax-table
  ;; (use-local-map sqf-mode-map)
  ;; (c-init-language-vars-for 'sqf-mode)
  ;; (c-common-init 'sqf-mode)
  ;; (c-add-style sqf-c-style "sqf")
  (c-set-offset 'substatement-open 'sqf-mode-substatement-open)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'brace-list-close 0)
  ;; TODO braces lsit close
  (c-set-offset 'arglist-cont-nonempty 0)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro +)
  (c-set-offset 'defun-close 'sqf-mode-defun-close)
  (c-set-offset 'block-close 'sqf-mode-block-close)
  (c-set-offset 'statement-block-intro 'sqf-mode-statement-block-intro)
  (setq font-lock-defaults sqf-font-lock-defaults)
  (setq font-lock-keywords-case-fold-search 1)
  (setq c-special-indent-hook nil)
  ;; (c-run-mode-hooks 'c-mode-common-hook 'sqf-mode-hook)
  ;; (c-update-modeline)
  )

(defalias 'arma-mode 'sqf-mode)
(provide 'arma-mode)

;;; arma-mode.el ends here
