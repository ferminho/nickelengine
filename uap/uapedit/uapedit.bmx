' UAPEdit - formerly known as VBAEdit from VoiD.net
' -----------
' Started 15-04-2006
' Latest VBAEdit Version 07-01-2007
' From that point on... use HG to determine
'
' Unif Studios 2006/2014

SuperStrict 

'Framework BRL.GlMax2D
?Win32
Import MaxGUI.Win32MaxGUIex
?MacOS
Import MaxGUI.CocoaMaxGUI
?Linux
Import MaxGUI.FLTKMaxGUI
?
Import BRL.PNGLoader
Import MaxGUI.MaxGUI
Import BRL.Retro

Import "voidfpgvgp.bmx"	' FPG/VGP manager
Import "../../graphics/ugp.bmx" ' UGPv2 new format
Import "voidvba.bmx"	' VBA system
Import "voidmath.bmx"	' math module


' ******** TIME CONTROL

' Time
Global InitialTime:Int = 0	' time 0 value
Global TD:Float = 1.0 ' Time distortion
Global TDDest:Float = 1.0	' Destination TD
Global TDOrig:Float = 1.0	' Origin TD
Global TDDestType:Int = 0	' 0 - uniform | 1 - logaritmic
Global TDTime1:Float = 0	' starting time
Global TDTime2:Float = 0	' finishing time
Global ActTime:Float = 0 ' Actual moment in time (ms) (DISTORTED)
Global LastTimer:Int = 0 ' Last frame's moment in time (ms) (REAL)
Global LastFrame:Float = 0.0 ' Last frame's duration in millisecs
Global RLastFrame:Float = 0.0 ' REAL Last frame's duration (no TD involved)
Global LastFPS:Int = 0	' Last moment in REAL time FPS were checked
Global FPScount:Int = 0	' Frames per actual second
Global FPS:Int = 0	' FPS (checked each second)
' Timer Backup
Global TDbak:Float = 1.0	' BACKUP to save/restore
Global TDDestbak:Float = 1.0
Global TDOrigbak:Float = 1.0
Global TDDestTypebak:Int = 0
Global TDTime1bak:Float = 0
Global TDTime2bak:Float = 0
Global ActTimebak:Float = 0
Global LastFramebak:Float = 0.0



Function Timer ()
' to call each frame first to calculate last frame's time
	Local Temp:Int = MilliSecs ()
	
	If (TDDest <> TD) Then
		Local RemTime:Int = TDTime2 - ActTime
		If (RemTime > 0)
			Select (TDDestType)
				Case 0
					' y = y1 + (((x - t1) / (t2 - t1)) * (y2 - y1))
					TD = TDOrig + (((ActTime - TDTime1)/(TDTime2 - TDTime1)) * (TDDest - TDOrig))
				Case 1
					' y = y1 + (((1 - ((x - t2) / (t2 - t1)) ^ 4) ^ (1 / 2)) * (y2 - y1))
					TD = TDOrig + ((Sqr (1.0 - ((ActTime - TDTime2)/(TDTime2 - TDTime1))^4.0)) * (TDDest - TDOrig))
			End Select
		Else
			TD = TDDest
		EndIf
	EndIf

	RLastFrame = Temp - LastTimer
	LastFrame = RLastFrame * TD
	ActTime:+ (RLastFrame * TD)
	LastTimer = Temp
	
	FPScount:+1
	'FPS update
	If (LastTimer > (LastFPS + 1000)) Then
		LastFPS = LastTimer
		FPS = FPScount
		FPScount = 0
	EndIf
End Function

Function SaveTimer ()
	TDbak = TD
	TDDestbak = TDDest
	TDOrigbak = TDOrig
	TDDestTypebak = TDDestType
	TDTime1bak = TDTime1
	TDTime2bak = TDTime2
	ActTimebak = ActTime
	LastFramebak = LastFrame
End Function


Function RestoreTimer ()
	TD = TDbak
	TDDest = TDDestbak
	TDOrig = TDOrigbak
	TDDestType = TDDestTypebak
	TDTime1 = TDTime1bak
	TDTime2 = TDTime2bak
	ActTime = ActTimebak
	LastFrame = LastFramebak
	LastTimer = MilliSecs ()
End Function

' *******************





' *********** CONSTS

Const AboutLine:String = "~n~nVoiD.net : VBA Edit 1.0 ~n by Ferminho~n~nUnif Studios 2006"


Const FILE_NUEVO:Int = 101
Const FILE_ABRIR:Int = 102
Const FILE_GUARDAR:Int = 103
Const FILE_GUARDARCOMO:Int = 104
Const FILE_SALIR:Int = 105
Const FILE_EXPORTAR:Int = 106

Const EDIT_COPIAR:Int = 201
Const EDIT_PEGAR:Int = 202
Const EDIT_ANT:Int = 203
Const EDIT_SIG:Int = 204
Const EDIT_ANTA:Int = 205
Const EDIT_SIGA:Int = 206
Const EDIT_ANTF:Int = 207
Const EDIT_SIGF:Int = 208

Const VIEW_PREVIEW:Int = 301
Const VIEW_BONEW:Int = 302
Const VIEW_ANIMSW:Int = 303
Const VIEW_SKEL:Int = 305
Const VIEW_GRID:Int = 306
Const VIEW_RESTORE:Int = 307

Const SET_HEIGHT:Int = 401
Const SET_ANIMDEL:Int = 402
Const SET_NAME:Int = 403

Const HELP_AYUDA:Int = 501
Const HELP_ACERCA:Int = 502

' *******************





' *********** PROGRAM GLOBALS


Global Bones:Int[64]
Global BoneSelected:Int = -1
Global LastBone:Int = -1

Type SBonePos
	Field X:Float = 0.0
	Field Y:Float = 0.0
End Type
Global BonePositions:SBonePos[64]
Local ICount:Int
For ICount = 0 To 63
	BonePositions[ICount] = New SBonePos
Next


Global V:VBA = Null
Global A:AnimState = Null
Global FileUGP:UGP = Null
Global FileVGP:VGP = Null

Global SavePath:String = "untitled.uap"
Global SaveFile:String = "untitled.uap"
Global UGPVGPName:String = "defbones.vgp"

Global IsSaved:Int = True
Global LastSaved:Int = True
Global PreviewVisible:Int = True
Global BonewVisible:Int = True
Global AnimswVisible:Int = True
Global SkelVisible:Int = True
Global GridVisible:Int = True
Global IsPlaying:Int = False
Global RedrawMain:Int = True
Global RedrawBone:Int = True
Global RedrawPreview:Int = True
Global AnimDelay:Int = 0	' postit : añadelo a los menus de Set para cambiarlo

Global Clipboard:VBAFrame = Null

Global Help:Int = True
Global TimeToHelp:Int = 0

Global ActAnim:Int = 0
Global ActFrame:Int = 0
Global ActFrameF:VBAFrame = Null

Global Shine:Int = 32

Global Cancel:Int = False
Global ShiftStatus:Int = False

Global PreviewZoom:Float = 1.0
Global PreviewX:Float = 0.0
Global PreviewY:Float = 0.0
Global MainZoom:Float = 1.0
Global MainZoomC:Float = 0.0
Global MainX:Float = 0.0
Global MainY:Float = 0.0

Global Stack1:VBABone[64]	' keeps pointers to bone (for recursive calcultation)
Global Stack2:Byte[64]		' keeps next son number to process
Global Stack3:Float[64]		' keeps processed angle
Global Stack4:Float[64]		' keeps processed X
Global Stack5:Float[64]		' keeps processed Y
Global OnStack:Int = -1		' tells which element in the stack is the top (-1 = empty)

Global SW:Int = GadgetWidth (Desktop ())
Global SH:Int = GadgetHeight (Desktop ())			' Screen Width & Height (Desktop)

Global FromWhereX:Float = -1.0
Global FromWhereY:Float = -1.0
Global Dragging:Int = 0	' 0-none 1-main/move 2-preview/move
						' 3-main/rotate_bone 4-main/move_bone

Global Grid:TImage = LoadImage ("grid.png", MIPMAPPEDIMAGE | FILTEREDIMAGE)
Global GraphicNotFound:TImage = LoadImage ("gnf.png", MIPMAPPEDIMAGE | FILTEREDIMAGE)
MidHandleImage (Grid)	' auto middle
MidHandleImage (GraphicNotFound)


' *******************





' *********** MAIN LOCALS

Local Evento:TEvent = Null
Local I:Int = 0, J:Int = 0
Local S:String = ""
Local X:Float = 0.0, Y:Float = 0.0
Local TX:Float = 0.0, TY:Float = 0.0, TA:Float = 0.0
Local Ani:VBAAnim = Null
Local Frame:VBAFrame = Null
Local F1P:VBAFrame, F1N:VBAFrame, F2P:VBAFrame, F2N:VBAFrame
Local VB:VBABone = Null
Local RB:RepBone = Null


' *******************


Global MARGINS:Int = 3

Global W_WINDOW:Int = 1200
Global H_WINDOW:Int = 700
Global H_STATUSBAR:Int = 75

Global W_MAIN:Int = 520
Global H_MAIN:Int = 540
Global W2_MAIN:Int = W_MAIN / 2
Global H2_MAIN:Int = H_MAIN / 2
Global W2_MAIN_F:Float = Float(W2_MAIN)
Global H2_MAIN_F:Float = Float(H2_MAIN)

Global W_BONES:Int = 305
Global H_BONES:Int = 370

' *********** MAIN


Global MainWindow:TGadget = CreateWindow ("", 50, 50, W_WINDOW, H_WINDOW, Null, ..
							WINDOW_STATUS | WINDOW_TITLEBAR | WINDOW_MENU)
Global BonesWindow:TGadget = CreatePanel (W_MAIN + MARGINS, 0, W_BONES, H_BONES, MainWindow, ..
							PANEL_GROUP, "Bones")
Global PreviewWindow:TGadget = CreatePanel (W_MAIN + MARGINS + W_BONES + MARGINS, 0, ..
							W_WINDOW - W_MAIN - MARGINS - W_BONES - MARGINS - 10, H_WINDOW - H_STATUSBAR, MainWindow, ..
							PANEL_GROUP, "Preview")
Global AnimsWindow:TGadget = CreatePanel (MARGINS, H_MAIN, W_MAIN - MARGINS, H_WINDOW - H_MAIN - H_STATUSBAR, MainWindow, ..
							PANEL_GROUP, "Anims")

Global TheAbout:TGadget = CreateWindow ("About", 300, 193, 200, 215, MainWindow, ..
							WINDOW_TITLEBAR | WINDOW_TOOL | WINDOW_HIDDEN)
Global TextAbout:TGadget = CreateLabel (AboutLine, 10, 10, 180, 150, TheAbout, ..
							LABEL_FRAME | LABEL_CENTER)
Global AboutOK:TGadget = CreateButton ("Nice!", 70, 165, 60, 20, TheAbout, BUTTON_CANCEL)

Global TextRequest:TGadget = CreateWindow ("Text Input", 100, 220, 600, 60, MainWindow, ..
							WINDOW_TITLEBAR | WINDOW_TOOL | WINDOW_HIDDEN)
Global TextRequestL:TGadget = CreateLabel ("", 20, 2, 560, 15, TextRequest, ..
											LABEL_CENTER)
Global TextRequestT:TGadget = CreateTextArea (20, 19, 560, 18, TextRequest)

Global PNGInfo:TGadget = CreateWindow ("Export Frame to PNG", 225, 270, 350, 140, MainWindow, ..
							WINDOW_TITLEBAR | WINDOW_TOOL | WINDOW_HIDDEN)
Global PNGInfoFPS:TGadget = CreateButton ("FPS", 15, 5, 50, 20, PNGInfo, BUTTON_RADIO)
Global PNGInfoFrames:TGadget = CreateButton ("Number of Frames", 70, 5, 110, 20, PNGInfo, BUTTON_RADIO)
Global PNGInfoPower2:TGadget = CreateButton ("Power of 2", 15, 30, 75, 20, PNGInfo, BUTTON_CHECKBOX)
Global PNGInfoSquare:TGadget = CreateButton ("Square", 100, 30, 60, 20, PNGInfo, BUTTON_CHECKBOX)
Global PNGInfoCentered:TGadget = CreateButton ("Centered", 170, 30, 80, 20, PNGInfo, BUTTON_CHECKBOX)
CreateLabel ("Scale X", 15, 62, 45, 20, PNGInfo, LABEL_CENTER)
Global PNGInfoSX:Tgadget = CreateTextArea (70, 60, 50, 20, PNGInfo)
SetTextAreaText (PNGInfoSX, "1.0")
CreateLabel ("Scale Y", 130, 62, 45, 20, PNGInfo, LABEL_CENTER)
Global PNGInfoSY:Tgadget = CreateTextArea (185, 60, 50, 20, PNGInfo)
SetTextAreaText (PNGInfoSY, "1.0")
Global PNGProgress:TGadget = CreateProgBar (10, 95, 325, 20, PNGInfo)
UpdateProgBar (PNGProgress, 0.0)
Global PNGInfoN:TGadget = CreateTextArea (195, 5, 40, 20, PNGInfo)
SetTextAreaText (PNGInfoN, "1")
Global PNGInfoOK:TGadget = CreateButton ("Export", 270, 15, 45, 26, PNGInfo, ..
									BUTTON_PUSH | BUTTON_OK)
Global PNGInfoCancel:TGadget = CreateButton ("Cancel", 270, 50, 45, 26, PNGInfo, ..
									BUTTON_PUSH | BUTTON_CANCEL)
SetButtonState (PNGInfoFrames, True)

Global WindowHidden:TGadget = CreateWindow ("", 0, 0, 900, 900, MainWindow, WINDOW_HIDDEN)
Global BigCanvas:TGadget = CreateCanvas (0, 0, SW, SH, WindowHidden)

Global FileMenu:TGadget = CreateMenu ("File", 0, WindowMenu (MainWindow), 0, MODIFIER_COMMAND)
	CreateMenu ("&New", FILE_NUEVO, FileMenu, KEY_N, MODIFIER_COMMAND)
	CreateMenu ("&Open", FILE_ABRIR, FileMenu, KEY_O, MODIFIER_COMMAND)
	CreateMenu ("&Save", FILE_GUARDAR, FileMenu, KEY_S, MODIFIER_COMMAND)
	CreateMenu ("Save &as", FILE_GUARDARCOMO, FileMenu, KEY_A, MODIFIER_COMMAND)
	CreateMenu ("", 0, FileMenu)
	CreateMenu ("Export Anim", FILE_EXPORTAR, FileMenu)
	CreateMenu ("", 0, FileMenu)
	CreateMenu ("Quit", FILE_SALIR, FileMenu, KEY_Q, MODIFIER_COMMAND)
Global EditMenu:TGadget = CreateMenu ("Edit", 0, WindowMenu (MainWindow), 0, MODIFIER_COMMAND)
	CreateMenu ("Copy Frame", EDIT_COPIAR, EditMenu, KEY_C, MODIFIER_COMMAND)
	Global Paste:TGadget = CreateMenu ("Paste Frame", EDIT_PEGAR, EditMenu, KEY_V, MODIFIER_COMMAND)
	DisableMenu (Paste)
	CreateMenu ("", 0, EditMenu)
	CreateMenu ("Previous Bone", EDIT_ANT, EditMenu, KEY_INSERT, MODIFIER_COMMAND)
	CreateMenu ("Next Bone", EDIT_SIG, EditMenu, KEY_DELETE, MODIFIER_COMMAND)
	CreateMenu ("", 0, EditMenu)
	CreateMenu ("Previous Anim", EDIT_ANTA, EditMenu, KEY_HOME, MODIFIER_COMMAND)
	CreateMenu ("Next Anim", EDIT_SIGA, EditMenu, KEY_END, MODIFIER_COMMAND)
	CreateMenu ("", 0, EditMenu)
	CreateMenu ("Previous Frame", EDIT_ANTF, EditMenu, KEY_PAGEUP, MODIFIER_COMMAND)
	CreateMenu ("Next Frame", EDIT_SIGF, EditMenu, KEY_PAGEDOWN, MODIFIER_COMMAND)
	CreateMenu ("", 0, EditMenu)
Global ViewMenu:TGadget = CreateMenu ("View", 0, WindowMenu (MainWindow), 0, MODIFIER_COMMAND)
	Global ViewWindows:TGadget = CreateMenu ("Windows", 0, ViewMenu)
	Global ViewPreview:TGadget = CreateMenu ("v Preview Window", VIEW_PREVIEW, ViewWindows)
	Global ViewBoneW:TGadget = CreateMenu ("v Bones Window", VIEW_BONEW, ViewWindows)
	Global ViewAnimsW:TGadget = CreateMenu ("v Anims Window", VIEW_ANIMSW, ViewWindows)
	CreateMenu ("", 0, ViewMenu)
	Global ViewGrid:TGadget = CreateMenu ("v Grid", VIEW_GRID, ViewMenu)
	Global ViewSkel:TGadget = CreateMenu ("v Skeleton", VIEW_SKEL, ViewMenu)
	CreateMenu ("", 0, ViewMenu)
	CreateMenu ("Restore Window &View", VIEW_RESTORE, ViewMenu, KEY_V, MODIFIER_COMMAND)
Global SetMenu:TGadget = CreateMenu ("Set", 0, WindowMenu (MainWindow), 0, MODIFIER_COMMAND)
	Global SetHeight:TGadget = CreateMenu ("Height [0.00000000]", SET_HEIGHT, SetMenu)
	Global SetAnimDelay:TGadget = CreateMenu ("Animation Delay [0]", SET_ANIMDEL, SetMenu)
	CreateMenu ("", 0, SetMenu)
	Global SetName:TGadget = CreateMenu ("Bone Name", SET_NAME, SetMenu)
Global HelpMenu:TGadget = CreateMenu ("Help", 0, WindowMenu (MainWindow), 0, MODIFIER_COMMAND)
	CreateMenu ("Quick Help", HELP_AYUDA, HelpMenu, KEY_H, MODIFIER_COMMAND)
	CreateMenu ("About", HELP_ACERCA, HelpMenu)

Global MainCanvas:TGadget = CreateCanvas (0, 0, W_MAIN, H_MAIN, MainWindow)


Global BoneCanvas:TGadget = CreateCanvas (0, 0, 150, 150, BonesWindow)

'Global BoneNBones:TGadget = CreateLabel ("Bones : 0", 155, 0, 138, 15, BonesWindow, ..
'										LABEL_CENTER | LABEL_FRAME)
Global BoneList:TGadget = CreateListBox (155, 0, 138, 283, BonesWindow)
Global BoneX:TGadget = CreateLabel ("", 155, 288, 138, 15, BonesWindow, LABEL_CENTER)
Global BoneY:TGadget = CreateLabel ("", 155, 303, 138, 15, BonesWindow, LABEL_CENTER)
Global BoneAngle:TGadget = CreateLabel ("", 155, 318, 138, 15, BonesWindow, LABEL_CENTER)
Global BoneSize:TGadget = CreateLabel ("", 155, 333, 138, 15, BonesWindow, LABEL_CENTER)
Global BonePanel:TGadget = CreatePanel (5, 155, 140, 70, BonesWindow, ..
										PANEL_BORDER)
CreateLabel ("VGP & Bone Graph Range", 5, 3, 127, 15, BonePanel, LABEL_CENTER)
Global BoneVGP:TGadget = CreateLabel ("defbones.vgp", 5, 20, 127, 15, BonePanel, ..
										LABEL_CENTER | LABEL_FRAME)
Global BoneRange1:TGadget = CreateTextArea (2, 42, 31, 20, BonePanel)
SetTextAreaText (BoneRange1 , "1")
Global BoneRange2:TGadget = CreateTextArea (104, 42, 31, 20, BonePanel)
SetTextAreaText (BoneRange2, "1")
Global BoneGraph:TGadget = CreateLabel ("1", 57, 42, 25, 20, BonePanel, ..
										LABEL_CENTER | LABEL_FRAME)
Global BoneGraphS:TGadget = CreateSlider (34, 42, 18, 20, BonePanel, SLIDER_STEPPER)
SetSliderRange (BoneGraphS, 1, 3)
SetSliderValue (BoneGraphS, 2)
Global BoneGraphX:TGadget = CreateButton ("X", 86, 42, 17, 20, BonePanel)
Global BoneNew:TGadget = CreateButton ("New", 10, 230, 110, 20, BonesWindow)
Global BoneDelete:TGadget = CreateButton ("Delete", 10, 250, 110, 20, BonesWindow)
Global BoneChangeSize:TGadget = CreateButton ("Size", 10, 280, 110, 20, BonesWindow)
Global BoneChangeVGP:TGadget = CreateButton ("Change UGP", 10, 310, 110, 20, BonesWindow)
CreateLabel ("Z", 134, 233, 8, 15, BonesWindow, LABEL_CENTER)
Global BoneZChange:TGadget = CreateSlider (128, 250, 18, 95, BonesWindow, SLIDER_STEPPER)
SetSliderRange (BoneZChange, 1, 3)
SetSliderValue (BoneZChange, 2)

Global W_PREVIEW:Int = 350
Global H_PREVIEW:Int = 500
Global W2_PREVIEW:Int = W_PREVIEW / 2
Global H2_PREVIEW:Int = H_PREVIEW / 2
Global W2_PREVIEW_F:Float = Float(W2_PREVIEW)
Global H2_PREVIEW_F:Float = Float(H2_PREVIEW)

Global PreviewCanvas:TGadget = CreateCanvas (0, 0, W_PREVIEW, H_PREVIEW, PreviewWindow)	

Global PreviewPanelBig:TGadget = CreatePanel (0, H_PREVIEW + MARGINS, W_PREVIEW, 100, PreviewWindow, ..
							PANEL_BORDER)
Global PreviewZoomL:TGadget = CreateLabel ("Zoom", MARGINS, MARGINS, 30, 15, PreviewPanelBig, LABEL_CENTER)
Global PreviewZoomT:TGadget = CreateLabel ("1x", MARGINS, MARGINS + 15, 30, 15, PreviewPanelBig, LABEL_CENTER)
Global PreviewZoomS:TGadget = CreateSlider (40, MARGINS, W_PREVIEW - 40, 30, PreviewPanelBig, ..
							SLIDER_HORIZONTAL | SLIDER_TRACKBAR)
SetSliderRange (PreviewZoomS, -9, 9)
SetSliderValue (PreviewZoomS, 0)
Global PreviewTimeL:TGadget = CreateLabel ("Time", MARGINS, 35, 30, 15, PreviewPanelBig, LABEL_CENTER)
Global PreviewTimeT:TGadget = CreateLabel ("1x", MARGINS, 50, 30, 15, PreviewPanelBig, LABEL_CENTER)
Global PreviewTimeS:TGadget = CreateSlider (40, 35, W_PREVIEW - 40, 30, PreviewPanelBig, ..
							SLIDER_HORIZONTAL | SLIDER_TRACKBAR)
SetSliderRange (PreviewTimeS, -9, 9)
SetSliderValue (PreviewTimeS, 0)
Global PreviewPlay:TGadget = CreateButton ("Play", MARGINS, 70, W_PREVIEW - MARGINS * 3, 25, PreviewPanelBig)

CreateLabel ("Anim", 2, 5, 33, 17, AnimsWindow, LABEL_CENTER)
CreateLabel ("Frame", 2, 25, 33, 17, AnimsWindow, LABEL_CENTER)
Global AnimsActAnim:TGadget = CreateLabel ("0", 37, 2, 25, 20, AnimsWindow, ..
										LABEL_CENTER | LABEL_FRAME)
Global AnimsActFrame:TGadget = CreateLabel ("0", 37, 22, 25, 20, AnimsWindow, ..
										LABEL_CENTER | LABEL_FRAME)
CreateLabel ("/", 62, 5, 9, 20, AnimsWindow, LABEL_CENTER)
CreateLabel ("/", 62, 25, 9, 20, AnimsWindow, LABEL_CENTER)
Global AnimsNAnims:TGadget = CreateLabel ("0", 71, 2, 25, 20, AnimsWindow, ..
										LABEL_CENTER | LABEL_FRAME)
Global AnimsNFrames:TGadget = CreateLabel ("0", 71, 22, 25, 20, AnimsWindow, ..
										LABEL_CENTER | LABEL_FRAME)
Global AnimsAnimS:TGadget = CreateSlider (98, 2, 21, 20, AnimsWindow, SLIDER_STEPPER)
SetSliderRange (AnimsAnimS, 1, 3)
SetSliderValue (AnimsAnimS, 2)
Global AnimsFrameS:TGadget = CreateSlider (98, 22, 21, 20, AnimsWindow, SLIDER_STEPPER)
SetSliderRange (AnimsFrameS, 1, 3)
SetSliderValue (AnimsFrameS, 2)
Global AnimsGotoAnimT:TGadget = CreateTextArea (122, 2, 29, 20, AnimsWindow)
SetTextAreaText (AnimsGotoAnimT, "0")
Global AnimsGotoFrameT:TGadget = CreateTextArea (122, 22, 29, 20, AnimsWindow)
SetTextAreaText (AnimsGotoFrameT, "0")
Global AnimsGotoAnim:TGadget = CreateButton ("Goto", 153, 2, 32, 20, AnimsWindow)
Global AnimsGotoFrame:TGadget = CreateButton ("Goto", 153, 22, 32, 20, AnimsWindow)

Global AnimsAddAnim:TGadget = CreateButton ("Add", 195, 2, 35, 20, AnimsWindow)
Global AnimsAddDupeAnim:TGadget = CreateButton ("Add Dupe", 230, 2, 65, 20, AnimsWindow)
Global AnimsInsAnim:TGadget = CreateButton ("Insert", 295, 2, 40, 20, AnimsWindow)
Global AnimsInsDupeAnim:TGadget = CreateButton ("Insert Dupe", 335, 2, 75, 20, AnimsWindow)
Global AnimsClrAnim:TGadget = CreateButton ("Clear", 410, 2, 40, 20, AnimsWindow)
Global AnimsDelAnim:TGadget = CreateButton ("Delete", 450, 2, 45, 20, AnimsWindow)

Global AnimsAddFrame:TGadget = CreateButton ("Add", 195, 22, 35, 20, AnimsWindow)
Global AnimsAddDupeFrame:TGadget = CreateButton ("Add Dupe", 230, 22, 65, 20, AnimsWindow)
Global AnimsInsFrame:TGadget = CreateButton ("Insert", 295, 22, 40, 20, AnimsWindow)
Global AnimsInsDupeFrame:TGadget = CreateButton ("Insert Dupe", 335, 22, 75, 20, AnimsWindow)
Global AnimsClrFrame:TGadget = CreateButton ("Clear", 410, 22, 40, 20, AnimsWindow)
Global AnimsDelFrame:TGadget = CreateButton ("Delete", 450, 22, 45, 20, AnimsWindow)

CreateLabel ("Frame Duration ms", 2, 47, 100, 17, AnimsWindow, LABEL_CENTER)
Global AnimsFrameDur:TGadget = CreateLabel ("100", 105, 44, 37, 20, AnimsWindow, ..
								LABEL_CENTER | LABEL_FRAME)
Global AnimsFrameDurT:TGadget = CreateTextArea (145, 44, 43, 20, AnimsWindow)
Global AnimsFrameDurB:TGadget = CreateButton ("Set", 190, 44, 40, 20, AnimsWindow)
CreateLabel ("Exchange", 265, 47, 50, 17, AnimsWindow, LABEL_CENTER)
Global AnimsExchangeT:TGadget = CreateTextArea (320, 44, 25, 20, AnimsWindow)
Global AnimsExchangeAnim:TGadget = CreateButton ("Anim", 345, 44, 45, 20, AnimsWindow)
Global AnimsExchangeFrame:TGadget = CreateButton ("Frame", 390, 44, 45, 20, AnimsWindow)

UpdateWindowMenu (MainWindow)
ActivateWindow (MainWindow)

FileVGP = VGP.LoadVGP ("defbones.vgp")
FileUGP = Null
UGPVGPName = "defbones.vgp"

FileNew ()
InitVTrigonometric ()	' init cos & sin table
SetStatusText (MainWindow, "UAPEdit Started")
Help = False
TimeToHelp = MilliSecs () + 5000

InitialTime = MilliSecs ()
ActTime = 0.0
LastTimer = InitialTime
LastFrame = 0.0
RLastFrame = 0.0

Local piece:Int = MilliSecs ()

While True
	Timer ()
	Shine:+1
	If (Shine > 128) Then Shine = 32
	If (Not Help)	' Status text has recent info
		If (MilliSecs () > TimeToHelp)
			TimeToHelp = 0
			Help = True
			SetStatusText (MainWindow, "")
		EndIf
	EndIf
	If (IsSaved <> LastSaved)
		If (IsSaved)
			SetGadgetText (MainWindow, "UAPEdit - " + SaveFile)
		Else
			SetGadgetText (MainWindow, "UAPEdit - " + SaveFile + "*")
		EndIf
		LastSaved = IsSaved
	EndIf
	BoneSelected = SelectedGadgetItem (BoneList)
	If (BoneSelected <> LastBone)
		UpdateBoneInfo ()
		LastBone = BoneSelected
		RedrawBone = True
	EndIf

	While (PollEvent ())

		Select (EventID ())
			Case EVENT_GADGETPAINT
			' REPINTAR ALGO
				Select (EventSource ())
					Case MainCanvas
						RedrawMain = True
					Case PreviewCanvas
						RedrawPreview = True
					Case BoneCanvas
						RedrawBone = True
				End Select
			Case EVENT_MOUSEDOWN
			' MOUSE PULSADO
				Select (EventSource ())
					Case MainCanvas
						If (Dragging = 0)
							FromWhereX = EventX ()
							FromWhereY = EventY ()
							Select (EventData ())
								Case 3	Dragging = 1
								Case 1
									If (ShiftStatus)
										J = 0
										TX = 500000.0
										For I = 0 To (V.NBones - 1)
											TY = Sqr (..
												((BonePositions[I].X - FromWhereX) ^ 2.0) + ..
												((BonePositions[I].Y - FromWhereY) ^ 2.0))
											If (TY < TX)
												J = I
												TX = TY
											EndIf
										Next
										SelectBone (J)
									Else
										Dragging = 4
									EndIf
								Case 2	Dragging = 3
							End Select
						EndIf
					Case PreviewCanvas
						If (Dragging = 0)
							FromWhereX = EventX ()
							FromWhereY = EventY ()
							If (EventData () = 3)
								Dragging = 2
							EndIf
						EndIf
				End Select
			Case EVENT_MOUSEMOVE
			' MOUSE MOVIDO
				If (Dragging > 0)
					Select (Dragging)
						Case 1
							MainX:+ (EventX () - FromWhereX)
							MainY:+ (EventY () - FromWhereY)
							RedrawMain = True
						Case 2
							PreviewX:+ (EventX () - FromWhereX)
							PreviewY:+ (EventY () - FromWhereY)
							RedrawPreview = True
						Case 3
							If (BoneSelected > -1)
								ActFrameF.Bones[Bones[BoneSelected]].Angle:+ (EventX () - FromWhereX) / MainZoom
								ActFrameF.Bones[Bones[BoneSelected]].Angle = ActFrameF.Bones[Bones[BoneSelected]].Angle Mod 360.0
								If (ActFrameF.Bones[Bones[BoneSelected]].Angle < 0.0) Then ActFrameF.Bones[Bones[BoneSelected]].Angle:+ 360.0
								UpdateBoneInfo ()
								RedrawMain = True
								RedrawPreview = True
								IsSaved = False
							EndIf
						Case 4
							If (BoneSelected > -1)
								TX = (EventX () - FromWhereX) / MainZoom
								TY = (EventY () - FromWhereY) / MainZoom
								TA = 0.0
								VB = V.SearchBoneFather (Bones[BoneSelected])
								While (VB)
									TA:+ ActFrameF.Bones[VB.ID].Angle
									VB = V.SearchBoneFather (VB.ID)
								Wend
								TA = -TA Mod 360.0
								If (TA < 0.0) Then TA:+ 360.0
								ActFrameF.Bones[Bones[BoneSelected]].X:+ (TX * VCos[Int (TA * 10.0)]) - (TY * VSin[Int (TA * 10.0)])
								ActFrameF.Bones[Bones[BoneSelected]].Y:+ (TY * VCos[Int (TA * 10.0)]) + (TX * VSin[Int (TA * 10.0)])
								UpdateBoneInfo ()
								RedrawMain = True
								RedrawPreview = True
								IsSaved = False
							EndIf
					End Select
					FromWhereX = EventX ()
					FromWhereY = EventY ()
				Else
					If (EventSource () = MainCanvas)
						X = ToScrollX (EventX ())
						Y = ToScrollY (EventY ())
						SetStatusText (MainWindow, "X = " + X + "  Y = " + Y)
						Help = False
						TimeToHelp = MilliSecs () + 10000
					EndIf
				EndIf
			Case EVENT_MOUSEUP
			' MOUSE LEVANTADO
				Select (EventSource ())
					Case MainCanvas
						Dragging = 0
					Case PreviewCanvas
						Dragging = 0
				End Select
			Case EVENT_KEYDOWN
				If ((EventData () = KEY_LSHIFT) Or (EventData () = KEY_RSHIFT))
					ShiftStatus = True
				EndIf
			Case EVENT_KEYUP
				If ((EventData () = KEY_LSHIFT) Or (EventData () = KEY_RSHIFT))
					ShiftStatus = False
				EndIf
			Case EVENT_MOUSEWHEEL
			' MOUSE WHEELEADO
				Select (EventSource ())
					Case MainCanvas
						If (Dragging = 0)
							If (EventData () < 0)
								MainZoomC:- 0.5
								If (MainZoomC < -4.5) Then MainZoomC = -4.5
							Else
								MainZoomC:+ 0.5
								If (MainZoomC > 4.5) Then MainZoomC = 4.5
							EndIf
							If (MainZoomC < 0.0)
								MainZoom = 1.0 / Abs (MainZoomC - 1.0)
							Else
								MainZoom = MainZoomC + 1.0
							EndIf
							RedrawMain = True
						EndIf
					Case PreviewCanvas
						If (Dragging = 0)
							I = SliderValue (PreviewZoomS)
							If (EventData () < 0)
								If (I > -9)
									SetSliderValue (PreviewZoomS, I - 1)
									UpdatePreviewSliders ()
								EndIf
							Else
								If (I < 9)
									SetSliderValue (PreviewZoomS, I + 1)
									UpdatePreviewSliders ()
								EndIf
							EndIf
							RedrawPreview = True
						EndIf
				End Select
			Case EVENT_GADGETACTION
			' BOTONES Y DEMAS
				Select (EventSource ())
					Case AboutOK
						EnableGadget (MainWindow)
						EnableGadget (PreviewWindow)
						EnableGadget (AnimsWindow)
						EnableGadget (BonesWindow)
						HideGadget (TheAbout)
						ActivateGadget (MainWindow)
					Case PNGInfoN
						If (TextAreaLen (PNGInfoN) > 3)
							SetTextAreaText (PNGInfoN, Left (TextAreaText (PNGInfoN), 3))
						EndIf
						If (TextAreaLen (PNGInfoN) > 0)
							SetTextAreaText (PNGInfoN, NaNPure (TextAreaText (PNGInfoN)))
						EndIf
					Case PNGInfoSX
						If (TextAreaLen (PNGInfoSX) > 8) 
							SetTextAreaText (PNGInfoSX, Left (TextAreaText (PNGInfoSX), 3))
						EndIf
						If (TextAreaLen (PNGInfoSX) > 0)
							Print (TextAreaText (PNGInfoSX) + " a2 " + AntiNan (TextAreaText (PNGInfoSX)))
							SetTextAreaText (PNGInfoSX, AntiNan (TextAreaText (PNGInfoSX)))
						EndIf
					Case PNGInfoSY
						If (TextAreaLen (PNGInfoSY) > 8)
							SetTextAreaText (PNGInfoSY, Left (TextAreaText (PNGInfoSY), 3))
						EndIf
						If (TextAreaLen (PNGInfoSY) > 0)
							SetTextAreaText (PNGInfoSY, AntiNan (TextAreaText (PNGInfoSY)))
						EndIf
					Case PNGInfoOK
						S = RequestFile ("Choose PNG prefix and destination", "", True, CurrentDir () + "/")
						If (S)
							ExportActAnim (S, Float (TextAreaText (PNGInfoSX)), ..
											Float (TextAreaText (PNGInfoSY)), ..
											ButtonState (PNGInfoPower2), ..
											ButtonState (PNGInfoSquare), ..
											ButtonState (PNGInfoCentered))
						EndIf
						EnableGadget (MainWindow)
						EnableGadget (PreviewWindow)
						EnableGadget (AnimsWindow)
						EnableGadget (BonesWindow)
						HideGadget (PNGInfo)
						ActivateGadget (MainWindow)
					Case PNGInfoCancel
						EnableGadget (MainWindow)
						EnableGadget (PreviewWindow)
						EnableGadget (AnimsWindow)
						EnableGadget (BonesWindow)
						HideGadget (PNGInfo)
						ActivateGadget (MainWindow)
					Case PreviewZoomS
						UpdatePreviewSliders ()
						RedrawPreview = True
					Case PreviewTimeS
						UpdatePreviewSliders ()
						RedrawPreview = True
					Case PreviewPlay
						If (IsPlaying)
							IsPlaying = False
							A.ChangeAnim (ActAnim, ActTime, 0)
							A.FrameTime2 = 0
							SetGadgetText (PreviewPlay, "Play")
						Else
							IsPlaying = True
							A.ChangeAnim (ActAnim, ActTime, 0)
							SetGadgetText (PreviewPlay, "Stop")
						EndIf
						RedrawPreview = True
					Case BoneGraphX
						If (BoneSelected > -1)
							If (ActFrameF.Bones[Bones[BoneSelected]].Graph = 0)
								VB = V.SearchBone (Bones[BoneSelected])
								ActFrameF.Bones[Bones[BoneSelected]].Graph = VB.Range1
							Else
								ActFrameF.Bones[Bones[BoneSelected]].Graph = 0
							EndIf
							UpdateBoneInfo ()
							RedrawMain = True
							RedrawPreview = True
							RedrawBone = True
							IsSaved = False
						EndIf
					Case BoneRange1
						If (TextAreaLen (BoneRange1) > 3)
							SetTextAreaText (BoneRange1 , Left (TextAreaText (BoneRange1) , 3) )
						EndIf
						If (TextAreaLen (BoneRange1) > 0)
							SetTextAreaText (BoneRange1, (NaNPure (TextAreaText (BoneRange1))))
						EndIf
						If (BoneSelected > -1)
							VB = V.SearchBone (Bones[BoneSelected])
							VB.Range1 = Int (TextAreaText (BoneRange1))
							If ((ActFrameF.Bones[Bones[BoneSelected]].Graph < VB.Range1) And ..
								ActFrameF.Bones[Bones[BoneSelected]].Graph <> 0)
								ActFrameF.Bones[Bones[BoneSelected]].Graph = VB.Range1
								RedrawMain = True
								RedrawPreview = True
								RedrawBone = True
								IsSaved = False
							EndIf
						EndIf
					Case BoneRange2
						If (TextAreaLen (BoneRange2) > 3)
							SetTextAreaText (BoneRange2, Left (TextAreaText (BoneRange2), 3))
						EndIf
						If (TextAreaLen (BoneRange2) > 0)
							SetTextAreaText (BoneRange2, NaNPure (TextAreaText (BoneRange2)))
						EndIf
						If (BoneSelected > -1)
							VB = V.SearchBone (Bones[BoneSelected])
							VB.Range2 = Int (TextAreaText (BoneRange2))
							If ((ActFrameF.Bones[Bones[BoneSelected]].Graph > VB.Range2))
								ActFrameF.Bones[Bones[BoneSelected]].Graph = VB.Range2
								RedrawMain = True
								RedrawPreview = True
								RedrawBone = True
								IsSaved = False
							EndIf
						EndIf
					Case BoneGraphS
						VB = V.SearchBone (Bones[BoneSelected])
						If (SliderValue (BoneGraphS) = 1)
							ActFrameF.Bones[Bones[BoneSelected]].Graph:- 1
							If (ActFrameF.Bones[Bones[BoneSelected]].Graph < VB.Range1)
								ActFrameF.Bones[Bones[BoneSelected]].Graph = VB.Range1
							EndIf
							If (ActFrameF.Bones[Bones[BoneSelected]].Graph > VB.Range2)
								ActFrameF.Bones[Bones[BoneSelected]].Graph = VB.Range2
							EndIf
						ElseIf (SliderValue (BoneGraphS) = 3)
							ActFrameF.Bones[Bones[BoneSelected]].Graph:+ 1
							If (ActFrameF.Bones[Bones[BoneSelected]].Graph < VB.Range1)
								ActFrameF.Bones[Bones[BoneSelected]].Graph = VB.Range1
							EndIf
							If (ActFrameF.Bones[Bones[BoneSelected]].Graph > VB.Range2)
								ActFrameF.Bones[Bones[BoneSelected]].Graph = VB.Range2
							EndIf
						EndIf
						RedrawMain = True
						RedrawPreview = True
						RedrawBone = True
						UpdateBoneInfo ()
						SetSliderValue (BoneGraphS, 2)
						IsSaved = False
					Case BoneZChange
						If (SliderValue (BoneZChange) = 3)
							MoveBoneUp ()
						ElseIf (SliderValue (BoneZChange) = 1)
							MoveBoneDown ()
						EndIf
						RedrawMain = True
						RedrawPreview = True
						SetSliderValue (BoneZChange, 2)
					Case BoneNew
						If (V.NBones < 64)
							S = RequestString ("Enter bone name", 15)
							If (Not Cancel)
								V.NBones:+ 1
								If (BoneSelected = -1)
									VB = V.Father
									V.Father = New VBABone
									V.Father.Range1 = VB.Range1
									V.Father.Range2 = VB.Range2
									V.Father.ID = V.NBones - 1
									V.Father.Name = S
									V.Father.NSons = 1
									V.Father.Sons = V.Father.Sons [..1]
									V.Father.Sons[0] = VB
									Bones[V.NBones - 1] = V.NBones - 1
								Else
									VB = V.SearchBone (Bones[BoneSelected])
									VB.NSons:+ 1
									VB.Sons = VB.Sons [..VB.NSons]
									VB.Sons[VB.NSons - 1] = New VBABone
									VB.Sons[VB.NSons - 1].ID = V.NBones - 1
									VB.Sons[VB.NSons - 1].Name = S
									VB.Sons[VB.NSons - 1].Range1 = VB.Range1
									VB.Sons[VB.NSons - 1].Range2 = VB.Range2
									Bones[V.NBones - 1] = V.NBones - 1
								EndIf
								For I = 0 To (V.NAnims - 1)
									Frame = V.Anim[I].Frames
									While (Frame)
										Frame.Bones = Frame.Bones[..V.NBones]
										Frame.Bones[V.NBones - 1] = New VBABoneState
										Frame.Bones[V.NBones - 1].Z = V.NBones - 1
										Frame = Frame.NextFrame
									Wend
								Next
								UpdateBoneWindow ()
								SelectGadgetItem (BoneList, V.NBones - 1)
								SelectBone (V.NBones - 1)
								RedrawMain = True
								RedrawPreview = True
								RedrawBone = True
								Clipboard = Null
								DisableMenu (Paste)
								UpdateWindowMenu (MainWindow)
								IsSaved = False
							EndIf
						EndIf
					Case BoneDelete
						If ((V.NBones > 0) And (BoneSelected > -1))
							If (V.NBones > 1)
								VB = V.SearchBone (Bones[BoneSelected])
								If (Confirm ("Delete bone " + VB.Name + "?"))
									Select (V.DeleteBone (Bones[BoneSelected]))
										Case 2
											Notify ("Selected bone has more than one son~nDelete sons first")
										Case 1
											Notify ("Bone not found? Unexpected error", True)
										Default
											For I = BoneSelected To (V.NBones - 1)
												Bones[I] = Bones[I + 1]
											Next
											BoneSelected:- 1
											UpdateBoneWindow ()
									End Select
									RedrawMain = True
									RedrawPreview = True
									RedrawBone = True
									Clipboard = Null
									DisableMenu (Paste)
									UpdateWindowMenu (MainWindow)
									IsSaved = False
								EndIf
							Else
								Notify ("At least one base bone is required")
							EndIf
						EndIf
					Case BoneChangeVGP
						S = RequestFile ("UAPEdit - UGP to load?", "Unif Graphics Pack UGP (*.png):png;VoiD Graphics Pack VGP (*.vgp):vgp", False, CurrentDir () + "/")
						If (S)
							If (FileUGP) Then FileUGP.Unload()
							If (FileVGP) Then FileVGP.Unload()
							FileUGP = Null
							FileVGP = Null
							If (Lower(Right(Trim(S), 3)) = "png")
								FileUGP = UGP.LoadUGP(S, FILTEREDIMAGE)
							Else
								FileVGP = VGP.LoadVGP (S)
							EndIf
'							File.ChangeRanges (255, 255, 255, 120, 240, 200, 255, 0, 0)
							UGPVGPName = StripDir (S)
							RedrawMain = True
							RedrawPreview = True
							RedrawBone = True
							If (FileUGP = Null And FileVGP = Null)
								Notify("Bad format reading UGP/VGP")
								FileVGP = VGP.LoadVGP ("defbones.vgp")
								FileUGP = Null
								UGPVGPName = "defbones.vgp"
							EndIf
						EndIf
					Case BoneChangeSize
						S = RequestString ("Enter new Bone Size", 10, True)
						If (S)
							If (Float (S) >= 0.0)
								ActFrameF.Bones[Bones[BoneSelected]].Size = Float (S)
								RedrawMain = True
								RedrawPreview = True
								RedrawBone = True
								UpdateBoneWindow ()
							EndIf
						EndIf
					Case AnimsAnimS
						If (SliderValue (AnimsAnimS) = 3)
							SigAnim ()
						ElseIf (SliderValue (AnimsAnimS) = 1)
							AntAnim ()
						EndIf
						SetSliderValue (AnimsAnimS, 2)
					Case AnimsFrameS
						If (SliderValue (AnimsFrameS) = 3)
							SigFrame ()
						ElseIf (SliderValue (AnimsFrameS) = 1)
							AntFrame ()
						EndIf
						SetSliderValue (AnimsFrameS, 2)
					Case AnimsGotoAnimT
						If (TextAreaLen (AnimsGotoAnimT) > 3)
							SetTextAreaText (AnimsGotoAnimT, Left (TextAreaText (AnimsGotoAnimT), 3))
						EndIf
						If (TextAreaLen (AnimsGotoAnimT) > 0)
							SetTextAreaText (AnimsGotoAnimT, NaNPure (TextAreaText (AnimsGotoAnimT)))
						EndIf
					Case AnimsGotoFrameT
						If (TextAreaLen (AnimsGotoFrameT) > 3)
							SetTextAreaText (AnimsGotoFrameT, Left (TextAreaText (AnimsGotoFrameT), 3))
						EndIf
						If (TextAreaLen (AnimsGotoFrameT) > 0)
							SetTextAreaText (AnimsGotoFrameT, NaNPure (TextAreaText (AnimsGotoFrameT)))
						EndIf
					Case AnimsGotoAnim
						I = Int (TextAreaText (AnimsGotoAnimT))
						If (I > -1 And I < V.NAnims)
							PutAnim (I)
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							SelectBone (I)
							RedrawMain = True
							RedrawPreview = True
							RedrawBone = True
							UpdateAnimsWindow ()
						EndIf
					Case AnimsGotoFrame
						I = Int (TextAreaText (AnimsGotoFrameT))
							If (I > -1 And I < V.Anim[ActAnim].NFrames)
								ActFrame = I
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							ActFrameF = V.Anim[ActAnim].SearchFrame (I)
							SelectBone (I)
							RedrawMain = True
							RedrawPreview = True
							RedrawBone = True
							UpdateAnimsWindow ()
						EndIf
					Case AnimsAddAnim
						If (BoneSelected > -1)
							I = Bones[BoneSelected]
						Else
							I = -1
						EndIf
						V.Anim = V.Anim[..V.NAnims + 1]
						V.Anim[V.NAnims] = New VBAAnim
						V.Anim[V.NAnims].NFrames = 1
						V.Anim[V.NAnims].Frames = V.Anim[0].Frames.Copy ()
						V.NAnims:+ 1
						PutAnim (V.NAnims - 1)
						SelectBone (I)
						UpdateAnimsWindow ()
						RedrawBone = True
						RedrawMain = True
						IsSaved = False
					Case AnimsAddDupeAnim
						V.Anim = V.Anim[..V.NAnims + 1]
						V.Anim[V.NAnims] = New VBAAnim
						V.Anim[V.NAnims].NFrames = V.Anim[ActAnim].NFrames
						V.Anim[V.NAnims].Frames = V.Anim[ActAnim].Frames.Copy ()
						F1P = V.Anim[V.NAnims].Frames
						F2P = V.Anim[ActAnim].Frames
						For I = 1 To (V.Anim[V.NAnims].NFrames - 1)
							F1P.NextFrame = F2P.NextFrame.Copy ()
							F1P = F1P.NextFrame
							F2P = F2P.NextFrame
						Next
						V.NAnims:+ 1
						If (BoneSelected > -1)
							I = Bones[BoneSelected]
						Else
							I = -1
						EndIf
						PutAnim (V.NAnims - 1)
						SelectBone (I)
						UpdateAnimsWindow ()
						RedrawBone = True
						RedrawMain = True
						IsSaved = False
					Case AnimsInsAnim
						If (ActAnim > 0)
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							V.InsertAnim (ActAnim)
							V.Anim[ActAnim].Frames = V.Anim[0].Frames.Copy ()
							V.Anim[ActAnim].NFrames = 1
							PutAnim (ActAnim)
							SelectBone (I)
							UpdateAnimsWindow ()
							RedrawBone = True
							RedrawMain = True
							IsSaved = False
						Else
							Notify ("No anim may be inserted before Anim 0 (Base Anim)")
						EndIf
					Case AnimsInsDupeAnim
						If (ActAnim > 0)
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							V.InsertAnim (ActAnim)
							V.Anim[ActAnim].NFrames = V.Anim[ActAnim + 1].NFrames
							V.Anim[ActAnim].Frames = V.Anim[ActAnim + 1].Frames.Copy ()
							F1P = V.Anim[ActAnim].Frames
							F2P = V.Anim[ActAnim + 1].Frames
							For I = 1 To (V.Anim[ActAnim].NFrames - 1)
								F1P.NextFrame = F2P.NextFrame.Copy ()
								F1P = F1P.NextFrame
								F2P = F2P.NextFrame
							Next
							V.Anim[ActAnim].Frames = V.Anim[0].Frames.Copy ()
							PutAnim (ActAnim)
							SelectBone (I)
							UpdateAnimsWindow ()
							RedrawBone = True
							RedrawMain = True
							IsSaved = False
						Else
							Notify ("No anim may be inserted before Anim 0 (Base Anim)")
						EndIf
					Case AnimsClrAnim
						If (ActAnim > 0)
							If (Confirm ("Clear Anim " + ActAnim + "? All frames will be deleted and anim will be restored to a Base Anim value"))
								If (BoneSelected > -1)
									I = Bones[BoneSelected]
								Else
									I = -1
								EndIf
								V.Anim[ActAnim].ClearFrames ()
								V.Anim[ActAnim].Frames = V.Anim[0].Frames.Copy ()
								V.Anim[ActAnim].NFrames = 1
								PutAnim (ActAnim)
								SelectBone (I)
								UpdateAnimsWindow ()
								RedrawBone = True
								RedrawMain = True
								IsSaved = False
							EndIf
						Else
							Notify ("Anim 0 (Base Anim) cannot be cleared")
						EndIf
					Case AnimsDelAnim
						If (ActAnim > 0)
							If (Confirm ("Delete Anim " + ActAnim + "? All its frames will be deleted, and Anim IDs will be shifted left"))
								If (BoneSelected > -1)
									I = Bones[BoneSelected]
								Else
									I = -1
								EndIf
								V.DeleteAnim (ActAnim)
								If (ActAnim = V.NAnims)
									' last anim was deleted
									PutAnim (ActAnim - 1)
								Else
									PutAnim (ActAnim)
								EndIf
								SelectBone (I)
								UpdateAnimsWindow ()
								RedrawBone = True
								RedrawMain = True
								IsSaved = False
							EndIf
						Else
							Notify ("Anim 0 (Base Anim) cannot be deleted")
						EndIf
					Case AnimsAddFrame
						If (ActAnim > 0)
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							Frame = V.Anim[ActAnim].SearchFrame (V.Anim[ActAnim].NFrames - 1)
							Frame.NextFrame = V.Anim[0].Frames.Copy ()
							V.Anim[ActAnim].NFrames:+ 1
							SelectBone (I)
							PutAnim (ActAnim)
							ActFrame = V.Anim[ActAnim].NFrames - 1
							ActFrameF = V.Anim[ActAnim].SearchFrame (ActFrame)
							UpdateAnimsWindow ()
							RedrawBone = True
							RedrawMain = True
							IsSaved = False
						Else
							Notify ("Anim 0 (Base Anim) must have only one frame")
						EndIf
					Case AnimsAddDupeFrame
						If (ActAnim > 0)
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							Frame = V.Anim[ActAnim].SearchFrame (V.Anim[ActAnim].NFrames - 1)
							Frame.NextFrame = ActFrameF.Copy ()
							V.Anim[ActAnim].NFrames:+ 1
							SelectBone (I)
							PutAnim (ActAnim)
							ActFrame = V.Anim[ActAnim].NFrames - 1
							ActFrameF = V.Anim[ActAnim].SearchFrame (ActFrame)
							UpdateAnimsWindow ()
							RedrawBone = True
							RedrawMain = True
							IsSaved = False
						Else
							Notify ("Anim 0 (Base Anim) must have only one frame")
						EndIf
					Case AnimsInsFrame
						If (ActAnim > 0)
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							If (ActFrame = 0)
								Frame = V.Anim[ActAnim].Frames
								V.Anim[ActAnim].Frames = V.Anim[0].Frames.Copy ()
								V.Anim[ActAnim].Frames.NextFrame = Frame
							Else
								Frame = V.Anim[ActAnim].SearchFrame (ActFrame - 1)
								Frame.NextFrame = V.Anim[0].Frames.Copy ()
								Frame.NextFrame.NextFrame = ActFrameF
							EndIf
							V.Anim[ActAnim].NFrames:+ 1
							SelectBone (I)
							UpdateAnimsWindow ()
							I = ActFrame
							PutAnim (ActAnim)
							ActFrame = I
							ActFrameF = V.Anim[ActAnim].SearchFrame (ActFrame)
							RedrawBone = True
							RedrawMain = True
							IsSaved = False
						Else
							Notify ("Anim 0 (Base Anim) must have only one frame")
						EndIf
					Case AnimsInsDupeFrame
						If (ActAnim > 0)
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							If (ActFrame = 0)
								Frame = V.Anim[ActAnim].Frames
								V.Anim[ActAnim].Frames = ActFrameF.Copy ()
								V.Anim[ActAnim].Frames.NextFrame = Frame
							Else
								Frame = V.Anim[ActAnim].SearchFrame (ActFrame - 1)
								Frame.NextFrame = ActFrameF.Copy ()
								Frame.NextFrame.NextFrame = ActFrameF
							EndIf
							V.Anim[ActAnim].NFrames:+ 1
							SelectBone (I)
							UpdateAnimsWindow ()
							I = ActFrame
							PutAnim (ActAnim)
							ActFrame = I
							ActFrameF = V.Anim[ActAnim].SearchFrame (ActFrame)
							RedrawBone = True
							RedrawMain = True
							IsSaved = False
						Else
							Notify ("Anim 0 (Base Anim) must have only one frame")
						EndIf
					Case AnimsClrFrame
						If (ActAnim > 0)
							If (Confirm ("Clear Frame " + ActFrame + "? It will be restored to a Base Anim value"))
								If (BoneSelected > -1)
									I = Bones[BoneSelected]
								Else
									I = -1
								EndIf
								If (ActFrame = 0)
									V.Anim[ActAnim].Frames.Bones = V.Anim[ActAnim].Frames.Bones [..0]
									V.Anim[ActAnim].Frames = V.Anim[0].Frames.Copy ()
								Else
									J = 0
									Frame = V.Anim[ActAnim].Frames
									While (J < (ActFrame - 1))
										Frame = Frame.NextFrame
										J:+ 1
									Wend
									If (V.NBones > 0)
										Frame.NextFrame.Bones = Frame.NextFrame.Bones[..0]
									EndIf
									Frame.NextFrame =  V.Anim[0].Frames.Copy ()
								EndIf
								SelectBone (I)
								UpdateAnimsWindow ()
								I = ActFrame
								Frame = ActFrameF
								PutAnim (ActAnim)
								ActFrame = I
								ActFrameF = Frame	
								RedrawBone = True
								RedrawMain = True
								IsSaved = False
							EndIf
						Else
							Notify ("Anim 0 (Base Anim) frame cannot be cleared")
						EndIf
					Case AnimsDelFrame
						If (V.Anim[ActAnim].NFrames > 1)
							If (Confirm ("Delete Frame " + ActFrame + "? Next frames will be shifted left"))
								If (BoneSelected > -1)
									I = Bones[BoneSelected]
								Else
									I = -1
								EndIf
								V.Anim[ActAnim].DelFrame (ActFrame)
								If (ActFrame = V.Anim[ActAnim].NFrames)
									' last frame was deleted
									ActFrame:- 1
								EndIf
								ActFrameF = V.Anim[ActAnim].SearchFrame (ActFrame)
								SelectBone (I)
								UpdateAnimsWindow ()
								I = ActFrame
								Frame = ActFrameF
								PutAnim (ActAnim)
								ActFrame = I
								ActFrameF = Frame
								RedrawBone = True
								RedrawMain = True
								IsSaved = False
							EndIf
						Else
							Notify ("Every anim must have at least one frame")
						EndIf
					Case AnimsFrameDurT
						If (TextAreaLen (AnimsFrameDurT) > 5)
							SetTextAreaText (AnimsFrameDurT, Left (TextAreaText (AnimsFrameDurT), 5))
						EndIf
						If (TextAreaLen (AnimsFrameDurT) > 0)
							SetTextAreaText (AnimsFrameDurT, NaNPure (TextAreaText (AnimsFrameDurT)))
						EndIf
					Case AnimsFrameDurB
						If (TextAreaLen (AnimsFrameDurT) > 0)
							ActFrameF.Duration = TextAreaText (AnimsFrameDurT).ToInt ()
						Else
							SetTextAreaText (AnimsFrameDurT, 0)
							ActFrameF.Duration = 0
						EndIf
						IsSaved = False
						UpdateAnimsWindow ()
					Case AnimsExchangeT
						If (TextAreaLen (AnimsExchangeT) > 3)
							SetTextAreaText (AnimsExchangeT, Left (TextAreaText (AnimsExchangeT), 3))
						EndIf
						If (TextAreaLen (AnimsFrameDurT) > 0)
							SetTextAreaText (AnimsExchangeT, NaNPure (TextAreaText (AnimsExchangeT)))
						EndIf
					Case AnimsExchangeAnim
						If (ActAnim > 0)
							I = Int (TextAreaText (AnimsExchangeT))
							If (I <> 0)
								If (I > 0 And I < V.NAnims)
									If (I <> ActAnim)
										Ani = V.Anim[ActAnim]
										V.Anim[ActAnim] = V.Anim[I]
										V.Anim[I] = Ani
										ActAnim = I
										A.CurrentAnim = I
										UpdateAnimsWindow ()
										IsSaved = False
									EndIf
								Else
									Notify ("Selected anim is out of bounds")
								EndIf
							Else
								Notify ("Base anim cannot be exchanged with any other one")
							EndIf
						Else
							Notify ("Base anim cannot be exchanged with any other one")
						EndIf
					Case AnimsExchangeFrame
						If (ActAnim > 0)
							I = Int (TextAreaText (AnimsExchangeT))
							If (I > -1 And I < V.Anim[ActAnim].NFrames)
								If (I <> ActFrame)
									If (Abs (ActFrame - I) > 1)
										' they're correlative - uf
										If (ActFrame < I)
											If (ActFrame = 0)
												Frame = ActFrameF.NextFrame
												F2N = Frame.NextFrame
												V.Anim[ActAnim].Frames = Frame
												Frame.NextFrame = ActFrameF
												ActFrameF.NextFrame = F2N
											Else
												F1P = V.Anim[ActAnim].SearchFrame (ActFrame - 1)
												Frame = ActFrameF.NextFrame
												F2N = Frame.NextFrame
												F1P.NextFrame = Frame
												Frame.NextFrame = ActFrameF
												ActFrameF.NextFrame = F2N
											EndIf
										Else
											If (I = 0)
												Frame = V.Anim[ActAnim].Frames.NextFrame
												F1N = ActFrameF.NextFrame
												V.Anim[ActAnim].Frames = ActFrameF
												ActFrameF.NextFrame = Frame
												Frame.NextFrame = F1N
											Else
												F2P = V.Anim[ActAnim].SearchFrame (I - 1)
												Frame = F2P.NextFrame
												F1N = ActFrameF.NextFrame
												F2P.NextFrame = ActFrameF
												ActFrameF.NextFrame = Frame
												Frame.NextFrame = F1N
											EndIf
										EndIf
									Else
										If (ActFrame = 0)
											F1N = ActFrameF.NextFrame
											F2P = V.Anim[ActAnim].SearchFrame (I - 1)
											Frame = F2P.NextFrame
											F2N = Frame.NextFrame
											V.Anim[ActAnim].Frames = Frame
											F2P.NextFrame = ActFrameF
											Frame.NextFrame = F1N
											ActFrameF.NextFrame = F2N
										Else
											If (I = 0)
												F1P = V.Anim[ActAnim].SearchFrame (ActFrame - 1)
												F1N = ActFrameF.NextFrame
												Frame = V.Anim[ActAnim].Frames
												F2N = Frame.NextFrame
												F1P.NextFrame = Frame
												V.Anim[ActAnim].Frames = ActFrameF
												Frame.NextFrame = F1N
												ActFrameF.NextFrame = F2N
											Else
												F1P = V.Anim[ActAnim].SearchFrame (ActFrame - 1)
												F1N = ActFrameF.NextFrame
												F2P = V.Anim[ActAnim].SearchFrame (I - 1)
												Frame = F2P.NextFrame
												F2N = Frame.NextFrame
												F1P.NextFrame = Frame
												F2P.NextFrame = ActFrameF
												Frame.NextFrame = F1N
												ActFrameF.NextFrame = F2N
											EndIf
										EndIf
									EndIf
									UpdateAnimsWindow ()
									Frame = ActFrameF
									PutAnim (ActAnim)
									ActFrameF = Frame
									ActFrame = I
									RedrawPreview = True
									IsSaved = False
								EndIf
							Else
								Notify ("Selected frame is out of bounds")
							EndIf
						Else
							Notify ("In base anim the only frame cannot be exchanged")
						EndIf
				End Select
			' CLICK EN MENU
			Case EVENT_MENUACTION
				Select (EventData ())
					Case FILE_NUEVO
						If (IsSaved Or Confirm ("Start new Anim Pack? Changes will be lost")) Then FileNew ()
					Case FILE_ABRIR
						If (IsSaved Or Confirm ("Load Anim Pack? Changes will be lost"))
							S = RequestFile ("UAPEdit - Select Anim Pack to open", "Unif Anim Pack (*.uap):uap;VoiD Bone Animation (*.vba):vba", False, CurrentDir () + "/")
							If (S) Then FileLoad (S)
						EndIf
					Case FILE_GUARDAR
						If (SaveFile <> "untitled.uap")
							V.SaveVBA (SavePath)
							IsSaved = True
						Else
							S = RequestFile ("UAPEdit - Select Anim Pack to save as", "Unif Anim Pack (*.uap):uap;VoiD Bone Animation (*.vba):vba", True, CurrentDir () + "/")
							If (S)
								If (Len (StripExt (S)) = Len (S)) Then S = S + ".uap"
								If (Lower(Right(Trim(S), 3)) = "vba")
									V.SaveVBA (S)
								Else
									V.SaveUAP (S)
								EndIf								
								SaveFile = StripDir (S)
								SavePath = S
								IsSaved = True
							EndIf
						EndIf
					Case FILE_GUARDARCOMO
						S = RequestFile ("UAPEdit - Select VBA to save as", "Unif Anim Pack (*.uap):uap;VoiD Bone Animation (*.vba):vba", True, CurrentDir () + "/")
						If (S)
							If (Len (StripExt (S)) = Len (S)) Then S = S + ".uap"
							If (Lower(Right(Trim(S), 3)) = "vba")
								V.SaveVBA (S)
							Else
								V.SaveUAP (S)
							EndIf
							SaveFile = StripDir (S)
							SavePath = S
							IsSaved = True
						EndIf
					Case FILE_EXPORTAR
						DisableGadget (MainWindow)
						DisableGadget (BonesWindow)
						DisableGadget (AnimsWindow)
						DisableGadget (PreviewWindow)
						UpdateProgBar (PNGProgress, 0.0)
						ShowGadget (PNGInfo)
					Case FILE_SALIR
						Evento = CreateEvent (EVENT_WINDOWCLOSE)
						Evento.Source = MainWindow
						PostEvent (Evento)
					Case EDIT_COPIAR 'postit
						Clipboard = ActFrameF.Copy ()
						EnableMenu (Paste)
						UpdateWindowMenu (MainWindow)
					Case EDIT_PEGAR ' postit
						If (Clipboard)
							If (BoneSelected > -1)
								I = Bones[BoneSelected]
							Else
								I = -1
							EndIf
							Frame = ActFrameF.NextFrame
							ActFrameF = Clipboard.Copy ()
							If (ActFrame = 0)
								V.Anim[ActAnim].Frames = ActFrameF
							Else
								V.Anim[ActAnim].SearchFrame (ActFrame - 1).NextFrame = ActFrameF
							EndIf
							ActFrameF.NextFrame = Frame
							SelectBone (I)
							UpdateAnimsWindow ()
							RedrawMain = True
							RedrawBone = True
							IsSaved = False
						EndIf
					Case EDIT_ANT
						If (BoneSelected > 0)
							BoneSelected:- 1
							SelectGadgetItem (BoneList, BoneSelected)
							RedrawBone = True
						EndIf
					Case EDIT_SIG
						If (BoneSelected < (V.NBones - 1))
							BoneSelected:+ 1
							SelectGadgetItem (BoneList, BoneSelected)
							RedrawBone = True
						EndIf
					Case EDIT_ANTA
						AntAnim ()
					Case EDIT_SIGA
						SigAnim ()
					Case EDIT_ANTF
						AntFrame ()
					Case EDIT_SIGF
						SigFrame ()
					Case VIEW_PREVIEW
						If (PreviewVisible)
							HideGadget (PreviewWindow)
							PreviewVisible = False
							SetGadgetText (ViewPreview, "x Preview Window")
						Else
							ShowGadget (PreviewWindow)
							PreviewVisible = True
							SetGadgetText (ViewPreview, "v Preview Window")
							RedrawPreview = True
						EndIf
						UpdateWindowMenu (MainWindow)
					Case VIEW_BONEW
						If (BonewVisible)
							HideGadget (BonesWindow)
							BonewVisible = False
							SetGadgetText (ViewBoneW, "x Bones Window")
						Else
							ShowGadget (BonesWindow)
							BonewVisible = True
							SetGadgetText (ViewBoneW, "v Bones Window")
							RedrawBone = True
						EndIf
						UpdateWindowMenu (MainWindow)
					Case VIEW_ANIMSW
						If (AnimswVisible)
							HideGadget (AnimsWindow)
							AnimswVisible = False
							SetGadgetText (ViewAnimsW, "x Anims Window")
						Else
							ShowGadget (AnimsWindow)
							AnimswVisible = True
							SetGadgetText (ViewAnimsW, "v Anims Window")
						EndIf
						UpdateWindowMenu (MainWindow)
					Case VIEW_GRID
						If (GridVisible)
							GridVisible = False
							SetGadgetText (ViewGrid, "x Grid")
						Else
							GridVisible = True
							SetGadgetText (ViewGrid, "v Grid")
						EndIf
						RedrawMain = True
						RedrawPreview = True
						UpdateWindowMenu (MainWindow)
					Case VIEW_SKEL
						If (SkelVisible)
							SkelVisible = False
							SetGadgetText (ViewSkel, "x Skeleton")
						Else
							SkelVisible = True
							SetGadgetText (ViewSkel, "v Skeleton")
						EndIf
						RedrawMain = True
						UpdateWindowMenu (MainWindow)
					Case VIEW_RESTORE
						SetGadgetShape (MainWindow, 50, 50, 820, 655)
					Case SET_HEIGHT
						S = RequestString ("New VBA height? (actual : " + V.Height + ")", 10, True)
						If (Not Cancel)
							If (Len (S) = 0) Then S = "0"
							V.Height = Float (S)
							SetGadgetText (SetHeight, "Height [" + V.Height + "]")
							UpdateWindowMenu (MainWindow)
							IsSaved = False
						EndIf
					Case SET_ANIMDEL
						S = RequestString ("New anim change delay? (actual : " + AnimDelay + ") (ms)", 8, True)
						If (Not Cancel)
							If (Len (S) = 0) Then S = "0"
							AnimDelay = Int (S)
							SetGadgetText (SetAnimDelay, "Anim Change Delay [" + AnimDelay + "]")
							UpdateWindowMenu (MainWindow)
						EndIf
					Case SET_NAME
						If (BoneSelected > -1)
							S = RequestString ("Enter new bone name", 15)
							If (Not Cancel)
								VB = V.SearchBone (Bones[BoneSelected])
								VB.Name = S
								UpdateBoneWindow ()
								IsSaved = False
							EndIf
						EndIf
					Case HELP_ACERCA
						DisableGadget (MainWindow)
						DisableGadget (PreviewWindow)
						DisableGadget (AnimsWindow)
						DisableGadget (BonesWindow)
						ShowGadget (TheAbout)
				End Select
			' SALIR
			Case EVENT_WINDOWCLOSE
				Select (EventSource ())
					Case MainWindow
						If (IsSaved) Then End
						If (Confirm ("Exit UAPEdit? Changes will be lost"))
							'postit : autosave?
							End
						EndIf
					Case TheAbout
						EnableGadget (MainWindow)
						EnableGadget (PreviewWindow)
						HideGadget (TheAbout)
						ActivateGadget (MainWindow)
					Case PNGInfo
						EnableGadget (MainWindow)
						EnableGadget (PreviewWindow)
						EnableGadget (AnimsWindow)
						EnableGadget (BonesWindow)
						HideGadget (PNGInfo)
						ActivateGadget (MainWindow)
				End Select
			Default
		End Select
	Wend

	Local ugp:Int = FileUGP <> Null

'	If (RedrawMain)
		SetGraphics (CanvasGraphics (MainCanvas))
		SetViewport (0, 0, W_MAIN, H_MAIN)
		Cls
		If (GridVisible)
			SetAlpha (1.0)
			SetTransform (0.0, MainZoom, MainZoom)
			SetColor (255, 255, 255)
			DrawImage (Grid, W2_MAIN + MainX, H2_MAIN + MainY)
		EndIf
		
		' backup 
		I = A.FrameTime2
		Frame = A.CurrentFrame
		A.FrameTime2 = 0
		A.CurrentFrame = ActFrameF
		A.PrepareBoneList (ActTime)
		' restore :)
		A.FrameTime2 = I
		A.CurrentFrame = Frame
		
		For RB = EachIn A.BoneList
			' draw time
			If (RB.Graph >= 0)
				SetRotation (RB.Angle)
				SetScale (MainZoom * RB.Size, MainZoom * RB.Size)
				If (ugp And RB.Graph < FileUGP.frames)
					SetImageHandle(FileUGP.image, FileUGP.xHandles[RB.Graph], FileUGP.yHandles[RB.Graph])
					DrawImage(FileUGP.image, ToScreenX (RB.X), ToScreenY (RB.Y), RB.Graph)
				ElseIf (Not ugp And RB.Graph < FileVGP.Size)
					If (FileVGP.Graph[RB.Graph])
						DrawImage (FileVGP.Graph[RB.Graph].I, ToScreenX (RB.X), ToScreenY (RB.Y))
'					Else
'						DrawImage (GraphicNotFound, ToScreenX (RB.X), ToScreenY (RB.Y))
					EndIf
'				Else
'					DrawImage (GraphicNotFound, ToScreenX (RB.X), ToScreenY (RB.Y))
				EndIf
			EndIf
		Next
		If (SkelVisible) Then DrawSkel ()
		Flip (1)
		RedrawMain = False
'	EndIf

	A.Update (ActTime)

	If (PreviewVisible)
		A.PrepareBoneList (ActTime)
		SetGraphics (CanvasGraphics (PreviewCanvas))
		SetViewport (0, 0, W_PREVIEW, H_PREVIEW)
		Cls
		If (GridVisible)
			SetAlpha (1.0)
			SetTransform (0.0, PreviewZoom, PreviewZoom)
			SetColor (255, 255, 255)
			DrawImage (Grid, W2_PREVIEW + PreviewX, H2_PREVIEW + PreviewY)
		EndIf
		SetAlpha (1.0)
		SetColor (255, 255, 255)
		SetBlend (ALPHABLEND)
		For RB = EachIn A.BoneList
			' draw time
			If (RB.Graph >= 0)
				SetRotation (RB.Angle)
				SetScale (PreviewZoom * RB.Size, PreviewZoom * RB.Size)
				If (ugp And RB.Graph < FileUGP.frames)
					SetImageHandle(FileUGP.image, FileUGP.xHandles[RB.Graph], FileUGP.yHandles[RB.Graph])
					DrawImage(FileUGP.image, ToScreen2X (RB.X), ToScreen2Y (RB.Y), RB.Graph)
				ElseIf (Not ugp And RB.Graph < FileVGP.Size)
					If (FileVGP.Graph[RB.Graph])
						DrawImage (FileVGP.Graph[RB.Graph].I, ToScreen2X (RB.X), ToScreen2Y (RB.Y))
'					Else
'						DrawImage (GraphicNotFound, ToScreen2X (RB.X), ToScreen2Y (RB.Y))
					EndIf
'				Else
'					DrawImage (GraphicNotFound, ToScreen2X (RB.X), ToScreen2Y (RB.Y))
				EndIf
			EndIf
		Next
		SetTransform (0.0, 1.0, 1.0)
		DrawText (FPS, W_PREVIEW - 35, H_PREVIEW - 40)
		DrawText (Int (ActTime / 1000.0), 5, H_PREVIEW - 40)
		Flip
		'RedrawPreview = False
	EndIf
	
	If (BonewVisible And RedrawBone)
		SetGraphics (CanvasGraphics (BoneCanvas))
		SetViewport (0, 0, 800, 600)
		Cls
		SetBlend (ALPHABLEND)
		SetTransform (0.0, 1.0, 1.0)
		SetAlpha (1.0)
		SetColor (255, 255, 255)
		If (BoneSelected > -1)
			I = ActFrameF.Bones[Bones[BoneSelected]].Graph
			If (I >= 0)
				If (ugp And I < FileUGP.frames)
					SetImageHandle(FileUGP.image, FileUGP.xHandles[I], FileUGP.yHandles[I])
					DrawImage(FileUGP.image, 75.0, 75.0, I)
				ElseIf (Not ugp And I < FileVGP.Size)
					If (FileVGP.Graph[I])
						DrawImage (FileVGP.Graph[I].I, 75.0, 75.0)
					Else
						DrawImage (GraphicNotFound, 75.0, 75.0)
					EndIf
				Else
					DrawImage (GraphicNotFound, 75.0, 75.0)
				EndIf
			EndIf
		EndIf
		Flip
		RedrawBone = False
	EndIf
Wend


' *******************





' *********** FUNCTIONS


Function UpdatePreviewSliders ()
	Select (SliderValue (PreviewZoomS))
		Case 9 SetGadgetText (PreviewZoomT, "5.5x") PreviewZoom = 5.5
		Case 8 SetGadgetText (PreviewZoomT, "5x") PreviewZoom = 5.0
		Case 7 SetGadgetText (PreviewZoomT, "4.5x") PreviewZoom = 4.5
		Case 6 SetGadgetText (PreviewZoomT, "4x") PreviewZoom = 4.0
		Case 5 SetGadgetText (PreviewZoomT, "3.5x") PreviewZoom = 3.5
		Case 4 SetGadgetText (PreviewZoomT, "3x") PreviewZoom = 3.0
		Case 3 SetGadgetText (PreviewZoomT, "2.5x") PreviewZoom = 2.5
		Case 2 SetGadgetText (PreviewZoomT, "2x") PreviewZoom = 2.0
		Case 1 SetGadgetText (PreviewZoomT, "1.5x") PreviewZoom = 1.5
		Case 0 SetGadgetText (PreviewZoomT, "1x") PreviewZoom = 1.0
		Case -1 SetGadgetText (PreviewZoomT, "1/1.5x") PreviewZoom = 1.0 / 1.5
		Case -2 SetGadgetText (PreviewZoomT, "1/2x") PreviewZoom = 1.0 / 2.0
		Case -3 SetGadgetText (PreviewZoomT, "1/2.5x") PreviewZoom = 1.0 / 2.5
		Case -4 SetGadgetText (PreviewZoomT, "1/3x") PreviewZoom = 1.0 / 3.0
		Case -5 SetGadgetText (PreviewZoomT, "1/3.5x") PreviewZoom = 1.0 / 3.5
		Case -6 SetGadgetText (PreviewZoomT, "1/4x") PreviewZoom = 1.0 / 4.0
		Case -7 SetGadgetText (PreviewZoomT, "1/4.5x") PreviewZoom = 1.0 / 4.5
		Case -8 SetGadgetText (PreviewZoomT, "1/5x") PreviewZoom = 1.0 / 5.0
		Case -9 SetGadgetText (PreviewZoomT, "1/5.5x") PreviewZoom = 1.0 / 5.5
	End Select
	Select (SliderValue (PreviewTimeS))
		Case 9 SetGadgetText (PreviewTimeT, "5.5x") TD = 5.5
		Case 8 SetGadgetText (PreviewTimeT, "5x") TD = 5.0
		Case 7 SetGadgetText (PreviewTimeT, "4.5x") TD = 4.5
		Case 6 SetGadgetText (PreviewTimeT, "4x") TD = 4.0
		Case 5 SetGadgetText (PreviewTimeT, "3.5x") TD = 3.5
		Case 4 SetGadgetText (PreviewTimeT, "3x") TD = 3.0
		Case 3 SetGadgetText (PreviewTimeT, "2.5x") TD = 2.5
		Case 2 SetGadgetText (PreviewTimeT, "2x") TD = 2.0
		Case 1 SetGadgetText (PreviewTimeT, "1.5x") TD = 1.5
		Case 0 SetGadgetText (PreviewTimeT, "1x") TD = 1.0
		Case -1 SetGadgetText (PreviewTimeT, "1/1.5x") TD = 1.0 / 1.5
		Case -2 SetGadgetText (PreviewTimeT, "1/2x") TD = 1.0 / 2.0
		Case -3 SetGadgetText (PreviewTimeT, "1/2.5x") TD = 1.0 / 2.5
		Case -4 SetGadgetText (PreviewTimeT, "1/3x") TD = 1.0 / 3.0
		Case -5 SetGadgetText (PreviewTimeT, "1/3.5x") TD = 1.0 / 3.5
		Case -6 SetGadgetText (PreviewTimeT, "1/4x") TD = 1.0 / 4.0
		Case -7 SetGadgetText (PreviewTimeT, "1/4.5x") TD = 1.0 / 4.5
		Case -8 SetGadgetText (PreviewTimeT, "1/5x") TD = 1.0 / 5.0
		Case -9 SetGadgetText (PreviewTimeT, "1/5.5x") TD = 1.0 / 5.5
	End Select
	TDDest = TD
End Function

Function UpdateBoneWindow ()
	Local I:Int = 0
	Local VB:VBABone = Null
'	SetGadgetText (BoneNBones, "Bones : " + V.NBones)
	SetGadgetText (BoneVGP, UGPVGPName)
	' Bone List
	For I = 0 To (V.NBones - 1)
		Bones[ActFrameF.Bones[I].Z] = I
	Next	
	ClearGadgetItems (BoneList)
	For I = 0 To (V.NBones - 1)
		VB = V.SearchBone (Bones[I])
		If (VB) Then AddGadgetItem (BoneList, Bones[I] + ". " + VB.Name, 0, -1, VB.Name, String (I))
	Next
	UpdateBoneInfo ()
	If (BoneSelected > -1 And BoneSelected < CountGadgetItems (BoneList))
		SelectGadgetItem (BoneList, BoneSelected)
	EndIf
End Function

Function UpdateBoneInfo ()
	Local VBS:VBABoneState = Null
	Local VB:VBABone = Null
	If (BoneSelected > -1)
		VBS = ActFrameF.Bones[Bones[BoneSelected]]
		VB = V.SearchBone (Bones[BoneSelected])
		SetTextAreaText (BoneRange1, VB.Range1)
		SetTextAreaText (BoneRange2, VB.Range2)
		SetGadgetText (BoneGraph, VBS.Graph)
		SetGadgetText (BoneX, "X : " + VBS.X)
		SetGadgetText (BoneY, "Y : " + VBS.Y)
		SetGadgetText (BoneAngle, "Angle : " + VBS.Angle)
		SetGadgetText (BoneSize, "Size : " + VBS.Size)
	Else
		SetTextAreaText (BoneRange1, "1")
		SetTextAreaText (BoneRange2, "1")
		SetGadgetText (BoneGraph, "--")
		SetGadgetText (BoneX, "X : --")
		SetGadgetText (BoneY, "Y : --")
		SetGadgetText (BoneAngle, "Angle : --")
		SetGadgetText (BoneSize, "Size : --")
	EndIf
End Function

Function UpdateAnimsWindow ()
	SetGadgetText (AnimsActAnim, ActAnim)
	SetGadgetText (AnimsActFrame, ActFrame)
	SetGadgetText (AnimsNAnims, V.NAnims - 1)
	SetGadgetText (AnimsNFrames, V.Anim[ActAnim].NFrames - 1)
	SetGadgetText (AnimsFrameDur, ActFrameF.Duration)
End Function

Function MoveBoneUp ()
	Local I:Int
	If (BoneSelected < 1) Then Return
	I = Bones[BoneSelected]
	Bones[BoneSelected] = Bones[BoneSelected - 1]
	Bones[BoneSelected - 1] = I
	I = ActFrameF.Bones[Bones[BoneSelected - 1]].Z
	ActFrameF.Bones[Bones[BoneSelected - 1]].Z = ActFrameF.Bones[Bones[BoneSelected]].Z
	ActFrameF.Bones[Bones[BoneSelected]].Z = I
	BoneSelected:- 1
	UpdateBoneWindow ()
	SelectGadgetItem (BoneList, BoneSelected)
	IsSaved = False
End Function

Function MoveBoneDown ()
	Local I:Int
	If (BoneSelected > (V.NBones - 2)) Then Return 
	I = Bones[BoneSelected]
	Bones[BoneSelected] = Bones[BoneSelected + 1]
	Bones[BoneSelected + 1] = I
	I = ActFrameF.Bones[Bones[BoneSelected + 1]].Z
	ActFrameF.Bones[Bones[BoneSelected + 1]].Z = ActFrameF.Bones[Bones[BoneSelected]].Z
	ActFrameF.Bones[Bones[BoneSelected]].Z = I
	BoneSelected:+ 1
	UpdateBoneWindow ()
	SelectGadgetItem (BoneList, BoneSelected)
	IsSaved = False
End Function

Function FileNew ()
	Local I:Int
	If (V) Then V.Unload ()
	If (A) Then A.Unload ()
'	If (File) Then File.Unload ()
'	File = VGP.LoadVGP ("tools/defbones.vgp")
'	UGPVGPName = "defbones.vgp"
	V = New VBA
	V.NBones = 1
	V.Father = New VBABone
	V.Father.ID = 0
	V.Father.Name = "Base"
	V.NAnims = 1
	V.Anim = V.Anim[..1]
	V.Anim[0] = New VBAAnim
	V.Anim[0].NFrames = 1
	V.Anim[0].Frames = New VBAFrame
	V.Anim[0].Frames.Duration = 100
	V.Anim[0].Frames.Bones = V.Anim[0].Frames.Bones[..1]
	V.Anim[0].Frames.Bones[0] = New VBABoneState
	A = AnimState.Create (V)
	A.V = V
	A.Loop = True
	IsPlaying = False
	SetGadgetText (PreviewPlay, "Play")
	I = AnimDelay
	AnimDelay = 0
	PutAnim (0)
	AnimDelay = I
	A.FrameTime2 = 0
	Bones = Bones[0..0]
	Bones = Bones[..64]
	SetStatusText (MainWindow, "New VBA started")
	SetGadgetText (SetHeight, "Height [" + V.Height + "]")
	SaveFile = "untitled.uap"
	SavePath = "untitled.uap"
	UpdateBoneWindow ()
	UpdateAnimsWindow ()
	IsSaved = True
	LastSaved = False
	Help = False
	TimeToHelp = MilliSecs () + 5000
	RedrawMain = True
	RedrawBone = True
	RedrawPreview = True
	Clipboard = Null
	ShiftStatus = False
	SelectBone (0)
	SetTextAreaText (AnimsFrameDurT, "0")
	SetTextAreaText (AnimsGotoFrameT, "0")
	SetTextAreaText (AnimsGotoFrameT, "0")
	SetTextAreaText (AnimsExchangeT, "0")
	DisableMenu (Paste)
	UpdateWindowMenu (MainWindow)
End Function


Function FileLoad (S:String)
	Local I:Int
	If (V) Then V.Unload ()
	If (A) Then A.Unload ()
'	If (File) Then File.Unload ()
'	File = VGP.LoadVGP ("tools/defbones.vgp")
'	UGPVGPName = "defbones.vgp"
	If (Lower(Right(Trim(S), 3)) = "vba")
		V = VBA.LoadVBA (S)
	Else
		V = VBA.LoadUAP (S)
	EndIf
	A = AnimState.Create (V)
	A.V = V
	A.Loop = True
	IsPlaying = False
	SetGadgetText (PreviewPlay, "Play")
	I = AnimDelay
	AnimDelay = 0
	PutAnim (0)
	AnimDelay = I
	A.FrameTime2 = 0
	Bones = Bones[0..0]
	Bones = Bones[..64]
	SetStatusText (MainWindow, "VBA/UAP "+S+" loaded OK")
	SetGadgetText (SetHeight, "Height [" + V.Height + "]")
	SaveFile = StripDir (S)
	SavePath = S
	UpdateBoneWindow ()
	UpdateAnimsWindow ()
	IsSaved = True
	LastSaved = False
	Help = False
	TimeToHelp = MilliSecs () + 5000
	RedrawMain = True
	RedrawBone = True
	RedrawPreview = True
	Clipboard = Null
	ShiftStatus = False
	SelectBone (V.Father.ID)
	SetTextAreaText (AnimsFrameDurT, "0")
	SetTextAreaText (AnimsGotoFrameT, "0")
	SetTextAreaText (AnimsGotoFrameT, "0")
	SetTextAreaText (AnimsExchangeT, "0")
	DisableMenu (Paste)
	UpdateWindowMenu (MainWindow)
End Function


Function RequestString:String (Texto:String, Limit:Int = 0, OnlyNumbers:Int = False)
	Local Salir:Int = False
	Local Backup:Float = TD
	Cancel = False
	DisableGadget (MainWindow)
	DisableGadget (PreviewWindow)
	DisableGadget (AnimsWindow)
	DisableGadget (BonesWindow)
	ShowGadget (TextRequest)
	SetGadgetText (TextRequestL, Texto)
	SetTextAreaText (TextRequestT, "")
	ActivateGadget (TextRequestT)
	TD = 0.0
	While (Not Salir)
		Timer ()
		While (PollEvent () And (Not Salir))
			Select (EventID ())
				Case EVENT_GADGETACTION
					If (Instr (TextAreaText (TextRequestT), "~n") > 0)
						SetTextAreaText (TextRequestT, Left (TextAreaText (TextRequestT), TextAreaLen (TextRequestT) - 1))
						Salir = True
					Else
						If (OnlyNumbers)
							SetTextAreaText (TextRequestT, AntiNaN (TextAreaText (TextRequestT)))
						EndIf
						If (Limit > 0)
							If (TextAreaLen (TextRequestT) > Limit)
								SetTextAreaText (TextRequestT, Left (TextAreaText (TextRequestT), Limit))
							EndIf
						EndIf
					EndIf
				Case EVENT_WINDOWCLOSE
					' this window is closed / canceled operation
					Cancel = True
					Salir = True
			End Select
		Wend
	Wend
	TD = Backup
	EnableGadget (MainWindow)
	EnableGadget (PreviewWindow)
	EnableGadget (AnimsWindow)
	EnableGadget (BonesWindow)
	HideGadget (TextRequest)
	ActivateGadget (MainWindow)
	Return (TextAreaText (TextRequestT))
End Function

Function AntiNan:String (S:String)
	Local I:Int = 0
	Local P:String = ""
	Local Point:Int = False
	While (I < Len (S))
		P = Mid (S, I + 1, 1)
		If ((P = "1") Or (P = "2") Or (P = "3") Or (P = "4") Or (P = "5") Or (P = "6") ..
			Or (P = "7") Or (P = "8") Or (P = "9") Or (P = "0") Or ((P = ".") And Not Point))
			I:+ 1
			If (P = ".") Then Point = True
		Else
			If ((I = 0) And ((P = "-") Or (P = "+")))
				I:+ 1
			Else
				If (I > 0)
					S = Left (S, I) + Right (S, Len (S) - I - 1)
				Else
					S = Right (S, Len (S) - 1)
				EndIf
			EndIf
		EndIf
	Wend
	Return (S)
End Function

Function NaNPure:String (S:String)
	Local I:Int = 0
	Local P:String = ""
	While (I < Len (S))
		P = Mid (S, I + 1, 1)
		If ((P = "1") Or (P = "2") Or (P = "3") Or (P = "4") Or (P = "5") Or (P = "6") ..
			Or (P = "7") Or (P = "8") Or (P = "9") Or (P = "0"))
			I:+ 1
		Else
			If (I > 0)
				S = Left (S, I) + Right (S, Len (S) - I - 1)
			Else
				S = Right (S, Len (S) - 1)
			EndIf
		EndIf
	Wend
	Return (S)
End Function

Function SigAnim ()
	Local I:Int = 0
	If (ActAnim < (V.NAnims - 1))
		If (BoneSelected > -1)
			I = Bones[BoneSelected]
		Else
			I = -1
		EndIf
		PutAnim (ActAnim + 1)
		SelectBone (I)
		RedrawMain = True
		RedrawPreview = True
		RedrawBone = True
		UpdateAnimsWindow ()
	Else
		If (BoneSelected > -1)
			I = Bones[BoneSelected]
		Else
			I = -1
		EndIf
		PutAnim (0)
		SelectBone (I)
		RedrawMain = True
		RedrawPreview = True
		RedrawBone = True
		UpdateAnimsWindow ()
	EndIf
End Function

Function AntAnim ()
	Local I:Int = 0
	If (ActAnim > 0)
		If (BoneSelected > -1)
			I = Bones[BoneSelected]
		Else
			I = -1
		EndIf
		PutAnim (ActAnim - 1)
		SelectBone (I)
		RedrawMain = True
		RedrawPreview = True
		RedrawBone = True
		UpdateAnimsWindow ()
	Else
		If (BoneSelected > -1)
			I = Bones[BoneSelected]
		Else
			I = -1
		EndIf
		PutAnim (V.NAnims - 1)
		SelectBone (I)
		RedrawMain = True
		RedrawPreview = True
		RedrawBone = True
		UpdateAnimsWindow ()
	EndIf
End Function

Function SigFrame ()
	Local I:Int = 0
	If (ActFrame < (V.Anim[ActAnim].NFrames - 1))
		If (BoneSelected > -1)
			I = Bones[BoneSelected]
		Else
			I = -1
		EndIf
		ActFrame:+ 1
		ActFrameF = ActFrameF.NextFrame
		SelectBone (I)
		RedrawMain = True
		RedrawPreview = True
		RedrawBone = True
		UpdateAnimsWindow ()
	Else
		If (BoneSelected > -1)
			I = Bones[BoneSelected]
		Else
			I = -1
		EndIf
		ActFrame = 0
		ActFrameF = V.Anim[ActAnim].Frames
		SelectBone (I)
		RedrawMain = True
		RedrawPreview = True
		RedrawBone = True
		UpdateAnimsWindow ()
	EndIf
End Function

Function AntFrame ()
	Local I:Int = 0
	If (ActFrame > 0)
		If (BoneSelected > -1)
			I = Bones[BoneSelected]
		Else
			I = -1
		EndIf
		ActFrame:- 1
		ActFrameF = V.Anim[ActAnim].SearchFrame (ActFrame)
		SelectBone (I)
		RedrawMain = True
		RedrawPreview = True
		RedrawBone = True
		UpdateAnimsWindow ()
	Else
		If (BoneSelected > -1)
			I = Bones[BoneSelected]
		Else
			I = -1
		EndIf
		ActFrame = V.Anim[ActAnim].NFrames - 1
		ActFrameF = V.Anim[ActAnim].SearchFrame (ActFrame)
		SelectBone (I)
		RedrawMain = True
		RedrawPreview = True
		RedrawBone = True
		UpdateAnimsWindow ()
	EndIf
End Function

Function SelectBone (I:Int)
	Local J:Int = 0
	If (I = -1)
		BoneSelected = -1
	Else
		While (Bones[J] <> I)
			J:+ 1
		Wend
		BoneSelected = J
	EndIf
	UpdateBoneWindow ()
End Function

Function DrawSkel ()
	Local Per1:Float, Per2:Float
	Local Bo:VBABone
	Local RelAngle:Float
	Local RelX:Float, RelY:Float
	Local BS:VBABoneState
	Local I:Int = 0 ' to iterate bones sons
	If (V.NBones = 0) Then Return 'if no bones, what are you expecting to anim?
	' static anim
	BS = ActFrameF.Bones[V.Father.ID]
	RelAngle = 0.0
'	RelAngle = BS.Angle Mod 360.0
'	If (RelAngle < 0.0) Then RelAngle:+ 360.0
	RelX = (BS.X * VCos[Int (RelAngle * 10.0)]) - (BS.Y * VSin[Int (RelAngle * 10.0)])
	RelY = (BS.Y * VCos[Int (RelAngle * 10.0)]) + (BS.X * VSin[Int (RelAngle * 10.0)])
	RelAngle = BS.Angle
	OnStack = 0
	Stack1[0] = V.Father	Stack2[0] = 0	Stack3[0] = RelAngle
	Stack4[0] = RelX	Stack5[0] = RelY
	If ((BoneSelected > -1) And (V.Father.ID = Bones[BoneSelected]))
		DrawNode (V.Father.ID, RelX, RelY, True)
	Else
		DrawNode (V.Father.ID, RelX, RelY)
	EndIf
	While (OnStack > -1)
		Bo = Stack1[OnStack]
		If (Stack2[OnStack] < Bo.NSons)
			' still sons left to iterate
			Bo = Bo.Sons[Stack2[OnStack]]
			BS = ActFrameF.Bones[Bo.ID]
			RelAngle = Stack3[OnStack] Mod 360.0
			If (RelAngle < 0.0) Then RelAngle:+ 360.0
			RelX = Stack4[OnStack] + (BS.X * VCos[Int (RelAngle * 10.0)]) - (BS.Y * VSin[Int (RelAngle * 10.0)])
			RelY = Stack5[OnStack] + (BS.Y * VCos[Int (RelAngle * 10.0)]) + (BS.X * VSin[Int (RelAngle * 10.0)])
			If ((BoneSelected > -1) And (Bo.ID = Bones[BoneSelected]))
				DrawRelation (Stack4[OnStack], Stack5[OnStack], RelX, RelY, True)
				DrawNode (Bo.ID, RelX, RelY, True)
			Else
				DrawRelation (Stack4[OnStack], Stack5[OnStack], RelX, RelY)
				DrawNode (Bo.ID, RelX, RelY)
			EndIf
			Stack2[OnStack]:+1
			OnStack:+ 1
			Stack1[OnStack] = Bo
			Stack2[OnStack] = 0	' begin with first son
			Stack3[OnStack] = RelAngle + BS.Angle
			Stack4[OnStack] = RelX
			Stack5[OnStack] = RelY
		Else
			' this branch is over!
			Onstack:- 1
		EndIf
	Wend
End Function

Function DrawNode (ID:Int, X:Float, Y:Float, IsThis:Int = False)
	Local MX:Float, MY:Float
	SetTransform ()
	SetAlpha (1.0)
	SetBlend (ALPHABLEND)
	MX = ToScreenX (X)
	MY = ToScreenY (Y)
	SetColor (255, 255, 255)
	BonePositions[ID].X = MX
	BonePositions[ID].Y = MY
	DrawOval (MX - 10.0, MY - 7.5, 20.0, 15.0)
	If (IsThis) Then SetColor (0, 255, 0) Else SetColor (255, 0, 0)
	DrawOval (MX - 9.0, MY - 6.5, 18.0, 13.0)
	SetColor (255, 255, 255)
	If (ID < 10) 
		DrawText (ID, MX - 4.0, MY - 6.0)
	Else
		DrawText (ID, MX - 7.5, MY - 6.0)
	EndIf
End Function

Function DrawRelation (X1:Float, Y1:Float, X2:Float, Y2:Float, IsThis:Int = False)
	Local Angle:Float = ATan2 (Y2 - Y1, X2 - X1)
	Local X:Float, Y:Float, A:Float
	SetTransform ()
	SetAlpha (1.0)
	SetBlend (ALPHABLEND)
	If (IsThis) SetColor (0, Shine + 127, 0) Else SetColor (Shine, Shine, Shine)
	A = (Angle + 90.0) Mod 360.0
	If (A < 0.0) Then A:+ 360.0
	A:* 10.0
	X = X1 + (7.5 * VCos[Int (A)] / MainZoom)
	Y = Y1 + (7.5 * VSin[Int (A)] / MainZoom)
	DrawLine (ToScreenX (X), ToScreenY (Y), ToScreenX (X2), ToScreenY (Y2), False)
	If (IsThis) Then DrawLine (ToScreenX (X), ToScreenY (Y) + 1, ToScreenX (X2), ToScreenY (Y2) + 1, False)
	A = (Angle - 90.0) Mod 360.0
	If (A < 0.0) Then A:+ 360.0
	A:* 10.0
	X = X1 + (7.5 * VCos[Int (A)] / MainZoom)
	Y = Y1 + (7.5 * VSin[Int (A)] / MainZoom)
	DrawLine (ToScreenX (X), ToScreenY (Y), ToScreenX (X2), ToScreenY (Y2), False)
End Function

Function ExportActAnim (S:String, SX:Float, SY:Float, Power:Int, Square:Int, Center:Int)
	Local IncFrame:Int
	Local TAnimTime:Int
	Local RB:RepBone
	Local X1:Float, X2:Float
	Local Y1:Float, Y2:Float
	Local W1:Float, W2:Float, H1:Float, H2:Float
	Local BBX1:Float, BBY1:Float, BBX2:Float, BBY2:Float ' bounds relative To bone 0 x,y
	Local Found:Int
	Local I:Int = 0
	Local PM:TPixmap = Null
	SaveTimer ()
	ActTime = 0.0
	' IncFrame contains time increment per frame
	' TAnimTime contains total anim time
	TAnimTime = V.Anim[ActAnim].CountTime ()
	If (ButtonState (PNGInfoFrames))
		' number of frames
		IncFrame = TAnimTime / Int (TextAreaText (PNGInfoN))
	Else
		' FPS
		IncFrame = 1000 / Int (TextAreaText (PNGInfoN))
	EndIf
	A.ChangeAnim (ActAnim, ActTime, 0)
	' first pass... let's calculate bounds
	SetGraphics (CanvasGraphics (BigCanvas))
	SetAlpha (1.0)
	SetColor (255, 255, 255)
	SetBlend (ALPHABLEND)
	SetTransform (0.0, SX, SY)
	SetViewport (0, 0, SW, SH)
	BBX1 = (SW / 2) - 1	BBY1 = (SH / 2) - 1
	BBX2 = BBX1	BBY2 = BBY1
	Local ugp:Int = FileUGP <> Null
	While (ActTime < Float (TAnimTime))
		ActTime:+ Float (IncFrame)
		A.Update (ActTime)
		A.PrepareBoneList (ActTime)
		SetClsColor (0, 0, 0)
		Cls
		For RB = EachIn A.BoneList
			' bounding time
			If (RB.Graph >= 0)
				SetRotation (RB.Angle)
				SetScale (RB.Size * SX, RB.Size * SY)
				If (ugp And RB.Graph < FileUGP.frames)
					SetImageHandle(FileUGP.image, FileUGP.xHandles[RB.Graph], FileUGP.yHandles[RB.Graph])
					DrawImage(FileUGP.image, Float (SW / 2) - 1.0 + (RB.X * SX), Float (SH / 2) - 1.0 + (RB.Y * SY), RB.Graph)
				ElseIf (Not ugp And RB.Graph < FileVGP.Size)
					If (FileVGP.Graph[RB.Graph])
						DrawImage (FileVGP.Graph[RB.Graph].I, Float (SW / 2) - 1.0 + (RB.X * SX), Float (SH / 2) - 1.0 + (RB.Y * SY))
					EndIf
				EndIf
			EndIf
		Next
		PM = GrabPixmap (0, 0, SW, SH)
		Found = False
		For X1 = 0 To SW - 1
			For I = 0 To SH - 1
				If ((ReadPixel (PM, X1, I) | $FF000000) <> $FF000000)
					' alpha > 0
'					If (ActTime = IncFrame) Then SavePixmapPNG (PM, "somierdas.png")
					Found = True
					Exit
				Else
					If ((I = (SH - 1)) And (X1 = (SW - 1)))
						' this map was all empty
						X1 = SW
						Found = True
						Exit
					EndIf
				EndIf
			Next
			If (Found) Then Exit
		Next
		If (X1 < SW)
			Found = False
			For Y1 = 0 To SH - 1
				For I = 0 To SW - 1
					If ((ReadPixel (PM, I, Y1) | $FF000000) <> $FF000000) 
						Found = True
						Exit
					EndIf
				Next
				If (Found) Then Exit
			Next
			Found = False
			For X2 = SW - 1 To 0 Step -1
				For I = 0 To SH - 1
					If ((ReadPixel (PM, X2, I) | $FF000000) <> $FF000000)
						Found = True
						Exit
					EndIf
				Next
				If (Found) Then Exit
			Next
			Found = False
			For Y2 = SH - 1 To 0 Step -1
				For I = 0 To SW - 1
					If ((ReadPixel (PM, I, Y2) | $FF000000) <> $FF000000)
						Found = True
						Exit
					EndIf
				Next
				If (Found) Then Exit
			Next
			If (X1 < BBX1) Then BBX1 = X1
			If (Y1 < BBY1) Then BBY1 = Y1
			If (X2 > BBX2) Then BBX2 = X2
			If (Y2 > BBY2) Then BBY2 = Y2
		EndIf
		' second pass, white color
		SetClsColor (255, 255, 255)
		Cls
		For RB = EachIn A.BoneList
			' bounding time
			If (RB.Graph >= 0)
				SetRotation (RB.Angle)
				SetScale (RB.Size * SX, RB.Size * SY)
				If (ugp And RB.Graph < FileUGP.frames)
					SetImageHandle(FileUGP.image, FileUGP.xHandles[RB.Graph], FileUGP.yHandles[RB.Graph])
					DrawImage (FileUGP.image, Float (SW / 2) - 1.0 + (RB.X * SX), Float (SH / 2) - 1.0 + (RB.Y * SY), RB.Graph)
				ElseIf (Not ugp And RB.Graph < FileVGP.Size)
					If (FileVGP.Graph[RB.Graph])
						DrawImage (FileVGP.Graph[RB.Graph].I, Float (SW / 2) - 1.0 + (RB.X * SX), Float (SH / 2) - 1.0 + (RB.Y * SY))
					EndIf
				EndIf
			EndIf
		Next
		PM = GrabPixmap (0, 0, SW, SH)
		Found = False
		For X1 = 0 To SW - 1
			For I = 0 To SH - 1
				If ((ReadPixel (PM, X1, I) | $FF000000) <> $FFFFFFFF)
					' alpha > 0
					Found = True
					Exit
				Else
					If ((I = (SH - 1)) And (X1 = (SW - 1)))
						' this map was all empty
						X1 = SW
						Found = True
						Exit
					EndIf
				EndIf
			Next
			If (Found) Then Exit
		Next
		If (X1 < SW)
			Found = False
			For Y1 = 0 To SH - 1
				For I = 0 To SW - 1
					If ((ReadPixel (PM, I, Y1) | $FF000000) <> $FFFFFFFF) 
						Found = True
						Exit
					EndIf
				Next
				If (Found) Then Exit
			Next
			Found = False
			For X2 = SW - 1 To 0 Step -1
				For I = 0 To SH - 1
					If ((ReadPixel (PM, X2, I) | $FF000000) <> $FFFFFFFF)
						Found = True
						Exit
					EndIf
				Next
				If (Found) Then Exit
			Next
			Found = False
			For Y2 = SH - 1 To 0 Step -1
				For I = 0 To SW - 1
					If ((ReadPixel (PM, I, Y2) | $FF000000) <> $FFFFFFFF)
						Found = True
						Exit
					EndIf
				Next
				If (Found) Then Exit
			Next
			If (X1 < BBX1) Then BBX1 = X1
			If (Y1 < BBY1) Then BBY1 = Y1
			If (X2 > BBX2) Then BBX2 = X2
			If (Y2 > BBY2) Then BBY2 = Y2
		EndIf
		UpdateProgBar (PNGProgress, 0.0 + (ActTime / Float (TAnimTime)) * 0.5)
	Wend
	BBX1:- (SW / 2.0) - 2.0	BBY1:- (SH / 2.0) - 2.0	BBX2:- (SW / 2.0)	BBY2:- (SH / 2.0)
	If (Center)
		W2 = Max (Abs (BBX1), Abs (BBX2)) * 2.0
		H2 = Max (Abs (BBY1), Abs (BBY2)) * 2.0
	Else
		W2 = BBX2 - BBX1 + 1.0
		H2 = BBY2 - BBY1 + 1.0
	EndIf
	If (Power)
		W2 = NextPower2 (W2)
		H2 = NextPower2 (H2)
	EndIf
	If (Square)
		W2 = Max (W2, H2)
		H2 = W2
	EndIf
		
	ActTime = 0.0
	A.ChangeAnim (ActAnim, ActTime, 0)
	SetGraphics (CanvasGraphics (BigCanvas))
	SetAlpha (1.0)
	SetColor (255, 255, 255)
	SetBlend (ALPHABLEND)
	SetTransform (0.0, SX, SY)
	SetViewport (0, 0, W2, H2)
	SetClsColor (0, 0, 0)
	I = 0
	While (ActTime < Float (TAnimTime))
		ActTime:+ Float (IncFrame)
		A.Update (ActTime)
		A.PrepareBoneList (ActTime)
		Cls
		For RB = EachIn A.BoneList
			' draw time
			If (RB.Graph >= 0)
				SetRotation (RB.Angle)
				SetScale (RB.Size * SX, RB.Size * SY)
				If (ugp And RB.Graph < FileUGP.frames)
					SetImageHandle(FileUGP.image, FileUGP.xHandles[RB.Graph], FileUGP.yHandles[RB.Graph])
					If (Center)
						DrawImage (FileUGP.image, (W2 / 2.0) + (RB.X * SX), (H2 / 2.0) + (RB.Y * SY), RB.Graph)
					Else
						DrawImage (FileUGP.image, -BBX1 + (RB.X * SX), -BBY1 + (RB.Y * SY), RB.Graph)
					EndIf
				ElseIf (Not ugp And RB.Graph < FileVGP.Size)
					If (FileVGP.Graph[RB.Graph])
						If (Center)
							DrawImage (FileVGP.Graph[RB.Graph].I, (W2 / 2.0) + (RB.X * SX), (H2 / 2.0) + (RB.Y * SY))
						Else
							DrawImage (FileVGP.Graph[RB.Graph].I, -BBX1 + (RB.X * SX), -BBY1 + (RB.Y * SY))
						EndIf
					EndIf
				EndIf
			EndIf
		Next
		PM = GrabPixmap (0, 0, W2, H2)
		SavePixmapPNG (PM, S + FillZeros (String (I), 5) + ".png")
		I:+ 1
		UpdateProgBar (PNGProgress, 0.5 + (ActTime / Float (TAnimTime)) * 0.5)
	Wend
	RestoreTimer ()
	PutAnim (ActAnim)
	RedrawMain = True
	RedrawBone = True
End Function

Function FillZeros:String (S:String, N:Int)
	While (Len (S) < N)
		S = "0" + S
	Wend
	Return (S)
End Function

Function PutAnim (N:Int)
	If (N < 0 Or N > (V.NAnims - 1)) Then Return
	ActAnim = N
	ActFrame = 0
	ActFrameF = V.Anim[N].Frames
	If (IsPlaying)
		A.ChangeAnim (N, ActTime, AnimDelay)
	Else
		A.ChangeAnim (N, ActTime, 0)
		A.FrameTime2 = 0
	EndIf
End Function

Function ToScrollX:Float (X:Float)
	Return (((X - W2_MAIN_F) - MainX) / MainZoom)
End Function

Function ToScrollY:Float (Y:Float)
	Return (((Y - H2_MAIN_F) - MainY) / MainZoom) - 50.0
End Function

Function ToScreenX:Float (X:Float)
	Return ((X * MainZoom) + MainX + W2_MAIN_F)
End Function

Function ToScreenY:Float (Y:Float)
	Return (((Y + 50.0) * MainZoom) + MainY + H2_MAIN_F)
End Function

Function ToScroll2X:Float (X:Float)
	Return (((X - W2_PREVIEW_F) - PreviewX) / PreviewZoom)
End Function

Function ToScroll2Y:Float (Y:Float)
	Return (((Y - H2_PREVIEW_F) - PreviewY) / PreviewZoom) - 50.0
End Function

Function ToScreen2X:Float (X:Float)
	Return ((X * PreviewZoom) + PreviewX + W2_PREVIEW_F)
End Function

Function ToScreen2Y:Float (Y:Float)
	Return (((Y + 50.0) * PreviewZoom) + PreviewY + H2_PREVIEW_F)
End Function

Function NextPower2:Int (N:Int)
	Local I:Int = 1
	While (True)
		If (I > N) Then Return (I)
		I:* 2
	Wend
End Function



