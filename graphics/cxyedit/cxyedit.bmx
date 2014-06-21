' CXYEdit
' --------
'
' Unif Studios 2014

SuperStrict 

Framework BRL.GlMax2D
Import BRL.StandardIO

Import "..\ugpioutils.bmx"

Const G_WIDTH:Int = 1280
Const G_HEIGHT:Int = 720

Global currentImage:Int = 0
Global camX:Float = 0.0
Global camY:Float = 0.0
Global camZoom:Float = 1.0
Global background:TImage 

Global allFiles:String[] = LoadDir(CurrentDir())
SelectValidPNGs(allFiles)

If (Len(validFiles) <= 0)
	RuntimeError("No images found")
EndIf

Graphics(G_WIDTH, G_HEIGHT, 0, 60, GRAPHICS_BACKBUFFER)
LoadImages()
CreateBackground()

While Not (KeyHit(KEY_ESCAPE))
	Cls()
	DrawBackground()

	Flip(1)
Wend


End


Function DrawBackground()
	SetAlpha(1.0)
	SetBlend(SOLIDBLEND)
	SetScale(1.0, 1.0)
	SetRotation(0.0)
	
	Local cols:Int = G_WIDTH / BG_SIDE
	Local rows:Int = G_HEIGHT / BG_SIDE
	
	For Local y:Int = -1 To rows
		For Local x:Int = -1 To cols
			DrawImage(background, x * BG_SIDE, y * BG_SIDE)
		Next
	Next
	
End Function


Const BG_SIDE:Int = 64
Const BG_R1:Int = 75
Const BG_G1:Int = 190
Const BG_B1:Int = 110
Const BG_R2:Int = 240
Const BG_G2:Int = 240
Const BG_B2:Int = 240

Function CreateBackground()
	background = CreateImage(BG_SIDE, BG_SIDE, 1, DYNAMICIMAGE)
	Local px:TPixmap = LockImage(background)
	Local halfSide:Int = BG_SIDE / 2
	For Local x:Int = 0 To BG_SIDE - 1
		For Local y:Int = 0 To BG_SIDE - 1
			Local color:Int
			If ((x < halfSide And y < halfSide) Or (x >= halfSide And y >= halfSide))
				color = (255 Shl 24) | (BG_R1 Shl 16) | (BG_G1 Shl 8) | BG_B1
			Else
				color = (255 Shl 24) | (BG_R2 Shl 16) | (BG_G2 Shl 8) | BG_B2
			EndIf
			WritePixel(px, x, y, color)
		Next
	Next
	UnlockImage(background)
End Function