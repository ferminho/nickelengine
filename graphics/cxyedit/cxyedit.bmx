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
Const G_WIDTH2:Int = G_WIDTH / 2
Const G_HEIGHT2:Int = G_HEIGHT / 2

Global currentImage:Int = 0
Global camX:Float = 0.0
Global camY:Float = 0.0
Global camZoomInv:Float = 1.0
Global background:TImage
Global bgXD:Float = 0.0
Global bgYD:Float = 0.0

Global allFiles:String[] = LoadDir(CurrentDir())

SelectValidPNGs(allFiles)

If (Len(validFiles) <= 0)
	RuntimeError("No images found")
EndIf

Graphics(G_WIDTH, G_HEIGHT, 0, 60, GRAPHICS_BACKBUFFER)
LoadImages(True, True)
CreateBackground()

While Not (KeyHit(KEY_ESCAPE))
	Cls()
	CheckControls()
	DrawBackground()
	DrawCurrentImage()
	DrawOverlays()
	Flip(1)
Wend


End



Function CheckControls()
	If (MouseHit(1))

	Else
		Local mz:Int = MouseZ()
		If (mz < 0)
			camZoomInv = camZoomInv * (0.90 ^ -mz)
		ElseIf (mz > 0)
			camZoomInv = camZoomInv * (1.10 ^ mz)
		EndIf
	EndIf
	
FlushMouse()
End Function

Function DrawBackground()
	SetAlpha(1.0)
	SetBlend(SOLIDBLEND)
	SetScale(1.0, 1.0)
	SetRotation(0.0)
	
	bgXD = (bgXD + 0.5) Mod BG_SIDE
	bgYD = (bgYD + 0.25) Mod BG_SIDE
	
	Local cols2:Int = (G_WIDTH / BG_SIDE) / 2
	Local rows2:Int = (G_HEIGHT / BG_SIDE) / 2
	
	For Local y:Int = -rows2 - 2 To rows2 + 1
		For Local x:Int = -cols2 - 2 To cols2 + 1
			DrawImage(background, bgXD + G_WIDTH2 + x * BG_SIDE, bgYD + G_HEIGHT2 + y * BG_SIDE)
		Next
	Next
	
End Function

Function DrawCurrentImage()
	SetAlpha(1.0)
	SetBlend(ALPHABLEND)
	SetScale(camZoomInv, camZoomInv)
	SetRotation(0.0)

	DrawImage(images[currentImage], G_WIDTH2, G_HEIGHT2)
End Function

Function DrawOverlays()
	SetAlpha(1.0)
	SetBlend(ALPHABLEND)
	SetScale(camZoomInv, camZoomInv)
	SetRotation(0.0)

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