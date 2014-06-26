' CXYEdit
' --------
'
' Unif Studios 2014

SuperStrict 

Framework BRL.GlMax2D
Import BRL.StandardIO

Import "..\ugpioutils.bmx"

Const G_WIDTH:Float = 1280
Const G_HEIGHT:Float = 720
Const G_WIDTH2:Float = G_WIDTH / 2.0
Const G_HEIGHT2:Float = G_HEIGHT / 2.0

Const ZOOM_SPEED:Float = 0.1
Const ZOOM_SPEED_IN:Float = 1.0 + ZOOM_SPEED
Const ZOOM_SPEED_OUT:Float = 1.0 - ZOOM_SPEED
Const CAM_SPEED:Float = 16 / 60.0
Const CAM_SPEED_MULT:Float = 1.5


Global camX:Float = 0.0
Global camY:Float = 0.0
Global camZoomInv:Float = 1.0
Global background:TImage
Global bgXD:Float = 0.0
Global bgYD:Float = 0.0

Global currentImage:Int = 0

Global frameCounter:Int = 0

Global allFiles:String[] = LoadDir(CurrentDir())

SelectValidPNGs(allFiles)

If (Len(validFiles) <= 0)
	RuntimeError("No images found")
EndIf

Graphics(G_WIDTH, G_HEIGHT, 0, 60, GRAPHICS_BACKBUFFER)
LoadImages(True, True)
CreateBackground()

While Not (KeyHit(KEY_ESCAPE))
	frameCounter :+ 1
	Cls()
	CheckControls()
	DrawBackground()
	DrawCurrentImage()
	DrawCornerOverlays()
	Flip(1)
Wend


End



Function CheckControls()

	If (KeyHit(KEY_SPACE))
		camX = 0.0
		camY = 0.0
		camZoomInv = 1.0
	EndIf

	Local camSpeed:Float = CAM_SPEED
	If (KeyDown(KEY_LSHIFT) Or KeyDown(KEY_RSHIFT))
		camSpeed :* CAM_SPEED_MULT * 3.0
	ElseIf (KeyDown(KEY_LCONTROL) Or KeyDown(KEY_RCONTROL))
		camSpeed :* CAM_SPEED_MULT * 2.0
	ElseIf (KeyDown(KEY_LALT) Or KeyDown(KEY_RALT))
		camSpeed :* CAM_SPEED_MULT
	EndIf
	
	If (KeyDown(KEY_A)) 
		camX :- camSpeed * camZoomInv
	ElseIf (KeyDown(KEY_D)) 
		camX :+ camSpeed * camZoomInv
	EndIf
	If (KeyDown(KEY_W)) 
		camY :- camSpeed * camZoomInv
	ElseIf (KeyDown(KEY_S)) 
		camY :+ camSpeed * camZoomInv
	EndIf
	
	If (MouseHit(1))

	Else
		Local mz:Int = MouseZ()
		If (mz < 0)
			camZoomInv = camZoomInv * (ZOOM_SPEED_OUT ^ -mz)
		ElseIf (mz > 0)
			camZoomInv = camZoomInv * (ZOOM_SPEED_IN ^ mz)
		EndIf
	EndIf
	
FlushMouse()
End Function

Function DrawBackground()
	SetAlpha(1.0)
	SetBlend(SOLIDBLEND)
	SetScale(1.0, 1.0)
	SetRotation(0.0)
	SetColor(255, 255, 255)
	
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
	SetColor(255, 255, 255)

	DrawImage(images[currentImage], camX + G_WIDTH2, camY + G_HEIGHT2)
End Function

Function DrawCornerOverlays()
	SetBlend(ALPHABLEND)
	SetScale(1.0, 1.0)
	SetRotation(0.0)
	
	Local color:Int = frameCounter Mod 256
	Local alpha:Float = 1.0 - Abs(((frameCounter * 0.01) Mod 1.0) - 0.5)
	SetColor(frameCounter, 0, 0)
	SetAlpha(alpha)
	
	Local w:Int = ImageWidth(images[currentImage])
	Local h:Int = ImageHeight(images[currentImage])
	Local w2:Float = w / 2.0
	Local h2:Float = h / 2.0
	
	Local x1:Float = G_WIDTH2 + ((-w2 - 5.0) * camZoomInv)  	
	Local x2:Float = G_WIDTH2 + (-w2 * camZoomInv)
	Local x3:Float = G_WIDTH2 + (w2 * camZoomInv)
	Local x4:Float = G_WIDTH2 + ((w2 + 5.0) * camZoomInv)
	Local y1:Float = G_HEIGHT2 + ((-h2 - 5.0) * camZoomInv)
	Local y2:Float = G_HEIGHT2 + (-h2 * camZoomInv)
	Local y3:Float = G_HEIGHT2 + (h2 * camZoomInv)
	Local y4:Float = G_HEIGHT2 + ((h2 + 6.0) * camZoomInv)

	DrawRect(x1, y1, x2 - x1, y3 - y1)
	DrawRect(x1, y3, x3 - x1, y4 - y3)
	DrawRect(x2, y1, x4 - x2, y2 - y1)
	DrawRect(x3, y2, x4 - x3, y4 - y2)

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