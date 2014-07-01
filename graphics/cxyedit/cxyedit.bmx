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
Const BORDER_MARGIN:Float = 7.0

Global camX:Float = 0.0
Global camY:Float = 0.0
Global camZoomInv:Float = 1.0
Global background:TImage
Global bgXD:Float = 0.0
Global bgYD:Float = 0.0

Global currentImage:Int = 0
Global currentPixelX:Int = -1
Global currentPixelY:Int = -1

Global frameCounter:Int = 0

Global allFiles:String[] = LoadDir(CurrentDir())

SelectValidPNGs(allFiles)

If (Len(validFiles) <= 0)
	RuntimeError("No images found")
EndIf

Graphics(G_WIDTH, G_HEIGHT, 0, 60, GRAPHICS_BACKBUFFER)
LoadImages(True, True)
CreateBackground()
SelectImage(0)

While Not (KeyHit(KEY_ESCAPE))
	frameCounter :+ 1
	Cls()
	UpdateMouseXY()
	CheckControls()
	DrawBackground()
	DrawCurrentImage()
	DrawCornerOverlays()
	DrawCXYOverlays()
	DrawImageName()
	DrawXY()
	Flip(1)
Wend


End


Function SelectImage(num:Int)
	currentImage = num
	camX = 0.0
	camY = 0.0
	Local w:Float = ImageWidth(images[num])
	Local h:Float = ImageHeight(images[num])
	Local r:Float = Max(w / (G_WIDTH - BORDER_MARGIN), h / (G_HEIGHT - BORDER_MARGIN))
	camZoomInv = 1.0 / r
End Function

Function UpdateMouseXY()
	Local w:Int = ImageWidth(images[currentImage])
	Local h:Int = ImageHeight(images[currentImage])
	Local w2:Float = w / 2.0
	Local h2:Float = h / 2.0
	Local x0:Float = G_WIDTH2 + camX + (-w2 * camZoomInv)
	Local y0:Float = G_HEIGHT2 + camY + (-h2 * camZoomInv)
	Local x:Float = (MouseX() - x0) / camZoomInv
	Local y:Float = (MouseY() - y0) / camZoomInv
	If (x >= 0.0 And x < w And y >= 0.0 And y < h)
		currentPixelX = x
		currentPixelY = y
	Else
		currentPixelX = -1
		currentPixelY = -1
	EndIf
End Function

Global frameDelayCounter:Int = 0
Function CheckControls()

	If (KeyDown(KEY_Q) And currentImage > 0)
		If (frameDelayCounter= 0 Or frameDelayCounter> 60)
			SelectImage(currentImage - 1)
		EndIf
		frameDelayCounter :+ 1
	ElseIf (KeyDown(KEY_E) And currentImage < Len(images) - 1)
		If (frameDelayCounter = 0 Or frameDelayCounter > 60)
			SelectImage(currentImage + 1)
		EndIf
		frameDelayCounter :+ 1
	Else
		frameDelayCounter = 0
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
			camX :+ camSpeed * camZoomInv
		ElseIf (KeyDown(KEY_D)) 
			camX :- camSpeed * camZoomInv
		EndIf
		If (KeyDown(KEY_W)) 
			camY :+ camSpeed * camZoomInv
		ElseIf (KeyDown(KEY_S)) 
			camY :- camSpeed * camZoomInv
		EndIf
		
		If (MouseHit(1))
			If (currentPixelX > -1 And currentPixelY > -1)
				xHandles[currentImage] = currentPixelX
				yHandles[currentImage] = currentPixelY
				SaveCurrentHandle()
			EndIf
		Else
			Local mz:Int = MouseZ()
			If (mz < 0)
				camZoomInv = camZoomInv * (ZOOM_SPEED_OUT ^ -mz)
			ElseIf (mz > 0)
				camZoomInv = camZoomInv * (ZOOM_SPEED_IN ^ mz)
			EndIf
		EndIf
	
		FlushMouse()
	EndIf
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
	SetColor(color, 0, 0)
	SetAlpha(alpha)
	
	Local w:Int = ImageWidth(images[currentImage])
	Local h:Int = ImageHeight(images[currentImage])
	Local w2:Float = w / 2.0
	Local h2:Float = h / 2.0
	
	Local x1:Float = G_WIDTH2 + camX + (-w2 * camZoomInv) - BORDER_MARGIN
	Local x2:Float = G_WIDTH2 + camX + (-w2 * camZoomInv)
	Local x3:Float = G_WIDTH2 + camX + (w2 * camZoomInv)
	Local x4:Float = G_WIDTH2 + camX + (w2 * camZoomInv)  + BORDER_MARGIN
	Local y1:Float = G_HEIGHT2 + camY + (-h2 * camZoomInv) - BORDER_MARGIN
	Local y2:Float = G_HEIGHT2 + camY + (-h2 * camZoomInv)
	Local y3:Float = G_HEIGHT2 + camY + (h2 * camZoomInv)
	Local y4:Float = G_HEIGHT2 + camY + (h2 * camZoomInv) + BORDER_MARGIN

	DrawRect(x1, y1, x2 - x1, y3 - y1)
	DrawRect(x1, y3, x3 - x1, y4 - y3)
	DrawRect(x2, y1, x4 - x2, y2 - y1)
	DrawRect(x3, y2, x4 - x3, y4 - y2)
End Function

Function DrawCXYOverlays()
	SetBlend(ALPHABLEND)
	SetScale(camZoomInv, camZoomInv)
	SetRotation(0.0)
	
	Local color:Int = Abs(((frameCounter * 0.05) Mod 512.0) - 256.0)
	Local alpha:Float = Abs(((frameCounter * 0.05) Mod 2.0) - 1.0)
	SetColor(color, color, 0)
	SetAlpha(alpha)
	
	Local w:Int = ImageWidth(images[currentImage])
	Local h:Int = ImageHeight(images[currentImage])
	Local w2:Float = w / 2.0
	Local h2:Float = h / 2.0
	
	Local x:Float = G_WIDTH2 + camX + (-w2 + xHandles[currentImage]) * camZoomInv 
	Local y:Float = G_HEIGHT2 + camY + (-w2 + yHandles[currentImage]) * camZoomInv 

	DrawRect(0.0, y, G_WIDTH, 1.0)
	DrawRect(x, 0.0, 1.0, G_HEIGHT)
	SetColor(0, 0, 0)
End Function

Function DrawImageName()
	SetBlend(ALPHABLEND)
	SetRotation(0.0)
	Local alpha:Float = Abs(((frameCounter * 0.025) Mod 2.0) - 1.0)
	SetAlpha(alpha)
	SetColor(0, 0, 0)
	SetScale(5.1, 5.1)
	DrawText(StripDir(validFiles[currentImage]), 20.0, 20.0)
	SetColor(255, 255, 255)
	SetScale(5.0, 5.0)
	DrawText(StripDir(validFiles[currentImage]), 20.0, 20.0)
End Function

Function DrawXY()
	Local xText:String
	Local yText:String
	If (currentPixelX > -1 And currentPixelY > -1)
		xText = currentPixelX
		yText = currentPixelY
	Else
		xText = "---"
		yText = "---"
	EndIf
	
	SetBlend(ALPHABLEND)
	SetRotation(0.0)
	Local alpha:Float = Abs(((frameCounter * 0.03) Mod 2.0) - 1.0)
	SetAlpha(alpha)
	SetColor(0, 0, 0)
	SetScale(4.1, 4.1)
	DrawText(xText, G_WIDTH - 150.0, G_HEIGHT - 100.0)
	SetColor(255, 0, 0)
	SetScale(4.0, 4.0)
	DrawText(xText, G_WIDTH - 150.0, G_HEIGHT - 100.0)
	SetColor(0, 0, 0)
	SetScale(4.1, 4.1)
	DrawText(yText, G_WIDTH - 150.0, G_HEIGHT - 50.0)
	SetColor(255, 0, 0)
	SetScale(4.0, 4.0)
	DrawText(yText, G_WIDTH - 150.0, G_HEIGHT - 50.0)
End Function

Function SaveCurrentHandle()
	Local file:String = validFiles[currentImage]
	Local handlesFile:String = StripExt(file) + ".cxy"
	If (FileType(handlesFile) = FILETYPE_DIR) Then RuntimeError("Cannot update " + handlesFile + " since file exists as a directory!")
	If (FileType(handlesFile) = FILETYPE_FILE And Not DeleteFile(handlesFile)) Then RuntimeError ("Cannot update " + handlesFile + " since original file can't be deleted")
	If (Not CreateFile(handlesFile)) Then RuntimeError ("Cannot create " + handlesFile)
	Local stream:TStream = OpenFile(handlesFile, False, True)
	WriteLine(stream, xHandles[currentImage])
	WriteLine(stream, yHandles[currentImage])
	CloseStream(stream)
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