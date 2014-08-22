' GPack
' --------
'
' Unif Studios 2014

SuperStrict 

Import BRL.FileSystem
Import BRL.PNGLoader
Import BRL.Retro

Import "..\ugpioutils.bmx"

Const GRID_R:Int = 255
Const GRID_G:Int = 0
Const GRID_B:Int = 0
Const GRID_OUT_R:Int = 128
Const GRID_OUT_G:Int = 0
Const GRID_OUT_B:Int = 0


Global totalRows:Int = 0
Global totalCols:Int = 0
Global globalWidth:Int = 0
Global globalHeight:Int = 0

Global filenameBase:String


Local allFiles:String[] = LoadDir(CurrentDir())
SelectValidPNGs(allFiles)
'PrintArrayString("FILES", validFiles)
'PrintArrayInt("NUMBERS", numbers)

If (Len(validFiles) > 0)
	LoadImages()
	GetOptimalSize()
	GetFileNameBase()
	DrawAndWriteImages()
	WriteHandles()
EndIf

End




Function GetOptimalSize()
	Local numFrames:Int = maxNumber + 1
	Print("total frames " + numFrames)

	Local bestUnusedArea:Int = 2147483647
	Local candidateArea:Int[] = New Int[numFrames]
	Local candidateCols:Int[] = New Int[numFrames]
		
	For Local rows:Int = 1 To numFrames
		Local cols:Int = Ceil(Float(numFrames) / rows)
		Local width:Int = cols * maxWidth
		Local height:Int = rows * maxHeight
		Local unusedArea:Int = (NextPOT(width) * NextPOT(height)) - (numFrames * maxWidth * maxHeight)
		Print ("rows " + rows + " cols " + cols + " = " + width + " x " + height + " -> " + NextPOT(width) + " x " + NextPOT(height) + " ; unused: " + unusedArea)
		If (unusedArea <= bestUnusedArea)
			bestUnusedArea = unusedArea
		EndIf
		candidateArea[rows - 1] = unusedArea
		candidateCols[rows - 1] = cols
	Next
	
	Local bestDistance:Int = 2147483647
	Local bestRows:Int
	Local bestCols:Int
	
	For Local rows:Int = 1 To numFrames
		If (candidateArea[rows - 1] = bestUnusedArea)
			Local distance:Int = Abs(rows - candidateCols[rows - 1])
			If (distance < bestDistance)
				bestDistance = distance
				bestRows = rows
				bestCols = candidateCols[rows - 1]
			EndIf
		EndIf
	Next
	
	totalRows = bestRows
	totalCols = bestCols
	globalWidth = totalCols * maxWidth
	globalHeight = totalRows * maxHeight
	Print ("ROWSxCOLS : " + totalRows + " x " + totalCols + " --> " + globalWidth + " x " + globalHeight)
End Function

Function GetFilenameBase:String()
	Local folder:String = CurrentDir()
	folder = StripAll(folder)
	filenameBase = folder + "_" + (maxNumber + 1) + "-" + maxWidth + "x" + maxHeight 
End Function

Function DrawAndWriteImages()
	Local globalImg:TImage = CreateImage(globalWidth, globalHeight)
	Local testImg:TImage = CreateImage(globalWidth + totalCols + 1, globalHeight + totalRows + 1)
	Graphics(100, 100)
	Local px:TPixmap = LockImage(globalImg)
	DrawImagesIntoPixmap(px)
	SavePixmapPNG(px, filenameBase + ".png", 9)
	UnlockImage(globalImg)	
	px = LockImage(testImg)
	DrawImagesIntoPixmap(px, True)
	SavePixmapPNG(px, filenameBase + ".test.png", 9)
	UnlockImage(testImg)	
End Function

Function WriteHandles()
	Local handlesFile:String = filenameBase + ".cxy"
	If (FileType(handlesFile) = FILETYPE_FILE)
		If (Not DeleteFile(handlesFile))
			Print("WARNING: Cannot update final CXY file, existing and cannot overwrite")
			Return
		EndIf
	ElseIf (FileType(handlesFile) = FILETYPE_DIR)
		Print("WARNING: Cannot create final CXY file, existing as directory")
		Return
	EndIf
	
	If (Not CreateFile(handlesFile))
		Print("WARNING: Cannot create final CXY file")
	Else
		Local stream:TStream = OpenFile(handlesFile, True, True)
		For Local i:Int = 0 To maxNumber
			WriteLine(stream, xHandles[i])
			WriteLine(stream, yHandles[i])
		Next
		CloseStream(stream)
	EndIf	
End Function

Function DrawImagesIntoPixmap(px:TPixmap, drawGrid:Int = False)
	ClearPixmap(px, 0, 0, 0, 0)
	For Local i:Int = 0 To Len(numbers) - 1
		Local row:Int = numbers[i] / totalCols
		Local col:Int = numbers[i] Mod totalCols
		Local x:Int = col * maxWidth
		Local y:Int = row * maxHeight

		' center image in cell
		Local xDisp:Int = (maxWidth - PixmapWidth(pixmaps[i])) / 2
		Local yDisp:Int = (maxHeight - PixmapHeight(pixmaps[i])) / 2
		x = x + xDisp
		y = y + yDisp

		If (drawGrid)
			x = x + col + 1
			y = y + row + 1
		Else
			' adjust handles (only if not drawing grid, ie first pass)
			xHandles[numbers[i]] = xHandles[numbers[i]] + xDisp
			yHandles[numbers[i]] = yHandles[numbers[i]] + yDisp
		EndIf
		DrawPixmapIntoPixmap(pixmaps[i], px, x, y)
	Next
	' FILTER EDGES : will "extend" a border around non-100%-transparent pixels, color-averaging the surrounding pixels but with 0% alpha
	' This will make pixel interpolation look nice on edges
	FilterAlphaEdges(px)
	
	If (drawGrid)
		For Local i:Int = 1 To totalCols
			DrawVerticalLineIntoPixmap(px, i * (maxWidth + 1), GRID_R, GRID_G, GRID_B) 
		Next
		For Local i:Int = 1 To totalRows
			DrawHorizontalLineIntoPixmap(px, i * (maxHeight + 1), GRID_R, GRID_G, GRID_B) 
		Next
		DrawVerticalLineIntoPixmap(px, 0, GRID_OUT_R, GRID_OUT_G, GRID_OUT_B)
		DrawVerticalLineIntoPixmap(px, PixmapWidth(px) - 1, GRID_OUT_R, GRID_OUT_G, GRID_OUT_B)
		DrawHorizontalLineIntoPixmap(px, 0, GRID_OUT_R, GRID_OUT_G, GRID_OUT_B)
		DrawHorizontalLineIntoPixmap(px, PixmapHeight(px) - 1, GRID_OUT_R, GRID_OUT_G, GRID_OUT_B)
	EndIf
End Function

Function DrawPixmapIntoPixmap(px:TPixmap, bigPx:TPixmap, x0:Int, y0:Int)
	For Local y:Int = 0 To PixmapHeight(px) - 1
		For Local x:Int = 0 To PixmapWidth(px) - 1
			WritePixel(bigPx, x0 + x, y0 + y, ReadPixel(px, x, y))
		Next
	Next
End Function

Function DrawHorizontalLineIntoPixmap(px:TPixmap, y:Int, r:Int, g:Int, b:Int)
	Local color:Int = ($FF Shl 24) | (r Shl 16) | (g Shl 8) | b
	For Local x:Int = 0 To PixmapWidth(px) - 1
		WritePixel(px, x, y, color)
	Next
End Function

Function DrawVerticalLineIntoPixmap(px:TPixmap, x:Int, r:Int, g:Int, b:Int)
	Local color:Int = ($FF Shl 24) | (r Shl 16) | (g Shl 8) | b
	For Local y:Int = 0 To PixmapHeight(px) - 1
		WritePixel(px, x, y, color)
	Next
End Function

Function ClearPixmap(px:TPixmap, a:Int, r:Int, g:Int, b:Int)
	Local color:Int = (a Shl 24) | (r Shl 16) | (g Shl 8) | b
	For Local y:Int = 0 To PixmapHeight(px) - 1
		For Local x:Int = 0 To PixmapWidth(px) - 1
			WritePixel(px, x, y, color)
		Next
	Next
End Function

Function FilterAlphaEdges(px:TPixmap)
	Local w:Int = PixmapWidth(px)
	Local h:Int = PixmapHeight(px)
	Local newPixels:Int[] = New Int[w * h]
	Local x:Int, y:Int
	For y = 0 To h - 1
		For x = 0 To w - 1
			If ((ReadPixel(px, x, y) Shr 24) = 0) ' all alpha 0 pixels must be checked
				newPixels[y * w + x] = AveragePixelsNoAlpha(px, x, y)
			EndIf
		Next
	Next
	For y = 0 To h - 1
		For x = 0 To w - 1
			If (newPixels[y * w + x] <> 0)
				WritePixel(px, x, y, newPixels[y * w + x])
			EndIf
		Next
	Next
End Function

Function AveragePixelsNoAlpha:Int(px:TPixmap, x:Int, y:Int)
	Local w:Int = PixmapWidth(px)
	Local h:Int = PixmapHeight(px)
	
	Local argb:Int
	Local a:Float
	Local r:Int, rAcum:Float
	Local g:Int, gAcum:Float
	Local b:Int, bAcum:Float
	Local n:Float
	
	Local x0:Int = x - 1
	Local x1:Int = x + 1
	Local y0:Int = y - 1
	Local y1:Int = y + 1
	
	If (x0 < 0) Then x0 = 0
	If (x1 >= w) Then x1 = w - 1
	If (y0 < 0) Then y0 = 0
	If (y1 >= h) Then y1 = h - 1
	
	For y = y0 To y1
		For x = x0 To x1
			argb = ReadPixel(px, x, y)
			a = argb Shr 24
			If (a > 0)
				a = a / 255.0
				r = (argb Shl 8) Shr 24
				g = (argb Shl 16) Shr 24
				b = (argb Shl 24) Shr 24
				rAcum :+ r * a
				gAcum :+ g * a
				bAcum :+ b * a
				n :+ 1.0
			EndIf
		Next
	Next
	If (n = 0.0) Then Return 0
	r = rAcum / n
	g = gAcum / n
	b = bAcum / n
	Return (r Shl 16) + (g Shl 8) + b ' 0 alpha
	
End Function

Function PrintArrayString(title:String, strs:String[])
	Print(title + ":")
	For Local s:String = EachIn strs
		Print (s)
	Next
	Print("~n")
End Function

Function PrintArrayInt(title:String, ints:Int[])
	Print(title + ":")
	For Local i:Int = EachIn ints
		Print (i)
	Next
	Print("~n")
End Function