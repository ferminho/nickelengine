' GPack
' --------
'
' Unif Studios 2014

SuperStrict 

Import BRL.FileSystem
Import BRL.PNGLoader
Import BRL.Retro


Const MAX_GRAPHICS:Int = 4096

Const GRID_R:Int = 255
Const GRID_G:Int = 0
Const GRID_B:Int = 0
Const GRID_OUT_R:Int = 128
Const GRID_OUT_G:Int = 0
Const GRID_OUT_B:Int = 0

Global validFiles:String[] = New String[0]
Global numbers:Int[] = New Int[0]
Global pixmaps:TPixmap[]
Global xHandles:Int[]
Global yHandles:Int[]

Global maxWidth:Int = 0
Global maxHeight:Int = 0
Global maxNumber:Int = 0

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



Function SelectValidPNGs(files:String[])
	Local number:Int
	For Local file:String = EachIn files
		If (Lower(ExtractExt(file)) = "png")
			number = GetNumber(StripAll(file))
			If (number > -1)
				If (ArrayContainsInt(numbers, number))
					Print("WARNING: repeated number " + number + " for file " + file)
				ElseIf (number >= MAX_GRAPHICS)
					Print("WARNING: graphic " + number + " out of limits for file " + file)
				Else
					If (number > maxNumber) Then maxNumber = number
					validFiles = validFiles[..Len(validFiles) + 1]
					validFiles[Len(validFiles) - 1] = file
					numbers = numbers[..Len(numbers) + 1]
					numbers[Len(numbers) - 1] = number
				EndIf
			EndIf
		EndIf
	Next
End Function

Function GetNumber:Int(s:String)
	If (Len(s) = 0)
		Return -1
	EndIf
	For Local i:Int = 0 To Len(s) - 1
		If (s[i] < Asc("0") Or s[i] > Asc("9"))
			Return -1
		EndIf
	Next
	Return Int(s)
End Function

Function ArrayContainsInt:Int(ints:Int[], i:Int)
	For Local j:Int = EachIn ints
		If (j = i)
			Return True
		EndIf
	Next
	Return False
End Function

Function LoadImages()
	AutoMidHandle(False)
	pixmaps = New TPixmap[Len(validFiles)]
	xHandles = New Int[maxNumber + 1]
	yHandles = New Int[maxNumber + 1]
	Local i:Int = 0
	For Local file:String = EachIn validFiles
		Try
			pixmaps[i] = LoadPixmap(file)
			If (pixmaps[i] = Null)
				Throw "Null"
			EndIf
			SetHandles(i, file)
			If (PixmapWidth(pixmaps[i]) > MaxWidth) Then MaxWidth = PixmapWidth(pixmaps[i])
			If (PixmapHeight(pixmaps[i]) > MaxHeight) Then MaxHeight = PixmapHeight(pixmaps[i])
		Catch e:String
			RuntimeError("Cannot load image " + file)
		EndTry
		i = i + 1
	Next
End Function

Function SetHandles(i:Int, file:String)
	Local x:Int = 0
	Local y:Int = 0
	Local handlesFile:String = StripExt(file) + ".cxy"
	If (FileType(handlesFile) = FILETYPE_FILE)
		Local stream:TStream = OpenFile(handlesFile, True, False)
		If (stream = Null)
			Print("WARNING: cannot open file " + handlesFile)
		Else
			x = GetNumber(ReadLine(stream))
			y = GetNumber(ReadLine(stream))
			If (x < 0 Or y < 0)
				x = 0
				y = 0
				Print("WARNING: file " + handlesFile + " bad format")
			EndIf
			CloseStream(stream)
		EndIf
	ElseIf (FileType(handlesFile) = FILETYPE_DIR)
		Print("WARNING: cannot open file " + handlesFile + " since it's a directory")
	EndIf
	xHandles[numbers[i]] = x
	yHandles[numbers[i]] = y
End Function

Function GetOptimalSize()
	Local numFrames:Int = maxNumber + 1
	Print("total frames " + numFrames)

	Local bestUnusedArea:Int = 2147483647
	Local candidateArea:Int[] = New Int[numFrames]
	Local candidateCols:Int[] = New Int[numFrames]
		
	For Local rows:Int = 1 To numFrames
		Local cols:Int = Ceil(Float(numFrames) / rows)
		Local width:Int = NextPOT(cols * maxWidth)
		Local height:Int = NextPOT(rows * maxHeight)
		Local unusedArea:Int = (width * height) - (numFrames * maxWidth * maxHeight)
		Print ("rows " + rows + " cols " + cols + " = " + width + " x " + height + " ; unused: " + unusedArea)
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
	globalWidth = NextPOT(totalCols * maxWidth)
	globalHeight = NextPOT(totalRows * maxHeight)
	
	Print ("ROWSxCOLS : " + totalRows + " x " + totalCols + " --> " + globalWidth + " x " + globalHeight)
End Function

Function GetFilenameBase:String()
	Local folder:String = CurrentDir()
	folder = StripAll(folder)
	filenameBase = folder + "_" + maxWidth + "x" + maxHeight
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

Function NextPOT:Int(i : Int)
	Local n:Int = 1
	While (n < i)
		n = n * 2
	Wend
	Return n
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