' Unif Graphics Pack (v2)
' --------
'
' Unif Studios 2014

SuperStrict 

Import BRL.Retro
Import BRL.PNGLoader

Public

Const MAX_GRAPHICS:Int = 4096

Global validFiles:String[] = New String[0]
Global numbers:Int[] = New Int[0]
Global pixmaps:TPixmap[]
Global xHandles:Int[]
Global yHandles:Int[]

Global maxWidth:Int = 0
Global maxHeight:Int = 0
Global maxNumber:Int = 0

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

Function SetHandles(i:Int, file:String, cxyEditMode:Int = False)
	Local x:Int = 0
	Local y:Int = 0
	Local handlesFile:String = StripExt(file) + ".cxy"
	If (FileType(handlesFile) = FILETYPE_FILE)
		Local stream:TStream = OpenFile(handlesFile, True, False)
		If (stream = Null)
			If (Not cxyEditMode) Then Print("WARNING: cannot open file " + handlesFile)
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
		If (cxyEditMode)
			RuntimeError("Cannot open file " + handlesFile + " since it's a directory")
		Else
			Print("WARNING: cannot open file " + handlesFile + " since it's a directory")
		EndIf
	EndIf
	xHandles[numbers[i]] = x
	yHandles[numbers[i]] = y
End Function

Function NextPOT:Int(i : Int)
	Local n:Int = 1
	While (n < i)
		n = n * 2
	Wend
	Return n
End Function
