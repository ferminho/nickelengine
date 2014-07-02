' Unif Graphics Pack (v2)
' --------
'
' Unif Studios 2014

SuperStrict 

Import BRL.Retro
Import BRL.PNGLoader

Public

Type UGP

	Field image:TImage = Null
	Field frames:Int = 0
	Field frameWidth:Int = 0
	Field frameHeight:Int = 0
	Field xHandles:Int[] = New Int[0]
	Field yHandles:Int[] = New Int[0]
	
	Field pngFile:String
	Field cxyFile:String

	Function LoadUGP:UGP(pngFile:String, flags:Int=FILTEREDIMAGE | MIPMAPPEDIMAGE)
	
		Local ugp:UGP = New UGP
		ugp.pngFile = pngFile
		
		ugp = LoadImageForUGP(ugp, flags)
		If (ugp = Null) Then Return Null
		ugp = LoadCXYForUGP(ugp)
		If (ugp = Null) Then Return Null
		
		Return ugp
	End Function

	Method SaveCXY:Int(cxyFile:String = Null)
		If (cxyFile = Null) Then cxyFile = Self.cxyFile
		If (FileType(cxyFile) = FILETYPE_DIR) Then Return False
		If (FileType(cxyFile) = FILETYPE_FILE)
			If (Not DeleteFile(cxyFile)) Then Return False
		EndIf
		If (Not CreateFile(cxyFile)) Then Return False
		
		Local stream:TStream
		Try
			stream = OpenFile(cxyFile)
			For Local i:Int = 0 To frames - 1
				WriteLine(stream, xHandles[i])
				WriteLine(stream, yHandles[i])
			Next
			Return True
		Catch ex:String
			If (stream <> Null) Then CloseStream(stream)
			
		EndTry

		Return False
	End Method

	Method Unload()
		image = Null
		xHandles = xHandles[0..0]
		yHandles= yHandles[0..0]
		pngFile = ""
		cxyFile = ""
		frames = 0
	End Method
End Type


Private


Function LoadImageForUGP:UGP(ugp:UGP, flags:Int)
	If (FileType(ugp.pngFile) <> FILETYPE_FILE) Then Return Null

	Local baseName:String = StripExt(ugp.pngFile)
	Local underScorePos:Int = Instr(baseName, "_")
	If (underScorePos < 0) Then Return Null
	Local i:Int = underScorePos
	While (i > 0)
		i = Instr(baseName, "_", i + 1)
		If (i > 0) Then underScorePos = i
	Wend
	
	Local dashPos:Int = Instr(baseName, "-", underScorePos)
	If (dashPos < 0) Then Return Null
	ugp.frames = Int(Mid(baseName, underScorePos + 1, dashPos - (underScorePos + 1)))
	
	Local xPos:Int = Instr(baseName, "x", dashPos)
	If (xPos < 0) Then Return Null
	ugp.frameWidth = Int(Mid(baseName, dashPos + 1, xPos - (dashPos + 1)))
	ugp.frameHeight = Int(Mid(baseName, xPos + 1, Len(baseName) - (xPos + 1)))
	If (ugp.frameWidth <= 0 Or ugp.frameHeight <= 0) Then Return Null

	ugp.Image = LoadAnimImage(ugp.pngFile, ugp.frameWidth, ugp.frameHeight, 0, ugp.frames, flags)
	If (ugp.image = Null) Then Return Null
	Return ugp
End Function

Function LoadCXYForUGP:UGP(ugp:UGP)
	ugp.cxyFile = StripExt(ugp.pngFile) + ".cxy"
	If (FileType(ugp.cxyFile) <> FILETYPE_FILE) Then Return DefaultCXYForUGP(ugp)
	
	Local stream:TStream = Null
	Try
		stream = OpenStream(ugp.cxyFile, True, False)
		If (stream = Null) Then Return DefaultCXYForUGP(ugp)
		
		For Local i:Int = 0 To ugp.frames - 1
			Local line:String = ReadLine(stream)
			If (line = Null Or Len(Trim(line)) = 0 Or Int(line) < 0) Then Return DefaultCXYForUGP(ugp)
			ugp.xHandles[i] = Int(line)
			line = ReadLine(stream)
			If (line = Null Or Len(Trim(line)) = 0 Or Int(line) < 0) Then Return DefaultCXYForUGP(ugp)
			ugp.yHandles[i] = Int(line)
		Next
		
		CloseStream(stream)
		Return ugp
	Catch ex:String
		If (stream <> Null) Then CloseStream(stream)
	EndTry
	Return DefaultCXYForUGP(ugp)
End Function

Function DefaultCXYForUGP:UGP(ugp:UGP)
	ugp.xHandles = New Int[ugp.frames]	
	ugp.yHandles = New Int[ugp.frames]	
	For Local i:Int = 0 To ugp.frames - 1
		ugp.xHandles[i] = 0
		ugp.yHandles[i] = 0
	Next
End Function