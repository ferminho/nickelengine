Strict

Private

Import mojo
Import brl.filepath
Import brl.stringutils

Public

Class UGP
Private

	Field image:Image
	Field frameWidth:Int
	Field frameHeight:Int
	Field xHandles:Int[]
	Field yHandles:Int[]
	
	Field pngFile:String
	Field cxyFile:String
		
Public

	Method New(pngFile:String, mandatoryCXY:Bool = False, flags:Int = DefaultFlags)
		Self.pngFile = pngFile
		If (Not ParseLoadImage(flags))
			Throw "Image for UGP " + pngFile + " cannot be loaded"
		End If
		If (Not ParseLoadCXY())
			If (mandatoryCXY) Then Throw "CXY for UGP " + pngFile + " cannot be loaded"
			xHandles = New Int[image.Frames]
			yHandles = New Int[image.Frames]
		End If
	End Method

	Method DrawImage:Void(frame:Int, x:Float, y:Float)
		image.SetHandle(xHandles[frame], yHandles[frame])
		DrawImage(image, x, y, frame)
	End Method
	
	Method DrawImage:Void(frame:Int, x:Float, y:Float, rotation:Float, scaleX:Float, scaleY:Float)
		image.SetHandle(xHandles[frame], yHandles[frame])
		DrawImage(image, x, y, rotation, scaleX, scaleY, frame)
	End Method	
	
	Method GetFrames:Int()
		Return image.Frames()
	End Method

Private

	Method ParseLoadImage:Bool(flags:Int)
		Local baseName:String = StripExt(pngFile)
		Local underScorePos:Int = baseName.Find("_")
		If (underScorePos < 0) Then Return False
		
		Local i:Int = underScorePos
		While (i > 0)
			i = baseName.Find("_", i + 1)
			If (i > 0) Then underScorePos = i
		Wend
	
		Local dashPos:Int = baseName.Find("-", underScorePos)
		If (dashPos < 0) Then Return False
		Local frames:Int = Int(baseName[underScorePos + 1..dashPos])
	
		Local xPos:Int = baseName.Find("x", dashPos)
		If (xPos < 0) Then Return False
		frameWidth = Int(baseName[dashPos + 1..xPos])
		frameHeight = Int(baseName[xPos + 1..])
		If (frameWidth <= 0 Or frameHeight <= 0) Then Return False

		image = LoadImage(pngFile, frameWidth, frameHeight, frames, flags)
	End Method
	
	Method ParseLoadCXY:Bool()
		cxyFile = StripExt(ugp.pngFile) + ".cxy"
		
		Local cxyString:String = LoadString(cxyFile)
		
		If (cxyString = Null Or cxyString.Trim().Length = 0) Then Return False

		cxyString.Replace("~r~n", "~n")
		Local lines:String[] = cxyString.Split("~n")
		
		If (lines.Length <> image.Frames * 2) Then Return False

		xHandles = New Int[image.Frames]
		yHandles = New Int[image.Frames]

		Local line:String
		For Local i:Int = 0 To image.Frames() - 1
			line = lines[i * 2]
			If (line = Null Or line.Trim().Length = 0) Then Return False
			xHandles[i] = Int(line.Trim())
			line = lines[(i * 2) + 1]
			If (line = Null Or line.Trim().Length = 0) Then Return False
			yHandles[i] = Int(line.Trim())
		Next
			
			CloseStream(stream)
			Return ugp
		Catch ex:String
			If (stream <> Null) Then CloseStream(stream)
		EndTry
	Return DefaultCXYForUGP(ugp)
	
	End Method
End Class