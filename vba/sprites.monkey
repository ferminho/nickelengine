Strict


Private

Import mojo.mojo
Import mojo.graphics

Import util.arrayutils

Public



' Sprite collection or package, formerly known as VGP
Class SpritePack
Private

	Const IMAGE_FLAGS:Int = Image.MidHandle | Image.XYPadding | Image.BilinearFiltering
	Const IMAGE_FILENAME_DIGITS:Int = 4
	Const CPLIST_FILE:String = "vgpcps.txt"
	Const CPLIST_START_SPRITE:String = "["
	Const CPLIST_END_SPRITE:String = "]"
	Const CPLIST_SPRITE_DELIM:String = ":"
	Const CPLIST_START_CP:String = "{"
	Const CPLIST_END_CP:String = "}"
	Const CPLIST_COORD_DELIM:String = ","

	Global emptySprite:Sprite = Null
		
	Field sprites:Sprite[] =[]
	
	Public
	
	Method New()
		' lazy init emptySprite
		If(emptySprite = Null)
			Local img:Image = LoadImage(Constants.PATH_GFX + Constants.NAME_EMPTY_SPRITE)
			If (img = Null)
				Print("CANNOT LOAD EMPTY SPRITE")
			End If
			emptySprite = New Sprite(img)
		End If
	End Method
	
	Method GetSprite:Sprite(n:Int)
		' No safe checks, for the sake of speed
		Return sprites[n]
	End Method
	
	Method GetSize:Int()
		If(sprites = null)
			Return 0
		Else
			Return sprites.Length
		End If
	End Method
	
	Method Destroy:Void()
		if(sprites <> Null)
			For Local sprite:Sprite = EachIn sprites
				sprite.Destroy()
			End For
			sprites = Null
		End If
	End Method

	' packSize: size of the resulting package (loaded sprites will be indexed from 0 to packSize -1)
	Function LoadFolder:SpritePack(folder:String, packSize:Int)
		Local result:SpritePack = Null
	
		Local img:Image = Null
		Local files:String[] =[]
		
		Local lastChar:String = folder[folder.Length - 1 ..]
		If (lastChar = "/" Or lastChar = "\")
			folder = folder[ .. folder.Length - 1]
		End If
		folder = folder + "/"
		Print("Loading SpritePack from " + folder)
		result = New SpritePack()
			
		result.sprites = New Sprite[packSize]
		For Local i:Int = 0 To(packSize - 1)
			img = LoadImage(folder + GetImageFilename(i))
			If (img <> Null)
				result.sprites[i] = New Sprite(img)
			Else
				result.sprites[i] = emptySprite
			End If
		End For

		result.LoadCPs(folder + CPLIST_FILE, result.sprites)
		
		Return result
	
	End Function
	
Private

	
	Function GetImageFilename:String(i:Int)
		Local filename:String = ""
	
		While (i > 0)
			filename = (i Mod 10) + filename
			i = i / 10
		End While
	
		While (filename.Length < IMAGE_FILENAME_DIGITS)
			filename = "0" + filename
		Wend
	
		Return(filename + ".png")
	End Function

	Method LoadCPs:Void(cpFile:String, sprites:Sprite[])
		Local contents:String = LoadString(cpFile)
		Local posStart:Int = contents.Find(CPLIST_START_SPRITE)
		Local posEnd:Int = 0
		Local posDelim:Int = 0
		Local spriteNumber:Int = 0
		Local spriteContent:String = ""
		Local controlPointsContent:String[] =[]
		Local controlPointContent:String = ""
		Local controlPoints:ControlPoint[] =[]
		Local coords:String[] =[]
		Local cpNumber:Int = 0
		
		While(posStart <> - 1)
			posEnd = contents.Find(CPLIST_END_SPRITE, posStart + 1)
			If(posEnd <> - 1)
				spriteContent = contents[posStart + 1 .. posEnd]
				posDelim = spriteContent.Find(CPLIST_SPRITE_DELIM)
				spriteNumber = Int(spriteContent[ .. posDelim])
				If (spriteNumber >= 0 And spriteNumber < sprites.Length)
					controlPointsContent = spriteContent[posDelim + 1 ..].Split(CPLIST_END_CP)
					If(controlPointsContent.Length > 1)
						controlPoints = New ControlPoint[controlPointsContent.Length - 1]
						cpNumber = 0
						For controlPointContent = EachIn controlPointsContent
							If (controlPointContent.Length > 0 And controlPointContent.Find(CPLIST_START_CP) = 0)
								controlPointContent = controlPointContent[1 ..]
								coords = controlPointContent.Split(CPLIST_COORD_DELIM)
								If (coords.Length = 2)
									controlPoints[cpNumber] = New ControlPoint(Int(coords[0]), Int(coords[1]))
									If (cpNumber = 0)
										' Set image handle
										If (sprites[spriteNumber] <> emptySprite)
											sprites[spriteNumber].image.SetHandle(
												controlPoints[cpNumber].x,
												controlPoints[cpNumber].y)
										End If
									End If
								End If
							End If
							cpNumber = cpNumber + 1
						End For
						sprites[spriteNumber].SetControlPoints(controlPoints)
					End If
				End If
				
				posStart = contents.Find(CPLIST_START_SPRITE, posEnd)
			Else
				posStart = -1 'end
			End If
		End While
	End Method
	
End Class



' Sprite, an image with some extra attributes
Class Sprite
Private
	
	Field controlPoints:ControlPoint[] =[]
	Field image:Image = Null
	
Public

	Method New(image:Image)
		Self.image = image
	End Method
	
	Method GetImage:Image()
		Return image
	End Method
	
	Method GetControlPoint:ControlPoint(n:Int)
		Return controlPoints[n]
	End Method

	Method GetControlPointsNumber:Int()
		If (controlPoints.Length = 0)
			Return 0
		End If
		Return controlPoints.Length
	End Method
	
	Method SetControlPoints:Void(newControlPoints:ControlPoint[])
		controlPoints = ArrayUtils<ControlPoint>.Clone(newControlPoints)
	End Method
	
	Method Destroy:Void()
		If(image <> Null)
			image.Discard
			image = null
			controlPoints =[]
		End If
	End Method
	
End Class


' Control point: a point of interest inside a sprite
Class ControlPoint

Private

	Field x:Int = 0
	Field y:Int = 0
	
Public
	
	Method New(x:Int, y:Int)
		Self.x = x
		Self.y = y
	End Method
	
	Method GetX:Int()
		Return x
	End Method
	
	Method GetY:Int()
		Return y
	End Method

End Class
