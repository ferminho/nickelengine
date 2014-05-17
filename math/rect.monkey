Strict

Private

Import mojo

Import NickelEngine.math.mathutils

Public

Class Rect
Private

	Global tempX:Float
	Global tempY:Float
	Global tempSin:Float
	Global tempCos:Float
	
Public
	
	Field x1:Float
	Field y1:Float
	Field x2:Float
	Field y2:Float
	Field x3:Float
	Field y3:Float
	Field x4:Float
	Field y4:Float
	
	Method New()
		SetBounds(0.0, 0.0, 0.0, 0.0)
	End Method
	
	Method New(x1:Float, y1:Float, x2:Float, y2:Float)
		SetBounds(x1, y1, x2, y2)
	End Method

	Method New(x1:Float, y1:Float, x2:Float, y2:Float,
				x3:Float, y3:Float, x4:Float, y4:Float)
		SetBounds(x1, y1, x2, y2, x3, y3, x4, y4)
	End Method
	
	Method Copy:Void(rect:Rect)
		x1 = rect.x1
		y1 = rect.y1
		x2 = rect.x2
		y2 = rect.y2
		x3 = rect.x3
		y3 = rect.y3
		x4 = rect.x4
		y4 = rect.y4
	End Method
	
	Method SetBounds:Void(x1:Float, y1:Float, x2:Float, y2:Float)
		SetBounds(x1, y1, x2, y1, x2, y2, x1, y2) ' axis aligned
	End Method

	Method SetBounds:Void(x1:Float, y1:Float, x2:Float, y2:Float,
				x3:Float, y3:Float, x4:Float, y4:Float)
		Self.x1 = x1
		Self.y1 = y1
		Self.x2 = x2
		Self.y2 = y2
		Self.x3 = x3
		Self.y3 = y3
		Self.x4 = x4
		Self.y4 = y4
	End Method
	
	Method Translate:Void(x:Float, y:Float)
		x1 += x
		y1 += y	
		x2 += x
		y2 += y	
		x3 += x
		y3 += y	
		x4 += x
		y4 += y	
	End Method
	
	Method Scale:Void(scale:Float)
		Scale(scale, scale)
	End Method
	
	Method Scale:Void(scaleX:Float, scaleY:Float)
		x1 *= scaleX
		y1 *= scaleY
		x2 *= scaleX
		y2 *= scaleY
		x3 *= scaleX
		y3 *= scaleY
		x4 *= scaleX
		y4 *= scaleY
	End Method
	
	Method Rotate:Void(angle:Float)
		If (angle Mod 360.0 <> 0.0)
			' x' = x * cosA - y * sinA 'but Ys are inverted! so signs will change
			' y' = x * sinA + y * cosA
			tempCos = Float(MathUtils.PCos(angle)) / MathUtils.DECIMALS_F
			tempSin = -Float(MathUtils.PSin(angle)) / MathUtils.DECIMALS_F
			tempX = x1
			x1 = tempX * tempCos - y1 * tempSin
			y1 = tempX * tempSin + y1 * tempCos
			tempX = x2
			x2 = tempX * tempCos - y2 * tempSin
			y2 = tempX * tempSin + y2 * tempCos
			tempX = x3
			x3 = tempX * tempCos - y3 * tempSin
			y3 = tempX * tempSin + y3 * tempCos
			tempX = x4
			x4 = tempX * tempCos - y4 * tempSin
			y4 = tempX * tempSin + y4 * tempCos
		End If
	End Method
	
	Method ScaleAbsolute:Void(size:Float)
		ScaleAbsolute(size, size)
	End Method
	
	Method ScaleAbsolute:Void(sizeX:Float, sizeY:Float)
		x1 -= sizeX
		y1 -= sizeY
		x2 += sizeX
		y2 -= sizeY
		x3 += sizeX
		y3 += sizeY
		x4 -= sizeX
		y4 += sizeY
	End Method
	
	Method ConvertToAABoundingBox:Void()
		Local minX:Float = Min(Min(Min(x1, x2), x3), x4)
		Local minY:Float = Min(Min(Min(y1, y2), y3), y4)
		Local maxX:Float = Max(Max(Max(x1, x2), x3), x4)
		Local maxY:Float = Max(Max(Max(y1, y2), y3), y4)
		SetBounds(minX, minY, maxX, maxY)
	End Method
	
	Method Intersects:Bool(other:Rect, bothAreAA:Bool = False)
		If (other <> Null)
			If (bothAreAA)
				'Remember standard for AA rects is upper left (x1,y1) lower right (x3,y3)
				If ( (x1 >= other.x1 And x1 <= other.x3) Or
					 (x1 < other.x1 And x3 >= other.x1))
					If ( (y1 >= other.y1 And y1 <= other.y3) Or
						 (y1 < other.y1 And y3 >= other.y1))
						Return True
					End If
				End If
			Else
				'TODO
				Error("woops! Rect.Intersects not implemented for non AA")
			End If
		End If
		Return False
	End Method
	
	Method Draw:Void(r:Float, g:Float, b:Float)
		PushMatrix()
		SetAlpha(1.0)
		SetBlend(AlphaBlend)
		SetColor(r, g, b)
		DrawLine(x1, y1, x2, y2)
		DrawLine(x2, y2, x3, y3)
		DrawLine(x3, y3, x4, y4)
		DrawLine(x4, y4, x1, y1)
		PopMatrix()
	End Method
	
	Method PrintCoords:Void()
		Print("[ (" + Int(x1) + ", " + Int(y1) + ") (" + Int(x2) + ", " + Int(y2) + ") (" + Int(x3) + ", " + Int(y3) + ") (" + Int(x4) + ", " + Int(y4) + ") ]")
	End Method
End Class