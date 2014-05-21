Strict

Private

Import mojo

Public

Class MathUtils Abstract
Public

	Const DECIMALS:Int = 1000	' 1000 = 3 decimals - this should be OK
	Const DECIMALS_F:Float = Float(DECIMALS)

Private

	Const PRE_SIN_COS_DECIMALS:Int = 100
	Const PRE_SIN_COS_DECIMALS_F:Float = Float(PRE_SIN_COS_DECIMALS)
	Const RANGE:Int = 360 * PRE_SIN_COS_DECIMALS
	Const REAL_RANGE:Int = 360 * DECIMALS
	Const CONVERSION_VALUE:Int = DECIMALS / PRE_SIN_COS_DECIMALS
	Const CONVERSION_VALUE_F:float = Float(CONVERSION_VALUE)

	Global preSin:Int[RANGE]
	Global preCos:Int[RANGE]
	
	Global initialized:Bool = False
	
Public

	Function Initialize:Void()
		If (Not initialized)
			random.Seed = Millisecs()
			For Local i:Int = 0 Until (RANGE)
				Local value:Float = Float(i) / PRE_SIN_COS_DECIMALS_F
				preCos[i] = ToDecimalInt(Cos(value))
				preSin[i] = ToDecimalInt(Sin(value))
			Next
			initialized = True
		End If 
	End Function
	
	Function ToDecimalInt:Int(x:Float)
		Return Int(x * DECIMALS_F)
	End Function
	
	Function ToFloat:Float(decimalX:Int)
		Return Float(decimalX) / DECIMALS_F
	End Function
	
	Function PSin:Int(x:Float)
		Local value:Int = Int(x * PRE_SIN_COS_DECIMALS_F)
		value = value Mod RANGE
		If (value < 0) Then value += RANGE
		Return preSin[value]
	End Function
	
	Function PSinF:Float(x:Float)
		Local value:Int = Int(x * PRE_SIN_COS_DECIMALS_F)
		value = value Mod RANGE
		If (value < 0) Then value += RANGE
		Local sin:Int = preSin[value]
		Return ToFloat(sin)
	End Function
	
	Function PCos:Int(x:Float)
		Local value:Int = Int(x * PRE_SIN_COS_DECIMALS_F)
		value = value Mod RANGE
		If (value < 0) Then value += RANGE
		Return preCos[value]
	End Function

	Function PCosF:Float(x:Float)
		Local value:Int = Int(x * PRE_SIN_COS_DECIMALS_F)
		value = value Mod RANGE
		If (value < 0) Then value += RANGE
		Local cos:Int = preCos[value]
		Return ToFloat(cos)
	End Function
		
	Function PSin:Int(x:Int)
		Local value:Int = Int(x * PRE_SIN_COS_DECIMALS_F)
		value = value Mod RANGE
		If (value < 0) Then value += RANGE
		Return preSin[value]
	End Function
	
	Function PCos:Int(x:Int)
		Local value:Int = Int(x * PRE_SIN_COS_DECIMALS_F)
		value = value Mod RANGE
		If (value < 0) Then value += RANGE
		Return preCos[value]
	End Function
	
	Function PSin2:Int(x:Int)
		Return preSin[x / CONVERSION_VALUE]
	End Function
	
	Function PCos2:Int(x:Int)
		Return preCos[x / CONVERSION_VALUE]
	End Function
	
	Function NormalizeAngle:Int(x:Int)
		Local value:Int = x
		value = value Mod REAL_RANGE
		If (value < 0) Then value += REAL_RANGE
		Return value
	End Function
	
	Function NormalizedAngleToFloat:Float(x:Int)
		Return Float(x) / DECIMALS_F
	End Function
	
	Function MinInArrayInt:Int(arrayInt:Int[])
		Local xMin:Int = arrayInt[0]
		For Local i:Int = 1 To (arrayInt.Length - 1)
			If (arrayInt[i] < xMin) Then xMin = arrayInt[i]
		End For
		Return xMin
	End Function
	
	Function MaxInArrayInt:Int(arrayInt:Int[])
		Local xMax:Int = arrayInt[0]
		For Local i:Int = 1 To (arrayInt.Length - 1)
			If (arrayInt[i] > xMax) Then xMax = arrayInt[i]
		End For
		Return xMax
	End Function
	
	Function RndSmallInt:Int(min:Int, max:Int)
		Local fixedMin:Int = min, fixedMax:Int = max
		If (min < 0)
			fixedMin = 0
			fixedMax = max - min
		End If
		Local rnd:Float = Rnd(fixedMin * 1000.0, (fixedMax + 1) * 1000.0) / 1000.0
		If (min < 0)
			rnd += Float(min)
		End If
		Return rnd
	End Function
	
End Class