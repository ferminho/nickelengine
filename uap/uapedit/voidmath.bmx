' VoiD.net
' --------
'
' Math utils module 
' -----------
' Started 25-08-2005
' Latest Version 29-01-2005
'
' Unif Studios 2005/2007


Strict

'Framework BRL.Max2D


Public
' ---------------------


' UTIL FUNCTIONS


' PRE : points must form the main diagonal (Upper Left, Lower Right) . NO other points
Function RectsInt:Int (X1a:Int, Y1a:Int, X2a:Int, Y2a:Int, X1b:Int, Y1b:Int, X2b:Int, Y2b:Int)
	If (X1a <= X2b)
		If (Y1a <= Y2b)
			If (X2a >= X1b)
				If (Y2a >= Y1b)
					Return (True)
				EndIf
			EndIf
		EndIf
	EndIf
	Return (False)
End Function


' TRIGONOMETRIC

Global VSin:Float[3600]
Global VCos:Float[3600]

Function InitVTrigonometric ()
	Local I:Int = 0
	For I = 0 To 3599
		VSin[I] = Sin (Float (I) / 10.0)
		VCos[I] = Cos (Float (I) / 10.0)
	Next
End Function
	
' MISC

Const SQRT2:Float = 1.4142135623730950488016887242097
	
Private
' ---------------------
