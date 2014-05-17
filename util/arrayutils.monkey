Strict

Public

Class ArrayUtils
'I did this as generic class <T> but it didn't let me define ArrayUtils<Int> for example :S
Public

	Function CloneIntArray:Int[](arrayToClone:Int[])
		If (arrayToClone.Length = 0) Then Return []
		Local length:Int = arrayToClone.Length()
		Local newArray:Int[] = New Int[length]
		For Local i:Int = 0 Until length
			newArray[i] = arrayToClone[i]
		End For
		Return newArray
	End Function

End Class