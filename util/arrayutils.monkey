Strict

Public

Class ArrayUtils<T>
Public
	
	Function Clone:T[](inputArray:T[])
		If (inputArray.Length = 0)
			Return[]
		End If
		
		Local outputArray:T[] = New T[inputArray.Length]
		
		For Local i:Int = 0 To inputArray.Length - 1
			outputArray[i] = inputArray[i]
		End For
		
		Return outputArray
	End Function

End Class