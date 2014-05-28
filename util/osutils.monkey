Strict

Public

Class OSUtils
Public
	
	Function StripDir:String(path:String)
		Local i:Int = path.FindLast("/")
		If i = -1 i = path.FindLast("\")
		If i <> - 1 Return path[i + 1 ..]
		Return path
	End

	Function ExtractDir:String(path:String)
		Local i:Int = path.FindLast("/")
		If i = -1 i = path.FindLast("\")
		If i <> - 1 Return path[ .. i]
	End

	Function StripExt$( path$ )
		Local i:Int = path.FindLast(".")
		If i <> - 1 And path.Find("/", i + 1) = -1 And path.Find("\", i + 1) = -1 Return path[ .. i]
		Return path
	End
	
	Function ExtractExt$( path$ )
		Local i:Int = path.FindLast(".")
		If i <> - 1 And path.Find("/", i + 1) = -1 And path.Find("\", i + 1) = -1 Return path[i + 1 ..]
		Return ""
	End	
	
	Function StripAll:String(path:String)
		Return StripDir(StripExt(path))
	End
End Class