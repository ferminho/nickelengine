' VoiD.net
' --------
'
' FPG/VGP Module
' -----------
' Started 07-06-2005
' Latest Version 22-01-2007
'
' Unif Studios 2005/2007


Strict

' POSTIT : CODE and use SSVGP x) atm we can make it w/o it, but it's to be done someday
' CHANGES : graph 0 becomes an empty image, graph numbers are conserved from fpg

'Framework BRL.Max2D
Import BRL.StandardIO	'postit : debug
Import BRL.Max2D
Import BRL.Endianstream
Import BRL.Filesystem
Import BRL.PNGLoader
Import BRL.TGALoader
Import BRL.BMPLoader
Import BRL.JPGLoader


Public
' ---------------------


Type VGP
' ---
	Field Size:Int = 0
	Field VFlags:Int = 0
	Field Graph:TGraph[0]
' ---
	Function LoadFPG:VGP (File:String, Flags:Int = FILTEREDIMAGE | MIPMAPPEDIMAGE)
	' DYNAMICIMAGE is always added to flags
		Local FH:TStream = LittleEndianStream (OpenStream (File$, True, False))
		Local F:VGP = New VGP
		Local S:Int = 0
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local R:Int = 0, G:Int = 0, B:Int = 0
		Local Argb:Int = 0
		Local CX:Int = 0, CY:Int = 0
		Local T:TGraph = Null
		Local Im:TImage = Null
		
		If (Not FH) Then Crash ("FPG " + File$ + " not found")
		F.VFlags = Flags | DYNAMICIMAGE
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		If (FH.ReadByte () <> $66) Then Crash ("Wrong header reading FPG " + File) 'Header f16
		If (FH.ReadByte () <> $31) Then Crash ("Wrong header reading FPG " + File) 
		If (FH.ReadByte () <> $36) Then Crash ("Wrong header reading FPG " + File)

		If (FH.ReadByte () <> $1a) Then Crash ("Wrong header reading FPG " + File) 'Rest of header
		If (FH.ReadByte () <> $0d) Then Crash ("Wrong header reading FPG " + File)
		If (FH.ReadByte () <> $0a) Then Crash ("Wrong header reading FPG " + File)
		If (FH.ReadByte () <> $00) Then Crash ("Wrong header reading FPG " + File)
		FH.Skipbytes (1) 'Version byte

		SetMaskColor (0, 0, 0)

		While (Not FH.Eof ())
			I = FH.ReadInt ()

			If (F.Size <= I) Then
				F.Size = I + 1
				F.Graph = F.Graph[..F.Size]
			EndIf

			F.Graph[I] = TGraph.Create()
			T = F.Graph[I]
			FH.SkipBytes (4) 'Bitmap length - useless
			
			FH.SkipBytes (32) 'Description of map - useless
			FH.SkipBytes (12) 'Name of map filename - useless
			
			T.W = FH.ReadInt () 'Width
			T.H = FH.ReadInt () 'Height
			T.I = CreateImage (T.W, T.H, 1, F.VFlags)
			Im = T.I
			
			T.CPs = FH.ReadInt () 'Number of Control Points
			If (T.CPs > 0) Then 
				T.CP = T.CP[..T.CPs]
				T.CP[0] = New TPoint
				T.CP[0].X = FH.ReadShort () 'CP 0 'x'
				T.CP[0].Y = FH.ReadShort () 'CP 0 'y'
				CX = T.CP[0].X
				CY = T.CP[0].Y
				If (T.CPs > 1) Then
					For X = 1 To (F.Graph[I].CPs - 1)
						T.CP[X] = New TPoint
						T.CP[X].X = FH.ReadShort () 'CP 'x'
						T.CP[X].Y = FH.ReadShort () 'CP 'y'
					Next
				EndIf
			Else
				CX = T.W / 2
				CY = T.H / 2
			EndIf
			
			PM = LockImage (Im, 0)
			For Y = 0 To (T.H - 1)
				For X = 0 To (T.W - 1)
					S = 0 + FH.ReadShort ()
					R = ((S Shr 11) * 255) / 31
					G = (((S Shl 21) Shr 26) * 255) / 63
					B = (((S Shl 27) Shr 27) * 255) / 31
					' commented 3 - 6 - 2006 (why is this??)
'					If (R = 0 And G = 0 And B = 0) Then
'						Argb = 0
'					Else
						Argb = ($FF Shl 24) + (R Shl 16) + (G Shl 8) + B
'					EndIf
					PM.WritePixel (X, Y, Argb)
				Next
			Next
			UnlockImage (Im)
			SetImageHandle (Im, CX, CY)
			PM = Null
'			FlushMem ()
		Wend

		F.Graph[0] = GBlack
		For I = 1 To (F.Size - 1)
			If (Not F.Graph[I]) Then F.Graph[I] = GBlack
		Next

		CloseFile (FH)
		T = Null
		Im = Null
		FH = Null
'		FlushMem ()
		Return (F)
	End Function
' ---
	Function LoadFPGasVGP:VGP (File:String, Flags:Int = FILTEREDIMAGE | MIPMAPPEDIMAGE)
	' DYNAMICIMAGE is always added to flags
		Local FH:TStream = LittleEndianStream (OpenStream (File$, True, False))
		Local F:VGP = New VGP
		Local S:Int = 0
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local R:Int = 0, G:Int = 0, B:Int = 0
		Local Argb:Int = 0
		Local CX:Int = 0, CY:Int = 0
		Local T:TGraph = Null
		Local Name:String = ""
		
		If (Not FH) Then Crash ("FPG " + File$ + " not found")

		F.VFlags = Flags | DYNAMICIMAGE

		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		If (Not FH) Then Crash ("FPG " + File$ + " not found")

		If (FH.ReadByte () <> $66) Then Crash ("Wrong header reading FPG " + File) 'Header f16
		If (FH.ReadByte () <> $31) Then Crash ("Wrong header reading FPG " + File) 
		If (FH.ReadByte () <> $36) Then Crash ("Wrong header reading FPG " + File)

		If (FH.ReadByte () <> $1a) Then Crash ("Wrong header reading FPG " + File) 'Rest of header
		If (FH.ReadByte () <> $0d) Then Crash ("Wrong header reading FPG " + File)
		If (FH.ReadByte () <> $0a) Then Crash ("Wrong header reading FPG " + File)
		If (FH.ReadByte () <> $00) Then Crash ("Wrong header reading FPG " + File)
		FH.Skipbytes (1) 'Version byte

		SetMaskColor (0, 0, 0)
		File = ExtractDir (File) + "/"

		While (Not FH.Eof ())
			I = FH.ReadInt ()

			If (F.Size <= I) Then
				F.Size = I + 1
				F.Graph = F.Graph[..F.Size]
			EndIf

			F.Graph[I] = TGraph.Create()
			T = F.Graph[I]
			FH.SkipBytes (4) 'Bitmap length - useless
			
			FH.SkipBytes (32) 'Description of map - useless
			Name = FReadString (FH, 12) 'Name of map filename - useless
			T.W = FH.ReadInt () 'Width
			T.H = FH.ReadInt () 'Height
			T.I = LoadImage (File + Name, F.VFlags)
			
			T.CPs = FH.ReadInt () 'Number of Control Points
			If (T.CPs > 0) Then 
				T.CP = T.CP[..T.CPs]
				T.CP[0] = New TPoint
				T.CP[0].X = FH.ReadShort () 'CP 0 'x'
				T.CP[0].Y = FH.ReadShort () 'CP 0 'y'
				CX = T.CP[0].X
				CY = T.CP[0].Y
				If (T.CPs > 1) Then
					For X = 1 To (F.Graph[I].CPs - 1)
						T.CP[X] = New TPoint
						T.CP[X].X = FH.ReadShort () 'CP 'x'
						T.CP[X].Y = FH.ReadShort () 'CP 'y'
					Next
				EndIf
			Else
				CX = T.W / 2
				CY = T.H / 2
			EndIf
			SetImageHandle (T.I, CX, CY)
			FH.SkipBytes ((T.H * T.W) * 2)
'			FlushMem ()
		Wend

		F.Graph[0] = GBlack
		For I = 1 To (F.Size - 1)
			If (Not F.Graph[I]) Then F.Graph[I] = GBlack
		Next

		CloseFile (FH)
		Name = ""
		T = Null
		FH = Null
'		FlushMem ()
		Return (F)
	End Function
' ---
	Function LoadVGP:VGP (File:String, Flags:Int = FILTEREDIMAGE | MIPMAPPEDIMAGE, Recenter:Int = True)
	' DYNAMICIMAGE is always added to flags
		Local FH:TStream = LittleEndianStream (OpenStream (File$, True, False))
		Local F:VGP = New VGP
		Local S:Int = 0
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local R:Int = 0, G:Int = 0, B:Int = 0
		Local Argb:Int = 0
		Local CX:Int = 0, CY:Int = 0
		Local T:TGraph = Null
		Local Im:TImage = Null

		If (Not FH) Then Crash ("VGP " + File$ + " not found")

		F.VFlags = Flags | DYNAMICIMAGE
				
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		If (Not FH) Then Crash ("VGP " + File$ + " not found")

		If (FH.ReadByte () <> $56) Then Crash ("Wrong header reading VGP " + File) 'Header VGP
		If (FH.ReadByte () <> $47) Then Crash ("Wrong header reading VGP " + File) 
		If (FH.ReadByte () <> $50) Then Crash ("Wrong header reading VGP " + File)

		FH.Skipbytes (1) 'Version byte

		SetMaskColor (0, 0, 0)

		While (Not FH.Eof ())
			I = FH.ReadInt ()

			If (F.Size <= I) Then
				F.Size = I + 1
				F.Graph = F.Graph[..F.Size]
			EndIf

			F.Graph[I] = TGraph.Create()
			T = F.Graph[I]
			FH.SkipBytes (4) 'Bitmap length - useless
			
			T.W = FH.ReadInt () 'Width
			T.H = FH.ReadInt () 'Height
			T.I = CreateImage (T.W, T.H, 1, F.VFlags)
			Im = T.I
			
			T.CPs = FH.ReadInt () 'Number of Control Points
			If (T.CPs > 0) Then 
				T.CP = T.CP[..T.CPs]
				T.CP[0] = New TPoint
				T.CP[0].X = FH.ReadShort () 'CP 0 'x'
				T.CP[0].Y = FH.ReadShort () 'CP 0 'y'
				CX = T.CP[0].X
				CY = T.CP[0].Y
				If (T.CPs > 1) Then
					For X = 1 To (T.CPs - 1)
						T.CP[X] = New TPoint
						T.CP[X].X = FH.ReadShort () 'CP 'x'
						T.CP[X].Y = FH.ReadShort () 'CP 'y'
					Next
				EndIf
			Else
				CX = T.W / 2
				CY = T.H / 2
			EndIf
			
			PM = LockImage (Im, 0)
			For Y = 0 To (T.H - 1)
				For X = 0 To (T.W - 1)
					S = FH.ReadByte ()
					R = FH.ReadByte ()
					G = FH.ReadByte ()
					B = FH.ReadByte ()
'	Removed : old mask mode
'					If (R = 0 And G = 0 And B = 0) Then
'						Argb = 0
'					Else
						Argb = (S Shl 24) + (R Shl 16) + (G Shl 8) + B
'					EndIf
					PM.WritePixel (X, Y, Argb)
'					FlushMem ()
				Next
			Next
			UnlockImage (Im)
			SetImageHandle (Im, CX, CY)
			PM = Null
'			FlushMem ()
		Wend

		F.Graph[0] = GBlack
		For I = 1 To (F.Size - 1)
			If (Not F.Graph[I]) Then F.Graph[I] = GBlack
		Next

		CloseFile (FH)
		If (Recenter) Then RecenterPoints (F)
		T = Null
		Im = Null
		FH = Null
'		FlushMem ()
		Return (F)
	End Function
' ---
	Method SaveVGP (File:String)
	' DYNAMICIMAGE is always added to flags
		Local FH:TStream = LittleEndianStream (WriteFile (File$))
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local Argb:Int = 0
		Local T:TGraph = Null
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		If (Not FH) Then Crash ("Impossible to write VGP " + File$)

		FH.WriteByte ($56)
		FH.WriteByte ($47)
		FH.WriteByte ($50)
		FH.WriteByte (VGPVersion) 'Version byte

		For I = 1 To (Size - 1)
			If (Graph[I]) Then
				If (Graph[I] <> GBlack) Then
					T = Graph[I]
					FH.WriteInt (I)
		
					FH.WriteInt (T.W * T.H * 4) 'Bitmap length
				
					FH.WriteInt (T.W) 'Width
					FH.WriteInt (T.H) 'Height
					
					FH.WriteInt (T.CPs) 'Number of Control Points
					If (T.CPs > 0) Then
						For X = 0 To (T.CPs - 1)
							FH.WriteShort (T.CP[X].X) 'CP 'x'
							FH.WriteShort (T.CP[X].Y) 'CP 'y'
						Next
					EndIf
					PM = LockImage (T.I, 0)
					For Y = 0 To (T.H - 1)
						For X = 0 To (T.W - 1)
							Argb = PM.ReadPixel (X, Y)
							FH.WriteByte (Argb Shr 24)
							If ((Argb Shr 24) = 0) 'fix
								FH.WriteByte (0)
								FH.WriteByte (0)
								FH.WriteByte (0)
							Else
								FH.WriteByte ((Argb Shl 8) Shr 24)
								FH.WriteByte ((Argb Shl 16) Shr 24)
								FH.WriteByte ((Argb Shl 24) Shr 24)
							EndIf
'							FlushMem ()
						Next
					Next
					UnlockImage (T.I)
					PM = Null
'					FlushMem ()
				EndIf
			EndIf
		Next

		CloseFile (FH)
		T = Null
		FH = Null
'		FlushMem ()
	End Method
' ---
	Method Copy:VGP ()
		' not useful if still knowing the path/filename. LoadFPG/VGP is generally faster (up to 3x) than Copy
		Local F:VGP = New VGP
		Local I:Int = 0, J:Int = 0
		Local T:TGraph = Null, T2:TGraph = Null
		Local PM:TPixmap = Null, PM2:TPixmap = Null
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		F.Size = Size
		F.VFlags = VFlags
		F.Graph = F.Graph[..Size]
		For I = 0 To (Size - 1)
			If (Graph[I]) Then
				If (Graph[I] = GBlack) Then
					F.Graph[I] = GBlack
				Else
					F.Graph[I] = New TGraph
					T = F.Graph[I]
					T2 = Graph[I]
					T.W = T2.W
					T.H = T2.H
					T.CPs = T2.CPs
					T.CP = T.CP[..T.CPs]
					For J = 0 To (T2.CPs - 1)
						T.CP[J] = New TPoint
						T.CP[J].X = T2.CP[J].X
						T.CP[J].Y = T2.CP[J].Y
					Next
					T.I = CreateImage (T2.W, T2.H, 1, VFlags)
					PastePixMap (LockImage (T2.I), LockImage (T.I), 0, 0, T2.W, T2.H, 0, 0)
					UnlockImage (T2.I)
					UnlockImage (T.I)
'					FlushMem ()
					If (T2.CPs > 0)
						SetImageHandle (T.I, T2.CP[0].X, T2.CP[0].Y)
					Else
						SetImageHandle (T.I, (T2.W / 2), (T2.H / 2))
					EndIf
				EndIf
			EndIf
		Next
		
		T = Null
		T2 = Null
		Return (F)
	End Method
' ---
	Method BlackMask ()
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local ARGB:Int = 0
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		For I = 1 To (Size - 1)
			If (Graph[I]) Then
			If (Graph[I] <> GBlack) Then
				PM = LockImage (Graph[I].I)
				For Y = 0 To (Graph[I].H - 1)
					For X = 0 To (Graph[I].W - 1)
						ARGB = ReadPixel (PM, X, Y)
						If (ARGB <> 0) 'if 0 (transparent) - leave it be
							WritePixel (PM, X, Y, BLACKMASKARGB)
						EndIf
					Next
				Next
				UnlockImage (Graph[I].I)
				PM = Null
'				FlushMem ()
			EndIf
			EndIf
		Next
	End Method
' ---
	Method ChangeRanges (R1:Byte, G1:Byte, B1:Byte, R2:Byte, G2:Byte, B2:Byte, R3:Byte, G3:Byte, B3:Byte, Tolerance:Byte = 1)
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local ARGB:Int = 0
		Local A:Int = 0, R:Int = 0, G:Int = 0, B:Int = 0

		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		For I = 1 To (Size - 1)
			If (Graph[I]) Then
			If (Graph[I] <> GBlack) Then
				PM = LockImage (Graph[I].I)
				For Y = 0 To (Graph[I].H - 1)
					For X = 0 To (Graph[I].W - 1)
						ARGB = ReadPixel (PM, X, Y)
						A = ARGB Shr 24
						If (A > 0) Then
							R = (ARGB Shl 8) Shr 24
							G = (ARGB Shl 16) Shr 24
							B = (ARGB Shl 24) Shr 24
							If (R > Tolerance And G <= Tolerance And B <= Tolerance)
								ARGB = (A Shl 24) + (((R * R1) / 255) Shl 16) + (((R * G1) / 255) Shl 8) + ((R * B1) / 255)
								WritePixel (PM, X, Y, ARGB)
							ElseIf (R <= Tolerance And G > Tolerance And B <= Tolerance)
								ARGB = (A Shl 24) + (((G * R2) / 255) Shl 16) + (((G * G2) / 255) Shl 8) + ((G * B2) / 255)
								WritePixel (PM, X, Y, ARGB)
							ElseIf (R <= Tolerance And G <= Tolerance And B > Tolerance)
								ARGB = (A Shl 24) + (((B * R3) / 255) Shl 16) + (((B * G3) / 255) Shl 8) + ((B * B3) / 255)
								WritePixel (PM, X, Y, ARGB)
							EndIf
						EndIf
					Next
				Next
				UnlockImage (Graph[I].I)
				PM = Null
'				FlushMem ()
			EndIf
			EndIf
		Next
	End Method
' ---
	Method Unload () 'unreferences and cleans Garbage Collector
		Local I:Int = 0, J:Int = 0
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		For I = 0 To (Size - 1)
			If (Graph[I]) Then
				If (Graph[I] <> GBlack) Then
					Graph[I].I = Null
					For J = 0 To (Graph[I].CPs - 1)
						Graph[I].CP[J] = Null
					Next
					Graph[I] = Null
				EndIf
			EndIf
'			FlushMem ()
		Next
		Graph = Graph[0..0]
'		FlushMem ()
	End Method 
' ---
	Method Delete ()
		Graph = Graph[0..0]
'		FlushMem ()
	End Method
' ---
End Type

' SINGLE SURFACE VGP
'
' Keeps all images info into a single 2^n, divided in equal cells image
' Warning : no control point info is saved.
' Utility : great for tiles collections, specially if tilesize = 2^n

Type SSVGP
' ---
	Field Size:Int = 0
	Field VFlags:Int = 0
	Field Graph:TGraph[0]
' ---
	Function LoadFPGasVGP:VGP (File:String, Flags:Int = FILTEREDIMAGE | MIPMAPPEDIMAGE)
	' DYNAMICIMAGE is always added to flags
		Local FH:TStream = LittleEndianStream (OpenStream (File$, True, False))
		Local F:VGP = New VGP
		Local S:Int = 0
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local R:Int = 0, G:Int = 0, B:Int = 0
		Local Argb:Int = 0
		Local CX:Int = 0, CY:Int = 0
		Local T:TGraph = Null
		Local Name:String = ""
		
		If (Not FH) Then Crash ("FPG " + File$ + " not found")

		F.VFlags = Flags | DYNAMICIMAGE

		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		If (Not FH) Then Crash ("FPG " + File$ + " not found")

		If (FH.ReadByte () <> $66) Then Crash ("Wrong header reading FPG " + File) 'Header f16
		If (FH.ReadByte () <> $31) Then Crash ("Wrong header reading FPG " + File) 
		If (FH.ReadByte () <> $36) Then Crash ("Wrong header reading FPG " + File)

		If (FH.ReadByte () <> $1a) Then Crash ("Wrong header reading FPG " + File) 'Rest of header
		If (FH.ReadByte () <> $0d) Then Crash ("Wrong header reading FPG " + File)
		If (FH.ReadByte () <> $0a) Then Crash ("Wrong header reading FPG " + File)
		If (FH.ReadByte () <> $00) Then Crash ("Wrong header reading FPG " + File)
		FH.Skipbytes (1) 'Version byte

		SetMaskColor (0, 0, 0)
		File = ExtractDir (File) + "/"

		While (Not FH.Eof ())
			I = FH.ReadInt ()

			If (F.Size < I) Then
				F.Size = I
				F.Graph = F.Graph[..F.Size]
			EndIf

			F.Graph[I - 1] = TGraph.Create()
			T = F.Graph[I - 1]
			FH.SkipBytes (4) 'Bitmap length - useless
			
			FH.SkipBytes (32) 'Description of map - useless
			Name = FReadString (FH, 12) 'Name of map filename - useless
			T.W = FH.ReadInt () 'Width
			T.H = FH.ReadInt () 'Height
			T.I = LoadImage (File + Name, F.VFlags)
			
			T.CPs = FH.ReadInt () 'Number of Control Points
			If (T.CPs > 0) Then 
				T.CP = T.CP[..T.CPs]
				T.CP[0] = New TPoint
				T.CP[0].X = FH.ReadShort () 'CP 0 'x'
				T.CP[0].Y = FH.ReadShort () 'CP 0 'y'
				CX = T.CP[0].X
				CY = T.CP[0].Y
				If (T.CPs > 1) Then
					For X = 1 To (F.Graph[I - 1].CPs - 1)
						T.CP[X] = New TPoint
						T.CP[X].X = FH.ReadShort () 'CP 'x'
						T.CP[X].Y = FH.ReadShort () 'CP 'y'
					Next
				EndIf
			Else
				CX = T.W / 2
				CY = T.H / 2
			EndIf
			SetImageHandle (T.I, CX, CY)
			FH.SkipBytes ((T.H * T.W) * 2)
'			FlushMem ()
		Wend

		For I = 0 To (F.Size - 2)
			If (Not F.Graph[I]) Then F.Graph[I] = GBlack
		Next

		CloseFile (FH)
		Name = ""
		T = Null
		FH = Null
'		FlushMem ()
		Return (F)
	End Function
' ---
	Function LoadVGP:VGP (File:String, Flags:Int = FILTEREDIMAGE | MIPMAPPEDIMAGE)
	' DYNAMICIMAGE is always added to flags
		Local FH:TStream = LittleEndianStream (OpenStream (File$, True, False))
		Local F:VGP = New VGP
		Local S:Int = 0
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local R:Int = 0, G:Int = 0, B:Int = 0
		Local Argb:Int = 0
		Local CX:Int = 0, CY:Int = 0
		Local T:TGraph = Null
		Local Im:TImage = Null

		If (Not FH) Then Crash ("VGP " + File$ + " not found")

		F.VFlags = Flags | DYNAMICIMAGE
				
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		If (Not FH) Then Crash ("VGP " + File$ + " not found")

		If (FH.ReadByte () <> $56) Then Crash ("Wrong header reading VGP " + File) 'Header VGP
		If (FH.ReadByte () <> $47) Then Crash ("Wrong header reading VGP " + File) 
		If (FH.ReadByte () <> $50) Then Crash ("Wrong header reading VGP " + File)

		FH.Skipbytes (1) 'Version byte

		SetMaskColor (0, 0, 0)

		While (Not FH.Eof ())
			I = FH.ReadInt ()

			If (F.Size < (I + 1)) Then
				F.Size = (I + 1)
				F.Graph = F.Graph[..F.Size]
			EndIf

			F.Graph[I] = TGraph.Create()
			T = F.Graph[I]
			FH.SkipBytes (4) 'Bitmap length - useless
			
			T.W = FH.ReadInt () 'Width
			T.H = FH.ReadInt () 'Height
			T.I = CreateImage (T.W, T.H, 1, F.VFlags)
			Im = T.I
			
			T.CPs = FH.ReadInt () 'Number of Control Points
			If (T.CPs > 0) Then 
				T.CP = T.CP[..T.CPs]
				T.CP[0] = New TPoint
				T.CP[0].X = FH.ReadShort () 'CP 0 'x'
				T.CP[0].Y = FH.ReadShort () 'CP 0 'y'
				CX = T.CP[0].X
				CY = T.CP[0].Y
				If (T.CPs > 1) Then
					For X = 1 To (T.CPs - 1)
						T.CP[X] = New TPoint
						T.CP[X].X = FH.ReadShort () 'CP 'x'
						T.CP[X].Y = FH.ReadShort () 'CP 'y'
					Next
				EndIf
			Else
				CX = T.W / 2
				CY = T.H / 2
			EndIf
			
			PM = LockImage (Im, 0)
			For Y = 0 To (T.H - 1)
				For X = 0 To (T.W - 1)
					S = FH.ReadByte ()
					R = FH.ReadByte ()
					G = FH.ReadByte ()
					B = FH.ReadByte ()
'	Removed : old mask mode
'					If (R = 0 And G = 0 And B = 0) Then
'						Argb = 0
'					Else
						Argb = (S Shl 24) + (R Shl 16) + (G Shl 8) + B
'					EndIf
					PM.WritePixel (X, Y, Argb)
'					FlushMem ()
				Next
			Next
			UnlockImage (Im)
			SetImageHandle (Im, CX, CY)
			PM = Null
'			FlushMem ()
		Wend

		For I = 0 To (F.Size - 2)
			If (Not F.Graph[I]) Then F.Graph[I] = GBlack
		Next

		CloseFile (FH)
		T = Null
		Im = Null
		FH = Null
'		FlushMem ()
		Return (F)
	End Function
' ---
	Method SaveGridVGP (File:String)
	' DYNAMICIMAGE is always added to flags
		Local FH:TStream = LittleEndianStream (WriteFile (File$))
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local Argb:Int = 0
		Local T:TGraph = Null
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		If (Not FH) Then Crash ("Impossible to write VGP " + File$)

		FH.WriteByte ($56)
		FH.WriteByte ($47)
		FH.WriteByte ($50)
		FH.WriteByte (VGPVersion) 'Version byte

		For I = 0 To (Size - 1)
			If (Graph[I]) Then
				If (Graph[I] <> GBlack) Then
					T = Graph[I]
					FH.WriteInt (I)
		
					FH.WriteInt (T.W * T.H * 4) 'Bitmap length
				
					FH.WriteInt (T.W) 'Width
					FH.WriteInt (T.H) 'Height
					
					FH.WriteInt (T.CPs) 'Number of Control Points
					If (T.CPs > 0) Then
						For X = 0 To (T.CPs - 1)
							FH.WriteShort (T.CP[X].X) 'CP 'x'
							FH.WriteShort (T.CP[X].Y) 'CP 'y'
						Next
					EndIf
					PM = LockImage (T.I, 0)
					For Y = 0 To (T.H - 1)
						For X = 0 To (T.W - 1)
							Argb = PM.ReadPixel (X, Y)
							FH.WriteByte (Argb Shr 24)
							FH.WriteByte ((Argb Shl 8) Shr 24)
							FH.WriteByte ((Argb Shl 16) Shr 24)
							FH.WriteByte ((Argb Shl 24) Shr 24)
'							FlushMem ()
						Next
					Next
					UnlockImage (T.I)
					PM = Null
'					FlushMem ()
				EndIf
			EndIf
		Next

		CloseFile (FH)
		T = Null
		FH = Null
'		FlushMem ()
	End Method
' ---
	Method Copy:VGP ()
		' not useful if still knowing the path/filename. LoadFPG/VGP is generally faster (up to 3x) than Copy
		Local F:VGP = New VGP
		Local I:Int = 0, J:Int = 0
		Local T:TGraph = Null, T2:TGraph = Null
		Local PM:TPixmap = Null, PM2:TPixmap = Null
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		F.Size = Size
		F.VFlags = VFlags
		F.Graph = F.Graph[..Size]
		For I = 0 To (Size - 1)
			If (Graph[I]) Then
				If (Graph[I] = GBlack) Then
					F.Graph[I] = GBlack
				Else
					F.Graph[I] = New TGraph
					T = F.Graph[I]
					T2 = Graph[I]
					T.W = T2.W
					T.H = T2.H
					T.CPs = T2.CPs
					T.CP = T.CP[..T.CPs]
					For J = 0 To (T2.CPs - 1)
						T.CP[J] = New TPoint
						T.CP[J].X = T2.CP[J].X
						T.CP[J].Y = T2.CP[J].Y
					Next
					T.I = CreateImage (T2.W, T2.H, 1, VFlags)
					PastePixMap (LockImage (T2.I), LockImage (T.I), 0, 0, T2.W, T2.H, 0, 0)
					UnlockImage (T2.I)
					UnlockImage (T.I)
'					FlushMem ()
					If (T2.CPs > 0)
						SetImageHandle (T.I, T2.CP[0].X, T2.CP[0].Y)
					Else
						SetImageHandle (T.I, (T2.W / 2), (T2.H / 2))
					EndIf
				EndIf
			EndIf
		Next
		
		T = Null
		T2 = Null
		Return (F)
	End Method
' ---
	Method BlackMask ()
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local ARGB:Int = 0
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		For I = 0 To (Size - 1)
			If (Graph[I]) Then
				PM = LockImage (Graph[I].I)
				For Y = 0 To (Graph[I].H - 1)
					For X = 0 To (Graph[I].W - 1)
						ARGB = ReadPixel (PM, X, Y)
						If (ARGB <> 0) 'if 0 (transparent) - leave it be
							WritePixel (PM, X, Y, BLACKMASKARGB)
						EndIf
					Next
				Next
				UnlockImage (Graph[I].I)
				PM = Null
'				FlushMem ()
			EndIf
		Next
	End Method
' ---
	Method ChangeRanges (R1:Byte, G1:Byte, B1:Byte, R2:Byte, G2:Byte, B2:Byte, R3:Byte, G3:Byte, B3:Byte, Tolerance:Byte = 1)
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local PM:TPixmap = Null
		Local ARGB:Int = 0
		Local A:Int = 0, R:Int = 0, G:Int = 0, B:Int = 0

		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		For I = 0 To (Size - 1)
			If (Graph[I]) Then
				PM = LockImage (Graph[I].I)
				For Y = 0 To (Graph[I].H - 1)
					For X = 0 To (Graph[I].W - 1)
						ARGB = ReadPixel (PM, X, Y)
						A = ARGB Shr 24
						If (A > 0) Then
							R = (ARGB Shl 8) Shr 24
							G = (ARGB Shl 16) Shr 24
							B = (ARGB Shl 24) Shr 24
							If (R > Tolerance And G <= Tolerance And B <= Tolerance)
								ARGB = (A Shl 24) + (((R * R1) / 255) Shl 16) + (((R * G1) / 255) Shl 8) + ((R * B1) / 255)
								WritePixel (PM, X, Y, ARGB)
							ElseIf (R <= Tolerance And G > Tolerance And B <= Tolerance)
								ARGB = (A Shl 24) + (((G * R2) / 255) Shl 16) + (((G * G2) / 255) Shl 8) + ((G * B2) / 255)
								WritePixel (PM, X, Y, ARGB)
							ElseIf (R <= Tolerance And G <= Tolerance And B > Tolerance)
								ARGB = (A Shl 24) + (((B * R3) / 255) Shl 16) + (((B * G3) / 255) Shl 8) + ((B * B3) / 255)
								WritePixel (PM, X, Y, ARGB)
							EndIf
						EndIf
					Next
				Next
				UnlockImage (Graph[I].I)
				PM = Null
'				FlushMem ()
			EndIf
		Next
	End Method
' ---
	Method Unload () 'unreferences and cleans Garbage Collector
		Local I:Int = 0, J:Int = 0
		
		If (Not GBlack) Then 
			GBlack = TGraph.Create ()
			GBlack.W = 1
			GBlack.H = 1
			GBlack.I = CreateImage (1, 1, 1, 0)
		EndIf

		For I = 0 To (Size - 1)
			If (Graph[I]) Then
				If (Graph[I] <> GBlack) Then
					Graph[I].I = Null
					For J = 0 To (Graph[I].CPs - 1)
						Graph[I].CP[J] = Null
					Next
					Graph[I] = Null
				EndIf
			EndIf
'			FlushMem ()
		Next
		Graph = Graph[0..0]
'		FlushMem ()
	End Method 
' ---
	Method Delete ()
		Graph = Graph[0..0]
'		FlushMem ()
	End Method
' ---
End Type


Type PVGP
' ---
	Field Size:Int = 0
	Field Graph:TPixmap[0]
' ---
	Function LoadPFPG:PVGP (File$)
		Local FH:TStream = LittleEndianStream (OpenStream (File$, True, False))
		Local F:PVGP = New PVGP
		Local W:Int = 0, H:Int = 0
		Local S:Int = 0
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local R:Int = 0, G:Int = 0, B:Int = 0
		Local Argb:Int = 0
		Local PM:TPixmap = Null
		
		If (Not FH) Then Crash ("FPG " + File$ + " not found")

		If (FH.ReadByte () <> $66) Then Crash ("Wrong header reading FPG " + File) 'Header f16
		If (FH.ReadByte () <> $31) Then Crash ("Wrong header reading FPG " + File) 
		If (FH.ReadByte () <> $36) Then Crash ("Wrong header reading FPG " + File)

		If (FH.ReadByte () <> $1a) Then Crash ("Wrong header reading FPG " + File) 'Rest of header
		If (FH.ReadByte () <> $0d) Then Crash ("Wrong header reading FPG " + File)
		If (FH.ReadByte () <> $0a) Then Crash ("Wrong header reading FPG " + File)
		If (FH.ReadByte () <> $00) Then Crash ("Wrong header reading FPG " + File)
		FH.Skipbytes (1) 'Version byte

		SetMaskColor (0, 0, 0)

		While (Not FH.Eof ())
			I = FH.ReadInt ()

			If (F.Size <= I) Then
				F.Size = I + 1
				F.Graph = F.Graph[..F.Size]
			EndIf
			FH.SkipBytes (4) 'Bitmap length - useless
			
			FH.SkipBytes (32) 'Description of map - useless
			FH.SkipBytes (12) 'Name of map filename - useless
			
			W = FH.ReadInt () 'Width
			H = FH.ReadInt () 'Height
			F.Graph[I] = CreatePixmap (W, H, PF_RGBA8888)
			PM = F.Graph[I]
			
			X = FH.ReadInt () 'Number of Control Points
			While (X > 0)
				FH.Skipbytes (4) ' X and Y - useless
				X:-1
			Wend
						
			For Y = 0 To (H - 1)
				For X = 0 To (W - 1)
					S = 0 + FH.ReadShort ()
					R = ((S Shr 11) * 255) / 31
					G = (((S Shl 21) Shr 26) * 255) / 63
					B = (((S Shl 27) Shr 27) * 255) / 31
'	Removed : old mask mode
'					If (R = 0 And G = 0 And B = 0) Then
'						Argb = 0
'					Else
						Argb = ($FF Shl 24) + (R Shl 16) + (G Shl 8) + B
'					EndIf
					PM.WritePixel (X, Y, Argb)
'					FlushMem ()
				Next
			Next
		Wend

		CloseFile (FH)
		FH = Null
		PM = Null
'		FlushMem ()
		Return (F)
	End Function
' ---
	Function LoadPVGP:PVGP (File$)
		Local FH:TStream = LittleEndianStream (OpenStream (File$, True, False))
		Local F:PVGP = New PVGP
		Local W:Int = 0, H:Int = 0
		Local S:Int = 0
		Local I:Int = 0
		Local X:Int = 0, Y:Int = 0
		Local R:Int = 0, G:Int = 0, B:Int = 0
		Local Argb:Int = 0
		Local PM:TPixmap = Null
		
		If (Not FH) Then Crash ("FPG " + File$ + " not found")

		If (FH.ReadByte () <> $56) Then Crash ("Wrong header reading VGP " + File) 'Header VGP
		If (FH.ReadByte () <> $47) Then Crash ("Wrong header reading VGP " + File) 
		If (FH.ReadByte () <> $50) Then Crash ("Wrong header reading VGP " + File)

		FH.Skipbytes (1) 'Version byte

		SetMaskColor (0, 0, 0)

		While (Not FH.Eof ())
			I = FH.ReadInt ()

			If (F.Size <= I) Then
				F.Size = I + 1
				F.Graph = F.Graph[..F.Size]
			EndIf
			FH.SkipBytes (4) 'Bitmap length - useless
			
			W = FH.ReadInt () 'Width
			H = FH.ReadInt () 'Height
			F.Graph[I] = CreatePixmap (W, H, PF_RGBA8888)
			PM = F.Graph[I]
			
			X = FH.ReadInt () 'Number of Control Points
			While (X > 0)
				FH.Skipbytes (4) ' X and Y - useless
				X:-1
			Wend
						
			For Y = 0 To (H - 1)
				For X = 0 To (W - 1)
					S = FH.ReadByte ()
					R = FH.ReadByte ()
					G = FH.ReadByte ()
					B = FH.ReadByte ()
'	Removed : old mask mode
'					If (R = 0 And G = 0 And B = 0) Then
'						Argb = 0
'					Else
						Argb = (S Shl 24) + (R Shl 16) + (G Shl 8) + B
'					EndIf
					PM.WritePixel (X, Y, Argb)
'					FlushMem ()
				Next
			Next
		Wend

		CloseFile (FH)
		FH = Null
		PM = Null
'		FlushMem ()
		Return (F)
	End Function
' ---
	Method Unload () 'unreferences and cleans Garbage Collector
		Local I:Int = 0, J:Int = 0
		
		For I = 0 To (Size - 1)
			If (Graph[I]) Then
				Graph[I] = Null
			EndIf
'			FlushMem ()
		Next
		Graph = Graph[0..0]
'		FlushMem ()
	End Method 
' ---
	Method Delete ()
		Graph = Graph[0..0]
'		FlushMem ()
	End Method
' ---
End Type


Type TGraph
' ---
	Field W:Int = 0, H:Int = 0
	Field CPs:Int = 0
	Field CP:TPoint[0]
	Field I:TImage
' ---
	Function Create:TGraph ()
		Return (New TGraph)
	End Function
' ---
	Method Delete ()
		CP = CP[0..0]
		I = Null
'		FlushMem ()
	End Method
' ---
End Type


Type TPoint
' ---
	Field X:Int, Y:Int
' ---
End Type


' IndiePath's code
' TAnimImage : Single Surface Images
' version: 8-10-2005
Rem
Type TAnimImage
' ---
	Field Image:TImage
	Field width:Int
	Field height:Int
	Field u0:Float[]
	Field v0:Float[]
	Field u1:Float[]
	Field v1:Float[] 
' ---
	Function Load:TAnimImage (url:Object, cell_width:Float, cell_height:Float, ..
								start:Int,frames:Int,flags=-1)
		Local t:TAnimImage = New TAnimImage
		Local tx:Float
		Local ty:Float
		Local x_Cells:Int
		
		t.u0 = New Float[frames]
		t.v0 = New Float[frames]
		t.u1 = New Float[frames]
		t.v1 = New Float[frames]
		t.Image = LoadImage(url,flags)
		Local xDelta:Float = t.Image.Width / Pow2Size (t.image.width)
		Local yDelta:Float = t.Image.Height / Pow2Size (t.image.height)
		x_cells = t.Image.Width  / cell_width
		For Local f = start To frames - 1
			tx = ((f Mod x_cells) * cell_width) * xdelta
			ty = ((f * cell_Height) / x_cells) * ydelta
			t.u0[f]= Float (tx) / Float (t.Image.Width)
			t.v0[f] = Float (ty) / Float (t.Image.Height)
			t.u1[f]= Float (tx + cell_width * xdelta)  / Float (t.Image.Width)
			t.v1[f]= Float (ty + cell_Height * ydelta) / Float (t.Image.Height)
		Next
		Return t
	End Function
' ---
	Function  Pow2Size:Float (n)
		Local t:Int = 1
		
		While (t < n)
			t:*2
		Wend
		Return t
	End Function
' ---
Method Delete ()
	Image = Null
'	FlushMem()
End Method
' ---
Method Draw (x:Float, y:Float, width:Float, height:Float, frame:Int = 0)
	Local DXFrame:TDX7ImageFrame = TDX7ImageFrame (image.frame(0))
	
	If DXFrame
		DXFrame.setUV (u0[frame], v0[frame], u1[frame], v1[frame])
	Else
		Local GLFrame:TGLImageFrame = TGLImageFrame (image.frame(0))
		GLFrame.u0 = u0[frame]
		GLFrame.u1 = u1[frame]
		GLFrame.v0 = v0[frame]
		GLFrame.v1 = v1[frame]
	EndIf
	DrawImageRect (Self.Image, x, y, width, height)
End Method
' ---
End Type
' END of IndiePath's code
End Rem

Private
' ---------------------


' Empty (black) 1x1 image for filling non-graph holes so no error when trying to draw it
' WARNING : fills holes BETWEEN graphs, not beyond last graph, only for VGP class (not PVGP)
Global GBlack:TGraph = Null


' Consts

Const BLACKMASKARGB = $FF000000 ' Black used for blackmasking
Const VGPVersion = $01 


Function RecenterPoints (V:VGP)
	Local I:Int, J:Int
	Local G:TGraph
	Local CX:Int, CY:Int
	
	If (Not V Or V.Size = 0) Return
	For I = 0 To (V.Size - 1)
		G = V.Graph[I]
		If (G And G.CPs > 1)
			CX = G.CP[0].X	CY = G.CP[0].Y
			For J = 1 To G.CPs - 1
				G.CP[J].X:- CX
				G.CP[J].Y:- CY
'				FlushMem ()
			Next
		EndIf
'		FlushMem ()
	Next
End Function


Function FReadString:String (FH:TStream, L)
	Local S:String = ""
	Local B:Byte = $FF
	Local R:Int = 0
	
	While ((Not FH.Eof ()) And (R < L) And B > 0)
		B = FH.ReadByte ()
		If (B > 0)
			S:+ Chr(B)
		EndIf
		R:+ 1
'		FlushMem ()
	Wend
	While ((Not FH.Eof ()) And (R < L))
		FH.ReadByte ()
		R:+ 1
'		FlushMem ()
	Wend
	Return (S)
End Function

Function FReadShort:Short (FH:TStream)
	Local S:Short = FH.ReadByte() + (FH.ReadByte() * 256)
	Return (S)
End Function

Function FReadInt:Int (FH:TStream)
	Local I:Int = FH.ReadByte() + (FH.ReadByte() * 256) + (FH.ReadByte() * 65536) + (FH.ReadByte() * 16777216)
	Return (I)
End Function

Function PastePixmap (P1:TPixmap, P2:TPixmap, X1, Y1, Width, Height, X2, Y2)
' pastes part of P1 into P2
	Local I:Int = 0, J:Int = 0	

	For I = Y1 To (Y1 + Height - 1)
		For J = X1 To (X1 + Width - 1)
			WritePixel (P2, X2 + J, Y2 + I, ReadPixel (P1, X1 + J, Y2 + I))
'			FlushMem ()
		Next
	Next
End Function

Function Crash (S:String)
	Notify (S)
	WriteStdout (S)
	RuntimeError ("VoiD.net VGP module abnormal termination")
End Function

' TESTING

Rem
SetGraphicsDriver GLMax2DDriver()
Graphics (800, 600, 0)

Local N:Int, N2:Int = 0
Local Test:VGP = Null
Local Test2:VGP = Null
Local Test3:PVGP = Null

	SetClsColor (255, 0, 0)
	Cls ()
'	FlushMem ()
	WriteStdout (MemAlloced () + " bytes allocated")
	
	N = MilliSecs ()
	Test = VGP.LoadFPG ("fer5.fpg")
	N = MilliSecs () - N
	WriteStdout (N + " ms to Test.loadfpg fer5.fpg")
	N2:+N
	N = MilliSecs ()
	Test.ChangeRanges (45, 80, 120, 75, 85, 75, 0, 65, 100) ' Angel
	Test.ChangeRanges (90, 160, 240, 150, 170, 150, 65, 130, 255) ' Archangel
	Test.ChangeRanges (155, 230, 255, 220, 255, 220, 130, 195, 255) ' Apostle
	Test.ChangeRanges (255, 255, 255, 255, 255, 255, 255, 255, 255) ' Messiah
	Test.ChangeRanges (50, 30, 10, 200, 30, 30, 60, 60, 60) ' Fallen Angel
	Test.ChangeRanges (155, 0, 0, 255, 0, 0, 255, 0, 0) ' Abomination
	Test.ChangeRanges (20, 20, 20, 30, 30, 30, 50, 50, 50) ' Stealth Angel
	N = MilliSecs () - N
	WriteStdout (N + " ms to changeranges Test:VGP")
	N2:+N
	N = MilliSecs ()
	Test2 = Test.Copy ()
	N = MilliSecs () - N
	WriteStdout (N + " ms to copy Test:VGP to Test2:VGP")
	N2:+N
	N = MilliSecs ()
	Test3 = PVGP.LoadPFPG ("fer5.fpg")
	N = MilliSecs () - N
	WriteStdout (N + " ms to Test3.loadPfpg fer5.fpg")
	N2:+N
	N = MilliSecs ()
	Test2.BlackMask()
	N = MilliSecs () - N
	WriteStdout (N + " ms to blackmask Test2:VGO")
	N2:+N
	N = MilliSecs ()
	WriteStdout (MemAlloced () + " bytes allocated")
	FlushKeys ()
	While (Not KeyHit(KEY_ESCAPE))
		N:+1
		SetScale (1.0, 1.0)
		SetRotation (N)
		DrawImage (Test.Graph[(N / 10) Mod 4].I, 100.0, 100)
		DrawImage (Test2.Graph[(N / 10) Mod 4].I, 300.0, 100)
		DrawPixmap (Test3.Graph[(N / 10) Mod 4], 300.0, 300)
		Flip
		Cls
'		FlushMem ()
	Wend
	WriteStdout ("Freeing up...")
	Test.Unload ()
	Test2.Unload ()
	Test3.Unload ()
	Test = Null
	Test2 = Null
	Test3 = Null
'	FlushMem ()	
	WriteStdout (MemAlloced () + " bytes allocated")
	WriteStdout ("Normal program termination")

End Rem