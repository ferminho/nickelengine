' VoiD.net
' --------
'
' VBA Module
' -----------
' Started 15-04-2006
' Latest Version 29-01-2007
'
' Unif Studios 2006/2007


Strict


'Framework BRL.GlMax2D

Import BRL.Blitz
Import BRL.Endianstream
Import BRL.Filesystem
Import BRL.StandardIO

Import "voidmath.bmx"	' Maths module


Public
' ---------------------

' ANIMSTATE TYPE
' represents a character's animation state
' handles everything about obtaining current state of animations, switching anims, etc


Type AnimState
' --- 
	' Current state
	Field V:VBA = Null
	Field FrameTime1:Int = 0	' current frame and next frame time
	Field FrameTime2:Int = 0	' when 0 it means the animation is stopped/paused
	Field CurrentAnim:Int = -1
	Field CurrentFrame:VBAFrame = Null
	Field CurrentSpeed:Float = 1.0
	Field NextFrame:VBAFrame = Null
	Field Loop:Int = False
	Field Finished:Int = True	' activated when a non-loop animation is over
	' static : declared here to avoid on-the-fly redeclaration many times per sec
	' Structure where the representation data is processed and collected when necessary
	' contains RepBone with relative coordinates to the global sprite X Y Angle
	Field BoneList:TList = Null
	Field Stack1:VBABone[64]	' keeps pointers to bone (for recursive calcultation)
	Field Stack2:Byte[64]		' keeps next son number to process
	Field Stack3:Float[64]		' keeps processed angle
	Field Stack4:Float[64]		' keeps processed X
	Field Stack5:Float[64]		' keeps processed Y
	Field OnStack:Int = -1		' tells which element in the stack is the top (-1 = empty)
' --- 
	Function Create:AnimState (V:VBA)
		Local An:AnimState
		If (Not V) Then Return (Null)
		An = New AnimState
		An.V = V
		An.BoneList = CreateList ()
		Return (An)
	End Function
' --- 
	Method Update (ActTime:Float)	' ActTime = Actual moment in time
		If ((CurrentAnim = -1) Or (Not CurrentFrame) Or (FrameTime2 = 0)) Then Return
		While ((FrameTime2 < ActTime) And (FrameTime2 <> 0)) ' animation end
			CurrentFrame = NextFrame
			NextFrame = CurrentFrame.NextFrame
			If (NextFrame)
				FrameTime1 = FrameTime2
				FrameTime2 = FrameTime1 + (Float (CurrentFrame.Duration) / CurrentSpeed)
			Else
				If (Loop)
					NextFrame = V.Anim[CurrentAnim].Frames
					FrameTime1 = FrameTime2
					FrameTime2 = FrameTime1 + (Float (CurrentFrame.Duration) / CurrentSpeed)
				Else
					FrameTime2 = 0
					Finished = True
				EndIf
			EndIf
		Wend
	End Method
' --- 
	' if NewLength > 0 provided, anim's duration is overriden and adjusted to new length
	' this way a new speed playback can be specified
	Method ChangeAnim:Int (Anim:Int, ActTime:Float, Looping:Int = False, ..
							TimeDelay:Int = 0, NewLength:Int = 0)
		If (Anim < -1 Or Anim >= V.NAnims) Then Return (False)
		CurrentAnim = Anim
		Finished = False
		Loop = Looping
		CurrentSpeed = 1.0
		If (Anim > -1) 
			If (NewLength > 0 And V.Anim[Anim].Duration > 0)
				CurrentSpeed = Float (V.Anim[Anim].Duration) / Float (NewLength)
			EndIf
			If (TimeDelay <> 0)
				' delay to change anim
				NextFrame = V.Anim[Anim].Frames
				FrameTime1 = ActTime
				FrameTime2 = ActTime + TimeDelay
			Else
				' instant change
				CurrentFrame = V.Anim[Anim].Frames
				NextFrame = CurrentFrame.NextFrame
				If (NextFrame)
					FrameTime1 = ActTime
					FrameTime2 = FrameTime1 + (Float (CurrentFrame.Duration) / CurrentSpeed)
				Else
					FrameTime2 = 0
					If (Not Loop) Then Finished = True
				EndIf
			EndIf
		EndIf
		Return (True)
	End Method
' --- 
	' A new playback speed can be specified via this method
	Method ChangeAnimP:Int (Anim:Int, ActTime:Float, Looping:Int = False, ..
							TimeDelay:Int = 0, Speed:Float = 1.0)
		If (Anim < -1 Or Anim >= V.NAnims) Then Return (False)
		CurrentAnim = Anim
		Finished = False
		Loop = Looping
		CurrentSpeed = 1.0
		If (Anim > -1) 
			CurrentSpeed = Speed
			If (TimeDelay <> 0)
				' delay to change anim
				NextFrame = V.Anim[Anim].Frames
				FrameTime1 = ActTime
				FrameTime2 = ActTime + TimeDelay
			Else
				' instant change
				CurrentFrame = V.Anim[Anim].Frames
				NextFrame = CurrentFrame.NextFrame
				If (NextFrame)
					FrameTime1 = ActTime
					FrameTime2 = FrameTime1 + (Float (CurrentFrame.Duration) / CurrentSpeed)
				Else
					FrameTime2 = 0
					If (Not Loop) Then Finished = True
				EndIf
			EndIf
		EndIf
		Return (True)
	End Method
' --- 
	' Prepares the bone list with representation data
	' warning : must be provided with SizeX and SizeY not taking Zooms into account ;
	' will calculate the final position depending on the ACTUAL size on screen
	' postit : funcionara con sizex negativo para los flips? si no, arreglarlo
	Method PrepareBoneList (ActTime:Float, SizeX:Float = 1.0, SizeY:Float = 1.0, Angle = 0.0)
		Local Per1:Float, Per2:Float
		Local Bo:VBABone
		Local RelAngle:Float
		Local RelX:Float, RelY:Float
		Local BS:VBABoneState, BS2:VBABoneState
		Local I:Int = 0 ' to iterate bones sons
		Local Rep:RepBone
		Local TX:Float, TY:Float
		BoneList.Clear ()
		If (SizeX = 0.0 Or SizeY = 0.0) Then Return
		If (CurrentAnim > -1 And (CurrentFrame))
			'because if no anim, nothing to do
			If (V.NBones > 0)
				'if no bones, what are you expecting to anim?
				If ((Not NextFrame) Or (FrameTime2 = 0))
					' static anim
					Bo = V.Father
					' process first (no father)
					If (Bo)
						BS = CurrentFrame.Bones[V.Father.ID]
						RelAngle = Angle Mod 360.0
						If (RelAngle < 0.0) Then RelAngle:+ 360.0
						RelX = (BS.X * VCos[Int (RelAngle * 10.0)]) - (BS.Y * VSin[Int (RelAngle * 10.0)])
						RelY = (BS.Y * VCos[Int (RelAngle * 10.0)]) + (BS.X * VSin[Int (RelAngle * 10.0)])
						RelAngle:+  BS.Angle
						OnStack = 0
						Stack1[0] = Bo	Stack2[0] = 0	Stack3[0] = RelAngle
						Stack4[0] = RelX	Stack5[0] = RelY
						Rep = New RepBone	Rep.X = RelX * SizeX	Rep.Y = RelY * SizeY
						If ((SizeX * SizeY) < 0.0)
							Rep.Angle = -RelAngle
						Else
							Rep.Angle = RelAngle
						EndIf
						Rep.Z = BS.Z	Rep.Graph = BS.Graph
						Rep.Size = BS.Size
						BoneList.AddLast (Rep)
						While (OnStack > -1)
							Bo = Stack1[OnStack]
							If (Stack2[OnStack] < Bo.NSons)
								' still sons left to iterate
								Bo = Bo.Sons[Stack2[OnStack]]
								BS = CurrentFrame.Bones[Bo.ID]
								RelAngle = Stack3[OnStack] Mod 360.0
								If (RelAngle < 0.0) Then RelAngle:+ 360.0
								RelX = Stack4[OnStack] + (BS.X * VCos[Int (RelAngle * 10.0)]) - (BS.Y * VSin[Int (RelAngle * 10.0)])
								RelY = Stack5[OnStack] + (BS.Y * VCos[Int (RelAngle * 10.0)]) + (BS.X * VSin[Int (RelAngle * 10.0)])
								RelAngle:+ BS.Angle
								' Add to repbone list
								Rep = New RepBone	Rep.X = RelX * SizeX	Rep.Y = RelY * SizeY
								If ((SizeX * SizeY) < 0.0)
									Rep.Angle = -RelAngle
								Else
									Rep.Angle = RelAngle
								EndIf
								Rep.Size = BS.Size
								Rep.Z = BS.Z	Rep.Graph = BS.Graph
								BoneList.AddLast (Rep)
								Stack2[OnStack]:+1
								OnStack:+ 1
								Stack1[OnStack] = Bo
								Stack2[OnStack] = 0	' begin with first son
								Stack3[OnStack] = RelAngle
								Stack4[OnStack] = RelX
								Stack5[OnStack] = RelY
							Else
								' this branch is over!
								OnStack:- 1
							EndIf
						Wend
					EndIf
				Else
					'animate!
					If ((FrameTime2 - FrameTime1) = 0)
						Per2 = 1.0
					Else
						Per2 = (Float (ActTime - FrameTime1) / Float (FrameTime2 - FrameTime1))
					EndIf
					Per1 = 1.0 - Per2
					Bo = V.Father
					' process first (no father)
					If (Bo)
						BS = CurrentFrame.Bones[V.Father.ID]
						BS2 = NextFrame.Bones[V.Father.ID]
						RelAngle = Angle Mod 360.0
						If (RelAngle < 0.0) Then RelAngle:+ 360.0
						TX = (BS.X * Per1) + (BS2.X * Per2)
						TY = (BS.Y * Per1) + (BS2.Y * Per2)
						RelX = ((TX * VCos[Int (RelAngle * 10.0)]) - (TY * VSin[Int (RelAngle * 10.0)]) * SizeX)
						RelY = ((TY * VCos[Int (RelAngle * 10.0)]) + (TX * VSin[Int (RelAngle * 10.0)]) * SizeY)
						' angles are supposed to be between 0.0 and 359.99999 !
						If (Abs (BS.Angle - BS2.Angle) < 180.0) ' short route
							RelAngle:+ (BS.Angle * Per1) + (BS2.Angle * Per2)
						Else ' "long" route
							If (BS.Angle < BS2.Angle)
								RelAngle:+ (BS.Angle * Per1) + ((BS2.Angle - 360.0) * Per2)
							Else
								RelAngle:+ ((BS.Angle - 360.0) * Per1) + (BS2.Angle * Per2)
							EndIf
						EndIf
						OnStack = 0
						Stack1[0] = Bo	Stack2[0] = 0	Stack3[0] = RelAngle
						Stack4[0] = RelX	Stack5[0] = RelY
						Rep = New RepBone	Rep.X = RelX * SizeX	Rep.Y = RelY * SizeY
						If ((SizeX * SizeY) < 0.0)
							Rep.Angle = -RelAngle
						Else
							Rep.Angle = RelAngle
						EndIf
						Rep.Z = BS.Z	Rep.Graph = BS.Graph
						Rep.Size = BS.Size
						BoneList.AddLast (Rep)
						While (OnStack > -1)
							Bo = Stack1[OnStack]
							If (Stack2[OnStack] < Bo.NSons)
								' still sons left to iterate
								Bo = Bo.Sons[Stack2[OnStack]]
								BS = CurrentFrame.Bones[Bo.ID]
								BS2 = NextFrame.Bones[Bo.ID]
								RelAngle = Stack3[OnStack] Mod 360.0
								If (RelAngle < 0.0) Then RelAngle:+ 360.0
								TX = (BS.X * Per1) + (BS2.X * Per2)
								TY = (BS.Y * Per1) + (BS2.Y * Per2)
								RelX = Stack4[OnStack] + (TX * VCos[Int (RelAngle * 10.0)]) - (TY * VSin[Int (RelAngle * 10.0)])
								RelY = Stack5[OnStack] + (TY * VCos[Int (RelAngle * 10.0)]) + (TX * VSin[Int (RelAngle * 10.0)])
								' angles are supposed to be between 0.0 and 359.99999 !
								If (Abs (BS.Angle - BS2.Angle) < 180.0) ' short route
									RelAngle:+ (BS.Angle * Per1) + (BS2.Angle * Per2)
								Else ' "long" route
									If (BS.Angle < BS2.Angle)
										RelAngle:+ (BS.Angle * Per1) + ((BS2.Angle - 360.0) * Per2)
									Else
										RelAngle:+ ((BS.Angle - 360.0) * Per1) + (BS2.Angle * Per2)
									EndIf
								EndIf
								' Add to repbone list
								Rep = New RepBone	Rep.X = RelX * SizeX	Rep.Y = RelY * SizeY
								If ((SizeX * SizeY) < 0.0)
									Rep.Angle = -RelAngle
								Else
									Rep.Angle = RelAngle
								EndIf
								Rep.Z = BS.Z	Rep.Graph = BS.Graph
								Rep.Size = BS.Size
								BoneList.AddLast (Rep)
								Stack2[OnStack]:+1
								OnStack:+ 1
								Stack1[OnStack] = Bo
								Stack2[OnStack] = 0	' begin with first son
								Stack3[OnStack] = RelAngle
								Stack4[OnStack] = RelX
								Stack5[OnStack] = RelY
							Else
								' this branch is over!
								Onstack:- 1
							EndIf
						Wend
					EndIf
				EndIf
				BoneList.Sort ()
			EndIf
		EndIf
	End Method
' ---
	Method Unload () 
		Local I:Int = 0
		BoneList.Clear ()
		For I = 0 To 63
			Stack1[I] = Null
		Next
	End Method
' ---
End Type


' VBA TYPE
' represents sprite animation


Type VBA
' --- 
	Field NAnims:Int = 0	' animations number
	Field Height:Float = 0.0	' character height
	' Bones Hierarchy
	Field NBones:Int = 0	' bones number
	Field Father:VBABone = Null
	' Animations
	Field Anim:VBAAnim[0]
' ---
	Method SearchBone:VBABone (ID:Int)
		' slow method. To use only in editor
		Local StackInt:Int[65]
		Local StackBone:VBABone[65]
		Local Top:Int = 0
		Local VB:VBABone = Null
		If (ID >= NBones) Then Return (Null)
		If (Not Father) Then Return (Null)
		If (Father.ID = ID) Then Return (Father)
		StackBone[0] = Father
		StackInt[0] = 0
		While (Top > -1)
			If (StackInt[Top] < StackBone[Top].NSons)
				' still sons to search
				VB = StackBone[Top] 
				If (VB.Sons[StackInt[Top]].ID = ID) Then Return (VB.Sons[StackInt[Top]])
				StackBone[Top + 1] = VB.Sons[StackInt[Top]]
				StackInt[Top + 1] = 0
				StackInt[Top]:+1
				Top:+ 1
			Else
				' backtrack
				Top:- 1
			EndIf
		Wend
		Return (Null)
	End Method
' ---
	Method SearchBoneFather:VBABone (ID:Int)
		' slow method. To use only in editor
		Local StackInt:Int[65]
		Local StackBone:VBABone[65]
		Local Top:Int = 0
		Local VB:VBABone = Null
		If (ID >= NBones) Then Return (Null)
		If (Not Father) Then Return (Null)
		If (Father.ID = ID) Then Return (Null)
		StackBone[0] = Father
		StackInt[0] = 0
		While (Top > -1)
			If (StackInt[Top] < StackBone[Top].NSons)
				' still sons to search
				VB = StackBone[Top] 
				If (VB.Sons[StackInt[Top]].ID = ID) Then Return (StackBone[Top])
				StackBone[Top + 1] = VB.Sons[StackInt[Top]]
				StackInt[Top + 1] = 0
				StackInt[Top]:+1
				Top:+ 1
			Else
				' backtrack
				Top:- 1
			EndIf
		Wend
		Return (Null)
	End Method
' ---
	Method DeleteBone:Int (ID:Int)
		' slow method. To use only in editor
		' returns 0 if deleted, 1 if not found, 2 if has more than one son (non deletable)
		Local StackInt:Int[65]
		Local StackBone:VBABone[65]
		Local Top:Int = 0
		Local VB:VBABone = Null
		Local Frame:VBAFrame = Null
		Local I:Int, J:Int
		If (ID > (NBones - 1)) Then Return (1)
		If (Father.ID = ID)
			'it's the father
			If (Father.NSons > 1) Then Return (2)
			If (Father.NSons > 0)
				VB = Father
				Father = VB.Sons[0]
				VB.Sons = VB.Sons[..0]
				ShiftIDs (VB.ID, -1)
				VB = Null
			Else
				Father = Null
			EndIf
			NBones:- 1
			' anim fix
			For I = 0 To (NAnims - 1)
				Frame = Anim[I].Frames
				While (Frame)
					For J = ID To NBones - 1
						Frame.Bones[J] = Frame.Bones[J + 1]
					Next
					Frame.Bones = Frame.Bones[..NBones]
					For J = 0 To NBones - 1
						If (Frame.Bones[J].Z > ID) Then Frame.Bones[J].Z:- 1
					Next
					Frame = Frame.NextFrame
				Wend
			Next
			Return (0)
		EndIf
		StackBone[0] = Father
		StackInt[0] = 0
		While (Top > -1)
			If (StackInt[Top] < StackBone[Top].NSons)
				' still sons to search
				VB = StackBone[Top] 
				If (VB.Sons[StackInt[Top]].ID = ID)
					VB = VB.Sons[StackInt[Top]] 
					If (VB.NSons > 1) Then Return (2)
					If (VB.NSons > 0)
						StackBone[Top].Sons[StackInt[Top] - 1] = VB.Sons[0]
						VB.Sons = VB.Sons[..0]
						ShiftIDs (VB.ID, -1)
						VB = Null
					Else
						StackBone[Top].NSons:-1
						For I = StackInt[Top] To (StackBone[Top].NSons - 1)
							StackBone[Top].Sons[I] = StackBone[Top].Sons[I + 1]
						Next
						StackBone[Top].Sons = StackBone[Top].Sons[..StackBone[Top].NSons]
						VB.Sons = VB.Sons[..0]
						ShiftIDs (VB.ID, -1)
						VB = Null
					EndIf
					NBones:- 1
					' anim fix
					For I = 0 To (NAnims - 1)
						Frame = Anim[I].Frames
						While (Frame)
							For J = ID To NBones - 1
								Frame.Bones[J] = Frame.Bones[J + 1]
							Next
							Frame.Bones = Frame.Bones[..NBones]
							For J = 0 To NBones - 1
								If (Frame.Bones[J].Z > ID) Then Frame.Bones[J].Z:- 1
							Next
							Frame = Frame.NextFrame
						Wend
					Next
					Return (0)
				EndIf
				StackBone[Top + 1] = VB.Sons[StackInt[Top]]
				StackInt[Top]:+1
				StackInt[Top + 1] = 0
				Top:+ 1
			Else
				' backtrack
				Top:- 1
			EndIf
		Wend
		Return (1)
	End Method
' ---
	Method ShiftIDs (Start:Int, Value:Int)
		' slow method. To use only in editor
		Local StackInt:Int[65]
		Local StackBone:VBABone[65]
		Local Top:Int = 0
		Local VB:VBABone = Null
		If (Not Father) Then Return
		StackBone[0] = Father
		StackInt[0] = 0
		While (Top > -1)
			If (StackInt[Top] < StackBone[Top].NSons)
				' still sons to search
				VB = StackBone[Top] 
				If (VB.Sons[StackInt[Top]].ID >= Start) Then VB.Sons[StackInt[Top]].ID:+ Value
				StackBone[Top + 1] = VB.Sons[StackInt[Top]]
				StackInt[Top]:+1
				StackInt[Top + 1] = 0
				Top:+ 1
			Else
				' backtrack
				Top:- 1
			EndIf
		Wend
	End Method
' ---
	Method DeleteAnim (N:Int)
		Local I:Int
		Anim[N].ClearFrames ()
		NAnims:- 1
		For I = N To (NAnims - 1)
			Anim[I] = Anim[I + 1]
		Next
	End Method
' ---
	Method InsertAnim (N:Int)
		Local I:Int
		Anim = Anim[..NAnims + 1]
		NAnims:+ 1
		For I = (N + 1) To (NAnims - 1)
			Anim[I] = Anim[I - 1]
		Next
		Anim[I] = New VBAAnim
	End Method
' ---
	Function LoadVBA:VBA (File:String)
		Local FH:TStream = LittleEndianStream (OpenStream (File, True, False))
		Local V:VBA = New VBA
		Local Version:Int = 0
		Local StackBone:VBABone[65]
		Local VB:VBABone = Null
		Local Frame:VBAFrame = Null
		Local I:Int = 0, J:Int = 0
		Local OnStack:Int = 0
		
		If (Not FH) Then Crash ("VBA " + File$ + " not found")
		
		If (FH.ReadByte () <> Asc ("v")) Then Crash ("VBA " + File$ + " bad file format")
		If (FH.ReadByte () <> Asc (".")) Then Crash ("VBA " + File$ + " bad file format")
		If (FH.ReadByte () <> Asc ("n")) Then Crash ("VBA " + File$ + " bad file format")
		
		Version = FH.ReadByte ()
		If (Version > VBAVersion) Then Crash ("VBA " + File$ + " wrong version")

		If (FH.ReadByte () <> Asc ("v")) Then Crash ("VBA " + File$ + " bad file format")
		If (FH.ReadByte () <> Asc ("b")) Then Crash ("VBA " + File$ + " bad file format")
		If (FH.ReadByte () <> Asc ("a")) Then Crash ("VBA " + File$ + " bad file format")
		
		V.Height = FH.ReadShort ()
		
		' base bone
		I = FH.ReadByte ()
		If (I > 63) Then Crash ("VBA " + File$ + " bad file format")
		V.NBones = 1
		V.Father = New VBABone
		V.Father.ID = I
		V.Father.Name = FH.ReadLine ()
		V.Father.Range1 = FH.ReadShort ()
		V.Father.Range2 = FH.ReadShort ()
		StackBone[0] = V.Father
		OnStack = 0
		While (OnStack > -1)
			I = FH.ReadByte ()
			If (I < 255)
				If (I > 63) Then Crash ("VBA " + File$ + " bad file format")
				V.NBones:+ 1
				StackBone[OnStack].NSons:+ 1
				StackBone[OnStack].Sons = StackBone[OnStack].Sons [..StackBone[OnStack].NSons]
				StackBone[OnStack].Sons[StackBone[OnStack].NSons - 1] = New VBABone
				VB = StackBone[OnStack].Sons[StackBone[OnStack].NSons - 1]
				VB.ID = I
				VB.Name = FH.ReadLine ()
				VB.Range1 = FH.ReadShort ()
				VB.Range2 = FH.ReadShort ()
				OnStack:+ 1
				StackBone[OnStack] = VB
			Else
				OnStack:- 1
			EndIf
		Wend
		' anims
		While (Not FH.Eof ())
			V.NAnims:+ 1
			V.Anim = V.Anim[..V.NAnims]
			V.Anim[V.NAnims - 1] = New VBAAnim
			V.Anim[V.NAnims - 1].NFrames = FH.ReadShort ()
			V.Anim[V.NAnims - 1].Duration = 0
			V.Anim[V.NAnims - 1].Frames = New VBAFrame
			' frame base
			Frame = V.Anim[V.NAnims - 1].Frames
			Frame.Duration = FH.ReadShort ()
			V.Anim[V.NAnims - 1].Duration :+ Frame.Duration
			Frame.Bones = Frame.Bones[..V.NBones]
			For J = 0 To V.NBones - 1
				Frame.Bones[J] = New VBABoneState
				Frame.Bones[J].X = FH.ReadFloat ()
				Frame.Bones[J].Y = FH.ReadFloat ()
				Frame.Bones[J].Angle = FH.ReadFloat ()
				Frame.Bones[J].Z = FH.ReadShort ()
				If (Version > 1) 
					Frame.Bones[J].Size = FH.ReadFloat ()
				Else
					Frame.Bones[J].Size = 1.0
				EndIf
				Frame.Bones[J].Graph = FH.ReadShort ()
			Next
			For I = 1 To (V.Anim[V.NAnims - 1].NFrames - 1)
				Frame.NextFrame = New VBAFrame
				Frame = Frame.NextFrame
				Frame.Duration = FH.ReadShort ()
				V.Anim[V.NAnims - 1].Duration :+ Frame.Duration
				Frame.Bones = Frame.Bones[..V.NBones]
				For J = 0 To V.NBones - 1
					Frame.Bones[J] = New VBABoneState
					Frame.Bones[J].X = FH.ReadFloat ()
					Frame.Bones[J].Y = FH.ReadFloat ()
					Frame.Bones[J].Angle = FH.ReadFloat ()
					Frame.Bones[J].Z = FH.ReadShort ()
					If (Version > 1) 
						Frame.Bones[J].Size = FH.ReadFloat ()
					Else
						Frame.Bones[J].Size = 1.0
					EndIf
					Frame.Bones[J].Graph = FH.ReadShort ()
				Next
			Next
		Wend
		FH.Close ()
		FH = Null
		Return (V)
	End Function
' ---
	Method SaveVBA (File:String)
		Local FH:TStream = LittleEndianStream (WriteStream (File))
		Local V:VBA = New VBA
		Local StackBone:VBABone[65]
		Local StackInt:Int[65]
		Local VB:VBABone = Null
		Local Frame:VBAFrame = Null
		Local I:Int = 0, J:Int = 0
		Local OnStack:Int = 0
		
		
		If (Not FH) Then Crash ("Could not create VBA " + File$)
		
		FH.WriteByte (Asc ("v"))
		FH.WriteByte (Asc ("."))
		FH.WriteByte (Asc ("n"))
		FH.WriteByte (VBAVersion)
		FH.WriteByte (Asc ("v"))
		FH.WriteByte (Asc ("b"))
		FH.WriteByte (Asc ("a"))
		
		FH.WriteShort (Height)
		
		' base bone
		FH.WriteByte (Father.ID)
		FH.WriteLine (Father.Name)
		FH.WriteShort (Father.Range1)
		FH.WriteShort (Father.Range2)
		
		StackBone[0] = Father
		StackInt[0] = 0
		OnStack = 0
		While (OnStack > -1)
			If (StackInt[OnStack] < StackBone[OnStack].NSons)
				VB = StackBone[OnStack].Sons[StackInt[OnStack]]
				FH.WriteByte (VB.ID)
				FH.WriteLine (VB.Name)
				FH.WriteShort (VB.Range1)
				FH.WriteShort (VB.Range2)
				StackInt[OnStack]:+ 1
				StackInt[OnStack + 1] = 0
				StackBone[OnStack + 1] = VB
				OnStack:+ 1
			Else
				OnStack:- 1
				FH.WriteByte (255)
			EndIf
		Wend
		
		For I = 0 To (NAnims - 1)
			FH.WriteShort (Anim[I].NFrames)
			Frame = Anim[I].Frames
			While (Frame)
				FH.WriteShort (Frame.Duration)
				For J = 0 To (NBones - 1)
					FH.WriteFloat (Frame.Bones[J].X)
					FH.WriteFloat (Frame.Bones[J].Y)
					FH.WriteFloat (Frame.Bones[J].Angle)
					FH.WriteShort (Frame.Bones[J].Z)
					FH.WriteFloat (Frame.Bones[J].Size)
					FH.WriteShort (Frame.Bones[J].Graph)
				Next
				Frame = Frame.NextFrame
			Wend
		Next
		FH.Close ()
		FH = Null
	End Method
' ---
	Method Unload ()
		Local I:Int
		Local F:VBAFrame = Null
		Local F2:VBAFrame = Null
		Local StackInt:Int[65]
		Local StackBone:VBABone[65]
		Local Top:Int = 0
		Local VB:VBABone = Null
		For I = 0 To (NAnims - 1)
			Anim[I].ClearFrames ()
			Anim[I] = Null
		Next
		Anim = Anim[..0]
		If (Not Father) Then Return
		StackBone[0] = Father
		StackInt[0] = 0
		While (Top > -1)
			If (StackInt[Top] < StackBone[Top].NSons)
				' still sons to iterate
				VB = StackBone[Top] 
				StackBone[Top + 1] = VB.Sons[StackInt[Top]]
				StackInt[Top + 1] = 0
				StackInt[Top]:+1
				Top:+ 1
			Else
				' backtrack, but before kill all this sons
				StackBone[Top].Sons = StackBone[Top].Sons[..0]
				Top:- 1
			EndIf
		Wend
		Father = Null
	End Method
' ---
End Type


Type VBABone	' bone
' ---
	Field ID:Int = 0	' unique id, from 0 to bones-1
	Field Name:String = ""	' important only for the editor
	Field Range1:Int = 1	' same
	Field Range2:Int = 1	' same
' ---
	Field NSons:Int = 0	' sons
	Field Sons:VBABone[0]
' ---
End Type


Type RepBone	' Bone representation info
' ---
	' all relative to father X, Y
	Field X:Float = 0.0
	Field Y:Float = 0.0
	Field Z:Int = 0
	Field Angle:Float = 0.0
	Field Size:Float = 0.0
	Field Graph:Int = 0
' ---
	Method Compare (O:Object) Final
		Local Comp:RepBone = RepBone (O)
		If (Z > Comp.Z)
			Return (-1)
		ElseIf (Z < Comp.Z)
			Return (1)
		Else
			Return (0)
		EndIf
	End Method
' ---
End Type


Type VBAAnim
' ---
	Field NFrames:Int = 0
	Field Frames:VBAFrame = Null
	Field Duration:Int = 0
' ---
	Method CountTime:Int ()
		Local F:VBAFrame = Frames
		Local I:Int = 0
		While (F)
			I:+ F.Duration
			F = F.NextFrame
		Wend
		Return (I)
	End Method
' ---
	Method SearchFrame:VBAFrame (Count:Int)
		Local F:VBAFrame = Frames
		Local I:Int = 0
		While (F <> Null)
			If (I = Count) Then Return (F)
			I:+ 1
			F = F.NextFrame
		Wend
		Return (Null)
	End Method
' ---
	Method ClearFrames ()
		Local F:VBAFrame
		Local F2:VBAFrame
		F = Frames
		While (F)
			F.Bones = F.Bones[..0]
			F2 = F
			F = F.NextFrame
			F2.NextFrame = Null
		Wend
		NFrames = 0
		Frames = Null
	End Method
' ---
	Method DelFrame (Count:Int)
		Local F:VBAFrame = Frames
		Local I:Int = 0
		
		If (Count = 0)
			Frames.Bones = Frames.Bones[..0]
			Frames = Frames.NextFrame
		Else
			F = Frames
			While (F <> Null)
				If (I = (Count - 1))
					F.NextFrame.Bones = F.NextFrame.Bones[..0]
					F.NextFrame = F.NextFrame.NextFrame
					Exit
				EndIf
				I:+ 1
				F = F.NextFrame
			Wend
		EndIf
		NFrames:- 1
	End Method
' ---
End Type


Type VBAFrame
' ---
	Field Duration:Int = 0
	Field Bones:VBABoneState[0]
	Field NextFrame:VBAFrame = Null
' ---
	Method Copy:VBAFrame ()
		Local F:VBAFrame = New VBAFrame
		Local I:Int
		F.Duration = Duration
		F.NextFrame = Null
		If (Bones.Dimensions())
			F.Bones = F.Bones[0..Bones.Dimensions()[0]]
			For I = 0 To (Bones.Dimensions ()[0] - 1)
				F.Bones[I] = Bones[I].Copy ()
			Next
		EndIf
		Return (F)
	End Method
' ---
End Type


Type VBABoneState
' ---
	Field X:Float = 0.0
	Field Y:Float = 0.0
	Field Angle:Float = 0.0
	Field Size:Float = 1.0
	Field Z:Int = 0
	Field Graph:Int = 0
' ---
	Method Copy:VBABoneState ()
		Local VBS:VBABoneState = New VBABoneState
		VBS.X = X
		VBS.Y = Y
		VBS.Angle = Angle
		VBS.Z = Z
		VBS.Size = Size
		VBS.Graph = Graph
		Return VBS
	End Method
' ---
End Type


Private
'------

Const VBAVersion:Byte = 2

Function Crash (S:String)
	Notify (S)
	Print (S)
	Print ("VoiD Engine: VBA module abnormal termination")
	End
End Function


