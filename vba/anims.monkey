Strict

Private

Import mojo

Import math.mathutils

Public

' Animator: main way of using an AnimationPack
' It will keep the animation status for a given entity, meaning that 
'    each entity should have its animator even if they use the same AnimationPack
Class Animator
Private
	
	Field pack:AnimationPack = Null
	Field frameTime1:Int = 0	' current frame time
	Field frameTime2:Int = 0	' next frame time (0 = animation stopped)
	Field currentSpeed:Float = 1.0
	Field currentAnim:Animation = Null
	Field currentAnimId:Int = -1
	Field currentFrame:Frame = Null
	Field nextFrame:Frame = Null
	Field loop:Bool = False
	Field finished:Bool = True ' activated when non-loop anim is over
		
	' internal temporary stuff declared as attributes to avoid reallocating constantly:
	Field stack1:Bone[64] ' pointers to bone for recursive calculations
	Field stack2:Int[64] ' next son number to process
	Field stack3:Float[64] ' processed angles
	Field stack4:Float[64] ' processed X
	Field stack5:Float[64] ' processed Y
	Field topStack:Int ' tells which element in the stack is on top (-1 = none / empty)

Public

	Method New(pack:AnimationPack)
		Self.pack = pack
		If (pack = Null)
			Throw New Throwable
		End If
	End Method
	
	Method GetCurrentAnimation:Animation()
		Return currentAnim
	End Method

	Method GetCurrentAnimationId:Int()
		Return currentAnimId
	End Method
			
	Method GetLoop:Bool()
		Return loop
	End Method
	
	Method GetFinished:Bool()
		Return finished
	End Method
	
	Method Update:Void(currentTimeMs:Float)
		If (currentAnim <> Null And currentFrame <> Null And frameTime2 <> 0)
			While (frameTime2 < currentTimeMs And frameTime2 <> 0) ' animation end
				currentFrame = nextFrame
				nextFrame = currentFrame.nextFrame
				If (nextFrame <> Null)
					frameTime1 = frameTime2
					frameTime2 = frameTime2 + (Float(currentFrame.duration / currentSpeed))
				Else
					If (loop)
						nextFrame = currentAnim.frames
						frameTime1 = frameTime2
						frameTime2 = frameTime2 + (Float(currentFrame.duration / currentSpeed))
					Else
						frameTime2 = 0
						finished = True						
					End If
				End If
			End While
		End If
	End Method
	
	' transitionTime may be specified to morph the current animation into the new one over the specified time
	' newLength may be specified to set a specific animation length (duration)
	' speed may be altered to vary the animation speed (both newLength and this will affet speed, in fact)
	Method ChangeAnimation:Void(animId:Int, currentTimeMs:Float, loop:Bool = False,
								transitionTime:Int = 0, newLength:Int = 0, speed:Float = 1.0)
		currentAnimId = animId
		ChangeAnimation(pack.GetAnimation(animId), currentTimeMs, loop, transitionTime, newLength, speed)
	End Method
	Method ChangeAnimation:Void(anim:Animation, currentTimeMs:Float, loop:Bool = False,
								transitionTime:Int = 0, newLength:Int = 0, speed:Float = 1.0)
		If (anim <> Null)
			currentAnim = anim
			finished = False
			Self.loop = loop
			currentSpeed = speed
			
			If (newLength > 0 And anim.duration > 0)
				currentSpeed = currentSpeed * (Float(anim.duration) / newLength)
			End If
			
			If (transitionTime > 0)
				nextFrame = anim.frames
				frameTime1 = currentTimeMs
				frameTime2 = currentTimeMs + transitionTime
			Else
				' instant change
				currentFrame = anim.frames
				nextFrame = currentFrame.nextFrame
				If (nextFrame <> Null)
					frameTime1 = currentTimeMs
					frameTime2 = currentTimeMs + (Float(currentFrame.duration) / currentSpeed)
				Else
					frameTime2 = 0
					finished = Not loop
				End If
			End If
		Else
			currentAnimId = -1
			currentAnim = Null
		End If
	End Method
	
	' Prepares the bone list with representation data
	' warning : must be provided with SizeX and SizeY not taking Zooms into account ;
	' will calculate the final position depending on the ACTUAL size on screen	
	Method PrepareBonesList:List<BoneState>(currentTimeMs:Float, sizeX:Float = 1.0,
										sizeY:Float = 1.0, angle:Float = 0.0)
		Local percent1:Float = 0.0
		Local percent2:Float = 0.0
		Local boneState1:BoneState = Null
		Local boneState2:BoneState = Null
		Local bone:Bone = Null
		Local repBone:BoneState = Null
		Local relAngle:Float = 0.0
		Local relX:Float = 0.0
		Local relY:Float = 0.0
		Local tempX:Float = 0.0
		Local tempY:Float = 0.0
		
		Local boneList:BoneStateList = New BoneStateList()
		
		Update(currentTimeMs)
		
		If (currentAnim <> Null And currentFrame <> Null And pack.bonesNumber > 0)
			If (nextFrame = Null Or frameTime2 = 0)
				' static anim
				bone = pack.parentBone
				' start with first (no parent)
				boneState1 = currentFrame.boneStates[bone.id]
				relAngle = Math.NormalizeAngle(relAngle)
				relX = (boneState1.x * Math.COS[Int(relAngle * 10.0)]) - (boneState1.y * Math.SIN[Int(relAngle * 10.0)])
				relY = (-boneState1.y * Math.COS[Int(relAngle * 10.0)]) + (boneState1.x * Math.SIN[Int(relAngle * 10.0)])
				relAngle = relAngle + boneState1.angle
				topStack = 0
				stack1[0] = bone
				stack2[0] = 0
				stack3[0] = relAngle
				stack4[0] = relX
				stack5[0] = relY
				repBone = New BoneState(relX * sizeX, relY * sizeY, relAngle, boneState1.size, boneState1.z, boneState1.graph)
				If ( (sizeX * sizeY) < 0.0)
					repBone.angle = -relAngle
				End If
				boneList.AddLast(repBone)
				
				While (topStack > - 1)
					bone = stack1[topStack]
					If (stack2[topStack] < bone.sons.Length)
						' sons left to iterate
						bone = bone.sons[stack2[topStack]]
						boneState1 = currentFrame.boneStates[bone.id]
						relAngle = Math.NormalizeAngle(stack3[topStack])
						relX = stack4[topStack] + (boneState1.x * Math.COS[Int(relAngle * 10.0)]) - (boneState1.y * Math.SIN[Int(relAngle * 10.0)])
						relY = stack5[topStack] + (boneState1.y * Math.COS[Int(relAngle * 10.0)]) + (boneState1.x * Math.SIN[Int(relAngle * 10.0)])
						relAngle = relAngle + boneState1.angle
						' add to list
						repBone = New BoneState(relX * sizeX, relY * sizeY, relAngle, boneState1.size, boneState1.z, boneState1.graph)
						If ( (sizeX * sizeY) < 0.0)
							repBone.angle = -relAngle
						End If
						boneList.AddLast(repBone)
						
						stack2[topStack] = stack2[topStack] + 1
						topStack = topStack + 1
						stack1[topStack] = bone
						stack2[topStack] = 0 ' begin with first son
						stack3[topStack] = relAngle
						stack4[topStack] = relX
						stack5[topStack] = relY
					Else
						' branch over
						topStack = topStack - 1
					End If
				End While
			Else
				' animate!
				If ( (frameTime2 - frameTime1) = 0)
					percent2 = 1.0
				Else
					percent2 = (Float(currentTimeMs - frameTime1) / Float(frameTime2 - frameTime1))
				End If
				percent1 = 1.0 - percent2
				bone = pack.parentBone
				' start with first (no parent)
				boneState1 = currentFrame.boneStates[pack.parentBone.id]
				boneState2 = nextFrame.boneStates[pack.parentBone.id]
				relAngle = Math.NormalizeAngle(angle)
				tempX = (boneState1.x * percent1) + (boneState2.x * percent2)
				tempY = (boneState1.y * percent1) + (boneState2.y * percent2)
				relX = ( (tempX * Math.COS[Int(relAngle * 10.0)]) - (tempY * Math.SIN[Int(relAngle * 10.0)])) * sizeX
				relY = ( (tempY * Math.COS[Int(relAngle * 10.0)]) + (tempX * Math.SIN[Int(relAngle * 10.0)])) * sizeY
				' angles are supposed to be between 0.0 and 359.99999 !
				If (Abs(boneState1.angle - boneState2.angle) < 180.0) ' short route
					relAngle = relAngle + (boneState1.angle * percent1) + (boneState2.angle * percent2)
				Else ' long route
					If (boneState1.angle < boneState2.angle)
						relAngle = relAngle + (boneState1.angle * percent1) + ( (boneState2.angle - 360.0) * percent2)
					Else
						relAngle = relAngle + ( (boneState1.angle - 360.0) * percent1) + (boneState2.angle * percent2)
					End If
				EndIf
				
				topStack = 0
				stack1[0] = bone
				stack2[0] = 0
				stack3[0] = relAngle
				stack4[0] = relX
				stack5[0] = relY
				repBone = New BoneState(relX * sizeX, relY * sizeY, relAngle, boneState1.size, boneState1.z, boneState1.graph)
				If ( (sizeX * sizeY) < 0.0)
					repBone.angle = -relAngle
				End If
				boneList.AddLast(repBone)

				While (topStack > - 1)
					bone = stack1[topStack]
					If (stack2[topStack] < bone.sons.Length)
						' sons left to iterate
						bone = bone.sons[stack2[topStack]]
						boneState1 = currentFrame.boneStates[bone.id]
						boneState2 = nextFrame.boneStates[bone.id]
						relAngle = Math.NormalizeAngle(stack3[topStack])
						
						tempX = (boneState1.x * percent1) + (boneState2.x * percent2)
						tempY = (boneState1.y * percent1) + (boneState2.y * percent2)
						relX = stack4[topStack] + ( ( (tempX * Math.COS[Int(relAngle * 10.0)]) - (tempY * Math.SIN[Int(relAngle * 10.0)])) * sizeX)
						relY = stack5[topStack] + ( ( (tempY * Math.COS[Int(relAngle * 10.0)]) + (tempX * Math.SIN[Int(relAngle * 10.0)])) * sizeY)
						' angles are supposed to be between 0.0 and 359.99999 !
						If (Abs(boneState1.angle - boneState2.angle) < 180.0) ' short route
							relAngle = relAngle + (boneState1.angle * percent1) + (boneState2.angle * percent2)
						Else ' long route
							If (boneState1.angle < boneState2.angle)
								relAngle = relAngle + (boneState1.angle * percent1) + ( (boneState2.angle - 360.0) * percent2)
							Else
								relAngle = relAngle + ( (boneState1.angle - 360.0) * percent1) + (boneState2.angle * percent2)
							End If
						EndIf
						' add to list
						repBone = New BoneState(relX * sizeX, relY * sizeY, relAngle, boneState1.size, boneState1.z, boneState1.graph)
						If ( (sizeX * sizeY) < 0.0)
							repBone.angle = -relAngle
						End If
						boneList.AddLast(repBone)
						
						stack2[topStack] = stack2[topStack] + 1
						topStack = topStack + 1
						stack1[topStack] = bone
						stack2[topStack] = 0 ' begin with first son
						stack3[topStack] = relAngle
						stack4[topStack] = relX
						stack5[topStack] = relY
						
					Else
						' branch over
						topStack = topStack - 1
					End If
				End While								
								
			End If
			boneList.Sort()
		End If
		
		Return boneList
	End Method
	
	Method Destroy:Void()
		pack = Null
		boneStack =[]
		sonStack =[]
		angleStack =[]
		xStack =[]
		yStack =[]
		topStack = -1
		currentAnim = Null
		currentFrame = Null
		nextFrame = Null
	End Method
		
End Class


' Package or collection of animations, formerly known as VBA
Class AnimationPack
Private
		
	Field height:Float = 0.0
	Field bonesNumber:Int = 0
	Field parentBone:Bone = Null
	Field animations:Animation[] =[]
	
	
Public
	
	Method GetAnimation:Animation(animId:Int)
		' yeah, no safe checks. Before, the field was just accessed directly for the sake of speed.
		' so I want at least a getter, even if it's unsafe
		Return animations[animId]
	End Method
	
	Method GetAnimationsNumber:Int()
		Return animations.Length
	End Method
	
	Function LoadUAP:AnimationPack(file:String)
		Local result:AnimationPack = Null
		Local fileContent:String = LoadString(file)
		Local sr:StringReader = Null
		Local boneSr:StringReader = Null
		Local animSr:StringReader = Null
		
		Print("Loading UAP from " + file)
		
		If (fileContent.Trim().Length > 0)
			result = New AnimationPack()
			sr = New StringReader(fileContent)
			result.height = sr.ReadFloat()
			result.bonesNumber = sr.ReadInt()
			boneSr = New StringReader(sr.ReadSection())
			result.parentBone = Bone.LoadFromUAP(boneSr)
			animSr = New StringReader(sr.ReadSection())
			result.animations = Animation.LoadFromUAP(animSr)
		End If
		
		Return result
	End Function
	
End Class


' Represents a "bone", that is, a default sprite and a position relative to a parent bone, with child bones
Class Bone
Private
	
	Field id:Int = 0
	Field sons:Bone[] =[]
	'TODO: editor-related info (which is stored in the file but not loaded yet)
	
Public
	
	Function LoadFromUAP:Bone(sr:StringReader)
		Local result:Bone = Null
		
		If (sr.HasValue())
			result = New Bone()
			result.id = sr.ReadInt()
			sr.ReadString() 'TODO: name - unused yet
			sr.ReadInt() 'TODO: range1 - unused yet
			sr.ReadInt() 'TODO: range2 - unused yet
			
			'children
			sr.ReadSubSectionStart()
			result.sons =[]
			Local nextSubsectionStart:Int = sr.str.Find(StringReader.SubSectionStart)
			Local nextSubsectionEnd:Int = sr.str.Find(StringReader.SubSectionEnd)

			While (nextSubsectionStart >= 0 And nextSubsectionStart < nextSubsectionEnd)
				'there are more sons
				result.sons = result.sons.Resize(result.sons.Length + 1)
				result.sons[result.sons.Length - 1] = Bone.LoadFromUAP(sr)
				
				nextSubsectionStart = sr.str.Find(StringReader.SubSectionStart)
				nextSubsectionEnd = sr.str.Find(StringReader.SubSectionEnd)
			End While
			sr.ReadSubSectionEnd()
						
		End If
		
		Return result
	End Function
	
	Method Destroy:Void()
		
	End Method
	
End Class


' A single animations, consisting on keyframes containing bone positions, angles and sprites
Class Animation
Private
	
	Field frames:Frame
	Field duration:Int = 0
	
	
Public

	Function LoadFromUAP:Animation[](sr:StringReader)
		Local animationsContent:String[] = sr.str.Split(StringReader.AnimSeparator)
		Local animations:Animation[] =[]
		
		
		If (animationsContent.Length = 1 And animationsContent[0].Trim().Length = 0)
			Return []
		End If
		
		animations = New Animation[animationsContent.Length]
		
		For Local i:Int = 0 To animationsContent.Length - 1
			animations[i] = New Animation()
			animations[i].frames = Frame.LoadFromUAP(animationsContent[i].Split(StringReader.FrameSeparator))
			If (animations[i].frames <> Null)
				animations[i].duration = animations[i].frames.GetTotalDuration()
			End If
		End For
		
		Return animations
	End Function
	
	Method Destroy:Void()
		If (frames <> Null)
			frames.Destroy()
		End If
	End Method

End Class


' A (key)frame inside an animation
Class Frame
Private
	
	Field duration:Int = 0
	Field boneStates:BoneState[] =[]
	Field nextFrame:Frame = Null
	
Public
	
	Method GetTotalDuration:Int()
		If (nextFrame <> Null)
			Return duration + nextFrame.GetTotalDuration()
		Else
			Return duration
		End If
	End Method
	
	Function LoadFromUAP:Frame(framesContent:String[])
		If (framesContent.Length = 0)
			Return Null
		End If
		
		Local sr:StringReader = New StringReader(framesContent[0])
		
		If ( Not sr.HasValue())
			Return Null
		End If
		
		Local frame:Frame = New Frame()
		Local boneStatesContent:String[] =[]
		frame.duration = sr.ReadInt()
		boneStatesContent = sr.str.Split(StringReader.BoneStateSeparator)
		frame.boneStates = New BoneState[boneStatesContent.Length]

		For Local i:Int = 0 To boneStatesContent.Length - 1
			frame.boneStates[i] = BoneState.LoadFromUAP(New StringReader(boneStatesContent[i]))
		End For

		frame.nextFrame = Frame.LoadFromUAP(framesContent[1 ..])
		
		Return frame
	End Function
	
	Method Destroy:Void()
		For Local boneState:BoneState = EachIn boneStates
			boneState.Destroy()
		End For
		boneStates =[]
		If (nextFrame <> Null)
			nextFrame.Destroy()
			nextFrame = Null
		End If
	End Method
	
End Class


' A class depicting a particular state of any given bone
Class BoneState
Public
'Private ' you know. Access as quickly as possible... in fact this is not much more than a struct

	Field x:Float = 0.0
	Field y:Float = 0.0
	Field angle:Float = 0.0
	Field size:Float = 1.0
	Field z:Int = 0
	Field graph:Int = 0
	
	
Public
	
	Method New(x:Float, y:Float, angle:Float, size:Float, z:Int, graph:Int)
		Self.x = x
		Self.y = y
		Self.angle = angle
		Self.size = size
		Self.z = z
		Self.graph = graph
	End Method
	
	Function LoadFromUAP:BoneState(sr:StringReader)
		Local bs:BoneState = New BoneState()
		
		bs.x = sr.ReadFloat()
		bs.y = sr.ReadFloat()
		bs.angle = sr.ReadFloat()
		bs.z = sr.ReadInt()
		bs.size = sr.ReadFloat()
		bs.graph = sr.ReadInt()
		
		Return bs
	End Function
	
	Method Destroy:Void()
	End Method
End Class


Private


' A list of BoneStates
Class BoneStateList Extends List<BoneState>

	Method Equals:Bool(lhs:BoneState, rhs:BoneState)
		Return lhs.graph = rhs.graph And lhs.angle = rhs.angle And lhs.x = rhs.x And lhs.y = rhs.y And lhs.z = rhs.z And lhs.size = rhs.size
	End
	
	Method Compare:Int(lhs:BoneState, rhs:BoneState)
		Return rhs.z - lhs.z
	End
		
End Class


' An utility class for reading from string-files
Class StringReader
	Const Delimiter:String = "~~~~"
	Const AnimSeparator:String = "::"
	Const FrameSeparator:String = "||"
	Const BoneStateSeparator:String = "##"
	Const SectionStart:String = "[["
	Const SectionEnd:String = "]]"
	Const SubSectionStart:String = "{{"
	Const SubSectionEnd:String = "}}"

	Field str:String = ""

	Method New(str:String)
		Self.str = str
	End Method

	Method HasValue:Bool()
		Local i:Int = str.Find(Delimiter)
		Return i >= 0
	End Method
	
	Method ReadFloat:Float()
		Local i:Int = str.Find(Delimiter)
		Local j:Int = str.Find(Delimiter, i + 2)
		Local f:Float = Float(str[i + 2 .. j])
		str = str[j + 2 ..]
		Return f
	End Method

	Method ReadInt:Float()
		Local i:Int = str.Find(Delimiter)
		Local j:Int = str.Find(Delimiter, i + 2)
		Local in:Int = Int(str[i + 2 .. j])
		str = str[j + 2 ..]
		Return in
	End Method
	
	Method ReadString:String()
		Local i:Int = str.Find(Delimiter)
		Local j:Int = str.Find(Delimiter, i + 2)
		Local s:String = str[i + 2 .. j]
		str = str[j + 2 ..]
		Return s
	End Method
	
	Method ReadSection:String()
		Local i:Int = str.Find(SectionStart)
		Local j:Int = str.Find(SectionEnd, i + 2)
		Local s:String = str[i + 2 .. j]
		str = str[j + 2 ..]
		Return s
	End Method
	
	Method ReadSubSectionStart:Void()
		Local i:Int = str.Find(SubSectionStart)
		If (i >= 0)
			str = str[i + 2 ..]
		End If
	End Method

	Method ReadSubSectionEnd:Void()
		Local i:Int = str.Find(SubSectionEnd)
		If (i >= 0)
			str = str[i + 2 ..]
		End If
	End Method

End Class