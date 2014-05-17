Strict

Private

Import mojo

Public

Class FPSCounter
Public

	Field drawFPSEnabled:Bool = True

Private

	Field fps:Int = 0
	Field fpsCounter:Int = 0
	Field fpsTime:Int = 0
	Field lastMillisecs:Int = 0
	Const FPS_COUNTER_FRAMES:Int = 60

Public

	Global instance:FPSCounter = New FPSCounter()
	
	Method GetFPS:Int()
		Return fps
	End Method

	Method Initialize:Void()
		lastMillisecs = Millisecs()
		fps = 0
		fpsCounter = 0
		fpsTime = 0
	End Method

	Method NotifyNewFrame:Void()
		Local now:Int = Millisecs()
		Local lastFrame:Int = now - lastMillisecs
		
		fpsTime += lastFrame
		fpsCounter += 1
		If (fpsCounter = FPS_COUNTER_FRAMES)
			fps = (FPS_COUNTER_FRAMES * 1000) / fpsTime
			fpsTime = 0
			fpsCounter = 0
		End If
		lastMillisecs = now
		
		#If CONFIG="debug"
			If (drawFPSEnabled)
				DrawFPS()
			End If
		#End
	End Method
	
	Method DrawFPS:Void()
		PushMatrix()
		
		Local color:Float = (lastMillisecs / 5) Mod 255.0
		SetColor(color, color, color)
		SetBlend(AlphaBlend)
		SetAlpha(1.0)
		Translate(5.0, 5.0)
		Scale(1.5, 1.5)
		DrawText(fps, 0.0, 0.0)
		PopMatrix()
	End Method

End Class