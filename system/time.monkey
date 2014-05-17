Strict

Private

Import mojo

Import NickelEngine.math.mathutils

Public

'ALL values are in plain millisecs - the class uses internally a decimal counter, but returned values are millisecs. 
Class Time
Public

	Global instance:Time = New Time()
	' real timer
	Field mainTimer:Int = 0
	' timer affected by time distortion
	Field processedTimer:Int = 0
	' last frame millisecs, int or float
	Field lastFrame:Int = 0
	Field lastFrameF:Float = 0.0
	' last frame millisecs affected by time distortion, int or float
	Field lastFrameProcessed:Int = 0
	Field lastFrameProcessedF:Float = 0.0

Private

	Field processedDecimals:Int = 0
	Field lastMillisecs:Int = 0
	Field timeDistortion:Int = 1 * MathUtils.DECIMALS

Public

	'Can be called multiple times to forget last frame time, distortion, etc
	'Useful f.e. after a loading time which would provoke "lag" effect (too much time accumulated between frames) 
	'Note: the timers (main, processed) ARE NOT restarted
	Method Initialize:Void()
		lastMillisecs = Millisecs()
		lastFrame = 0
		lastFrame = 0
		processedDecimals = 0
		timeDistortion = 1 * MathUtils.DECIMALS
	End Method

	Method Update:Void()
		Local now:Int = Millisecs()
		lastFrame = now - lastMillisecs
		Local lastFramePD:Int = lastFrame * timeDistortion
		lastFrameProcessed = lastFramePD / MathUtils.DECIMALS
		processedDecimals += lastFramePD Mod MathUtils.DECIMALS
		' check decimals overflow
		if (processedDecimals >= MathUtils.DECIMALS)
			processedDecimals -= MathUtils.DECIMALS
			lastFrameProcessed += 1
		End If
		mainTimer += lastFrame
		processedTimer += lastFrameProcessed
		lastFrameF = Float(lastFrame)
		lastFrameProcessedF = Float(lastFrameProcessed)
		lastMillisecs = now
	End Method

	Method GetTimeDistortion:Float()
		Return MathUtils.ToFloat(timeDistortion)
	End Method

	Method SetTimeDistortion:Void(x:Float)
		If (x < 0.0) Then Error("Cannot set Time Distortion to a value < 0.0")
		timeDistortion = MathUtils.ToInt(x)
	End Method

End Class