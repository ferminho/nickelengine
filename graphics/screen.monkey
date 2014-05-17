Strict

Private

Import mojo

Import NickelEngine.math.mathutils

Public

Class Screen
Private

	Field virtualWidth:Int = 0
	Field virtualHeight:Int = 0
	Field virtualWidthF:Float = 0.0
	Field virtualHeightF:Float = 0.0
	Field virtualWidthF2:Float = 0.0
	Field virtualHeightF2:Float = 0.0

Public

	Global instance:Screen = New Screen()

	Method Initialize:Void(vWidth:Float, vHeight:Float)
		virtualWidth = MathUtils.ToDecimalInt(vWidth)
		virtualHeight = MathUtils.ToDecimalInt(vHeight)
		virtualWidthF = vWidth
		virtualHeightF = vHeight
		virtualWidthF2 = vWidth / 2.0
		virtualHeightF2 = vHeight / 2.0
	
		#If CONFIG="debug"
			Print("Virtual Res: " + virtualWidthF + ", " + virtualHeightF)
			Print("Device Res: " + DeviceWidth() + ", " + DeviceHeight())
		#End
	End Method

	Method ToRealX:Float(virtualX:Int)
		Local sW:Float = Float(DeviceWidth())
		Local x:Float = MathUtils.ToFloat(virtualX)
		Return (x / virtualWidthF) * sW
	End Method

	Method ToRealY:Float(virtualY:Int)
		Local sH:Float = Float(DeviceHeight())
		Local y:Float = MathUtils.ToFloat(virtualY)
		Return (y / virtualHeightF) * sH
	End Method

	Method ApplyScreenScaleMatrix:Void()
		Local scaleX:Float = DeviceWidth() / virtualWidthF
		Local scaleY:Float = DeviceHeight() / virtualHeightF
		Scale(scaleX, scaleY)
	End Method

End Class