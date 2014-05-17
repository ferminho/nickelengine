Strict

Private

Import mojo

Import NickelEngine.math.mathutils

Public

Class Screen Abstract
Public

	Global virtualWidth:Int = 0
	Global virtualHeight:Int = 0
	Global virtualWidthF:Float = 0.0
	Global virtualHeightF:Float = 0.0
	Global virtualWidthF2:Float = 0.0
	Global virtualHeightF2:Float = 0.0

Public

	Function ToRealX:Float(virtualX:Int)
		Local sW:Float = Float(DeviceWidth())
		Local x:Float = MathUtils.ToFloat(virtualX)
		Return (x / virtualWidthF) * sW
	End Function

	Function ToRealY:Float(virtualY:Int)
		Local sH:Float = Float(DeviceHeight())
		Local y:Float = MathUtils.ToFloat(virtualY)
		Return (y / virtualHeightF) * sH
	End Function

	Function ApplyScreenScaleMatrix:Void()
		Local scaleX:Float = DeviceWidth() / virtualWidthF
		Local scaleY:Float = DeviceHeight() / virtualHeightF
		Scale(scaleX, scaleY)
	End Function

End Class