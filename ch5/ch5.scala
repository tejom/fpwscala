package ch5

object test{
	if2[A](cond: Boolean , onTrue: () => A, onFalse: () => A):A=
		if(cond) onTrue() else onFalse()
}