"Filed out from Dolphin Smalltalk"!

Object subclass: #Hospital
	instanceVariableNames: 'nombre maquinasExp verRecaudoSemanal'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Hospital guid: (GUID fromString: '{8d2fe85d-8d61-4e20-ac72-c87540dcc737}')!

Hospital comment: ''!

!Hospital categoriesForClass!integrador! !

!Hospital methodsFor!

getNombre
^nombre!

setNombre: unNombre
	(unNombre isKindOf: String) ifFalse: [self error: 'El nombre no es un string'].
	nombre := unNombre!

verMaquinasExp
	^0! !

!Hospital categoriesForMethods!
getNombre!public! !
setNombre:!public! !
verMaquinasExp!public! !
!

!Hospital class methodsFor!

nombre: unNombre
	^self new
		setNombre: unNombre;
		yourself! !

!Hospital class categoriesForMethods!
nombre:!public! !
!

