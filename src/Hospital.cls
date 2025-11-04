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

addMaquina: maquina
maquinasExp add: maquina.!

comprar: maquinaId producto: prodId pago: tipoPago datoCliente: unCliente
	| maquina respuesta |
	maquina := self getMaquinaPorId: maquinaId.
	respuesta := maquina
				comprarProducto: prodId
				pago: tipoPago
				datoCliente: unCliente.
	^respuesta!

comprar: maquinaId productos: prodIds pago: tipoPago datoCliente: unCliente
	| maquina respuesta |
	maquina := self getMaquinaPorId: maquinaId.
	respuesta := maquina
				comprarProducto: prodIds
				pago: tipoPago
				datoCliente: unCliente.
	^respuesta!

getMaquinaPorId: unId
	^maquinasExp detect: [:each | each getId = unId] ifNone: [nil]!

getNombre
^nombre!

initializeWith: unNombre
	self setNombre: unNombre.
	maquinasExp := OrderedCollection new!

setNombre: unNombre
	(unNombre isKindOf: String) ifFalse: [self error: 'El nombre no es un string'].
	nombre := unNombre! !

!Hospital categoriesForMethods!
addMaquina:!public! !
comprar:producto:pago:datoCliente:!public! !
comprar:productos:pago:datoCliente:!public! !
getMaquinaPorId:!public! !
getNombre!public! !
initializeWith:!private! !
setNombre:!public! !
!

!Hospital class methodsFor!

nombre: unNombre
	^self new
		initializeWith: unNombre! !

!Hospital class categoriesForMethods!
nombre:!public! !
!

