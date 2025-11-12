"Filed out from Dolphin Smalltalk"!

Object subclass: #Respuesta
	instanceVariableNames: 'mensaje exito compra'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Respuesta guid: (GUID fromString: '{c810bbed-f3a8-4ef3-829b-f4c1f1e77ca9}')!

Respuesta comment: ''!

!Respuesta categoriesForClass!Kernel-Objects! !

!Respuesta methodsFor!

getCompra
	^compra!

getMensaje
	^mensaje!

initializeWith: unMensaje exito: unEstado compra: unaCompra
	(unMensaje isKindOf: String) ifFalse: [unMensaje inspect. self error: 'El mensaje debe ser un string'].
	(unEstado isKindOf: Boolean) ifFalse: [self error: 'El mensaje debe ser un string'].
	unaCompra isNil | (unaCompra isKindOf: Compras)
		ifFalse: [self error: 'El mensaje debe ser un string'].
	mensaje := unMensaje.
	exito := unEstado.
	compra := unaCompra!

tuvoExito
	^exito! !

!Respuesta categoriesForMethods!
getCompra!public! !
getMensaje!public! !
initializeWith:exito:compra:!private! !
tuvoExito!public! !
!

!Respuesta class methodsFor!

mensaje: unMensaje exito: unEstado
	^self new
		initializeWith: unMensaje
		exito: unEstado
		compra: nil!

mensaje: unMensaje exito: unEstado compra: unaCompra
	^self new
		initializeWith: unMensaje
		exito: unEstado
		compra: unaCompra! !

!Respuesta class categoriesForMethods!
mensaje:exito:!public! !
mensaje:exito:compra:!public! !
!

