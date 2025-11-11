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

initializeWith: unMensaje exito: unEstado
	mensaje := unMensaje.
	exito := unEstado!

initializeWith: unMensaje exito: unEstado compra: unaCompra
	mensaje := unMensaje.
	exito := unEstado.
	compra := unaCompra!

mensaje
	^mensaje!

tuvoExito
	^exito! !

!Respuesta categoriesForMethods!
initializeWith:exito:!private! !
initializeWith:exito:compra:!private! !
mensaje!public! !
tuvoExito!public! !
!

!Respuesta class methodsFor!

mensaje: unMensaje exito: unEstado
	^self new initializeWith: unMensaje exito: unEstado!

mensaje: unMensaje exito: unEstado compra: unaCompra
	^self new initializeWith: unMensaje exito: unEstado compra: unaCompra! !

!Respuesta class categoriesForMethods!
mensaje:exito:!public! !
mensaje:exito:compra:!public! !
!

