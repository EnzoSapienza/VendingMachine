"Filed out from Dolphin Smalltalk"!

Pago subclass: #PagoQR
	instanceVariableNames: 'link'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PagoQR guid: (GUID fromString: '{0e8d8b53-a83a-4ec4-aa21-29598404f805}')!

PagoQR comment: ''!

!PagoQR categoriesForClass!Kernel-Objects! !

!PagoQR methodsFor!

getLink
	^link!

initializeWith: unLink Compra: unaCompraId monto: unMonto
	(unLink isKindOf: String) ifFalse: [self error: 'Parametro Invalido: el link no es un string'].
	link := unLink.
	super initializeWith: unaCompraId monto: unMonto! !

!PagoQR categoriesForMethods!
getLink!public! !
initializeWith:Compra:monto:!private! !
!

!PagoQR class methodsFor!

compra: unaCompraId monto: unMonto link: unLink
	^self new
		initializeWith: unLink
		Compra: unaCompraId
		monto: unMonto! !

!PagoQR class categoriesForMethods!
compra:monto:link:!public! !
!

