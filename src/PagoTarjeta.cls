"Filed out from Dolphin Smalltalk"!

Pago subclass: #PagoTarjeta
	instanceVariableNames: 'numTarjeta'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PagoTarjeta guid: (GUID fromString: '{bb3e76f1-3feb-4486-b7f4-cfe5ef602ce0}')!

PagoTarjeta comment: ''!

!PagoTarjeta categoriesForClass!Kernel-Objects! !

!PagoTarjeta methodsFor!

getTarjeta
	^numTarjeta!

initializeWith: unaTarjeta Compra: unaCompraId monto: unMonto
	(unaTarjeta isKindOf: Integer)
		ifFalse: [self error: 'Parametro Invalido: la tarjeta no es un entero'].
	numTarjeta := unaTarjeta.
	super initializeWith: unaCompraId monto: unMonto! !

!PagoTarjeta categoriesForMethods!
getTarjeta!public! !
initializeWith:Compra:monto:!private! !
!

!PagoTarjeta class methodsFor!

compra: unaCompraId monto: unMonto tarjeta: unaTarjeta
	^self new
		initializeWith: unaTarjeta
		Compra: unaCompraId
		monto: unMonto! !

!PagoTarjeta class categoriesForMethods!
compra:monto:tarjeta:!public! !
!

