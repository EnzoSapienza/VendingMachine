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

cobrar
	| compra |
	Transcript
		show: 'Se han cobrado a la tarjeta ' , numTarjeta , ' un total de: ' , monto;
		cr.
	compra := Compras nuevoGenerico.
	TablaCompras add: compra.
	^compra!

getTarjeta
	^numTarjeta!

initializeWith: unaTarjeta monto: unMonto
	(unaTarjeta isKindOf: Integer)
		ifFalse: [self error: 'Parametro Invalido: la tarjeta no es un entero'].
	numTarjeta := unaTarjeta.
	super initializeWith: unMonto! !

!PagoTarjeta categoriesForMethods!
cobrar!public! !
getTarjeta!public! !
initializeWith:monto:!private! !
!

!PagoTarjeta class methodsFor!

monto: unMonto datoCliente: unaTarjeta
	^self new
		initializeWith: unaTarjeta
		monto: unMonto!

tipo
^#Tarjeta! !

!PagoTarjeta class categoriesForMethods!
monto:datoCliente:!public! !
tipo!public! !
!

