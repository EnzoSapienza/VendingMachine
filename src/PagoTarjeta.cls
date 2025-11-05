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

cobrar: productos
	| compra |
	(productos allSatisfy: [:each | each isKindOf: Producto])
		ifFalse: [self error: 'Parametro Invalido: se esperaban productos.'].

	monto := productos inject: 0 into: [:acum :each | each getPrecio + acum].
	compra := Compras nuevoGenerico: (productos collect: [:each | each getId]).
	Transcript
		show: 'Se han cobrado a la tarjeta ' , numTarjeta printString , ' un total de: ' , monto printString;
		cr.
	TablaCompras add: compra.
	idCompra := compra getId.
	^compra!

getTarjeta
	^numTarjeta!

initializeWith: unaTarjeta
	(unaTarjeta isKindOf: Integer)
		ifFalse: [self error: 'Parametro Invalido: la tarjeta no es un entero'].
	numTarjeta := unaTarjeta.! !

!PagoTarjeta categoriesForMethods!
cobrar:!public! !
getTarjeta!public! !
initializeWith:!private! !
!

!PagoTarjeta class methodsFor!

datoCliente: unaTarjeta
	^self new initializeWith: unaTarjeta!

monto: unMonto datoCliente: unaTarjeta
	^self new
		initializeWith: unaTarjeta
		monto: unMonto! !

!PagoTarjeta class categoriesForMethods!
datoCliente:!public! !
monto:datoCliente:!public! !
!

