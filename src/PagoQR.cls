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

cobrar: productos
	| compra |
	(productos allSatisfy: [:each | each isKindOf: Producto])
		ifFalse: [self error: 'Parametro Invalido: se esperaban productos.'].
	monto := productos inject: 0 into: [:acum :each | each getPrecio + acum].
	compra := Compras nuevoGenerico: (productos collect: [:each | each getId]).
	Transcript
		show: 'Se han cobrado al link ' , link , ' un total de: ' , monto printString;
		cr.
	TablaCompras add: compra.
	idCompra := compra getId.
	compra setPago: self.
	^Respuesta
		mensaje: 'Compra Existosa'
		exito: true
		compra: compra!

getLink
	^link!

initializeWith: unLink
	(unLink isKindOf: String) ifFalse: [self error: 'Parametro Invalido: el link no es un string'].
	link := unLink! !

!PagoQR categoriesForMethods!
cobrar:!public! !
getLink!public! !
initializeWith:!private! !
!

!PagoQR class methodsFor!

datoCliente: unLink
	^self new initializeWith: unLink! !

!PagoQR class categoriesForMethods!
datoCliente:!public! !
!

