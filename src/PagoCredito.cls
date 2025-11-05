"Filed out from Dolphin Smalltalk"!

Pago subclass: #PagoCredito
	instanceVariableNames: 'idMedico'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PagoCredito guid: (GUID fromString: '{0e0fe7b7-9a0d-467b-9801-0e1babbe0620}')!

PagoCredito comment: ''!

!PagoCredito categoriesForClass!Kernel-Objects! !

!PagoCredito methodsFor!

cobrar: productos
	| compra medico |
	(productos allSatisfy: [:each | each isKindOf: Producto])
		ifFalse: [self error: 'Parametro Invalido: se esperaban productos.'].
	medico := TablaMedicos getPorId: idMedico.
	monto := productos inject: 0 into: [:acum :each | each getPrecio + acum].
	Transcript
		show: 'Se han cobrado al empleado ' , medico getNombre , ' un total de: ' , monto printString;
		cr.
	medico cobrar: monto.
	compra := Compras cliente: medico prodIds: (productos collect: [:each | each getId]).
	TablaCompras add: compra.
	idCompra := compra getId.
	^compra!

getMedicoId
	^idMedico!

initializeWith: medicoId
	(medicoId isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: id no es un entero'].
	idMedico := medicoId.!

setMonto: unMonto
(unMonto isKindOf: Number) ifFalse: [self error: 'El monto no es un número'].
monto := unMonto.! !

!PagoCredito categoriesForMethods!
cobrar:!public! !
getMedicoId!public! !
initializeWith:!private! !
setMonto:!public! !
!

!PagoCredito class methodsFor!

datoCliente: idMedico
	^self new
		initializeWith: idMedico! !

!PagoCredito class categoriesForMethods!
datoCliente:!public! !
!

