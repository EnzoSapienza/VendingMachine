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

cobrar
	| compra medico |
	medico := TablaMedicos getPorId: idMedico.
	Transcript
		show: 'Se han cobrado al empleado ' , medico getNombre , ' un total de: ' , monto;
		cr.
	medico cobrar: monto.
	compra := Compras cliente: medico.
	TablaCompras add: compra.
	idCompra := compra getId.
	^compra!

getMedicoId
	^idMedico!

initializeWith: medicoId monto: unMonto
	(idMedico isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: id no es un entero'].
	idMedico := medicoId.
	super initializeWith: unMonto! !

!PagoCredito categoriesForMethods!
cobrar!public! !
getMedicoId!public! !
initializeWith:monto:!private! !
!

!PagoCredito class methodsFor!

monto: unMonto datoCliente: idMedico
	^self new
		initializeWith: idMedico
		monto: unMonto!

tipo
^#Credito! !

!PagoCredito class categoriesForMethods!
monto:datoCliente:!public! !
tipo!public! !
!

