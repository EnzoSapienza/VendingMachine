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

getMedicoId
	^idMedico!

initializeWith: medicoId Compra: unaCompraId monto: unMonto
	(idMedico isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: id no es un entero'].
	idMedico := medicoId.
	super initializeWith: unaCompraId monto: unMonto! !

!PagoCredito categoriesForMethods!
getMedicoId!public! !
initializeWith:Compra:monto:!private! !
!

!PagoCredito class methodsFor!

compra: unaCompraId monto: unMonto medico: idMedico
	^self new
		initializeWith: idMedico
		Compra: unaCompraId
		monto: unMonto! !

!PagoCredito class categoriesForMethods!
compra:monto:medico:!public! !
!

