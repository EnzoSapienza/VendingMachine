"Filed out from Dolphin Smalltalk"!

Object subclass: #Compras
	instanceVariableNames: 'id cliente esGenerico fecha'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Compras guid: (GUID fromString: '{cba00593-ee11-47b7-92c9-367c2c530167}')!

Compras comment: ''!

!Compras categoriesForClass!Kernel-Objects! !

!Compras methodsFor!

comprarSnack:snackId metodo: unMetodo cliente: unCliente
!

esGenerico
^esGenerico.!

getCliente
^cliente.!

getFecha
^fecha!

getId
^id.!

initializeWith: unCliente generic: unBooleano fecha: unaFecha
	unCliente = nil | (unCliente isKindOf: Medico)
		ifFalse: [self error: 'El cliente no es un dato nulo o un medico'].
	(unBooleano isKindOf: Boolean)
		ifFalse: [self error: 'No se aclaró si el cliente es generico o no'].
	(unaFecha isKindOf: Date) ifFalse: [self error: 'Fecha no valida'].
	cliente := unCliente.
	esGenerico := unBooleano.
	fecha := unaFecha!

setId: unId
	(unId isKindOf: Integer) ifTrue: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]! !

!Compras categoriesForMethods!
comprarSnack:metodo:cliente:!public! !
esGenerico!public! !
getCliente!public! !
getFecha!public! !
getId!public! !
initializeWith:generic:fecha:!private! !
setId:!public! !
!

!Compras class methodsFor!

cliente: unCliente
	unCliente class = Medico ifFalse: [self error: 'Atributo Inválido'].
	^self new
		initializeWith: unCliente
		generic: false
		fecha: Date today!

cliente: unCliente fecha: unaFecha
	unCliente class = Medico ifFalse: [self error: 'Atributo Inválido'].
	^self new
		initializeWith: unCliente
		generic: false
		fecha: unaFecha!

nuevoGenerico: unaFecha
	^self new
		initializeWith: nil
			generic: true
			fecha: unaFecha! !

!Compras class categoriesForMethods!
cliente:!public! !
cliente:fecha:!public! !
nuevoGenerico:!public! !
!

