"Filed out from Dolphin Smalltalk"!

Object subclass: #Compras
	instanceVariableNames: 'id cliente esGenerico'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Compras guid: (GUID fromString: '{cba00593-ee11-47b7-92c9-367c2c530167}')!

Compras comment: ''!

!Compras categoriesForClass!Kernel-Objects! !

!Compras methodsFor!

esGenerico
^esGenerico.!

getCliente
^cliente.!

getId
^id.!

initializeWith: unCliente generic: unBooleano
cliente := unCliente.
esGenerico := unBooleano.!

setId: unId
	(unId isKindOf: Integer) ifTrue: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]! !

!Compras categoriesForMethods!
esGenerico!public! !
getCliente!public! !
getId!public! !
initializeWith:generic:!private! !
setId:!public! !
!

!Compras class methodsFor!

cliente: unCliente
(unCliente class = Medico) ifFalse: [self error: 'Atributo Inválido'].
^self new initializeWith: unCliente generic: false.!

nuevoGenerico
	^self new initializeWith: nil generic: true! !

!Compras class categoriesForMethods!
cliente:!public! !
nuevoGenerico!public! !
!

