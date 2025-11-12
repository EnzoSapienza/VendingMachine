"Filed out from Dolphin Smalltalk"!

Object subclass: #Pago
	instanceVariableNames: 'idCompra monto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Pago guid: (GUID fromString: '{a4936274-1121-4049-9b73-4319c3d9314d}')!

Pago comment: ''!

!Pago categoriesForClass!Kernel-Objects! !

!Pago methodsFor!

cobrar: anObject
	self subclassResponsibility!

getCompraId
	^idCompra!

getId
	^id!

getMonto
	^monto!

setCompraId: unId
	idCompra := unId!

setId: unId
	(unId isKindOf: Integer) ifFalse: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]! !

!Pago categoriesForMethods!
cobrar:!public! !
getCompraId!public! !
getId!public! !
getMonto!public! !
setCompraId:!public! !
setId:!public! !
!

!Pago class methodsFor!

datoCliente: datoCliente
	self subclassResponsibility!

new
	self = Pago ifTrue: [self error: 'La clase Pago es abstracta y no puede instanciarse'].
	^super new!

tipo: tipoDePago datoCliente: unCliente
	^(self subclasses detect: [:clase | clase name = tipoDePago]) datoCliente: unCliente! !

!Pago class categoriesForMethods!
datoCliente:!public! !
new!public! !
tipo:datoCliente:!public! !
!

