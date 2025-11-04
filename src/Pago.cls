"Filed out from Dolphin Smalltalk"!

Object subclass: #Pago
	instanceVariableNames: 'id idCompra monto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Pago guid: (GUID fromString: '{a4936274-1121-4049-9b73-4319c3d9314d}')!

Pago comment: ''!

!Pago categoriesForClass!Kernel-Objects! !

!Pago methodsFor!

cobrar
	self subclassResponsibility!

getCompraId
^idCompra!

getId
	^id!

getMonto
	^monto!

initializeWith: unMonto
	(unMonto isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: monto no es un número'].
	unMonto > 0 ifFalse: [self error: 'Parametro Invalido: monto es negativo'].
	monto := unMonto.!

setCompraId: unId
idCompra := unId.!

setId: unId
	(unId isKindOf: Integer) ifTrue: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]! !

!Pago categoriesForMethods!
cobrar!public! !
getCompraId!public! !
getId!public! !
getMonto!public! !
initializeWith:!private! !
setCompraId:!public! !
setId:!public! !
!

!Pago class methodsFor!

monto: unMonto datoCliente: datoCliente
	self subclassResponsibility!

new
    self = Pago ifTrue: [
        self error: 'La clase Pago es abstracta y no puede instanciarse' ].
    ^super new.!

tipo
	self subclassResponsibility!

tipo: tipoDePago monto: unMonto cliente: unCliente
	^(self subclasses detect: [:clase | clase tipo = tipoDePago]) monto: unMonto datoCliente: unCliente.! !

!Pago class categoriesForMethods!
monto:datoCliente:!public! !
new!public! !
tipo!public! !
tipo:monto:cliente:!public! !
!

