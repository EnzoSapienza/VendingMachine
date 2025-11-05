"Filed out from Dolphin Smalltalk"!

Object subclass: #Compras
	instanceVariableNames: 'id cliente esGenerico fecha items'
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

getFecha
^fecha!

getId
^id.!

initializeWith: unCliente generic: unBooleano prodIds: prodIds
	unCliente = nil | (unCliente isKindOf: Medico)
		ifFalse: [self error: 'El cliente no es un dato nulo o un medico'].
	(unBooleano isKindOf: Boolean)
		ifFalse: [self error: 'No se aclaró si el cliente es generico o no'].
	(prodIds allSatisfy: [:each | each isKindOf: Integer]) ifFalse: [self error: 'Producto no valido'].
	items := prodIds collect: [:each | ItemCompra prod: each compra: 1].
	cliente := unCliente.
	esGenerico := unBooleano.
	fecha := Date today!

setId: unId
	(unId isKindOf: Integer) ifFalse: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil
		ifTrue: 
			[id := unId.
			items do: [:v | v setIdCompra: id]]!

setProductos: prodIds
items := prodIds collect:[:v | ItemCompra new ]! !

!Compras categoriesForMethods!
esGenerico!public! !
getCliente!public! !
getFecha!public! !
getId!public! !
initializeWith:generic:prodIds:!private! !
setId:!public! !
setProductos:!public! !
!

!Compras class methodsFor!

cliente: unCliente prodIds: prodIds
	unCliente class = Medico ifFalse: [self error: 'Atributo Inválido'].
	^self new
		initializeWith: unCliente
		generic: false
		prodIds: prodIds!

nuevoGenerico: prodIds
	^self new
		initializeWith: nil
		generic: true
		prodIds: prodIds! !

!Compras class categoriesForMethods!
cliente:prodIds:!public! !
nuevoGenerico:!public! !
!

