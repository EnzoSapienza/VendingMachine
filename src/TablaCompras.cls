"Filed out from Dolphin Smalltalk"!

AbstractTabla subclass: #TablaCompras
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

TablaCompras guid: (GUID fromString: '{e4d02a7a-13dd-48f8-82f1-4efa534b8323}')!

TablaCompras comment: ''!

!TablaCompras categoriesForClass!Kernel-Objects! !

!TablaCompras class methodsFor!

add: compra
	(compra isKindOf: Compras) ifFalse: [self error: 'No es una compra'].
	self establecerTabla.
	compra setId: Id.
	Collection at: compra getId set: compra.
	Id := Id + 1! !

!TablaCompras class categoriesForMethods!
add:!public! !
!

