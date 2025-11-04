"Filed out from Dolphin Smalltalk"!

AbstractTabla subclass: #TablaProductos
	instanceVariableNames: ''
	classVariableNames: 'Collection Id'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

TablaProductos guid: (GUID fromString: '{75884272-4ebb-4a0e-a867-576c236e7169}')!

TablaProductos comment: ''!

!TablaProductos categoriesForClass!Kernel-Objects! !

!TablaProductos class methodsFor!

addProducto: producto
	(producto isKindOf: Producto) ifFalse: [self error: 'No es un producto'].
	self establecerTabla.
	producto setId: Id.
	Collection add: producto.
	Id := Id + 1.!

establecerTabla
	"Private - Instanciar tabla si no existe"

	Collection = nil
		ifTrue: 
			[Collection := OrderedCollection new.
			Id := 1]! !

!TablaProductos class categoriesForMethods!
addProducto:!public! !
establecerTabla!private! !
!

