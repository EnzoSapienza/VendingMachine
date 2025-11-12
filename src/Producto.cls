"Filed out from Dolphin Smalltalk"!

Object subclass: #Producto
	instanceVariableNames: 'id nombre precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Producto guid: (GUID fromString: '{27f458ec-9c07-4573-aa70-3cc6f84955d3}')!

Producto comment: ''!

!Producto categoriesForClass!Kernel-Objects! !

!Producto methodsFor!

getId
	^id!

getNombre
	^nombre!

getPrecio
	^precio!

initializeWithNombre: unNombre precio: unPrecio
	nombre := unNombre.
	precio := unPrecio!

setId: unId
	(unId isKindOf: Integer) ifFalse: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]!

setPrecio: unPrecio
	(unPrecio isKindOf: Number) ifFalse: [self error: 'Precio inválido'].
	precio := unPrecio! !

!Producto categoriesForMethods!
getId!public! !
getNombre!public! !
getPrecio!public! !
initializeWithNombre:precio:!private! !
setId:!public! !
setPrecio:!public! !
!

!Producto class methodsFor!

new
	self = Producto ifTrue: [self error: 'La clase Producto es abstracta y no puede instanciarse'].
	^super new! !

!Producto class categoriesForMethods!
new!public! !
!

