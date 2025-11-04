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

getStock
^stock.!

getStockMinimo
^stockminimo.!

getTipo
^tipo.!

initializeWithNombre: unNombre precio: unPrecio
nombre := unNombre.
precio := unPrecio.!

setId: unId
	(unId isKindOf: Integer) ifTrue: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]! !

!Producto categoriesForMethods!
getId!public! !

setNombre: unNombre
nombre := unNombre.!

setPrecio: unPrecio
precio := unPrecio.!

setStock: unStock
stock := unStock.!

setStockMinimo: unStock
stockminimo := unStock.!

!Producto categoriesForMethods!
getNombre!public! !
getPrecio!public! !
getStock!public! !
getStockMinimo!public! !
initializeWithNombre:precio:!private! !
setId:!public! !
!

!Producto class methodsFor!

new
    self = Producto ifTrue: [
        self error: 'La clase Producto es abstracta y no puede instanciarse' ].
    ^super new.! !

!Producto class categoriesForMethods!
new!public! !
!

