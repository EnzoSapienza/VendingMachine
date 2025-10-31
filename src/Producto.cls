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

getNombre
	^nombre!

getPrecio
	^precio!

initializeWithNombre: unNombre precio: unPrecio
nombre := unNombre.
precio := unPrecio.! !

!Producto categoriesForMethods!
getNombre!public! !
getPrecio!public! !
initializeWithNombre:precio:!private! !
!

!Producto class methodsFor!

new
    self = Producto ifTrue: [
        self error: 'La clase Producto es abstracta y no puede instanciarse' ].
    ^super new.! !

!Producto class categoriesForMethods!
new!public! !
!

