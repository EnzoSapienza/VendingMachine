"Filed out from Dolphin Smalltalk"!

Producto subclass: #Snack
	instanceVariableNames: 'marca peso'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Snack guid: (GUID fromString: '{3f66b3cb-debd-42e1-a656-54f5d11f97a3}')!

Snack comment: ''!

!Snack categoriesForClass!Kernel-Objects! !

!Snack methodsFor!

getMarca
	^marca!

getPeso
	^peso!

initializeWithNombre: unNombre precio: unPrecio marca: unaMarca peso: unPeso
	(unPeso isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: el peso no es un número'].
	unPeso > 0 ifFalse: [self error: 'Parametro Invalido: el peso no es mayor que 0'].
	(unaMarca isKindOf: String) ifFalse: [self error: 'Parametro Invalido: la marca no es un string'].
	peso := unPeso.
	marca := unaMarca.
	super initializeWithNombre: unNombre precio: unPrecio! !

!Snack categoriesForMethods!
getMarca!public! !
getPeso!public! !
initializeWithNombre:precio:marca:peso:!private! !
!

!Snack class methodsFor!

nombre: unNombre precio: unPrecio marca: unaMarca peso: unPeso
	^self new
		initializeWithNombre: unNombre
		precio: unPrecio
		marca: unaMarca
		peso: unPeso! !

!Snack class categoriesForMethods!
nombre:precio:marca:peso:!public! !
!

