"Filed out from Dolphin Smalltalk"!

Producto subclass: #TipoCafe
	instanceVariableNames: 'azucarNecesaria lecheNecesaria'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

TipoCafe guid: (GUID fromString: '{344f7eb8-4991-41bd-933e-c383b17f8eb8}')!

TipoCafe comment: ''!

!TipoCafe categoriesForClass!Kernel-Objects! !

!TipoCafe methodsFor!

getAzucarNecesaria
	^azucarNecesaria!

getLecheNecesaria
	^lecheNecesaria!

initializeWithNombre: unNombre precio: unPrecio azucar: azucar leche: leche
	(azucar isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: la azucar no es un entero'].
	azucar > 0 ifFalse: [self error: 'Parametro Invalido: la azucar no es mayor que 0'].
	(leche isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: la leche no es un número'].
	leche > 0 ifFalse: [self error: 'Parametro Invalido: la leche no es mayor que 0'].
	azucarNecesaria := azucar.
	lecheNecesaria := leche.
	super initializeWithNombre: unNombre precio: unPrecio! !

!TipoCafe categoriesForMethods!
getAzucarNecesaria!public! !
getLecheNecesaria!public! !
initializeWithNombre:precio:azucar:leche:!private! !
!

!TipoCafe class methodsFor!

nombre: unNombre precio: unPrecio azucar: azucar leche: leche
	^self new
		initializeWithNombre: unNombre
		precio: unPrecio
		azucar: azucar
		leche: leche! !

!TipoCafe class categoriesForMethods!
nombre:precio:azucar:leche:!public! !
!

