"Filed out from Dolphin Smalltalk"!

Producto subclass: #TipoCafe
	instanceVariableNames: 'azucarNecesaria lecheNecesaria aguaNecesaria'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

TipoCafe guid: (GUID fromString: '{344f7eb8-4991-41bd-933e-c383b17f8eb8}')!

TipoCafe comment: ''!

!TipoCafe categoriesForClass!Kernel-Objects! !

!TipoCafe methodsFor!

getAguaNecesaria
	^aguaNecesaria!

getAzucarNecesaria
	^azucarNecesaria!

getLecheNecesaria
	^lecheNecesaria!

initializeWithNombre: unNombre precio: unPrecio azucar: azucar leche: leche agua: agua
	"Validaciones"

	(azucar isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: la azucar no es un entero'].
	azucar > 0 ifFalse: [self error: 'Parametro Invalido: la azucar no es mayor que 0'].
	(leche isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: la leche no es un número'].
	leche > 0 ifFalse: [self error: 'Parametro Invalido: la leche no es mayor que 0'].
	(agua isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: el agua no es un número'].
	agua > 0 ifFalse: [self error: 'Parametro Invalido: el agua no es mayor que 0'].

	"Instancia"
	azucarNecesaria := azucar.
	lecheNecesaria := leche.
	aguaNecesaria := agua.
	super initializeWithNombre: unNombre precio: unPrecio! !

!TipoCafe categoriesForMethods!
getAguaNecesaria!accessing!public! !
getAzucarNecesaria!public! !
getLecheNecesaria!public! !
initializeWithNombre:precio:azucar:leche:agua:!private! !
!

!TipoCafe class methodsFor!

nombre: unNombre precio: unPrecio azucar: azucar leche: leche agua: agua
	^self new
		initializeWithNombre: unNombre
		precio: unPrecio
		azucar: azucar
		leche: leche
		agua: agua! !

!TipoCafe class categoriesForMethods!
nombre:precio:azucar:leche:agua:!public! !
!

