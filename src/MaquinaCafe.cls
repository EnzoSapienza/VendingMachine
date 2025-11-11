"Filed out from Dolphin Smalltalk"!

MaquinaExpendedora subclass: #MaquinaCafe
	instanceVariableNames: 'lecheDisponible capsulas aguaDisponible azucarDisponible'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MaquinaCafe guid: (GUID fromString: '{50a9010d-d5d4-433c-b498-4b2aec432c36}')!

MaquinaCafe comment: ''!

!MaquinaCafe categoriesForClass!integrador! !

!MaquinaCafe methodsFor!

comprarProductos: prodIds pago: pago
	| productos lecheTotal aguaTotal azucarTotal compra |
	"Validar producto"
	(prodIds allSatisfy: [:each | each isKindOf: Integer]) ifFalse: [self error: 'Id invalido'].
	(pago isKindOf: Pago) ifFalse: [self error: 'Pago invalido'].
	productos := prodIds collect: [:v | TablaProductos getPorId: v].
	(productos allSatisfy: [:each | each isKindOf: TipoCafe])
		ifFalse: [^Respuesta mensaje: 'Cafe no encontrado' exito: false].

	"Validar materiales restantes"
	lecheTotal := productos inject: 0 into: [:acum :v | acum + v getLecheNecesaria].
	aguaTotal := productos inject: 0 into: [:acum :v | acum + v getAguaNecesaria].
	azucarTotal := productos inject: 0 into: [:acum :v | acum + v getAzucarNecesaria].
	lecheDisponible < lecheTotal ifTrue: [^Respuesta mensaje: 'Leche insuficiente' exito: false].
	aguaDisponible < aguaTotal ifTrue: [^Respuesta mensaje: 'Agua insuficiente' exito: false].
	azucarDisponible < azucarTotal ifTrue: [^Respuesta mensaje: 'Azucar insuficiente' exito: false].
	capsulas > 0 ifFalse: [^Respuesta mensaje: 'No hay más capsulas' exito: false].

	"Cobrar producto"
	compra := pago cobrar: productos.
	self addCompra: compra.
	lecheDisponible := lecheDisponible - lecheTotal.
	aguaDisponible := aguaDisponible - aguaTotal.
	azucarDisponible := azucarDisponible - azucarTotal.
	capsulas := capsulas - 1.
	^Respuesta mensaje: 'Compra realizada con éxito' exito: true compra: compra!

getAguaDisponible
	^aguaDisponible!

getCapsulas
	^capsulas!

getLecheDisponible
	^lecheDisponible!

initializeWithUbicacion: unaUbicacion compras: unasCompras leche: leche capsulas: unEntero agua: agua azucar: azucar
	"Validaciones - leche"

	(leche isKindOf: Integer)
		ifFalse: [self error: 'Parametro Invalido: Leche en Mililitros no es un entero'].
	leche > 0 ifFalse: [self error: 'Parametro Invalido: Leche negativa'].

	"Capsulas"
	(unEntero isKindOf: Integer)
		ifFalse: [self error: 'Parametro Invalido: La cantidad de capsulas no es entera'].
	unEntero > 0 ifFalse: [self error: 'Parametro Invalido: Capsulas negativas'].

	"Agua"
	(agua isKindOf: Integer)
		ifFalse: [self error: 'Parametro Invalido: Agua en Mililitros no es un entero'].
	agua > 0 ifFalse: [self error: 'Parametro Invalido: Agua negativa'].

	"Azucar"
	(azucar isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: Azucar no es un entero'].
	azucar > 0 ifFalse: [self error: 'Parametro Invalido: Azucar negativa'].


	"Asignaciones"
	aguaDisponible := agua.
	lecheDisponible := leche.
	azucarDisponible := azucar.
	capsulas := unEntero.
	super initializeWith: unaUbicacion compras: unasCompras! !

!MaquinaCafe categoriesForMethods!
comprarProductos:pago:!public! !
getAguaDisponible!public! !
getCapsulas!public! !
getLecheDisponible!public! !
initializeWithUbicacion:compras:leche:capsulas:agua:azucar:!private! !
!

!MaquinaCafe class methodsFor!

ubicacion: unaUbicacion compras: unasCompras leche: leche capsulas: unEntero agua: agua azucar: azucar
	^self new
		initializeWithUbicacion: unaUbicacion
		compras: unasCompras
		leche: leche
		capsulas: unEntero
		agua: agua
azucar: azucar! !

!MaquinaCafe class categoriesForMethods!
ubicacion:compras:leche:capsulas:agua:azucar:!public! !
!

