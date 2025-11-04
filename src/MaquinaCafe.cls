"Filed out from Dolphin Smalltalk"!

MaquinaExpendedora subclass: #MaquinaCafe
	instanceVariableNames: 'lecheDisponible capsulas aguaDisponible cafesDisponibles'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MaquinaCafe guid: (GUID fromString: '{50a9010d-d5d4-433c-b498-4b2aec432c36}')!

MaquinaCafe comment: ''!

!MaquinaCafe categoriesForClass!integrador! !

!MaquinaCafe methodsFor!

comprarProductos: prodIds pago: tipoPago datoCliente: unCliente
	| pago compra producto |
	"Validar producto"
	producto := TablaProductos getPorId: prodIds.
	(producto isKindOf: TipoCafe) ifFalse: [].

	compra = Compras!

getAguaDisponible
	^aguaDisponible!

getCafesDisponibles
	^cafesDisponibles!

getCapsulas
	^capsulas!

getLecheDisponible
	^lecheDisponible!

initializeWithUbicacion: unaUbicacion compras: unasCompras leche: leche capsulas: unEntero agua: agua cafes: cafes
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

	"Cafes"
	(cafes isKindOf: OrderedCollection)
		ifFalse: [self error: 'Parametro Invalido: Los cafés no es una colección ordenada'].

	"Asignaciones"
	aguaDisponible := agua.
	lecheDisponible := leche.
	capsulas := unEntero.
	cafesDisponibles := cafes.
	super initializeWith: unaUbicacion compras: unasCompras! !

!MaquinaCafe categoriesForMethods!
comprarProductos:pago:datoCliente:!public! !
getAguaDisponible!public! !
getCafesDisponibles!public! !
getCapsulas!public! !
getLecheDisponible!public! !
initializeWithUbicacion:compras:leche:capsulas:agua:cafes:!private! !
!

!MaquinaCafe class methodsFor!

ubicacion: unaUbicacion compras: unasCompras leche: leche capsulas: unEntero agua: agua cafes: cafes
	^self new
		initializeWithUbicacion: unaUbicacion
		compras: unasCompras
		leche: leche
		capsulas: unEntero
		agua: agua
		cafes: cafes! !

!MaquinaCafe class categoriesForMethods!
ubicacion:compras:leche:capsulas:agua:cafes:!public! !
!

