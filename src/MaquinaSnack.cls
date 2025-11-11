"Filed out from Dolphin Smalltalk"!

MaquinaExpendedora subclass: #MaquinaSnack
	instanceVariableNames: 'snacksDisponibles'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MaquinaSnack guid: (GUID fromString: '{df0b6fa9-5874-4b3c-b68b-d30bb8784a30}')!

MaquinaSnack comment: ''!

!MaquinaSnack categoriesForClass!integrador! !

!MaquinaSnack methodsFor!

comprarProductos: prodIds pago: pago
	| productos compra |

	"Validar producto"
	(prodIds allSatisfy: [:each | each isKindOf: Integer])
		ifFalse: [self error: 'Id invalido'].
	(pago isKindOf: Pago)
		ifFalse: [self error: 'Pago invalido'].

	"Obtener los productos desde la tabla"
	productos := prodIds collect: [:v | TablaProductos getPorId: v].
	(productos allSatisfy: [:each | each isKindOf: Snack])
		ifFalse: [^Respuesta mensaje: 'Snack no encontrado' exito: false].

	"Validar stock disponible"
	(snacksDisponibles size < productos size)
		ifTrue: [^Respuesta mensaje: 'Stock insuficiente de snacks' exito: false].

	"Verificar que todos los snacks pedidos estén disponibles"
	(productos allSatisfy: [:p | snacksDisponibles includes: p])
		ifFalse: [^Respuesta mensaje: 'Algunos snacks no están disponibles' exito: false].

	"Cobrar producto"
	compra := pago cobrar: productos.

	"Registrar compra en la máquina"
	self addCompra: compra.

	"Actualizar stock (remover los snacks vendidos)"
	productos do: [:p | snacksDisponibles remove: p ifAbsent: []].

	^Respuesta mensaje: 'Compra realizada con éxito' exito: true!

getSnacksDisponibles
	^snacksDisponibles select:[:v | v getStock > 0]!

initializeWith: unaUbicacion compras: unasCompras snacksDisponibles: snacks
	(snacks isKindOf: OrderedCollection)
		ifFalse: [self error: 'Parametro Invalido: Los Snacks no son una colección ordenada'].
	(snacks allSatisfy: [:each | each isKindOf: Snack])
		ifFalse: [self error: 'Parametro Invalido: Elemento desconocido encontrado en los snacks'].
	snacksDisponibles := snacks copy.
	super initializeWith: unaUbicacion compras: unasCompras! !

!MaquinaSnack categoriesForMethods!
comprarProductos:pago:!public! !
getSnacksDisponibles!public! !
initializeWith:compras:snacksDisponibles:!private! !
!

!MaquinaSnack class methodsFor!

ubicacion: unaUbicacion compras: unasCompras snacks: snacks
	^self new
		initializeWith: unaUbicacion
		compras: unasCompras
		snacksDisponibles: snacks! !

!MaquinaSnack class categoriesForMethods!
ubicacion:compras:snacks:!public! !
!

