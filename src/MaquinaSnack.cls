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
	| compra productos |
	"Validar producto"
	productos := prodIds collect: [:v | TablaProductos getPorId: v].
	(productos allSatisfy: [:each | each isKindOf: Snack]) ifFalse: [self error: 'Snack no encontrado'].
	"Faltan validaciones"
	compra := pago cobrar: nil.
	^Respuesta mensaje: 'Compra realizada con éxito' exito: true!

getSnacksDisponibles
	^snacksDisponibles select:[:v | v getStock > 0]!

initializeWith: unaUbicacion compras: unasCompras snacksDisponibles: snacks
	(snacks isKindOf: OrderedCollection)
		ifFalse: [self error: 'Parametro Invalido: Los Snacks no son una colección ordenada'].
	(snacks allSatisfy: [:each | each class = SnackDisponibles])
		ifFalse: [self error: 'Parametro Invalido: Elemento desconocido encontrada en los snack'].
	snacksDisponibles := snacks.
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

