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

addSnack: idSnack stock: unStock
	snacksDisponibles add: (SnackDisponibles stock: unStock idSnack: idSnack)!

comprarProductos: prodIds pago: pago
	| productos compra pedidosPorId insuficiente pagoRespuesta |
	"Validar producto y pago"
	(prodIds allSatisfy: [:each | each isKindOf: Integer]) ifFalse: [self error: 'Id invalido'].
	(pago isKindOf: Pago) ifFalse: [self error: 'Pago invalido'].

	"Obtener los productos desde la tabla"
	productos := prodIds collect: [:v | TablaProductos getPorId: v].
	(productos allSatisfy: [:each | each isKindOf: Snack])
		ifFalse: [^Respuesta mensaje: 'Snack no encontrado' exito: false].

	"Verificar que todos los snacks pedidos estén disponibles"
	(productos allSatisfy: [:p | self getSnacksDisponibles includes: p])
		ifFalse: [^Respuesta mensaje: 'Algunos snacks no están disponibles' exito: false].

	"Contar cuántas unidades se piden de cada producto"
	pedidosPorId := prodIds inject: Dictionary new
				into: 
					[:acum :v |
					| temp |
					temp := acum at: v ifAbsent: [0].
					acum at: v put: temp + 1.
					acum].

	"Verificar stock suficiente"
	insuficiente := pedidosPorId keys detect: 
					[:v |
					| snack |
					snack := self getSnacksDisponibles detect: [:s | s getId = v] ifNone: [nil].
					snack isNil or: [(self getStockDe: snack) < (pedidosPorId at: v)]]
				ifNone: [nil].
	insuficiente
		ifNotNil: 
			[^Respuesta mensaje: 'Stock insuficiente para el producto: ' , insuficiente printString exito: false].

	"Cobrar producto"
	pagoRespuesta := pago cobrar: productos.
	pagoRespuesta tuvoExito ifFalse: [^pagoRespuesta].
	compra := pagoRespuesta getCompra.

	"Registrar compra en la máquina"
	self addCompra: compra.

	"Añadir compra a la trabla abstracta"
	TablaCompras add: compra.

	"Actualizar stock (remover los snacks vendidos)"
	prodIds do: [:p | (snacksDisponibles detect: [:i | i getSnackId = p]) stockMenosUno].
	^pagoRespuesta!

getSnacksDisponibles
	^(((snacksDisponibles select: [:v | v getStock > 0]) collect: [:v | TablaProductos getPorId: (v getSnackId)])
		sort: [:a :b | a getId < b getId])!

getStockDe: unProducto
	| snack |
	snack := snacksDisponibles detect: [:each | each getSnackId = unProducto getId] ifNone: [nil].
	^snack ifNotNil: [snack getStock] ifNil: [0]!

initializeWith: unaUbicacion compras: unasCompras snacksDisponibles: snacks
	(snacks isKindOf: OrderedCollection)
		ifFalse: [self error: 'Parametro Invalido: Los Snacks no son una colección ordenada'].
	(snacks allSatisfy: [:each | each isKindOf: Snack])
		ifFalse: [self error: 'Parametro Invalido: Elemento desconocido encontrado en los snacks'].
	snacksDisponibles := snacks copy.
	super initializeWith: unaUbicacion compras: unasCompras! !

!MaquinaSnack categoriesForMethods!
addSnack:stock:!public! !
comprarProductos:pago:!public! !
getSnacksDisponibles!public! !
getStockDe:!public! !
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

