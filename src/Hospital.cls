"Filed out from Dolphin Smalltalk"!

Object subclass: #Hospital
	instanceVariableNames: 'nombre maquinasExp id'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Hospital guid: (GUID fromString: '{8d2fe85d-8d61-4e20-ac72-c87540dcc737}')!

Hospital comment: ''!

!Hospital categoriesForClass!integrador! !

!Hospital methodsFor!

actualizarPrecioProducto: unId nuevoPrecio: unPrecio
	| producto |
	producto := TablaProductos getPorId: unId.
	producto isNil ifTrue: [self error: 'Producto no encontrado'].
	(unPrecio isKindOf: Number) ifFalse: [self error: 'Precio inválido'].
	producto setPrecio: unPrecio.!

actualizarUbicacionMaquina: unId nuevaUbicacion: unaUbicacion
	| maquina |
	maquina := self getMaquinaPorId: unId.
	maquina isNil ifTrue: [self error: 'Máquina no encontrada'].
	maquina setUbicacion: unaUbicacion!

addMaquina: unaMaquina
	(unaMaquina isKindOf: MaquinaExpendedora)
		ifFalse: [self error: 'Dato invalido: no es una máquina expendedora'].
	unaMaquina setId: id.
	maquinasExp add: unaMaquina.
	id := id + 1.!

addProducto: unProducto
	(unProducto isKindOf: Producto)
		ifFalse: [self error: 'Dato invalido: no es un producto'].
	TablaProductos agregar: unProducto.!

comprar: maquinaId productos: prodIds pago: tipoPago datoCliente: unCliente
	| maquina pago |
	maquina := self getMaquinaPorId: maquinaId.
	pago := Pago tipo: tipoPago datoCliente: unCliente.
	^maquina comprarProductos: prodIds pago: pago!

eliminarMaquinaPorId: unId
	| maquina |
	maquina := self getMaquinaPorId: unId.
	maquina isNil ifTrue: [self error: 'Máquina no encontrada'].
	maquinasExp remove: maquina ifAbsent: [self error: 'Error al eliminar máquina'].!

eliminarProductoPorId: unId
	| producto |
	producto := TablaProductos getPorId: unId.
	producto isNil ifTrue: [self error: 'Producto no encontrado'].
	TablaProductos eliminarPorId: unId.!

getMaquinaPorId: unId
	^maquinasExp detect: [:each | each getId = unId] ifNone: [nil]!

getMaquinas
	^maquinasExp copy!

getNombre
^nombre!

getProductoPorId: unId
	^TablaProductos getPorId: unId!

getProductos
	^TablaProductos listar!

initializeWith: unNombre
	self setNombre: unNombre.
	maquinasExp := OrderedCollection new.
	id := 1!

nombre: anObject
	nombre := anObject!

setNombre: unNombre
	(unNombre isKindOf: String) ifFalse: [self error: 'El nombre no es un string'].
	nombre := unNombre.! !

!Hospital categoriesForMethods!
actualizarPrecioProducto:nuevoPrecio:!public! !
actualizarUbicacionMaquina:nuevaUbicacion:!public! !
addMaquina:!public! !
addProducto:!public! !
comprar:productos:pago:datoCliente:!public! !
eliminarMaquinaPorId:!public! !
eliminarProductoPorId:!public! !
getMaquinaPorId:!public! !
getMaquinas!public! !
getNombre!public! !
getProductoPorId:!public! !
getProductos!public! !
initializeWith:!private! !
nombre:!accessing!public! !
setNombre:!public! !
!

!Hospital class methodsFor!

nombre: unNombre
	^self new
		initializeWith: unNombre! !

!Hospital class categoriesForMethods!
nombre:!public! !
!

