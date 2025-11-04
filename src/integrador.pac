| package |
package := Package name: 'integrador'.
package paxVersion: 1;
	basicComment: 'Ctrl+-'.

package classNames
	add: #AbstractTabla;
	add: #Compras;
	add: #Hospital;
	add: #ItemCompra;
	add: #MaquinaCafe;
	add: #MaquinaExpendedora;
	add: #MaquinaSnack;
	add: #Medico;
	add: #Pago;
	add: #PagoCredito;
	add: #PagoQR;
	add: #PagoTarjeta;
	add: #Producto;
	add: #Snack;
	add: #SnackDisponibles;
	add: #TablaCompras;
	add: #TablaMedicos;
	add: #TablaProductos;
	add: #TipoCafe;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\..\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\..\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time').

package!

"Class Definitions"!

Object subclass: #AbstractTabla
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'Collection Id'!

Object subclass: #Compras
	instanceVariableNames: 'id cliente esGenerico fecha'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Hospital
	instanceVariableNames: 'nombre maquinasExp verRecaudoSemanal'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #ItemCompra
	instanceVariableNames: 'idCompra idProducto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #MaquinaExpendedora
	instanceVariableNames: 'id ubicacion compras'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Medico
	instanceVariableNames: 'id nombre saldo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Pago
	instanceVariableNames: 'id idCompra monto cobrado'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Producto
	instanceVariableNames: 'id nombre precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #SnackDisponibles
	instanceVariableNames: 'idSnack stock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AbstractTabla subclass: #TablaCompras
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AbstractTabla subclass: #TablaMedicos
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AbstractTabla subclass: #TablaProductos
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MaquinaExpendedora subclass: #MaquinaCafe
	instanceVariableNames: 'lecheDisponible capsulas aguaDisponible cafesDisponibles'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MaquinaExpendedora subclass: #MaquinaSnack
	instanceVariableNames: 'snacksDisponibles'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Pago subclass: #PagoCredito
	instanceVariableNames: 'idMedico'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Pago subclass: #PagoQR
	instanceVariableNames: 'link'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Pago subclass: #PagoTarjeta
	instanceVariableNames: 'numTarjeta'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Producto subclass: #Snack
	instanceVariableNames: 'marca peso'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Producto subclass: #TipoCafe
	instanceVariableNames: 'azucarNecesaria lecheNecesaria'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"End of package definition"!

"Source Globals"!

"Classes"!

AbstractTabla guid: (GUID fromString: '{3bf80c10-84e7-44d9-ac83-89174c1bb6a1}')!

AbstractTabla comment: ''!

!AbstractTabla categoriesForClass!Kernel-Objects! !

!AbstractTabla class methodsFor!

establecerTabla
	"Private - Instanciar tabla si no existe"

	Collection = nil
		ifTrue: 
			[Collection := OrderedCollection new.
			Id := 1]!

getAll
	^Collection!

getPorId: unId
	self establecerTabla.
	^Collection detect: [:each | each getId = unId] ifNone: [nil]!

new
	self error: 'La clase ' , self printString , ' es estática y no debe instanciarte'! !

!AbstractTabla class categoriesForMethods!
establecerTabla!private! !
getAll!public! !
getPorId:!public! !
new!public! !
!

Compras guid: (GUID fromString: '{cba00593-ee11-47b7-92c9-367c2c530167}')!

Compras comment: ''!

!Compras categoriesForClass!Kernel-Objects! !

!Compras methodsFor!

comprarSnack:snackId metodo: unMetodo cliente: unCliente
!

esGenerico
^esGenerico.!

getCliente
^cliente.!

getFecha
^fecha!

getId
^id.!

initializeWith: unCliente generic: unBooleano fecha: unaFecha
	unCliente = nil | (unCliente isKindOf: Medico)
		ifFalse: [self error: 'El cliente no es un dato nulo o un medico'].
	(unBooleano isKindOf: Boolean)
		ifFalse: [self error: 'No se aclaró si el cliente es generico o no'].
	(unaFecha isKindOf: Date) ifFalse: [self error: 'Fecha no valida'].
	cliente := unCliente.
	esGenerico := unBooleano.
	fecha := unaFecha!

setId: unId
	(unId isKindOf: Integer) ifTrue: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]! !

!Compras categoriesForMethods!
comprarSnack:metodo:cliente:!public! !
esGenerico!public! !
getCliente!public! !
getFecha!public! !
getId!public! !
initializeWith:generic:fecha:!private! !
setId:!public! !
!

!Compras class methodsFor!

cliente: unCliente
	unCliente class = Medico ifFalse: [self error: 'Atributo Inválido'].
	^self new
		initializeWith: unCliente
		generic: false
		fecha: Date today!

cliente: unCliente fecha: unaFecha
	unCliente class = Medico ifFalse: [self error: 'Atributo Inválido'].
	^self new
		initializeWith: unCliente
		generic: false
		fecha: unaFecha!

nuevoGenerico: unaFecha
	^self new
		initializeWith: nil
			generic: true
			fecha: unaFecha! !

!Compras class categoriesForMethods!
cliente:!public! !
cliente:fecha:!public! !
nuevoGenerico:!public! !
!

Hospital guid: (GUID fromString: '{8d2fe85d-8d61-4e20-ac72-c87540dcc737}')!

Hospital comment: ''!

!Hospital categoriesForClass!integrador! !

!Hospital methodsFor!

addMaquina: maquina
maquinasExp add: maquina.!

comprar: maquinaId producto: prodId pago: tipoPago datoCliente: unCliente
	| maquina respuesta |
	maquina := self getMaquinaPorId: maquinaId.
	respuesta := maquina
				comprarProducto: prodId
				pago: tipoPago
				datoCliente: unCliente.
	^respuesta!

comprar: maquinaId productos: prodIds pago: tipoPago datoCliente: unCliente
	| maquina respuesta |
	maquina := self getMaquinaPorId: maquinaId.
	respuesta := maquina
				comprarProducto: prodIds
				pago: tipoPago
				datoCliente: unCliente.
	^respuesta!

getMaquinaPorId: unId
	^maquinasExp detect: [:each | each getId = unId] ifNone: [nil]!

getNombre
^nombre!

initializeWith: unNombre
	self setNombre: unNombre.
	maquinasExp := OrderedCollection new!

setNombre: unNombre
	(unNombre isKindOf: String) ifFalse: [self error: 'El nombre no es un string'].
	nombre := unNombre! !

!Hospital categoriesForMethods!
addMaquina:!public! !
comprar:producto:pago:datoCliente:!public! !
comprar:productos:pago:datoCliente:!public! !
getMaquinaPorId:!public! !
getNombre!public! !
initializeWith:!private! !
setNombre:!public! !
!

!Hospital class methodsFor!

nombre: unNombre
	^self new
		initializeWith: unNombre! !

!Hospital class categoriesForMethods!
nombre:!public! !
!

ItemCompra guid: (GUID fromString: '{35f1706d-1318-45fa-9b0d-91d5ca352f81}')!

ItemCompra comment: ''!

!ItemCompra categoriesForClass!Kernel-Objects! !

MaquinaExpendedora guid: (GUID fromString: '{a06523da-c26c-46c1-a1b7-fea446bf6b2d}')!

MaquinaExpendedora comment: ''!

!MaquinaExpendedora categoriesForClass!integrador! !

!MaquinaExpendedora methodsFor!

getCompras
	^compras!

getId
	^id!

getUbicacion
	^ubicacion!

initializeWith: unaUbicacion compras: unasCompras
	(unaUbicacion isKindOf: String)
		ifFalse: [self error: 'Parametro Invalido: La ubicacion no es un string'].
	(unasCompras isKindOf: OrderedCollection)
		ifFalse: [self error: 'Parametro Invalido: Las compras no es una colección'].
	(unasCompras allSatisfy: [:each | each class = Compras])
		ifFalse: [self error: 'Parametro Invaldo: no-compras encontradas en la lista'].
	ubicacion := unaUbicacion.
	compras := unasCompras!

setId: unId
	(unId isKindOf: Integer) ifTrue: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id := unId! !

!MaquinaExpendedora categoriesForMethods!
getCompras!public! !
getId!public! !
getUbicacion!public! !
initializeWith:compras:!private! !
setId:!public! !
!

!MaquinaExpendedora class methodsFor!

new
    self = MaquinaExpendedora ifTrue: [
        self error: 'La clase MaquinaExpendedora es abstracta y no puede instanciarse' ].
    ^super new.! !

!MaquinaExpendedora class categoriesForMethods!
new!public! !
!

Medico guid: (GUID fromString: '{a14d2c54-70e9-42df-9d25-4121496c446b}')!

Medico comment: ''!

!Medico categoriesForClass!Kernel-Objects! !

!Medico methodsFor!

getId
^id.!

getNombre
^nombre.!

getSaldo
    ^saldo.!

initializeWith: unNombre saldo: unSaldo
	nombre := unNombre.
	saldo := unSaldo!

printString
	^'Nombre: ' , nombre , ' Saldo: ' , saldo printString , ' ID: ', id printString!

setId: unId
	(unId isKindOf: Integer) ifFalse: [self error: 'Id no es un entero'].
	unId <= 0 ifTrue: [self error: 'Id es negativo'].
	id = nil ifTrue: [id := unId]! !

!Medico categoriesForMethods!
getId!public! !
getNombre!public! !
getSaldo!public! !
initializeWith:saldo:!private! !
printString!public! !
setId:!public! !
!

!Medico class methodsFor!

nombre: unNombre saldo: unSaldo
(unNombre isKindOf: String) ifFalse: [self error: 'Parametro Invalido'].
(unSaldo isKindOf: Number) ifFalse: [self error: 'Parametro Invalido'].
^self new initializeWith: unNombre saldo: unSaldo.! !

!Medico class categoriesForMethods!
nombre:saldo:!public! !
!

Pago guid: (GUID fromString: '{a4936274-1121-4049-9b73-4319c3d9314d}')!

Pago comment: ''!

!Pago categoriesForClass!Kernel-Objects! !

!Pago methodsFor!

cobrar
(self = Pago) ifTrue: [self error: 'Pago Abstracto no debe implementar este método'. ].!

getCompraId
^idCompra!

getId
	^id!

getMonto
	^monto!

initializeWith: unId monto: unMonto
	(unId isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: id no es un entero'].
	(unMonto isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: monto no es un número'].
	unMonto > 0 ifFalse: [self error: 'Parametro Invalido: monto es negativo'].
	idCompra := unId.
	monto := unMonto.
	cobrado := false.!

setId: unId
	(unId isKindOf: Integer) ifTrue: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]! !

!Pago categoriesForMethods!
cobrar!public! !
getCompraId!public! !
getId!public! !
getMonto!public! !
initializeWith:monto:!private! !
setId:!public! !
!

!Pago class methodsFor!

new
    self = Pago ifTrue: [
        self error: 'La clase Pago es abstracta y no puede instanciarse' ].
    ^super new.!

tipo: tipoDePago
! !

!Pago class categoriesForMethods!
new!public! !
tipo:!public! !
!

Producto guid: (GUID fromString: '{27f458ec-9c07-4573-aa70-3cc6f84955d3}')!

Producto comment: ''!

!Producto categoriesForClass!Kernel-Objects! !

!Producto methodsFor!

getId
	^id!

getNombre
	^nombre!

getPrecio
	^precio!

initializeWithNombre: unNombre precio: unPrecio
nombre := unNombre.
precio := unPrecio.!

setId: unId
	(unId isKindOf: Integer) ifTrue: [self error: 'Id invalido'].
	unId <= 0 ifTrue: [self error: 'Id invalido'].
	id = nil ifTrue: [id := unId]! !

!Producto categoriesForMethods!
getId!public! !
getNombre!public! !
getPrecio!public! !
initializeWithNombre:precio:!private! !
setId:!public! !
!

!Producto class methodsFor!

new
    self = Producto ifTrue: [
        self error: 'La clase Producto es abstracta y no puede instanciarse' ].
    ^super new.! !

!Producto class categoriesForMethods!
new!public! !
!

SnackDisponibles guid: (GUID fromString: '{10dbba14-d007-4202-8e8a-baa0c9a3e746}')!

SnackDisponibles comment: ''!

!SnackDisponibles categoriesForClass!Kernel-Objects! !

!SnackDisponibles methodsFor!

getSnackId
	^idSnack!

getStock
	^stock!

initializeWith: unStock
stock := unStock.! !

!SnackDisponibles categoriesForMethods!
getSnackId!public! !
getStock!public! !
initializeWith:!private! !
!

!SnackDisponibles class methodsFor!

stock: unStock
	(unStock isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: Stock no es un número'].
	unStock > 0 ifFalse: [self error: 'Parametro Invalido: Stock no es un positivo'].
	^self new initializeWith: unStock! !

!SnackDisponibles class categoriesForMethods!
stock:!public! !
!

TablaCompras guid: (GUID fromString: '{e4d02a7a-13dd-48f8-82f1-4efa534b8323}')!

TablaCompras comment: ''!

!TablaCompras categoriesForClass!Kernel-Objects! !

!TablaCompras class methodsFor!

addCompra: compra
	(compra isKindOf: Compras) ifFalse: [self error: 'No es una compra'].
	self establecerTabla.
	compra setId: Id.
	Collection add: compra.
	Id := Id + 1.! !

!TablaCompras class categoriesForMethods!
addCompra:!public! !
!

TablaMedicos guid: (GUID fromString: '{9492eed8-3e56-45d6-b9f2-875a1020791e}')!

TablaMedicos comment: ''!

!TablaMedicos categoriesForClass!Kernel-Objects! !

!TablaMedicos class methodsFor!

addMedico: medico
	(medico isKindOf: Medico) ifFalse: [self error: 'No es un medico'].
	self establecerTabla.
	medico setId: Id.
	Collection add: medico.
	Id := Id + 1! !

!TablaMedicos class categoriesForMethods!
addMedico:!public! !
!

TablaProductos guid: (GUID fromString: '{75884272-4ebb-4a0e-a867-576c236e7169}')!

TablaProductos comment: ''!

!TablaProductos categoriesForClass!Kernel-Objects! !

!TablaProductos class methodsFor!

addProducto: producto
	(producto isKindOf: Producto) ifFalse: [self error: 'No es un producto'].
	self establecerTabla.
	producto setId: Id.
	Collection add: producto.
	Id := Id + 1.! !

!TablaProductos class categoriesForMethods!
addProducto:!public! !
!

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

MaquinaSnack guid: (GUID fromString: '{df0b6fa9-5874-4b3c-b68b-d30bb8784a30}')!

MaquinaSnack comment: ''!

!MaquinaSnack categoriesForClass!integrador! !

!MaquinaSnack methodsFor!

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

PagoCredito guid: (GUID fromString: '{0e0fe7b7-9a0d-467b-9801-0e1babbe0620}')!

PagoCredito comment: ''!

!PagoCredito categoriesForClass!Kernel-Objects! !

!PagoCredito methodsFor!

getMedicoId
	^idMedico!

initializeWith: medicoId Compra: unaCompraId monto: unMonto
	(idMedico isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: id no es un entero'].
	idMedico := medicoId.
	super initializeWith: unaCompraId monto: unMonto! !

!PagoCredito categoriesForMethods!
getMedicoId!public! !
initializeWith:Compra:monto:!private! !
!

!PagoCredito class methodsFor!

compra: unaCompraId monto: unMonto medico: idMedico
	^self new
		initializeWith: idMedico
		Compra: unaCompraId
		monto: unMonto! !

!PagoCredito class categoriesForMethods!
compra:monto:medico:!public! !
!

PagoQR guid: (GUID fromString: '{0e8d8b53-a83a-4ec4-aa21-29598404f805}')!

PagoQR comment: ''!

!PagoQR categoriesForClass!Kernel-Objects! !

!PagoQR methodsFor!

getLink
	^link!

initializeWith: unLink Compra: unaCompraId monto: unMonto
	(unLink isKindOf: String) ifFalse: [self error: 'Parametro Invalido: el link no es un string'].
	link := unLink.
	super initializeWith: unaCompraId monto: unMonto! !

!PagoQR categoriesForMethods!
getLink!public! !
initializeWith:Compra:monto:!private! !
!

!PagoQR class methodsFor!

compra: unaCompraId monto: unMonto link: unLink
	^self new
		initializeWith: unLink
		Compra: unaCompraId
		monto: unMonto! !

!PagoQR class categoriesForMethods!
compra:monto:link:!public! !
!

PagoTarjeta guid: (GUID fromString: '{bb3e76f1-3feb-4486-b7f4-cfe5ef602ce0}')!

PagoTarjeta comment: ''!

!PagoTarjeta categoriesForClass!Kernel-Objects! !

!PagoTarjeta methodsFor!

getTarjeta
	^numTarjeta!

initializeWith: unaTarjeta Compra: unaCompraId monto: unMonto
	(unaTarjeta isKindOf: Integer)
		ifFalse: [self error: 'Parametro Invalido: la tarjeta no es un entero'].
	numTarjeta := unaTarjeta.
	super initializeWith: unaCompraId monto: unMonto! !

!PagoTarjeta categoriesForMethods!
getTarjeta!public! !
initializeWith:Compra:monto:!private! !
!

!PagoTarjeta class methodsFor!

compra: unaCompraId monto: unMonto tarjeta: unaTarjeta
	^self new
		initializeWith: unaTarjeta
		Compra: unaCompraId
		monto: unMonto! !

!PagoTarjeta class categoriesForMethods!
compra:monto:tarjeta:!public! !
!

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

"Binary Globals"!

