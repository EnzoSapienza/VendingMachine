| package |
package := Package name: 'integrador'.
package paxVersion: 1;
	basicComment: 'Ctrl+-'.

package classNames
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
	add: #TipoCafe;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin').

package!

"Class Definitions"!

Object subclass: #Compras
	instanceVariableNames: 'id cliente esGenerico'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Hospital
	instanceVariableNames: ''
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
	instanceVariableNames: 'id idCompra monto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Producto
	instanceVariableNames: 'id nombre precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #SnackDisponibles
	instanceVariableNames: 'idSnack Stock'
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

Compras guid: (GUID fromString: '{cba00593-ee11-47b7-92c9-367c2c530167}')!

Compras comment: ''!

!Compras categoriesForClass!Kernel-Objects! !

!Compras methodsFor!

esGenerico
^esGenerico.!

getCliente
^cliente.!

getId
^id.!

initializeWith: unCliente generic: unBooleano
cliente := unCliente.
esGenerico := unBooleano.! !

!Compras categoriesForMethods!
esGenerico!public! !
getCliente!public! !
getId!public! !
initializeWith:generic:!private! !
!

!Compras class methodsFor!

cliente: unCliente
(unCliente class = Medico) ifFalse: [self error: 'Atributo Inválido'].
^self new initializeWith: unCliente generic: false.!

generico
^((self new) initializeWith: nil generic: true)! !

!Compras class categoriesForMethods!
cliente:!public! !
generico!public! !
!

Hospital guid: (GUID fromString: '{8d2fe85d-8d61-4e20-ac72-c87540dcc737}')!

Hospital comment: ''!

!Hospital categoriesForClass!integrador! !

!Hospital methodsFor!

verMaquinasExp
	^0! !

!Hospital categoriesForMethods!
verMaquinasExp!public! !
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
	compras := unasCompras! !

!MaquinaExpendedora categoriesForMethods!
getCompras!public! !
getUbicacion!public! !
initializeWith:compras:!private! !
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

getNombre
^nombre.!

getSaldo
    ^saldo.!

initializeWith: unNombre saldo: unSaldo
	nombre := unNombre.
	saldo := unSaldo! !

!Medico categoriesForMethods!
getNombre!public! !
getSaldo!public! !
initializeWith:saldo:!private! !
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

getCompraId
^idCompra!

getMonto
	^monto!

initializeWith: unId monto: unMonto
	(unId isKindOf: Integer) ifFalse: [self error: 'Parametro Invalido: id no es un entero'].
	(unMonto isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: monto no es un número'].
	unMonto > 0 ifFalse: [self error: 'Parametro Invalido: monto es negativo'].
	idCompra := unId.
	monto := unMonto! !

!Pago categoriesForMethods!
getCompraId!public! !
getMonto!public! !
initializeWith:monto:!private! !
!

!Pago class methodsFor!

new
    self = Pago ifTrue: [
        self error: 'La clase Pago es abstracta y no puede instanciarse' ].
    ^super new.! !

!Pago class categoriesForMethods!
new!public! !
!

Producto guid: (GUID fromString: '{27f458ec-9c07-4573-aa70-3cc6f84955d3}')!

Producto comment: ''!

!Producto categoriesForClass!Kernel-Objects! !

!Producto methodsFor!

getCodigo
^codigo.!

getNombre
	^nombre!

getPrecio
	^precio!

getProvedor
^proveedor.!

getStock
^stock.!

getStockMinimo
^stockminimo.!

getTipo
^tipo.!

initializeWithNombre: unNombre precio: unPrecio
nombre := unNombre.
precio := unPrecio.!

setCodigo: unCodigo
codigo := unCodigo.!

setNombre: unNombre
nombre := unNombre.!

setPrecio: unPrecio
precio := unPrecio.!

setProvedor: unProveedor
proveedor := unProveedor.!

setStock: unStock
stock := unStock.!

setStockMinimo: unStock
stockminimo := unStock.!

setTipo: unTipo
tipo := unTipo.! !

!Producto categoriesForMethods!
getCodigo!public! !
getNombre!public! !
getPrecio!public! !
getProvedor!public! !
getStock!public! !
getStockMinimo!public! !
getTipo!public! !
initializeWithNombre:precio:!private! !
setCodigo:!public! !
setNombre:!public! !
setPrecio:!public! !
setProvedor:!public! !
setStock:!public! !
setStockMinimo:!public! !
setTipo:!public! !
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
	^Stock!

initializeWith: unStock
Stock := unStock.! !

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

MaquinaCafe guid: (GUID fromString: '{50a9010d-d5d4-433c-b498-4b2aec432c36}')!

MaquinaCafe comment: ''!

!MaquinaCafe categoriesForClass!integrador! !

!MaquinaCafe methodsFor!

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

