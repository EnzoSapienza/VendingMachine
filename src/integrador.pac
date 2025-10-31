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
	'..\..\..\..\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin').

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
	instanceVariableNames: 'idSnack stock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MaquinaExpendedora subclass: #MaquinaCafe
	instanceVariableNames: 'lecheDisponibleEnMililitros capsulas aguaDisponibleEnMililitros cafesDisponibles'
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

MaquinaCafe guid: (GUID fromString: '{50a9010d-d5d4-433c-b498-4b2aec432c36}')!

MaquinaCafe comment: ''!

!MaquinaCafe categoriesForClass!integrador! !

MaquinaSnack guid: (GUID fromString: '{df0b6fa9-5874-4b3c-b68b-d30bb8784a30}')!

MaquinaSnack comment: ''!

!MaquinaSnack categoriesForClass!integrador! !

PagoCredito guid: (GUID fromString: '{0e0fe7b7-9a0d-467b-9801-0e1babbe0620}')!

PagoCredito comment: ''!

!PagoCredito categoriesForClass!Kernel-Objects! !

PagoQR guid: (GUID fromString: '{0e8d8b53-a83a-4ec4-aa21-29598404f805}')!

PagoQR comment: ''!

!PagoQR categoriesForClass!Kernel-Objects! !

PagoTarjeta guid: (GUID fromString: '{bb3e76f1-3feb-4486-b7f4-cfe5ef602ce0}')!

PagoTarjeta comment: ''!

!PagoTarjeta categoriesForClass!Kernel-Objects! !

Snack guid: (GUID fromString: '{3f66b3cb-debd-42e1-a656-54f5d11f97a3}')!

Snack comment: ''!

!Snack categoriesForClass!Kernel-Objects! !

TipoCafe guid: (GUID fromString: '{344f7eb8-4991-41bd-933e-c383b17f8eb8}')!

TipoCafe comment: ''!

!TipoCafe categoriesForClass!Kernel-Objects! !

"Binary Globals"!

