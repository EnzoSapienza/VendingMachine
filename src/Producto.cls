"Filed out from Dolphin Smalltalk"!

Object subclass: #Producto
	instanceVariableNames: 'id nombre precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

