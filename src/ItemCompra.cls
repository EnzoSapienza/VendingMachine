"Filed out from Dolphin Smalltalk"!

Object subclass: #ItemCompra
	instanceVariableNames: 'idCompra idProducto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

ItemCompra guid: (GUID fromString: '{35f1706d-1318-45fa-9b0d-91d5ca352f81}')!

ItemCompra comment: ''!

!ItemCompra categoriesForClass!Kernel-Objects! !

!ItemCompra methodsFor!

getIdCompra
    ^idCompra.!

getIdProducto
    ^idProducto.!

initializeWith: unIdCompra producto: unIdProducto
    idCompra := unIdCompra.
    idProducto := unIdProducto.!

setIdCompra: unId
	(unId isKindOf: Integer) ifFalse: [self error: 'Id invalido'].
	idCompra := unId! !

!ItemCompra categoriesForMethods!
getIdCompra!public! !
getIdProducto!public! !
initializeWith:producto:!private! !
setIdCompra:!accessing!public! !
!

!ItemCompra class methodsFor!

prod: unIdProd compra: unIdCompra
	(unIdCompra isKindOf: Integer) & (unIdProd isKindOf: Integer) ifFalse: [self error: 'Id invalido'].
	^self new initializeWith: unIdProd producto: unIdCompra! !

!ItemCompra class categoriesForMethods!
prod:compra:!public! !
!

