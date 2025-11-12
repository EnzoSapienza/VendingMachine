"Filed out from Dolphin Smalltalk"!

Object subclass: #SnackDisponibles
	instanceVariableNames: 'idSnack stock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

SnackDisponibles guid: (GUID fromString: '{10dbba14-d007-4202-8e8a-baa0c9a3e746}')!

SnackDisponibles comment: ''!

!SnackDisponibles categoriesForClass!Kernel-Objects! !

!SnackDisponibles methodsFor!

getSnackId
	^idSnack!

getStock
	^stock!

initializeWith: unStock idSnack: unIdSnack
	stock := unStock.
	idSnack := unIdSnack!

stockMenosUno
stock := stock - 1.! !

!SnackDisponibles categoriesForMethods!
getSnackId!public! !
getStock!public! !
initializeWith:idSnack:!private! !
stockMenosUno!public! !
!

!SnackDisponibles class methodsFor!

stock: unStock idSnack: anInteger
	(unStock isKindOf: Number) ifFalse: [self error: 'Parametro Invalido: Stock no es un número'].
	unStock > 0 ifFalse: [self error: 'Parametro Invalido: Stock no es un positivo'].
	^self new initializeWith: unStock idSnack: anInteger! !

!SnackDisponibles class categoriesForMethods!
stock:idSnack:!public! !
!

