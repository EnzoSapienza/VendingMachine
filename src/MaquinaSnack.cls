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

