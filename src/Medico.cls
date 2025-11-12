"Filed out from Dolphin Smalltalk"!

Object subclass: #Medico
	instanceVariableNames: 'id nombre saldo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Medico guid: (GUID fromString: '{a14d2c54-70e9-42df-9d25-4121496c446b}')!

Medico comment: ''!

!Medico categoriesForClass!Kernel-Objects! !

!Medico methodsFor!

cobrar: unMonto
	saldo := saldo - unMonto!

getId
	^id!

getNombre
	^nombre!

getSaldo
	^saldo!

initializeWith: unNombre saldo: unSaldo
	nombre := unNombre.
	saldo := unSaldo!

printString
	^'Nombre: ' , nombre , ' Saldo: ' , saldo printString , ' ID: ' , id printString!

setId: unId
	(unId isKindOf: Integer) ifFalse: [self error: 'Id no es un entero'].
	unId <= 0 ifTrue: [self error: 'Id es negativo'].
	id = nil ifTrue: [id := unId]! !

!Medico categoriesForMethods!
cobrar:!public! !
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
	^self new initializeWith: unNombre saldo: unSaldo! !

!Medico class categoriesForMethods!
nombre:saldo:!public! !
!

