"Filed out from Dolphin Smalltalk"!

Object subclass: #MaquinaExpendedora
	instanceVariableNames: 'id ubicacion compras'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

