"Filed out from Dolphin Smalltalk"!

Object subclass: #MaquinaExpendedora
	instanceVariableNames: 'id ubicacion compras'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

