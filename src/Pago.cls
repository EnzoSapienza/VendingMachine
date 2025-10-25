"Filed out from Dolphin Smalltalk"!

Object subclass: #Pago
	instanceVariableNames: 'id idCompra monto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

