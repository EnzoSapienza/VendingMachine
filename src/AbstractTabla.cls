"Filed out from Dolphin Smalltalk"!

Object subclass: #AbstractTabla
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AbstractTabla guid: (GUID fromString: '{3bf80c10-84e7-44d9-ac83-89174c1bb6a1}')!

AbstractTabla comment: ''!

!AbstractTabla categoriesForClass!Kernel-Objects! !

!AbstractTabla class methodsFor!

getPorId: unId
	self establecerTabla.
	^Collection detect: [:each | each getId = unId]!

new
	self error: 'La clase ' , self printString , ' es estática y no debe instanciarte'! !

!AbstractTabla class categoriesForMethods!
getPorId:!public! !
new!public! !
!

