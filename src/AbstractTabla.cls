"Filed out from Dolphin Smalltalk"!

Object subclass: #AbstractTabla
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'Collection Id'!

AbstractTabla guid: (GUID fromString: '{3bf80c10-84e7-44d9-ac83-89174c1bb6a1}')!

AbstractTabla comment: ''!

!AbstractTabla categoriesForClass!Kernel-Objects! !

!AbstractTabla class methodsFor!

establecerTabla
	"Private - Instanciar tabla si no existe"

	Collection = nil
		ifTrue: 
			[Collection := OrderedCollection new.
			Id := 1]!

getAll
	^Collection!

getPorId: unId
	self establecerTabla.
	^Collection detect: [:each | each getId = unId] ifNone: [nil]!

new
	self error: 'La clase ' , self printString , ' es estática y no debe instanciarte'! !

!AbstractTabla class categoriesForMethods!
establecerTabla!private! !
getAll!public! !
getPorId:!public! !
new!public! !
!

