"Filed out from Dolphin Smalltalk"!

AbstractTabla subclass: #TablaItems
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

TablaItems guid: (GUID fromString: '{7bcfeb3b-ad19-4a85-991f-84806fd802f0}')!

TablaItems comment: ''!

!TablaItems categoriesForClass!Kernel-Objects! !

!TablaItems class methodsFor!

add: item
	(item isKindOf: ItemCompra ) ifFalse: [self error: 'No es un item'].
	self establecerTabla.
	item setId: Id.
	Collection at: Id put: item.
	Id := Id + 1! !

!TablaItems class categoriesForMethods!
add:!public! !
!

