## Expressions solutions
	^ value
	^ expression evaluate negated
	^ left evaluate + right evaluate
	^ left evaluate + right evaluate
	^ ENegation new expression: self
	^ ENegation new expression: self
	^ self new expression: anExpression
	^ self new left: anExp ; right: anExp2
	aStream nextPutAll: '( '.
	left printOn: aStream. 
	aStream nextPutAll: ' + '.
	right printOn: aStream.
	aStream nextPutAll: ' )'
	aStream nextPutAll: '( '.
	left printOn: aStream. 
	aStream nextPutAll: ' * '.
	right printOn: aStream.
	aStream nextPutAll: ' )'
	^ expression
	^ (right evaluateWith: anObject) + (left evaluateWith: anObject)