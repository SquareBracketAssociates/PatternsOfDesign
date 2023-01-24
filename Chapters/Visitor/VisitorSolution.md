## Visitor solutions

	^ aVisitor visitAddition: self
	
	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEAddition left accept: self.
	evaluationOfRight := anEAddition right accept: self.
	^ evaluationOfLeft + evaluationOfRight
	^ aVisitor visitNegation: self

	| aNumber |
	aNumber := anENegation expression accept: self.
	^ aNumber negated
	
	^ aVisitor visitMultiplication: self

	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEMultiplication left accept: self.
	evaluationOfRight := anEMultiplication right accept: self.
	^ evaluationOfLeft * evaluationOfRight

	^ aVisitor visitDivision: self

	| denom numerator |
	denom := aDivision denominator accept: self.
	denom isZero
		ifTrue: [ EZeroDenominator signal ].
	numerator := aDivision numerator accept: self. 
	^ numerator / denom
	^ aVisitor visitVariable: self

	^ bindings at: aVariable id

	| left right |
	left := anAddition left accept: self.
	right := anAddition right accept: self.
	^ '(', left , ' + ', right, ')'

	| left right |
	left := aDivision left accept: self.
	right := aDivision right accept: self.
	^ '(', left , ' / ', right, ')'

	| subExpression |
	subExpression := aNegation expression accept: self.
	^ subExpression , ' negated'

	^ aVariable id asString