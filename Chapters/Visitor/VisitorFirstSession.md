```

	^ aVariable id asString

	^ aVariable id

	| subExpression |
	subExpression := aNegation expression accept: self.
	^ subExpression , ' negated'

	|  subExpression |
	subExpression := aNegation expression accept: self.
	^ subExpression , ' negated'

	| left right subExpression |
	subExpression := aNegation expression accept: self.
	^ subExpression , ' negated'

	| left right |
	left := aDivision left accept: self.
	right := aDivision right accept: self.
	^ '(', left , ' / ', right, ')'

	| left right |
	left := anAddition left accept: self.
	right := anAddition right accept: self.
	^ '(', left , ' + ', right, ')'

	| left right |
	left := aMultiplication left accept: self.
	right := aMultiplication right accept: self.
	^ '(', left , ' * ', right, ')'

	| left right |
	left := aMultiplication left accept: self.
	right := aMultiplication right accept: self.
	^ '(', left , ' * ', right, ')'

	| left right |
	left := aMultiplication left accept: self.
	right := aMultiplication right accept: self.
	^ '(', left , ' * ', right, ')'

	^ aConstant value asString

	^ aConstant value asString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: 'bindings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'

	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEMultiplication left accept: self.
	evaluationOfRight := anEMultiplication right accept: self.
	^ evaluationOfLeft * evaluationOfRight

	| evaluationOfLeft evaluationOfRight |
		1/0.

	evaluationOfLeft := anEMultiplication left accept: self.
	evaluationOfRight := anEMultiplication right accept: self.
	^ evaluationOfLeft * evaluationOfRight

	| expression result |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 3).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 2

	| expression result |
	1/0.
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 3).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 2

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	bindings := aDictionary

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.
	
	^ self accept: EEvaluatorVisitor new
	
	^ self accept: EEvaluatorVisitor new
	
	^ self accept: EEvaluatorVisitor new
	
	^ self accept: EEvaluatorVisitor new
	| expression result visitor |
	expression := EVariable id: #answerToTheQuestion.

	visitor := EEvaluatorVisitor new.
	visitor at: #answerToTheQuestion put: 42.
	
	result := expression accept: visitor.
	self assert: result equals: 42
	^ id

	bindings at: anId put: aValue
	| expression result visitor |
	expression := (EVariable id: #answerToTheQuestion) negated.

	visitor := EEvaluatorVisitor new.
	visitor at: #answerToTheQuestion put: 42.
	
	result := expression accept: visitor.
	self assert: result equals: 42
	| expression result visitor |
	expression := (EVariable id: #answerToTheQuestion) negated.
	visitor := EEvaluatorVisitor new.
	result := expression accept: visitor.
	self assert: result equals: 42

	^ self new
		id: anId;
		yourself

	^ self new
		id: anId;
		yourself

	| expression result |
	expression := (EVariable id: #answerToTheQuestion) negated.

	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 42

	^ bindings at: aVariable id

	super initialize.
	bindings := Dictionary new
	instanceVariableNames: 'bindings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'

	^ aVisitor visitVariable: self

	| expression  |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 0).
	
	self
		should: [expression accept: EEvaluatorVisitor new]
		raise: EZeroDenominator

	| expression result |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 0).
	
	self
		should: [expression accept: EEvaluatorVisitor new]
		raise: EZeroDenominator

	^ self new
		numerator: aNumeratorExpression;
		denominator: aDenominatorExpression;
		yourself

	^ self new
		numerator: aNumeratorExpression;
		denominator: aDenominatorExpression;
		yourself

	| expression result |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 3).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 2

	| denom numerator |
	denom := aDivision denominator accept: self.
	denom isZero
		ifTrue: [ EZeroDenominator signal ].
	numerator := aDivision numerator accept: self. 
	^ numerator / denom

	^ aVisitor visitDivision: self
	^ denominator
	^ numerator

	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEMultiplication left accept: self.
	evaluationOfRight := anEMultiplication right accept: self.
	^ evaluationOfLeft * evaluationOfRight
	self shouldBeImplemented.
	
	^ aVisitor visitMultiplication: self
	self shouldBeImplemented.

	| expression result |
	expression := EMultiplication
		left: (EConstant value: 7)
		right: (EConstant value: -2).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: -14
	^ (expression evaluateWith: anObject) negated
	
	^ self accept: EEvaluatorVisitor new

	| aNumber |
	aNumber := anENegation expression accept: self.
	^ aNumber negated
	
	^ expression
	self shouldBeImplemented.
	
	^ aVisitor visitNegation: self
	self shouldBeImplemented.

	| expression result |
	expression := (EConstant value: 7) negated.
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: -7
	^ (right evaluateWith: anObject) + (left evaluateWith: anObject)
	
	^ self accept: EEvaluatorVisitor new

	^ aVisitor visitAddition: self
	
	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEAddition left accept: self.
	evaluationOfRight := anEAddition right accept: self.
	^ evaluationOfLeft + evaluationOfRight
	
	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEAddition left accept: self.
	evaluationOfRight := anEAddition left accept: self.
	^ evaluationOfLeft + evaluationOfRight
	^ right
	^ left
	self shouldBeImplemented.

	| expression result |
	expression := EAddition
		left: (EConstant value: 7)
		right: (EConstant value: -2).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 5

	aVisitor visitAddition: self
	
	^ self accept: EEvaluatorVisitor new

	| constant result |
	constant := EConstant value: 5.
	
	result := constant accept: EEvaluatorVisitor new.
	
	self assert: result equals: 5

	| constant result |
	constant := EConstant value: 5.
	
	result := constant accept: EEvaluatorVisitor new.
	
	self assert: result equals: 5
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'

	^ aConstant value

	^ aConstant value
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'

	^ aVisitor visitConstant: self

	self subclassResponsibility

	self subclassResponsibility
	aStream nexPutAll: id asString

	id := aSymbol

	^ (aBindingSet at: id)

	aStream nextPut: $-.
	aStream nextPut: Character space. 
	expression printOn: aStream

	^ expression
	expression := anEConstant
	^ (expression evaluateWith: anObject) negated
	^ self new expression: anInteger

	^ ENegation new expression: self
	
	aStream 
		nextPutAll: '( ';
		nextPutAll: numerator printString; 
		nextPutAll: ' / ';
		nextPutAll: denominator printString;
		nextPutAll: ' )'
	numerator := anInteger
	| denom |
	denom := denominator evaluateWith: Dictionary new.
	denom isZero
		ifTrue: [ EZeroDenominator signal ].
	^ (numerator evaluateWith: Dictionary new) / denom
	denominator := anInteger

	value := anInteger

	^ value

	aStream nextPutAll: value printString
	
	^ value
	
	^ self new value: anInteger 
	<sampleInstance>
	^ self new value: 5 
	^ ' * '
	^ (left evaluateWith: anObject) * (right evaluateWith: anObject)

	^ self new left: anInteger ; right: anInteger2
	<sampleInstance>
	| ep1 ep2 |
	ep1 := (EConstant new value: 5).
	ep2 := (EConstant new value: 3).
   ^ EMultiplication new left: ep1 ; right: ep2 
	right := anEConstant
	
	aStream nextPutAll: '( '.
	left printOn: aStream.
	aStream nextPutAll: self operatorString.
	right printOn: aStream.
	aStream nextPutAll: ' )'
	^ ' + '
	left := anEConstant
	
	
	^ (right evaluateWith: anObject) + (left evaluateWith: anObject)

	^ self new left: anInteger ; right: anInteger2 
	<sampleInstance>
	| ep1 ep2 |
	ep1 := (EConstant new value: 5).
	ep2 := (EConstant new value: 3).
	^ self new left: ep1 ; right: ep2 

	self assert: ((EVariable new id: #x) negated evaluateWith: {#x -> 10} asDictionary) equals: -10. 

	self assert: ((EVariable new id: #x) evaluateWith: {#x -> 10} asDictionary) equals: 10. 

	| ep1 ep2 add |
	ep1 := (EVariable new id: #x).
	ep2 := (EVariable new id: #y).
	add := (EAddition left: ep1 right: ep2).
	
	self assert: (add evaluateWith: { #x -> 10 . #y -> 2 } asDictionary) equals: 12.
	self assert: (add evaluateWith: { #x -> 10 . #y -> 12 } asDictionary) equals: 22
	
	| exp |
	exp := (EConstant new value: 6) negated.
	self assert: exp printString = '- 6'. 
	self assert: exp negated printString = '6'. 

	
	
	self assert: ((EConstant new value: 6) negated negated evaluateWith: Dictionary new) equals: 6
	
	| exp |
	exp := (EConstant new value: 6).
	self assert: exp negated class = ENegation. 
	self assert: exp negated negated equals: exp.

	
	
	self assert: ((EConstant new value: 6) negated evaluateWith: Dictionary new) equals: -6.
	self assert: (EAddition fivePlusThree negated evaluateWith: Dictionary new) equals: -8
	self assert: ((ENegation expression: (EConstant value: 5)) evaluateWith: Dictionary new) equals: -5
	self assert: ((ENegation new expression: (EConstant new value: 5)) evaluateWith: Dictionary new) equals: -5

	self assert: (EMultiplication fiveTimesThree) negated printString equals:  '- ( 5 * 3 )'
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self assert: ((EMultiplication left: ep1 right: ep2) evaluateWith: Dictionary new) equals: 15
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self
		assert:
			((EMultiplication new
				right: ep1;
				left: ep2) negated evaluateWith: Dictionary new)
		equals: -15
	self assert: (EMultiplication fiveTimesThree negated evaluateWith: Dictionary new) equals: -15
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self
		assert:
			((EMultiplication new
				right: ep1;
				left: ep2) evaluateWith: Dictionary new)
		equals: 15
	self
		should: [ 
			(EDivision new
				numerator: (EConstant value: 4);
				denominator: (EConstant value: 0)) evaluateWith: Dictionary new ]
		raise: EZeroDenominator
	self
		assert:
			((EDivision new
				numerator: (EConstant value: 4);
				denominator: (EConstant value: 2)) evaluateWith: Dictionary new)
		equals: 2
	
	self assert:(EConstant value: 5) value equals: 5.

	
	
	self assert:(EConstant new value: 5) value equals:5.
	self assert:(EConstant constant5) value equals:5
	
	
	self assert:(EConstant new value: 5) printString equals: '5'.
	
	self assert: ((EConstant new value: 6) negated evaluateWith: Dictionary new) equals: -6
	self assert: ((EConstant new value: 5) evaluateWith: Dictionary new) equals: 5
	self assert: (EAddition fivePlusThree) printString equals:  '( 5 + 3 )'.
	self assert: (EAddition fivePlusThree) negated printString equals:  '- ( 5 + 3 )'
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self
		assert:
			((EAddition new
				right: ep1;
				left: ep2) negated evaluateWith: Dictionary new)
		equals: -8
	| ep1 ep2 |
	ep1 := ENegation new expression: (EConstant new value: 5).
	ep2 := EConstant new value: 3.
	self
		assert:
			((EAddition new
				right: ep1;
				left: ep2) evaluateWith: Dictionary new)
		equals: -2
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self assert: ((EAddition left: ep1 right: ep2) evaluateWith: Dictionary new) equals: 8
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self
		assert:
			((EAddition new
				right: ep1;
				left: ep2) evaluateWith: Dictionary new)
		equals: 8
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: 'id'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: 'expression'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: 'numerator denominator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: 'left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'