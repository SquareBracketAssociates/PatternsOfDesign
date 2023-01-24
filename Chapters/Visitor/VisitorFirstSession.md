```EPrinterVisitor >> visitVariable: aVariable

	^ aVariable id asString``````EPrinterVisitor >> visitVariable: aVariable

	^ aVariable id``````EPrinterVisitor >> visitNegation: aNegation

	| subExpression |
	subExpression := aNegation expression accept: self.
	^ subExpression , ' negated'``````EPrinterVisitor >> visitNegation: aNegation

	|  subExpression |
	subExpression := aNegation expression accept: self.
	^ subExpression , ' negated'``````EPrinterVisitor >> visitNegation: aNegation

	| left right subExpression |
	subExpression := aNegation expression accept: self.
	^ subExpression , ' negated'``````EPrinterVisitor >> visitDivision: aDivision

	| left right |
	left := aDivision left accept: self.
	right := aDivision right accept: self.
	^ '(', left , ' / ', right, ')'``````EPrinterVisitor >> visitAddition: anAddition

	| left right |
	left := anAddition left accept: self.
	right := anAddition right accept: self.
	^ '(', left , ' + ', right, ')'``````EPrinterVisitor >> visitMutiplication: aMultiplication

	| left right |
	left := aMultiplication left accept: self.
	right := aMultiplication right accept: self.
	^ '(', left , ' * ', right, ')'``````EPrinterVisitor >> visitMultiplication: aMultiplication

	| left right |
	left := aMultiplication left accept: self.
	right := aMultiplication right accept: self.
	^ '(', left , ' * ', right, ')'``````EPrinterVisitor >> visitMutiplication: aMultiplication

	| left right |
	left := aMultiplication left accept: self.
	right := aMultiplication right accept: self.
	^ '(', left , ' * ', right, ')'``````EPrinterVisitor >> visitConstant: aConstant

	^ aConstant value asString``````EPrinterVisitor >> visitConstant: aConstant

	^ aConstant value asString``````Object subclass: #EPrinterVisitor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````Object subclass: #EPrinterVisitor
	instanceVariableNames: 'bindings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EEvaluatorVisitor >> visitMultiplication: anEMultiplication 

	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEMultiplication left accept: self.
	evaluationOfRight := anEMultiplication right accept: self.
	^ evaluationOfLeft * evaluationOfRight``````EEvaluatorVisitor >> visitMultiplication: anEMultiplication 

	| evaluationOfLeft evaluationOfRight |
		1/0.

	evaluationOfLeft := anEMultiplication left accept: self.
	evaluationOfRight := anEMultiplication right accept: self.
	^ evaluationOfLeft * evaluationOfRight``````EEvaluatorVisitorTest >> testVisitDivisionReturnsDivisionResult

	| expression result |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 3).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 2``````EEvaluatorVisitorTest >> testVisitDivisionReturnsDivisionResult

	| expression result |
	1/0.
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 3).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 2``````EMultiplication >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EAddition >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````ENegation >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EDivision >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EConstant >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EVariable >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EExpression >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EEvaluatorVisitor >> bindings: aDictionary

	bindings := aDictionary``````EVariable >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````ENegation >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EMultiplication >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EDivision >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EConstant >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EAddition >> evaluateWith: anObject

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anObject.
	^ self accept: visitor.``````EAddition >> evaluateWith: anObject
	
	^ self accept: EEvaluatorVisitor new``````EMultiplication >> evaluateWith: anObject
	
	^ self accept: EEvaluatorVisitor new``````ENegation >> evaluateWith: anObject
	
	^ self accept: EEvaluatorVisitor new``````EDivision >> evaluateWith: anObject
	
	^ self accept: EEvaluatorVisitor new``````EEvaluatorVisitorTest >> testVisitVariableReturnsVariableValue
	| expression result visitor |
	expression := EVariable id: #answerToTheQuestion.

	visitor := EEvaluatorVisitor new.
	visitor at: #answerToTheQuestion put: 42.
	
	result := expression accept: visitor.
	self assert: result equals: 42``````EVariable >> id
	^ id``````EEvaluatorVisitor >> at: anId put: aValue

	bindings at: anId put: aValue``````EEvaluatorVisitorTest >> testVisitVariableReturnsVariableValue
	| expression result visitor |
	expression := (EVariable id: #answerToTheQuestion) negated.

	visitor := EEvaluatorVisitor new.
	visitor at: #answerToTheQuestion put: 42.
	
	result := expression accept: visitor.
	self assert: result equals: 42``````EEvaluatorVisitorTest >> testVisitVariableReturnsVariableValue
	| expression result visitor |
	expression := (EVariable id: #answerToTheQuestion) negated.
	visitor := EEvaluatorVisitor new.
	result := expression accept: visitor.
	self assert: result equals: 42``````EVariable class class >> id: anId

	^ self new
		id: anId;
		yourself``````EVariable class class >> id: anId

	^ self new
		id: anId;
		yourself``````EEvaluatorVisitorTest >> testVisitVariableReturnsVariableValue

	| expression result |
	expression := (EVariable id: #answerToTheQuestion) negated.

	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 42``````EEvaluatorVisitor >> visitVariable: aVariable

	^ bindings at: aVariable id``````EEvaluatorVisitor >> initialize

	super initialize.
	bindings := Dictionary new``````Object subclass: #EEvaluatorVisitor
	instanceVariableNames: 'bindings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EVariable >> accept: aVisitor

	^ aVisitor visitVariable: self``````EEvaluatorVisitorTest >> testVisitDivisionByZeroThrowsException

	| expression  |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 0).
	
	self
		should: [expression accept: EEvaluatorVisitor new]
		raise: EZeroDenominator``````EEvaluatorVisitorTest >> testVisitDivisionByZeroThrowsException

	| expression result |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 0).
	
	self
		should: [expression accept: EEvaluatorVisitor new]
		raise: EZeroDenominator``````EDivision class class >> numerator: aNumeratorExpression denominator: aDenominatorExpression

	^ self new
		numerator: aNumeratorExpression;
		denominator: aDenominatorExpression;
		yourself``````EDivision class class >> numerator: aNumeratorExpression denominator: aDenominatorExpression

	^ self new
		numerator: aNumeratorExpression;
		denominator: aDenominatorExpression;
		yourself``````EEvaluatorVisitorTest >> testVisitDivisionReturnsDivisionResult

	| expression result |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 3).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 2``````EEvaluatorVisitor >> visitDivision: aDivision 

	| denom numerator |
	denom := aDivision denominator accept: self.
	denom isZero
		ifTrue: [ EZeroDenominator signal ].
	numerator := aDivision numerator accept: self. 
	^ numerator / denom``````EDivision >> accept: aVisitor

	^ aVisitor visitDivision: self``````EDivision >> denominator
	^ denominator``````EDivision >> numerator
	^ numerator``````EEvaluatorVisitor >> visitMultiplication: anEMultiplication 

	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEMultiplication left accept: self.
	evaluationOfRight := anEMultiplication right accept: self.
	^ evaluationOfLeft * evaluationOfRight``````EEvaluatorVisitor >> visitMultiplication: anEMultiplication 
	self shouldBeImplemented.``````EMultiplication >> accept: aVisitor
	
	^ aVisitor visitMultiplication: self``````EMultiplication >> accept: anEEvaluatorVisitor 
	self shouldBeImplemented.``````EEvaluatorVisitorTest >> testVisitMultiplicationReturnsMultiplicationResult

	| expression result |
	expression := EMultiplication
		left: (EConstant value: 7)
		right: (EConstant value: -2).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: -14``````ENegation >> evaluateWith: anObject
	^ (expression evaluateWith: anObject) negated``````ENegation >> evaluateWith: anObject
	
	^ self accept: EEvaluatorVisitor new``````EEvaluatorVisitor >> visitNegation: anENegation 

	| aNumber |
	aNumber := anENegation expression accept: self.
	^ aNumber negated``````ENegation >> expression
	
	^ expression``````EEvaluatorVisitor >> visitNegation: anENegation 
	self shouldBeImplemented.``````ENegation >> accept: aVisitor
	
	^ aVisitor visitNegation: self``````ENegation >> accept: anEEvaluatorVisitor 
	self shouldBeImplemented.``````EEvaluatorVisitorTest >> testVisitNegationReturnsNegatedContant

	| expression result |
	expression := (EConstant value: 7) negated.
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: -7``````EAddition >> evaluateWith: anObject
	^ (right evaluateWith: anObject) + (left evaluateWith: anObject)``````EAddition >> evaluateWith: anObject
	
	^ self accept: EEvaluatorVisitor new``````EAddition >> accept: aVisitor

	^ aVisitor visitAddition: self``````EEvaluatorVisitor >> visitAddition: anEAddition
	
	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEAddition left accept: self.
	evaluationOfRight := anEAddition right accept: self.
	^ evaluationOfLeft + evaluationOfRight``````EEvaluatorVisitor >> visitAddition: anEAddition
	
	| evaluationOfLeft evaluationOfRight |
	evaluationOfLeft := anEAddition left accept: self.
	evaluationOfRight := anEAddition left accept: self.
	^ evaluationOfLeft + evaluationOfRight``````EBinaryExpression >> right
	^ right``````EBinaryExpression >> left
	^ left``````EEvaluatorVisitor >> visitAddition: anEAddition 
	self shouldBeImplemented.``````EEvaluatorVisitorTest >> testVisitAdditionReturnsAdditionResult

	| expression result |
	expression := EAddition
		left: (EConstant value: 7)
		right: (EConstant value: -2).
	
	result := expression accept: EEvaluatorVisitor new.
	
	self assert: result equals: 5``````EAddition >> accept: aVisitor

	aVisitor visitAddition: self``````EConstant >> evaluateWith: anObject
	
	^ self accept: EEvaluatorVisitor new``````EEvaluatorVisitorTest >> testVisitConstantReturnsConstantValue

	| constant result |
	constant := EConstant value: 5.
	
	result := constant accept: EEvaluatorVisitor new.
	
	self assert: result equals: 5``````EEvaluatorVisitorTest >> testVisitConstantReturnsConstantValue

	| constant result |
	constant := EConstant value: 5.
	
	result := constant accept: EEvaluatorVisitor new.
	
	self assert: result equals: 5``````TestCase subclass: #EEvaluatorVisitorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'``````EEvaluatorVisitor >> visitConstant: aConstant

	^ aConstant value``````EEvaluatorVisitor >> visitConstant: aConstant

	^ aConstant value``````Object subclass: #EEvaluatorVisitor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EConstant >> accept: aVisitor

	^ aVisitor visitConstant: self``````EExpression >> accept: aVisitor

	self subclassResponsibility``````EExpression >> accept: aVisitor

	self subclassResponsibility``````EVariable >> printOn: aStream 
	aStream nexPutAll: id asString``````EVariable >> id: aSymbol

	id := aSymbol``````EVariable >> evaluateWith: aBindingSet

	^ (aBindingSet at: id)``````ENegation >> printOn: aStream

	aStream nextPut: $-.
	aStream nextPut: Character space. 
	expression printOn: aStream``````ENegation >> negated

	^ expression``````ENegation >> expression: anEConstant 
	expression := anEConstant``````ENegation >> evaluateWith: anObject
	^ (expression evaluateWith: anObject) negated``````ENegation class class >> expression: anInteger
	^ self new expression: anInteger``````EExpression >> negated

	^ ENegation new expression: self``````EDivision >> printOn: aStream
	
	aStream 
		nextPutAll: '( ';
		nextPutAll: numerator printString; 
		nextPutAll: ' / ';
		nextPutAll: denominator printString;
		nextPutAll: ' )'``````EDivision >> numerator: anInteger 
	numerator := anInteger``````EDivision >> evaluateWith: anObject
	| denom |
	denom := denominator evaluateWith: Dictionary new.
	denom isZero
		ifTrue: [ EZeroDenominator signal ].
	^ (numerator evaluateWith: Dictionary new) / denom``````EDivision >> denominator: anInteger 
	denominator := anInteger``````EConstant >> value: anInteger

	value := anInteger``````EConstant >> value 

	^ value``````EConstant >> printOn: aStream

	aStream nextPutAll: value printString``````EConstant >> evaluateWith: anObject
	
	^ value``````EConstant class class >> value: anInteger
	
	^ self new value: anInteger ``````EConstant class class >> constant5
	<sampleInstance>
	^ self new value: 5 ``````EMultiplication >> operatorString
	^ ' * '``````EMultiplication >> evaluateWith: anObject
	^ (left evaluateWith: anObject) * (right evaluateWith: anObject)``````EMultiplication class class >> left: anInteger right: anInteger2

	^ self new left: anInteger ; right: anInteger2``````EMultiplication class class >> fiveTimesThree
	<sampleInstance>
	| ep1 ep2 |
	ep1 := (EConstant new value: 5).
	ep2 := (EConstant new value: 3).
   ^ EMultiplication new left: ep1 ; right: ep2 ``````EBinaryExpression >> right: anEConstant
	right := anEConstant
	``````EBinaryExpression >> printOn: aStream
	aStream nextPutAll: '( '.
	left printOn: aStream.
	aStream nextPutAll: self operatorString.
	right printOn: aStream.
	aStream nextPutAll: ' )'``````EBinaryExpression >> operatorString
	^ ' + '``````EBinaryExpression >> left: anEConstant
	left := anEConstant
	
	``````EAddition >> evaluateWith: anObject
	^ (right evaluateWith: anObject) + (left evaluateWith: anObject)``````EAddition class class >> left: anInteger right: anInteger2

	^ self new left: anInteger ; right: anInteger2 ``````EAddition class class >> fivePlusThree
	<sampleInstance>
	| ep1 ep2 |
	ep1 := (EConstant new value: 5).
	ep2 := (EConstant new value: 3).
	^ self new left: ep1 ; right: ep2 ``````EVariableTest >> testValueOfxInNegation

	self assert: ((EVariable new id: #x) negated evaluateWith: {#x -> 10} asDictionary) equals: -10. ``````EVariableTest >> testValueOfx

	self assert: ((EVariable new id: #x) evaluateWith: {#x -> 10} asDictionary) equals: 10. ``````EVariableTest >> testEvaluateXplusY

	| ep1 ep2 add |
	ep1 := (EVariable new id: #x).
	ep2 := (EVariable new id: #y).
	add := (EAddition left: ep1 right: ep2).
	
	self assert: (add evaluateWith: { #x -> 10 . #y -> 2 } asDictionary) equals: 12.
	self assert: (add evaluateWith: { #x -> 10 . #y -> 12 } asDictionary) equals: 22``````ENegationTest >> testPrinting
	
	| exp |
	exp := (EConstant new value: 6) negated.
	self assert: exp printString = '- 6'. 
	self assert: exp negated printString = '6'. 

	
	``````ENegationTest >> testNegationNegated
	self assert: ((EConstant new value: 6) negated negated evaluateWith: Dictionary new) equals: 6``````ENegationTest >> testNegatedStructureIsCorrect
	
	| exp |
	exp := (EConstant new value: 6).
	self assert: exp negated class = ENegation. 
	self assert: exp negated negated equals: exp.

	
	``````ENegationTest >> testNegated
	self assert: ((EConstant new value: 6) negated evaluateWith: Dictionary new) equals: -6.
	self assert: (EAddition fivePlusThree negated evaluateWith: Dictionary new) equals: -8``````ENegationTest >> testEvaluateWithClassCreationMessage
	self assert: ((ENegation expression: (EConstant value: 5)) evaluateWith: Dictionary new) equals: -5``````ENegationTest >> testEvaluate
	self assert: ((ENegation new expression: (EConstant new value: 5)) evaluateWith: Dictionary new) equals: -5``````EMultiplicationTest >> testPrinting

	self assert: (EMultiplication fiveTimesThree) negated printString equals:  '- ( 5 * 3 )'``````EMultiplicationTest >> testEvaluateWithClassCreationMessage
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self assert: ((EMultiplication left: ep1 right: ep2) evaluateWith: Dictionary new) equals: 15``````EMultiplicationTest >> testEvaluateNegatedSimple
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self
		assert:
			((EMultiplication new
				right: ep1;
				left: ep2) negated evaluateWith: Dictionary new)
		equals: -15``````EMultiplicationTest >> testEvaluateNegated
	self assert: (EMultiplication fiveTimesThree negated evaluateWith: Dictionary new) equals: -15``````EMultiplicationTest >> testEvaluate
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self
		assert:
			((EMultiplication new
				right: ep1;
				left: ep2) evaluateWith: Dictionary new)
		equals: 15``````EDivisionTest >> testEvaluateWithError
	self
		should: [ 
			(EDivision new
				numerator: (EConstant value: 4);
				denominator: (EConstant value: 0)) evaluateWith: Dictionary new ]
		raise: EZeroDenominator``````EDivisionTest >> testEvaluateNoError
	self
		assert:
			((EDivision new
				numerator: (EConstant value: 4);
				denominator: (EConstant value: 2)) evaluateWith: Dictionary new)
		equals: 2``````EConstantTest >> testValueFromClass
	
	self assert:(EConstant value: 5) value equals: 5.

	``````EConstantTest >> testValue 
	
	self assert:(EConstant new value: 5) value equals:5.
	self assert:(EConstant constant5) value equals:5
	``````EConstantTest >> testPrinting
	
	self assert:(EConstant new value: 5) printString equals: '5'.
	``````EConstantTest >> testNegated
	self assert: ((EConstant new value: 6) negated evaluateWith: Dictionary new) equals: -6``````EConstantTest >> testEvaluate
	self assert: ((EConstant new value: 5) evaluateWith: Dictionary new) equals: 5``````EAdditionTest >> testPrinting
	self assert: (EAddition fivePlusThree) printString equals:  '( 5 + 3 )'.
	self assert: (EAddition fivePlusThree) negated printString equals:  '- ( 5 + 3 )'``````EAdditionTest >> testNegated
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self
		assert:
			((EAddition new
				right: ep1;
				left: ep2) negated evaluateWith: Dictionary new)
		equals: -8``````EAdditionTest >> testEvaluateWithNegation
	| ep1 ep2 |
	ep1 := ENegation new expression: (EConstant new value: 5).
	ep2 := EConstant new value: 3.
	self
		assert:
			((EAddition new
				right: ep1;
				left: ep2) evaluateWith: Dictionary new)
		equals: -2``````EAdditionTest >> testEvaluateWithClassCreationMessage
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self assert: ((EAddition left: ep1 right: ep2) evaluateWith: Dictionary new) equals: 8``````EAdditionTest >> testEvaluate
	| ep1 ep2 |
	ep1 := EConstant new value: 5.
	ep2 := EConstant new value: 3.
	self
		assert:
			((EAddition new
				right: ep1;
				left: ep2) evaluateWith: Dictionary new)
		equals: 8``````Error subclass: #EZeroDenominator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EExpression subclass: #EVariable
	instanceVariableNames: 'id'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EExpression subclass: #ENegation
	instanceVariableNames: 'expression'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EExpression subclass: #EDivision
	instanceVariableNames: 'numerator denominator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EExpression subclass: #EConstant
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EBinaryExpression subclass: #EMultiplication
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EBinaryExpression subclass: #EAddition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````EExpression subclass: #EBinaryExpression
	instanceVariableNames: 'left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````Object subclass: #EExpression
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Model'``````TestCase subclass: #EVariableTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'``````TestCase subclass: #ENegationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'``````TestCase subclass: #EMultiplicationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'``````TestCase subclass: #EDivisionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'``````TestCase subclass: #EConstantTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'``````TestCase subclass: #EAdditionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Expressions-Test'```