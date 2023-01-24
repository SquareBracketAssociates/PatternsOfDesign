## Understanding Visitors
  ^ aVisitor visitConstant: self
  ^ aConstant value
constant := EConstant value: 5.
constant accept: EEvaluatorVisitor new. 

	self subclassResponsibility

	^ aVisitor visitConstant: self
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Expressions-Model'

	^ aConstant value
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Expressions-Test'

	| constant result |
	constant := EConstant value: 5.
	result := constant accept: EEvaluatorVisitor new.
	self assert: result equals: 5
	
	^ self accept: EEvaluatorVisitor new

	... Your code ...

	| expression result |
	expression := EAddition
		left: (EConstant value: 7)
		right: (EConstant value: -2).
	result := expression accept: EEvaluatorVisitor new.	
	self assert: result equals: 5
	^ left
	^ right
	... Your code ...
	^ self accept: EEvaluatorVisitor new

	| expression result |
	expression := (EConstant value: 7) negated.
	result := expression accept: EEvaluatorVisitor new.
	self assert: result equals: -7
	... Your code ...
	^ expression
	... Your code ...
	^ self accept: EEvaluatorVisitor new

	| expression result |
	expression := EMultiplication
		left: (EConstant value: 7)
		right: (EConstant value: -2).	
	result := expression accept: EEvaluatorVisitor new.
	self assert: result equals: -14
	
	... Your code ...

	... Your report ...

	| expression result |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 3).
	result := expression accept: EEvaluatorVisitor new.
	self assert: result equals: 2

	| expression result |
	expression := EDivision
		numerator: (EConstant value: 6)
		denominator: (EConstant value: 0).	
	self
		should: [expression accept: EEvaluatorVisitor new]
		raise: EZeroDenominator

	| expression  |
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
	^ numerator
	^ denominator

	... Your code ...
	... Your code ...
	

	^ self accept: EEvaluatorVisitor new
	| expression result visitor |
	expression := EVariable id: #answerToTheQuestion.

	visitor := EEvaluatorVisitor new.
	visitor at: #answerToTheQuestion put: 42.
	
	result := expression accept: visitor.
	self assert: result equals: 42
	instanceVariableNames: 'bindings'
	classVariableNames: ''
	package: 'Expressions-Model'

	super initialize.
	bindings := Dictionary new
	bindings at: anId put: aValue

	^ self new id: anId; yourself
	... Your code ...

	... Your code ...

	| visitor |
	visitor := EEvaluatorVisitor new.
	visitor bindings: anEnvironment.
	^ self accept: visitor.

	bindings := aDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Expressions-Model'
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Expressions-Model'
	^ aConstant value asString

	| left right |
	left := aMultiplication left accept: self.
	right := aMultiplication right accept: self.
	^ '(', left , ' * ', right, ')'
	... Your code ...
	... Your code ...
	... Your code ...
	... Your code ...