## Stone Paper Scissors
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'StonePaperScissors'
	self assert: (Stone new play: Paper new) equals: #paper
	self assert: (Scissors new play: Paper new) equals: #scissors
	self assert: (Stone new play: Stone new) equals: #draw
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'StonePaperScissors'
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'StonePaperScissors'
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'StonePaperScissors'
	self assert: (Stone new play: Paper new) = #paper
	... Your code ...
	... Your code ...
	... Your code ...
	... Your code ...
	self assert: (Stone new play: Scissors new) equals: #stone
	self assert: (Stone new play: Stone new) equals: #draw
	self assert: (Scissors new play: Paper new) equals: #scissors
	... Your code ...
	... Your code ...
	... Your code ...
	... Your code ...
	self assert: (Scissors new play: Stone new) equals: #stone
	self assert: (Scissors new play: Scissors new) equals: #draw
	... Your code ...
	... Your code ...
	... Your code ...
	... Your code ...
	self assert: (Paper new play: Scissor new) equals: #scissors
	self assert: (Paper new play: Stone new) equals: #paper
	self assert: (Paper new play: Paper new) equals: #draw
	ifTrue: [ do something for aGameElement]
	(aGameElement play: anotherGameElement) = -1 
	onDraw: [ Game incrementDraw ]
	onReceiverWin: [ ]
	onReceiverLose: [ ]
	onDraw: [ ]
	onReceiverWin: [ Game incrementPaper ]
	onReceiverLose: [ ]
	onDraw: aDrawBlock 
	onWin: aWinBlock 
	onLose: aLoseBlock
	
	^ anElement
		playAgainstPaper: self
		onDraw: aDrawBlock
		onReceiverWin: aWinBlock
		onReceiverLose: aLoseBlock
	onDraw: aDrawBlock onReceiverWin: 
	aWinBlock 
	onReceiverLose: aLoseBlock
	^ aDrawBlock value
	result: aResultHolder
	^ anElement playAgainstPaper: self result: aResultHolder
	aResultHolder paperWins