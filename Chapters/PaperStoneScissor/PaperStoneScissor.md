## Stone Paper Scissors@cha_stoneAs we already saw sending a message is in fact making a choice. Indeed when we send a message, the method associated with the method in the class hierarchy of the receiver will be selected and executed. Now we often have cases where we would like to select a method based on the receiver of the message and one argument. Again there is a simple solution named double dispatch that consists in sending another message to the argument hence making two choices one after the other. This technique while simple can be challenging to grasp because programmers are so used to think that choices are made using explicit conditionals.  In this chapter we will show an example of double dispatch via the paper stone scissors game. ### Starting with a couple of testsWe start by implementing a couple of tests.Let us define a test class named `StonePaperScissorsTest`.```TestCase subclass: #StonePaperScissorsTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'StonePaperScissors'```Now we can define a couple of tests showing for example that a paper is winning when a stone plays against a paper. We consider that the following tests are self explanatory.```StonePaperScissorsTest >> testStoneAgainstPaperIsWinning
	self assert: (Stone new play: Paper new) equals: #paper``````StonePaperScissorsTest >> testScissorAgsinstPaperIsWinning
	self assert: (Scissors new play: Paper new) equals: #scissors``````StonePaperScissorsTest >> testStoneAgainsStone
	self assert: (Stone new play: Stone new) equals: #draw```Define them because we will use the tests in the future.### Creating the classesFirst let us create the classes that will correspond to the different players.```Object subclass: #Paper
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'StonePaperScissors'``````Object subclass: #Scissors
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'StonePaperScissors'``````Object subclass: #Stone
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'StonePaperScissors'```They could share a common superclass but we let it to you. ### With messagesWe are read to make sure that a first test is passing. Let us work on `testPaperIsWinning`.```StonePaperScissorsTest >> testStoneAgainstPaperIsWinning
	self assert: (Stone new play: Paper new) = #paper```The first method that we define is `play:` and it takes another player as argument. ```Stone >> play: anotherTool
	... Your code ...```To implement this method we will use the fact that we know when its body is executed what is the receiver of the message. Here we are sure that the receiver is an instance of the class `Stone`.So let us imaging that we have another method named `playAgainstStone:`In the class `Paper`, it is clear that the method should return `#paper` because a paper wins against a stone. So just define it. ```Paper >> playAgainstStone: aStone
	... Your code ...```Now using the method `playAgainstStone:`, we can easily implement the previous method `play:` in the class `Stone`.% [[[% Stone >> play: anotherTool% 	^ anotherTool playAgainstStone: self% ]]]% [[[% Paper >> playAgainstStone: aStone% 	^ #paper% ]]]Do it and the test should pass now. #### playAgainstStone:Since we have started to implement `playAgainstStone:`, let us continue and implement two other methods one in the class `Scissors` and the other in the class `Stone`.In the class `Scissors` the method should return that a stone wins.```Scissors >> playAgainstStone: aStone
	... Your code ...```In the class `Stone`, the method should return a draw.```Stone >> playAgainstStone: aStone
	... Your code ...```% [[[% Scissors >> playAgainstStone: aStone% 	^ #stone% ]]]% [[[% Stone >> playAgainstStone: aStone% 	^ #draw% ]]]Let us verify that the following tests are passing. For this we only execute the tests whose receiver of the `play:` message are stone instance. First we add a test to check new scenario and now we have all the scenarios where a stone is the receiver.```StonePaperScissorsTest >> testStoneAgainstScissorsIsWinning
	self assert: (Stone new play: Scissors new) equals: #stone``````StonePaperScissorsTest >> testStoneAgainsStone
	self assert: (Stone new play: Stone new) equals: #draw```The case where stone is the receiver of the message play is handled and we can pass to another class,for example, `Scissors`.#### Scissors nowLet us write first a test if this is already done. What we see is that a scissor is winning against a paper. ```StonePaperScissorsTest >> testScissorIsWinning
	self assert: (Scissors new play: Paper new) equals: #scissors```Now we are read to define the corresponding methods.First we define the methods `playAgainstScissors:` in the correspoding classes. ```Scissors >> playAgainstScissors: aScissors
	... Your code ...``````Paper >> playAgainstScissors: aScissors
	... Your code ...``````Stone >> playAgainstScissors: aScissors
	... Your code ...```% [[[% Scissors >> playAgainstScissors: aScissors% 	^ #draw% ]]]% [[[% Paper >> playAgainstScissors: aScissors%% 	^ #scissors% ]]]% [[[% Stone >> playAgainstScissors: aScissors% 	^ #stone% ]]]Now we are ready to we define the method `play:` in the class `Scissors`.```Scissors >> play: anotherTool
	... Your code ...```% [[[% Scissors >> play: anotherTool% 	^ anotherTool playAgainstScissors: self% ]]]You can define a couple of tests to make sure that your code is correct. ```StonePaperScissorsTest >> testScissorAgainstStoneIsLosing
	self assert: (Scissors new play: Stone new) equals: #stone``````StonePaperScissorsTest >> testScissorAgainstScissors
	self assert: (Scissors new play: Scissors new) equals: #draw```#### Paper nowWe are now ready to do the same with the case of `Paper`.You should start to see the pattern. Define the method `playAgainstPaper:` in their corresponding classes.```Scissors >> playAgainstPaper: aPaper
	... Your code ...``````Paper >> playAgainstPaper: aPaper
	... Your code ...``````Stone >> playAgainstPaper: aPaper
	... Your code ...```And now we can define the method `play:` in the `Paper` class.```Paper >> play: anotherTool
	... Your code ...```% [[[% Scissors >> playAgainstPaper: aPaper%% 	^ #scissors% ]]]% [[[% Paper >> playAgainstPaper: aPaper% 	^ #draw% ]]]% [[[% Stone >> playAgainstPaper: aPaper% 	^ #paper% ]]]% [[[% Paper >> play: anotherTool% 	^ anotherTool playAgainstPaper: self% ]]]Let us add more tests to cover the new cases.```StonePaperScissorsTest >> testPaperAgainstScissorIsLosing
	self assert: (Paper new play: Scissor new) equals: #scissors``````StonePaperScissorsTest >> testPaperAgainstStoneIsWinning
	self assert: (Paper new play: Stone new) equals: #paper``````StonePaperScissorsTest >> testPaperAgainstPaper
	self assert: (Paper new play: Paper new) equals: #draw```![An overview of a possible solution using double dispatch.](figures/StonePaperScissors.pdf width=80)The methods could return a value such as 1 when the receiver wins, 0 when there is draw and -1 when the receiver loses.  Add new tests and check this version. ### About double dispatchThis exercise about double dispatch is really simple and it has two aspects that you may not find in other situations:First it is symetrical. You play a stone against a paper or the inverse. Not all the double dispatch are symetrical. For example, when drawing an object against a canvas the operation for example `drawOn: aCanva` is directed. It does not change much about the double dispatch but we wanted to make clear that it does not have to be this way. Second the secondary methods \(`playAgainstXXX`\) do not use the argument and this is because the example is super simple. In real life example, the secondary methods do use the argument for example to call back behavior on the argument. We will see this with the visitor design pattern.### A Better APIBoth previous approaches either returning a symbol or a number are working but we can ask ourselves how the client will use this code. Most of the time he will have to check again the returned result to perform some actions.```(aGameElement play: anotherGameElement) = 1 
	ifTrue: [ do something for aGameElement]
	(aGameElement play: anotherGameElement) = -1 ```So all in all, while this was a good exercise to help you understand that we do not need to have explicit conditionals and that we can use message passing instead, it felt a bit disappointing. But there is a much better solution using double dispatch. The idea is to pass the action to be executed to the object and that the object decide what to do. ```Paper new competeWith: Paper new
	onDraw: [ Game incrementDraw ]
	onReceiverWin: [ ]
	onReceiverLose: [ ]``````Paper new competeWith: Stone new
	onDraw: [ ]
	onReceiverWin: [ Game incrementPaper ]
	onReceiverLose: [ ]```Propose an implementation.### About alternative implementationsHere is a possible alternate implementation. ```Paper >> play: anElement 
	onDraw: aDrawBlock 
	onWin: aWinBlock 
	onLose: aLoseBlock
	
	^ anElement
		playAgainstPaper: self
		onDraw: aDrawBlock
		onReceiverWin: aWinBlock
		onReceiverLose: aLoseBlock``````Paper >> playAgainstPaper: anElement 
	onDraw: aDrawBlock onReceiverWin: 
	aWinBlock 
	onReceiverLose: aLoseBlock
	^ aDrawBlock value```What we see is that this new API is not that nice. Being forced to create blocks is not that great.A possibility would be to pass an object know what do do.```Paper new competeWith: Paper new
	result: aResultHolder```Here is a sketche of a possible implementation:```Paper >> competeWith: anElement result: aResultHolder
	^ anElement playAgainstPaper: self result: aResultHolder```We still have the double dispatch but we only need one object taking take of the results.```Stone >> playAgainstPaper: anElement result: aResultHolder
	aResultHolder paperWins```### ConclusionSending a message is making a choice amongst several methods. Depending on the receiver of a message the correct method will be selected. Therefore sending a message is making a choice and the different classes represent the possible alternatives. Now this example illustrates this point but going even further. Here we wanted to be able to make a choice depending on both an object and the argument of the message. The solution shows that it is enough to send back another message to the argument to perform a second selection that because of the first message now realizes a choice based on a message receiver and its argument. 