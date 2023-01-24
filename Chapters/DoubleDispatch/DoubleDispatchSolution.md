## Revisiting the Die DSL: a Case for Double Dispatch```Die >> sumWithDie: aDie

	^ DieHandle new
		addDie: self;
		addDie: aDie; yourself``````Die >> + aDicable	
	^ aDicable sumWithDie: self``````DieHandle >> sumWithDie: aDie
	| handle |
	handle := self class new.
	self dice do: [ :each | handle addDie: each ].
	handle addDie: aDie.
	^ handle``````DieHandle >> sumWithHandle: aDieHandle
	| handle |
	handle := self class new.
	self dice do: [ :each | handle addDie: each ].
	aDieHandle dice do: [ :each | handle addDie: each ].
	^ handle``````DieHandle >> + aDicable
	^ aDicable sumWithHandle: self``````Die >> sumWithHandle: aDieHandle
	| handle |
	handle := DieHandle new.
	aDieHandle dice do: [ :each | handle addDie: each ].
	handle addDie: self.
	^ handle```