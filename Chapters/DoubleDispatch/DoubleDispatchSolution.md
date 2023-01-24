## Revisiting the Die DSL: a Case for Double Dispatch

	^ DieHandle new
		addDie: self;
		addDie: aDie; yourself
	^ aDicable sumWithDie: self
	| handle |
	handle := self class new.
	self dice do: [ :each | handle addDie: each ].
	handle addDie: aDie.
	^ handle
	| handle |
	handle := self class new.
	self dice do: [ :each | handle addDie: each ].
	aDieHandle dice do: [ :each | handle addDie: each ].
	^ handle
	^ aDicable sumWithHandle: self
	| handle |
	handle := DieHandle new.
	aDieHandle dice do: [ :each | handle addDie: each ].
	handle addDie: self.
	^ handle