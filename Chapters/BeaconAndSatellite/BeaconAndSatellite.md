## Beacons and Satellites 
	instanceVariableNames: 'observers'
	classVariableNames: ''
	package: 'SatelitteAndBeacon'
	observers := OrderedCollection new
	instanceVariableNames: 'data'
	classVariableNames: ''
	package: 'SatelitteAndBeacon'
	self addObserver: aBeacon
	position := aPoint. 
	self notify
	observers do: [ :aBeacon | aBeacon satelittePositionChanged: self ]
	(aSat position isBetween: self start and: self end)
	self emitData: self data to: aSat
	aSatelitte1 addObserver: aBeacon1 with: aEventClass
	observerDict at: anEventClass iAbsentPut: [OrderedCollection new].
	(observerDict at: anEventClass) add: anObserver
	position := aPoint. 
	self notify: (PositionChanged with: self)
	(observersDict at: anEvent class) ifPresent: [ :aBeaconList | 
		aBeaconList do: [:aBeacon| anEvent fireOn: aBeacon ]
	instanceVariableNames: 'observable'
	classVariableNames: ''
	package: 'SatelitteAndBeacon'
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'SatelitteAndBeacon'
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'SatelitteAndBeacon'
	anObserver salelittePositionChanged: observable
	anObserver salelitteProtocolChanged: observable
	observerDict at: anEventClass iAbsentPut: [OrderedCollection new].
	(observerDict at: anEventClass) add: (aSelector -> anObserver)
	position := aPoint. 
	self notify: (PositionChanged with: self)
	(observersDict at: anEvent class) ifPresent: [ :aBeaconList | 
		aBeaconList do: [ :aBeaconAssoc | 
			aBeaconAssoc value perform: aBeaconAssoc key with: anEvent) ]
	instanceVariableNames: 'selector observer'
	instanceVariableNames: 'observerDict'
	observerDict at: anEventClass iAbsentPut: [ OrderedCollection new] .
	(observerDict at: anEventClass) add: 
		(BSAnnouncement send: aSelector to: anObserver)
	(observersDict at: anEvent class) ifPresent: [ :aBeaconList | 
		aBeaconList do: [ :anAnnouncement | 
			anAnnouncement observer
				perform: anAnnouncement selector 
				with: anEvent) ]
	self announcer notify: anEvent
	self announcer when: anEventClass send: aSelector to: anObserver