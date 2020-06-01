BookmarksGuidsStorage = new function(){

	var self = this;

	const KEY_NAME = "guid_storage";
	const TEMP_KEY_NAME = "guid_storage_temp";

	var activeKeyName = KEY_NAME;

	var id_GlobalId = {};
	var globalId_id = {};

	var needWrite = false; // flag to need write database to persistent storage

	setInterval( function(){

		if( needWrite ){
			store();
		}

	}, 1000 );

	function _setNeedWrite(){
		needWrite = true;
	}

	function isMassChange(){
		return activeKeyName == TEMP_KEY_NAME;
	}

	// write to persistent storage
	function store(){
		localStorage[ activeKeyName ] = JSON.stringify( id_GlobalId );
		needWrite = false;
	}

	// read from persistent storage
	function read() {
		id_GlobalId = {};
		globalId_id = {};
		if(localStorage[TEMP_KEY_NAME]) {
			return self.applyMassGuidsChange();
		}
		try{
			id_GlobalId = JSON.parse( localStorage[ KEY_NAME ] );
			makeGlobalId_id();
		}
		catch( ex ){
		}
	}

	// create associative object [global_id] => [id]
	function makeGlobalId_id() {
		globalId_id = {};
		for(var id in id_GlobalId) {
			globalId_id[id_GlobalId[id]] = id;
		}
	}

	// start mass write session
	this.startMassGuidsChange = function() {
		activeKeyName = TEMP_KEY_NAME;
	};

	// move from temp storage to main storage
	this.applyMassGuidsChange = function() {
		if(needWrite) {
			store();
		}
		if(!localStorage[TEMP_KEY_NAME]) {
			return;
		}
		activeKeyName = KEY_NAME;
		id_GlobalId = JSON.parse( localStorage[ TEMP_KEY_NAME ] );
		delete localStorage[ TEMP_KEY_NAME ];
		makeGlobalId_id();
		this.store();
	};

	// cancel mass change without saveing changes
	this.cancelMassGuidsChange = function() {
		needWrite = false;
		activeKeyName = KEY_NAME;
		read();
	};

	// mass store [id] => [global_id] association, overwrites old association
	this.setMass = function(data) {
		id_GlobalId = data;
		makeGlobalId_id();
		_setNeedWrite();
	};

	// interface for store bookmarks
	this.store = function() {
		store();
	};

	this.clear = function() {
		id_GlobalId = {};
		globalId_id = {};
	};

	this.getAllGuids = function(){
		var tmp = Object.keys(globalId_id);
		// need to remove unsorted from array
		var unsortedIndex = tmp.indexOf( "unsorted" );
		if(unsortedIndex != -1) {
			tmp.splice(unsortedIndex, 1);
		}

		return tmp;
	};

	this.getAllIds = function() {
		return Object.keys(id_GlobalId);
	};

	this.getGuid = function(id) {
		if(typeof id_GlobalId[id] == "undefined") {
			return null;
		}
		return id_GlobalId[id];
	};

	this.getId = function(guid) {
		if( typeof globalId_id[guid] == "undefined" ){
			return null;
		}
		return globalId_id[guid];
	};

	this.setGuid = function(id, guid) {
		globalId_id[guid] = id;
		id_GlobalId[id] = guid;
		_setNeedWrite();
	};

	this.removeGuid = function(guid) {
		if(typeof globalId_id[guid] != "undefined") {
			var id = globalId_id[guid];
			delete globalId_id[guid];
			delete id_GlobalId[id];
			_setNeedWrite();
		}
	};

	this.removeGuidById = function(id) {
		if(typeof id_GlobalId[id] != "undefined") {
			var guid = id_GlobalId[id];
			delete id_GlobalId[id];
			delete globalId_id[guid];
			_setNeedWrite();
		}
	};

	this.getGuidsCount = function(){
		return Object.keys(id_GlobalId).length;
	};
	// initialization
	read();
};
