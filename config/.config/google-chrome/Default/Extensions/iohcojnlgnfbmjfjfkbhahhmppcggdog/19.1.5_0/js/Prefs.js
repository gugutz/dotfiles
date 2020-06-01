// singletone


if (window == chrome.extension.getBackgroundPage()) {

	(function(){
		Prefs = function(){

		};

		Prefs.prototype = {
			_prefsPrefix: "prefs.",
			_changeListeners: [],

			// default values
			_defaults: {
				"activity_state": "not_logged", /* not_logged, logged */
				"sync_state": "normal",
				"login": "",
				"password": "",
				"not_confirmed_account_data": "",
				"not_confirmed_change_password": "",

				"last_settings_tab_index": "account",

				"autobackup.enabled": true,

				"speeddial.first_sync_after": "login", // none, login, register, install
				"speeddial.to_sync_data": "{}",
				"speeddial.last_dials_update": "",
				"speeddial.last_groups_update": "",
				"speeddial.enable_autosync": true,

				"bookmarks.first_sync_after": "login", // none, login, register, install
				"bookmarks.to_sync_data": "{}",
				"bookmarks.last_update": "",
				"bookmarks.need_index_global_ids": true,
				"bookmarks.last_update_time": "",
				"bookmarks.archived": false,
				"bookmarks.backup.need_set_change_on_start": false,
				"bookmarks.enable_autosync": true,
				"dont_display_ds_chromesync_message": false,
				"dont_display_sync_access_warning": false

			},

			dump: function( callback ){

				var result = {};
				for( var k in this._defaults ){
					result[k] = this.get(k);
				}

				callback(result);

			},

			toggle: function( name ){
				var newVal = !_b( this.get( name ) );
				this.set( name, newVal );
			},

			defaultValue: function( settingName ){
				if (typeof this._defaults[settingName] != "undefined") {
					return this._defaults[settingName];
				}
				else {
					return null;
				}
			},

			restore: function( settingName ){
				if (typeof this._defaults[settingName] != "undefined") {
					this.set( settingName, this._defaults[settingName] );
				}
				else {

				}
			},

			get: function(name, defaultValue){

				if (typeof defaultValue == "undefined") {
					if (typeof this._defaults[name] != "undefined") {
						defaultValue = this._defaults[name];
					}
					else {
						defaultValue = null;
					}
				}

				var name = this._name(name);
				if (typeof localStorage[name] == "undefined") {
					return defaultValue;
				}

				return localStorage[name];
			},

			set: function(name, value){

				var oldValue = this.get(name);

				var badListeners = [];

				if ( _r(oldValue) != _r(value) ) {
					localStorage[this._name(name)] = value;
					// call change listeners
					for (var i = 0; i != this._changeListeners.length; i++) {
						var listener = this._changeListeners[i];
						// try catch exception because listener exception cannot breaks running listeners chain
						try{
							listener(name, value);
						}
						catch( ex ){
							badListeners.push( listener );
						}

					}
				}

				for( var i = 0; i != badListeners.length; i++ ){
					this.removeChangeListener( badListeners[i] );
				}

			},

			addChangeListener: function(listener){
				if (this._changeListeners.indexOf(listener) != -1) {
					return;
				}
				this._changeListeners.push(listener);
			},

			removeChangeListener: function(listener){
				var index = this._changeListeners.indexOf(listener);
				if (index != -1) {
					this._changeListeners.splice(index, 1);
				}
			},

			_name: function(name){
				return this._prefsPrefix + name;
			}
		};

		this.Prefs = new Prefs();
	}).apply(fvdSynchronizer);

}
else{
	fvdSynchronizer.Prefs = chrome.extension.getBackgroundPage().fvdSynchronizer.Prefs;
}
