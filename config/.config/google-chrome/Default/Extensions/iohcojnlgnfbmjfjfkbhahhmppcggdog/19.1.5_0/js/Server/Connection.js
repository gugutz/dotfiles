if (window == chrome.extension.getBackgroundPage()) {

	(function(){
		var BLOCKED_COOKIES_EN = "https://everhelper.desk.com/customer/en/portal/articles/2963404-why-do-i-get-an-%22incorrect-username-or-password%22-error-in-google-chrome-";
		var BLOCKED_COOKIES_RU = "https://everhelper.desk.com/customer/portal/articles/2963403-%D0%9F%D0%BE%D1%87%D0%B5%D0%BC%D1%83-%D0%B2%D0%BE%D0%B7%D0%BD%D0%B8%D0%BA%D0%B0%D0%B5%D1%82-%D0%BE%D1%88%D0%B8%D0%B1%D0%BA%D0%B0-%22%D0%9D%D0%B5%D0%BF%D1%80%D0%B0%D0%B2%D0%B8%D0%BB%D1%8C%D0%BD%D1%8B%D0%B9-%D0%BB%D0%BE%D0%B3%D0%B8%D0%BD-%D0%B8%D0%BB%D0%B8-%D0%BF%D0%B0%D1%80%D0%BE%D0%BB%D1%8C%22-%D0%B2-google-chrome-?b_id=11251";

		var Errors = fvdSynchronizer.Errors;

		function SyncRequest(aData, aCallback, form){

			aData = aData || {};

			function encodeRequest(requestData){
				return JSON.stringify(requestData);
			}

			function decodeResponse(responseData){
				return JSON.parse(responseData);
			}

			aData._client_software = "chrome_addon";

			var data = encodeRequest(aData);

			var req = new XMLHttpRequest();
			var url = "https://sync-eh.everhelper.me";
						
			if( form && aData.action ){
				url += "/" + aData.action;
			}

			req.open('POST', url, true);

			req.setRequestHeader( "EverHelper-Token", fvdSynchronizer.Server.Connection.getCurrentToken() );
			req.setRequestHeader("X-Client-Version", chrome.runtime.getManifest().version);
			req.withCredentials = true;	// Task #1985
			
			req.onload = function() {
				
				try {
					var response = decodeResponse(req.responseText);
				}
				catch (ex) {
					console.log("Malformed response: " + req.responseText + "(" + ex + ")\n");
					aCallback(Errors.ERROR_SERVER_RESPONSE_MALFORMED);
					return;
				}

				if ( response && response.errorCode ) {
					if ( response.errorCode == fvdSynchronizer.Errors.ERROR_AUTH_FAILED &&
						aData.action != "user_exists" && aData.action != "user:logout" ) {

						fvdSynchronizer.Server.Sync.activityState( function( state ){

							if ( state == "logged" ) {

								fvdSynchronizer.Server.Sync.setActivityState( "not_logged" );

								if (fvdSynchronizer.Server.Sync.isNoHostPremission()) { // Need auth
									fvdSynchronizer.Dialogs.alertWindow(
										_("dlg_alert_wrong_login_password_title"),
										_("dlg_alert_wrong_login_password_text"),
										{
										single: true
									});
								} else { // Blocked thrid party cookies
									console.warn('Check third party cookies')
									/*
									var link = (_("lang") == 'ru') ? BLOCKED_COOKIES_RU : BLOCKED_COOKIES_EN;

									fvdSynchronizer.Dialogs.alertWindow(
										_("blocked_cookies_title"),
										_("blocked_cookies_text"),
										{
										single: true,
										btnText: _("blocked_cookies_button"),
										btnLink: link,
									});
									*/
								}
							}
						});
					}
				}

				aCallback(response.errorCode, response.body);
			};

			req.onerror = function() {
				console.log("Request error\n", arguments);

				aCallback(Errors.ERROR_CONNECTION_ERROR);
			};

			if(form) {
				req.send(form);
			}
			else{
				req.send(data);
			}
		}

		var Connection = function() {

			var currentToken = "";

			this.setCurrentToken = function( token ){
				currentToken = token;
			};

			this.getCurrentToken = function(){
				return currentToken;
			};

			this.request = function(data, callback, form){
				new SyncRequest(data, callback, form);
			};

			this.simpleRequest = function( url, method, data, callback ){

				var req = new XMLHttpRequest();
				req.open( method, url );

				req.onload = function(){
					callback( JSON.parse( req.responseText ) );
				};

				req.onerror = function(){
					callback( null );
				};

				req.send( data );
			};

			this.get = function( url, callback ){
				return this.simpleRequest( url, "GET", null, callback );
			};

			this.post = function( url, dataString, callback ){
				return this.simpleRequest( url, "POST", dataString, callback );
			};


		};

		this.Connection = new Connection();

	}).apply(fvdSynchronizer.Server);

}
else{
	fvdSynchronizer.Server.Connection = chrome.extension.getBackgroundPage().fvdSynchronizer.Server.Connection;
}
