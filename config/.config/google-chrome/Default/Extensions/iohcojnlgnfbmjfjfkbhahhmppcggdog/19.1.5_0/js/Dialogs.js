(function(){

	var Dialogs = function(){

		var that = this;

		if( window == chrome.extension.getBackgroundPage() ){

			document.addEventListener("DOMContentLoaded", function(){

				chrome.windows.onRemoved.addListener(function(id){

					var index = that._alertWindowsIds.indexOf(id);
					if (index != -1) {
						that._alertWindowsIds.splice(index, 1);
					}

				});

			}, false);

		}

	}

	Dialogs.prototype = {

		_erroredFields: [],
		_alertWindowsIds: [], // not clean after window close
		// default dialogs

		fireOpenedAlerts: function(){

			chrome.extension.getBackgroundPage().fvdSynchronizer.Dialogs._alertWindowsIds.forEach(function(id){

				chrome.windows.update(id, {
					focused: true
				});

			});

		},

		alertWindow: function(title, text, params){

			params = params ||
			{};

			var that = this;

			fvdSynchronizer.Utils.Async.chain([function(chainCallback){

				if (params.single) {

					that.alertWindowOpened(function(opened){
						if (!opened) {
							chainCallback();
						}
					});

				}
				else {
					chainCallback();
				}

			}, function(){

				const INITIAL_WIDTH = 400;
				const INITIAL_HEIGHT = 150;

				var left = Math.round(window.screen.width / 2 - INITIAL_WIDTH / 2);
				var top = Math.round(window.screen.height / 2 - INITIAL_HEIGHT / 2);

				var link = chrome.extension.getURL("alert.html?title=" + encodeURIComponent(title) + "&text=" + encodeURIComponent(text));

				if(params.btnText) link += "&btn_text="+encodeURIComponent(params.btnText);
				if(params.btnLink) link += "&btn_link="+encodeURIComponent(params.btnLink);

				chrome.windows.create({
					url: link,
					width: INITIAL_WIDTH,
					height: INITIAL_HEIGHT,
					left: left,
					top: top,
					focused: true,
					type: "popup"
				}, function(w){

					//w.document.getElementsByTagName( "title" )[0].textContent = title;
					//w.document.getElementsByTagName( "body" )[0].textContent = text;

					chrome.extension.getBackgroundPage().fvdSynchronizer.Dialogs._alertWindowsIds.push(w.id);

				});

			}

			]);

		},

		alertWindowOpened: function(callback){

			fvdSynchronizer.Utils.Async.arrayProcess( chrome.extension.getBackgroundPage().fvdSynchronizer.Dialogs._alertWindowsIds, function(windowId, arrayProcessCallback){

				chrome.windows.get(windowId, function(w){

					if (w) {
						callback(true);
					}
					else {
						arrayProcessCallback();
					}

				});

			}, function(){

				callback(false);

			});

		},

		typedWindow: function(type, params){

			params = params ||
			{};
			params.query = params.query ||
			{};

			var that = this;

			fvdSynchronizer.Utils.Async.chain([function(chainCallback){

				if (params.single) {

					that.alertWindowOpened(function(opened){
						if (!opened) {
							chainCallback();
						}
						else{
							that.fireOpenedAlerts();
						}
					});

				}
				else {
					chainCallback();
				}

			}, function(){


				if ( type == "restartAfterPluginSync" ) {
					// some actions for restart after plugin sync window
					// in future maybe need  move this code in other part of addon?
					fvdSynchronizer.Observer.fireEvent("manualSyncSuccess");
				}
				else if( type == "syncQuotaExceed" ){
					 fvdSynchronizer.Observer.fireEvent("syncQuotaExceed");
				}
				else if ( type == "actionAfterPluginSync" ) {
					if (_b(fvdSynchronizer.Prefs.get("dont_display_after_plugin_sync_action_alert"))) {
						return;
					}
				}

				var INITIAL_WIDTH = 400;
				var INITIAL_HEIGHT = 150;

				if(type == "needSyncSoftware") {
				  INITIAL_WIDTH = 505;
				  INITIAL_HEIGHT = 170;
				  if(_b(fvdSynchronizer.Prefs.get("dont_display_alert_sync_software")) && !params.force) {
				    return;
				  }
				}

				var left = Math.round(window.screen.width / 2 - INITIAL_WIDTH / 2);
				var top = Math.round(window.screen.height / 2 - INITIAL_HEIGHT / 2);

				var additionalQuery = "";
				var _tmp = [];
				for (var k in params.query) {
					var v = params.query[k];
					_tmp.push(k + "=" + encodeURIComponent(v));
				}
				if (_tmp.length > 0) {
					additionalQuery = "&" + _tmp.join("&");
				}

				chrome.windows.create({
					url: chrome.extension.getURL("alert.html?type=" + type + additionalQuery),
					width: INITIAL_WIDTH,
					height: INITIAL_HEIGHT,
					left: left,
					top: top,
					focused: true,
					type: "popup"
				}, function(w){

					chrome.extension.getBackgroundPage().fvdSynchronizer.Dialogs._alertWindowsIds.push(w.id);

					//w.document.getElementsByTagName( "title" )[0].textContent = title;
					//w.document.getElementsByTagName( "body" )[0].textContent = text;

				});

			}

			]);


		},

		alert: function(title, text, params, callback){
			if(typeof params == "function") {
				callback = params;
				params = {};
			};

			var btns = {};
			btns[_("dlg_alert_ok")] = function(){
				dlg.close();
				if (callback) {
					callback();
				}
			};

			if(typeof params !== "object") params = {};

			params.width = 400;
			params.title = title;
			params.content = text;
			params.buttons = btns;
			var dlg = new Dialog(params);
			return dlg;
		},

		confirm: function(title, text, callback){

			var btns = {};
			btns[_("dlg_confirm_ok")] = function(){
				dlg.close();
				callback(true);
			};

			btns[_("dlg_confirm_cancel")] = function(){
				dlg.close();
				callback(false);
			};

			var dlg = new Dialog({
				width: 400,
				enterOnButton: _("dlg_confirm_ok"),
				title: title,
				content: text,
				buttons: btns
			});

		},

		alertCheck: function( title, text, cbText, cbInitState, callback, params ){

			params = params || {};
			params.width = params.width || 400;
			params.className = params.className || "";

			if( document.getElementById("dialogAlertCheck_text") ){
				return;
			}

			var btns = {};
			btns[_("dlg_alert_ok")] = function(){
				dlg.close();
				if( callback ){
					callback( document.getElementById( "dialogAlertCheck_checkbox" ).checked );
				}
			};


			var dlg = new Dialog({
				className: "alertDialog " + params.className,
				width: params.width,
				title: title,
				content: chrome.extension.getBackgroundPage().document.getElementById("prototype_dialogAlertCheck").innerHTML,
				buttons: btns,
				onShow: function(){
					document.getElementById( "dialogAlertCheck_text" ).innerHTML = text;
					document.getElementById( "dialogAlertCheck_checkBoxLabel" ).innerHTML = cbText;
					document.getElementById( "dialogAlertCheck_checkbox" ).checked = cbInitState;
				}
			});

		},


		confirmCheck: function( title, text, cbText, cbInitState, callback ){

			if( document.getElementById("dialogAlertCheck_text") ){
				return;
			}

			var btns = {};
			btns[_("dlg_confirm_ok")] = function(){
				dlg.close();
				if( callback ){
					callback( true, document.getElementById( "dialogAlertCheck_checkbox" ).checked );
				}
			};

			btns[_("dlg_confirm_cancel")] = function(){
				dlg.close();
				if( callback ){
					callback( false, document.getElementById( "dialogAlertCheck_checkbox" ).checked );
				}
			};

			var dlg = new Dialog({
				className: "alertDialog",
				width: 400,
				title: title,
				content: chrome.extension.getBackgroundPage().document.getElementById("prototype_dialogAlertCheck").innerHTML,
				buttons: btns,
				onShow: function(){
					document.getElementById( "dialogAlertCheck_text" ).innerHTML = text;
					document.getElementById( "dialogAlertCheck_checkBoxLabel" ).innerHTML = cbText;
					document.getElementById( "dialogAlertCheck_checkbox" ).checked = cbInitState;
				}
			});

		},

		syncProgressSimple: function(syncDriver, syncAction, callback, additional) {

			if( fvdSynchronizer.Driver[syncDriver].canSyncAndReaction ){
				if( !fvdSynchronizer.Driver[syncDriver].canSyncAndReaction() ){
					return;
				}
			}

			additional = additional || {};

			var btns = {};

			function __beforeUnloadListener(){
				return _("dlg_sync_in_progress_before_close");
			}

			window.addEventListener("beforeunload", __beforeUnloadListener, false);

			var syncInProgress = true;

			if (syncAction != "makeBackup" && syncAction != "restoreBackup") {

				btns[_("dlg_sync_progress_cancel")] = function(){

					if (syncInProgress) {

						fvdSynchronizer.Dialogs.confirm(_("dlg_confirm_abort_sync_title"), _("dlg_confirm_abort_sync_title"), function(r){

							if (r) {
								fvdSynchronizer.Driver[syncDriver].abortCurrentSync();
							}

						});

					}
					else {

						dlg.close();

					}

				}

			}

			function _removeListeners(){
				window.removeEventListener("beforeunload", __beforeUnloadListener);
			}

			function endCallback(error){

				_removeListeners();

				syncInProgress = false;

				var manualSyncProgressMessage = document.querySelector(".syncProgressDialog  .manualSyncInProcess");

				if (manualSyncProgressMessage) {
					manualSyncProgressMessage.setAttribute("hidden", true);
				}

				var manualSyncSuccessMessage = document.querySelector(".syncProgressDialog  .manualSyncSuccess");

				if (error) {

				}
				else
					if (manualSyncSuccessMessage.hasAttribute("display")) {
						if (manualSyncSuccessMessage) {
							manualSyncSuccessMessage.removeAttribute("hidden");
						}
					}

				var stateText = dlg.container.querySelector(".syncProgressDialog .syncProgressLine");

				if (error != 0) {
				  if(error == fvdSynchronizer.Errors.ERROR_NEED_ADDITIONAL_SOFTWARE) {
				    try{
              var actionsContainer = dlg.container.querySelector(".syncProgressDialog").parentNode.parentNode.querySelector(".dialog-actions");
              var whyElem = document.createElement("a");
              whyElem.className = "why-sync-failed";
              whyElem.textContent = _("dlg_sync_progress_why_failed");
              actionsContainer.appendChild(whyElem);
              whyElem.addEventListener("click", function() {
                fvdSynchronizer.Dialogs.typedWindow( "needSyncSoftware", {
                  force: true
                });
              }, false);
				    }
				    catch(ex) {
				      console.error(ex);
				    }
				  }
					stateText.setAttribute("state", "fail");
					stateText.textContent = _("dlg_sync_progress_sync_failed");

					setTimeout(function(){

						if (error == fvdSynchronizer.Errors.ERROR_QUOTA_EXCEED) {
							fvdSynchronizer.Dialogs.alert(_("dlg_sync_quota_exceed_title"), _("dlg_sync_quota_exceed_text"));
						}
						else
							if (error == fvdSynchronizer.Errors.ERROR_ALREADY_LOCKED) {
								fvdSynchronizer.Dialogs.alert(_("popup_alert_sync_already_active_title"), _("popup_alert_sync_already_active"));
							}

					}, 0);


					//errorAll();
				}
				else {
					stateText.setAttribute("state", "success");
					stateText.textContent = _("dlg_sync_progress_sync_success");
				}

				dlg.container.querySelector("#dlgSyncProgressBottomText").setAttribute("hidden", true);

				try {
					dlg.container.querySelector(".fvdButton").textContent = _("dlg_sync_progress_close");
					dlg.container.querySelector(".fvdButton").removeAttribute("hidden");
				}
				catch (ex) {

				}

				if (callback) {
					callback(error);
				}

			}

			var title = _("dlg_sync_progress_title");

			if (syncAction == "makeBackup") {
				title = _("dlg_sync_progress_title_backup");
			}
			else
				if (syncAction == "restoreBackup") {
					title = _("dlg_sync_progress_title_restore");
				}

			var dialogHTML = chrome.extension.getBackgroundPage().document.getElementById("prototype_dialogSyncProgressSimple").innerHTML;
			dialogHTML = dialogHTML.replace("{{locale}}", chrome.i18n.getMessage("extension_locale"), dialogHTML);
			var dlg = new Dialog({
				width: 400,
				title: title,
				content: dialogHTML,
				buttons: btns,
				onShow: function( _dlg ){

					try{
						_dlg.container.querySelector(".fvdButton").setAttribute("hidden", true);
					}
					catch( ex ){

					}


					if (syncAction == "makeBackup") {
						fvdSynchronizer.Driver[syncDriver].Backup.make(function(){

							setTimeout(function(){
								_removeListeners();
								dlg.close();
								callback(0);
							}, 1000);

						});
					}
					else
						if (syncAction == "restoreBackup") {

							_dlg.container.querySelector(".simpleProgressItems").setAttribute("appear", 1);
							var _progressCheckInterval = setInterval(function(){

								var p = fvdSynchronizer.Driver[syncDriver].Backup.getRestoreProgress();

								if( p.max == 0 ){
									return;
								}

								var titleElem = _dlg.container.querySelector(".simpleProgressItems .message");
								titleElem.textContent = _("dlg_sync_progress_type_" + p.type);

								_dlg.container.querySelector(".simpleProgressItems .value").textContent = p.current + "/" + p.max;

							}, 500);

							fvdSynchronizer.Driver[syncDriver].Backup.restore( additional.dir, function(){

								clearInterval( _progressCheckInterval );

								setTimeout(function(){
									_removeListeners();
									dlg.close();
									callback(0);
								}, 1000);

							});

						}
						else {
							fvdSynchronizer.Driver[syncDriver][syncAction](endCallback);
						}

				}
			});

      return dlg;
		},

		syncProgress: function(syncType, callback) {
			var syncInProgress = true;

			var btns = {};

			function __beforeUnloadListener(){
				return _("dlg_sync_in_progress_before_close");
			}

			window.addEventListener("beforeunload", __beforeUnloadListener, false);

			btns[_("dlg_sync_progress_cancel")] = function(){

				if (syncInProgress) {

					fvdSynchronizer.Dialogs.confirm(_("dlg_confirm_abort_sync_title"), _("dlg_confirm_abort_sync_title"), function(r){

						if (r) {
							var stateText = document.querySelector(".syncProgressDialog .syncProgressLine");
							fvdSynchronizer.Driver.Speeddial.abortCurrentSync();
							stateText.setAttribute("state", "fail");
							stateText.textContent = _("dlg_sync_progress_aborting");
						}

					});

				}
				else {

					dlg.close();

				}

			}

			function successInProgressStep(){

				var elem = document.querySelector(".syncProgressDialog div[state=progress]");
				if (elem) {
					elem.setAttribute("state", "success");
				}

			}

			function changeStateCallback(type, data){

				if (data) {

					if (typeof data.groupsCount != "undefined") {
						document.getElementById("dlgSyncProgress_syncedCountGroups").textContent = data.groupsCount;
					}
					if (typeof data.dialsCount != "undefined") {
						document.getElementById("dlgSyncProgress_syncedCountDials").textContent = data.dialsCount;
					}

				}

				successInProgressStep();

				var elem = "";

				switch (type) {
					case "syncGroups":
						elem = "dlgSyncProgress_progressStateGroups";
						break;
					case "syncDials":
						elem = "dlgSyncProgress_progressStateDials";
						break;
					case "applyChanges":
						elem = "dlgSyncProgress_progressStateApplyChanges";
						dlg.container.querySelector(".fvdButton").setAttribute("hidden", true);
						break;
				}

				document.getElementById(elem).setAttribute("state", "progress");

			}

			function errorAll(){
				var elements = document.querySelectorAll(".syncProgressDialog  .progressElements > div");
				for (var i = 0; i != elements.length; i++) {
					elements[i].setAttribute("state", "fail");
				}
			}

			function endCallback(error){

				window.removeEventListener("beforeunload", __beforeUnloadListener);

				document.getElementById("dlgSyncProgressBottomText").setAttribute("hidden", true);

				syncInProgress = false;

				successInProgressStep();

				var stateText = document.querySelector(".syncProgressDialog .syncProgressLine");

				if (error != 0) {
					stateText.setAttribute("state", "fail");
					stateText.textContent = _("dlg_sync_progress_sync_failed");
					errorAll();
				}
				else {
					stateText.setAttribute("state", "success");
					stateText.textContent = _("dlg_sync_progress_sync_success");
				}

				if (error == fvdSynchronizer.Errors.ERROR_SYNC_USER_ABORTED) {
					dlg.close();
				}

				dlg.container.querySelector(".fvdButton").removeAttribute("hidden");
				dlg.container.querySelector(".fvdButton").textContent = _("dlg_sync_progress_close");

				if (callback) {
					callback(error);
				}

				if (error == fvdSynchronizer.Errors.ERROR_ALREADY_LOCKED) {
					fvdSynchronizer.Dialogs.alert(_("popup_alert_sync_already_active_title"), _("popup_alert_sync_already_active"));
				}
			}

			var dlg = new Dialog({
				width: 400,
				title: _("dlg_sync_progress_title"),
				content: chrome.extension.getBackgroundPage().document.getElementById("prototype_dialogSyncProgress").innerHTML,
				buttons: btns,
				onShow: function(){

					switch (syncType) {

						case "sdMergeLocalAndServerData":

							fvdSynchronizer.Driver.Speeddial.mergeLocalAndServerData(endCallback, changeStateCallback);

							break;

						case "sdOverwriteServerData":

							fvdSynchronizer.Driver.Speeddial.overwriteServerData(endCallback, changeStateCallback);

							break;

						case "sdOverwriteLocalData":

							fvdSynchronizer.Driver.Speeddial.overwriteLocalData(endCallback, changeStateCallback);

							break;

					}

				}
			});

      return dlg;
		},


		errorToField: function(field, prnt, errorMessage){
			var dialogErrorBox = document.getElementById("dialogErrorBox");
			if (!dialogErrorBox) {
				dialogErrorBox = document.createElement("div");
				dialogErrorBox.className = "dialog-errorBox";
				dialogErrorBox.setAttribute("id", "dialogErrorBox");
				var span = document.createElement("div");
				dialogErrorBox.appendChild(span);
				prnt.appendChild(dialogErrorBox);
			}
			field.setAttribute("error", "1");
			this._erroredFields.push(field);

			var span = dialogErrorBox.getElementsByTagName("div")[0];

			span.textContent = errorMessage;
			var pos = fvdSynchronizer.Utils.getOffset(field);
			dialogErrorBox.style.left = pos.left + "px";
			dialogErrorBox.style.top = pos.top - 1 + field.offsetHeight + "px";
			dialogErrorBox.style.width = field.offsetWidth - 2 + "px";
			dialogErrorBox.setAttribute("active", 1);
		},

		hideErrorBox: function(){
			try {
				var dialogErrorBox = document.getElementById("dialogErrorBox");

				if (dialogErrorBox) {
					dialogErrorBox.setAttribute("active", 0);
				}
				for (var i = 0; i != this._erroredFields.length; i++) {
					this._erroredFields[i].setAttribute("error", 0);
				}
			}
			catch (ex) {

			}
		},


		addGroup: function(groupId, dialogParams){

			var btns = {};

			var buttonAddModifyText = _("dlg_button_add_group");
			if (groupId) {
				var buttonAddModifyText = _("dlg_button_modify_group");
			}

			btns[buttonAddModifyText] = function(){

				var nameElem = document.getElementById("addGroup_name");
				var name = nameElem.value.trim();

				if (!name) {
					that.errorToField(nameElem, document.body, _("error_must_be_filled"));
					return false;
				}

				if (dialogParams && dialogParams.commitToCallback) {

					if (dialogParams.existsGroupsNames) {
						var nameLC = name.toLowerCase();
						for (var i = 0; i != dialogParams.existsGroupsNames.length; i++) {
							if (dialogParams.existsGroupsNames[i].toLowerCase() == nameLC) {
								that.errorToField(nameElem, document.body, _("error_already_exists"));
								return false;
							}
						}
					}

					dialogParams.commitToCallback({
						result: true,
						data: {
							name: name
						}
					});

					dlg.close();

				}
				else {
					fvdSpeedDial.Storage.groupExists(name, function(exists){

						if (exists) {
							that.errorToField(nameElem, document.body, _("error_already_exists"));
							return false;
						}
						else {
							if (groupId) {
								fvdSpeedDial.Storage.groupUpdate(groupId, {
									name: name
								}, function(){
									fvdSpeedDial.SpeedDial.sheduleRebuildGroupsList();
									dlg.close();
								});

								fvdSpeedDial.Sync.addDataToSync("groups", groupId, {
									translate: "group"
								});
							}
							else {
								fvdSpeedDial.Storage.groupAdd(name, function(result){
									if (result.result) {
										fvdSpeedDial.Sync.addDataToSync(["groups", "newGroups"], result.id, {
											translate: "group"
										});

										// refresh speed dial
										fvdSpeedDial.SpeedDial.sheduleFullRebuild();
										dlg.close();
									}
								});
							}

						}
					}, groupId ? [groupId] : null);
				}
			}

			btns[_("dlg_button_cancel")] = function(){
				if (dialogParams && dialogParams.commitToCallback) {
					dialogParams.commitToCallback({
						result: false
					});
				}


				dlg.close();
			}

			var that = this;

			var dlg = new Dialog({
				width: 400,
				title: groupId ? this._title("modify_group") : this._title("add_group"),
				content: chrome.extension.getBackgroundPage().document.getElementById("prototype_dialogAddGroup").innerHTML,
				buttons: btns,
				clickCallback: function(){
					that.hideErrorBox();
				},
				closeCallback: function(){
					that.hideErrorBox();
				},
				onShow: function(){
					if (groupId) {

						fvdSpeedDial.Storage.getGroup(groupId, function(group){
							document.getElementById("addGroup_name").value = group.name;
						});

					}

					document.getElementById("addGroup_name").focus();
				},
				enterOnButton: buttonAddModifyText
			});

		},

		_title: function(code){
			return _("dialog_title_" + code);
		}



	};

	this.Dialogs = new Dialogs();

}).apply(fvdSynchronizer);

