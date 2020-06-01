var tab_account = null;
var tab_speeddial = null;
var tab_bookmarks = null;
var tab_backups = null;

// get sections
var login_section = null;
var manual_speed = null;
var manual_bookmarks = null;
var restore_section = null;

var backup_frame_src = null; // Task #2000

//account open
function account_open() {
	tab_account.className += ' tab_pressed';
	login_section.className = 'section_open';

	tab_speeddial.className = 'tab tab_blue tab_speeddial';
	tab_bookmarks.className = 'tab tab_green tab_bookmarks';
	tab_backups.className = 'tab tab_yellow tab_backups';

	manual_speed.className = '';
	manual_bookmarks.className = '';
	restore_section.className = '';

	fvdSynchronizer.Options.loadAccountFrame(); // Task #2000
}

//speed dial open
function speeddial_open() {
	tab_speeddial.className += ' tab_pressed';
	manual_speed.className = 'section_open';

	tab_account.className = 'tab tab_violet tab_account';
	tab_bookmarks.className = 'tab tab_green tab_bookmarks';
	tab_backups.className = 'tab tab_yellow tab_backups';

	login_section.className = '';
	manual_bookmarks.className = '';
	restore_section.className = '';
}

//bookmarks open
function bookmarks_open() {
	tab_bookmarks.className += ' tab_pressed';
	manual_bookmarks.className = 'section_open';

	tab_account.className = 'tab tab_violet tab_account';
	tab_speeddial.className = 'tab tab_blue tab_speeddial';
	tab_backups.className = 'tab tab_yellow tab_backups';

	login_section.className = '';
	manual_speed.className = '';
	restore_section.className = '';
}


var backupsPrevOpened = 'backups_server';

//backups open
function backups_open(sub) { // Task #2009 // Task #2000

	if(sub == 'backups_prev') sub = backupsPrevOpened;

	tab_backups.className += ' tab_pressed';
	restore_section.className = 'section_open';

	tab_account.className = 'tab tab_violet tab_account';
	tab_speeddial.className = 'tab tab_blue tab_speeddial';
	tab_bookmarks.className = 'tab tab_green tab_bookmarks';

	login_section.className = '';
	manual_speed.className = '';
	manual_bookmarks.className = '';

	var restoreSection = document.getElementById('restore_section');

	var subsectionServer = document.getElementById('subsectionBackupsServer');
	var subsectionLocal = document.getElementById('subsectionBackupsLocal');
	var subsectionCustoms = document.getElementById('subsectionBackupsCustoms');

	var customsLink = document.getElementById('customsLink');
	var localBackupsLink = document.getElementById('localBackupsLink');
	var serverBackupsLink = document.getElementById('serverBackupsLink');

	restoreSection.setAttribute( "sub", sub );

	subsectionServer.setAttribute( "hidden", 1 );
	subsectionLocal.setAttribute( "hidden", 1 );
	subsectionCustoms.setAttribute( "hidden", 1 );
	customsLink.removeAttribute( "active" );
	serverBackupsLink.removeAttribute( "active" );
	localBackupsLink.removeAttribute( "active" );

	if(sub == 'backups_customs'){
		subsectionCustoms.removeAttribute( "hidden" );
		customsLink.setAttribute( "active", 1 );
        fvdSynchronizer.Customs.ui.getGlobalState();
	}else if(sub == 'backups_local'){
		subsectionLocal.removeAttribute( "hidden" );
		localBackupsLink.setAttribute( "active", 1 );
		backupsPrevOpened = sub;
	}else{
		subsectionServer.removeAttribute( "hidden" );
		serverBackupsLink.setAttribute( "active", 1 );
		backupsPrevOpened = sub;
		setBackupFrameSrc();
	}

}

function setBackupFrameSrc() {
	var srcAuth = 'https://everhelper.me/client/stats?addon=chrome_sync';
	var srcBackups = 'https://everhelper.me/client/settings_inc?tab=backups&no_close=true&no_tab_navigation=true&no_pro_button=true&no_dialog=true';

	var iframe = document.getElementById('backupFrame');

	fvdSynchronizer.Server.Sync.getAuthState( function( error, authorized ){
		var src = authorized ? srcBackups : srcAuth;

		if(src != backup_frame_src){
			iframe.setAttribute('src', src);
			backup_frame_src = src;
		}
	});
}

document.addEventListener( "DOMContentLoaded", function(){
	// get tabs buttons
	tab_account = document.getElementById('tabHeadAccount');
	tab_speeddial = document.getElementById('tabHeadSync');
	tab_bookmarks = document.getElementById('tabHeadSyncBookmarks');
	tab_backups = document.getElementById('tabHeadBackupHistory');
	
	tab_backups_server = document.getElementById('serverBackupsLink');// Task #2000
	tab_backups_local = document.getElementById('localBackupsLink');// Task #2000

	tab_customs = document.getElementById('customsLink');// Task #2009

	tab_account.addEventListener( "click", function(){
		fvdSynchronizer.Options.setTab( "account" );
	}, false );
	tab_speeddial.addEventListener( "click", function(){
		fvdSynchronizer.Options.setTab( "speeddial" );
	}, false );
	tab_bookmarks.addEventListener( "click", function(){
		fvdSynchronizer.Options.setTab( "bookmarks" );
	}, false );
	tab_backups.addEventListener( "click", function(){
		fvdSynchronizer.Options.setTab( "backups" );
	}, false );
	tab_backups_server.addEventListener( "click", function(){// Task #2000
		fvdSynchronizer.Options.setTab( "backups_server" );
	}, false );
	tab_backups_local.addEventListener( "click", function(){// Task #2000
		fvdSynchronizer.Options.setTab( "backups_local" );
	}, false );

	if(tab_customs) tab_customs.addEventListener( "click", function(){// Task #2009
		fvdSynchronizer.Options.setTab( "backups_customs" );
	}, false );

	// get sections
	login_section = document.getElementById('login_section');
	manual_speed = document.getElementById('manual_speed');
	manual_bookmarks = document.getElementById('manual_bookmarks');
	restore_section = document.getElementById('restore_section');

  var howToUseBtn = document.querySelector("#topButtons .how-to-use-button");
  howToUseBtn.addEventListener("click", function() {
    var url = "https://everhelper.desk.com/customer/en/portal/topics/526842-eversync/articles";
    if(fvdSynchronizer.Utils.browserLocaleIs("ru")) {
      url = "https://everhelper.desk.com/customer/en/portal/topics/873013-eversync---%D0%92%D0%BE%D0%BF%D1%80%D0%BE%D1%81%D1%8B-%D0%B8-%D0%9E%D1%82%D0%B2%D0%B5%D1%82%D1%8B/articles";
    }
    window.open(url);
  }, false);
}, false );
