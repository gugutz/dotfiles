class classCustomsUI {
    DEV = true;
    ONCE: any = [];
    UI: any = {};

    actions: any = fvdSynchronizer.Customs.actions;

    state:any = {};
    pending:any = [];
    
    locale: any = {
        restore: _('customs_list_restore'),
        remove: _('customs_list_remove'),
        edit: _('customs_list_edit'),
        will_message: _("customs_will_backup_message"),
        will_list: _("customs_will_backup_list"),
    };

    constructor() {
        let ts = this;

        console.info('Customs UI constructor');

        $(()=>{
            ts.state.DOMLoaded = true;
            ts.init();
		});
    };
    
    init(){
        let ts = this;
        ts.once('init');
    };

    once(action:any, callback?:any) {
        let ts = this;

        if (ts.ONCE.indexOf(action) != -1) return;
        else ts.ONCE.push(action);

        switch (action) {
            case "init":
                console.info('Customs UI', 'init');

                ts.state.initiated = true;

                ts.getUI();
                ts.listeners();
                ts.getGlobalState();
            break;

            default:
                if (callback) callback();
        }
    };

    getUI():void{
        let ts = this;

        let $WRAP = $("#customs-block");

        ts.UI = {
            $wrap: $WRAP,
            tabs: {
                $backup_main: $('#tabHeadBackupHistory'),
                $customs : $('#customsLink'),
                $backup : $('#prevBackupsLink'),
            },
            btns: {
                $create: $WRAP.find('#customs-create-btn'),
                $restore: $WRAP.find('#customs-restore-btn'),
                $login: $WRAP.find('#customs-login-btn'),
                $premium: $WRAP.find('#customs-premium-btn'),
            },
            progress: {
                $wrap: $WRAP.find('#customs-informer'),
                $info: $WRAP.find('#customs-informer .customs-informer-progress'),
                $reload: $WRAP.find('#customs-informer #customs-reload-btn'),
                $seconds: $WRAP.find('#customs-informer #customs-reload-btn t'),
            },
            $list: $WRAP.find('#customs-list'),
        };
    };

    listeners():void {
        let ts = this;

        ts.UI.btns.$create.unbind("click").on("click", (e) => {
            ts.createConfirmation();
        });

        ts.UI.btns.$restore.unbind("click").on("click", (e) => {
            //ts.restore();
        });

        ts.UI.$list.on("click", ".customs-list-icons i", (event)=>{
            ts.actionsButton($(event.currentTarget));
        });

        ts.UI.$list.on("click", ".customs-list-button .button", (event)=>{
            ts.actionsButton($(event.currentTarget));
        });
        
        ts.UI.progress.$reload.unbind("click").on("click", (e) => {
            ts.UI.progress.$reload.attr("disabled","disabled");
            ts.actions.reloadAllPages();
        });

        ts.UI.btns.$login.unbind("click").on("click", (e) => {
            $('#tabHeadAccount').trigger('click');
            //AUTH.tab("login");
        });

        ts.UI.btns.$premium.unbind("click").on("click", (e) => {
            console.info('Premium btn');
            //$('#tabHeadAccount').trigger('click');
            chrome.tabs.create({url:'https://everhelper.me/everhelperplansru.php'});
        });
    };

    tabsPrm() {
        let ts = this;
        let prm = true;

        if(fvdSynchronizer.Prefs.get("last_settings_tab_index") != 'backups_customs') prm = false;

        /*
        if(!ts.UI.tabs.$backup_main.hasClass('tab_pressed')) prm = false;
        else
        if(parseInt(ts.UI.tabs.$customs.attr('active')) !== 1) prm = false;
        */

        return prm;
    };

    getGlobalState(){
        let ts = this;
        if(!ts.state.DOMLoaded) return;
        if(!ts.tabsPrm()) return;

        let prevAuth = ts.state.auth;

        //ts.draw("loading", true);

        fvdSynchronizer.Utils.Async.chain([
            function( chainCallback ){
                fvdSynchronizer.Driver.Speeddial.isAllowed(function(allowed){
                    if(!allowed){
                        ts.state.enabled = false;
                        $('#serverBackupsLink').trigger('click');
                    }else{
                        ts.state.enabled = true;
                    }

                    chainCallback();
                });
            },
            function( chainCallback ){
                if(!ts.state.enabled) chainCallback();
                else{
                    fvdSynchronizer.Server.Sync.activityState( function( aState ){
                        if(aState !== 'logged'){
                            ts.state.auth = false;
                        }else{
                            ts.state.auth = true;
                        }

                        chainCallback();
                    });
                }
            },
            function( chainCallback ){
                if(!ts.state.enabled || !ts.state.auth) chainCallback();
                else{
                    fvdSynchronizer.Server.Sync.userInfo(function(err, info) {
                        if(!err && info.premium.active){
                            ts.state.pro = true;
                        }else{
                            ts.state.pro = false;
                        }

                        chainCallback();
                    });
                }
            },
            function(){
                ts.draw(["enabled", "auth"]);
                
                if(true || prevAuth != ts.state.auth){ // #2088
                    ts.draw(["customs-list"]);
                }else{
                    ts.draw("loading");
                }
            }
        ]);
    };

    async draw(actions:any, mode?:any){
        let ts = this;
        
        if(typeof actions != "object") actions = [actions || false];
        if(typeof mode != "object") mode = {mode: mode || false};
        
        for(let action of actions){            
            switch(action){
                case "customs-list": // #2088           
                    let list = [];

                    if(ts.state.auth){
                        ts.draw('loading', true);
                        await ts.actions.getList();

                        for(let item of ts.actions.list.items){
                            let values = JSON.parse(item.value);

                            let $li = $('<li>')
                                .addClass('customs-list-item')
                                .attr('uuidHead', item.uuid)
                                .attr('uuidData', values.uuidData)
                                .attr('date', values.date)
                                .css('order', -1 * parseInt(String(values.date).substr(2, 8)) || 0)
                            ;

                            let $name = $('<span>')
                                .addClass('customs-list-name')
                                .text(values.name);

                            let $icons = $('<span>').addClass('customs-list-icons')
                                .append(
                                    $('<i>')
                                        .attr('action', 'edit')
                                        .addClass("cicon cicon-edit")
                                        .attr('title', ts.locale.edit)
                                )
                                .append(
                                    $('<i>')
                                        .attr('action', 'remove')
                                        .addClass("cicon cicon-remove")
                                        .attr('title', ts.locale.remove)
                                )
                            ;

                            let $button = $('<span>').addClass('customs-list-button')
                                .append(
                                    $('<span>')
                                        .attr('action', 'restore')
                                        .addClass("button btn-mini btn-success")
                                        .append(
                                            $('<span>')
                                                .text(ts.locale.restore)
                                        )
                                )
                            ;

                            $li.append($icons);
                            $li.append($name);
                            $li.append($button);

                            list.push($li);
                        }
                    }

                    ts.UI.$list.html('').append(list);

                    ts.draw(['empty', 'loading'], false);
                break;

                case "highlight":
                    let max = 0;
                    ts.UI.$list.find('li').each((N, el)=>{
                        max = Math.max(parseInt($(el).attr('date')) || 0,  max);
                    });
                    
                    let $last = ts.UI.$list.find(`li[date=${max}]`);

                    setTimeout(()=>{
                        $last.addClass('transition').addClass('highlight');
                    }, 10);

                    setTimeout(()=>{
                        $last.removeClass('highlight');

                        setTimeout(()=>{
                            $last.removeClass('transition');
                        },1e3);
                    },15e2);
                break;

                case "empty":
                    if(!ts.UI.$list.find('li').length){
                        ts.UI.$list.append(
                            $('<li>')
                                .addClass('customs-list-empty')
                                .text(_('customs_empty_message'))
                        )
                    }
                break;

                case "loading":
                    if(mode.mode){
                        ts.UI.$wrap.addClass('loading');
                        if(mode.pending) ts.pending = mode.pending;
                    }else{
                        ts.UI.$wrap.removeClass('loading');
                        if(ts.pending){
                            ts.draw(ts.pending);
                            ts.pending = [];
                        }
                    }
                break;

                case "progress":
                    ts.UI.progress.$wrap.removeClass('hide');

                    ts.UI.progress.$info.text(Math.min(100, Math.ceil( 100 * parseInt(mode.info.ready) / parseInt(mode.info.need))) + '%'); // Task #2083

                    if(mode.countdown !== false){
                        ts.UI.progress.$reload.removeClass('hide');
                        ts.UI.progress.$seconds.text(mode.countdown);
                    }
                break;

                case "auth":
                    ts.UI.$wrap.attr('mode', 'auth'); // #2088
                    //ts.UI.$wrap.attr('mode', ts.state.auth && ts.state.pro ? 'auth' : 'not-auth'); // #2088
                break;

                case "enabled":
                    ts.UI.tabs.$customs.attr('disabled', ts.state.enabled ? false : 'disabled');

                    if(
                        ts.UI.tabs.$customs.attr('active') == '1'
                        &&
                        String(fvdSynchronizer.Prefs.get( "last_settings_tab_index" )).indexOf('backups') === 0
                    ){
                        ts.UI.tabs.$backup.trigger( "click" );
                    }
                break;

                case "error":
                    console.warn(mode.error || 'Error');
                    ts.draw('loading');
                break;

                case "tips":
                    if(!_b(fvdSynchronizer.Prefs.get('prefs.backup.tips_dials_shown'))){
                        fvdSynchronizer.Dialogs.alert(_("customs_tips_backup_title"), _("customs_tips_backup_dials_message"));
                        fvdSynchronizer.Prefs.set('prefs.backup.tips_dials_shown', true);
                    }
                break;
            }
        }
    };

    actionsButton($action): void{
        let ts = this;
        if(!ts.premission()) return; // #2088

        let action = $action.attr('action');
        let $item = $action.parents('.customs-list-item');
        let uuidHead = $item.attr('uuidHead');
        let uuidData = $item.attr('uuidData');
        let $name = $item.find('.customs-list-name');
        let name = $name.text();
        
        if(!action || !uuidHead || !uuidData) return;

        if(action == 'edit'){
            ts.dialogName(
                name,
                _("customs_settings_rename_title"), 
                _("customs_settings_rename"), 
                function(value:any){
                    if(value){
                        $name.text(value);
                        ts.actions.rename(uuidHead, value);
                    }
                }
            );
        }else
        if(action == 'remove'){
            fvdSynchronizer.Dialogs.confirm(
                _("customs_remove_title"), 
                $('<span>').addClass('dialog-message-center-big').text(name)[0].outerHTML,
                function(r){
                    if (r) {
                        $item.remove();
                        ts.draw('empty');
                        ts.actions.remove(uuidHead);
                    }
                }
            );
        }else
        if(action == 'restore'){
            fvdSynchronizer.Dialogs.confirm(
                _("customs_restore_title"), 
                $('<span>').addClass('dialog-message-center-big').text(name)[0].outerHTML,
                function(r){
                    if (r) {
                        ts.draw('loading', true);
                        ts.actions.restore(uuidHead);
                    }
                }
            );
        }
    };

    createConfirmation(): void{
        let ts = this;
        if(!ts.premission()) return; // #2088

        let name = ts.nameGenerator();

        ts.dialogName(
            name,
            _("customs_settings_create_title"), 
            _("customs_settings_create"), 
            function(value:any){
                ts.draw('loading', {mode:true, pending:['tips']});
                ts.actions.create(value);
            }
        );
    };

    premission(): boolean{// #2088
        let ts = this;
        let prm = true;
        
        if(!ts.state.auth || !ts.state.pro){
            prm = false;

            let btns = [];
            btns[_("dlg_confirm_cancel")] = function(){
                dlg.close();
            };
            btns[_("customs_pro_popup_button")] = function(){
                dlg.close();
                chrome.tabs.create({url:'https://everhelper.me/everhelperplansru.php'});
            };
            let dlg = new Dialog({
                width: 400,
                //enterOnButton: _("customs_pro_popup_button"),
                title: _('customs_pro_popup_title'),
                content: _('customs_pro_popup_message'),
                buttons: btns
              });
        }

        return prm;
    };

    dialogName(value, title, button, cb): void{
        let ts = this;

        var $input = $("<input>")
            .attr('id', 'customizationName')
            .addClass("input-modal")
            .attr('placeholder', _("customs_settings_create_placeholder"))
            .attr('maxlength', 100)
        ;

        var $html = $("<div>")
            .append(
                $("<div>")
                .addClass("common-modal-body-inputs")
                .append(
                    $input
                )
            )
            .append(
                $("<div>")
                .addClass("common-modal-body-list")
                .append(
                    $('<p>')
                        .append(
                            $("<h5>").text(ts.locale.will_message),
                        )
                        .append(
                            $("<span>").text(ts.locale.will_list)
                        )
                )
            )
            /*
            .append(
                $("<div>")
                .addClass("common-modal-body-buttons")
                .append(
                    $("<button>")
                        .attr("data-dismiss", "modal")
                        .addClass("btn btn-success options-common-popup-btn options-common-popup-hide relative min-w96")
                        .text(button)
                        .on("click", function(){
                            if(cb) cb($input.val());
                        })
                )
            )
            */
        ;

        fvdSynchronizer.Dialogs.confirm(
            title, 
            $html[0].outerHTML,
            function(r){
                if (r) {
                    ts.dialogNameConfirm(cb);
                }
            }
        );

        $('#customizationName').val(value);
    };

    dialogNameConfirm(cb):any{
        let $text = $('#customizationName');
        if(cb) cb($text.val());
    };

    nameGenerator():any{
        let name = 'Backup ';
        name += String(browserName());//.capitalizeFirstLetter();
        name += ', ' + (new Date()).toLocaleString();
        return name;
    };
}

function browserName(){
    return 'Chrome';
}

// ########################## CALL ########################## //

fvdSynchronizer.Customs.ui = new classCustomsUI();