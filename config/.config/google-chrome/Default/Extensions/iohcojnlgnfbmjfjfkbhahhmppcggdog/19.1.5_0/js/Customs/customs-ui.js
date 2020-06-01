var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var classCustomsUI = (function () {
    function classCustomsUI() {
        this.DEV = true;
        this.ONCE = [];
        this.UI = {};
        this.actions = fvdSynchronizer.Customs.actions;
        this.state = {};
        this.pending = [];
        this.locale = {
            restore: _('customs_list_restore'),
            remove: _('customs_list_remove'),
            edit: _('customs_list_edit'),
            will_message: _("customs_will_backup_message"),
            will_list: _("customs_will_backup_list")
        };
        var ts = this;
        console.info('Customs UI constructor');
        $(function () {
            ts.state.DOMLoaded = true;
            ts.init();
        });
    }
    ;
    classCustomsUI.prototype.init = function () {
        var ts = this;
        ts.once('init');
    };
    ;
    classCustomsUI.prototype.once = function (action, callback) {
        var ts = this;
        if (ts.ONCE.indexOf(action) != -1)
            return;
        else
            ts.ONCE.push(action);
        switch (action) {
            case "init":
                console.info('Customs UI', 'init');
                ts.state.initiated = true;
                ts.getUI();
                ts.listeners();
                ts.getGlobalState();
                break;
            default:
                if (callback)
                    callback();
        }
    };
    ;
    classCustomsUI.prototype.getUI = function () {
        var ts = this;
        var $WRAP = $("#customs-block");
        ts.UI = {
            $wrap: $WRAP,
            tabs: {
                $backup_main: $('#tabHeadBackupHistory'),
                $customs: $('#customsLink'),
                $backup: $('#prevBackupsLink')
            },
            btns: {
                $create: $WRAP.find('#customs-create-btn'),
                $restore: $WRAP.find('#customs-restore-btn'),
                $login: $WRAP.find('#customs-login-btn'),
                $premium: $WRAP.find('#customs-premium-btn')
            },
            progress: {
                $wrap: $WRAP.find('#customs-informer'),
                $info: $WRAP.find('#customs-informer .customs-informer-progress'),
                $reload: $WRAP.find('#customs-informer #customs-reload-btn'),
                $seconds: $WRAP.find('#customs-informer #customs-reload-btn t')
            },
            $list: $WRAP.find('#customs-list')
        };
    };
    ;
    classCustomsUI.prototype.listeners = function () {
        var ts = this;
        ts.UI.btns.$create.unbind("click").on("click", function (e) {
            ts.createConfirmation();
        });
        ts.UI.btns.$restore.unbind("click").on("click", function (e) {
        });
        ts.UI.$list.on("click", ".customs-list-icons i", function (event) {
            ts.actionsButton($(event.currentTarget));
        });
        ts.UI.$list.on("click", ".customs-list-button .button", function (event) {
            ts.actionsButton($(event.currentTarget));
        });
        ts.UI.progress.$reload.unbind("click").on("click", function (e) {
            ts.UI.progress.$reload.attr("disabled", "disabled");
            ts.actions.reloadAllPages();
        });
        ts.UI.btns.$login.unbind("click").on("click", function (e) {
            $('#tabHeadAccount').trigger('click');
        });
        ts.UI.btns.$premium.unbind("click").on("click", function (e) {
            console.info('Premium btn');
            chrome.tabs.create({ url: 'https://everhelper.me/everhelperplansru.php' });
        });
    };
    ;
    classCustomsUI.prototype.tabsPrm = function () {
        var ts = this;
        var prm = true;
        if (fvdSynchronizer.Prefs.get("last_settings_tab_index") != 'backups_customs')
            prm = false;
        return prm;
    };
    ;
    classCustomsUI.prototype.getGlobalState = function () {
        var ts = this;
        if (!ts.state.DOMLoaded)
            return;
        if (!ts.tabsPrm())
            return;
        var prevAuth = ts.state.auth;
        fvdSynchronizer.Utils.Async.chain([
            function (chainCallback) {
                fvdSynchronizer.Driver.Speeddial.isAllowed(function (allowed) {
                    if (!allowed) {
                        ts.state.enabled = false;
                        $('#serverBackupsLink').trigger('click');
                    }
                    else {
                        ts.state.enabled = true;
                    }
                    chainCallback();
                });
            },
            function (chainCallback) {
                if (!ts.state.enabled)
                    chainCallback();
                else {
                    fvdSynchronizer.Server.Sync.activityState(function (aState) {
                        if (aState !== 'logged') {
                            ts.state.auth = false;
                        }
                        else {
                            ts.state.auth = true;
                        }
                        chainCallback();
                    });
                }
            },
            function (chainCallback) {
                if (!ts.state.enabled || !ts.state.auth)
                    chainCallback();
                else {
                    fvdSynchronizer.Server.Sync.userInfo(function (err, info) {
                        if (!err && info.premium.active) {
                            ts.state.pro = true;
                        }
                        else {
                            ts.state.pro = false;
                        }
                        chainCallback();
                    });
                }
            },
            function () {
                ts.draw(["enabled", "auth"]);
                if (true || prevAuth != ts.state.auth) {
                    ts.draw(["customs-list"]);
                }
                else {
                    ts.draw("loading");
                }
            }
        ]);
    };
    ;
    classCustomsUI.prototype.draw = function (actions, mode) {
        return __awaiter(this, void 0, void 0, function () {
            var ts, _loop_1, _i, actions_1, action;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        ts = this;
                        if (typeof actions != "object")
                            actions = [actions || false];
                        if (typeof mode != "object")
                            mode = { mode: mode || false };
                        _loop_1 = function (action) {
                            var _a, list, _i, _c, item, values, $li, $name, $icons, $button, max_1, $last_1;
                            return __generator(this, function (_d) {
                                switch (_d.label) {
                                    case 0:
                                        _a = action;
                                        switch (_a) {
                                            case "customs-list": return [3, 1];
                                            case "highlight": return [3, 4];
                                            case "empty": return [3, 5];
                                            case "loading": return [3, 6];
                                            case "progress": return [3, 7];
                                            case "auth": return [3, 8];
                                            case "enabled": return [3, 9];
                                            case "error": return [3, 10];
                                            case "tips": return [3, 11];
                                        }
                                        return [3, 12];
                                    case 1:
                                        list = [];
                                        if (!ts.state.auth) return [3, 3];
                                        ts.draw('loading', true);
                                        return [4, ts.actions.getList()];
                                    case 2:
                                        _d.sent();
                                        for (_i = 0, _c = ts.actions.list.items; _i < _c.length; _i++) {
                                            item = _c[_i];
                                            values = JSON.parse(item.value);
                                            $li = $('<li>')
                                                .addClass('customs-list-item')
                                                .attr('uuidHead', item.uuid)
                                                .attr('uuidData', values.uuidData)
                                                .attr('date', values.date)
                                                .css('order', -1 * parseInt(String(values.date).substr(2, 8)) || 0);
                                            $name = $('<span>')
                                                .addClass('customs-list-name')
                                                .text(values.name);
                                            $icons = $('<span>').addClass('customs-list-icons')
                                                .append($('<i>')
                                                .attr('action', 'edit')
                                                .addClass("cicon cicon-edit")
                                                .attr('title', ts.locale.edit))
                                                .append($('<i>')
                                                .attr('action', 'remove')
                                                .addClass("cicon cicon-remove")
                                                .attr('title', ts.locale.remove));
                                            $button = $('<span>').addClass('customs-list-button')
                                                .append($('<span>')
                                                .attr('action', 'restore')
                                                .addClass("button btn-mini btn-success")
                                                .append($('<span>')
                                                .text(ts.locale.restore)));
                                            $li.append($icons);
                                            $li.append($name);
                                            $li.append($button);
                                            list.push($li);
                                        }
                                        _d.label = 3;
                                    case 3:
                                        ts.UI.$list.html('').append(list);
                                        ts.draw(['empty', 'loading'], false);
                                        return [3, 12];
                                    case 4:
                                        max_1 = 0;
                                        ts.UI.$list.find('li').each(function (N, el) {
                                            max_1 = Math.max(parseInt($(el).attr('date')) || 0, max_1);
                                        });
                                        $last_1 = ts.UI.$list.find("li[date=" + max_1 + "]");
                                        setTimeout(function () {
                                            $last_1.addClass('transition').addClass('highlight');
                                        }, 10);
                                        setTimeout(function () {
                                            $last_1.removeClass('highlight');
                                            setTimeout(function () {
                                                $last_1.removeClass('transition');
                                            }, 1e3);
                                        }, 15e2);
                                        return [3, 12];
                                    case 5:
                                        if (!ts.UI.$list.find('li').length) {
                                            ts.UI.$list.append($('<li>')
                                                .addClass('customs-list-empty')
                                                .text(_('customs_empty_message')));
                                        }
                                        return [3, 12];
                                    case 6:
                                        if (mode.mode) {
                                            ts.UI.$wrap.addClass('loading');
                                            if (mode.pending)
                                                ts.pending = mode.pending;
                                        }
                                        else {
                                            ts.UI.$wrap.removeClass('loading');
                                            if (ts.pending) {
                                                ts.draw(ts.pending);
                                                ts.pending = [];
                                            }
                                        }
                                        return [3, 12];
                                    case 7:
                                        ts.UI.progress.$wrap.removeClass('hide');
                                        ts.UI.progress.$info.text(Math.min(100, Math.ceil(100 * parseInt(mode.info.ready) / parseInt(mode.info.need))) + '%');
                                        if (mode.countdown !== false) {
                                            ts.UI.progress.$reload.removeClass('hide');
                                            ts.UI.progress.$seconds.text(mode.countdown);
                                        }
                                        return [3, 12];
                                    case 8:
                                        ts.UI.$wrap.attr('mode', 'auth');
                                        return [3, 12];
                                    case 9:
                                        ts.UI.tabs.$customs.attr('disabled', ts.state.enabled ? false : 'disabled');
                                        if (ts.UI.tabs.$customs.attr('active') == '1'
                                            &&
                                                String(fvdSynchronizer.Prefs.get("last_settings_tab_index")).indexOf('backups') === 0) {
                                            ts.UI.tabs.$backup.trigger("click");
                                        }
                                        return [3, 12];
                                    case 10:
                                        console.warn(mode.error || 'Error');
                                        ts.draw('loading');
                                        return [3, 12];
                                    case 11:
                                        if (!_b(fvdSynchronizer.Prefs.get('prefs.backup.tips_dials_shown'))) {
                                            fvdSynchronizer.Dialogs.alert(_("customs_tips_backup_title"), _("customs_tips_backup_dials_message"));
                                            fvdSynchronizer.Prefs.set('prefs.backup.tips_dials_shown', true);
                                        }
                                        return [3, 12];
                                    case 12: return [2];
                                }
                            });
                        };
                        _i = 0, actions_1 = actions;
                        _a.label = 1;
                    case 1:
                        if (!(_i < actions_1.length)) return [3, 4];
                        action = actions_1[_i];
                        return [5, _loop_1(action)];
                    case 2:
                        _a.sent();
                        _a.label = 3;
                    case 3:
                        _i++;
                        return [3, 1];
                    case 4: return [2];
                }
            });
        });
    };
    ;
    classCustomsUI.prototype.actionsButton = function ($action) {
        var ts = this;
        if (!ts.premission())
            return;
        var action = $action.attr('action');
        var $item = $action.parents('.customs-list-item');
        var uuidHead = $item.attr('uuidHead');
        var uuidData = $item.attr('uuidData');
        var $name = $item.find('.customs-list-name');
        var name = $name.text();
        if (!action || !uuidHead || !uuidData)
            return;
        if (action == 'edit') {
            ts.dialogName(name, _("customs_settings_rename_title"), _("customs_settings_rename"), function (value) {
                if (value) {
                    $name.text(value);
                    ts.actions.rename(uuidHead, value);
                }
            });
        }
        else if (action == 'remove') {
            fvdSynchronizer.Dialogs.confirm(_("customs_remove_title"), $('<span>').addClass('dialog-message-center-big').text(name)[0].outerHTML, function (r) {
                if (r) {
                    $item.remove();
                    ts.draw('empty');
                    ts.actions.remove(uuidHead);
                }
            });
        }
        else if (action == 'restore') {
            fvdSynchronizer.Dialogs.confirm(_("customs_restore_title"), $('<span>').addClass('dialog-message-center-big').text(name)[0].outerHTML, function (r) {
                if (r) {
                    ts.draw('loading', true);
                    ts.actions.restore(uuidHead);
                }
            });
        }
    };
    ;
    classCustomsUI.prototype.createConfirmation = function () {
        var ts = this;
        if (!ts.premission())
            return;
        var name = ts.nameGenerator();
        ts.dialogName(name, _("customs_settings_create_title"), _("customs_settings_create"), function (value) {
            ts.draw('loading', { mode: true, pending: ['tips'] });
            ts.actions.create(value);
        });
    };
    ;
    classCustomsUI.prototype.premission = function () {
        var ts = this;
        var prm = true;
        if (!ts.state.auth || !ts.state.pro) {
            prm = false;
            var btns = [];
            btns[_("dlg_confirm_cancel")] = function () {
                dlg_1.close();
            };
            btns[_("customs_pro_popup_button")] = function () {
                dlg_1.close();
                chrome.tabs.create({ url: 'https://everhelper.me/everhelperplansru.php' });
            };
            var dlg_1 = new Dialog({
                width: 400,
                title: _('customs_pro_popup_title'),
                content: _('customs_pro_popup_message'),
                buttons: btns
            });
        }
        return prm;
    };
    ;
    classCustomsUI.prototype.dialogName = function (value, title, button, cb) {
        var ts = this;
        var $input = $("<input>")
            .attr('id', 'customizationName')
            .addClass("input-modal")
            .attr('placeholder', _("customs_settings_create_placeholder"))
            .attr('maxlength', 100);
        var $html = $("<div>")
            .append($("<div>")
            .addClass("common-modal-body-inputs")
            .append($input))
            .append($("<div>")
            .addClass("common-modal-body-list")
            .append($('<p>')
            .append($("<h5>").text(ts.locale.will_message))
            .append($("<span>").text(ts.locale.will_list))));
        fvdSynchronizer.Dialogs.confirm(title, $html[0].outerHTML, function (r) {
            if (r) {
                ts.dialogNameConfirm(cb);
            }
        });
        $('#customizationName').val(value);
    };
    ;
    classCustomsUI.prototype.dialogNameConfirm = function (cb) {
        var $text = $('#customizationName');
        if (cb)
            cb($text.val());
    };
    ;
    classCustomsUI.prototype.nameGenerator = function () {
        var name = 'Backup ';
        name += String(browserName());
        name += ', ' + (new Date()).toLocaleString();
        return name;
    };
    ;
    return classCustomsUI;
}());
function browserName() {
    return 'Chrome';
}
fvdSynchronizer.Customs.ui = new classCustomsUI();
//# sourceMappingURL=customs-ui.js.map