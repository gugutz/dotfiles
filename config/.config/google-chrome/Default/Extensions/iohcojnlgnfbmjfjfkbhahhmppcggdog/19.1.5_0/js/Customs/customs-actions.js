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
fvdSynchronizer.Customs = {};
var classCustomsActions = (function () {
    function classCustomsActions() {
        this.DEV = true;
        this.ONCE = [];
        this.API = {
            auth: "https://everhelper.me/auth/process.php?",
            server: "https://sync-eh.everhelper.me"
        };
        this.tables = {
            'fvdSynchronizer': { source: 'fvdSynchronizer', allow_empty: true },
            'fvdSpeedDial': { source: 'fvdSpeedDial', allow_empty: true },
            'misc': { source: 'misc', allow_empty: true, keys: ["sd.background"] },
            'deny': { source: 'deny', allow_empty: true }
        };
        this.state = {
            listKeyHead: 'custom_sd_head',
            listKeyData: 'custom_sd_data'
        };
        this.list = {};
        var ts = this;
        ts.once('init');
    }
    ;
    classCustomsActions.prototype.once = function (action, callback) {
        var ts = this;
        if (ts.ONCE.indexOf(action) != -1)
            return;
        else
            ts.ONCE.push(action);
        switch (action) {
            case "init":
                break;
            default:
                if (callback)
                    callback();
        }
    };
    ;
    classCustomsActions.prototype.getList = function () {
        return __awaiter(this, void 0, void 0, function () {
            var ts, _a;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0:
                        ts = this;
                        _a = ts;
                        return [4, ts.server({
                                send: {
                                    "action": "lists:get",
                                    "body": { key: ts.state.listKeyHead }
                                }
                            })["catch"](function (ex) {
                                console.info(ex);
                                ts.drawUI('error', { error: "Can't load list" });
                            })];
                    case 1:
                        _a.list = _b.sent();
                        return [2, ts.list];
                }
            });
        });
    };
    ;
    classCustomsActions.prototype.restore = function (uuidHead) {
        return __awaiter(this, void 0, void 0, function () {
            var ts, item, data, dataList, fileURL, i;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        ts = this;
                        item = ts.getItem(uuidHead);
                        data = false;
                        return [4, ts.server({
                                send: {
                                    "action": "lists:get",
                                    "body": {
                                        key: ts.state.listKeyData,
                                        uuid: item.uuidData
                                    }
                                }
                            })["catch"](function (ex) {
                                console.info(ex);
                                ts.drawUI('error', { error: "Can't load backup" });
                            })];
                    case 1:
                        dataList = _a.sent();
                        console.info('dataList', dataList);
                        fileURL = false;
                        for (i in dataList.items) {
                            if (dataList.items[i].uuid == item.uuidData) {
                                try {
                                    data = JSON.parse(dataList.items[i].value);
                                }
                                catch (ex) {
                                    console.warn(ex);
                                }
                                if (dataList.items[i].fileURL)
                                    fileURL = dataList.items[i].fileURL;
                                break;
                            }
                        }
                        ts.restoreData(data, fileURL);
                        return [2];
                }
            });
        });
    };
    ;
    classCustomsActions.prototype.restoreData = function (Back, fileURL) {
        var ts = this;
        console.info(Back);
        if (!Back) {
            ts.drawUI('error', { error: 'Backup is damaged' });
            return;
        }
        for (var key in ts.tables) {
            if (Back[key]) {
                switch (ts.tables[key].source) {
                    case "fvdSynchronizer":
                        ts.restorePrefsSync(key, Back[key]);
                        break;
                    case "fvdSpeedDial":
                        ts.restorePrefsSD(key, Back[key]);
                        break;
                    case "misc":
                        ts.restoreMiscSD(key, Back[key], fileURL);
                        break;
                    case "deny":
                        console.info('Restore deny');
                        ts.restoreDenySD(key, Back[key]);
                        break;
                }
            }
        }
        ts.progress();
    };
    ;
    classCustomsActions.prototype.restoreDenySD = function (Name, Data) {
        var ts = this;
        ts.tables[Name].need = Data.length;
        ts.tables[Name].redy = 0;
        console.info(Name, Data);
        fvdSynchronizer.Driver.Speeddial.gMessageToSD({ action: "Deny.set", data: Data });
        ts.tables[Name].redy = ts.tables[Name].need;
    };
    ;
    classCustomsActions.prototype.restoreMiscSD = function (Name, Data, fileURL) {
        var ts = this;
        ts.tables[Name].need = ts.tables[Name].length;
        ts.tables[Name].redy = 0;
        for (var key in ts.tables[Name].keys) {
            var dataSet = Data[key] || '';
            if (String(dataSet.val).indexOf('file:') === 0) {
                dataSet.val = fileURL;
            }
            console.info(dataSet);
            fvdSynchronizer.Driver.Speeddial.gMessageToSD({
                action: "miscItemSet",
                data: {
                    key: dataSet.key,
                    val: dataSet.val
                }
            });
            ts.tables[Name].redy++;
        }
    };
    ;
    classCustomsActions.prototype.restorePrefsSD = function (Name, Data) {
        var ts = this;
        ts.tables[Name].need = Data.length;
        ts.tables[Name].redy = 0;
        var updated = [];
        for (var key in Data) {
            ts.tables[Name].redy++;
            fvdSynchronizer.Driver.Speeddial.gMessageToSD({ action: "Prefs.set", data: { key: Data[key].key, val: Data[key].val } });
            updated.push(Data[key].key);
        }
    };
    ;
    classCustomsActions.prototype.restorePrefsSync = function (Name, Data) {
        var ts = this;
        ts.tables[Name].need = Data.length;
        ts.tables[Name].redy = 0;
        var updated = [];
        var safeKeys = ['autobackup.enabled', 'dont_display_ds_chromesync_message', 'dont_display_sync_access_warning'];
        for (var key in Data) {
            ts.tables[Name].redy++;
            if (safeKeys.indexOf(Data[key].key) === -1)
                continue;
            fvdSynchronizer.Prefs.set(Data[key].key, Data[key].val);
            updated.push(Data[key].key);
        }
    };
    ;
    classCustomsActions.prototype.progress = function () {
        var ts = this;
        var DonePrm = true;
        var info = { ready: 0, need: 0 };
        for (var key in ts.tables) {
            info.ready += ts.tables[key].redy || 0;
            info.need += ts.tables[key].need || 0;
            if (ts.tables[key].need > ts.tables[key].redy) {
                DonePrm = false;
            }
        }
        ts.drawUI("progress", { info: info, countdown: false });
        if (DonePrm) {
            console.info('Done');
            var delay = 10;
            ts.reloadAllPages(delay * 1000);
            fvdSynchronizer.Driver.Speeddial.gMessageToSD({ action: "setMisc" });
            var interval_1 = setInterval(function () {
                console.info(delay + 'sec before reload');
                delay--;
                ts.drawUI("progress", { info: info, countdown: Math.max(0, delay) });
                if (delay < 0)
                    clearInterval(interval_1);
            }, 1000);
        }
    };
    ;
    classCustomsActions.prototype.remove = function (uuidHead) {
        return __awaiter(this, void 0, void 0, function () {
            var ts, item, sendServerDelHead, sendServerDelData;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        ts = this;
                        item = ts.getItem(uuidHead);
                        sendServerDelHead = {
                            "action": "lists:delete",
                            "body": {
                                "key": ts.state.listKeyHead,
                                "uuid": [uuidHead]
                            }
                        };
                        sendServerDelData = {
                            "action": "lists:delete",
                            "body": {
                                "key": ts.state.listKeyData,
                                "uuid": [item.uuidData]
                            }
                        };
                        return [4, ts.server({ send: sendServerDelHead })];
                    case 1:
                        _a.sent();
                        ts.server({ send: sendServerDelData });
                        console.info('Remove finish');
                        return [2];
                }
            });
        });
    };
    ;
    classCustomsActions.prototype.rename = function (uuidHead, name) {
        return __awaiter(this, void 0, void 0, function () {
            var ts, item, sendServerAddHead, resultHead;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        ts = this;
                        item = ts.getItem(uuidHead);
                        item.name = name;
                        sendServerAddHead = {
                            "action": "lists:set",
                            "body": {
                                "key": ts.state.listKeyHead,
                                "item": [{
                                        uuid: uuidHead,
                                        value: JSON.stringify(item)
                                    }]
                            }
                        };
                        return [4, ts.server({ send: sendServerAddHead })];
                    case 1:
                        resultHead = _a.sent();
                        console.info('Rename finish');
                        return [2];
                }
            });
        });
    };
    ;
    classCustomsActions.prototype.getItem = function (uuidHead) {
        var ts = this;
        var item = false;
        for (var i in ts.list.items) {
            if (ts.list.items[i].uuid == uuidHead) {
                item = JSON.parse(ts.list.items[i].value);
                break;
            }
        }
        return item;
    };
    ;
    classCustomsActions.prototype.create = function (name) {
        var ts = this;
        console.info('Create: ', name);
        for (var key in ts.tables) {
            if (ts.tables[key].source == "fvdSynchronizer") {
                ts.readPrefsSync(key);
            }
            else if (ts.tables[key].source == "fvdSpeedDial") {
                ts.readPrefsSD(key);
            }
            else if (ts.tables[key].source == "misc") {
                ts.readMiscSD(key);
            }
            else if (ts.tables[key].source == "deny") {
                ts.readDenySD(key);
            }
        }
        var WaitInterval = setInterval(function () {
            var done = true;
            for (var key in ts.tables)
                if (ts.tables[key].wait)
                    done = false;
            if (done) {
                clearInterval(WaitInterval);
                ts.writeCustoms(name);
            }
        }, 750);
    };
    ;
    classCustomsActions.prototype.writeCustoms = function (name) {
        return __awaiter(this, void 0, void 0, function () {
            var ts, now, uuidHead, uuidData, tempfilename, customsData, k, sendServerAddHead, sendServerAddData, resultHead, resultData;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        ts = this;
                        now = Date.now();
                        uuidHead = ts.createUUID(now);
                        uuidData = ts.createUUID(now + 1);
                        tempfilename = null;
                        customsData = {};
                        for (k in ts.tables) {
                            customsData[k] = ts.tables[k].data;
                            if (ts.tables[k].file)
                                tempfilename = ts.tables[k].file;
                        }
                        sendServerAddHead = {
                            "action": "lists:set",
                            "body": {
                                "key": ts.state.listKeyHead,
                                "item": [{
                                        uuid: uuidHead,
                                        value: JSON.stringify({ name: name, date: now, uuidData: uuidData })
                                    }]
                            }
                        };
                        sendServerAddData = {
                            "action": "lists:set",
                            "body": {
                                "key": ts.state.listKeyData,
                                "item": [{
                                        uuid: uuidData,
                                        value: JSON.stringify(customsData),
                                        tempfilename: tempfilename
                                    }]
                            }
                        };
                        return [4, ts.server({ send: sendServerAddHead })];
                    case 1:
                        resultHead = _a.sent();
                        return [4, ts.server({ send: sendServerAddData })];
                    case 2:
                        resultData = _a.sent();
                        return [4, ts.removeTail()];
                    case 3:
                        _a.sent();
                        ts.drawUI(["customs-list", "highlight"]);
                        console.info('Finish');
                        return [2];
                }
            });
        });
    };
    ;
    classCustomsActions.prototype.removeTail = function () {
        return __awaiter(this, void 0, void 0, function () {
            var ts, tail, minDate, uuidHeadRemove, i, item, curDate;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        ts = this;
                        tail = 10;
                        minDate = false;
                        uuidHeadRemove = false;
                        if (!(ts.list.items.length >= tail)) return [3, 2];
                        for (i in ts.list.items) {
                            item = JSON.parse(ts.list.items[i].value);
                            curDate = parseInt(item.date) || 0;
                            minDate = minDate ? Math.min(minDate, curDate) : curDate;
                            if (curDate == minDate)
                                uuidHeadRemove = ts.list.items[i].uuid;
                        }
                        if (!uuidHeadRemove) return [3, 2];
                        console.info('Remove tail', uuidHeadRemove);
                        return [4, ts.remove(uuidHeadRemove)];
                    case 1:
                        _a.sent();
                        _a.label = 2;
                    case 2: return [2];
                }
            });
        });
    };
    ;
    classCustomsActions.prototype.drawUI = function (action, mode) {
        if (typeof action !== 'object')
            action = [action || "customs-list"];
        if (typeof fvdSynchronizer.Customs.ui == "object") {
            fvdSynchronizer.Customs.ui.draw(action, mode);
        }
    };
    ;
    classCustomsActions.prototype.readDenySD = function (Name) {
        var ts = this;
        ts.tables[Name].data = [];
        ts.tables[Name].wait = true;
        fvdSynchronizer.Driver.Speeddial.gMessageToSD({ action: "Deny.dump" }, function (response) {
            dump = response.result;
            for (var key in dump) {
                ts.tables[Name].data.push(dump[key]);
            }
            ts.tables[Name].success = true;
            ts.tables[Name].wait = false;
        });
    };
    ;
    classCustomsActions.prototype.readMiscSD = function (Name) {
        var ts = this;
        console.info('readMiscSD', Name);
        ts.tables[Name].data = [];
        ts.tables[Name].file = false;
        ts.tables[Name].wait = true;
        fvdSynchronizer.Utils.Async.arrayProcess(ts.tables[Name].keys, function (key, next) {
            console.info('readMiscSD item', key);
            fvdSynchronizer.Driver.Speeddial.gMessageToSD({ action: "miscItemGet", data: { key: key } }, function (response) {
                console.info('response', response);
                response.result = String(response.result);
                if (response.result.indexOf('data:') === 0 || response.result.indexOf('blob:') === 0) {
                    ts.getBlob(response.result, function (blob) {
                        console.info(blob);
                        var fname = key + Date.now();
                        ts.uploadTempFile(fname, blob, function (upResult) {
                            console.info(upResult);
                            var tempfile = false;
                            if (typeof upResult == "object" && typeof upResult.files == "object" && upResult.files.file) {
                                ts.tables[Name].file = upResult.files.file;
                                tempfile = upResult.files.file;
                            }
                            ts.tables[Name].data.push({ key: key, val: tempfile ? 'file:' + tempfile : false });
                            next();
                        });
                    });
                }
                else {
                    ts.tables[Name].data.push({ key: key, val: response.result });
                    next();
                }
            });
        }, function () {
            ts.tables[Name].success = true;
            ts.tables[Name].wait = false;
            console.info('Finish');
        });
    };
    ;
    classCustomsActions.prototype.uploadTempFile = function (filename, blob, callback) {
        fvdSynchronizer.Server.Sync.preUploadFile({
            blob: blob,
            name: filename
        }, function (error, data) {
            if (error) {
                return callback(error);
            }
            if (!data || !data.files) {
                return callback(fvdSynchronizer.Errors.ERROR_STORAGE_ENGINE_RETURNS_ERROR);
            }
            var filesMap = data.files;
            if (!filesMap.file) {
                return callback(fvdSynchronizer.Errors.ERROR_STORAGE_ENGINE_RETURNS_ERROR);
            }
            callback(data);
        });
    };
    ;
    classCustomsActions.prototype.getBlob = function (url, cb) {
        if (url.indexOf('data:') === 0) {
            var blob = fvdSynchronizer.Utils.dataURItoBlob(url);
            cb(blob);
        }
        else {
            var xhr = new XMLHttpRequest();
            xhr.open('GET', url, true);
            xhr.responseType = 'blob';
            xhr.onload = function (e) {
                if (this.status == 200) {
                    var myBlob = this.response;
                    cb(myBlob);
                }
            };
            xhr.send();
        }
    };
    ;
    classCustomsActions.prototype.readPrefsSD = function (Name) {
        var ts = this;
        console.info('readPrefs', Name);
        ts.tables[Name].data = [];
        ts.tables[Name].wait = true;
        fvdSynchronizer.Driver.Speeddial.gMessageToSD({ action: "Prefs.dump" }, function (response) {
            dump = response.result;
            for (var key in dump) {
                ts.tables[Name].data.push({ key: key, val: dump[key] });
            }
            ts.tables[Name].success = true;
            ts.tables[Name].wait = false;
        });
    };
    ;
    classCustomsActions.prototype.readPrefsSync = function (Name) {
        var ts = this;
        console.info('readPrefs', Name);
        ts.tables[Name].data = [];
        ts.tables[Name].wait = true;
        fvdSynchronizer.Prefs.dump(function (dump) {
            for (var key in dump) {
                ts.tables[Name].data.push({ key: key, val: dump[key] });
            }
            ts.tables[Name].success = true;
            ts.tables[Name].wait = false;
        });
    };
    ;
    classCustomsActions.prototype.server = function (data) {
        return __awaiter(this, void 0, void 0, function () {
            var ts;
            return __generator(this, function (_a) {
                ts = this;
                return [2, new Promise(function (resolve, reject) {
                        var xhr = ts.post(ts.API.server, JSON.stringify(data.send), function (result) {
                            if ((result.errorCode == 0) ||
                                ((data.options && data.options.errors) &&
                                    (data.options.errors === true || data.options.errors.indexOf(result.errorCode) > -1))) {
                                console.info(result);
                                resolve(result.body);
                            }
                            else {
                                console.warn(ts.API.server, data.send, result);
                                reject(result.errorCode);
                            }
                            xhr = null;
                        }, function (error) {
                            console.warn(error);
                            reject(error);
                        });
                    })];
            });
        });
    };
    ;
    classCustomsActions.prototype.post = function (url, data, successFunction, errorFunction) {
        $.post(url, data, successFunction)
            .fail(function (e) {
            if (errorFunction)
                errorFunction(e);
        });
    };
    classCustomsActions.prototype.createUUID = function (id) {
        var ts = this;
        var uuid = [];
        uuid.push(ts.crc32(String(id)).toString("16"));
        var date = new Date(parseInt(id) || id);
        uuid.push(date.getFullYear());
        uuid.push(String("0" + String(date.getMonth() + 1)).slice(-2)
            +
                String("0" + String(date.getDate())).slice(-2));
        uuid.push(String("0" + String(date.getHours())).slice(-2)
            +
                String("0" + String(date.getMinutes())).slice(-2));
        uuid.push(String(String(id).substr(1)));
        uuid = String(uuid.join('-'));
        return uuid;
    };
    ;
    classCustomsActions.prototype.reloadAllPages = function (timeout) {
        fvdSynchronizer.Driver.Speeddial.gMessageToSD({
            action: "reloadAllPages",
            timeout: timeout || 0
        });
        setTimeout(function () {
            document.location.reload();
        }, timeout || 150);
    };
    ;
    classCustomsActions.prototype.makeCRCTable = function () {
        var ts = this;
        var c;
        var crcTable = [];
        for (var n = 0; n < 256; n++) {
            c = n;
            for (var k = 0; k < 8; k++) {
                c = ((c & 1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1));
            }
            crcTable[n] = c;
        }
        return crcTable;
    };
    ;
    classCustomsActions.prototype.crc32 = function (str) {
        var ts = this;
        var crcTable = window.crcTable || (window.crcTable = ts.makeCRCTable());
        var crc = 0 ^ (-1);
        for (var i = 0; i < str.length; i++) {
            crc = (crc >>> 8) ^ crcTable[(crc ^ str.charCodeAt(i)) & 0xFF];
        }
        return (crc ^ (-1)) >>> 0;
    };
    ;
    return classCustomsActions;
}());
fvdSynchronizer.Customs.actions = new classCustomsActions();
//# sourceMappingURL=customs-actions.js.map