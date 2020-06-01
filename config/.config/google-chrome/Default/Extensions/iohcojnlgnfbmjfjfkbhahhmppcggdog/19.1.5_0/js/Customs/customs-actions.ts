fvdSynchronizer.Customs = {};

class classCustomsActions {
    DEV = true;
    ONCE: any = [];

    API = {
        auth  : "https://everhelper.me/auth/process.php?",
        server: "https://sync-eh.everhelper.me"
    };

    tables: any = {
        'fvdSynchronizer'  : {source:'fvdSynchronizer', allow_empty:true},
        'fvdSpeedDial'   : {source:'fvdSpeedDial', allow_empty:true},
        'misc'   : {source:'misc', allow_empty:true, keys:["sd.background"]}, // Task #2083
        'deny'   : {source:'deny', allow_empty:true}, // Task #2084
    };

    state = {
        listKeyHead: 'custom_sd_head',//'custom_lsp_head',
        listKeyData: 'custom_sd_data',//'custom_lsp_data',
    };

    list: any = {};

    constructor() {
        let ts = this;
        ts.once('init');
    };

    once(action: any, callback) {
        let ts = this;

        if (ts.ONCE.indexOf(action) != -1) return;
        else ts.ONCE.push(action);

        switch (action) {
            case "init":
                //console.info('Customs', 'hello');
            break;

            default:
                if (callback) callback();
        }
    };

    public async getList(){
        let ts = this;
        
        ts.list = await ts.server({
            send: {
                "action": "lists:get",
                "body": {key:ts.state.listKeyHead}
            }
        }).catch((ex)=>{
            console.info(ex);
            ts.drawUI('error', {error: "Can't load list"});
        });

        return ts.list;
    };

    public async restore(uuidHead): void{ // Task #2083
        let ts = this;
        let item:any = ts.getItem(uuidHead);
        let data:any = false;


        let dataList = await ts.server({
            send: {
                "action": "lists:get",
                "body": {
                    key:ts.state.listKeyData,
                    uuid: item.uuidData
                },
            }
        }).catch((ex)=>{
            console.info(ex);
            ts.drawUI('error', {error: "Can't load backup"});
        });

        console.info('dataList', dataList);

        let fileURL = false;

        for(let i in dataList.items){
            if(dataList.items[i].uuid == item.uuidData){
                try{
                    data = JSON.parse(dataList.items[i].value);
                }catch(ex){
                    console.warn(ex);
                }

                if(dataList.items[i].fileURL) fileURL = dataList.items[i].fileURL;

                break;
            }
        }

        ts.restoreData(data, fileURL);
    };

    restoreData(Back, fileURL):void {  // Task #2083
        let ts = this;
        
        console.info(Back);

        if(!Back){
            ts.drawUI('error', {error:'Backup is damaged'});
            return;
        }

        for(var key in ts.tables){
            if(Back[key]){
                switch(ts.tables[key].source){
                    case "fvdSynchronizer":
                        ts.restorePrefsSync(key, Back[key]);
                    break;
                    case "fvdSpeedDial":
                        ts.restorePrefsSD(key, Back[key]);
                    break;
                    case "misc":
                        ts.restoreMiscSD(key, Back[key], fileURL);
                    break;
                    case "deny": // #2084
                        console.info('Restore deny');
                        ts.restoreDenySD(key, Back[key]);
                    break;
                }
            }
        }

        ts.progress();
    };

    restoreDenySD(Name, Data):void{ // #2084
        let ts = this;

        ts.tables[Name].need = Data.length;
        ts.tables[Name].redy = 0;
        
        console.info(Name, Data);

        fvdSynchronizer.Driver.Speeddial.gMessageToSD({action: "Deny.set", data: Data});

        ts.tables[Name].redy = ts.tables[Name].need;
    };
    
    restoreMiscSD(Name, Data, fileURL):void{// Task #2083
        let ts = this;

        ts.tables[Name].need = ts.tables[Name].length;
        ts.tables[Name].redy = 0;
        
        for (var key in ts.tables[Name].keys) {
            let dataSet = Data[key] || '';

            if(String(dataSet.val).indexOf('file:') === 0){
                dataSet.val = fileURL;
            }

            console.info(dataSet);

            fvdSynchronizer.Driver.Speeddial.gMessageToSD({
                action: "miscItemSet", 
                data:{
                    key: dataSet.key,
                    val: dataSet.val
                }
            });

            ts.tables[Name].redy++;
        }
    };
    
    restorePrefsSD(Name, Data):void{
        let ts = this;

        ts.tables[Name].need = Data.length;
        ts.tables[Name].redy = 0;
        
        let updated = [];
        
        for (var key in Data) {
            ts.tables[Name].redy++;
            fvdSynchronizer.Driver.Speeddial.gMessageToSD({action: "Prefs.set", data:{key:Data[key].key, val:Data[key].val}});
            updated.push(Data[key].key);
        }    
    };
    
    restorePrefsSync(Name, Data):void{
        let ts = this;

        ts.tables[Name].need = Data.length;
        ts.tables[Name].redy = 0;
        
        let updated = [];

        let safeKeys = ['autobackup.enabled','dont_display_ds_chromesync_message','dont_display_sync_access_warning'];
        
        for (var key in Data) {
            ts.tables[Name].redy++;

            if(safeKeys.indexOf(Data[key].key) === -1) continue;

            fvdSynchronizer.Prefs.set(Data[key].key, Data[key].val);
            updated.push(Data[key].key);
        }    
    };

    progress(){
        let ts = this;

        let DonePrm = true;
        
        let info = {ready:0, need:0};

        for(var key in ts.tables){
            //let text = `${key}: ${ts.tables[key].redy} / ${ts.tables[key].need}`;

            info.ready += ts.tables[key].redy || 0;
            info.need += ts.tables[key].need || 0;
            
            if(ts.tables[key].need > ts.tables[key].redy){
                DonePrm = false;
            }
        }

        ts.drawUI("progress", {info:info, countdown:false});
        
        if(DonePrm){
            console.info('Done');

            var delay = 10;

            ts.reloadAllPages(delay*1000);

            fvdSynchronizer.Driver.Speeddial.gMessageToSD({action: "setMisc"}); // Task #2009

            let interval = setInterval(function(){
                console.info(delay + 'sec before reload');

                delay--;

                ts.drawUI("progress", {info:info, countdown:Math.max(0, delay)});

                if(delay < 0) clearInterval(interval);
            }, 1000);
        }
    };

    public async remove(uuidHead:string): void{
        let ts = this;
        let item:any = ts.getItem(uuidHead);
        

        let sendServerDelHead = {
            "action": "lists:delete",
            "body": {
                "key": ts.state.listKeyHead,
                "uuid": [uuidHead]
            }
        };

        let sendServerDelData = {
            "action": "lists:delete",
            "body": {
                "key": ts.state.listKeyData,
                "uuid": [item.uuidData]
            }
        };

        await ts.server({send: sendServerDelHead});
        ts.server({send: sendServerDelData});

        console.info('Remove finish');

        return;
    };

    public async rename(uuidHead:string, name:string):void{
        let ts = this;
        let item:any = ts.getItem(uuidHead);

        item.name = name;

        let sendServerAddHead = {
            "action": "lists:set",
            "body": {
                "key": ts.state.listKeyHead,
                "item": [{
                    uuid: uuidHead,
                    value: JSON.stringify(item),
                }]
            }
        };

        let resultHead = await ts.server({send: sendServerAddHead});

        console.info('Rename finish');
    };

    getItem(uuidHead:string):any{
        let ts = this;

        let item:any = false;

        for(let i in ts.list.items){
            if(ts.list.items[i].uuid == uuidHead){
                item = JSON.parse(ts.list.items[i].value);
                break;
            }
        }

        return item;
    };
    
    create(name:string):void{
        let ts = this;

        console.info('Create: ', name);

        for(var key in ts.tables){
            if(ts.tables[key].source == "fvdSynchronizer"){
                ts.readPrefsSync(key);
            }else
            if(ts.tables[key].source == "fvdSpeedDial"){
                ts.readPrefsSD(key);
                //ts.localStorageRead(key);
            }else
            if(ts.tables[key].source == "misc"){ // Task #2083
                ts.readMiscSD(key);
            }else
            if(ts.tables[key].source == "deny"){ // Task #2084
                ts.readDenySD(key);
            }
        }
        
        var WaitInterval = setInterval(function(){
            let done = true;
            for(var key in ts.tables) if(ts.tables[key].wait) done = false;
            
            if(done){
                clearInterval(WaitInterval);
                ts.writeCustoms(name);
            }
        }, 750);
    };

    async writeCustoms(name):any{// Task #2083
        let ts = this;
        let now = Date.now();

        let uuidHead = ts.createUUID(now);
        let uuidData = ts.createUUID(now+1);

        let tempfilename = null;

        let customsData = {};
        for(let k in ts.tables){
            customsData[k] = ts.tables[k].data;
            if(ts.tables[k].file) tempfilename = ts.tables[k].file;
        }

        let sendServerAddHead = {
            "action": "lists:set",
            "body": {
                "key": ts.state.listKeyHead,
                "item": [{
                    uuid: uuidHead,
                    value: JSON.stringify({name:name, date:now, uuidData:uuidData})
                }]
            }
        };

        let sendServerAddData = {
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

        let resultHead = await ts.server({send: sendServerAddHead});
        let resultData = await ts.server({send: sendServerAddData});
        await ts.removeTail();

        ts.drawUI(["customs-list", "highlight"]);

        console.info('Finish');
    };

    async removeTail(){
        let ts = this;
        
        let tail = 10;
        let minDate = false;
        let uuidHeadRemove = false;

        if(ts.list.items.length >= tail){
            for(let i in ts.list.items){
                let item = JSON.parse(ts.list.items[i].value);
                let curDate = parseInt(item.date) || 0;

                minDate = minDate ? Math.min(minDate, curDate) : curDate;
                if(curDate == minDate) uuidHeadRemove = ts.list.items[i].uuid;
            }
    
            if(uuidHeadRemove){
                console.info('Remove tail', uuidHeadRemove);
                await ts.remove(uuidHeadRemove);
            }
        }

        return;
    };

    drawUI(action:any, mode?:any){
        if(typeof action !== 'object') action = [action || "customs-list"];
        
        if(typeof fvdSynchronizer.Customs.ui == "object"){
            fvdSynchronizer.Customs.ui.draw(action, mode);
        }
    };

    readDenySD(Name):any{ // Task #2084
        let ts = this;

        ts.tables[Name].data=[];
        ts.tables[Name].wait = true;

        fvdSynchronizer.Driver.Speeddial.gMessageToSD(
            {action: "Deny.dump"},
            (response)=>{
                dump = response.result;

                for(let key in dump){
                    ts.tables[Name].data.push(dump[key]);
                }
    
                ts.tables[Name].success = true;
                ts.tables[Name].wait = false;
            }
        );
    };

    readMiscSD(Name):any{ // Task #2083
        let ts = this;

        console.info('readMiscSD', Name);

        ts.tables[Name].data=[];
        ts.tables[Name].file = false;
        ts.tables[Name].wait = true;

        fvdSynchronizer.Utils.Async.arrayProcess( ts.tables[Name].keys, function( key, next ) {
            console.info('readMiscSD item', key);

            fvdSynchronizer.Driver.Speeddial.gMessageToSD(
                {action: "miscItemGet", data:{key:key}},
                (response)=>{
                    console.info('response', response);
                    
                    response.result = String(response.result);

                    if(response.result.indexOf('data:') === 0 || response.result.indexOf('blob:') === 0){
                        ts.getBlob(response.result, (blob)=>{
                            console.info(blob);

                            let fname = key + Date.now();

                            ts.uploadTempFile(fname, blob, (upResult)=>{
                                console.info(upResult);

                                let tempfile = false;

                                if(typeof upResult == "object" && typeof upResult.files == "object" && upResult.files.file){
                                    ts.tables[Name].file = upResult.files.file;
                                    tempfile = upResult.files.file;
                                }

                                ts.tables[Name].data.push({key:key, val: tempfile ? 'file:'+tempfile : false});
                                next();
                            });
                        });
                    }else{
                        ts.tables[Name].data.push({key:key, val:response.result});
                        next();
                    }

                }
            );

        }, ()=>{
            ts.tables[Name].success = true;
            ts.tables[Name].wait = false;
            console.info('Finish');
        });
    };

    uploadTempFile(filename, blob, callback):any{
        fvdSynchronizer.Server.Sync.preUploadFile({
            blob: blob,
            name: filename
          }, function( error, data ){
            if(error) {
              return callback(error);
            }
            if(!data || !data.files) {
              return callback( fvdSynchronizer.Errors.ERROR_STORAGE_ENGINE_RETURNS_ERROR );
            }
            var filesMap = data.files;
            if(!filesMap.file) {
              return callback( fvdSynchronizer.Errors.ERROR_STORAGE_ENGINE_RETURNS_ERROR );
            }

            //console.info(data);
            callback( data );
        });
    };

    getBlob(url, cb):any{
        if(url.indexOf('data:') === 0){
            var blob = fvdSynchronizer.Utils.dataURItoBlob(url);
            cb(blob);
        }else{
            var xhr = new XMLHttpRequest();
            xhr.open('GET', url, true);
            xhr.responseType = 'blob';
            xhr.onload = function(e) {
              if (this.status == 200) {
                var myBlob = this.response;
                cb(myBlob);
              }
            };
            xhr.send();
        }
    };

    readPrefsSD(Name):any{
        let ts = this;

        console.info('readPrefs', Name);

        ts.tables[Name].data=[];
        ts.tables[Name].wait = true;

        fvdSynchronizer.Driver.Speeddial.gMessageToSD(
            {action: "Prefs.dump"},
            (response)=>{ // Task #2083
                //console.info('response', dump);
                dump = response.result;

                for(let key in dump){
                    ts.tables[Name].data.push({key:key, val:dump[key]});
                }
    
                ts.tables[Name].success = true;
                ts.tables[Name].wait = false;
            }
        );
    };

    readPrefsSync(Name):any{
        let ts = this;
        
        console.info('readPrefs', Name);

        ts.tables[Name].data=[];
        ts.tables[Name].wait = true;

        fvdSynchronizer.Prefs.dump((dump)=>{
            //console.info('Dump fvdSynchronizer', dump);

            for(let key in dump){
                ts.tables[Name].data.push({key:key, val:dump[key]});
            }

            ts.tables[Name].success = true;
            ts.tables[Name].wait = false;
        });

    };

    async server(data):any{ // data = {send, options, cb, error}
        let ts = this;
        
        return new Promise(function (resolve, reject) {
            let xhr = ts.post(
                ts.API.server, JSON.stringify(data.send),
                function(result){
                    if(
                        (result.errorCode == 0) || 
                        (
                            (data.options && data.options.errors) &&
                            (data.options.errors === true || data.options.errors.indexOf(result.errorCode) > -1)
                        )
                    ){
                        console.info(result);
                        resolve(result.body);
                    }else{
                        console.warn(ts.API.server, data.send, result);
                        reject(result.errorCode);
                    }
    
                    xhr = null;
                },
                function(error){
                    console.warn(error);
                    reject(error);
                }
            );
        });
    };

    post(url:any, data?:any, successFunction?:any, errorFunction?:any):any{
        $.post(url, data, successFunction)
            .fail(function(e) {
                if(errorFunction) errorFunction(e);
            })
        ;
    }

    createUUID(id){
        let ts = this;

        let uuid = [];
        
        uuid.push(ts.crc32(String(id)).toString("16"));
        
        let date = new Date(parseInt(id) || id);
        
        uuid.push(date.getFullYear());
        
        uuid.push(
            String("0"+String(date.getMonth()+1)).slice(-2)
            +
            String("0"+String(date.getDate())).slice(-2)
        );
        
        uuid.push(
            String("0"+String(date.getHours())).slice(-2)
            +
            String("0"+String(date.getMinutes())).slice(-2)
        );
        
        uuid.push(String(String(id).substr(1)));
        uuid = String(uuid.join('-'));
        
        return uuid;
    };

    reloadAllPages(timeout:any):void{
        
        fvdSynchronizer.Driver.Speeddial.gMessageToSD({
            action: "reloadAllPages", 
            timeout: timeout || 0
        });

        setTimeout(function(){
            document.location.reload();
        }, timeout || 150);
    };

    makeCRCTable():any{
        let ts = this;

        var c;
        var crcTable = [];
        for(var n =0; n < 256; n++){
            c = n;
            for(var k =0; k < 8; k++){
                c = ((c&1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1));
            }
            crcTable[n] = c;
        }

        return crcTable;
    };
    
    crc32(str:string):any {
        let ts = this;

        var crcTable = window.crcTable || (window.crcTable = ts.makeCRCTable());
        var crc = 0 ^ (-1);
    
        for (var i = 0; i < str.length; i++ ) {
            crc = (crc >>> 8) ^ crcTable[(crc ^ str.charCodeAt(i)) & 0xFF];
        }
    
        return (crc ^ (-1)) >>> 0;
    };
}

// ########################## CALL ########################## //

fvdSynchronizer.Customs.actions = new classCustomsActions();