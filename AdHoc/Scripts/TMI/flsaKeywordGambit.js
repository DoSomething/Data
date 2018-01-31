/*https://trello.com/c/dVJUNMfx/1171-data-request-tmi-foot-locker-short-code-metrics*/
db.messages.group({
    "initial": {
        "countstar": 0
    },
    "reduce": function(obj, prev) {
        if (true != null) if (true instanceof Array) prev.countstar += true.length;
        else prev.countstar++;
    },
    "cond": {
        "text": {$regex : /flsa/i}
    }
});