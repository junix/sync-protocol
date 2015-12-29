-ifndef(SUBSCRIBE_REQ_PB_H).
-define(SUBSCRIBE_REQ_PB_H, true).
-record(subscribe_req, {
    user_id = erlang:error({required, user_id}),
    session_key = erlang:error({required, session_key})
}).
-endif.

-ifndef(SUBSCRIBE_REP_PB_H).
-define(SUBSCRIBE_REP_PB_H, true).
-record(subscribe_rep, {
    code = erlang:error({required, code})
}).
-endif.

-ifndef(UNSUBSCRIBE_REQ_PB_H).
-define(UNSUBSCRIBE_REQ_PB_H, true).
-record(unsubscribe_req, {
    user_id = erlang:error({required, user_id}),
    session_key = erlang:error({required, session_key})
}).
-endif.

-ifndef(CLIENT_HEARTBEAT_PB_H).
-define(CLIENT_HEARTBEAT_PB_H, true).
-record(client_heartbeat, {
    
}).
-endif.

-ifndef(SERVER_HEARTBEAT_PB_H).
-define(SERVER_HEARTBEAT_PB_H, true).
-record(server_heartbeat, {
    
}).
-endif.

-ifndef(EVENT_PB_H).
-define(EVENT_PB_H, true).
-record(event, {
    id = erlang:error({required, id}),
    payload = erlang:error({required, payload}),
    date_added = erlang:error({required, date_added})
}).
-endif.

-ifndef(SYNC_FROM_PB_H).
-define(SYNC_FROM_PB_H, true).
-record(sync_from, {
    from = erlang:error({required, from}),
    limit = erlang:error({required, limit})
}).
-endif.

-ifndef(SYNC_PB_H).
-define(SYNC_PB_H, true).
-record(sync, {
    latest_id = erlang:error({required, latest_id}),
    events = []
}).
-endif.

