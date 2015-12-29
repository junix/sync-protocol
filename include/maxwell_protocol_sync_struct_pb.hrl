-ifndef(SUBSCRIBE_REQ_T_PB_H).
-define(SUBSCRIBE_REQ_T_PB_H, true).
-record(subscribe_req_t, {
    user_id = erlang:error({required, user_id}),
    session_key = erlang:error({required, session_key})
}).
-endif.

-ifndef(SUBSCRIBE_REP_T_PB_H).
-define(SUBSCRIBE_REP_T_PB_H, true).
-record(subscribe_rep_t, {
    errcode = erlang:error({required, errcode}),
    errmsg = erlang:error({required, errmsg})
}).
-endif.

-ifndef(UNSUBSCRIBE_T_PB_H).
-define(UNSUBSCRIBE_T_PB_H, true).
-record(unsubscribe_t, {
    user_id = erlang:error({required, user_id}),
    session_key = erlang:error({required, session_key})
}).
-endif.

-ifndef(CLIENT_HEARTBEAT_T_PB_H).
-define(CLIENT_HEARTBEAT_T_PB_H, true).
-record(client_heartbeat_t, {
    
}).
-endif.

-ifndef(SERVER_HEARTBEAT_T_PB_H).
-define(SERVER_HEARTBEAT_T_PB_H, true).
-record(server_heartbeat_t, {
    
}).
-endif.

-ifndef(SYNC_EVENT_T_PB_H).
-define(SYNC_EVENT_T_PB_H, true).
-record(sync_event_t, {
    id = erlang:error({required, id}),
    payload = erlang:error({required, payload}),
    created_at = erlang:error({required, created_at})
}).
-endif.

-ifndef(SYNC_FROM_T_PB_H).
-define(SYNC_FROM_T_PB_H, true).
-record(sync_from_t, {
    from = erlang:error({required, from}),
    limit
}).
-endif.

-ifndef(SYNC_DATA_T_PB_H).
-define(SYNC_DATA_T_PB_H, true).
-record(sync_data_t, {
    latest_id = erlang:error({required, latest_id}),
    events = []
}).
-endif.

